ano_inicio <- 2003
ano_fim <- 2024

# Seleção dinâmica da variável
bd_limpo <- bd %>%
  select(pais, ano, valor = chegadas)

# Lista completa para filtros
lista_paises <- concorrentes_diretos

# --- 2. Construção das Variáveis Explicativas (Independentes) ---
# Calcula o crescimento do TOTAL MUNDIAL (g_mundo_tour)
# Isso servirá para calcular o efeito Mix (Turismo vs PIB)
dados_mundo <- bd_limpo %>%
  filter(ano >= (ano_inicio - 1), ano <= ano_fim) %>%
  group_by(ano) %>%
  summarise(mundo_volume = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(g_mundo_tour = (mundo_volume / lag(mundo_volume)) - 1) %>%
  filter(ano >= ano_inicio) %>%
  left_join(pib, by = "ano") %>%
  mutate(
    # Diferença entre cresc. do Turismo Mundial e PIB Mundial
    diff_tour_gdp = g_mundo_tour - g_world_gdp
  ) %>%
  select(ano, g_world_gdp, g_mundo_tour, diff_tour_gdp)


# --- 3. Construção das Entidades para Regressão (Dependentes) ---

# A) Entidades Individuais (Brasil + Concorrentes separadamente)
dados_individuais <- bd_limpo %>%
  filter(pais %in% lista_paises,
         ano >= (ano_inicio - 1), ano <= ano_fim)

# B) Entidade Agregada (Concorrentes somados, excluindo Brasil)
# Somamos os volumes PRIMEIRO, depois calculamos o crescimento
dados_agregados <- bd_limpo %>%
  filter(pais %in% concorrentes_diretos, # Apenas concorrentes
         pais != "Brazil",               # Garante que Brasil está fora
         ano >= (ano_inicio - 1), ano <= ano_fim) %>%
  group_by(ano) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(pais = "Competitors (Aggr.)")   # Nome da nova entidade

# C) Unir tudo e calcular crescimento (g_pais)
dados_regressao <- bind_rows(dados_individuais, dados_agregados) %>%
  arrange(pais, ano) %>%
  group_by(pais) %>%
  mutate(
    g_pais = (valor / lag(valor)) - 1
  ) %>%
  filter(ano >= ano_inicio) %>% # Remove o ano do lag
  ungroup() %>%
  # Junta com as variáveis explicativas (Mundo)
  left_join(dados_mundo, by = "ano") %>%
  na.omit() # Remove NAs para a regressão rodar

# --- 4. Definição do Modelo ---
# Modelo: Cresc. País ~ PIB Global + (Turismo Global - PIB Global) + Covariáveis

bdbr <- dados_regressao |> filter(pais == "Brazil")
summary(lm(g_pais ~ g_mundo_tour, data = bdbr))
summary(lm(g_pais ~ g_mundo_tour + g_world_gdp, data = bdbr))
summary(lm(g_pais ~ g_mundo_tour + diff_tour_gdp, data = bdbr))

bdbr |> ggplot(aes(x=g_world_gdp, y=g_pais)) + geom_point()
dados_regressao |> ggplot(aes(x=g_world_gdp, y=g_pais)) + geom_point()
