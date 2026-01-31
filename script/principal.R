# Configurações iniciais ----
rm(list = ls(all=T))
library(tidyverse)
library(magrittr)
library(REAT)
library(readxl)
library(WDI)

# Chegadas ----
## Importa dados da OMT ----
path_chegadas <- "data/UN_Tourism_inbound_arrivals_10_2025.xlsx"
bd_raw_chegadas <- readxl::read_excel(path_chegadas, 
                             sheet = "Data",
                             col_types = c("text", "text", "text", "numeric", "text", "numeric",
                                           "text","numeric","numeric","text","text","text",
                                           "text","text")) |> 
  janitor::clean_names()

## Examina dados ----
bd_raw_chegadas |> count(indicator_code, indicator_label)
bd_raw_chegadas |> filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR")
bd_raw_chegadas |> filter(is.na(partner_area_label))
bd_raw_chegadas |> filter(partner_area_label != "World")

bd_raw_chegadas |> filter(partner_area_label == "World") |> count(indicator_code)
bd_raw_chegadas |> count(indicator_code)
bd_raw_chegadas |> group_by(indicator_code) |> filter(year %in% 2018) |> summarise(sum(value))
bd_raw_chegadas |> 
  group_by(indicator_code) |> 
  filter(year %in% 2018) |> count()

bd_raw_chegadas |> filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR", year %in% c(2002:2024)) |> 
  group_by(reporter_area_label) |> count() |> group_by(n) |> count() |> ggplot(aes(x=n, y=nn)) + 
  geom_bar(stat = "identity")

## Monta banco  ----
bd_chegadas <- bd_raw_chegadas |>
  filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR") |>
  summarise(chegadas = sum(value), .by = c(reporter_area_label, year)) |> 
  rename(pais = reporter_area_label,
         ano = year)


# Receitas ----
## Importa dados da OMT ----
path_receitas <- "data/UN_Tourism_inbound_expenditure_10_2025.xlsx"
bd_raw_receitas <- readxl::read_excel(path_receitas,
                             sheet = "Data",
                             col_types = c("text", "text", "text", "numeric", "text", "numeric",
                                           "text","numeric","numeric","text","text","text",
                                           "text","text")) |>
  janitor::clean_names()


## Examina dados ----
bd_raw_receitas |> count(indicator_label)
bd_raw_receitas |> count(indicator_code, indicator_label)
bd_raw_receitas |> filter(indicator_code == "INBD_EXPD_BPAY_TRVL_VSTR", year %in% c(2002:2024)) |> 
  group_by(reporter_area_label) |> count() |> group_by(n) |> count() |> ggplot(aes(x=n, y=nn)) + 
  geom_bar(stat = "identity")


## Monta banco  ----
bd_receitas <- bd_raw_receitas |> filter(indicator_code == "INBD_EXPD_BPAY_TRVL_VSTR") |> 
  summarise(receitas = sum(value), .by = c(reporter_area_label, year)) |>
  rename(pais = reporter_area_label,
         ano = year)

bd <- left_join(bd_chegadas, bd_receitas, by = join_by(pais, ano))

# PIB ----
# 1. Definição do indicador de Crescimento Anual (%)
# NY.GDP.MKTP.KD.ZG = GDP growth (annual %)
indicador_crescimento <- "NY.GDP.MKTP.KD.ZG"

# 2. Baixar dados (Mundo e Brasil para comparação)
# iso2c "1W" = Mundo, "BR" = Brasil, "CN" = China, "US" = EUA
paises_interesse <- c("1W")


dados_cresc <- WDI(
  country = paises_interesse,
  indicator = indicador_crescimento,
  start = 2002,
  end = 2024
)

# 3. Limpeza
dados_limpos <- dados_cresc |>
  as_tibble() |>
  rename(crescimento_anual = NY.GDP.MKTP.KD.ZG) |>
  filter(!is.na(crescimento_anual))

attr(dados_limpos$crescimento_anual, "label") <- NULL

# # 4. Visualização com linha de referência em 0
# ggplot(dados_limpos, aes(x = year, y = crescimento_anual, color = country)) +
#   # Linha horizontal no zero para destacar recessões
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   # Ajustar eixo Y para mostrar porcentagem
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   scale_x_continuous(breaks = seq(2003, 2025, 2)) + # Pula de 2 em 2 anos
#   labs(
#     title = "Taxa de Crescimento Real do PIB (% Anual)",
#     subtitle = "Comparativo: Brasil, EUA e Média Mundial",
#     y = "Crescimento (%)",
#     x = "Ano",
#     caption = "Fonte: Banco Mundial (Indicador NY.GDP.MKTP.KD.ZG)",
#     color = "Região/País"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

world_gdp  <- dados_limpos |> arrange(year) |> mutate(crescimento_anual2 = crescimento_anual / 100) |> pull(crescimento_anual2)
pib <- data.frame(ano = 2002:2024, g_world_gdp = world_gdp)
# pib <- pib |> mutate(g_world_gdp = ifelse(ano == 2003, NA, g_world_gdp))

# Shift-share algébrica ----
# INSERIR O BENCHMARK DA AMÉRICA DO SUL
calcular_shift_share <- function(bd, df_pib, paises_concorrentes, variavel = "chegadas", ano_inicio, ano_fim) {
  
  # --- 1. Validação ---
  if (!variavel %in% c("chegadas", "receitas")) {
    stop("O argumento 'variavel' deve ser 'chegadas' ou 'receitas'.")
  }
  
  # Seleciona colunas e renomeia variável de interesse dinamicamente
  bd_limpo <- bd %>%
    select(pais, ano, valor = .data[[variavel]])
  
  # --- 2. Cálculo do Benchmark MUNDIAL (Todos os países da base) ---
  dados_mundo <- bd_limpo %>%
    filter(ano >= (ano_inicio - 1), ano <= ano_fim) %>%
    group_by(ano) %>%
    summarise(
      mundo_volume = sum(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      g_mundo_tour = (mundo_volume / lag(mundo_volume)) - 1
    )
  
  # --- 3. Cálculo do Benchmark GRUPO (Brasil + Concorrentes) ---
  lista_paises <- unique(c("Brazil", paises_concorrentes))
  
  dados_grupo <- bd_limpo %>%
    filter(pais %in% lista_paises,
           ano >= (ano_inicio - 1), ano <= ano_fim) %>%
    group_by(ano) %>%
    summarise(
      grupo_volume = sum(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      g_grupo_tour = (grupo_volume / lag(grupo_volume)) - 1
    )
  
  # --- 4. Preparação dos Dados Individuais ---
  dados_individuais <- bd_limpo %>%
    filter(pais %in% lista_paises,
           ano >= (ano_inicio - 1), ano <= ano_fim)
  
  # Agregado dos concorrentes (sem Brasil)
  dados_concorrentes_agg <- dados_individuais %>%
    filter(pais != "Brazil") %>%
    group_by(ano) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(pais = "Concorrentes")
  
  dados_todos <- bind_rows(dados_individuais, dados_concorrentes_agg)
  
  # --- 5. Cálculo dos Efeitos Shift-Share ---
  
  df_calculo <- dados_todos %>%
    arrange(pais, ano) %>%
    group_by(pais) %>%
    mutate(
      g_pais = (valor / lag(valor)) - 1,
      lag_valor = lag(valor)
    ) %>%
    filter(ano >= ano_inicio - 1) %>%
    left_join(df_pib, by = "ano") %>%
    left_join(dados_mundo, by = "ano") %>%
    left_join(dados_grupo, by = "ano") %>%
    mutate(
      # Efeitos (Baseados em Volume)
      GGE = lag_valor * g_world_gdp,
      
      GIME_Grupo = lag_valor * (g_grupo_tour - g_world_gdp),
      GCSE_Grupo = lag_valor * (g_pais - g_grupo_tour),
      
      GIME_Mundo = lag_valor * (g_mundo_tour - g_world_gdp),
      GCSE_Mundo = lag_valor * (g_pais - g_mundo_tour),
      
      Total_Change = valor - lag_valor,
      
      # Share do país em relação ao MUNDO naquele ano
      share_mundo_atual = (valor / mundo_volume) * 100,
      share_mundo_anterior = (lag_valor / lag(mundo_volume)) * 100,
      var_share_pp = share_mundo_atual - share_mundo_anterior # Variação em pontos percentuais
    ) %>%
    ungroup() |> 
    filter(ano >= ano_inicio)
  
  # --- 6. Output 1: Série Temporal do Brasil ---
  df_brasil_timeseries <- df_calculo %>%
    filter(pais == "Brazil") %>%
    select(ano, 
           brazil_volume = valor, 
           share_global_pct = share_mundo_atual,
           var_share_pp, 
           g_world_gdp, 
           g_mundo_turismo = g_mundo_tour,
           g_grupo_turismo = g_grupo_tour,
           g_brazil_turismo = g_pais, 
           GGE, 
           GIME_Mundo, GCSE_Mundo,
           GIME_Grupo, GCSE_Grupo
    )
  
  # --- 7. Output 2: Resumo Consolidado (COMPLETO) ---
  df_resumo_periodo <- df_calculo %>%
    group_by(pais) %>%
    summarise(
      # Volumetria
      Volume_Inicial = first(lag_valor),
      Volume_Final = last(valor),
      Variação_Total = sum(Total_Change, na.rm = TRUE),
      
      # Métricas de Share (Adicionadas)
      Share_Global_Inicial = first(share_mundo_anterior),
      Share_Global_Final = last(share_mundo_atual),
      Variação_Share_PP = Share_Global_Final - Share_Global_Inicial,
      
      # Efeitos acumulados (Todos mantidos)
      Total_GGE = sum(GGE, na.rm = TRUE),
      
      # Contexto Mundo
      Total_GIME_Mundo = sum(GIME_Mundo, na.rm = TRUE),
      Total_GCSE_Mundo = sum(GCSE_Mundo, na.rm = TRUE),
      
      # Contexto Grupo
      Total_GIME_Grupo = sum(GIME_Grupo, na.rm = TRUE),
      Total_GCSE_Grupo = sum(GCSE_Grupo, na.rm = TRUE)

    ) %>%
    arrange(desc(Total_GCSE_Mundo))
  
  return(list(
    serie_brasil = df_brasil_timeseries,
    resumo_geral = df_resumo_periodo
  ))
}



# Países de interesse ----
concorrentes <- c("Brazil","Mexico",
                  "Peru",
                  "United States of America",
                  "Dominican Republic",
                  "Colombia",
                  "Spain",
                  "Chile",
                  "Argentina",
                  "Canada",
                  "Italy",
                  "Uruguay",
                  "United Arab Emirates",
                  "Costa Rica",
                  "Egypt",
                  "India",
                  "Portugal",
                  "South Africa",
                  "Morocco",
                  "France",
                  "Jamaica",
                  "United Kingdom of Great Britain and Northern Ireland",
                  "Cuba")

concorrentes_diretos <- c("Brazil","Mexico",
                  "Peru",
                  "Dominican Republic",
                  "Colombia",
                  "Spain",
                  "Chile",
                  "Argentina",
                  "Costa Rica",
                  "Jamaica",
                  "Cuba")

# Rodar a função ----
resultados <- calcular_shift_share(
  bd = bd,             # Seu banco de dados
  df_pib = pib,        # Seu banco do PIB
  paises_concorrentes = concorrentes_diretos, 
  variavel = "chegadas",
  ano_inicio = 2003,
  ano_fim = 2019
)

# 1. Para ver a tabela de tempo original (Só Brasil)
print(resultados$serie_brasil, n = Inf)
resultados$serie_brasil |> View("Brasil")

# 2. Para ver o novo resumo comparativo (Brasil, Países Individuais e Grupo Concorrente)
print(resultados$resumo_geral, n = Inf)
resultados$resumo_geral |> arrange(desc(Total_GCSE_Mundo)) |> View("Resumo Geral")


# Visualização rápida com ggplot (opcional)

resultados$serie_brasil |>
  select(ano, GGE, GIME_Mundo, GCSE_Mundo) |>
  pivot_longer(cols = -ano, names_to = "Efeito", values_to = "Valor") |>
  ggplot(aes(x = ano, y = Valor, fill = Efeito)) +
  geom_col() +
  labs(title = "Decomposição Shift-Share: Brasil vs Competidores",
       y = "Variação de chegadas (nº)",
       subtitle = "Comparado ao grupo selecionado") +
  theme_minimal()

# Regressão shift-share ----
## -> Os resultados da regressão estão estranhos, provavelmente por causa da pandemia.
## -> Mas há poucas observações e os coeficientes estimados em geral não são signficantes.
## -> Estou na dúvida se a regressão ajuda em algo na análise. Talvez precise ler mais artigos que usaram regressão.

library(broom)
library(purrr)

calcular_regressao_shift_share <- function(bd, df_pib, df_covariaveis = NULL, paises_concorrentes, variavel = "chegadas", ano_inicio, ano_fim) {
  
  # --- 1. Validação e Preparação ---
  if (!variavel %in% c("chegadas", "receitas")) {
    stop("O argumento 'variavel' deve ser 'chegadas' ou 'receitas'.")
  }
  
  # Seleção dinâmica da variável
  bd_limpo <- bd %>%
    select(pais, ano, valor = .data[[variavel]])
  
  # Lista completa para filtros
  lista_paises <- unique(c("Brazil", paises_concorrentes))
  
  # --- 2. Construção das Variáveis Explicativas (Independentes) ---
  # Calcula o crescimento do TOTAL MUNDIAL (g_mundo_tour)
  # Isso servirá para calcular o efeito Mix (Turismo vs PIB)
  dados_mundo <- bd_limpo %>%
    filter(ano >= (ano_inicio - 1), ano <= ano_fim) %>%
    group_by(ano) %>%
    summarise(mundo_volume = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(g_mundo_tour = (mundo_volume / lag(mundo_volume)) - 1) %>%
    filter(ano >= ano_inicio) %>%
    left_join(df_pib, by = "ano") %>%
    mutate(
      # Diferença entre cresc. do Turismo Mundial e PIB Mundial
      diff_tour_gdp = g_mundo_tour - g_world_gdp
    ) %>%
    select(ano, g_world_gdp, diff_tour_gdp)
  
  # Se houver covariáveis, junta aqui
  if (!is.null(df_covariaveis)) {
    dados_mundo <- dados_mundo %>%
      left_join(df_covariaveis, by = "ano")
  }
  
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
  
  vars_base <- c("g_world_gdp", "diff_tour_gdp")
  
  # Adiciona nomes das covariáveis extras se existirem
  vars_extras <- if (!is.null(df_covariaveis)) setdiff(names(df_covariaveis), "ano") else NULL
  
  formula_str <- paste("g_pais ~", paste(c(vars_base, vars_extras), collapse = " + "))
  
  print(paste("Rodando regressões para", length(unique(dados_regressao$pais)), "entidades."))
  print(paste("Fórmula:", formula_str))
  
  # --- 5. Execução (Loop por Entidade) ---
  resultados_brutos <- dados_regressao %>%
    group_by(pais) %>%
    nest() %>%
    mutate(
      modelo = map(data, ~ lm(as.formula(formula_str), data = .x)),
      tidied = map(modelo, broom::tidy),       # Coeficientes
      glanced = map(modelo, broom::glance)    # R2, AIC, etc
    )
  
  # --- 6. Formatação Final ---
  tabela_coeficientes <- resultados_brutos %>%
    unnest(tidied) %>%
    mutate(
      significancia = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      ),
      Termo_Interpretado = case_when(
        term == "(Intercept)"   ~ "Alpha (Competitividade Autônoma)",
        term == "g_world_gdp"   ~ "Beta 1 (Sensibilidade ao PIB)",
        term == "diff_tour_gdp" ~ "Beta 2 (Sensibilidade ao Setor)",
        TRUE ~ term
      )
    ) %>%
    select(pais, Termo_Interpretado, estimativa = estimate, erro_padrao = std.error, 
           p_valor = p.value, significancia)
  
  tabela_qualidade <- resultados_brutos %>%
    unnest(glanced) %>%
    select(pais, r_quadrado = r.squared, r_quadrado_ajustado = adj.r.squared, obs = nobs)
  
  # Retorna lista com coeficientes e qualidade do ajuste
  return(list(coeficientes = tabela_coeficientes, ajuste = tabela_qualidade))
}

## Rodar a função da regressão ----
resultados_reg <- calcular_regressao_shift_share(
  bd = bd,
  df_pib = pib,
  paises_concorrentes = concorrentes_diretos,
  variavel = "chegadas", # ou "receitas"
  ano_inicio = 2003,
  ano_fim = 2019
)

# A) Coeficientes (Alphas e Betas)
# Beta 1 (Coeficiente do GGE): Mede a sensibilidade do país ao crescimento da economia global
# Beta 2 (Coeficiente do GIME): Mede a capacidade do país de capturar o crescimento "extra" do setor de turismo (o boom do setor).
# Gamma (Covariáveis): Captura efeitos explicáveis (ex: Câmbio desvalorizado ajuda? A Copa de 2014 ajudou?).
# Alpha (Intercepto): O que sobra é a competitividade "inexplicável" ou estrutural (o "Alpha" puro).
print(resultados_reg$coeficientes, n=Inf)
resultados_reg$coeficientes |> View("Coeficientes")
resultados_reg$coeficientes |> filter(Termo_Interpretado == "Alpha (Competitividade Autônoma)") |> arrange(desc(estimativa)) |> View("Alpha")
resultados_reg$coeficientes |> filter(Termo_Interpretado == "Beta 1 (Sensibilidade ao PIB)") |> arrange(desc(estimativa)) |> View("Beta 1")
resultados_reg$coeficientes |> filter(Termo_Interpretado == "Beta 2 (Sensibilidade ao Setor)") |> arrange(desc(estimativa)) |> View("Beta 2")

# B) Qualidade do modelo (R²) para cada entidade
print(resultados_reg$ajuste)
resultados_reg$ajuste |> View("R2")
