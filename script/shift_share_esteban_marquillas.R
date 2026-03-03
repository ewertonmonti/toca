#### CHATGPT ###
# Configurações iniciais ----
options(scipen = 999) 
rm(list = ls(all=T))
library(tidyverse)
library(magrittr)
library(readxl)
library(gt)
library(treemapify)
library(scales)

bd <- readRDS("data/untourism_tourists_visitors.rds")
bd %<>% rename(destino = reporter_area_label, origem = partner_area_label, ano = year, turistas = value)
bd %<>% relocate(origem, destino)
bd %<>% mutate(tipo_area = ifelse(partner_area_code > 1000, "regiao","pais"))
concorrentes <- c("Argentina", "Brazil", "Colombia", "Cuba", "Dominican Republic", "Jamaica", "Peru")
concorrentes_com_Espanha <- c("Argentina", "Brazil", "Colombia", "Cuba", "Dominican Republic", "Jamaica", "Peru", "Spain")


bd <- bd |> filter(tipo_area == "regiao", str_detect(origem, "UNWTO"), destino %in% concorrentes) |> 
  mutate(origem = case_when(
    origem == "Americas (UNWTO total)" ~ "Americas",
    origem == "Europe (UNWTO total)" ~ "Europe",
    .default = "Outros")) |> 
  summarise(turistas = sum(turistas), .by = c(origem, destino, ano)) |> 
  rename(chegadas = turistas)


# Descrição do grupo ----
# Gráfico da composição deste grupo
bd |> 
  filter(destino %in% concorrentes, ano %in% 2010:2019) |>
  summarise(chegadas = sum(chegadas), .by = c(destino, ano)) |> 
  ggplot(aes(x = ano, y = chegadas, fill = destino)) +
  geom_area() +
  labs(
    title = "Total de chegadas turísticas internacionais, por ano",
    y = "Chegadas (mil)",
    x = "Ano") + 
  scale_x_continuous(breaks = seq(2010, 2019, 1)) 
# +  scale_y_continuous(labels = fun_formatC_inteiros)

bd |> 
  filter(destino %in% concorrentes, ano == 2019) |>
  summarise(chegadas = sum(chegadas), .by = destino) |> 
  mutate(chegadas = round(chegadas / sum(chegadas) * 100, 1)) |> 
  slice_max(chegadas, n = 20) |>  
  ggplot(aes(area = chegadas, fill = destino, 
             label = paste0(destino, 
                            "\n",
                            formatC(round(chegadas, 1), format = "f", big.mark = ".", decimal.mark = ",", digits = 1),
                            "%"))) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text() +
  labs(title = "Participação em 2019")


# Shift-share estático + Esteban-Marquillas ----

shift_share_static_em <- function(bd, year0 = 2009, year1 = 2019) {
  
  # --- checagens mínimas
  stopifnot(all(c("ano","origem","destino","chegadas") %in% names(bd)))
  if (!all(c(year0, year1) %in% unique(bd$ano))) {
    stop("year0 e/ou year1 não existem em bd$ano.")
  }
  
  # --- agrega caso haja duplicidades por célula
  df <- bd %>%
    filter(ano %in% c(year0, year1)) %>%
    group_by(ano, origem, destino) %>%
    summarise(chegadas = sum(chegadas, na.rm = TRUE), .groups = "drop")
  
  # --- completa combinações faltantes como 0 (opcional, mas ajuda na consistência)
  # Se você não quiser completar com zeros, me diga.
  df <- df %>%
    tidyr::complete(
      ano = c(year0, year1),
      origem,
      destino,
      fill = list(chegadas = 0)
    )
  
  wide <- df %>%
    pivot_wider(names_from = ano, values_from = chegadas, names_sort = TRUE) %>%
    rename(T0 = !!as.character(year0),
           T1 = !!as.character(year1))
  
  # --- totais necessários
  T0_AREA  <- sum(wide$T0)
  T1_AREA  <- sum(wide$T1)
  if (T0_AREA == 0) stop("T0_AREA = 0. Não dá para calcular G_AREA.")
  
  G_AREA <- (T1_AREA - T0_AREA) / T0_AREA
  
  # Totais por origem para a 'área' (benchmark = conjunto de destinos)
  by_orig <- wide %>%
    group_by(origem) %>%
    summarise(T0_iAREA = sum(T0), T1_iAREA = sum(T1), .groups = "drop") %>%
    mutate(
      denom_ok = T0_iAREA != 0,
      G_iAREA  = if_else(denom_ok, (T1_iAREA - T0_iAREA) / T0_iAREA, NA_real_)
    )
  
  # Totais por destino no período 0
  by_dest0 <- wide %>%
    group_by(destino) %>%
    summarise(T0_j = sum(T0), .groups = "drop")
  
  # Junta tudo
  out <- wide %>%
    left_join(by_orig,  by = "origem") %>%
    left_join(by_dest0, by = "destino") %>%
    mutate(
      # checagens de denominadores
      denom_ok_cell  = (T0 != 0),
      denom_ok_iAREA = (T0_iAREA != 0),
      
      # taxas
      G_ij = if_else(denom_ok_cell, (T1 - T0) / T0, NA_real_),
      
      # homotético (T_hat_ij = T0_j * (T0_iAREA / T0_AREA))
      T_hat = T0_j * (T0_iAREA / T0_AREA),
      
      # componentes (Alavi & Yasin / Toh et al.)
      actual_growth = T1 - T0,
      area_wide     = T0 * G_AREA,
      mix_effect    = T0 * (G_iAREA - G_AREA),
      competitive   = T_hat * (G_ij - G_iAREA),
      allocation    = (T0 - T_hat) * (G_ij - G_iAREA),
      
      # checagem de identidade
      check_sum = area_wide + mix_effect + competitive + allocation
    )
  
  # --- se houver casos que impedem cálculo (NA em taxas), pare e avise
  bad <- out %>% filter(is.na(G_iAREA) | is.na(G_ij))
  if (nrow(bad) > 0) {
    msg <- bad %>%
      transmute(origem, destino, T0, T1, G_iAREA, G_ij) %>%
      head(20)
    stop(
      "Há células com denominador zero (T0==0 e/ou T0_iAREA==0), ",
      "o que torna G_ij e/ou G_iAREA indefinidos.\n",
      "Exemplos (até 20):\n",
      paste(capture.output(print(msg)), collapse = "\n"),
      "\n\nDiga como você quer tratar zeros (excluir, epsilon, etc.) que eu ajusto."
    )
  }
  
  list(
    params = list(year0 = year0, year1 = year1, T0_AREA = T0_AREA, T1_AREA = T1_AREA, G_AREA = G_AREA),
    cell_level = out %>%
      select(ano0 = T0, ano1 = T1, origem, destino,
             T0, T1, actual_growth, area_wide, mix_effect, competitive, allocation, check_sum),
    by_destino = out %>%
      group_by(destino) %>%
      summarise(across(c(actual_growth, area_wide, mix_effect, competitive, allocation), sum), .groups = "drop"),
    by_origem = out %>%
      group_by(origem) %>%
      summarise(across(c(actual_growth, area_wide, mix_effect, competitive, allocation), sum), .groups = "drop"),
    total = out %>%
      summarise(across(c(actual_growth, area_wide, mix_effect, competitive, allocation), sum))
  )
}

# ---- Exemplo de execução
res_static <- shift_share_static_em(bd, year0 = 2010, year1 = 2019)

# Resultados
res_static$total
res_static$by_destino
res_static$by_origem
res_static$cell_level  # nível origem-destino


# Shift-share dinâmico + Esteban-Marquillas ----
# Shift-Share (Esteban-Marquillas) - Dinâmico (somatório anual)

shift_share_dynamic_em <- function(bd, years = 2010:2019) {
  
  years <- sort(unique(years))
  if (length(years) < 2) stop("years precisa ter pelo menos 2 anos.")
  
  # roda estático para cada par consecutivo e soma
  pairs <- tibble(y0 = years[-length(years)], y1 = years[-1])
  
  pieces <- purrr::pmap_dfr(
    list(pairs$y0, pairs$y1),
    function(y0, y1) {
      res <- shift_share_static_em(bd, year0 = y0, year1 = y1)$cell_level
      res %>%
        mutate(year0 = y0, year1 = y1)
    }
  )
  
  summed_cell <- pieces %>%
    group_by(origem, destino) %>%
    summarise(
      actual_growth = sum(actual_growth, na.rm = TRUE),
      area_wide     = sum(area_wide, na.rm = TRUE),
      mix_effect    = sum(mix_effect, na.rm = TRUE),
      competitive   = sum(competitive, na.rm = TRUE),
      allocation    = sum(allocation, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    years = years,
    cell_level = summed_cell,
    by_destino = summed_cell %>%
      group_by(destino) %>%
      summarise(across(c(actual_growth, area_wide, mix_effect, competitive, allocation), sum), .groups = "drop"),
    by_origem = summed_cell %>%
      group_by(origem) %>%
      summarise(across(c(actual_growth, area_wide, mix_effect, competitive, allocation), sum), .groups = "drop"),
    total = summed_cell %>%
      summarise(across(c(actual_growth, area_wide, mix_effect, competitive, allocation), sum))
  )
}

# Execução ----
res_dyn <- shift_share_dynamic_em(bd, years = 2010:2019)

# Acessar resultados
res_dyn$total
res_dyn$by_destino
res_dyn$by_origem
res_dyn$cell_level

as.data.frame(res_dyn$by_destino) |> View()
as.data.frame(res_dyn$cell_level) |> View()

# Visualização dos resultados ----
## Gráfico completo ----
as.data.frame(res_dyn$by_destino) |> 
  pivot_longer(cols = c(actual_growth:allocation), values_to = "valor", names_to = "efeito") |> 
  mutate(efeito = factor(efeito, 
                         levels = c("actual_growth", "area_wide", "mix_effect", "competitive", "allocation"),
                         labels = c("Crescimento real","Area wide", "Region-mix", "Competitive", "Allocation"))) |> 
  ggplot(aes(x = destino, y = valor, fill = efeito)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Decomposição da evolução das chegadas internacionais \nem destinos concorrentes selecionados (2010-2019)",
       x = "Destino",
       y = "Chegadas (mil)",
       fill = "Efeito") +
  scale_x_discrete(labels = scales::label_wrap(10)) 
# +  scale_y_continuous(labels = fun_formatC_inteiros)



## Gráfico Brasil ----
as.data.frame(res_dyn$by_destino) |> 
  pivot_longer(cols = c(actual_growth:allocation), values_to = "valor", names_to = "efeito") |> 
  filter(destino == "Brazil") |> 
  mutate(efeito = factor(efeito, 
                         levels = c("actual_growth", "area_wide", "mix_effect", "competitive", "allocation"),
                         labels = c("Crescimento real","Area wide", "Region-mix", "Competitive", "Allocation"))) |> 
  ggplot(aes(x = efeito, y = valor)) + 
  geom_bar(stat = "identity", position = "dodge") +
  annotate("text", x = "Crescimento real", y = -500, colour = "blue",
           label="Crescimento real \nde 1,2 milhão de \nchegadas") +
  annotate("text", x = "Area wide", y = -500, colour = "blue",
           label="Crescimento esperado \nfoi maior do que \no real") +
  annotate("text", x = "Region-mix", y = -500, colour = "blue",
           label="Brasil com foco \nem mercados com \ncrescimento maior \ndo que a média") +
  annotate("text", x = "Competitive", y = 500, colour = "blue",
           label="Brasil não consegue \natrair chegadas no \nmesmo ritmo que \nconcorrentes") +
  annotate("text", x = "Allocation", y = 1000, colour = "blue",
           label="Brasil especializado \nem atrair chegadas \nde países em onde \nmaior competitividade") +
  labs(title = "Decomposição da evolução das chegadas internacionais \nao Brasil (2010-2019)",
       x = "Destino",
       y = "Chegadas (mil)") +
  scale_x_discrete(labels = scales::label_wrap(10)) 
# +  scale_y_continuous(labels = fun_formatC_inteiros)

as.data.frame(res_dyn$by_destino) |>
  pivot_longer(cols = c(actual_growth:allocation),
               values_to = "valor", names_to = "efeito") |>
  filter(destino == "Brazil") |>
  mutate(efeito = factor(efeito,
                         levels = c("actual_growth", "area_wide", "mix_effect", "competitive", "allocation"),
                         labels = c("Crescimento real","Area wide", "Region-mix", "Competitive", "Allocation")),
         alpha_barra = if_else(efeito == "Crescimento real", 1, 0.75)) |>
  ggplot(aes(x = efeito, y = valor)) +
  geom_col(aes(alpha = alpha_barra), color = NA) +
  scale_alpha_identity() +
  labs(title = "Decomposição da evolução das chegadas internacionais \nao Brasil (2010-2019)",
       x = "Destino",
       y = "Chegadas (mil)") +
  scale_x_discrete(labels = scales::label_wrap(10)) 
# +  scale_y_continuous(labels = fun_formatC_inteiros)

## Gráfico Brasil por mercado ----
as.data.frame(res_dyn$cell_level) |>
  pivot_longer(cols = c(actual_growth:allocation),
               values_to = "valor", names_to = "efeito") |>
  filter(destino == "Brazil") |>
  mutate(
    efeito = factor(
      efeito,
      levels = c("actual_growth", "area_wide", "mix_effect", "competitive", "allocation"),
      labels = c("Crescimento\nreal","Area wide", "Region-mix", "Competitive", "Allocation")
    ),
    alpha_barra = if_else(efeito == "Crescimento\nreal", 1, .6),
    origem = factor(origem)
  ) |>
  ggplot(aes(x = efeito, y = valor, fill = efeito)) +
  geom_col(aes(alpha = alpha_barra), width = 0.8) +
  scale_alpha_identity() +
  labs(
    title = "Decomposição da evolução das chegadas internacionais ao Brasil,\npor mercado emissor (2010-2019)",
    x = NULL,
    y = "Chegadas (mil)"
  ) +
  scale_x_discrete(labels = label_wrap(10)) +
  facet_wrap(~origem, scales = "free_y") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 9, lineheight = 0.95),
    plot.margin = margin(t = 5, r = 5, b = 18, l = 5)
  )
# +  scale_y_continuous(labels = fun_formatC_inteiros)

## Tabela Brasil ----
as.data.frame(res_dyn$cell_level) |> 
  filter(destino == "Brazil") |>
  dplyr::select(-destino) |> 
  gt(rowname_col = "origem") |>
  tab_stubhead(label = "Mercados") |> 
  fmt_number(decimals = 1, sep_mark = ".", dec_mark = ",") |> 
  tab_header(title = md("Decomposição da evolução das chegadas turísticas ao Brasil, por mercado (2010-2019)"))

# PENDENTE: replicar as tabelas normalmente apresentadas nos artigos.
# PENDENTE: Fazer a classificação em áreas especializadas ou não, competitivas ou não.

res_dyn$by_origem
res_dyn$cell_level





          
