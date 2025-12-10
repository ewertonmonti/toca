# ------------------------------------------------------------
# Shift-share (turismo internacional) com filtro de "World"
# e exportação de gráficos via ggsave
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(janitor)
})

# ===== 1) Ler dados e preparar =====
path <- "UN_Tourism_inbound_arrivals_by_region_10_2025.xlsx"
sheet_name <- "Data"

raw <- read_excel(path, sheet = sheet_name) %>% clean_names()

# Excluir linhas em que partner_area_label == "World" (coluna G)
raw_f <- raw %>% filter(is.na(partner_area_label) | partner_area_label != "World")

# Agregar por país-ano (value está em mil viagens)
df_pa <- raw_f %>%
  transmute(
    pais = reporter_area_label,
    ano  = as.integer(year),
    turistas = as.numeric(value)
  ) %>%
  tidyr::drop_na(pais, ano, turistas) %>%
  group_by(pais, ano) %>%
  summarise(turistas = sum(turistas), .groups = "drop")

# ===== 2) Shift-share dinâmico (2000–2024) =====
calc_shiftshare_dinamico <- function(data, from_year = 2000, to_year = 2024) {
  base <- data %>%
    filter(ano >= from_year, ano <= to_year) %>%
    arrange(pais, ano) %>%
    group_by(pais) %>%
    mutate(
      turistas_lag = dplyr::lag(turistas),
      g_i = dplyr::if_else(turistas_lag > 0, (turistas - turistas_lag) / turistas_lag, NA_real_)
    ) %>%
    ungroup() %>%
    tidyr::drop_na(g_i)
  
  g_global_por_ano <- base %>%
    group_by(ano) %>%
    summarise(
      g_global = sum(turistas_lag * g_i, na.rm = TRUE) / sum(turistas_lag, na.rm = TRUE),
      .groups = "drop"
    )
  
  din <- base %>%
    left_join(g_global_por_ano, by = "ano") %>%
    mutate(
      efeito_estrutural  = turistas_lag * g_global,
      efeito_competitivo = turistas_lag * (g_i - g_global)
    )
  
  acum <- din %>%
    group_by(pais) %>%
    summarise(
      turistas_inicial = first(turistas_lag),
      turistas_final   = last(turistas),
      efeito_estrutural_total  = sum(efeito_estrutural,  na.rm = TRUE),
      efeito_competitivo_total = sum(efeito_competitivo, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      variacao_total = turistas_final - turistas_inicial
    ) %>%
    arrange(desc(efeito_competitivo_total)) %>%
    mutate(rank_efeito_competitivo = dplyr::row_number())
  
  list(
    painel = din,
    resumo = acum,
    g_global_ano = g_global_por_ano
  )
}

din_res <- calc_shiftshare_dinamico(df_pa, 2000, 2024)

# ===== 3) Gráficos =====

# Top-N por efeito competitivo acumulado
plot_topN <- function(resumo, N = 15) {
  topN <- resumo %>% slice_head(n = N) %>% arrange(efeito_competitivo_total)
  ggplot(topN, aes(x = reorder(pais, efeito_competitivo_total), y = efeito_competitivo_total)) +
    geom_col() +
    coord_flip() +
    labs(
      x = "País",
      y = "Efeito competitivo total (mil viagens)",
      title = sprintf("Turismo receptivo – Efeito competitivo acumulado (2000–2024) – Top %d", N)
    )
}

# Brasil: efeito competitivo anual
plot_brasil_comp_ano <- function(painel) {
  br <- painel %>% filter(tolower(pais) == "brazil")
  ggplot(br, aes(x = ano, y = efeito_competitivo)) +
    geom_col() +
    labs(
      x = "Ano",
      y = "Efeito competitivo (mil viagens)",
      title = "Brasil – Efeito competitivo anual (shift-share dinâmico)"
    )
}

# Crescimento global por ano
plot_g_global <- function(g_global_ano) {
  ggplot(g_global_ano, aes(x = ano, y = g_global)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Ano",
      y = "Crescimento global ponderado (g_global)",
      title = "Crescimento global anual do turismo (g_global, 2001–2024)"
    )
}

p_top15 <- plot_topN(din_res$resumo, N = 15)
p_br    <- plot_brasil_comp_ano(din_res$painel)
p_g     <- plot_g_global(din_res$g_global_ano)

# ===== 4) Salvar PNGs com ggsave =====
dir.create("fig", showWarnings = FALSE)

ggsave("fig/fig_top15_efeito_competitivo_2000_2024.png",
       plot = p_top15, width = 10, height = 7, dpi = 300, units = "in")

ggsave("fig/fig_brasil_efeito_comp_anual_2000_2024.png",
       plot = p_br, width = 10, height = 7, dpi = 300, units = "in")

ggsave("fig/fig_g_global_anual_2001_2024.png",
       plot = p_g, width = 10, height = 6, dpi = 300, units = "in")

# ===== 5) (Opcional) Exportar tabelas como CSV =====
write.csv(din_res$resumo,      "shiftshare_turismo_dinamico_2000_2024.csv", row.names = FALSE)
write.csv(din_res$g_global_ano,"g_global_por_ano_2000_2024.csv",            row.names = FALSE)
write.csv(din_res$painel,      "shiftshare_painel_ano_a_ano_2000_2024.csv", row.names = FALSE)
