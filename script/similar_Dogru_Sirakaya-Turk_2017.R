
# Configurações iniciais ----

rm(list = ls(all = TRUE))
options(scipen = 999)

pkgs <- c("tidyverse", "magrittr", "purrr", "fixest", "stringr", "readxl", "WDI", "stringdist")
lapply(pkgs, library, character.only = TRUE)


# Parâmetros ----

concorrentes_diretos <- c(
  "Brazil",
  "Peru",
  "Dominican Republic",
  "Colombia",
  "Spain",
  "Chile",
  "Costa Rica",
  "Jamaica",
  "Cuba",
  "Argentina",
  "Mexico"
)

ano_inicio <- 2003
ano_fim    <- 2024
ano_start  <- ano_inicio - 1


# Chegadas ----

path_chegadas <- "data/UN_Tourism_inbound_arrivals_10_2025.xlsx"
bd_raw_chegadas <- readxl::read_excel(
  path_chegadas,
  sheet = "Data",
  col_types = c("text","text","text","numeric","text","numeric",
                "text","numeric","numeric","text","text","text",
                "text","text")
) |>
  janitor::clean_names()

bd_chegadas <- bd_raw_chegadas |>
  filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR") |>
  summarise(chegadas = sum(value), .by = c(reporter_area_label, year)) |>
  rename(pais = reporter_area_label, ano = year)


# Receitas ----

path_receitas <- "data/UN_Tourism_inbound_expenditure_10_2025.xlsx"
bd_raw_receitas <- readxl::read_excel(
  path_receitas,
  sheet = "Data",
  col_types = c("text","text","text","numeric","text","numeric",
                "text","numeric","numeric","text","text","text",
                "text","text")
) |>
  janitor::clean_names()

bd_receitas <- bd_raw_receitas |>
  filter(indicator_code == "INBD_EXPD_BPAY_TRVL_VSTR") |>
  summarise(receitas = sum(value), .by = c(reporter_area_label, year)) |>
  rename(pais = reporter_area_label, ano = year)

bd <- left_join(bd_chegadas, bd_receitas, by = join_by(pais, ano))


# PIB (WDI) ----

indicador_crescimento <- "NY.GDP.MKTP.KD.ZG"  # GDP growth (annual %)
indicador_nivel       <- "NY.GDP.MKTP.CD"     # GDP (current US$)

paises_interesse_nomes <- unique(c("World", "Brazil", concorrentes_diretos))

wdi_countries <- as_tibble(WDI_data$country) %>%
  transmute(
    wdi_country = country,
    iso2c = iso2c,
    norm = str_to_lower(str_squish(country)) %>%
      str_replace_all("[[:punct:]]+", " ") %>%
      str_replace_all("\\s+", " ")
  )

normalize_name <- function(x) {
  str_to_lower(str_squish(x)) %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_replace_all("\\s+", " ")
}

build_wdi_mapping <- function(nomes, top_n = 7) {
  nomes_tbl <- tibble(input_name = nomes) %>%
    mutate(
      norm = normalize_name(input_name),
      iso2c = if_else(norm == "world", "1W", NA_character_)
    )
  
  exact <- nomes_tbl %>%
    left_join(wdi_countries, by = "norm") %>%
    mutate(
      iso2c = coalesce(iso2c.x, iso2c.y),
      wdi_country = if_else(norm == "world", "World", wdi_country),
      status = case_when(
        norm == "world" ~ "ok_exact",
        !is.na(iso2c)   ~ "ok_exact",
        TRUE            ~ "not_found"
      )
    ) %>%
    select(input_name, wdi_country, iso2c, status, norm)
  
  not_found <- exact %>% filter(status == "not_found")
  if (nrow(not_found) == 0) {
    return(exact %>% select(-norm) %>% mutate(suggestions = NA_character_))
  }
  
  suggest_one <- function(nm_norm) {
    d <- stringdist::stringdist(nm_norm, wdi_countries$norm, method = "jw")
    idx <- order(d)[1:top_n]
    paste0(wdi_countries$wdi_country[idx], " (", wdi_countries$iso2c[idx], ")", collapse = " | ")
  }
  
  suggestions <- not_found %>%
    mutate(suggestions = map_chr(norm, suggest_one)) %>%
    select(input_name, suggestions)
  
  exact %>%
    left_join(suggestions, by = "input_name") %>%
    select(-norm)
}

map_paises <- build_wdi_mapping(paises_interesse_nomes, top_n = 7)

nao_mapeados <- map_paises %>% filter(status == "not_found")
if (nrow(nao_mapeados) > 0) {
  message("Atenção: alguns países não foram mapeados no WDI. Sugestões:")
  print(nao_mapeados %>% select(input_name, suggestions))
}

paises_interesse_iso2 <- map_paises %>%
  filter(status != "not_found") %>%
  pull(iso2c) %>%
  unique()

dados_pib_raw <- WDI(
  country = paises_interesse_iso2,
  indicator = c(gdp_growth_pct = indicador_crescimento,
                gdp_level_usd  = indicador_nivel),
  start = ano_inicio - 1,
  end   = ano_fim
) %>%
  as_tibble() %>%
  transmute(
    iso2c = iso2c,
    pais_wdi = country,
    ano = as.integer(year),
    gdp_growth_pct = as.numeric(gdp_growth_pct),
    gdp_level_usd  = as.numeric(gdp_level_usd),
    gdp_growth = gdp_growth_pct / 100
  ) %>%
  arrange(iso2c, ano)

dados_pib <- dados_pib_raw %>%
  group_by(iso2c) %>%
  arrange(ano) %>%
  mutate(
    gdp_growth = if_else(ano == min(ano), 0, gdp_growth),
    pib_index  = purrr::accumulate(1 + gdp_growth, `*`, .init = 1)[-1]
  ) %>%
  ungroup()

# (a) PIB mundial em taxa (decimal)
pib <- dados_pib %>%
  filter(iso2c == "1W", ano >= ano_inicio - 1, ano <= ano_fim) %>%
  select(ano, g_world_gdp = gdp_growth)

# (b) Mundo com nível + índice
pib_world_extra <- dados_pib %>%
  filter(iso2c == "1W", ano >= ano_inicio - 1, ano <= ano_fim) %>%
  select(ano,
         g_world_gdp = gdp_growth,
         pib_world_level = gdp_level_usd,
         pib_world_index = pib_index)

# (c) PIB por país (IMPORTANTE: nome = input_name do seu bd, via map_paises)
pib_paises <- dados_pib %>%
  filter(iso2c != "1W", ano >= ano_inicio - 1, ano <= ano_fim) %>%
  left_join(
    map_paises %>%
      filter(status != "not_found") %>%
      distinct(iso2c, .keep_all = TRUE) %>%
      transmute(iso2c, pais = input_name),
    by = "iso2c"
  ) %>%
  select(pais, iso2c, ano, gdp_growth, gdp_level_usd, gdp_index = pib_index)

stopifnot(all((ano_start:ano_fim) %in% pib$ano))


# Função (revisada) ----

run_shift_share_turismo <- function(
    bd,
    pib,
    pib_world_extra,
    pib_paises = NULL,
    concorrentes_diretos,
    variavel = c("chegadas", "receitas"),
    ano_inicio = 2003,
    ano_fim    = 2024,
    pais_referencia = "Brazil",
    regression_sample = c("targets", "all"),
    vcov_reg = c("cluster_pais", "hetero"),
    verbose = TRUE
) {
  
  variavel <- match.arg(variavel)
  regression_sample <- match.arg(regression_sample)
  vcov_reg <- match.arg(vcov_reg)
  
  safe_div <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, num / den)
  
  # remover duplicata do Brasil no vetor de concorrentes (se tiver)
  concorrentes_diretos <- setdiff(unique(concorrentes_diretos), pais_referencia)
  
  var_col <- if (variavel == "chegadas") "chegadas" else "receitas"
  min_year <- ano_inicio - 1
  
  bd0 <- bd %>%
    transmute(
      pais  = as.character(pais),
      ano   = as.integer(ano),
      valor = as.numeric(.data[[var_col]])
    ) %>%
    filter(ano >= min_year, ano <= ano_fim)
  
  lista_paises <- unique(c(pais_referencia, concorrentes_diretos))
  
  mundo <- bd0 %>%
    group_by(ano) %>%
    summarise(mundo_volume = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(ano) %>%
    mutate(g_mundo = safe_div(mundo_volume, lag(mundo_volume)) - 1)
  
  grupo <- bd0 %>%
    filter(pais %in% lista_paises) %>%
    group_by(ano) %>%
    summarise(grupo_volume = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(ano) %>%
    mutate(g_grupo = safe_div(grupo_volume, lag(grupo_volume)) - 1)
  
  dados_ind <- bd0 %>% filter(pais %in% lista_paises)
  
  dados_concorrentes_agg <- dados_ind %>%
    filter(pais != pais_referencia) %>%
    group_by(ano) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(pais = "Concorrentes")
  
  dados_todos <- bind_rows(dados_ind, dados_concorrentes_agg)
  targets <- unique(c(pais_referencia, concorrentes_diretos, "Concorrentes"))
  
  # PIB mundial em taxa (já vem em pib como g_world_gdp decimal)
  pib0 <- pib %>%
    transmute(ano = as.integer(ano), g_world_gdp = as.numeric(g_world_gdp)) %>%
    filter(ano >= min_year, ano <= ano_fim) %>%
    arrange(ano)
  
  pib_idx <- tibble(ano = min_year:ano_fim) %>%
    left_join(pib0, by = "ano") %>%
    arrange(ano) %>%
    mutate(
      g_world_gdp = if_else(ano == min_year, 0, g_world_gdp),
      pib_world_index = purrr::accumulate(1 + g_world_gdp, `*`, .init = 1)[-1]
    )
  
  # SSA dinâmico
  df_dyn <- dados_todos %>%
    arrange(pais, ano) %>%
    group_by(pais) %>%
    mutate(
      lag_valor = lag(valor),
      g_pais = safe_div(valor, lag_valor) - 1
    ) %>%
    ungroup() %>%
    left_join(mundo, by = "ano") %>%
    left_join(grupo, by = "ano") %>%
    left_join(pib_idx %>% select(ano, g_world_gdp), by = "ano") %>%
    mutate(
      GGE = lag_valor * g_world_gdp,
      GIME_Mundo = lag_valor * (g_mundo - g_world_gdp),
      GCSE_Mundo = lag_valor * (g_pais - g_mundo),
      GIME_Grupo = lag_valor * (g_grupo - g_world_gdp),
      GCSE_Grupo = lag_valor * (g_pais - g_grupo),
      Total_Change = valor - lag_valor,
      share_mundo_atual = (valor / mundo_volume) * 100,
      share_mundo_anterior = (lag_valor / lag(mundo_volume)) * 100,
      var_share_pp = share_mundo_atual - share_mundo_anterior
    ) %>%
    filter(ano >= ano_inicio, ano <= ano_fim, pais %in% targets)
  
  serie_brasil <- df_dyn %>%
    filter(pais == pais_referencia) %>%
    select(
      ano, valor,
      share_mundo_atual, var_share_pp,
      g_world_gdp, g_mundo, g_grupo, g_pais,
      GGE, GIME_Mundo, GCSE_Mundo, GIME_Grupo, GCSE_Grupo
    ) %>%
    rename(
      brazil_volume = valor,
      share_global_pct = share_mundo_atual,
      g_mundo_turismo = g_mundo,
      g_grupo_turismo = g_grupo,
      g_brazil = g_pais
    )
  
  resumo_dinamico <- df_dyn %>%
    group_by(pais) %>%
    summarise(
      variavel = variavel,
      ano_inicio = ano_inicio,
      ano_fim = ano_fim,
      Volume_Inicial = first(lag_valor),
      Volume_Final   = last(valor),
      Variacao_Total = sum(Total_Change, na.rm = TRUE),
      Share_Global_Inicial = first(share_mundo_anterior),
      Share_Global_Final   = last(share_mundo_atual),
      Variacao_Share_PP    = Share_Global_Final - Share_Global_Inicial,
      Total_GGE = sum(GGE, na.rm = TRUE),
      Total_GIME_Mundo = sum(GIME_Mundo, na.rm = TRUE),
      Total_GCSE_Mundo = sum(GCSE_Mundo, na.rm = TRUE),
      Total_GIME_Grupo = sum(GIME_Grupo, na.rm = TRUE),
      Total_GCSE_Grupo = sum(GCSE_Grupo, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # 6) Shift-Share Regression (REVISADA)
  
  if (!all(c("ano","pib_world_level") %in% names(pib_world_extra))) {
    stop("pib_world_extra precisa ter colunas: ano e pib_world_level.")
  }
  
  # base da regressão
  if (regression_sample == "targets") {
    reg0 <- df_dyn %>%
      select(pais, ano, valor, g_pais) %>%
      rename(g = g_pais)
  } else {
    reg0 <- bd0 %>%
      arrange(pais, ano) %>%
      group_by(pais) %>%
      mutate(g = safe_div(valor, lag(valor)) - 1) %>%
      ungroup() %>%
      filter(ano >= ano_inicio, ano <= ano_fim) %>%
      select(pais, ano, valor, g)
  }
  
  reg0 <- reg0 %>%
    left_join(mundo %>% select(ano, mundo_volume), by = "ano") %>%
    left_join(pib_world_extra %>% transmute(ano = as.integer(ano),
                                            pib_world_level = as.numeric(pib_world_level)),
              by = "ano") %>%
    mutate(
      w1 = valor / pib_world_level,
      w2 = valor / mundo_volume
    ) %>%
    filter(is.finite(g), is.finite(w1), is.finite(w2)) %>%
    mutate(pais = droplevels(as.factor(pais)))
  
  diag <- reg0 %>%
    group_by(ano) %>%
    summarise(n_paises = n_distinct(pais), .groups = "drop")
  if (any(diag$n_paises < 2)) {
    stop("Regressão não identificada com FE de ano: há ano(s) com < 2 países na amostra. ",
         "Isso costuma ser mismatch de nomes em bd$pais.")
  }
  
  reg_base12 <- reg0 %>%
    mutate(
      w1s = w1 * 1e9,
      w2s = w2 * 1e6
    )
  
  vcov_arg <- if (vcov_reg == "cluster_pais") {
    ~pais 
  } else {
    "hetero"
  }
  
  m_w1 <- fixest::feols(g ~ 0 + i(pais, w1s) | ano, data = reg_base12, vcov = vcov_arg)
  m_w2 <- fixest::feols(g ~ 0 + i(pais, w2s) | ano, data = reg_base12, vcov = vcov_arg)
  
  # W3 em base separada (não afeta W1/W2)
  m_w3 <- NULL
  reg_base3 <- NULL
  
  if (!is.null(pib_paises)) {
    if (!all(c("pais","ano","gdp_level_usd") %in% names(pib_paises))) {
      stop("pib_paises precisa ter colunas: pais, ano, gdp_level_usd.")
    }
    
    reg_base3 <- reg0 %>%
      left_join(pib_paises %>% transmute(pais = as.character(pais),
                                         ano = as.integer(ano),
                                         gdp_level_usd = as.numeric(gdp_level_usd)),
                by = c("pais","ano")) %>%
      mutate(
        w3  = (valor / gdp_level_usd) - (mundo_volume / pib_world_level),
        w3s = w3 * 1e6
      ) %>%
      filter(is.finite(w3s))
    
    if (n_distinct(reg_base3$pais) >= 2) {
      m_w3 <- fixest::feols(g ~ 0 + i(pais, w3s) | ano, data = reg_base3, vcov = vcov_arg)
    } else if (verbose) {
      message("W3 não estimado: após o join do PIB por país ficou <2 países com dados válidos.")
    }
  } else if (verbose) {
    message("Sem pib_paises: W3 (especialização) não será estimado.")
  }
  
  # Pós-processamento robusto (evita NaN em sqrt)
  slopes_and_diffs_vs_ref <- function(model, weight_var, ref_country = "Brazil") {
    
    b  <- stats::coef(model)
    V  <- stats::vcov(model)
    df <- stats::df.residual(model)
    
    dV <- diag(V)
    if (is.null(names(dV))) names(dV) <- colnames(V)
    
    pat   <- paste0("^pais::.+:", weight_var, "$")
    terms <- names(b)[grepl(pat, names(b))]
    
    parse_country <- function(term) sub(paste0(":", weight_var, "$"), "", sub("^pais::", "", term))
    
    tbl <- tibble::tibble(
      term = terms,
      pais = vapply(terms, parse_country, character(1)),
      slope = unname(b[terms]),
      se_slope = sqrt(pmax(unname(dV[terms]), 0))
    ) %>%
      dplyr::mutate(
        t_slope = slope / se_slope,
        p_slope = 2 * stats::pt(abs(t_slope), df = df, lower.tail = FALSE)
      )
    
    ref_term <- tbl$term[tbl$pais == ref_country][1]
    if (is.na(ref_term)) stop("País de referência ('", ref_country, "') não aparece no modelo.")
    
    ref_slope <- unname(b[ref_term])
    ref_var   <- unname(V[ref_term, ref_term])
    
    # Vetores 1-a-1 (um elemento por termo)
    var_term      <- unname(dV[terms])
    cov_term_ref  <- as.numeric(V[terms, ref_term, drop = TRUE])
    
    var_diff <- pmax(var_term + ref_var - 2 * cov_term_ref, 0)
    se_diff  <- sqrt(var_diff)
    
    tbl %>%
      dplyr::mutate(
        diff_vs_ref = slope - ref_slope,
        se_diff = se_diff,
        t_diff  = diff_vs_ref / se_diff,
        p_diff  = 2 * stats::pt(abs(t_diff), df = df, lower.tail = FALSE)
      ) %>%
      dplyr::select(pais, slope, se_slope, p_slope, diff_vs_ref, se_diff, p_diff)
  }
  
  
  coef_totais_vs_brasil <- bind_rows(
    slopes_and_diffs_vs_ref(m_w1, "w1s", ref_country = pais_referencia) %>%
      mutate(modelo = "W1 (valor/PIB mundial)"),
    slopes_and_diffs_vs_ref(m_w2, "w2s", ref_country = pais_referencia) %>%
      mutate(modelo = "W2 (valor/total mundial)"),
    if (!is.null(m_w3)) slopes_and_diffs_vs_ref(m_w3, "w3s", ref_country = pais_referencia) %>%
      mutate(modelo = "W3 (especialização)") else NULL
  ) %>%
    select(modelo, pais, slope, se_slope, p_slope, diff_vs_ref, se_diff, p_diff) %>%
    arrange(modelo, desc(slope))
  
  tidy_fixest <- function(model, label) {
    b <- stats::coef(model)
    V <- stats::vcov(model)
    dV <- diag(V)
    if (is.null(names(dV))) names(dV) <- colnames(V)
    se <- sqrt(pmax(dV[names(b)], 0))
    t  <- b / se
    df <- stats::df.residual(model)
    p  <- 2 * stats::pt(abs(t), df = df, lower.tail = FALSE)
    tibble(modelo = label, term = names(b), estimate = unname(b),
           std_error = unname(se), t = unname(t), p_value = unname(p))
  }
  
  reg_results <- bind_rows(
    tidy_fixest(m_w1, "W1"),
    tidy_fixest(m_w2, "W2"),
    if (!is.null(m_w3)) tidy_fixest(m_w3, "W3") else NULL
  )
  
  # retorno
  list(
    meta = list(
      variavel = variavel, ano_inicio = ano_inicio, ano_fim = ano_fim,
      referencia = pais_referencia, concorrentes_diretos = concorrentes_diretos,
      vcov_reg = vcov_reg, regression_sample = regression_sample
    ),
    benchmarks = list(mundo = mundo, grupo = grupo, pib_taxa = pib0, pib_index = pib_idx),
    dinamico = list(by_year = df_dyn, serie_brasil = serie_brasil, resumo = resumo_dinamico),
    estatico = NULL,
    regressao = list(
      data_W1W2 = reg_base12,
      data_W3   = reg_base3,
      modelos = list(W1 = m_w1, W2 = m_w2, W3 = m_w3),
      resultados = reg_results,
      coef_totais_vs_brasil = coef_totais_vs_brasil
    )
  )
}


# Executa a função ----

setdiff(concorrentes_diretos, unique(bd$pais))
bd |> filter(ano %in% 2002:2024, pais %in% unique(concorrentes_diretos)) |> count(pais)

res <- run_shift_share_turismo(
  bd = bd,
  pib = pib,
  pib_world_extra = pib_world_extra,
  pib_paises = pib_paises,
  concorrentes_diretos = concorrentes_diretos,
  variavel = "chegadas",
  ano_inicio = ano_inicio,
  ano_fim = ano_fim,
  pais_referencia = "Brazil",
  regression_sample = "targets",   # ou "all"
  vcov_reg = "cluster_pais"        # ou "hetero" para reduzir PSD warning
)

res$dinamico$resumo
View(res$dinamico$resumo)

res$dinamico$serie_brasil
View(res$dinamico$serie_brasil)

res$regressao$coef_totais_vs_brasil
View(res$regressao$coef_totais_vs_brasil)

res$regressao$resultados
View(res$regressao$resultados)
