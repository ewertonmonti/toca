# Configurações iniciais ----
rm(list = ls(all=T))
# library(readxl)
# library(dplyr)
# library(purrr)
# library(stringr)
# library(ggplot2)
library(tidyverse)
library(magrittr)

# Importa turistas ----
# Pasta com os arquivos
dir_path <- "data/un_tourism_tourists"

# Lista de .xlsx (evita arquivos temporários tipo "~$")
files <- list.files(
  path = dir_path,
  pattern = "\\.xlsx$",
  full.names = TRUE
) |>
  (\(x) x[!str_detect(basename(x), "^~\\$")])()

if (length(files) == 0) stop("Nenhum .xlsx encontrado em: ", dir_path)

# Colunas esperadas (na ordem)
expected_cols <- c(
  "reporter_area_code",
  "reporter_area_label",
  "indicator_code",
  "indicator_label",
  "indicator_previous_code",
  "partner_area_code",
  "partner_area_label",
  "year",
  "value",
  "flag",
  "flag_label",
  "unit",
  "notes"
)

# Tipos esperados (readxl)
col_types <- c(
  "numeric",  # reporter_area_code
  "text",     # reporter_area_label
  "text",     # indicator_code
  "text",     # indicator_label
  "text",     # indicator_previous_code
  "numeric",  # partner_area_code
  "text",     # partner_area_label
  "numeric",  # year
  "numeric",  # value
  "text",     # flag
  "text",     # flag_label
  "text",     # unit
  "text"      # notes
)

read_one <- function(f) {
  df <- read_excel(
    path = f,
    sheet = 2,
    col_types = col_types,
    .name_repair = "minimal"
  )
  
  # Garante exatamente os nomes esperados (se o arquivo vier com variações)
  # e mantém apenas as colunas esperadas (se vier algo extra)
  df <- df |>
    rename_with(~ expected_cols[seq_along(.)]) |>
    select(all_of(expected_cols))
  
  df
}

# Empilha tudo em um único data frame
bdt <- map_dfr(files, read_one)

# Checagens rápidas
nrow(bdt)
anyNA(bdt$year)
str(bdt)


# Importa visitantes ----
# Pasta com os arquivos
dir_path <- "data/un_tourism_visitors"

# Lista de .xlsx (evita arquivos temporários tipo "~$")
files <- list.files(
  path = dir_path,
  pattern = "\\.xlsx$",
  full.names = TRUE
) |>
  (\(x) x[!str_detect(basename(x), "^~\\$")])()

if (length(files) == 0) stop("Nenhum .xlsx encontrado em: ", dir_path)

# Colunas esperadas (na ordem)
expected_cols <- c(
  "reporter_area_code",
  "reporter_area_label",
  "indicator_code",
  "indicator_label",
  "indicator_previous_code",
  "partner_area_code",
  "partner_area_label",
  "year",
  "value",
  "flag",
  "flag_label",
  "unit",
  "notes"
)

# Tipos esperados (readxl)
col_types <- c(
  "numeric",  # reporter_area_code
  "text",     # reporter_area_label
  "text",     # indicator_code
  "text",     # indicator_label
  "text",     # indicator_previous_code
  "numeric",  # partner_area_code
  "text",     # partner_area_label
  "numeric",  # year
  "numeric",  # value
  "text",     # flag
  "text",     # flag_label
  "text",     # unit
  "text"      # notes
)

read_one <- function(f) {
  df <- read_excel(
    path = f,
    sheet = 2,
    col_types = col_types,
    .name_repair = "minimal"
  )
  
  # Garante exatamente os nomes esperados (se o arquivo vier com variações)
  # e mantém apenas as colunas esperadas (se vier algo extra)
  df <- df |>
    rename_with(~ expected_cols[seq_along(.)]) |>
    select(all_of(expected_cols))
  
  df
}

# Empilha tudo em um único data frame
bdv <- map_dfr(files, read_one)

# Checagens rápidas
nrow(bdv)
anyNA(bdv$year)
str(bdv)

# Junta turistas e visitantes ----
bd <- bind_rows(bdt, bdv)
rm(bdt)
rm(bdv)
rm(read_one)
saveRDS(bd, "data/untourism_tourists_visitors.rds")

# Exploração inicial ----
head(bd)

# Tipo turista x visitante
bd |> count(indicator_code, indicator_label)
bd %<>% mutate(tipo = ifelse(indicator_code == "INBD_TRIP_AREA_TOUR", "turistas", "visitantes"))
bd |> count(tipo)
bd |> group_by(tipo) |> count(year) |> 
  ggplot(aes(x = year, y = n)) + geom_bar(stat = "identity") + facet_wrap(~tipo)

# Destino
bd |> group_by(tipo) |> count(reporter_area_label) |> arrange(desc(n)) |> 
  pivot_wider(id_cols=reporter_area_label, values_from = n, names_from = tipo) |> 
  arrange(desc(turistas))

# Destinos só com visitantes
bd |> group_by(tipo) |> count(reporter_area_label) |> arrange(desc(n)) |> 
  pivot_wider(id_cols=reporter_area_label, values_from = n, names_from = tipo) |> 
  filter(is.na(turistas))

# Destinos só com turistas
bd |> group_by(tipo) |> count(reporter_area_label) |> arrange(desc(n)) |> 
  pivot_wider(id_cols=reporter_area_label, values_from = n, names_from = tipo) |> 
  filter(is.na(visitantes))


# Origem
bd |> group_by(tipo) |> count(partner_area_label) |> arrange(desc(n)) |> 
  pivot_wider(id_cols=partner_area_label, values_from = n, names_from = tipo) |> 
  arrange(desc(turistas))

# Origem só com visitantes
bd |> group_by(tipo) |> count(partner_area_label) |> arrange(desc(n)) |> 
  pivot_wider(id_cols=partner_area_label, values_from = n, names_from = tipo) |> 
  filter(is.na(turistas))

# Origem só com turistas
bd |> group_by(tipo) |> count(partner_area_label) |> arrange(desc(n)) |> 
  pivot_wider(id_cols=partner_area_label, values_from = n, names_from = tipo) |> 
  filter(is.na(visitantes))


# Destinos que estão com dados em unidade errada
conta_decimais_por_destino <- function(df,
                                       grupo_col = "reporter_area_label",
                                       value_col = "value",
                                       tol = sqrt(.Machine$double.eps)) {
  
  has_decimal_count <- function(x) {
    x <- x[!is.na(x)]
    n_total <- length(x)
    if (n_total == 0) return(c(n_total = 0, n_decimais = 0, pct_decimais = NA_real_))
    
    n_dec <- sum(abs(x - round(x)) > tol)
    c(n_total = n_total,
      n_decimais = n_dec,
      pct_decimais = 100 * n_dec / n_total)
  }
  
  sp <- split(df[[value_col]], df[[grupo_col]])
  tmp <- do.call(rbind, lapply(sp, has_decimal_count))
  
  data.frame(
    reporter_area_label = names(sp),
    as.data.frame(tmp, row.names = NULL),
    row.names = NULL
  )
}


res <- conta_decimais_por_destino(bd)
res
subset(res, pct_decimais < 1)

bdt <-  bd |> filter(tipo == "turistas")
rest <- conta_decimais_por_destino(bdt)
subset(rest, pct_decimais < 1)

bdv <-  bd |> filter(tipo == "visitantes")
resv <- conta_decimais_por_destino(bdv)
subset(resv, pct_decimais < 1)

regioes <- seq(10000, 70000, 10000)
# Pakistan
bd |> filter(reporter_area_label == "Pakistan", partner_area_code < 1000) |> summarise(sum(value), .by=c(year, tipo))

# Finland
bd |> filter(reporter_area_label == "Finland", partner_area_code < 1000) |> 
  summarise(sum(value), .by=c(year, tipo)) |> 
  arrange(tipo,year)

# Finland
bd |> filter(reporter_area_label == "Ireland", partner_area_code < 1000) |> 
  summarise(sum(value), .by=c(year, tipo)) |> 
  arrange(tipo,year) |> print(n=Inf)

# Oman
bd |> filter(reporter_area_label == "Oman", partner_area_code < 1000) |> 
  summarise(sum(value), .by=c(year, tipo)) |> 
  arrange(tipo,year) |> print(n=Inf)

# Paraguay
bd |> filter(reporter_area_label == "Paraguay", partner_area_code < 1000) |> 
  summarise(sum(value), .by=c(year, tipo)) |> 
  arrange(tipo,year)

# apliquei a correção para o Paraguai (x/1000) para todos os anos
bd <- bd |> mutate(value = ifelse(reporter_area_label == "Paraguay", value/1000, value))

saveRDS(bd, "data/untourism_tourists_visitors.rds")


# bd |> count(partner_area_label) |> arrange(desc(n))
# 
# bd |> filter(!partner_area_code %in% regioes) |> count(partner_area_label) |> arrange(desc(n))
# 
# bd2 <- bd |> filter(!partner_area_code %in% regioes) |> dplyr::select(reporter_area_label, partner_area_label, year, value)
# 
# 
# 
# 
# 
# saveRDS(bd2, "data/bd_untourism_tourists.rds")

