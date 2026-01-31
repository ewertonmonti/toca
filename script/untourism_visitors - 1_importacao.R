# Pacotes
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

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
bd <- map_dfr(files, read_one)

# (Opcional) checagens rápidas
nrow(bd)
str(bd)
anyNA(bd$year)


# Exploração inicial ----
View(head(bd))
bd |> count(reporter_area_label) |> arrange(desc(n))
bd |> count(year) |> ggplot(aes(x = year, y = n)) + geom_bar(stat = "identity")
bd |> count(partner_area_label) |> arrange(desc(n))
regioes <- seq(10000, 70000, 10000)
bd |> filter(!partner_area_code %in% regioes) |> count(partner_area_label) |> arrange(desc(n))

bd2 <- bd |> filter(!partner_area_code %in% regioes) |> dplyr::select(reporter_area_label, partner_area_label, year, value)

saveRDS(bd2, "data/bd_untourism_visitors.rds")