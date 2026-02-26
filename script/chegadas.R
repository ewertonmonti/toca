library(readr)
library(dplyr)
library(stringr)
library(purrr)

# https://dados.gov.br/dados/conjuntos-dados/estimativas-de-chegadas-de-turistas-internacionais-ao-brasil

pasta <- "data/chegadas"

# 2009-2024 ----
# nomes-canônicos (os do chegadas_2024.csv)
canon <- c(
  "Continente","cod continente","País","cod pais","UF","cod uf",
  "Via","cod via","ano","Mês","cod mes","Chegadas"
)

# lista de arquivos 2010-2024
arquivos <- list.files(
  path = pasta,
  pattern = "^chegadas_(2009|201[0-9]|202[0-4])\\.csv$",
  full.names = TRUE
)

# lê cada arquivo e substitui os nomes das colunas pelos nomes-canônicos (na ordem)
ler_padrao <- function(f) {
  df <- read_csv2(f, show_col_types = FALSE, locale = locale(encoding = "ISO-8859-1")) |>
    rename_with(~ str_squish(.x))
  names(df) <- canon
  df
}

# data frame final com todos os anos empilhados
chegadas <- map_dfr(arquivos, ler_padrao)

chegadas <- janitor::clean_names(chegadas)


# 2025 ----
# Ler 2025 (arquivo com hífen e colunas diferentes)
df_2025 <- read_csv2(
  file.path(pasta, "chegadas-2025.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "ISO-8859-1"))

df_2025 <- janitor::clean_names(df_2025)

df_2025 <- df_2025 |> rename(
  via = via_de_acesso,
  pais = nome_pais_correto
)


# Incorporar ao dataframe já existente (chegadas_df)
chegadas <- bind_rows(chegadas, df_2025)

# Corrige divergências em variáveis
chegadas |> count(mes) |> print(n=Inf)
chegadas <- chegadas |> mutate(mes = str_to_lower(mes))
chegadas <- chegadas |> mutate(mes = if_else(mes == "março","marco",mes))

chegadas |> count(via) |> print(n=Inf)
chegadas <- chegadas |> mutate(via = if_else(via == "Aéreo","Aérea",via))
chegadas <- chegadas |> mutate(via = if_else(via == "Marítimo","Marítima",via))

chegadas |> count(continente) |> print(n=Inf)

# Retira as observações com valor 0 de chegadas, pra reduzir tamanho do df.
chegadas <- chegadas |> filter(chegadas > 0)

# Salvar o rds consolidado
saveRDS(chegadas, file.path(pasta, "chegadas.rds"))

# Explora ----
principais <- chegadas |> filter(ano == 2024) |> 
  summarise(chegadas = sum(chegadas), .by = pais) |> 
  slice_max(n = 20, order_by = chegadas) |> pull(pais)

bdc <- chegadas |> filter(pais %in% principais) |> summarise(chegadas = sum(chegadas), .by = c(pais, ano))
bdc |> group_by(pais) |> count(sort = TRUE)

# Gráfico do total das chegadas
bd |> filter(ano %in% 2010:2024) |> 
  mutate(destino = fct_reorder(destino, if_else(ano == 2024, chegadas, NA_real_),
                               .fun = max, .na_rm = TRUE)) |> 
  ggplot(aes(x = ano, y = chegadas, fill = destino)) +
  geom_area() +
  labs(
    title = "Total de chegadas turísticas internacionais, por ano",
    y = "Chegadas (mil)",
    x = "Ano") + 
  scale_x_continuous(breaks = seq(2010, 2024, 2),
                     minor_breaks = seq(2010, 2024, 1))


