# =========================================================
# Estimativa de emissões aéreas entre aeroportos (footprint)
# =========================================================

# Instale os pacotes se necessário:
# install.packages(c("dplyr", "tidyr", "purrr", "writexl", "remotes"))
# remotes::install_github("acircleda/footprint")

library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(footprint)

# -----------------------------
# 1) Definir aeroportos exemplo
# -----------------------------
# 3 aeroportos dos EUA (origens)
origens <- c("JFK", "MIA", "LAX")

# 3 aeroportos do Brasil (destinos)
destinos <- c("GRU", "GIG", "BSB")

# Todas as classes tarifárias aceitas pela função
tarifas <- c("Unknown", "Economy", "Economy+", "Business", "First")

# Ano e métrica de saída
ano_calculo <- 2024
metrica <- "co2e"   # kg de CO2e

# ---------------------------------------------
# 2) Montar todas as combinações origem-destino
# ---------------------------------------------
tabela_emissoes <- expand_grid(
  origem = origens,
  destino = destinos,
  tarifa = tarifas
) %>%
  mutate(
    emissao_kg = pmap_dbl(
      list(origem, destino, tarifa),
      ~ airport_footprint(
        departure   = ..1,
        arrival     = ..2,
        flightClass = ..3,
        output      = metrica,
        year        = ano_calculo
      )
    )
  ) %>%
  mutate(
    emissao_ton = emissao_kg / 1000
  ) %>%
  arrange(origem, destino, tarifa)

# -----------------------------
# 3) Mostrar a tabela no console
# -----------------------------
print(tabela_emissoes)

# Se quiser visualizar melhor no RStudio:
# View(tabela_emissoes)

# --------------------------------
# 4) Exportar a tabela para Excel
# --------------------------------
arquivo_saida <- "emissoes_aereas_eua_brasil.xlsx"

write_xlsx(
  list(
    emissoes = tabela_emissoes
  ),
  path = arquivo_saida
)

cat("Arquivo exportado com sucesso para:", arquivo_saida, "\n")
