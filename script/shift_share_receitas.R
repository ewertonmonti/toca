# Shift-share dinâmico (turismo receptivo) com REAT::shiftd ----
rm(list = ls(all=T))
library(tidyverse)
library(magrittr)
library(REAT)
library(readxl)

pais_alvo <- "Brazil"

# Importa dados da OMT ---
path <- "data/UN_Tourism_inbound_expenditure_10_2025.xlsx"
bd_raw <- readxl::read_excel(path, 
                             sheet = "Data",
                             col_types = c("text", "text", "text", "numeric", "text", "numeric",
                                           "text","numeric","numeric","text","text","text",
                                           "text","text")) |> 
  janitor::clean_names()

# Verificação dos dados ----
bd_raw |> count(unit)
bd_raw |> count(indicator_label)
bd_raw |> count(partner_area_label)
bd_raw |> filter(year == 2024, reporter_area_label == pais_alvo) |> group_by(partner_area_label) |> summarise(sum(value))
bd_raw |> filter(year == 2024, reporter_area_label == pais_alvo) |> summarise(sum(value))
bd_raw |> filter(reporter_area_label== pais_alvo, year == 2018) |> group_by(year, indicator_label) |> summarise(total=sum(value)) |> 
  pivot_wider(id_cols = year, names_from = indicator_label, values_from = total)


# Total de receitas por país e ano ----
bd_pa <- bd_raw |>
  summarise(receitas = sum(value), .by = c(reporter_area_label, year)) |> 
  rename(pais = reporter_area_label,
         ano = year)

# Número de observações por país
bd_pa |> count(pais) |> group_by(n) |> count() |> print(n=Inf)

bd_pa |> filter(pais == pais_alvo) |> ggplot(aes(x = ano, y = receitas)) + 
  geom_bar(stat = "identity")

# Limita ao período de análise ----
v_anos <- c(2003:2019)

bd <- bd_pa |> filter(ano %in% v_anos)
bd |> filter(pais == pais_alvo) |> ggplot(aes(x = ano, y = receitas)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = v_anos, labels = as.character(v_anos)) +
  scale_y_continuous(n.breaks = length(v_anos))


# Série "mundo" ----
bd_mundo <- bd_pa |>
  summarise(receitas = sum(receitas, na.rm = TRUE), .by = ano) |>
  arrange(ano) |> 
  filter(ano %in% v_anos)

bd_mundo |> ggplot(aes(x = ano, y = receitas)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = v_anos, labels = as.character(v_anos)) +
  scale_y_continuous(n.breaks = length(v_anos))


# Shift-share ----
e_ij1	<- bd |> filter(pais == pais_alvo, ano == 2003) |> pull(receitas)

e_ij2	<- bd |> filter(pais == pais_alvo, ano > 2003) |> 
  pivot_wider(values_from = receitas, names_from = ano) |> 
  dplyr::select(-pais)|> as.data.frame()

e_i1 <-  bd_mundo |> filter(ano == 2003) |> pull(receitas)

e_i2	<- bd_mundo |> filter(ano > 2003) |> 
  pivot_wider(values_from = receitas, names_from = ano) |>
  as.data.frame()
time1 <- min(v_anos)
time2 <- max(v_anos)

res_ss <- REAT::shiftd(e_ij1 = e_ij1,
                       e_ij2 = e_ij2,
                       e_i1  = e_i1,
                       e_i2  = e_i2,
                       time1 = time1,
                       time2 = time2,
                       shift.method = "Dunn",
                       print.results = TRUE, plot.results = TRUE)
