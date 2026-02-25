rm(list = ls(all=T))
library(tidyverse)
library(magrittr)
library(readxl)
library(viridis)
library(WDI)

bde <- read_excel("data/UN_Tourism_bulk_data_download_12_2025/03_Outbound/01_Total_departures/UN_Tourism_outbound_departures_12_2025.xlsx", 
                  sheet = "Data",
                  col_types = c("text", "text", "text", "numeric", "text", "numeric",
                                "text","numeric","numeric","text","text","text","text")) |> 
  janitor::clean_names() |> 
  rename(origem = reporter_area_label,
         ano = year) |> 
  dplyr::select(-c(indicator_previous_code, reporter_area_code, partner_area_code))

bde |> count(indicator_code, indicator_label)

# Vetor de mercados prioritários e sensíveis ao clima
# OBS: não há dados para África do Sul
mercados <- c("Argentina","Chile","Paraguay","Uruguay", # Mercados consolidados
              "Germany","Spain", "United States of America","France","Portugal","United Kingdom of Great Britain and Northern Ireland", # Mercados essenciais
              "Canada","Colombia","Italy","Mexico","Netherlands (Kingdom of the)","Peru","Switzerland", # Mercados de crescimento
              "South Africa","Australia","Belgium","Bolivia (Plurinational State of)","China","Japan", # Mercados de oportunidade
              "Republic of Korea","Sweden","Denmark","Norway") # Selecionei devido à sensibildade ao clima + fluxo 2019 no Brasil

# Tabela do número de observações por país e indicador
bde |> filter(origem %in% mercados, 
              ano %in% 2010:2024, 
              indicator_code  %in% c("OUTB_TRIP_TOTL_TOTL_TOUR","OUTB_TRIP_TOTL_TOTL_VSTR")) |> 
  mutate(qtos = n(), .by = c(indicator_code,origem)) |> 
  dplyr::select(origem, qtos, indicator_code) |> 
  distinct() |> 
  pivot_wider(id_cols = origem, values_from = qtos, names_from = indicator_code) |> 
 View()

# Gráfico do número de observações por país, ano e indicador de TURISTAS
bde |> filter(origem %in% mercados, ano %in% 2010:2024, indicator_code  == "OUTB_TRIP_TOTL_TOTL_TOUR") |> 
  mutate(qtos = n(), .by = c(indicator_code,origem)) |> 
  mutate(origem = reorder(origem, qtos)) |> 
  ggplot(aes(x=ano, y=origem, fill = value)) + 
  geom_tile() +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  labs(title = "OUTB_TRIP_TOTL_TOTL_TOUR") 

# Gráfico do número de observações por país, ano e indicador de VISITANTES
bde |> filter(origem %in% mercados, ano %in% 2010:2024, indicator_code  != "OUTB_TRIP_TOTL_TOTL_VSTR") |> 
  mutate(qtos = n(), .by = c(indicator_code,origem)) |> 
  mutate(origem = reorder(origem, qtos)) |> 
  ggplot(aes(x=ano, y=origem, fill = value)) + 
  geom_tile() +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  labs(title = "OUTB_TRIP_TOTL_TOTL_VSTR") 

# Tabela do número de observações por ano e indicador
# Com base nesta tabela, decidi priorizar a série OUTB_TRIP_TOTL_TOTL_TOUR 
bde |> filter(origem %in% mercados, ano %in% 2010:2024, indicator_code  != "OUTB_TRIP_TOTL_TOTL_EXCR") |> 
  summarise(qtos = n(), .by = c(indicator_code, ano)) |> 
  pivot_wider(id_cols = ano, names_from = indicator_code, values_from = qtos)

# Análise das notas
bde |> filter(origem %in% mercados, ano %in% 2010:2024, indicator_code  != "OUTB_TRIP_TOTL_TOTL_EXCR") |> 
  group_by(indicator_code) |> count(notes) |> arrange(desc(n))

bde |> filter(origem %in% mercados, ano %in% 2010:2024, indicator_code  != "OUTB_TRIP_TOTL_TOTL_EXCR",
              str_detect(notes, "Value represents")) |> View("a")
# OBS: As notas reforçam a conclusão de usar TURISTAS


