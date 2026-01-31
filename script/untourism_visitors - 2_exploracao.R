# Configurações iniciais ----
rm(list = ls(all=T))
library(tidyverse)
library(magrittr)

bd <- readRDS("data/bd_untourism_visitors.rds")
head(bd)
bd %<>% rename(destino = reporter_area_label, origem = partner_area_label, ano = year, turistas = value)
bd %<>% relocate(origem, destino)

bd |> count(origem) |> arrange(desc(n))
bd |> count(destino) |> arrange(desc(n))
bd |> count(ano) |> ggplot(aes(x = ano, y = n)) + geom_bar(stat = "identity")
bd |> 
  summarise(total = sum(turistas, na.rm = TRUE), .by = ano) |> 
  ggplot(aes(x = ano, y = total)) + geom_bar(stat = "identity")

bd |> filter(ano == 2021) |> 
  summarise(total = sum(turistas, na.rm = TRUE), .by = origem) |> 
  mutate(pct = total / sum(total) * 100) |> 
  arrange(desc(pct))

bd |> filter(ano == 2021) |> mutate(pct = turistas / sum(turistas) * 100) |> arrange(desc(pct))
