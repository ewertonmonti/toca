library(readr)
library(tidyverse)
library(magrittr)
bd <- read_delim("data/Sensibilidade de países às mudanças climáticas.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)

bd %<>% janitor::clean_names()

bd <- bd |> group_by(pergunta) |> 
  arrange(desc(resultado), .by_group = TRUE) |> 
  mutate(ordem = row_number()) |> 
  ungroup() |> 
  arrange(pais) |> 
  mutate(posicao_media = mean(ordem), .by= pais)

bd |> dplyr::select(pais, posicao_media) |> arrange(posicao_media) |> distinct() |> View("posicao")
bd |> dplyr::select(pais, posicao_media) |> arrange(posicao_media) |> distinct() |> print(n=20)
# Desta lista acima, preciso retirar os principais emissores mundiais ou país com população acima de X (critério a definir)

tab <- bd |> pivot_wider(id_cols = pais, names_from = pergunta, values_from = resultado) |> 
  janitor::clean_names() |> 
  arrange(desc(cc_e_um_problema_importante))


