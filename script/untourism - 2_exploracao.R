# Configurações iniciais ----
rm(list = ls(all=T))
library(tidyverse)
library(magrittr)
library(viridis)

bd <- readRDS("data/untourism_tourists_visitors.rds")

head(bd)

bd %<>% rename(destino = reporter_area_label, origem = partner_area_label, ano = year, turistas = value)
bd %<>% relocate(origem, destino)
bd %<>% mutate(tipo_area = ifelse(partner_area_code > 1000, "regiao","pais"))
bd |> count(tipo_area)
bd |> group_by(tipo_area, tipo) |> summarise(sum(turistas)) # Os totais são maiores quando trabalho com regiões
bd  %<>% select(origem, destino, ano, turistas, tipo, tipo_area)


bd |> group_by(tipo, tipo_area) |> count(origem) |> arrange(desc(n))
bd |> group_by(tipo, tipo_area) |> count(destino) |> arrange(desc(n))
bd |> group_by(tipo, tipo_area) |> count(ano) |> ungroup() |> 
  ggplot(aes(x = ano, y = n)) + geom_bar(stat = "identity") + facet_wrap(vars(tipo, tipo_area))

bd |> 
  summarise(total = sum(turistas, na.rm = TRUE), .by = c(tipo, tipo_area, ano)) |> 
  ggplot(aes(x = ano, y = total)) + geom_bar(stat = "identity") + facet_wrap(vars(tipo, tipo_area))

bd |> filter(ano == 2024) |> 
  summarise(total = sum(turistas, na.rm = TRUE), .by = origem) |> 
  mutate(pct = total / sum(total) * 100) |> 
  arrange(desc(pct))

bd |> filter(ano == 2024) |> mutate(pct = turistas / sum(turistas) * 100) |> arrange(desc(pct))

# Vetor de concorrentes ----
concorrentes <- c("Brazil","Peru","Dominican Republic","Colombia","Spain","Chile","Costa Rica","Jamaica",
                          "Cuba","Argentina","Mexico")

bd |> filter(tipo_area == "pais" & tipo == "turistas" & destino %in% concorrentes) |> 
  summarise(turistas = sum(turistas), .by = c(destino,ano)) |> 
  ggplot(aes(x = ano, y = turistas, colour = destino)) +
  geom_line()

# Vetor de mercados ----
mercados <- c("Germany","Argentina","Australia","Canada","Chile","China","Colombia","Spain","United States of America",
              "France","Italy","Mexico","Paraguay","Peru","Portugal",
              "United Kingdom of Great Britain and Northern Ireland","Uruguay")

# Tentativas de identificar problemas nos dados
# Heatmaps 
# Países de interesse
bd |> filter(tipo_area == "pais",
             origem %in% mercados, 
             destino %in% concorrentes,
             ano >= 2003) |> 
  ggplot(aes(x=ano, y=destino, fill = turistas)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  facet_wrap(vars(origem, tipo))

# Mercados de interesse, regiões OMT
bd |> filter(tipo_area == "regiao",
             destino %in% concorrentes, 
             ano >= 2003) |> 
  ggplot(aes(x=ano, y=destino, fill = turistas)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  facet_wrap(vars(tipo))





bd |> filter(tipo_area == "pais" & tipo == "turistas" & origem %in% mercados) |> 
  summarise(turistas = sum(turistas), .by = c(origem,ano)) |> 
  ggplot(aes(x = ano, y = turistas, colour = origem)) +
  geom_line()

bd |> filter(tipo_area == "pais" & tipo == "turistas" & origem %in% mercados) |> 
  summarise(turistas = sum(turistas), .by = c(origem,ano))

bd |> filter(origem %in% mercados, origem != "Argentina") |> summarise(turistas = sum(turistas), .by = c(origem,ano)) |> 
  ggplot(aes(x = ano, y = turistas)) +
  geom_bar(stat="identity") + facet_wrap(~origem)

bd |> filter(origem %in% mercados, origem != "Argentina", ano >= 2010) |> summarise(turistas = sum(turistas), .by = c(origem,ano)) |> 
  ggplot(aes(x = ano, y = turistas)) +
  geom_bar(stat="identity") + facet_wrap(~origem)

# apliquei a correção para o Paraguai (x/1000) para todos os anos, mas preciso confirmar se vale para todos os anos mesmo
bd |> filter(destino == "Paraguay") |> summarise(sum(turistas), .by=ano)
bd |> filter(destino == "Pakistan") |> summarise(sum(turistas), .by=ano)

# Banco apenas com origens e destinos de interesse
bdp <- bd |>  filter(origem %in% mercados, destino %in% concorrentes) 
bdp %<>% filter(ano >= 2003)

bd_todos <- bdp |> 
  pivot_wider(id_cols = c(origem,destino), names_from = ano, values_from = turistas, names_sort = TRUE) |> 
  arrange(origem, destino)
# bd_arg <- bdp |> filter(origem == "Argentina") |> pivot_wider(id_cols = destino, names_from = ano, values_from = turistas, names_sort = TRUE)
# bd_aus <- bdp |> filter(origem == "Australia") |> pivot_wider(id_cols = destino, names_from = ano, values_from = turistas, names_sort = TRUE)
# bd_ale <- bdp |> filter(origem == "Germany") |> pivot_wider(id_cols = destino, names_from = ano, values_from = turistas, names_sort = TRUE)
# bd_chil <- bdp |> filter(origem == "Chile") |> pivot_wider(id_cols = destino, names_from = ano, values_from = turistas, names_sort = TRUE)
# bd_chin <- bdp |> filter(origem == "China") |> pivot_wider(id_cols = destino, names_from = ano, values_from = turistas, names_sort = TRUE)

# Banco em formato de matriz
bdpm <- bdp |> group_by(origem, destino) |> count()|> pivot_wider(id_cols = origem, names_from = destino, values_from = n)



# Libraries
library(hrbrthemes)
library(GGally)
library(viridis)

# Data set is provided by R natively

# Plot
bd_todos |> filter(origem == "United States of America") |> 
ggparcoord(columns = c(3:24), groupColumn = 2, scale = "globalminmax",
           showPoints = TRUE,
           title = "Parallel Coordinate Plot",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




             