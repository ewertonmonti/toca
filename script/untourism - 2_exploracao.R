# Pendências ----
## Dá pra misturar turistas e visitantes?
### Preciso verificar quantos casos tewm FLAG indicando que turistas = visitantes, e ver se posso aproveitar.

# Configurações iniciais ----
rm(list = ls(all=T))
library(tidyverse)
library(magrittr)
library(viridis)

# Funções de formatação de números
fun_formatC_inteiros <- function(x) {
  formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
}

fun_formatC_1_decimal <- function(x) {
  formatC(x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
}

fun_formatC_2_decimais <- function(x) {
  formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
}

fun_formatC_pct <- function(x) {
  paste0(
    formatC(x, format = "f", digits = 1, big.mark = ".", decimal.mark = ","),
    "%")
}

bd <- readRDS("data/untourism_tourists_visitors.rds")

head(bd)

bd %<>% rename(destino = reporter_area_label, origem = partner_area_label, ano = year, turistas = value)
bd %<>% relocate(origem, destino)
bd %<>% mutate(tipo_area = ifelse(partner_area_code > 1000, "regiao","pais"))
bd |> count(tipo_area)

#tipo_area x tipo
# Os totais são maiores quando trabalho com regiões
bd |> group_by(tipo_area, tipo) |> summarise(soma=sum(turistas))
bd |> group_by(tipo_area, tipo) |> filter(ano == 2019) |> summarise(soma=sum(turistas)) |> 
  pivot_wider(id_cols = tipo_area, names_from = tipo, values_from = soma) |>
  rempsyc::nice_table()		

bd |> filter(ano >= 2018) |> group_by(tipo_area, tipo, ano) |>  summarise(soma=sum(turistas)) |> 
  pivot_wider(id_cols = c(ano,tipo_area), names_from = tipo, values_from = soma) |> 
  arrange(ano, tipo_area) |> 
  rempsyc::nice_table(col.format.custom = c(1,3,4),
                      format.custom = c(
                        "fun_formatC_inteiros",
                        "fun_formatC_2_decimais","fun_formatC_2_decimais"))		

  



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

# Todas as ligações origem-destino
bd |> filter(ano == 2024) |> mutate(pct = turistas / sum(turistas) * 100, .by = c(tipo, tipo_area)) |> 
  arrange(tipo, tipo_area, desc(pct)) 

# Vetor de concorrentes ----
concorrentes <- c("Brazil","Peru","Dominican Republic","Colombia","Spain","Chile","Costa Rica","Jamaica",
                          "Cuba","Argentina","Mexico")

# Fluxo anual dos concorrentes
bd |> filter(destino %in% concorrentes) |> 
  summarise(turistas = sum(turistas), .by = c(destino,ano, tipo, tipo_area)) |> 
  ggplot(aes(x = ano, y = turistas, colour = destino)) +
  geom_line() +
  facet_wrap(vars(tipo, tipo_area))

# Sem Espanha
bd |> filter(destino %in% concorrentes, destino != "Spain") |> 
  summarise(turistas = sum(turistas), .by = c(destino,ano, tipo, tipo_area)) |> 
  ggplot(aes(x = ano, y = turistas, colour = destino)) +
  geom_line() +
  facet_wrap(vars(tipo, tipo_area))

# Sem Espanha e México
bd |> filter(destino %in% concorrentes, !destino %in% c("Spain", "Mexico")) |> 
  summarise(turistas = sum(turistas), .by = c(destino,ano, tipo, tipo_area)) |> 
  ggplot(aes(x = ano, y = turistas, colour = destino)) +
  geom_line() +
  facet_wrap(vars(tipo, tipo_area))

# Vetor de mercados ----
mercados <- c("Argentina","Chile","Paraguay","Uruguay", # Mercados consolidados
              "Germany","Spain", "United States of America","France","Portugal","United Kingdom of Great Britain and Northern Ireland", # Mercados essenciais
              "Canada","Colombia","Italy","Mexico","Netherlands (Kingdom of the)","Peru","Switzerland", # Mercados de crescimento
              "South Africa","Australia","Belgium","Bolivia (Plurinational State of)","China","Japan") # Mercados de oportunidade

# Fluxo anual dos mercados
bd |> filter(origem %in% mercados) |> 
  summarise(turistas = sum(turistas), .by = c(origem,ano, tipo, tipo_area)) |> 
  ggplot(aes(x = ano, y = turistas, colour = origem)) +
  geom_line() +
  facet_wrap(vars(tipo, tipo_area))


# Heatmaps ----
## Países de interesse ----
bd |> filter(tipo_area == "pais",
             origem %in% mercados, 
             destino %in% concorrentes,
             ano >= 2003) |> 
  ggplot(aes(x=ano, y=destino, fill = turistas)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  facet_wrap(vars(origem, tipo))

bd |> filter(tipo_area == "pais",
             origem == "Spain",
             destino %in% concorrentes,
             ano >= 2003) |> 
  ggplot(aes(x=ano, y=destino, fill = turistas)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  scale_y_discrete(limits = rev) +
  facet_wrap(vars(tipo)) +
  labs(title = "Número de chegadas oriundas da Espanha, \npor país de destino e ano",
       y = "País de destino",
       x = "Ano",
       fill = "Tipo de chegada\n (em mil)")

## Mercados de interesse, regiões OMT
bd |> filter(tipo_area == "regiao",
             destino %in% concorrentes, 
             ano >= 2003) |> 
  ggplot(aes(x=ano, y=destino, fill = turistas)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction=-1) +
  facet_wrap(vars(tipo))


## Número de data points por ano
bd |> filter(tipo_area == "pais",
             destino %in% concorrentes,
             origem %in% mercados,
             ano >= 2003) |>
  group_by(ano, tipo) |> 
  count() |> 
  pivot_wider(id_cols = ano, values_from = n, names_from = tipo) |> 
  print(n=Inf)

bd |> filter(tipo_area == "pais",
             destino %in% concorrentes,
             origem %in% mercados,
             ano >= 2003) |>
  group_by(ano, tipo) |> 
  count() |> 
  ggplot(aes(x = ano, y = n, colour = tipo)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 175, 25)) +
  scale_x_continuous(breaks = seq(2003, 2024, 2))+
  labs(title = "Número de data points de origem-destino, \npor ano e tipo de visitante",
       y = "Número de data points de origem-destino",
       x = "Ano",
       caption = "Restrito aos países de origem e destino de interesse")


## Número de data points por ano
bd |> filter(tipo_area == "pais",
             destino %in% concorrentes,
             origem %in% mercados,
             ano >= 2003) |>
  group_by(ano, tipo, origem) |> 
  count() |> 
  pivot_wider(id_cols = c(origem,ano), values_from = n, names_from = tipo) |> 
  print(n=Inf)
            

## Explorar mais os mercados
bd |> filter(tipo_area == "pais",
             destino %in% concorrentes,
             origem %in% mercados,
             ano >= 2003) |>
  group_by(ano, tipo, origem) |> 
  count()  |>
  ungroup() |> 
  ggplot(aes(x = ano, y = n, group=tipo, color=tipo)) +
  geom_point(show.legend = TRUE) +
  facet_wrap(~origem) +
  labs(title = "Número de data points de origem-destino, por origem, ano e tipo de visitante",
       caption = "Apenas origens e destinos de interesse")




bd |> filter(tipo_area == "pais",
             destino %in% concorrentes,
             origem %in% mercados,
             ano >= 2003) |>
  group_by(ano, tipo) |> 
  count(destino)


# Número de turistas por mercado e ano
bd |> filter(tipo_area == "pais" & tipo == "turistas" & origem %in% mercados) |> 
  summarise(turistas = sum(turistas), .by = c(origem,ano))

bd |> filter(tipo_area == "pais" & tipo == "turistas" & origem %in% mercados) |> 
  summarise(turistas = sum(turistas), .by = c(origem,ano)) |> 
  ggplot(aes(x = ano, y = turistas, colour = origem)) +
  geom_line()

# Número de turistas por mercado e ano, comparando tipos
bd |> filter(origem %in% mercados, origem != "Argentina", tipo_area == "pais") |> 
  summarise(turistas = sum(turistas), .by = c(origem, tipo, ano)) |> 
  ggplot(aes(x = ano, y = turistas)) +
  geom_bar(stat="identity") + facet_wrap(vars(origem, tipo))

# Turistas x visitantes ----
# Sobreposição entre turistas e visitantes
bd |> filter(origem %in% mercados, tipo_area == "pais") |> 
  summarise(turistas = sum(turistas), .by = c(origem, tipo, ano)) |> 
  ggplot(aes(x = ano, y = turistas, group = tipo, fill = tipo)) +
geom_bar(stat="identity", position = "dodge", alpha = .4, show.legend = TRUE) + 
  facet_wrap(vars(origem), scales = "free_y")

bd |> filter(origem %in% mercados, tipo_area == "pais") |> 
  summarise(turistas = sum(turistas), .by = c(origem, tipo, ano)) |> 
  ggplot(aes(x = ano, y = turistas, group = tipo, fill = tipo)) +
  geom_density(stat="identity", alpha = .3, show.legend = TRUE) + 
  facet_wrap(vars(origem), scales = "free_y")


bd |> filter(origem %in% mercados, origem != "Argentina", ano >= 2010) |> summarise(turistas = sum(turistas), .by = c(origem,ano)) |> 
  ggplot(aes(x = ano, y = turistas)) +
  geom_bar(stat="identity") + facet_wrap(~origem)

# Banco apenas com origens e destinos de interesse
bdp <- bd |>  filter(origem %in% mercados, destino %in% concorrentes) 
bdp %<>% filter(ano >= 2003)

bd_todos <- bdp |> 
  pivot_wider(id_cols = c(origem,destino), names_from = ano, values_from = turistas, names_sort = TRUE) |> 
  arrange(origem, destino)

# Matriz OD ----
# Banco em formato de matriz
matrizod <- bd |> 
  filter(origem %in% mercados & 
           destino %in% concorrentes & 
           tipo == "turistas" & 
           tipo_area == "pais" &
           ano >= 2003) |> 
  group_by(origem, destino) |> 
  rename(Destino = destino) |> 
  count()|> 
  pivot_wider(id_cols = Destino, names_from = origem, names_prefix = "Origem.", values_from = n, names_sort = TRUE) |> 
  arrange(Destino)

tab <- rempsyc::nice_table(matrizod, separate.header = TRUE)	

library(rempsyc)
library(flextable)
library(scales)

# Valores baixos = Vermelho, Valores altos = Verde
cor_escala <- col_numeric(
  palette = c("#ff4c4c", "#ffff8d", "#66bb6a"),
  domain = c(0, 22)
)

# 3. Aplicar a cor de fundo à coluna específica
tabela <- bg(tab, 
             j = c(2:24), 
             bg = cor_escala, 
             part = "body")

tabela
save_as_image(
  tabela,
  path = "output/matrizod.png")







             