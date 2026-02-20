```{r}
#| label: Importações data bulk 12/2025
#| eval: false
#| include: false

# Tipos esperados (readxl)
col_types <- c(
  "text",     # indicator_code
  "text",     # indicator_label
  "text",     # indicator_previous_code
  "numeric",  # reporter_area_code
  "text",     # reporter_area_label
  "numeric",  # partner_area_code
  "text",     # partner_area_label
  "numeric",  # year
  "numeric",  # value
  "text",     # flag
  "text",     # flag_label
  "text",     # unit
  "text"      # notes
)

total_arrivals <- read_excel("data/UN_Tourism_bulk_data_download_12_2025/02_Inbound/01_Total_arrivals/UN_Tourism_inbound_arrivals_12_2025.xlsx", 
                             sheet = "Data",
                             col_types = col_types
)

bd <- total_arrivals |> filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR")



# Cria tabela com os totais anuais dos concorrentes
tab_concorrentes <- total_arrivals |> 
  filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR") |> 
  filter(reporter_area_label %in% concorrentes) |> 
  summarise(chegadas = sum(value), .by = c(reporter_area_label, year))

# Heatmap pra verificar a completude anual dos dados
tab_concorrentes |> filter(year >= 2003) |> 
  ggplot(aes(x=year, y=reporter_area_label, fill = chegadas)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, direction=-1)

# Cria tabela com os totais anuais do mundo
tab_mundo <- total_arrivals |> 
  filter(indicator_code == "INBD_TRIP_TOTL_TOTL_TOUR") |> 
  filter(partner_area_label == "World") |> 
  summarise(chegadas = sum(value), .by = year) |> 
  mutate(reporter_area_label = "World")

# Gráfico do total mundial de chegadas
tab_mundo |> ggplot(aes(x = year, y = chegadas)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = formatC(chegadas, format = "f", big.mark = ".", decimal.mark = ",", digits = 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 2.5) +
  scale_x_continuous(breaks = seq(2003, 2024, 3),
                     minor_breaks = seq(2003, 2024, 1)) +
  labs(
    title = "Total de chegadas turísticas internacionais, por ano",
    y = "Chegadas (mil)",
    x = "Ano")

# Cria tabela com a participação anual de cada país no total mundial
bd_part <- left_join(tab_concorrentes, tab_mundo, by = join_by(year)) |> 
  rename(chegadas = chegadas.x,
         mundo = chegadas.y,
         destino = reporter_area_label.x,
         ano = year) |> 
  dplyr::select(-reporter_area_label.y) |> 
  filter(ano >= 2003) |> 
  mutate(pct = chegadas / mundo * 100)



# Gráfico do total das chegadas
bd_part |> ggplot(aes(x = ano, y = chegadas, colour = destino)) +
  geom_line()

# Gráfico do total das chegadas, sem Espanha e México
bd_part |> filter(!destino %in% c("Spain", "Mexico")) |> 
  ggplot(aes(x = ano, y = chegadas, colour = destino)) +
  geom_line()

# Gráfico da participação
bd_part |> ggplot(aes(x = ano, y = pct, colour = destino)) +
  geom_line()

# Gráfico da participação, sem Espanha e México
bd_part |> filter(!destino %in% c("Spain", "Mexico")) |> 
  ggplot(aes(x = ano, y = pct, colour = destino)) +
  geom_line()

# Gráfico da participação do Brasil
bd_part |> filter(destino == "Brazil") |> 
  ggplot(aes(x = ano, y = pct)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = formatC(pct, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 2.5) +
  scale_x_continuous(breaks = seq(2003, 2024, 3),
                     minor_breaks = seq(2003, 2024, 1)) +
  labs(
    title = "Participação do Brasil no total de chegadas turísticas internacionais, por ano",
    y = "Participação (%)",
    x = "Ano")

# Gráfico da participação de todos os países
bd_part |> ggplot(aes(x = ano, y = pct)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = formatC(pct, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 2.5) +
  scale_x_continuous(breaks = seq(2003, 2024, 3),
                     minor_breaks = seq(2003, 2024, 1)) +
  labs(
    title = "Participação no total de chegadas turísticas internacionais, por ano",
    y = "Participação (%)",
    x = "Ano") + 
  facet_wrap(~destino, scales = "free_y")




```


