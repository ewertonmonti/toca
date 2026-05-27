library(tidyverse)
library(magrittr)
library(readxl)
ttdi <- read_excel("data/TTDI_2019.xlsx", 
                   sheet = "rank 2019 principal")

ttdi |> pivot_longer(cols = c(Brazil:Peru), names_to = "Destino", values_to = "Posição") |> 
  rename(Dimensão = País) |> 
  mutate(Destino = factor(Destino,
                       levels = c("Brazil", "Argentina", "Colombia", "Dominican Republic", "Jamaica", "Peru"))) |> 
  mutate(Dimensão = factor(Dimensão,
                           levels = c("Posição geral", 
                                      "Ambiente habilitador", 
                                      "Políticas e condições habilitadoras para viagens e turismo", 
                                      "Infraestrutura e serviços", 
                                      "Recursos de viagens e turismo", 
                                      "Sustentabilidade de viagens e turismo"))) |> 
  ggplot(aes(x = Dimensão, y = Posição, color = Destino, group = Destino)) +
  geom_segment(aes(xend = Dimensão, yend = 1),
               position = position_dodge(width = 0.7),
               linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  scale_x_discrete(labels = scales::label_wrap(20)) +
  scale_y_reverse(limits = c(110, 1), breaks = c(1,15,30,45,60,75,90,105)) +
  labs(x = NULL, color = "Destino")

plot <- ttdi[1:6,] |> pivot_longer(cols = c(Brazil:Peru), names_to = "Destino", values_to = "Posição") |> 
  rename(Dimensão = País) |> 
  mutate(Destino = factor(Destino,
                          levels = c("Brazil", "Argentina", "Colombia", "Dominican Republic", "Jamaica", "Peru"))) |> 
  mutate(Dimensão = factor(Dimensão,
                           levels = c("Posição geral", 
                                      "Ambiente habilitador", 
                                      "Políticas e condições habilitadoras para viagens e turismo", 
                                      "Infraestrutura e serviços", 
                                      "Recursos de viagens e turismo", 
                                      "Sustentabilidade de viagens e turismo"))) |> 
  ggplot(aes(x = Destino, y = Posição, color = Dimensão, group = Dimensão)) +
  geom_segment(aes(xend = Destino, yend = 1),
               position = position_dodge(width = 0.7),
               linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_text(aes(label = Posição),
    position = position_dodge(width = 0.7),
    vjust = 1.8,
    size = 3,
    show.legend = FALSE) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(limits = c(110, 1), breaks = c(1,16,31,46,61,76,91,106)) +
  labs(x = NULL, color = "Dimensão") +
  theme(legend.position = "bottom")
plot
ggsave("output/fig_ttdi_ranking.jpg", plot = last_plot(), dpi = 300, width = 10, height = 6, units = "in")

