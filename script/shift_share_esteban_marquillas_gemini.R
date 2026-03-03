library(dplyr)
library(tidyr)

# 1. Preparação do Benchmark (Total dos 8 países)
benchmark_static <- bd %>%
  filter(ano %in% c(2010, 2019)) %>%
  group_by(ano, origem) %>%
  summarise(T_iAREA = sum(chegadas), .groups = 'drop') %>%
  group_by(ano) %>%
  mutate(T_AREA = sum(T_iAREA)) %>%
  ungroup()

# Taxas de crescimento do Benchmark (G_iAREA e G_AREA)
g_benchmark <- benchmark_static %>%
  pivot_wider(names_from = ano, values_from = c(T_iAREA, T_AREA)) %>%
  mutate(
    G_iAREA = (T_iAREA_2019 - T_iAREA_2010) / T_iAREA_2010,
    G_AREA = (T_AREA_2019 - T_AREA_2010) / T_AREA_2010
  ) %>%
  select(origem, G_iAREA, G_AREA)

# 2. Cálculo dos componentes por par Origem-Destino
estatico_detalhado <- bd %>%
  filter(ano %in% c(2010, 2019)) %>%
  pivot_wider(names_from = ano, values_from = chegadas, names_prefix = "Ano_") %>%
  # Cálculo de T_j_total_2010 (Soma de todas as origens para o destino j no ano 0)
  group_by(destino) %>%
  mutate(T_j_total_2010 = sum(Ano_2010)) %>%
  ungroup() %>%
  # Join com dados do benchmark (T_iAREA_2010 e T_AREA_2010 para o cálculo do Homothetic)
  left_join(benchmark_static %>% filter(ano == 2010) %>% select(origem, T_iAREA, T_AREA), by = "origem") %>%
  left_join(g_benchmark, by = "origem") %>%
  mutate(
    G_ij = (Ano_2019 - Ano_2010) / Ano_2010,
    T_hat = T_j_total_2010 * (T_iAREA / T_AREA), # Homothetic Arrival [cite: 235, 1338]
    
    Area_Wide = Ano_2010 * G_AREA,
    Region_Mix = Ano_2010 * (G_iAREA - G_AREA),
    Competitive = T_hat * (G_ij - G_iAREA),
    Allocation = (Ano_2010 - T_hat) * (G_ij - G_iAREA),
    Actual_Growth = Ano_2019 - Ano_2010
  )

# 3. Agregação Final por Destino
resultado_estatico_final <- estatico_detalhado %>%
  group_by(destino) %>%
  summarise(
    Crescimento_Real = sum(Actual_Growth),
    Efeito_Area = sum(Area_Wide),
    Efeito_Mix_Regiao = sum(Region_Mix),
    Efeito_Competitivo = sum(Competitive),
    Efeito_Alocacao = sum(Allocation)
  )

anos <- 2010:2019
lista_anual <- list()

for(i in 1:(length(anos)-1)){
  t0_ano <- anos[i]
  t1_ano <- anos[i+1]
  
  # Dados do par de anos
  dados_par <- bd %>% filter(ano %in% c(t0_ano, t1_ano))
  
  # Benchmark do par
  bm_par <- dados_par %>%
    group_by(ano, origem) %>%
    summarise(T_iAREA = sum(chegadas), .groups = 'drop') %>%
    group_by(ano) %>%
    mutate(T_AREA = sum(T_iAREA)) %>%
    ungroup()
  
  # Taxas do par
  taxas_par <- bm_par %>%
    pivot_wider(names_from = ano, values_from = c(T_iAREA, T_AREA)) %>%
    # Usando índices para evitar erro de nome de coluna
    rename(T_i0 = 2, T_i1 = 3, T_0 = 4, T_1 = 5) %>%
    mutate(G_iAREA = (T_i1 - T_i0) / T_i0, G_AREA = (T_1 - T_0) / T_0)
  
  # Componentes do par
  df_par <- dados_par %>%
    pivot_wider(names_from = ano, values_from = chegadas, names_prefix = "A") %>%
    rename(Val0 = 3, Val1 = 4) %>%
    group_by(destino) %>%
    mutate(Total_j0 = sum(Val0)) %>%
    ungroup() %>%
    left_join(taxas_par, by = "origem") %>%
    mutate(
      G_ij = (Val1 - Val0) / Val0,
      T_hat = Total_j0 * (T_i0 / T_0),
      
      Area_Wide = Val0 * G_AREA,
      Region_Mix = Val0 * (G_iAREA - G_AREA),
      Competitive = T_hat * (G_ij - G_iAREA),
      Allocation = (Val0 - T_hat) * (G_ij - G_iAREA),
      Actual = Val1 - Val0
    )
  
  lista_anual[[i]] <- df_par
}

# Consolidação Dinâmica Agregada por Destino
resultado_dinamico_final <- bind_rows(lista_anual) %>%
  group_by(destino) %>%
  summarise(
    Crescimento_Real_Total = sum(Actual),
    Efeito_Area_Acumulado = sum(Area_Wide),
    Efeito_Mix_Regiao_Acumulado = sum(Region_Mix),
    Efeito_Competitivo_Acumulado = sum(Competitive),
    Efeito_Alocacao_Acumulado = sum(Allocation)
  )