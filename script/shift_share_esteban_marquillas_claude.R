dados <- bd
# =============================================================================
# ANÁLISE SHIFT-SHARE PARA TURISMO - VERSÃO ESTEBAN-MARQUILLAS (1972)
# Baseado em: Alavi & Yasin (2000), Yasin et al. (2004), Sobral et al. (2007),
#             Toh et al. (2004), De Santana Ribeiro & De Lima Andrade (2015)
#
# Estrutura do banco de dados "bd":
#   - ano     : ano de referência (2009-2019)
#   - origem  : região emissora ("América", "Europa", "Outros")
#   - destino : país de destino (8 países)
#   - chegadas: número de chegadas turísticas internacionais
#
# Benchmark: todos os 8 países de destino combinados
# Output:    detalhado (destino × origem) e agregado por destino
# =============================================================================

library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# FUNÇÃO AUXILIAR: Classificação do efeito de alocação
# Quadrantes segundo Esteban-Marquillas (1972) / Alavi & Yasin (2000):
#   A,S  = Vantagem competitiva + Especializada        -> alocação positiva
#   A,N  = Vantagem competitiva + Não especializada    -> alocação negativa
#   D,S  = Desvantagem competitiva + Especializada     -> alocação negativa
#   D,N  = Desvantagem competitiva + Não especializada -> alocação positiva
# -----------------------------------------------------------------------------
classificar_alocacao <- function(efeito_competitivo, T0_ij, T0_hat_ij) {
  vantagem     <- efeito_competitivo >= 0   # TRUE = vantagem competitiva
  especializada <- T0_ij >= T0_hat_ij       # TRUE = especializada
  
  dplyr::case_when(
    vantagem &  especializada ~ "A,S",
    vantagem & !especializada ~ "A,N",
    !vantagem &  especializada ~ "D,S",
    !vantagem & !especializada ~ "D,N"
  )
}


# =============================================================================
# VERSÃO 1: SHIFT-SHARE ESTÁTICO
# Compara o ano inicial (t0) com o ano final (t1) do período completo.
# Período: 2010-2019 (usa 2009 apenas para calcular taxas relativas a 2010
#          se necessário; aqui o período de análise é definido pelos anos
#          extremos disponíveis excluindo 2009, que serve só de base).
#
# Componentes (notação de Alavi & Yasin, 2000):
#   Crescimento real    = T1_ij - T0_ij
#   Efeito Área         = T0_ij * G_AREA
#   Efeito Mix-Região   = T0_ij * (G_iAREA - G_AREA)
#   Efeito Competitivo  = T_hat_ij * (G_ij - G_iAREA)
#   Efeito Alocação     = (T0_ij - T_hat_ij) * (G_ij - G_iAREA)
#
# onde T_hat_ij (chegadas homotéticas) = T0_j * (T0_iAREA / T0_AREA)
# =============================================================================

shift_share_estatico <- function(dados,
                                 ano_inicial = 2011,
                                 ano_final   = 2019) {
  
  # --- 1. Filtrar anos de interesse -------------------------------------------
  t0 <- dados %>% filter(ano == ano_inicial)
  t1 <- dados %>% filter(ano == ano_final)
  
  # --- 2. Agregar: chegadas totais por (origem, AREA) no t0 e t1 -------------
  #   T0_iAREA = total de chegadas da origem i para TODOS os destinos no t0
  #   T1_iAREA = idem no t1
  T_iAREA <- left_join(
    t0 %>% group_by(origem) %>% summarise(T0_iAREA = sum(chegadas), .groups = "drop"),
    t1 %>% group_by(origem) %>% summarise(T1_iAREA = sum(chegadas), .groups = "drop"),
    by = "origem"
  ) %>%
    mutate(G_iAREA = (T1_iAREA - T0_iAREA) / T0_iAREA)
  
  # --- 3. Agregar: chegadas totais de TODAS origens para TODAS os destinos ---
  #   T0_AREA = total geral no t0
  #   T1_AREA = total geral no t1
  T0_AREA <- sum(t0$chegadas)
  T1_AREA <- sum(t1$chegadas)
  G_AREA  <- (T1_AREA - T0_AREA) / T0_AREA
  
  # --- 4. Agregar: chegadas totais por destino (soma de origens) no t0 -------
  #   T0_j = total recebido pelo destino j no t0 (necessário para T_hat)
  T_j_t0 <- t0 %>%
    group_by(destino) %>%
    summarise(T0_j = sum(chegadas), .groups = "drop")
  
  # --- 5. Montar par t0-t1 por (destino, origem) -----------------------------
  pares <- left_join(t0, t1, by = c("destino", "origem"),
                     suffix = c("_t0", "_t1")) %>%
    rename(T0_ij = chegadas_t0, T1_ij = chegadas_t1) %>%
    select(destino, origem, T0_ij, T1_ij)
  
  # --- 6. Juntar taxas de crescimento e totais --------------------------------
  pares <- pares %>%
    left_join(T_iAREA, by = "origem") %>%
    left_join(T_j_t0,  by = "destino") %>%
    mutate(
      G_AREA  = G_AREA,   # escalar -> coluna
      G_ij    = (T1_ij - T0_ij) / T0_ij,
      
      # Chegadas homotéticas: o que o destino j receberia da origem i
      # se tivesse a mesma proporção de mercado que o benchmark
      T0_hat_ij = T0_j * (T0_iAREA / T0_AREA)
    )
  
  # --- 7. Calcular os quatro componentes --------------------------------------
  resultados <- pares %>%
    mutate(
      crescimento_real  = T1_ij - T0_ij,
      efeito_area       = T0_ij * G_AREA,
      efeito_mix_regiao = T0_ij * (G_iAREA - G_AREA),
      efeito_competitivo = T0_hat_ij * (G_ij - G_iAREA),
      efeito_alocacao    = (T0_ij - T0_hat_ij) * (G_ij - G_iAREA),
      
      # Verificação: soma dos componentes deve igualar crescimento_real
      soma_componentes  = efeito_area + efeito_mix_regiao +
        efeito_competitivo + efeito_alocacao,
      
      # Classificação do quadrante
      codigo = classificar_alocacao(efeito_competitivo, T0_ij, T0_hat_ij)
    ) %>%
    select(destino, origem,
           T0_ij, T1_ij,
           crescimento_real,
           efeito_area, efeito_mix_regiao, efeito_competitivo, efeito_alocacao,
           soma_componentes, codigo)
  
  # --- 8. Resultado agregado por destino (soma das origens) ------------------
  agregado <- resultados %>%
    group_by(destino) %>%
    summarise(
      T0_total          = sum(T0_ij),
      T1_total          = sum(T1_ij),
      crescimento_real  = sum(crescimento_real),
      efeito_area       = sum(efeito_area),
      efeito_mix_regiao = sum(efeito_mix_regiao),
      efeito_competitivo = sum(efeito_competitivo),
      efeito_alocacao    = sum(efeito_alocacao),
      soma_componentes  = sum(soma_componentes),
      .groups = "drop"
    ) %>%
    mutate(
      # No agregado, o código é recalculado com base no efeito competitivo total
      # e na comparação T0_total vs T0_hat_total (aqui, positivo se acima da média)
      codigo_agregado = classificar_alocacao(efeito_competitivo,
                                             T0_total,
                                             T0_total * G_AREA)  
      # nota: T0_hat agregado ≈ T0_total * G_AREA é uma aproximação; 
      # o código por origem é mais informativo
    )
  
  list(
    detalhado  = resultados,
    agregado   = agregado,
    G_AREA     = G_AREA,
    T0_AREA    = T0_AREA,
    T1_AREA    = T1_AREA,
    ano_inicial = ano_inicial,
    ano_final   = ano_final,
    tipo        = "estatico"
  )
}


# =============================================================================
# VERSÃO 2: SHIFT-SHARE DINÂMICO (ENCADEADO ANUALMENTE)
# Para cada par de anos consecutivos (t, t+1), calcula os componentes
# shift-share e, ao final, soma os resultados ano a ano.
#
# Justificativa teórica/técnica:
#   A versão estática usa as chegadas do ano inicial como pesos fixos, o que
#   gera viés quando o período é longo ou a estrutura muda muito (Barff &
#   Knight, 1988). O encadeamento anual atualiza os pesos a cada período,
#   capturando mudanças graduais na estrutura de mercado.
#   Este procedimento é equivalente ao "dynamic shift-share" de Barff &
#   Knight (1988), citado por Toh et al. (2004) como alternativa mais robusta
#   para períodos longos.
#
# Limitação: a soma de períodos anuais não é equivalente a um único cálculo
# de longo prazo porque as taxas base mudam a cada ano. Os resultados devem
# ser interpretados como a contribuição acumulada de cada componente, não
# como decomposição de um único par de anos.
# =============================================================================

shift_share_dinamico <- function(dados,
                                 ano_inicio_analise = 2010,
                                 ano_fim_analise    = 2019) {
  
  # Anos de análise (pares consecutivos)
  anos <- seq(ano_inicio_analise - 1, ano_fim_analise - 1)
  # Ex.: para 2010-2019, os pares são (2009,2010), (2010,2011), ..., (2018,2019)
  # Isso requer que o ano (ano_inicio_analise - 1) esteja nos dados.
  
  todos_resultados <- vector("list", length(anos))
  
  for (k in seq_along(anos)) {
    ano_t0_k <- anos[k]
    ano_t1_k <- anos[k] + 1
    
    res_k <- shift_share_estatico(dados,
                                  ano_inicial = ano_t0_k,
                                  ano_final   = ano_t1_k)
    
    df_k <- res_k$detalhado %>%
      mutate(ano_t0 = ano_t0_k, ano_t1 = ano_t1_k)
    
    todos_resultados[[k]] <- df_k
  }
  
  # --- Empilhar todos os pares anuais ----------------------------------------
  detalhado_anual <- bind_rows(todos_resultados)
  
  # --- Somar os componentes ao longo dos anos por (destino, origem) ----------
  detalhado_acumulado <- detalhado_anual %>%
    group_by(destino, origem) %>%
    summarise(
      T0_inicial        = first(T0_ij),          # chegadas no 1º ano
      T1_final          = last(T1_ij),            # chegadas no último ano
      crescimento_real  = sum(crescimento_real),
      efeito_area       = sum(efeito_area),
      efeito_mix_regiao = sum(efeito_mix_regiao),
      efeito_competitivo = sum(efeito_competitivo),
      efeito_alocacao    = sum(efeito_alocacao),
      soma_componentes  = sum(soma_componentes),
      n_periodos        = n(),
      .groups = "drop"
    ) %>%
    # Código de alocação baseado na soma acumulada dos efeitos competitivo
    # e na diferença acumulada T0_ij - T0_hat_ij (aproximada pelo sinal
    # do efeito de alocação acumulado e do efeito competitivo acumulado)
    mutate(
      codigo = dplyr::case_when(
        efeito_competitivo >= 0 & efeito_alocacao >= 0 ~ "A,S",
        efeito_competitivo >= 0 & efeito_alocacao <  0 ~ "A,N",
        efeito_competitivo <  0 & efeito_alocacao <= 0 ~ "D,N",
        efeito_competitivo <  0 & efeito_alocacao >  0 ~ "D,S"
      )
    )
  
  # --- Resultado agregado por destino ----------------------------------------
  agregado_acumulado <- detalhado_acumulado %>%
    group_by(destino) %>%
    summarise(
      T0_inicial         = sum(T0_inicial),
      T1_final           = sum(T1_final),
      crescimento_real   = sum(crescimento_real),
      efeito_area        = sum(efeito_area),
      efeito_mix_regiao  = sum(efeito_mix_regiao),
      efeito_competitivo = sum(efeito_competitivo),
      efeito_alocacao    = sum(efeito_alocacao),
      soma_componentes   = sum(soma_componentes),
      .groups = "drop"
    ) %>%
    mutate(
      codigo_agregado = dplyr::case_when(
        efeito_competitivo >= 0 & efeito_alocacao >= 0 ~ "A,S",
        efeito_competitivo >= 0 & efeito_alocacao <  0 ~ "A,N",
        efeito_competitivo <  0 & efeito_alocacao <= 0 ~ "D,N",
        efeito_competitivo <  0 & efeito_alocacao >  0 ~ "D,S"
      )
    )
  
  list(
    detalhado_anual      = detalhado_anual,
    detalhado_acumulado  = detalhado_acumulado,
    agregado_acumulado   = agregado_acumulado,
    tipo                 = "dinamico",
    anos_analisados      = paste0(ano_inicio_analise, "-", ano_fim_analise)
  )
}


# =============================================================================
# EXECUÇÃO
# =============================================================================

# --- Versão estática (2010-2019) ---------------------------------------------
resultado_estatico <- shift_share_estatico(bd,
                                           ano_inicial = 2011,
                                           ano_final   = 2019)

cat("=== SHIFT-SHARE ESTÁTICO (2010-2019) ===\n")
cat(sprintf("Taxa de crescimento da área (G_AREA): %.4f (%.2f%%)\n\n",
            resultado_estatico$G_AREA,
            resultado_estatico$G_AREA * 100))

cat("--- Resultado DETALHADO (destino × origem) ---\n")
print(resultado_estatico$detalhado)

cat("\n--- Resultado AGREGADO por destino ---\n")
print(resultado_estatico$agregado)
View(resultado_estatico$agregado)


# --- Versão dinâmica (encadeamento anual 2010-2019; usa 2009 como base) ------
resultado_dinamico <- shift_share_dinamico(bd,
                                           ano_inicio_analise = 2010,
                                           ano_fim_analise    = 2019)

cat("\n\n=== SHIFT-SHARE DINÂMICO ENCADEADO (2010-2019) ===\n")

cat("--- Resultado ACUMULADO DETALHADO (destino × origem) ---\n")
print(resultado_dinamico$detalhado_acumulado)

cat("\n--- Resultado ACUMULADO AGREGADO por destino ---\n")
print(resultado_dinamico$agregado_acumulado)
View(resultado_dinamico$agregado_acumulado)

cat("\n--- Série anual completa (todos os pares de anos) ---\n")
print(resultado_dinamico$detalhado_anual)


# =============================================================================
# NOTAS METODOLÓGICAS
# =============================================================================
#
# 1. EFEITO ÁREA (Area-Wide Effect):
#    Quanto o destino j teria crescido se sua taxa fosse igual à do benchmark.
#    Representa a "quota de mercado" esperada.
#
# 2. EFEITO MIX-REGIÃO (Region-Mix Effect):
#    Positivo se a origem i cresce mais rápido que a média geral para o benchmark.
#    Indica se o destino está concentrado em origens dinâmicas.
#
# 3. EFEITO COMPETITIVO (Competitive Effect):
#    Positivo se o destino j cresce mais rápido que o benchmark para a origem i.
#    Indica vantagem competitiva específica naquela origem.
#
# 4. EFEITO ALOCAÇÃO (Allocation Effect - exclusivo de Esteban-Marquillas):
#    Captura a interação entre especialização e vantagem competitiva.
#    Positivo se o destino é especializado em origens onde tem vantagem (A,S)
#    ou não especializado onde tem desvantagem (D,N).
#
# 5. CÓDIGOS:
#    A,S = Competitive Advantage + Specialized     (alocação tende a +)
#    A,N = Competitive Advantage + Not Specialized (alocação tende a -)
#    D,S = Competitive Disadvantage + Specialized  (alocação tende a -)
#    D,N = Competitive Disadvantage + Not Specialized (alocação tende a +)
#
# 6. VERSÃO DINÂMICA vs. ESTÁTICA:
#    - Estática: um único par de anos (t0, t1); pesos fixos no t0.
#      Simples, comparável à literatura (Alavi & Yasin 2000; Sobral et al. 2007).
#    - Dinâmica encadeada: série de pares anuais somados.
#      Mais robusta para períodos longos; pesos atualizados anualmente.
#      Requer o ano anterior ao período de análise nos dados (aqui, 2009).
#
# 7. VERIFICAÇÃO DE CONSISTÊNCIA:
#    A coluna "soma_componentes" deve ser igual a "crescimento_real" em cada
#    linha (tolerância numérica de ~1e-6 aceitável).
# =============================================================================