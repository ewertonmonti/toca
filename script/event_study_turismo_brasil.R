# EVENT STUDY / INTERRUPTED TIME SERIES (ITS)
# Chegadas turísticas internacionais no Brasil
# Eventos: Copa 2014 | Rio 2016 | Recessão 2015 | Pandemia 2020
# Recessão entre 2º trimestre de 2014 e 4º trimestre de 2016, segundo https://portalibre.fgv.br/codace


# Configurações iniciais----
library(tidyverse)
library(tsModel)     # harmonic() para sazonalidade de Fourier
library(lmtest)      # coeftest(), bgtest()
library(sandwich)    # vcovHAC() — erros-padrão robustos à autocorrelação
library(forecast)    # auto.arima(), checkresiduals()


# DADOS ANUAIS ----

bd <- readRDS("data/chegadas/chegadas.rds")
bd |> count(ano)
bda <- bd |> summarise(chegadas = sum(chegadas),.by = ano)

# 1.1 Construção das variáveis ITS para dados anuais
# Para cada evento, cria-se:
#   D_evento  = 1 a partir do ano do evento (dummy de nível / "level change")
#   T_evento  = anos decorridos desde o evento, 0 antes (dummy de inclinação)
# Referência: López Bernal et al. (2017), equação corrigida (2020):
#   Yt = β0 + β1·T + β2·D + β3·(T - T0)·D + ε
# Com múltiplos eventos, empilhamos os pares (D, slope) para cada um.

bd_anual <- bda |>
  mutate(
    t = row_number(),                  # tendência linear geral (T)

    # Copa do Mundo 2014
    D_copa      = as.integer(ano >= 2014),
    slope_copa  = pmax(0, ano - 2013),

    # Recessão 2015 (início oficial da recessão brasileira)
    D_rec       = as.integer(ano >= 2015),
    slope_rec   = pmax(0, ano - 2014),

    # Olimpíadas Rio 2016
    D_rio       = as.integer(ano >= 2016),
    slope_rio   = pmax(0, ano - 2015),

    # Pandemia COVID-19 2020
    D_pand      = as.integer(ano >= 2020),
    slope_pand  = pmax(0, ano - 2019)
  )

## Modelo ITS ----
# dados anuais (OLS com erros HAC)

# O modelo estima, para cada evento:
#   β_D_k    = mudança abrupta no nível (efeito imediato)
#   β_slope_k = mudança na tendência após o evento (efeito acumulado por ano)

fit_anual <- lm(
  chegadas ~ t +
    D_copa     + slope_copa  +
    D_rec      + slope_rec   +
    D_rio      + slope_rio   +
    D_pand     + slope_pand,
  data = bd_anual
)

summary(fit_anual)

# Erros-padrão robustos à autocorrelação e heterocedasticidade (Newey-West HAC)
coeftest(fit_anual, vcov = vcovHAC(fit_anual))

## Diagnóstico de autocorrelação ----

# Teste Breusch-Godfrey (H0: sem autocorrelação nos resíduos)
bgtest(fit_anual, order = 2)

# Se autocorrelação for detectada, ajustar com ARIMA nos resíduos:
# (ver Seção 1.5)

# Gráfico dos resíduos
checkresiduals(fit_anual$residuals)

## Visualização ----

bd_anual <- bd_anual |>
  mutate(
    fitted    = fitted(fit_anual),
    # Contrafactual: remove os efeitos dos eventos mantendo apenas t + intercepto
    contraf   = coef(fit_anual)["(Intercept)"] +
                coef(fit_anual)["t"] * t
  )

ggplot(bd_anual, aes(x = ano)) +
  geom_point(aes(y = chegadas), color = "black", size = 2) +
  geom_line(aes(y = fitted),  color = "steelblue",  linewidth = 1,
            linetype = "solid",  na.rm = TRUE) +
  geom_line(aes(y = contraf), color = "gray50",    linewidth = 0.8,
            linetype = "dashed", na.rm = TRUE) +
  geom_vline(xintercept = c(2014, 2015, 2016, 2020),
             linetype = "dotted", color = "red", alpha = 0.7) +
  annotate("text", x = c(2014, 2015, 2016, 2020),
           y = max(bd_anual$chegadas, na.rm = TRUE),
           label = c("Copa", "Rec.", "Rio", "COVID"),
           angle = 90, vjust = -0.3, hjust = 1.1,
           size = 3, color = "red") +
  labs(
    title    = "ITS — Chegadas turísticas internacionais ao Brasil (anual)",
    subtitle = "Linha azul: modelo ajustado | Linha cinza: contrafactual (sem eventos)",
    x = "Ano", y = "Chegadas internacionais"
  ) +
  theme_minimal()


# Modelo ARIMA com regressores ITS (Opcional) ----
# Use se bgtest() rejeitar H0, indicando autocorrelação nos resíduos do OLS.

# Versão com todos os regressores
regressores_anual <- bd_anual |>
  select(t, D_copa, slope_copa, D_rec, slope_rec,
         D_rio, slope_rio, D_pand, slope_pand) |>
  as.matrix()

# Não roda por multicolinearidade entre alguns regressores
fit_arima_anual <- auto.arima(
  bd_anual$chegadas,
  xreg     = regressores_anual,
  seasonal = FALSE,   # dados anuais: sem sazonalidade
  stepwise = FALSE,
  approximation = FALSE
)

# Identificando os regressores com multicolinearidade
alias(fit_anual)
names(which(is.na(coef(fit_anual))))

# Excluindo os regressores com multicolinearidade ("slope_rec" "slope_rio")
regressores_anual <- bd_anual |>
  select(t, 
         D_copa, slope_copa, 
         D_rec, 
         D_rio, 
         D_pand, slope_pand) |>
  as.matrix()

fit_arima_anual <- auto.arima(
  bd_anual$chegadas,
  xreg     = regressores_anual,
  seasonal = FALSE,   # dados anuais: sem sazonalidade
  stepwise = FALSE,
  approximation = FALSE
)

summary(fit_arima_anual)

# Excluindo todos os slopes
regressores_anual <- bd_anual |>
  select(t, 
         D_copa, 
         D_rec, 
         D_rio, 
         D_pand) |>
  as.matrix()

fit_arima_anual <- auto.arima(
  bd_anual$chegadas,
  xreg     = regressores_anual,
  seasonal = FALSE,   # dados anuais: sem sazonalidade
  stepwise = FALSE,
  approximation = FALSE
)

summary(fit_arima_anual)


# DADOS MENSAIS ----

## Construção das variáveis ITS ----
bd <- bd |>
  mutate(
    mes = case_match(
      mes,
      "janeiro"   ~ 1L,
      "fevereiro" ~ 2L,
      "marco"     ~ 3L,
      "abril"     ~ 4L,
      "maio"      ~ 5L,
      "junho"     ~ 6L,
      "julho"     ~ 7L,
      "agosto"    ~ 8L,
      "setembro"  ~ 9L,
      "outubro"   ~ 10L,
      "novembro"  ~ 11L,
      "dezembro"  ~ 12L
    )
  )

bdm <- bd |> summarise(chegadas = sum(chegadas),.by = c(ano,mes))

bd_mensal <- bdm |>
  arrange(ano, mes) |>
  mutate(
    # Índice de tempo (1, 2, 3, ...) — tendência linear
    t = row_number(),

    # Datas numéricas fracionadas para comparação
    data_frac = ano + (mes - 1) / 12,

    # Copa do Mundo: junho de 2014 (início do torneio: 12/06/2014)
    D_copa     = as.integer(data_frac >= 2014 + 5/12),
    slope_copa = pmax(0, t - (which(ano == 2014 & mes == 6)[1] - 1)),

    # Recessão: início em 2015-01
    D_rec      = as.integer(data_frac >= 2015),
    slope_rec  = pmax(0, t - (which(ano == 2015 & mes == 1)[1] - 1)),

    # Olimpíadas Rio: agosto de 2016 (início: 05/08/2016)
    D_rio      = as.integer(data_frac >= 2016 + 7/12),
    slope_rio  = pmax(0, t - (which(ano == 2016 & mes == 8)[1] - 1)),

    # Pandemia: março de 2020 (OMS declarou emergência: 11/03/2020)
    D_pand     = as.integer(data_frac >= 2020 + 2/12),
    slope_pand = pmax(0, t - (which(ano == 2020 & mes == 3)[1] - 1))
  )

## Sazonalidade ----

# Turismo tem sazonalidade forte. Usamos termos de Fourier (pares seno/cosseno)
# para modelá-la de forma flexível, sem incluir 11 dummies de mês.
# harmonic(t, nharmonics, period) do {tsModel} gera os termos.

# Transforma chegadas em objeto ts (necessário para harmonic())
chegadas_ts <- ts(bd_mensal$chegadas, start = c(min(bd_mensal$ano), 1), frequency = 12)

# Matriz de termos de Fourier (2 pares = 4 colunas)
fourier_mat <- harmonic(chegadas_ts, nfreq = 2, period = 12)
colnames(fourier_mat) <- c("sin1", "cos1", "sin2", "cos2")

bd_mensal <- bind_cols(bd_mensal, as_tibble(fourier_mat))


## Modelo ITS ----
# OLS com erros HAC

fit_mensal <- lm(
  chegadas ~ t + sin1 + cos1 + sin2 + cos2 +
    D_copa    + slope_copa  +
    D_rec     + slope_rec   +
    D_rio     + slope_rio   +
    D_pand    + slope_pand,
  data = bd_mensal
)

summary(fit_mensal)

# Erros-padrão robustos HAC (Newey-West)
coeftest(fit_mensal, vcov = vcovHAC(fit_mensal))


## Diagnóstico de autocorrelação
# Para dados mensais, testar até ordem 12 (1 ano) (H0: sem autocorrelação nos resíduos)
bgtest(fit_mensal, order = 12)

checkresiduals(fit_mensal$residuals)


## Visualização ----

bd_mensal <- bd_mensal |>
  mutate(
    data_plot = as.Date(paste(ano, mes, "01", sep = "-")),
    fitted    = fitted(fit_mensal),
    # Contrafactual: apenas tendência + sazonalidade (sem eventos)
    contraf   = coef(fit_mensal)["(Intercept)"] +
                coef(fit_mensal)["t"]    * t    +
                coef(fit_mensal)["sin1"] * sin1 +
                coef(fit_mensal)["cos1"] * cos1 +
                coef(fit_mensal)["sin2"] * sin2 +
                coef(fit_mensal)["cos2"] * cos2
  )

datas_eventos <- as.Date(c("2014-06-01", "2015-01-01", "2016-08-01", "2020-03-01"))
labels_eventos <- c("Copa 2014", "Recessão 2015", "Rio 2016", "COVID-19 2020")

ggplot(bd_mensal, aes(x = data_plot)) +
  geom_point(aes(y = chegadas), color = "black", size = 0.8, alpha = 0.6) +
  geom_line(aes(y = fitted),  color = "steelblue", linewidth = 0.9,
            linetype = "solid",  na.rm = TRUE) +
  geom_line(aes(y = contraf), color = "gray50",   linewidth = 0.7,
            linetype = "dashed", na.rm = TRUE) +
  geom_vline(xintercept = datas_eventos,
             linetype = "dotted", color = "red", alpha = 0.7) +
  annotate("text", x = datas_eventos,
           y = max(bd_mensal$chegadas, na.rm = TRUE),
           label = labels_eventos,
           angle = 90, vjust = -0.3, hjust = 1.1,
           size = 2.8, color = "red") +
  labs(
    title    = "ITS — Chegadas turísticas internacionais ao Brasil (mensal)",
    subtitle = "Linha azul: modelo ajustado | Linha cinza: contrafactual (tendência + sazonalidade)",
    x = "Data", y = "Chegadas internacionais"
  ) +
  theme_minimal()

## Modelo SARIMA com regressores ITS (Opcional) ----
# Use se bgtest() rejeitar H0 nos resíduos do OLS mensal.
# auto.arima() detecta automaticamente a estrutura ARIMA + sazonal.

regressores_mensal <- bd_mensal |>
  select(t, sin1, cos1, sin2, cos2,
         D_copa, slope_copa, D_rec, slope_rec,
         D_rio, slope_rio, D_pand, slope_pand) |>
  as.matrix()

fit_sarima_mensal <- auto.arima(
  chegadas_ts,
  xreg        = regressores_mensal,
  seasonal    = TRUE,
  stepwise    = FALSE,
  approximation = FALSE
)

summary(fit_sarima_mensal)


# TABELA-RESUMO DOS EFEITOS ESTIMADOS ----

# Extrai e organiza os coeficientes dos dois modelos OLS principais
# para leitura direta dos efeitos estimados por evento.

extrair_efeitos <- function(modelo, label) {
  cf <- coeftest(modelo, vcov = vcovHAC(modelo))
  eventos <- c("copa", "rec", "rio", "pand")
  nomes   <- c("Copa 2014", "Recessão 2015", "Olimpíadas 2016", "Pandemia 2020")

  map2_dfr(eventos, nomes, function(ev, nm) {
    d_var <- paste0("D_",     ev)
    s_var <- paste0("slope_", ev)
    tibble(
      modelo        = label,
      evento        = nm,
      nivel_est     = cf[d_var, "Estimate"],
      nivel_p       = cf[d_var, "Pr(>|t|)"],
      inclinacao_est = cf[s_var, "Estimate"],
      inclinacao_p   = cf[s_var, "Pr(>|t|)"]
    )
  })
}

tabela_efeitos <- bind_rows(
  extrair_efeitos(fit_anual,  "Anual"),
  extrair_efeitos(fit_mensal, "Mensal")
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(tabela_efeitos)


