# ============================================================
# EVENT-STUDY / SÉRIE TEMPORAL INTERROMPIDA - DADOS ANUAIS
# Banco: bd
# Variável dependente: chegadas
# Colunas esperadas: ano, chegadas
# ============================================================

library(dplyr)
library(fixest)
library(broom)
library(ggplot2)

# ------------------------------------------------------------
# 1. Preparação da base anual
# ------------------------------------------------------------

bd_anual2 <- bd_anual |> 
  filter(ano >= 2003,
         ano <= 2025) %>%
  arrange(ano) %>%
  mutate(
    t = row_number(),
    log_chegadas = log(chegadas),
    
    rel_copa     = ano - 2014,
    rel_recessao = ano - 2015,
    rel_rio      = ano - 2016,
    rel_pandemia = ano - 2020,
    
    post_copa     = ifelse(ano >= 2014, 1, 0),
    post_recessao = ifelse(ano >= 2015, 1, 0),
    post_rio      = ifelse(ano >= 2016, 1, 0),
    post_pandemia = ifelse(ano >= 2020, 1, 0)
  )

# ------------------------------------------------------------
# 2. Modelo anual conjunto
#    Atenção: poucos graus de liberdade.
#    Interpretação deve ser cautelosa.
# ------------------------------------------------------------

mod_anual_conjunto <- feols(
  chegadas ~ 
    t +
    post_copa + t:post_copa +
    post_recessao + t:post_recessao +
    post_rio + t:post_rio +
    post_pandemia + t:post_pandemia,
  data = bd_anual2,
  panel.id = ~ano+t,
  vcov = "NW"
)

mod_anual_conjunto <- feols(
  log_chegadas ~ 
    t +
    post_copa + t:post_copa +
    post_recessao + t:post_recessao +
    post_rio + t:post_rio +
    post_pandemia + t:post_pandemia,
  data = bd_anual2,
  vcov = "hetero"
)

summary(mod_anual_conjunto)

res_anual_conjunto <- tidy(mod_anual_conjunto) %>%
  mutate(
    efeito_percentual = 100 * (exp(estimate) - 1),
    ic_inf = 100 * (exp(estimate - 1.96 * std.error) - 1),
    ic_sup = 100 * (exp(estimate + 1.96 * std.error) - 1)
  )

print(res_anual_conjunto)

# ------------------------------------------------------------
# 3. Modelos anuais separados
#    Usar janelas mais largas, mas com cautela.
# ------------------------------------------------------------

# Copa 2014
bd_copa_anual <- bd_anual %>%
  filter(rel_copa >= -5, rel_copa <= 5)

mod_copa_anual <- feols(
  log_chegadas ~ i(rel_copa, ref = -1) + t,
  data = bd_copa_anual,
  vcov = "hetero"
)

summary(mod_copa_anual)
iplot(mod_copa_anual, main = "Event-study anual - Copa 2014")

# Recessão 2015
bd_recessao_anual <- bd_anual %>%
  filter(rel_recessao >= -5, rel_recessao <= 5)

mod_recessao_anual <- feols(
  log_chegadas ~ i(rel_recessao, ref = -1) + t,
  data = bd_recessao_anual,
  vcov = "hetero"
)

summary(mod_recessao_anual)
iplot(mod_recessao_anual, main = "Event-study anual - Recessão 2015")

# Rio 2016
bd_rio_anual <- bd_anual %>%
  filter(rel_rio >= -5, rel_rio <= 5)

mod_rio_anual <- feols(
  log_chegadas ~ i(rel_rio, ref = -1) + t,
  data = bd_rio_anual,
  vcov = "hetero"
)

summary(mod_rio_anual)
iplot(mod_rio_anual, main = "Event-study anual - Rio 2016")

# Pandemia 2020
bd_pandemia_anual <- bd_anual %>%
  filter(rel_pandemia >= -5, rel_pandemia <= 5)

mod_pandemia_anual <- feols(
  log_chegadas ~ i(rel_pandemia, ref = -1) + t,
  data = bd_pandemia_anual,
  vcov = "hetero"
)

summary(mod_pandemia_anual)
iplot(mod_pandemia_anual, main = "Event-study anual - Pandemia 2020")

# Diagnósticar autocorrelação
library(lmtest)

mod_lm <- lm(
  log_chegadas ~ 
    t +
    post_copa +
    post_recessao +
    post_rio +
    post_pandemia,
  data = bd_anual
)

bgtest(mod_lm, order = 1)
bgtest(mod_lm, order = 2)

acf(residuals(mod_lm))
