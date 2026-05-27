
install.packages("CausalImpact")
library(CausalImpact)

# Define a janela pré e pós evento (índices na série mensal)
# Pandemia
pre_period  <- c(1, 134)
post_period <- c(135, nrow(bd_mensal))

# Copa
pre_period  <- c(1, 65)
post_period <- c(66, 136)

# Olimpíada
pre_period  <- c(1, 91)
post_period <- c(92, 136)

# Recessão (difícil definir a data, usei 01/2015)
pre_period  <- c(1, 72)
post_period <- c(73, 136)

impact <- CausalImpact(
  data       = bd_mensal$chegadas,
  pre.period  = pre_period,
  post.period = post_period
)

summary(impact)
plot(impact)
summary(impact, "report")


install.packages("segmented")
library(segmented)

fit_base <- lm(chegadas ~ t, data = bd_mensal)

fit_seg <- segmented(
  fit_base,
  seg.Z = ~t,
  psi = c(136, 145, 163, 207)  # chutes iniciais para os breakpoints em t
  # (índices aproximados de jun/2014, jan/2015,
  #  ago/2016, mar/2020 na sua série mensal)
)

summary(fit_seg)
slope(fit_seg)     # inclinações por segmento

install.packages("strucchange")
library(strucchange)

# Testa a presença e localização de quebras estruturais
bp <- breakpoints(chegadas ~ t, data = bd_mensal, breaks = 4)
summary(bp)
plot(bp)

breakdates(bp)

# Constrói o modelo com as quebras detectadas
coef(bp)


install.packages("mcp")
library(mcp)

model <- list(
  chegadas ~ 1 + t,          # segmento 1: antes da Copa
  ~ 1 + t,                   # segmento 2: Copa → Recessão
  ~ 1 + t,                   # segmento 3: Recessão → Rio
  ~ 1 + t,                   # segmento 4: Rio → Pandemia
  ~ 1 + t                    # segmento 5: pós-Pandemia
)

fit_mcp <- mcp(model, data = bd_mensal)
summary(fit_mcp)
plot(fit_mcp)

