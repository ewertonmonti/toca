# SCRIPT COMPLETO – ECONOMETRIC SHIFT-SHARE (FIRGO & FRITZ)

# Configurações iniciais ----
rm(list = ls(all=T))

packages <- c(
  "tidyverse",
  "readxl",
  "lubridate",
  "restriktor"
)

lapply(packages, require, character.only = TRUE)

############################################################
# 1. Meus parâmetros
############################################################

ano_inicio <- 2003
ano_fim <- 2019

concorrentes_diretos <- c("Brazil",
                          "Peru",
                          "Dominican Republic",
                          "Colombia",
                          "Spain",
                          "Chile",
                          "Costa Rica",
                          "Jamaica",
                          "Cuba")
# Excluí Argentina e México por não terem dados para todos os mercados e anos

# Regiões UN Tourism
# 10000	Africa (UNWTO total)
# 20000	Americas (UNWTO total)
# 30000	East Asia and the Pacific (UNWTO total)
# 40000	Europe (UNWTO total)
# 50000	Middle East (UNWTO total)
# 60000	South Asia (UNWTO total)
# 70000	Other not classified (UNWTO total)

regioes_selecionadas <- c(20000, 30000, 40000)


pais_foco <- "Brazil"

############################################################
# 2. LEITURA DOS DADOS
############################################################


raw <- read_excel("data/UN_Tourism_inbound_arrivals_by_region_10_2025.xlsx", sheet = "Data",
                  col_types = c("text", "text", "text", "numeric", "text", "numeric",
                                "text","numeric","numeric","text","text","text",
                                "text","text"))

raw |> count(indicator_code, indicator_label)
  

df <- raw %>% filter(indicator_code == "INBD_TRIP_REGN_TOUR", partner_area_code %in% regioes_selecionadas) |> 
  rename(
    country = reporter_area_label,
    region  = partner_area_label,
    year    = year,
    arrivals = value
  ) %>%
  filter(
    country %in% c(pais_foco, concorrentes_diretos),
    (year >= ano_inicio - 1 & year <= ano_fim)
  ) %>%
  arrange(country, region, year) |> 
  dplyr::select(country, region, year, arrivals) |> 
  mutate(region = case_when(
    region == "Americas (UNWTO total)" ~ "Americas",
    region == "East Asia and the Pacific (UNWTO total)" ~ "EastAsiaPacific",
    region == "Europe (UNWTO total)" ~ "Europe",
  ))

# Checagem de observações
df |> group_by(country, year) |> count() |> 
  pivot_wider(id_cols = year, names_from = country, values_from = n)


############################################################
# 3. TAXA DE CRESCIMENTO
############################################################

df <- df %>%
  group_by(country, region) %>%
  arrange(year) %>%
  mutate(
    growth = (arrivals / lag(arrivals)) - 1
  ) %>%
  ungroup() %>%
  filter(!is.na(growth))

############################################################
# 4. ESTRUTURA FINAL PARA ESTIMAÇÃO
############################################################

# Mapeamento conceitual:
# Artigo -> Aqui
# i (grupo) -> region
# n (região) -> country
# t	-> year
# e(i,n,t) ->	growth

df_est <- df %>%
  mutate(
    region  = factor(region),
    country = factor(country),
    year    = factor(year)
  )

############################################################
# 5. MODELO COMPLETO, SEM RESTRIÇÕES, EQUAÇÃO 1
############################################################

mod <- lm(
  growth ~
    region +
    region:country +
    year +
    region:year +
    country:year - 1,
  data = df_est
)

summary(mod)


############################################################
# 6. PESOS
############################################################

df_weights <- df_est %>%
  mutate(arrivals = as.numeric(arrivals))

############################################################
# 7. CONSTRUÇÃO DAS RESTRIÇÕES R1–R6
############################################################

# nomes dos coeficientes do modelo
coef_names <- names(coef(mod))
K <- length(coef_names)

# função auxiliar: cria UMA linha da matriz A
make_A_row <- function(terms, weights, coef_names) {
  
  Arow <- rep(0, length(coef_names))
  names(Arow) <- coef_names
  
  keep <- terms %in% coef_names
  if (!any(keep)) return(NULL)
  
  Arow[terms[keep]] <- weights[keep]
  Arow
}

# lista que vai acumular as linhas da matriz A
A_list <- list()


# R1 — region:country (m(i,n))
R1 <- df_weights %>%
  group_by(region, country) %>%
  summarise(w = sum(arrivals), .groups = "drop") %>%
  group_by(region) %>%
  mutate(a = w / sum(w))

for (r in unique(R1$region)) {
  
  tmp <- R1 %>% filter(region == r)
  
  terms <- paste0(
    "region", as.character(r), ":country", as.character(tmp$country)
  )
  
  Arow <- make_A_row(terms, tmp$a, coef_names)
  
  if (!is.null(Arow)) {
    A_list[[length(A_list) + 1]] <- Arow
  }
}


# R2 — region:year (f(i,t)) média zero por ano
R2 <- df_weights %>%
  group_by(region, year) %>%
  summarise(w = sum(arrivals), .groups = "drop") %>%
  group_by(year) %>%
  mutate(a = w / sum(w))

for (t in unique(R2$year)) {
  
  tmp <- R2 %>% filter(year == t)
  
  terms <- paste0(
    "region", as.character(tmp$region), ":year", as.character(t)
  )
  
  Arow <- make_A_row(terms, tmp$a, coef_names)
  
  if (!is.null(Arow)) {
    A_list[[length(A_list) + 1]] <- Arow
  }
}


# # R3 — region:year média zero ao longo do tempo
# R3 <- df_weights %>%
#   group_by(region, year) %>%
#   summarise(w = sum(arrivals), .groups = "drop") %>%
#   group_by(region) %>%
#   mutate(a = w / sum(w))
# 
# for (r in unique(R3$region)) {
#   
#   tmp <- R3 %>% filter(region == r)
#   
#   terms <- paste0(
#     "region", as.character(r), ":year", as.character(tmp$year)
#   )
#   
#   Arow <- make_A_row(terms, tmp$a, coef_names)
#   
#   if (!is.null(Arow)) {
#     A_list[[length(A_list) + 1]] <- Arow
#   }
# }


# R4 — country:year média zero por ano
R4 <- df_weights %>%
  group_by(country, year) %>%
  summarise(w = sum(arrivals), .groups = "drop") %>%
  group_by(year) %>%
  mutate(a = w / sum(w))

for (t in unique(R4$year)) {
  
  tmp <- R4 %>% filter(year == t)
  
  terms <- paste0(
    "country", as.character(tmp$country), ":year", as.character(t)
  )
  
  Arow <- make_A_row(terms, tmp$a, coef_names)
  
  if (!is.null(Arow)) {
    A_list[[length(A_list) + 1]] <- Arow
  }
}

# # R5 — country:year média zero ao longo do tempo
# R5 <- df_weights %>%
#   group_by(country, year) %>%
#   summarise(w = sum(arrivals), .groups = "drop") %>%
#   group_by(country) %>%
#   mutate(a = w / sum(w))
# 
# for (c in unique(R5$country)) {
#   
#   tmp <- R5 %>% filter(country == c)
#   
#   terms <- paste0(
#     "country", as.character(c), ":year", as.character(tmp$year)
#   )
#   
#   Arow <- make_A_row(terms, tmp$a, coef_names)
#   
#   if (!is.null(Arow)) {
#     A_list[[length(A_list) + 1]] <- Arow
#   }
# }
# 
# # R6 — year (ciclo agregado)
# R6 <- df_weights %>%
#   group_by(year) %>%
#   summarise(w = sum(arrivals), .groups = "drop") %>%
#   mutate(a = w / sum(w))
# 
# terms <- paste0("year", as.character(R6$year))
# 
# Arow <- make_A_row(terms, R6$a, coef_names)
# 
# if (!is.null(Arow)) {
#   A_list[[length(A_list) + 1]] <- Arow
# }



############################################################
# 8. ESTIMAÇÃO FINAL COM RESTRIÇÕES (FORMA CORRETA)
############################################################

A_mat <- do.call(rbind, lapply(A_list, function(x) x[coef_names]))

A_mat_full <- matrix(0, nrow(A_mat), length(coef(mod)))
colnames(A_mat_full) <- names(coef(mod))
A_mat_full[, colnames(A_mat)] <- A_mat

# vetor b 
b_vec <- rep(0, nrow(A_mat_full))

# res <- restriktor(
#   mod,
#   constraints = A_mat_full,
#   rhs = b_vec,
#   se = "boot.model.based",
#   mix_weights = "boot",
#   B = 200   # por exemplo
# )

res <- restriktor(
  mod,
  constraints  = A_mat_full,
  rhs          = b_vec,
  se           = "none",
  mix_weights  = "none"
)


summary(res)

############################################################
# 9. CRESCIMENTO VIRTUAL (Equações 2 e 3)
############################################################

# coeficientes restritos
beta <- coef(res)

# transformar em tibble
coef_tbl <- tibble(
  term = names(beta),
  value = as.numeric(beta)
)

# separar componentes
m_in <- coef_tbl %>%
  filter(str_detect(term, "region.*:country")) %>%
  separate(term, into = c("region", "country"), sep = ":country") %>%
  mutate(
    region  = str_remove(region, "region"),
    country = country
  )

f_it <- coef_tbl %>%
  filter(str_detect(term, "region.*:year")) %>%
  separate(term, into = c("region", "year"), sep = ":year") %>%
  mutate(
    region = str_remove(region, "region"),
    year   = as.integer(year)
  )

g_nt <- coef_tbl %>%
  filter(str_detect(term, "country.*:year")) %>%
  separate(term, into = c("country", "year"), sep = ":year") %>%
  mutate(
    country = str_remove(country, "country"),  # <<< ESSENCIAL
    year    = as.integer(year)
  )


h_t <- coef_tbl %>%
  filter(str_detect(term, "^year")) %>%
  mutate(year = as.integer(str_remove(term, "year")))


############################################################
# 10. INDICADOR W Equação 4)
############################################################

# Construção dos pesos
weights_itn <- df_weights %>%
  group_by(country, year) %>%
  mutate(
    s = arrivals / sum(arrivals)
  ) %>%
  ungroup()

# Juntar componentes e calcular W
weights_itn <- weights_itn %>%
  mutate(year = as.integer(as.character(year)))

f_it <- f_it %>%
  mutate(year = as.integer(year))

g_nt <- g_nt %>%
  mutate(year = as.integer(year))

weights_itn <- weights_itn %>%
  mutate(region = as.character(region),
         country = as.character(country))

f_it <- f_it %>%
  mutate(region = as.character(region))

g_nt <- g_nt %>%
  mutate(country = as.character(country))

# Teste
anti_join(weights_itn, f_it, by = c("region", "year"))

test_f <- weights_itn %>%
  left_join(f_it, by = c("region", "year"))

summary(test_f$value)

test_g <- weights_itn %>%
  left_join(g_nt, by = c("country", "year"))

summary(test_g$value)



W_df <- weights_itn %>%
  left_join(f_it, by = c("region", "year")) %>%
  rename(f_it = value) %>%
  left_join(g_nt, by = c("country", "year")) %>%
  rename(g_nt = value) %>%
  mutate(
    contrib = s * (f_it + g_nt)
  ) %>%
  group_by(country, year) %>%
  summarise(
    W = sum(contrib, na.rm = TRUE),
    .groups = "drop"
  )

summary(W_df$W)


###########################################################
# 11. RESULTADOS-CHAVE
############################################################

# Brasil
W_brazil <- W_df %>%
  filter(country == "Brazil")

# Concorrentes individuais (excluindo Brasil)
W_competitors <- W_df %>%
  filter(country != "Brazil")

# Agregado dos concorrentes (excluindo o Brasil)
W_competitors_agg <- W_competitors %>%
  group_by(year) %>%
  summarise(
    W = mean(W),
    .groups = "drop"
  )

# Gráfico comparativo
ggplot() +
  geom_line(data = W_brazil, aes(year, W, color = "Brazil"), linewidth = 1.2) +
  geom_line(data = W_competitors_agg, aes(year, W, color = "Competitors (avg)"),
            linetype = "dashed", linewidth = 1) +
  labs(
    x = NULL,
    y = "W(t)",
    color = NULL
  ) +
  theme_minimal()



W_country → Brasil vs. cada concorrente

W_concorrentes → Brasil vs. bloco concorrente

coeficientes country:year → choques idiossincráticos

coeficientes region:country → especialização estrutural

12) INTERPRETAÇÃO (CONSISTENTE COM FIRGO & FRITZ)

W > 1: desempenho acima do esperado dado o mix regional

W < 1: perda de competitividade relativa

comparação Brasil vs. agregado evita viés de país individual