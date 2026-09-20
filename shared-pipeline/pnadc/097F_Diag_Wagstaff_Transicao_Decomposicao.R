# ==============================================================================
# SCRIPT: 097F_Diag_Wagstaff_Transicao_Decomposicao.R
# ODA: portado de 4-DA-Code/2026-06_Harmonizing-BR-Data/R/02_validation/ em 2026-09-20;
#      caminhos relativos a data-raw/. Upstream do post wagstaff-transicao-condicional.
#
# DIAGNOSTICO (nao e figura de tese; nada vai para final/ nem para os .qmd).
# Pergunta do autor (2026-09-15): as series de Wagstaff do superior (097D) e da
# conclusao do EM (097E) andam juntas -- e endogeneidade ou mecanismo comum?
#
# Duas ligacoes mecanicas que este script mede, por ano e faixa etaria:
#
# (A) TRANSICAO ENTRE ELEGIVEIS. ens_sup ⊆ medio_completo por construcao (D19),
#     entao a desigualdade de acesso ao superior = desigualdade de
#     elegibilidade (concluir o EM) + desigualdade da transicao condicional.
#     Serie nova: W de ens_sup calculado SO entre quem tem medio_completo == 1,
#     com o rank de renda recomputado dentro dessa subpopulacao (indice
#     condicional padrao). Se ela cai em paralelo com W(sup), ha mecanismo
#     proprio do superior; se fica plana, a equalizacao do acesso terciario e
#     heranca do funil do EM.
#
# (B) DECOMPOSICAO EXATA DE C(EM). O indice de concentracao C = 2 cov(y, r)/mu
#     e linear em y para rank fixo, entao para medio_completo = ens_sup +
#     (medio_completo & !ens_sup):
#         C_EM * mu_EM = C_sup * mu_sup + C_EMso * mu_EMso
#     => C_EM = (mu_sup/mu_EM) * C_sup + (mu_EMso/mu_EM) * C_EMso.
#     A parcela (mu_sup/mu_EM) * C_sup / C_EM e quanto do indice do EM E o
#     indice do superior, por construcao. Verificado numericamente
#     (stop() se a soma nao fechar).
#
# Metodo: mesmo estimador do 097D/097E (Wagstaff via GLM ponderado, Kish
# DEFF = 2.0), com C devolvido explicitamente. Mesmo filtro de fonte
# (ANO_SPLICE = 2015), mesmas faixas etarias do 097E (18-24 e 25-64).
#
# FONTES:   output/Microdados_Todas_Idades_1992_2025.parquet
# SAIDAS:   output/097F_diag_wagstaff_transicao_decomposicao.rds (tabela)
#           6-images-tables/graphs/<timestamp>_097F_Diag_*.png (2 figuras)
# VER TAMBEM: 097D, 097E
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(survey)
  library(here)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 11))
options(survey.lonely.psu = "adjust")

BASE_DIR <- here::here("data-raw", "harmonizing-br-data")
IDADE_MIN_ADULTO <- 25L
IDADE_MAX_ADULTO <- 64L
LBL_JOVEM  <- "Ages 18-24"
LBL_ADULTO <- sprintf("Ages %d-%d", IDADE_MIN_ADULTO, IDADE_MAX_ADULTO)
ANO_SPLICE <- 2015L

# ==============================================================================
# 1. ESTIMADOR -- copia do 097D, devolvendo tambem C e mu
# ==============================================================================
weighted_frac_rank <- function(x, w) {
  Nw <- sum(w)
  ord <- order(x)
  x_s <- x[ord]
  w_s <- w[ord]
  cumw <- cumsum(w_s)
  r_s <- (cumw - w_s / 2) / Nw
  runs <- rle(x_s)
  pos <- 1L
  for (k in seq_along(runs$lengths)) {
    len <- runs$lengths[k]
    if (len > 1L) {
      idx <- pos:(pos + len - 1L)
      r_s[idx] <- weighted.mean(r_s[idx], w_s[idx])
    }
    pos <- pos + len
  }
  r <- numeric(length(x))
  r[ord] <- r_s
  r
}

vazio <- function() data.frame(mu = NA, C = NA, se_C = NA, W = NA, se_W = NA)

calc_C_W <- function(y, w, x_income) {
  ok <- !is.na(y) & !is.na(w) & !is.na(x_income) & w > 0
  y <- y[ok]; w <- w[ok]; x <- x_income[ok]
  if (length(y) < 20) return(vazio())
  mu <- weighted.mean(y, w)
  if (mu <= 0 | mu >= 1) return(vazio())
  r <- weighted_frac_rank(x, w)
  var_r <- weighted.mean((r - 0.5)^2, w)
  y_star <- 2 * var_r * (y / mu)
  des <- svydesign(ids = ~1, weights = ~w, data = data.frame(y_star = y_star, r = r, w = w))
  fit <- svyglm(y_star ~ r, design = des)
  C <- as.numeric(coef(fit)["r"])
  se_C <- as.numeric(SE(fit)["r"]) * sqrt(2.0)
  data.frame(mu = mu, C = C, se_C = se_C, W = C / (1 - mu), se_W = se_C / (1 - mu))
}

# ==============================================================================
# 2. CALCULO -- por ano x faixa: sup, EM, EM-so, sup|EM
# ==============================================================================
FORCE_RECALC <- FALSE
CACHE <- file.path(BASE_DIR, "output", "097F_diag_wagstaff_transicao_decomposicao.rds")

if (file.exists(CACHE) && !FORCE_RECALC) {
  cat("Lendo cache:", CACHE, "\n")
  res <- readRDS(CACHE)
} else {
  cat("Carregando microdados...\n")
  df <- read_parquet(
    file.path(BASE_DIR, "output/Microdados_Todas_Idades_1992_2025.parquet"),
    col_select = c("ano", "fonte", "idade", "peso", "renda_dom_pcta",
                   "renda_real", "medio_completo", "ens_sup")
  )
  base <- df |>
    filter(!is.na(peso), !is.na(renda_real), !is.na(renda_dom_pcta),
           renda_dom_pcta < 99999999999, renda_real > 0,
           !(fonte == "PNAD Contínua" & ano <= ANO_SPLICE),
           !is.na(medio_completo), !is.na(ens_sup)) |>
    mutate(em_so = as.integer(medio_completo == 1L & ens_sup == 0L))

  # Sanidade da hierarquia D19 (ens_sup ⊆ medio_completo) -- a decomposicao
  # so vale se isso for exato.
  viol <- base |> filter(ens_sup == 1L, medio_completo == 0L) |> nrow()
  if (viol > 0) stop("Hierarquia violada: ", viol, " linhas com ens_sup=1 e medio_completo=0")

  faixas <- list(
    list(lbl = LBL_JOVEM,  min = 18L, max = 24L),
    list(lbl = LBL_ADULTO, min = IDADE_MIN_ADULTO, max = IDADE_MAX_ADULTO)
  )

  res_list <- list()
  for (ano_cor in sort(unique(base$ano))) {
    d_ano <- base |> filter(ano == ano_cor)
    for (f in faixas) {
      d <- d_ano |> filter(idade >= f$min, idade <= f$max)
      d_eleg <- d |> filter(medio_completo == 1L)
      r_sup  <- calc_C_W(d$ens_sup,        d$peso, d$renda_real)
      r_em   <- calc_C_W(d$medio_completo, d$peso, d$renda_real)
      r_emso <- calc_C_W(d$em_so,          d$peso, d$renda_real)
      r_cond <- calc_C_W(d_eleg$ens_sup,   d_eleg$peso, d_eleg$renda_real)
      res_list[[length(res_list) + 1]] <- bind_rows(
        cbind(serie = "Tertiary access (all)",            r_sup),
        cbind(serie = "Upper secondary completion (all)", r_em),
        cbind(serie = "Completed US, no tertiary",        r_emso),
        cbind(serie = "Tertiary access | completed US",   r_cond)
      ) |> mutate(ano = ano_cor, faixa = f$lbl, .before = 1)
    }
    cat(sprintf("  %d ok\n", ano_cor))
  }
  res <- bind_rows(res_list)
  dir.create(dirname(CACHE), showWarnings = FALSE, recursive = TRUE)
  saveRDS(res, CACHE)
}

# ==============================================================================
# 3. DECOMPOSICAO EXATA + VERIFICACAO
# ==============================================================================
dec <- res |>
  select(ano, faixa, serie, mu, C) |>
  pivot_wider(names_from = serie, values_from = c(mu, C)) |>
  rename(mu_sup = `mu_Tertiary access (all)`, C_sup = `C_Tertiary access (all)`,
         mu_em = `mu_Upper secondary completion (all)`, C_em = `C_Upper secondary completion (all)`,
         mu_emso = `mu_Completed US, no tertiary`, C_emso = `C_Completed US, no tertiary`,
         mu_cond = `mu_Tertiary access | completed US`, C_cond = `C_Tertiary access | completed US`) |>
  mutate(
    contrib_sup  = (mu_sup / mu_em) * C_sup,
    contrib_emso = (mu_emso / mu_em) * C_emso,
    soma         = contrib_sup + contrib_emso,
    erro         = soma - C_em,
    share_sup    = contrib_sup / C_em,     # fracao de C_EM que E C_sup, por construcao
    peso_sup     = mu_sup / mu_em          # fracao dos concluintes que entraram no superior
  )

max_erro <- max(abs(dec$erro), na.rm = TRUE)
cat(sprintf("\nVerificacao da decomposicao: |erro| maximo = %.2e\n", max_erro))
# Tolerancia: a GLM estima C por regressao, nao por covariancia direta; a
# identidade vale exatamente para a covariancia e, com svyglm em y* sem
# intercepto restrito, fecha em ~1e-12. Qualquer coisa acima de 1e-6 indica
# que os ranks nao sao os mesmos entre as tres series.
if (max_erro > 1e-6) stop("Decomposicao nao fecha: |erro| max = ", max_erro)

cat("\nTABELA -- anos-marco (W = Wagstaff; share_sup = parcela de C_EM que e C_sup por construcao):\n")
tab <- res |>
  select(ano, faixa, serie, mu, W) |>
  pivot_wider(names_from = serie, values_from = c(mu, W)) |>
  left_join(dec |> select(ano, faixa, share_sup, peso_sup), by = c("ano", "faixa")) |>
  filter(ano %in% c(1992L, 1999L, 2002L, 2008L, 2014L, 2019L, 2025L)) |>
  transmute(ano, faixa,
            mu_sup = round(`mu_Tertiary access (all)`, 3),
            W_sup = round(`W_Tertiary access (all)`, 3),
            W_em = round(`W_Upper secondary completion (all)`, 3),
            mu_cond = round(`mu_Tertiary access | completed US`, 3),
            W_sup_dado_em = round(`W_Tertiary access | completed US`, 3),
            peso_sup = round(peso_sup, 2),
            share_sup = round(share_sup, 2)) |>
  arrange(faixa, ano)
print(as.data.frame(tab), row.names = FALSE)

# ==============================================================================
# 4. FIGURA 1 -- tres series de W por faixa etaria
# ==============================================================================
NIV <- c("Upper secondary completion (all)", "Tertiary access (all)",
         "Tertiary access | completed US")
COR <- setNames(PALETA_QUALITATIVA[1:3], NIV)
d1 <- res |> filter(serie %in% NIV) |>
  mutate(serie = factor(serie, levels = NIV),
         lower = W - 1.96 * se_W, upper = W + 1.96 * se_W,
         seg = cumsum(c(1L, as.integer(diff(ano) > 1L))), .by = c(faixa, serie))

p1 <- ggplot(d1, aes(x = ano, y = W, colour = serie, group = interaction(serie, seg))) +
  geom_vline(xintercept = 2015.5, colour = "#9A9A9A", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = serie), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_wrap(~faixa, ncol = 1) +
  scale_colour_manual(values = COR, name = NULL) +
  scale_fill_manual(values = COR, name = NULL, guide = "none") +
  scale_x_anos_tese(anos = c(ANOS_TRANSICAO_GOV, max(res$ano))) +
  labs(x = "Year", y = "Wagstaff index (W)",
       title = "Diagnostic 097F (A): is tertiary equalisation inherited from the US funnel?",
       subtitle = paste0("Green = W of tertiary access among those who completed upper secondary
",
                         "(rank recomputed within eligibles). Vertical line = PNAD/PNADC splice.")) +
  guides(colour = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11), plot.subtitle = element_text(size = 8))

salvar_grafico(p1, prefixo = "097F_Diag_A_Wagstaff_Transicao_Condicional",
               largura = LARGURA_TEXTO, altura = 7.0, unidades = "in", formato = "png")

# ==============================================================================
# 5. FIGURA 2 -- decomposicao de C(EM): contribuicao de cada parcela
# ==============================================================================
d2 <- dec |>
  select(ano, faixa, C_em, contrib_sup, contrib_emso, share_sup, peso_sup) |>
  pivot_longer(c(contrib_sup, contrib_emso), names_to = "parcela", values_to = "contrib") |>
  mutate(parcela = factor(parcela, levels = c("contrib_emso", "contrib_sup"),
                          labels = c("Completed US, no tertiary: (mu_emso/mu_em) x C_emso",
                                     "Tertiary access: (mu_sup/mu_em) x C_sup")))

p2a <- ggplot(d2, aes(x = ano, y = contrib, fill = parcela)) +
  geom_col(width = 0.8) +
  geom_line(data = dec, aes(x = ano, y = C_em), inherit.aes = FALSE,
            colour = "black", linewidth = 0.5) +
  geom_point(data = dec, aes(x = ano, y = C_em), inherit.aes = FALSE,
             colour = "black", size = 1.1) +
  facet_wrap(~faixa, ncol = 1) +
  scale_fill_manual(values = c(PALETA_QUALITATIVA[[3]], PALETA_QUALITATIVA[[2]]), name = NULL) +
  # limits explicitos: scale_x_anos_tese fixa limits = c(min, max) e as barras
  # de 1992/2025 (largura 0,8) vazavam para fora e eram descartadas (oob).
  scale_x_anos_tese(anos = c(ANOS_TRANSICAO_GOV, max(res$ano)),
                    limits = c(min(res$ano) - 0.6, max(res$ano) + 0.6),
                    expand = expansion(add = c(0.4, 0.4))) +
  labs(x = NULL, y = "Concentration index C(US completion)",
       title = "Diagnostic 097F (B): exact decomposition of C(US completion)",
       subtitle = paste0("Bars sum exactly to C_em (black line). Blue = the part of C_em that IS
",
                         "C(tertiary access), by construction (ens_sup within medio_completo).")) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11), plot.subtitle = element_text(size = 8))

d3 <- dec |> select(ano, faixa, share_sup, peso_sup) |>
  pivot_longer(c(share_sup, peso_sup), names_to = "q", values_to = "v") |>
  mutate(q = factor(q, levels = c("peso_sup", "share_sup"),
                    labels = c("mu_sup / mu_em (completers who entered tertiary)",
                               "share of C_em from the tertiary term")))
p2b <- ggplot(d3, aes(x = ano, y = v, colour = q)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.4) +
  facet_wrap(~faixa, ncol = 1) +
  scale_colour_manual(values = c("grey40", PALETA_QUALITATIVA[[2]]), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  # limits explicitos: scale_x_anos_tese fixa limits = c(min, max) e as barras
  # de 1992/2025 (largura 0,8) vazavam para fora e eram descartadas (oob).
  scale_x_anos_tese(anos = c(ANOS_TRANSICAO_GOV, max(res$ano)),
                    limits = c(min(res$ano) - 0.6, max(res$ano) + 0.6),
                    expand = expansion(add = c(0.4, 0.4))) +
  labs(x = "Year", y = "Share") +
  guides(colour = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

p2 <- p2a / p2b + plot_layout(heights = c(3, 2))
salvar_grafico(p2, prefixo = "097F_Diag_B_Decomposicao_C_EM",
               largura = LARGURA_TEXTO, altura = 9.0, unidades = "in", formato = "png")

cat("====== 097F concluido ======\n")
