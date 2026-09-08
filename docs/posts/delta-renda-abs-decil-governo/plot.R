# ==============================================================================
# SCRIPT: 042D_Tese_Dotplot_Renda.R
#
# Versão "renda" de 042C_Tese_Dotplot.R — MESMO grid 3×3, MESMA estética
# (dotplot de Cleveland, geom_pointrange, scale_colour_delta, tema_grade_densa),
# trocando a variável de interesse:
#
#   042C_Tese_Dotplot.R       → ens_sup_a (matrícula ativa, 18–24 anos)
#   042D_Tese_Dotplot_Renda.R → renda_dom_pcta REAL (jan/2024), toda a
#                               população (renda domiciliar per capita representa
#                               o domicílio inteiro — não há razão para restringir
#                               a 18–24 anos, diferente da variável de matrícula)
#
# GERA DUAS VERSÕES:
#   p_abs — delta ABSOLUTO: t1 − t0 em R$/mês (jan/2024)
#           Responde: "quanto cresceu em poder de compra?"
#           Naturalmente maior para decis de renda alta (D10 sempre > D1 em R$)
#
#   p_rel — delta RELATIVO: (t1/t0 − 1) × 100 em %
#           Responde: "quanto cresceu proporcionalmente à renda que já tinha?"
#           Análogo ao crescimento anualizado da GIC (script 230), mas por
#           mandato e em termos totais (não anualizados)
#           Favorece a comparação entre decis de magnitudes muito diferentes
#
# DIFERENÇAS EM RELAÇÃO AO 042C:
#   • Parquet: Microdados_Todas_Idades_1992_2024.parquet
#   • Política de fonte única: PNAD Anual 1992–2015, PNADC 2016+
#     (mesmo critério do script 231 — codebook B.3/B.4)
#   • Variável: renda_real (já deflacionada para jan/2024 no pipeline)
#   • SE (PNAD Anual e PNADC): variância ponderada (Hmisc::wtd.var) / n_eff
#     Kish (DEFF = 2) — parquet completo não preserva strata/upa para
#     svydesign exato (ao contrário do parquet 18–24 + cache raw de 042C)
#   • delta_abs: se_delta = sqrt(se1² + se0²)  [propagação direta]
#   • delta_rel: se_delta via delta method
#     sqrt((100·se1/t0)² + (100·t1·se0/t0²)²)
#
# VER TAMBÉM:
#   042C_Tese_Dotplot.R          — matrícula ativa (18–24)
#   042C_Tese_Dotplot_Acesso.R   — acesso (ever enrolled, 18–24)
#   230_Tese_GIC_Renda_Percentil.R — GIC por percentil, 3 janelas temáticas
#   231_Tese_Renda_Decil_Indexada.R — série histórica indexada (2002 = 100)
# ==============================================================================

MOSTRAR_TITULO  <- FALSE
MOSTRAR_LEGENDA <- TRUE
MOSTRAR_FONTE   <- FALSE

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(scales); library(here); library(Hmisc)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 14))
options(scipen = 999)

ANO_FIM <- 2023L
DEFF    <- 2.0   # design effect (Kish) — mesmo valor de 042C para PNAD Anual

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET  <- file.path(BASE_DIR, "output", "Microdados_Todas_Idades_1992_2024.parquet")

# ==============================================================================
# 1. CARREGAR E PREPARAR DADOS
# Política de fonte única: PNAD Anual 1992–2015, PNADC 2016+
# (mesmo critério de 231_Tese_Renda_Decil_Indexada.R — codebook B.3/B.4).
# Filtros: renda_real > 0 (D01), peso > 0, decil não-NA (D02/D03).
# ==============================================================================
cat("Carregando parquet...\n")
microdados <- arrow::read_parquet(
  PARQUET,
  col_select = c("ano", "fonte", "peso", "renda_real", "decil")
) |>
  filter(
    !(fonte == "PNAD Contínua" & ano <= 2015L),
    renda_real > 0, !is.na(renda_real),
    peso > 0,       !is.na(peso),
    !is.na(decil)
  ) |>
  mutate(decil_num = as.integer(decil))

cat(sprintf("Anos disponíveis: %s\n",
            paste(sort(unique(microdados$ano)), collapse = ", ")))

# ==============================================================================
# 2. RENDA MÉDIA E SE POR DECIL E ANO
# SE da média = sqrt(variância ponderada / n_eff), onde n_eff = Kish / DEFF.
# Hmisc::wtd.var(..., normwt = FALSE) retorna a variância ponderada com os
# pesos originais de survey (expansion factors) — dividir por n_eff dá o
# erro padrão da média ponderada, conservadoramente ajustado pelo efeito de
# delineamento. Ver WRITING-STYLE §13.x e NEWS.md 2026-07-08.
# ==============================================================================
cat("Calculando renda média e SE por decil e ano...\n")
df_dec <- microdados |>
  group_by(ano, decil_num) |>
  summarise(
    media  = weighted.mean(renda_real, peso),
    var_wt = Hmisc::wtd.var(renda_real, weights = peso, normwt = FALSE,
                             na.rm = TRUE),
    n_eff  = (sum(peso)^2 / sum(peso^2)) / DEFF,
    .groups = "drop"
  ) |>
  mutate(se = sqrt(var_wt / pmax(n_eff, 1))) |>
  arrange(ano, decil_num)

cat("\nSANIDADE — renda média (R$/mês jan/2024) em anos-âncora:\n")
print(
  df_dec |>
    filter(ano %in% c(1992L, 2003L, 2011L, 2023L),
           decil_num %in% c(1L, 5L, 10L)) |>
    mutate(media = round(media, 0)) |>
    select(ano, decil_num, media) |>
    pivot_wider(names_from = decil_num, values_from = media,
                names_prefix = "D") |>
    as.data.frame()
)

# ==============================================================================
# 3. GOVERNOS E FUNÇÃO nearest()
# (idêntico a 042C_Tese_Dotplot.R)
# ==============================================================================
GOVERNOS <- data.frame(
  nome = c("Itamar", "FHC I", "FHC II", "Lula I", "Lula II",
           "Dilma I", "Dilma II/Temer", "Bolsonaro"),
  ini  = c(1992, 1995, 1999, 2003, 2007, 2011, 2015, 2019),
  fim  = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  stringsAsFactors = FALSE
)

anos_disp <- sort(unique(df_dec$ano))
nearest   <- function(y) anos_disp[which.min(abs(anos_disp - y))]

# ==============================================================================
# 4. CALCULAR DELTAS (absoluto e relativo) POR GOVERNO E DECIL
# delta_abs: propagação direta  → se_abs = sqrt(se1² + se0²)
# delta_rel: delta method       → se_rel = sqrt((100·se1/t0)² + (100·t1·se0/t0²)²)
# ==============================================================================
construir_delta <- function(t0_df, t1_df, nome_gov, y0, y1) {
  left_join(t1_df, t0_df, by = "decil_num") |>
    mutate(
      # Absoluto (R$/mês)
      delta_abs   = t1 - t0,
      se_abs      = sqrt(se1^2 + se0^2),
      ci_low_abs  = delta_abs - 1.96 * se_abs,
      ci_high_abs = delta_abs + 1.96 * se_abs,
      # Relativo (%) — delta method para f(t1, t0) = (t1/t0 − 1) × 100
      delta_rel   = (t1 / t0 - 1) * 100,
      se_rel      = sqrt((100 * se1 / t0)^2 + (100 * t1 * se0 / t0^2)^2),
      ci_low_rel  = delta_rel - 1.96 * se_rel,
      ci_high_rel = delta_rel + 1.96 * se_rel,
      # Metadados
      governo      = paste0(nome_gov, " (", y0, "–", sprintf("%02d", y1 %% 100), ")"),
      gov_nome     = nome_gov,
      positivo_abs = delta_abs >= 0,
      positivo_rel = delta_rel >= 0
    )
}

delta_gov <- lapply(seq_len(nrow(GOVERNOS)), function(i) {
  g  <- GOVERNOS[i, ]
  y0 <- nearest(g$ini); y1 <- nearest(g$fim)
  if (y0 == y1) return(NULL)
  t0 <- filter(df_dec, ano == y0) |> select(decil_num, t0 = media, se0 = se)
  t1 <- filter(df_dec, ano == y1) |> select(decil_num, t1 = media, se1 = se)
  construir_delta(t0, t1, g$nome, y0, y1)
}) |> bind_rows()

# Painel Full period
y0_full <- nearest(1992); y1_full <- nearest(ANO_FIM)
t0_f <- filter(df_dec, ano == y0_full) |> select(decil_num, t0 = media, se0 = se)
t1_f <- filter(df_dec, ano == y1_full) |> select(decil_num, t1 = media, se1 = se)
full_delta <- left_join(t1_f, t0_f, by = "decil_num") |>
  mutate(
    delta_abs   = t1 - t0,
    se_abs      = sqrt(se1^2 + se0^2),
    ci_low_abs  = delta_abs - 1.96 * se_abs,
    ci_high_abs = delta_abs + 1.96 * se_abs,
    delta_rel   = (t1 / t0 - 1) * 100,
    se_rel      = sqrt((100 * se1 / t0)^2 + (100 * t1 * se0 / t0^2)^2),
    ci_low_rel  = delta_rel - 1.96 * se_rel,
    ci_high_rel = delta_rel + 1.96 * se_rel,
    governo     = paste0("Full period (", y0_full, "–", y1_full, ")"),
    gov_nome    = "Full period",
    positivo_abs = delta_abs >= 0,
    positivo_rel = delta_rel >= 0
  )

delta_dec <- bind_rows(delta_gov, full_delta) |>
  mutate(governo = factor(governo, levels = unique(governo)))

cat("\nSANIDADE — delta absoluto (R$/mês) por governo, D1 e D10:\n")
print(
  delta_dec |>
    filter(decil_num %in% c(1L, 10L)) |>
    select(governo, decil_num, delta_abs) |>
    mutate(delta_abs = round(delta_abs, 0)) |>
    pivot_wider(names_from = decil_num, values_from = delta_abs,
                names_prefix = "D") |>
    as.data.frame()
)
cat("\nSANIDADE — delta relativo (%) por governo, D1 e D10:\n")
print(
  delta_dec |>
    filter(decil_num %in% c(1L, 10L)) |>
    select(governo, decil_num, delta_rel) |>
    mutate(delta_rel = round(delta_rel, 1)) |>
    pivot_wider(names_from = decil_num, values_from = delta_rel,
                names_prefix = "D") |>
    as.data.frame()
)

# ==============================================================================
# 5. ELEMENTOS VISUAIS COMUNS
# (estrutura idêntica a 042C, com colunas .x / .xmin / .xmax renomeadas em
# cada df para que o mesmo geoms_list possa ser reutilizado nas duas figuras)
# ==============================================================================
Y_DEC <- paste0("D", 1:10)

panel_theme <- theme(panel.grid.major.y = element_blank()) +
  tema_grade_densa()

# geom_pointrange usa colunas `.xmin` e `.xmax` injetadas em cada df.
# ggplot2 >= 4.0.0: `fatten` foi removido — `size` controla o ponto diretamente
# (equivalente anterior: ponto = fatten × size_old = 2.4 × 0.42 ≈ 1.0).
geoms_dotplot <- list(
  geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.55),
  geom_hline(yintercept = 5.5, colour = "#888888",
             linewidth = 0.3, linetype = "dotted"),
  geom_pointrange(
    aes(xmin = .xmin, xmax = .xmax),
    orientation = "y", size = 1.0, linewidth = 0.6
  )
)

theme_extra <- theme(
  legend.position = if (MOSTRAR_LEGENDA) "bottom" else "none",
  axis.text.y     = element_text(size = 10),
  plot.title      = element_text(size = 18, face = "bold", family = THESIS_FONT,
                                 margin = margin(b = 4)),
  plot.subtitle   = element_text(size = 13, colour = "#444444", family = THESIS_FONT,
                                 margin = margin(b = 8)),
  plot.caption    = element_text(size = 10, colour = "#666666", family = THESIS_FONT,
                                 hjust = 0, margin = margin(t = 8))
)

# ==============================================================================
# 6a. FIGURA ABSOLUTA — delta em R$/mês (jan/2024)
# ==============================================================================
df_abs <- delta_dec |>
  mutate(.x = delta_abs, .xmin = ci_low_abs, .xmax = ci_high_abs,
         .positivo = positivo_abs)

xlim_abs <- range(c(df_abs$ci_low_abs, df_abs$ci_high_abs), na.rm = TRUE)
# Arredonda para múltiplo de 100 com margem
xlim_abs <- c(floor(xlim_abs[1] / 100) * 100 - 50,
              ceiling(xlim_abs[2] / 100) * 100 + 50)

lbl_abs <- function(x) {
  # big.mark = "," (separador de milhar anglófono) — evita conflito com
  # decimal.mark = "." no locale padrão do R (warning do prettyNum)
  paste0(ifelse(x > 0, "+", ""), "R$\u00A0",
         format(round(x), big.mark = ",", scientific = FALSE))
}

p_abs <- ggplot(df_abs, aes(x = .x, y = factor(decil_num), colour = .positivo)) +
  geoms_dotplot +
  facet_wrap(~governo, ncol = 3, scales = "fixed") +
  scale_colour_delta(
    labels = c("TRUE"  = "Gain (R$/month, real)",
               "FALSE" = "Loss (R$/month, real)"),
    guide  = if (MOSTRAR_LEGENDA) "legend" else "none"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = lbl_abs,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_discrete(labels = Y_DEC) +
  coord_cartesian(xlim = xlim_abs, clip = "off") +
  labs(
    x = "Total change in real per-capita income (R$/month, Jan 2024 prices)",
    y = "Income decile (D1 = poorest \u2192 D10 = richest)",
    title    = if (MOSTRAR_TITULO) paste0(
      "Total change in real household per-capita income by income decile") else NULL,
    subtitle = if (MOSTRAR_TITULO) "R$/month total change per presidential term (Jan 2024 prices)" else NULL,
    caption  = if (MOSTRAR_FONTE) paste0(
      "Source: IBGE \u2014 PNAD (1992\u20132015) and PNAD Cont\u00ednua (2016\u20132023).") else NULL
  ) +
  panel_theme + theme_extra

salvar_grafico(p_abs,
               prefixo  = "042D_Tese_Dotplot_Renda_Absoluta",
               largura  = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM,
               unidades = "cm", formato = "pdf")

finalizar_figura(
  plot        = p_abs,
  fig_label   = "delta-renda-abs-decil-governo",
  fig_cap     = "Total change in real per-capita household income (R\\$/month) by income decile and presidential term, Brazil, 1992\\textendash{}2023.",
  fonte       = "IBGE --- PNAD (1992--2015) and PNAD Cont\\'{i}nua (2016--2023)",
  nota        = "Each point is the total change in mean real per-capita household income (R\\$/month, January 2024 prices) for each income decile, from the first available year of a presidential term to the first available year of the next (blue = gain, orange = loss); horizontal lines are 95\\% CIs. Bottom-right panel: full 1992--2023 period. CIs are computed as 95\\% normal approximation intervals propagating the SE of each endpoint: $\\text{se}_{\\Delta}=\\sqrt{\\text{se}_1^2+\\text{se}_0^2}$. SE of each endpoint: weighted sample variance divided by Kish effective sample size (DEFF = 2). \\hyperref[sec-fignote-delta-renda-abs-decil-governo]{Appendix~\\ref*{sec-fignote-delta-renda-abs-decil-governo}}.",
  apendice    = NULL,   # já embutido na nota acima
  script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                            "02_validation", "042D_Tese_Dotplot_Renda.R"),
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm"
)

# ==============================================================================
# 6b. FIGURA RELATIVA — delta em % ((t1/t0 − 1) × 100)
# SE via delta method: sqrt((100·se1/t0)² + (100·t1·se0/t0²)²)
# ==============================================================================
df_rel <- delta_dec |>
  mutate(.x = delta_rel, .xmin = ci_low_rel, .xmax = ci_high_rel,
         .positivo = positivo_rel)

xlim_rel <- range(c(df_rel$ci_low_rel, df_rel$ci_high_rel), na.rm = TRUE)
xlim_rel <- c(floor(xlim_rel[1] / 10) * 10 - 5,
              ceiling(xlim_rel[2] / 10) * 10 + 5)

lbl_rel <- function(x) paste0(ifelse(x > 0, "+", ""), round(x, 1), "%")

p_rel <- ggplot(df_rel, aes(x = .x, y = factor(decil_num), colour = .positivo)) +
  geoms_dotplot +
  facet_wrap(~governo, ncol = 3, scales = "fixed") +
  scale_colour_delta(
    labels = c("TRUE"  = "Gain (% total)",
               "FALSE" = "Loss (% total)"),
    guide  = if (MOSTRAR_LEGENDA) "legend" else "none"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = lbl_rel,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_discrete(labels = Y_DEC) +
  coord_cartesian(xlim = xlim_rel, clip = "off") +
  labs(
    x = "Total change in real per-capita income (% over term)",
    y = "Income decile (D1 = poorest \u2192 D10 = richest)",
    title    = if (MOSTRAR_TITULO) paste0(
      "Total percentage change in real household per-capita income by income decile") else NULL,
    subtitle = if (MOSTRAR_TITULO) "% total change per presidential term" else NULL,
    caption  = if (MOSTRAR_FONTE) paste0(
      "Source: IBGE \u2014 PNAD (1992\u20132015) and PNAD Cont\u00ednua (2016\u20132023).") else NULL
  ) +
  panel_theme + theme_extra

salvar_grafico(p_rel,
               prefixo  = "042D_Tese_Dotplot_Renda_Relativa",
               largura  = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM,
               unidades = "cm", formato = "pdf")

finalizar_figura(
  plot        = p_rel,
  fig_label   = "delta-renda-rel-decil-governo",
  fig_cap     = "Total percentage change in real per-capita household income by income decile and presidential term, Brazil, 1992\\textendash{}2023.",
  fonte       = "IBGE --- PNAD (1992--2015) and PNAD Cont\\'{i}nua (2016--2023)",
  nota        = "Each point is the percentage change in mean real per-capita household income for each income decile, from the first available year of a presidential term to the first available year of the next (blue = gain, orange = loss); horizontal lines are 95\\% CIs. Bottom-right panel: full 1992--2023 period. CIs are computed via the delta method for $f(t_1,t_0)=(t_1/t_0-1)\\times 100$: $\\text{se}_{\\Delta}=\\sqrt{(100\\,\\text{se}_1/t_0)^2+(100\\,t_1\\,\\text{se}_0/t_0^2)^2}$. SE of each endpoint: weighted sample variance divided by Kish effective sample size (DEFF = 2). \\hyperref[sec-fignote-delta-renda-rel-decil-governo]{Appendix~\\ref*{sec-fignote-delta-renda-rel-decil-governo}}.",
  apendice    = NULL,
  script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                            "02_validation", "042D_Tese_Dotplot_Renda.R"),
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm"
)

cat("══════ Script 042D_Tese_Dotplot_Renda concluído ══════\n")
