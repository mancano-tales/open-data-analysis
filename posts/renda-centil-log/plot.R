# ==============================================================================
# SCRIPT: 220C_Tese_Renda_Centil_Log.R
#
# Variante log-scale de 220B_Tese_Renda_Centil.R, seguindo os padrões
# visuais de Healy (2019): título bold, subtítulo descritivo, fonte no
# rodapé — figura auto-contida, sem depender só da legenda do .qmd.
#
# Diferenças em relação a 220B:
#   - Eixo Y logarítmico (base 10): revela diferenças em toda a distribuição,
#     não só no topo; P2–P70 deixam de ser comprimidos
#   - plot.title / plot.subtitle / plot.caption dentro do painel (Healy cap. 5)
#   - Sem truncamento YLIM_MAX: escala log dispensa corte artificial
#   - fig_label próprio ("renda-centil-log") para não sobrescrever 220B
#
# Herda todas as decisões metodológicas de 220B:
#   - renda_real deflacionada via IPCA para jan/2024 (Decisão D01)
#   - 2012 filtrado para PNAD Anual (evita dupla contagem do overlap, B.3)
#   - SM por ano: ipeadatar MTE12_SALMIN12 + deflateBR::deflate
#   - Cache SM em output/220B_sm_real_cache.rds (compartilhado com 220B)
#   - Marcadores: 1 MW, 1.5 MW, 3 MW per capita abaixo da curva de 2002
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(scales); library(here); library(lubridate)
  library(ipeadatar); library(deflateBR)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

# ------------------------------------------------------------------------------
# CONSTANTES
# ------------------------------------------------------------------------------
# 2026-08-09: ano final 2024 -> 2025 (extensao da serie); ver nota no 220B.
ANOS_SEL    <- c(2002L, 2007L, 2012L, 2017L, 2025L)
MULTS       <- c(1.0, 1.5, 3.0)
LABELS_MW   <- c("1 MW", "1.5 MW", "3 MW")
SCRIPT_PATH <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data",
                           "R", "02_validation", "220C_Tese_Renda_Centil_Log.R")
BASE_DIR    <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET     <- file.path(BASE_DIR, "output",
                          "Microdados_Todas_Idades_1992_2025.parquet")
CACHE_SM    <- file.path(BASE_DIR, "output", "220B_sm_real_cache.rds")

if (!file.exists(PARQUET))
  stop("Parquet nao encontrado: ", PARQUET)

# ==============================================================================
# 1. SALARIO MINIMO HISTORICO + DEFLACAO  (cache compartilhado com 220B)
# ==============================================================================
sm_df <- if (file.exists(CACHE_SM)) readRDS(CACHE_SM) else NULL

if (is.null(sm_df) || !all(ANOS_SEL %in% sm_df$ano)) {
  cat("Buscando serie SM via ipeadatar (MTE12_SALMIN12)...\n")
  sm_serie <- ipeadata("MTE12_SALMIN12")
  sm_df <- sm_serie |>
    filter(month(date) == 1L, year(date) %in% ANOS_SEL) |>
    transmute(ano = as.integer(year(date)), sm_nominal = value, date = date) |>
    arrange(ano)
  sm_df <- sm_df |>
    mutate(sm_real = deflateBR::deflate(sm_nominal, date, "01/2024", index = "ipca"))
  saveRDS(sm_df, CACHE_SM)
  cat("Cache salvo.\n")
} else {
  cat("SM cache carregado.\n")
}

sm_df <- sm_df |> filter(ano %in% ANOS_SEL) |> arrange(ano)
stopifnot(identical(sm_df$ano, ANOS_SEL))

cat("\nSalario minimo real (jan/2024 R$):\n")
print(sm_df |>
  mutate(
    crescimento = round((sm_real / sm_real[1] - 1) * 100, 1),
    sm_real_fmt = paste0("R$ ", format(round(sm_real), big.mark = ".",
                                        decimal.mark = ","))
  ) |>
  select(ano, sm_nominal, sm_real_fmt, crescimento))

# ==============================================================================
# 2. CARGA E CALCULO DE CENTIS
# ==============================================================================
cat("\nCarregando parquet...\n")
microdados <- arrow::read_parquet(
  PARQUET,
  col_select = c("ano", "fonte", "peso", "renda_real")
) |>
  filter(
    ano %in% ANOS_SEL,
    !(ano %in% 2012:2015) | fonte == "PNAD Anual",
    renda_real > 0, !is.na(renda_real),
    peso > 0,       !is.na(peso)
  )
cat(sprintf("  %s obs em %d anos\n",
            format(nrow(microdados), big.mark = "."), length(unique(microdados$ano))))

criar_centis <- function(df) {
  df |>
    arrange(renda_real) |>
    mutate(
      peso_cum = cumsum(peso) / sum(peso),
      centil   = pmin(pmax(ceiling(peso_cum * 100L), 1L), 100L)
    )
}

cat("Calculando centis...\n")
res <- microdados |>
  group_by(ano) |>
  group_modify(~ criar_centis(.x)) |>
  group_by(ano, centil) |>
  summarise(renda_media = weighted.mean(renda_real, peso, na.rm = TRUE),
            .groups = "drop") |>
  filter(centil >= 2L, centil <= 99L)

# ==============================================================================
# 3. INTERSECAO SM x DISTRIBUICAO
# ==============================================================================
pontos_sm <- crossing(
  sm_df |> select(ano, sm_real),
  tibble(mult = MULTS, mw_label = factor(LABELS_MW, levels = LABELS_MW))
) |>
  mutate(target = sm_real * mult) |>
  left_join(res, by = "ano", relationship = "many-to-many") |>
  group_by(ano, mult) |>
  slice_min(abs(renda_media - target), n = 1L, with_ties = FALSE) |>
  ungroup()

cat("\nIntersecoes SM:\n")
pontos_sm |>
  mutate(renda_fmt = paste0("R$ ", format(round(renda_media),
                                           big.mark = ".", decimal.mark = ","))) |>
  select(mw_label, ano, centil, renda_fmt) |>
  arrange(mw_label, ano) |>
  print(n = Inf)

# ==============================================================================
# 4. ELEMENTOS AUXILIARES
# ==============================================================================
cores          <- setNames(PALETA_QUALITATIVA[seq_along(ANOS_SEL)],
                           as.character(ANOS_SEL))
labels_mw_2002 <- pontos_sm |> filter(ano == 2002L)

# ==============================================================================
# 5. FIGURA  — escala log + padrões Healy (título / subtítulo / fonte)
# ==============================================================================
anos_str <- paste0(min(ANOS_SEL), "–", max(ANOS_SEL))

p <- ggplot(res, aes(x = centil, y = renda_media,
    colour = factor(ano), group = factor(ano))) +
  geom_line(linewidth = 0.65) +
  geom_point(
    data = pontos_sm, aes(x = centil, y = renda_media, colour = factor(ano)),
    inherit.aes = FALSE, shape = 21, fill = "white",
    size = 2.2, stroke = 0.85, show.legend = FALSE
  ) +
  geom_text(
    data = labels_mw_2002,
    aes(x = centil, y = renda_media, label = as.character(mw_label)),
    inherit.aes = FALSE, colour = "grey35",
    hjust = 0.5, vjust = 2.2, size = 2.3, family = THESIS_FONT,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(10, 90, by = 10),
    labels = paste0("P", seq(10, 90, by = 10)),
    limits = c(NA, 102),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_log10(
    breaks = c(50, 100, 200, 500, 1000, 2000, 5000),
    labels = function(x)
      paste0("R$ ", format(x, big.mark = ".", decimal.mark = ",",
                                 scientific = FALSE)),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_colour_manual(values = cores) +
  labs(
    title    = paste0("Per-capita household income by percentile, Brazil ", anos_str),
    subtitle = paste0(
      "Weighted mean real income within each percentile group (Jan 2024 R$, log scale)\n",
      "Open circles mark 1×, 1.5×, and 3× the minimum wage per capita for each year"
    ),
    x       = "Income percentile (P2 = poorest, P99 = richest)",
    y       = "Mean per-capita household income (Jan 2024 R$)",
    caption = paste0(
      "Source: IBGE — PNAD (2002–2012) and PNAD Contínua (2017–2024)",
      " · IPEADATA (minimum wage series MTE12_SALMIN12)"
    )
  ) +
  guides(colour = guide_legend(
    title    = NULL,
    reverse  = TRUE,
    keywidth  = unit(0.7, "cm"),
    keyheight = unit(0.35, "cm"),
    override.aes = list(linewidth = 1.2)
  )) +
  theme(
    plot.title             = element_text(face = "bold", size = rel(1.0),
                               family = THESIS_FONT, hjust = 0,
                               margin = margin(b = 3)),
    plot.subtitle          = element_text(size = rel(0.80), colour = "grey40",
                               family = THESIS_FONT, hjust = 0,
                               margin = margin(b = 6)),
    plot.caption           = element_text(size = rel(0.72), colour = "grey50",
                               family = THESIS_FONT, hjust = 1,
                               margin = margin(t = 4)),
    legend.position        = "inside",
    legend.position.inside = c(0.10, 0.82),
    legend.justification   = c(0, 1),
    legend.background      = element_rect(fill = "white", colour = "grey85",
                               linewidth = 0.3),
    legend.text            = element_text(size = 7.5, family = THESIS_FONT),
    legend.margin          = margin(4, 8, 4, 6),
    legend.key             = element_blank(),
    plot.margin            = margin(6, 8, 6, 8)
  )

# ==============================================================================
# 6. SALVAR
# ==============================================================================
cat("\n")
finalizar_figura(
  plot      = p,
  fig_label = "renda-centil-log",
  fig_cap   = paste0(
    "Average per-capita household income by income percentile, Brazil ",
    anos_str, " (log scale)."
  ),
  nota      = paste0(
    "Each line shows the weighted mean real per-capita household income ",
    "(deflated to January 2024 prices via the IPCA; Appendix B, Decision D01) ",
    "within each income percentile group, for the selected years. ",
    "The y-axis uses a base-10 logarithmic scale, which spreads the lower and ",
    "middle of the distribution and reveals proportional differences across years. ",
    "Percentiles rank all individuals with strictly positive per-capita household ",
    "income by income, using sampling weights; each individual is assigned the ",
    "centile given by the ceiling of the weighted cumulative population share ",
    "multiplied by 100 (Appendix B, Decisions D02). ",
    "Centiles 1 and 100 are excluded due to sampling instability at the extremes. ",
    "For 2012, only the PNAD Anual sample is used to avoid double-counting the ",
    "PNAD/PNADC overlap years (Appendix B.3). ",
    "Open circles mark the percentile at which mean per-capita household income ",
    "equals one (1 MW), one-and-a-half (1.5 MW), and three (3 MW) times the ",
    "national minimum wage in force in January of that year, deflated to ",
    "January 2024 prices via \\texttt{deflateBR}. ",
    "Minimum wage values (nominal, January): ",
    "2002 R\\$\\,180; 2007 R\\$\\,350; 2012 R\\$\\,622; 2017 R\\$\\,937; 2024 R\\$\\,1{,}412."
  ),
  fonte     = paste0(
    "IBGE — PNAD (2002–2012) and PNAD Contínua (2017–2024), ",
    "harmonized series; IPEADATA (minimum wage series MTE12_SALMIN12)"
  ),
  script_path = SCRIPT_PATH,
  largura     = LARGURA_TEXTO,
  altura      = ALTURA_PADRAO
)

cat("\n═════ Script 220C concluído ═════\n")
