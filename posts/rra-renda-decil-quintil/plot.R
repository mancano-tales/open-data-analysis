# ==============================================================================
# SCRIPT: plot.R (rra-renda-decil-quintil)
#
# Adjusted Relative Risk (RRA) of tertiary-education access by household
# income — decile (D10/D1) and quintile (Q5/Q1) panels side by side, an
# adapted replication of Figure 3 (REN panel) in Salata, Bringhenti &
# Miranda (2025, Dados).
#
# PRE-REQUISITE: shared-pipeline/pnadc/035_Splice_Microdados.R and
#   037_Modelos_Logit_RRA_Renda.R must have already run (produces
#   RRA_Renda_1992_2025.csv, read below).
#
# DESIGN DECISIONS:
#   - Log10 y-axis: the RRA spans ~5 to ~400 across the series (a ratio of
#     probabilities, not a probability) — on a linear scale the post-2000
#     decline is illegible, flattened near zero by the 1990s peak.
#   - Years without a PNAD/PNADC estimate (1994, 2000, 2010, 2010,
#     2020-2021) are treated the same way as this repo's Wagstaff time
#     series post: no visual interpolation — the underlying CSV DOES carry
#     an interpolated point for those years (linear midpoint of the
#     neighboring years, replicating footnote 21 of the Salata et al.
#     paper), but for the plot those points are set to NA so
#     geom_line()/geom_ribbon() (with na.rm = TRUE) break naturally instead
#     of bridging across the gap.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(here)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

BASE_DIR   <- here::here("data-raw", "harmonizing-br-data")
OUTPUT_DIR <- file.path(BASE_DIR, "output")

ANOS_SEM_DADO <- c(1994L, 2000L, 2010L, 2020L, 2021L)

# ── 1. Load RRA ──────────────────────────────────────────────────────────────
df_rra <- read.csv(file.path(OUTPUT_DIR, "RRA_Renda_1992_2025.csv"),
                    stringsAsFactors = FALSE) %>%
  mutate(
    interpolado = ano %in% ANOS_SEM_DADO,
    rra    = if_else(interpolado, NA_real_, rra),
    ic_inf = if_else(interpolado, NA_real_, ic_inf),
    ic_sup = if_else(interpolado, NA_real_, ic_sup)
  )

COR_LINHA <- PALETA_QUALITATIVA[4]  # dark blue

# ── 2. One panel (decile OR quintile) ───────────────────────────────────────
plotar_rra <- function(dados, var, titulo_eixo_topo, titulo_eixo_base) {
  d <- dados %>% filter(agregacao == var) %>% arrange(ano)
  anos_disp <- d$ano

  ggplot(d, aes(x = ano, y = rra)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), fill = COR_LINHA, alpha = 0.18, na.rm = TRUE) +
    geom_line(colour = COR_LINHA, linewidth = 0.6, na.rm = TRUE) +
    geom_point(colour = COR_LINHA, size = 1.3, na.rm = TRUE) +
    scale_x_anos_tese(anos = anos_disp) +
    scale_y_log10(
      breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 400),
      labels = label_number(accuracy = 1)
    ) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = sprintf("Adjusted relative risk (%s / %s), log scale", titulo_eixo_topo, titulo_eixo_base))
}

p_decil   <- plotar_rra(df_rra, "decil",   "D10", "D1")
p_quintil <- plotar_rra(df_rra, "quintil", "Q5",  "Q1")

p_final <- p_decil + p_quintil + plot_layout(ncol = 2)

# ── 3. Save ──────────────────────────────────────────────────────────────────
ggsave(here::here("posts", "rra-renda-decil-quintil", "thumbnail.png"),
       p_final, width = LARGURA_TEXTO * 2, height = ALTURA_PADRAO, dpi = DPI_IMPRESSAO)

cat("Saved: posts/rra-renda-decil-quintil/thumbnail.png\n")
