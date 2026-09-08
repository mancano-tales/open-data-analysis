# ==============================================================================
# SCRIPT: 042D_Tese_Dotplot_Renda_Anualizada.R
#
# Variante de 042D_Tese_Dotplot_Renda.R com três diferenças:
#   1. Sem o governo Itamar (série começa em 1995)
#   2. Delta ANUALIZADO em % a.a. — crescimento geométrico anualizado:
#      δ_ann = ((t1/t0)^(1/n) − 1) × 100, onde n = t1 − t0 em anos
#      (mesmo conceito da GIC, script 230; permite comparar diretamente
#       mandatos de durações distintas — ex.: Lula III tem 1 ano de dados)
#   3. Lula III (2023–2024) incluído — a PNADC tem 2024 no parquet
#
# Grade: 8 mandatos (FHC I → Lula III) + painel Full period (1995–2024)
# = 9 painéis → grade 3×3 idêntica ao 042C_Tese_Dotplot.R
#
# SE da taxa anualizada — delta method para f(t1, t0) = ((t1/t0)^(1/n) − 1)×100:
#   ∂f/∂t1 = (100/n) × (t1/t0)^(1/n) / t1
#   ∂f/∂t0 = −(100/n) × (t1/t0)^(1/n) / t0
#   se_ann  = (100/n) × (t1/t0)^(1/n) × sqrt((se1/t1)² + (se0/t0)²)
#
# VER TAMBÉM:
#   042D_Tese_Dotplot_Renda.R      — versão total (com Itamar, sem Lula III)
#   230_Tese_GIC_Renda_Percentil.R — GIC anualizada por percentil, 3 janelas
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

DEFF <- 2.0  # design effect (Kish) — mesmo critério de 042C/042D

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET  <- file.path(BASE_DIR, "output", "Microdados_Todas_Idades_1992_2025.parquet")

# ==============================================================================
# 1. CARREGAR E PREPARAR DADOS
# Fonte única por ano: PNAD Anual 1992–2015, PNADC 2016+ (codebook B.3/B.4).
# 2024 está disponível no parquet (PNADC) — necessário para Lula III.
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
# (idêntico a 042D_Tese_Dotplot_Renda.R)
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
    filter(ano %in% c(1995L, 2003L, 2011L, 2023L, 2025L),
           decil_num %in% c(1L, 5L, 10L)) |>
    mutate(media = round(media, 0)) |>
    select(ano, decil_num, media) |>
    pivot_wider(names_from = decil_num, values_from = media,
                names_prefix = "D") |>
    as.data.frame()
)

# ==============================================================================
# 3. GOVERNOS — sem Itamar, com Lula III (2023–2024)
# Full period: 1995–2024 (9 painéis → grade 3×3 exata)
# ==============================================================================
GOVERNOS <- data.frame(
  nome = c("FHC I", "FHC II", "Lula I", "Lula II",
           "Dilma I", "Dilma II/Temer", "Bolsonaro", "Lula III"),
  ini  = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  # 2026-08-09 (decisao do autor): Lula III passa de 2023-2024 para 2023-2025
  # com a entrada do ano novo. A extensao e legitima AQUI, e nao no 042C,
  # porque esta figura mede crescimento ANUALIZADO (% a.a.), comparavel entre
  # mandatos de duracoes diferentes por construcao — o 042C mede variacao
  # TOTAL em p.p., que nao e. Com tres anos em vez de um, o IC de Lula III
  # tambem encolhe.
  fim  = c(1999, 2003, 2007, 2011, 2015, 2019, 2023, 2025),
  stringsAsFactors = FALSE
)

ANO_INICIO_FULL <- 1995L
ANO_FIM_FULL    <- 2025L

anos_disp <- sort(unique(df_dec$ano))
nearest   <- function(y) anos_disp[which.min(abs(anos_disp - y))]

# ==============================================================================
# 4. CALCULAR DELTA ANUALIZADO (% a.a.) VIA DELTA METHOD
#
# f(t1, t0, n) = ((t1/t0)^(1/n) − 1) × 100
# ∂f/∂t1 =  (100/n) × (t1/t0)^(1/n) / t1
# ∂f/∂t0 = −(100/n) × (t1/t0)^(1/n) / t0
# se_ann  =  (100/n) × (t1/t0)^(1/n) × sqrt((se1/t1)² + (se0/t0)²)
#
# Caso n = 1 (Lula III: 2023→2024): delta_ann = delta_total (% simples)
# ==============================================================================
construir_delta_anualizado <- function(t0_df, t1_df, nome_gov, y0, y1) {
  n_anos <- y1 - y0
  left_join(t1_df, t0_df, by = "decil_num") |>
    mutate(
      ratio      = t1 / t0,
      delta_ann  = (ratio^(1 / n_anos) - 1) * 100,
      se_ann     = (100 / n_anos) * ratio^(1 / n_anos) *
                   sqrt((se1 / t1)^2 + (se0 / t0)^2),
      ci_low     = delta_ann - 1.96 * se_ann,
      ci_high    = delta_ann + 1.96 * se_ann,
      governo    = paste0(nome_gov, " (", y0, "–",
                          sprintf("%02d", y1 %% 100), ")"),
      gov_nome   = nome_gov,
      positivo   = delta_ann >= 0,
      n_anos     = n_anos
    )
}

delta_gov <- lapply(seq_len(nrow(GOVERNOS)), function(i) {
  g  <- GOVERNOS[i, ]
  y0 <- nearest(g$ini); y1 <- nearest(g$fim)
  if (y0 == y1) return(NULL)
  t0 <- filter(df_dec, ano == y0) |> select(decil_num, t0 = media, se0 = se)
  t1 <- filter(df_dec, ano == y1) |> select(decil_num, t1 = media, se1 = se)
  construir_delta_anualizado(t0, t1, g$nome, y0, y1)
}) |> bind_rows()

# Painel Full period (1995–2024)
y0_full <- nearest(ANO_INICIO_FULL); y1_full <- nearest(ANO_FIM_FULL)
t0_f <- filter(df_dec, ano == y0_full) |> select(decil_num, t0 = media, se0 = se)
t1_f <- filter(df_dec, ano == y1_full) |> select(decil_num, t1 = media, se1 = se)
full_delta <- construir_delta_anualizado(
  t0_f, t1_f,
  paste0("Full period"), y0_full, y1_full
) |>
  mutate(governo = paste0("Full period (", y0_full, "–", y1_full, ")"),
         gov_nome = "Full period")

delta_dec <- bind_rows(delta_gov, full_delta) |>
  mutate(governo = factor(governo, levels = unique(governo)))

cat("\nSANIDADE — delta anualizado (% a.a.) por governo, D1 e D10:\n")
print(
  delta_dec |>
    filter(decil_num %in% c(1L, 10L)) |>
    select(governo, decil_num, delta_ann, n_anos) |>
    mutate(delta_ann = round(delta_ann, 2)) |>
    pivot_wider(names_from = decil_num, values_from = delta_ann,
                names_prefix = "D") |>
    as.data.frame()
)

# ==============================================================================
# 5. FIGURA
# ==============================================================================
Y_DEC <- paste0("D", 1:10)

df_plot <- delta_dec |>
  mutate(.x = delta_ann, .xmin = ci_low, .xmax = ci_high)

xlim_shared <- {
  all_x <- c(df_plot$ci_low, df_plot$ci_high)
  c(floor(min(all_x, na.rm = TRUE) / 2) * 2 - 1,
    ceiling(max(all_x, na.rm = TRUE) / 2) * 2 + 1)
}

lbl_rel_ann <- function(x) paste0(ifelse(x > 0, "+", ""), round(x, 1), "%")

panel_theme <- theme(panel.grid.major.y = element_blank()) +
  tema_grade_densa()

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

# ggplot2 >= 4.0.0: `fatten` está deprecated mas ainda funciona — manter
# idêntico a 042C_Tese_Dotplot.R para consistência visual entre figuras.
p <- ggplot(df_plot, aes(x = .x, y = factor(decil_num), colour = positivo)) +
  geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.55) +
  geom_hline(yintercept = 5.5, colour = "#888888",
             linewidth = 0.3, linetype = "dotted") +
  geom_pointrange(aes(xmin = .xmin, xmax = .xmax),
                  orientation = "y", size = 0.42, linewidth = 0.6, fatten = 2.4) +
  facet_wrap(~governo, ncol = 3, scales = "fixed") +
  scale_colour_delta(
    labels = c("TRUE"  = "Gain (% p.a., real)",
               "FALSE" = "Loss (% p.a., real)"),
    guide  = if (MOSTRAR_LEGENDA) "legend" else "none"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = lbl_rel_ann,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_discrete(labels = Y_DEC) +
  coord_cartesian(xlim = xlim_shared, clip = "off") +
  labs(
    x = "Annualized real per-capita income growth (% per year)",
    y = "Income decile (D1 = poorest \u2192 D10 = richest)",
    title    = if (MOSTRAR_TITULO) paste0(
      "Annualized real household per-capita income growth by income decile") else NULL,
    subtitle = if (MOSTRAR_TITULO) "% per year, by presidential term (Jan 2024 prices)" else NULL,
    caption  = if (MOSTRAR_FONTE) paste0(
      "Source: IBGE \u2014 PNAD (1995\u20132015) and PNAD Cont\u00ednua (2016\u20132025).") else NULL
  ) +
  panel_theme + theme_extra

# Promoção para final/ é MANUAL — chamar finalizar_figura() só quando o autor
# decidir que a figura está pronta para citar no capítulo.
salvar_grafico(p,
               prefixo  = "042D_Tese_Dotplot_Renda_Anualizada",
               largura  = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM,
               unidades = "cm", formato = "pdf")

# ── Repromoção 2026-08-09 (extensão até 2025) ────────────────────────────────
# A chamada abaixo estava ausente do script vivo: o autor a removeu depois de
# promover em 2026-07-08, para que rodar o script não repromovesse sem querer.
# Restaurada agora porque o autor pediu explicitamente a atualização das
# figuras promovidas. A redação de fig_cap/fonte/nota é a DELE, copiada do
# snapshot 2026-07-08_1054 que acompanha o PDF promovido; alterei apenas os
# anos e a frase sobre a cobertura do painel Lula III, que passou de um ano
# para dois anos de crescimento (2023→2025).
PROMOVER <- TRUE
if (PROMOVER) {
  finalizar_figura(
    plot        = p,
    fig_label   = "delta-renda-anualizada-decil-governo",
    fig_cap     = "Annualized real per-capita household income growth by income decile and presidential term, Brazil, 1995\\textendash{}2025.",
    fonte       = "IBGE --- PNAD (1995--2015) and PNAD Cont\\'{i}nua (2016--2025)",
    nota        = "Each point is the annualized geometric real income growth rate (\\% per year, January 2024 prices) for each income decile, from the first available year of a presidential term to the first available year of the next (blue = gain, orange = loss); horizontal lines are 95\\% CIs. Itamar Franco (1992--1995) omitted. Lula III panel covers two years of growth (2023--2025). Bottom-right panel: full 1995--2025 period. CIs via delta method for $f(t_1,t_0)=((t_1/t_0)^{1/n}-1)\\times 100$: $\\text{se}=\\frac{100}{n}\\cdot(t_1/t_0)^{1/n}\\cdot\\sqrt{(\\text{se}_1/t_1)^2+(\\text{se}_0/t_0)^2}$. SE of each endpoint: weighted sample variance divided by Kish effective sample size (DEFF~=~2). \\hyperref[sec-fignote-delta-renda-anualizada-decil-governo]{Appendix~\\ref*{sec-fignote-delta-renda-anualizada-decil-governo}}.",
    apendice    = NULL,
    script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                              "02_validation", "042D_Tese_Dotplot_Renda_Anualizada.R"),
    largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm"
  )
}

cat("══════ Script 042D_Tese_Dotplot_Renda_Anualizada concluído ══════\n")
