# ==============================================================================
# SCRIPT: 220B_Tese_Renda_Centil.R
#
# Versao tese de 220_Renda_Media_por_Centil.R (2026-02_SALATA_PNAD/220_...).
# Diferencas em relacao ao original:
#   - ggplot2 (PDF vetorial, thesis_theme()) em vez de plotly
#   - renda_real (deflacionada para jan/2024 via IPCA, Decisao D01)
#   - Marcadores de salario minimo POR ANO: SM de cada ano deflacionado para
#     jan/2024 via deflateBR; SM nominal buscado via ipeadatar
#   - 3 marcadores por curva: 1 MW, 1.5 MW e 3 MW per capita
#   - 5 anos: 2002, 2007, 2012, 2017, 2024
#   - 2012 filtrado para PNAD Anual: o parquet carrega 2012-2015 nas DUAS
#     fontes (overlap intencional do splice, ver codebook B.3) — sem o
#     filtro a curva de 2012 misturaria as duas pesquisas (dupla contagem)
#   - Labels e eixos em ingles
#   - finalizar_figura() -> bundle .pdf+.R+.qmd em 6-images-tables/final/
#
# DADOS SM: ipeadatar serie MTE12_SALMIN12 (salario minimo nominal mensal)
#           deflacionado via deflateBR::deflate(..., "01/2024", "ipca")
#           Cache em output/220B_sm_real_cache.rds (evita re-fetch da API)
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(here)
  library(lubridate)
  library(ipeadatar)
  library(deflateBR)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

# ------------------------------------------------------------------------------
# CONSTANTES
# ------------------------------------------------------------------------------
# 2026-08-09, DUAS mudancas no mesmo dia — a segunda corrige a primeira.
#   (a) ano final 2024 -> 2025, pela extensao da serie, TROCANDO o ultimo
#       ponto em vez de acrescentar, para manter cinco curvas legiveis;
#   (b) o autor observou que isso abria um vao: a grade era quinquenal
#       (2002, 2007, 2012, 2017) e o quinto ponto passou a ficar OITO anos
#       depois do quarto. Pior, a linha de fonte descrevia a selecao como se
#       fosse cobertura ("PNAD Contínua (2017–2025)"), sugerindo que nao ha
#       dado entre 2013 e 2016 — ha, e a serie e continua ali. Acrescentado
#       2022, que restaura o passo de cinco anos E preserva o fim da serie.
#       Sao seis curvas; a PALETA_QUALITATIVA tem oito cores.
ANOS_SEL    <- c(2002L, 2007L, 2012L, 2017L, 2022L, 2025L)
MULTS       <- c(1.0, 1.5, 3.0)
LABELS_MW   <- c("1 MW", "1.5 MW", "3 MW")
YLIM_MAX    <- 8000
SCRIPT_PATH <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data",
                           "R", "02_validation", "220B_Tese_Renda_Centil.R")
BASE_DIR    <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET     <- file.path(BASE_DIR, "output",
                          "Microdados_Todas_Idades_1992_2025.parquet")
CACHE_SM    <- file.path(BASE_DIR, "output", "220B_sm_real_cache.rds")

if (!file.exists(PARQUET))
  stop("Parquet nao encontrado: ", PARQUET,
       "\nExecute o pipeline (035_Splice_Microdados.R) antes deste script.")

# ==============================================================================
# 1. SALARIO MINIMO HISTORICO + DEFLACAO
#
# SM nominal: ipeadatar serie MTE12_SALMIN12, janeiro de cada ano selecionado
# Deflacao:   deflateBR::deflate(..., "01/2024", "ipca") — mesma base de
#             renda_real na pipeline (Decisao D01)
# Cache:      salvo em output/220B_sm_real_cache.rds para evitar re-fetch.
#             O cache pode conter anos de execucoes anteriores com outro
#             ANOS_SEL: sem o filtro abaixo, anos extras viram linhas NA em
#             pontos_sm (e o print de crescimento usa o ano errado como
#             base); sem a checagem de completude, um ano novo em ANOS_SEL
#             ficaria sem marcador silenciosamente.
# ==============================================================================
sm_df <- if (file.exists(CACHE_SM)) readRDS(CACHE_SM) else NULL

if (is.null(sm_df) || !all(ANOS_SEL %in% sm_df$ano)) {
  cat("Buscando serie SM via ipeadatar (MTE12_SALMIN12)...\n")
  sm_serie <- ipeadata("MTE12_SALMIN12")
  sm_df <- sm_serie |>
    filter(month(date) == 1L, year(date) %in% ANOS_SEL) |>
    transmute(
      ano        = as.integer(year(date)),
      sm_nominal = value,
      date       = date
    ) |>
    arrange(ano)

  cat("Deflacionando SM para jan/2024 via deflateBR...\n")
  sm_df <- sm_df |>
    mutate(sm_real = deflateBR::deflate(sm_nominal, date, "01/2024", index = "ipca"))

  saveRDS(sm_df, CACHE_SM)
  cat("Cache salvo em", CACHE_SM, "\n")
} else {
  cat("SM cache carregado de", CACHE_SM, "\n")
}

sm_df <- sm_df |>
  filter(ano %in% ANOS_SEL) |>
  arrange(ano)
stopifnot(identical(sm_df$ano, ANOS_SEL))

cat("\nSalario minimo real (jan/2024 R$):\n")
print(
  sm_df |>
    mutate(
      crescimento = round((sm_real / sm_real[1] - 1) * 100, 1),
      sm_real_fmt = paste0("R$ ", format(round(sm_real), big.mark = ".",
                                          decimal.mark = ","))
    ) |>
    select(ano, sm_nominal, sm_real_fmt, crescimento)
)

# ==============================================================================
# 2. CARGA E CALCULO DE CENTIS
# ==============================================================================
cat("\nCarregando parquet (todos as idades)...\n")
microdados <- arrow::read_parquet(
  PARQUET,
  col_select = c("ano", "fonte", "peso", "renda_real")
) |>
  filter(
    ano        %in% ANOS_SEL,
    # 2012-2015 existem nas DUAS fontes (overlap do splice, codebook B.3):
    # manter apenas PNAD Anual nesses anos para evitar dupla contagem
    !(ano %in% 2012:2015) | fonte == "PNAD Anual",
    renda_real  >  0,
    !is.na(renda_real),
    peso        >  0,
    !is.na(peso)
  )
cat(sprintf("  %s observacoes em %d anos\n",
            format(nrow(microdados), big.mark = ".", decimal.mark = ","),
            length(unique(microdados$ano))))

criar_centis <- function(df) {
  df |>
    arrange(renda_real) |>
    mutate(
      peso_cum = cumsum(peso) / sum(peso),
      centil   = pmin(pmax(ceiling(peso_cum * 100L), 1L), 100L)
    )
}

cat("Calculando centis por ano...\n")
res <- microdados |>
  group_by(ano) |>
  group_modify(~ criar_centis(.x)) |>
  group_by(ano, centil) |>
  summarise(
    renda_media = weighted.mean(renda_real, peso, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  filter(centil >= 2L, centil <= 99L)

# ==============================================================================
# 3. PONTOS DE INTERSECAO SM x DISTRIBUICAO
#
# Para cada (ano, mult), encontrar o centil com renda_media mais proxima de
# sm_real * mult. Resultado: 3 marcadores por curva = 15 pontos no total.
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

cat("\nPontos de intersecao SM (centil onde renda_media == SM * mult):\n")
pontos_sm |>
  mutate(renda_fmt = paste0("R$ ", format(round(renda_media), big.mark = ".",
                                           decimal.mark = ","))) |>
  select(mw_label, ano, centil, renda_fmt) |>
  arrange(mw_label, ano) |>
  print(n = Inf)

# ==============================================================================
# 4. ELEMENTOS AUXILIARES DO GRAFICO
# ==============================================================================

# Paleta qualitativa Okabe-Ito (colorblind-safe): cores distintas por ano
cores <- setNames(
  PALETA_QUALITATIVA[seq_along(ANOS_SEL)],
  as.character(ANOS_SEL)
)

# Rotulos MW: abaixo do circulo da curva de 2002 (a mais baixa em cada threshold)
# vjust > 1 empurra o texto para baixo do ponto; espaco livre abaixo de 2002.
labels_mw_2002 <- pontos_sm |> filter(ano == 2002L)

# ==============================================================================
# 5. FIGURA
# ==============================================================================
p <- ggplot(
    res,
    aes(x      = centil,
        y      = renda_media,
        colour = factor(ano),
        group  = factor(ano))
  ) +
  geom_line(linewidth = 0.65) +
  # Marcadores SM: circulos vazados sobre cada curva
  geom_point(
    data        = pontos_sm,
    aes(x = centil, y = renda_media, colour = factor(ano)),
    inherit.aes = FALSE,
    shape       = 21,
    fill        = "white",
    size        = 2.2,
    stroke      = 0.85,
    show.legend = FALSE
  ) +
  # Rotulos MW abaixo do circulo de 2002 (curva mais baixa em cada threshold)
  geom_text(
    data        = labels_mw_2002,
    aes(x = centil, y = renda_media, label = as.character(mw_label)),
    inherit.aes = FALSE,
    colour      = "grey35",
    hjust       = 0.5,
    vjust       = 2.0,
    size        = 2.3,
    family      = THESIS_FONT,
    show.legend = FALSE
  ) +
  # Escalas
  scale_x_continuous(
    breaks = c(seq(10, 90, by = 10), 99),
    labels = paste0("P", c(seq(10, 90, by = 10), 99)),
    limits = c(NA, 102),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(0, YLIM_MAX, by = 1000),
    labels = function(x) {
      ifelse(x == 0, "R$ 0",
             paste0("R$ ", format(x, big.mark = ",", scientific = FALSE)))
    },
    limits = c(0, YLIM_MAX * 1.01),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_colour_manual(values = cores) +
  labs(
    x = "Income percentile (P2 = poorest, P99 = richest)",
    y = "Mean per-capita household income (Jan 2024 R$)"
  ) +
  guides(colour = guide_legend(
    title    = NULL,
    reverse  = TRUE,          # 2024 no topo, 2002 na base — espelha a ordem visual
    keywidth  = unit(0.7, "cm"),
    keyheight = unit(0.35, "cm"),
    override.aes = list(linewidth = 1.2)
  )) +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.10, 0.80),
    legend.justification   = c(0, 1),
    legend.background    = element_rect(fill = "white", colour = "grey85", linewidth = 0.3),
    legend.text          = element_text(size = 7.5, family = THESIS_FONT),
    legend.margin        = margin(4, 8, 4, 6),
    legend.key           = element_blank(),
    plot.margin          = margin(8, 8, 8, 8)
  )

# ==============================================================================
# 6. SALVAR
# ==============================================================================
anos_str <- paste0(min(ANOS_SEL), "–", max(ANOS_SEL))
cat("\n")
finalizar_figura(
  plot        = p,
  fig_label   = "renda-media-centil",
  # Sem "Source:" aqui — finalizar_figura() ja anexa o parametro `fonte`
  # como "Source: ..."; duplicar geraria duas frases "Source:" na legenda
  fig_cap     = paste0(
    "Average per-capita household income by income percentile, Brazil ",
    anos_str, "."
  ),
  nota        = paste0(
    "Each line is weighted mean real per-capita household income (Jan. 2024 ",
    "prices) by income percentile. Years shown: ",
    paste(ANOS_SEL, collapse = ", "),
    " --- five-year snapshots of a continuous annual series, not the whole ",
    "of it. Open circles mark ",
    "the percentile at 1, 1.5, and 3 times the minimum wage that year. ",
    "Y-axis truncated at R\\$", format(YLIM_MAX, big.mark = ","), "."
  ),
  apendice    = "sec-fignote-renda-media-centil",
  # A fonte descrevia a SELECAO como se fosse cobertura — "PNAD (2002–2012)
  # and PNAD Contínua (2017–2024)" —, o que sugeria um buraco de dados entre
  # 2013 e 2016 que nao existe: a serie harmonizada e continua ali, e a emenda
  # PNAD->PNADC cai em 2015|2016, nao em 2012|2017. Apontado pelo autor em
  # 2026-08-09. Agora a fonte descreve a fonte, e a selecao vai na nota.
  fonte       = paste0(
    "IBGE — PNAD (1992–2015) and PNAD Contínua (2016–", max(ANOS_SEL),
    "), harmonized series; IPEADATA (minimum wage series `MTE12_SALMIN12`)"
  ),
  script_path = SCRIPT_PATH,
  largura     = LARGURA_TEXTO,
  altura      = ALTURA_PADRAO
)

cat("\n═════ Script 220B_Tese_Renda_Centil concluído ═════\n")
