# ==============================================================================
# SCRIPT: 220B_Tese_Renda_Centil.R
#
# RENDA DOMICILIAR PER CAPITA MEDIA POR CENTIL, BRASIL, anos selecionados,
# em R$ de jan/2024, com marcadores de 1, 1,5 e 3 salarios minimos.
#
# Versao tese de 220_Renda_Media_por_Centil.R (2026-02_SALATA_PNAD/220_...).
#
# REFORMULADO EM 2026-09-16 (decisao do autor: "quero tudo domiciliar"), na
# esteira da reformulacao do 242. Ate entao o script lia o parquet principal e
# herdava dele a unidade familiar ate 2015 (D21) e a exclusao dos zeros (D02);
# e buscava o salario minimo de JANEIRO deflacionado de janeiro. A auditoria
# de 2026-09-16 (9-vers/llm-reviews) mediu as duas incoerencias:
#
#   (a) UNIDADE E UNIVERSO. Contra a base do 242 (domiciliar em toda a serie,
#       universo oficial com zeros), a mediana do parquet ficava +4,5% (2002),
#       +4,4% (2007) e +3,4% (2012) acima, mas so +1,8% (2017), +1,7% (2022) e
#       0,0% (2025): a assimetria entre os dois lados da emenda era de ~3 pp,
#       somada ao degrau do instrumento. Por centil, a troca de unidade sozinha
#       vale ~-2,3% no meio da distribuicao dos anos de PNAD; os zeros pesam
#       -10% a -13% em P5 e quase nada acima de P30.
#       AQUI: mesma base do 242 — bruto da PNAD via utils/renda_domiciliar_
#       pnad.R (V4721 / moradores fora de {6,7,8}) e VD5008 da PNADC, com os
#       domicilios de renda zero. A curva P50 desta figura e, por construcao,
#       a mediana da @fig-renda-mediana-domiciliar-pc (trava na secao 2).
#
#   (b) SALARIO MINIMO. A renda e deflacionada da data de referencia da
#       pesquisa (D01: 1/set na PNAD, 15/jun na PNADC), mas o SM vinha de
#       janeiro e era deflacionado de janeiro. Em 2002 e 2007 o SM ainda era
#       reajustado em abril, entao o de janeiro (R$ 180 / R$ 350) nem era o
#       vigente em setembro (R$ 200 / R$ 380); de 2012 em diante o nominal
#       batia, mas deflacionar de janeiro superestimava o valor real em
#       1,4% a 4,6%. O vies trocava de sinal em 2012: marcador de 1 MW em
#       P53/P56/P59/P63/P61/P56 quando o coerente era P55/P59/P56/P61/P57/P53.
#       AQUI: SM do mes de referencia, deflacionado da mesma data da renda.
#
# Demais escolhas (mantidas):
#   - 3 marcadores por curva: 1 MW, 1.5 MW e 3 MW per capita, no centil cuja
#     renda media e a mais proxima do limiar
#   - anos: 2002, 2007, 2012, 2017, 2022, 2025 (grade quinquenal + fim da
#     serie; ver historico de 2026-08-09 abaixo)
#   - centis 1 e 100 excluidos (instabilidade amostral nos extremos)
#   - labels e eixos em ingles; finalizar_figura() -> bundle em final/
#
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
#
# FONTES: data-raw/pnad_anual_raw/<ano>/ (bruto, via dicionario SAS do IBGE)
#         data-raw/pnadc_consolidado_2012_2025_interview1.rds (preserva zeros)
#         ipeadatar MTE12_SALMIN12 (SM nominal mensal)
# CACHES: data-raw/harmonizing-br-data/output/220B_microdados_domiciliar.rds (apagar para reextrair)
#         output/220B_sm_ref_cache.rds (SM do mes de referencia; o cache
#         antigo 220B_sm_real_cache.rds, de janeiro, fica para o 220C e o
#         220B_Log_Test, que ainda nao foram atualizados)
# VER TAMBEM: shared-pipeline/utils/renda_domiciliar_pnad.R. A trava contra o
#   242 (mediana da mesma base, figura da dissertacao nao publicada neste
#   catalogo) so roda se o cache 242_serie_renda_domiciliar.rds existir.
# ESTILO: shared-pipeline/utils/plot_theme.R
# ==============================================================================

PROMOVER <- TRUE
FORCE_REEXTRACAO <- FALSE
MANTER_ZEROS <- TRUE # universo oficial (242/238); FALSE reproduz a D02

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(here)
  library(lubridate)
  library(ipeadatar)
  library(deflateBR)
  library(Hmisc) # wtd.quantile (trava contra o 242)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

# ------------------------------------------------------------------------------
# CONSTANTES
# ------------------------------------------------------------------------------
ANOS_SEL    <- c(2002L, 2007L, 2012L, 2017L, 2022L, 2025L)
ANO_SPLICE  <- 2015L # PNAD Anual ate aqui; PNADC a partir de 2016
MULTS       <- c(1.0, 1.5, 3.0)
LABELS_MW   <- c("1 MW", "1.5 MW", "3 MW")
YLIM_MAX    <- 8000
BASE_DIR    <- here::here("data-raw", "harmonizing-br-data")
OUTPUT_DIR  <- file.path(BASE_DIR, "output")
SCRIPT_PATH <- here::here("posts", "renda-media-centil", "plot.R")
CACHE_PNAD  <- here::here("data-raw", "pnad_anual_raw")   # escrito por shared-pipeline/pnadc/020
CACHE_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2025_interview1.rds")   # escrito por shared-pipeline/pnadc/000
CACHE_MICRO <- file.path(OUTPUT_DIR, "220B_microdados_domiciliar.rds")
CACHE_SM    <- file.path(OUTPUT_DIR, "220B_sm_ref_cache.rds")
CACHE_242   <- file.path(OUTPUT_DIR, "242_serie_renda_domiciliar.rds")

source(here::here("shared-pipeline", "utils", "renda_domiciliar_pnad.R"))

# Data de referencia de cada ano — a MESMA que o 035 e o 242 usam para
# deflacionar a renda (D01). O SM e buscado e deflacionado nessa data.
data_referencia <- function(ano) {
  as.Date(ifelse(ano <= ANO_SPLICE,
    sprintf("%d-09-01", ano), # PNAD Anual: mes de referencia = setembro
    sprintf("%d-06-15", ano)  # PNADC visita 1: espalhada no ano, meio do ano
  ))
}

# ==============================================================================
# 1. SALARIO MINIMO NO MES DE REFERENCIA + DEFLACAO
# ==============================================================================
sm_df <- if (file.exists(CACHE_SM)) readRDS(CACHE_SM) else NULL

if (is.null(sm_df) || !all(ANOS_SEL %in% sm_df$ano)) {
  cat("Buscando serie SM via ipeadatar (MTE12_SALMIN12)...\n")
  sm_serie <- ipeadata("MTE12_SALMIN12") |>
    transmute(ano = as.integer(year(date)), mes = as.integer(month(date)),
              sm_nominal = value)
  sm_df <- tibble(ano = ANOS_SEL, ref_date = data_referencia(ANOS_SEL)) |>
    mutate(mes = as.integer(month(ref_date))) |>
    inner_join(sm_serie, by = c("ano", "mes")) |>
    mutate(sm_real = deflateBR::ipca(sm_nominal, ref_date, "01/2024")) |>
    select(ano, ref_date, sm_nominal, sm_real) |>
    arrange(ano)
  saveRDS(sm_df, CACHE_SM)
  cat("Cache salvo em", CACHE_SM, "\n")
} else {
  cat("SM cache carregado de", CACHE_SM, "\n")
}

sm_df <- sm_df |> filter(ano %in% ANOS_SEL) |> arrange(ano)
stopifnot(identical(sm_df$ano, ANOS_SEL))

cat("\nSalario minimo no mes de referencia da pesquisa (R$ jan/2024):\n")
print(as.data.frame(sm_df |> mutate(
  sm_real = round(sm_real),
  crescimento = round((sm_real / sm_real[1] - 1) * 100, 1)
)), row.names = FALSE)

# ==============================================================================
# 2. MICRODADOS — domiciliar per capita, universo oficial (mesma base do 242)
# ==============================================================================
if (file.exists(CACHE_MICRO) && !FORCE_REEXTRACAO &&
    all(ANOS_SEL %in% readRDS(CACHE_MICRO)$ano)) {
  cat("\nLendo cache dos microdados (", basename(CACHE_MICRO), ") — apague para reextrair.\n")
  microdados <- readRDS(CACHE_MICRO)
} else {
  anos_pnad <- ANOS_SEL[ANOS_SEL <= ANO_SPLICE]
  anos_pnadc <- ANOS_SEL[ANOS_SEL > ANO_SPLICE]

  cat("\nExtraindo PNAD Anual do bruto (renda domiciliar per capita, com zeros)...\n")
  pnad <- bind_rows(lapply(anos_pnad, extrai_pnad, cache_dir = CACHE_PNAD))
  faltando <- setdiff(anos_pnad, unique(pnad$ano))
  if (length(faltando) > 0) {
    stop(sprintf("Anos da PNAD nao extraidos: %s — conferir localiza()/dicionario",
                 paste(faltando, collapse = ", ")))
  }
  pnad <- pnad |> mutate(fonte = "PNAD Anual")

  cat("Lendo PNADC (VD5008 ja e domiciliar per capita; cache preserva zeros)...\n")
  pnadc <- readRDS(CACHE_PNADC) |>
    filter(ano %in% anos_pnadc, !is.na(peso), peso > 0, !is.na(renda_dom_pcta)) |>
    select(ano, renda_dom_pcta, peso) |>
    mutate(fonte = "PNAD Contínua")

  microdados <- bind_rows(pnad, pnadc) |>
    mutate(
      ref_date = data_referencia(ano),
      renda_real = deflateBR::ipca(renda_dom_pcta, ref_date, "01/2024")
    ) |>
    select(ano, fonte, renda_real, peso)
  saveRDS(microdados, CACHE_MICRO)
  cat("Cache gravado:", CACHE_MICRO, "\n")
}

microdados <- microdados |>
  filter(ano %in% ANOS_SEL, !is.na(renda_real), peso > 0)
if (!MANTER_ZEROS) microdados <- microdados |> filter(renda_real > 0)

cat(sprintf("  %s observacoes (domicilios na PNAD, pessoas na PNADC) em %d anos\n",
            format(nrow(microdados), big.mark = ".", decimal.mark = ","),
            length(unique(microdados$ano))))

# Trava de coerencia com a @fig-renda-mediana-domiciliar-pc: a mediana desta
# base tem de ser a do 242 (mesma unidade, mesmo universo, mesma deflacao).
if (MANTER_ZEROS && file.exists(CACHE_242)) {
  med_242 <- readRDS(CACHE_242)$principal |> select(ano, med_242 = mediana)
  med_aqui <- microdados |>
    group_by(ano) |>
    summarise(med_220B = as.numeric(wtd.quantile(renda_real, weights = peso,
                                                 probs = 0.5, normwt = FALSE)),
              pct_zero = 100 * weighted.mean(renda_real == 0, peso),
              .groups = "drop") |>
    inner_join(med_242, by = "ano") |>
    mutate(dif_pct = 100 * (med_220B / med_242 - 1))
  cat("\nTrava: mediana desta base vs 242 (tem de bater):\n")
  print(as.data.frame(med_aqui |> mutate(across(c(med_220B, med_242), round),
                                         across(c(pct_zero, dif_pct), ~ round(.x, 2)))),
        row.names = FALSE)
  if (any(abs(med_aqui$dif_pct) > 0.5)) {
    stop("Mediana diverge do 242 em mais de 0,5% — as duas figuras deixaram de ter a mesma base")
  }
}

# ==============================================================================
# 3. CENTIS
# ==============================================================================
criar_centis <- function(df) {
  df |>
    arrange(renda_real) |>
    mutate(
      peso_cum = cumsum(peso) / sum(peso),
      centil   = pmin(pmax(ceiling(peso_cum * 100L), 1L), 100L)
    )
}

cat("\nCalculando centis por ano...\n")
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
# 4. PONTOS DE INTERSECAO SM x DISTRIBUICAO
#
# Para cada (ano, mult), o centil com renda_media mais proxima de
# sm_real * mult. Resultado: 3 marcadores por curva.
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

# Parcela da populacao ABAIXO de cada limiar, direto da distribuicao (sem o
# arredondamento do centil): e o numero que o texto deve citar.
parcela_abaixo <- pontos_sm |>
  select(ano, mult, mw_label, target) |>
  rowwise() |>
  mutate(pct_abaixo = {
    d <- microdados[microdados$ano == ano, ]
    100 * sum(d$peso[d$renda_real <= target]) / sum(d$peso)
  }) |>
  ungroup() |>
  select(-target)

cat("\nMarcadores SM (centil de renda media mais proxima) e parcela abaixo do limiar:\n")
pontos_sm |>
  inner_join(parcela_abaixo, by = c("ano", "mult", "mw_label")) |>
  transmute(mw_label, ano, centil, renda_media = round(renda_media),
            pct_abaixo = round(pct_abaixo, 1)) |>
  arrange(mw_label, ano) |>
  as.data.frame() |>
  print(row.names = FALSE)

# ==============================================================================
# 5. ELEMENTOS AUXILIARES DO GRAFICO
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
# 6. FIGURA
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
             paste0("R$ ", format(x, big.mark = ",", scientific = FALSE)))
    },
    limits = c(0, YLIM_MAX * 1.01),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_colour_manual(values = cores) +
  labs(
    x = "Income percentile (P2 = poorest, P99 = richest)",
    # "Mean" e a agregacao intra-centil (detalhe de metodo, que esta na nota),
    # nao a grandeza; e o "R$" ja esta nos ticks. Paralelo ao eixo do 242.
    y = "Real per-capita household income"
  ) +
  guides(colour = guide_legend(
    title    = NULL,
    reverse  = TRUE,          # ano final no topo, 2002 na base — espelha a ordem visual
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
# 7. SALVAR / PROMOVER
# ==============================================================================
anos_str <- paste0(min(ANOS_SEL), "–", max(ANOS_SEL))
cat("\n")
if (PROMOVER) {
  finalizar_figura(
    plot        = p,
    fig_label   = "renda-media-centil",
    # Sem "Source:" aqui — finalizar_figura() ja anexa o parametro `fonte`
    # como "Source: ..."; duplicar geraria duas frases "Source:" na legenda
    fig_cap     = paste0(
      "Average per-capita household income by income percentile, Brazil, ",
      anos_str, "."
    ),
    nota        = paste0(
      "Each line is weighted mean real per-capita household income (Jan. 2024 ",
      "prices, deflated by the IPCA from each survey's reference month) by ",
      "income percentile. Income is measured per capita within the household ",
      "throughout, and households reporting no income are retained, so the ",
      "series is on the same footing as the official statistics and as ",
      "Figure~\\ref{fig-renda-mediana-domiciliar-pc}, whose median is the ",
      "50th percentile here. Years shown: ",
      paste(ANOS_SEL, collapse = ", "),
      " --- five-year snapshots of a continuous annual series, not the whole ",
      "of it. Open circles mark the percentile at 1, 1.5, and 3 times the ",
      "minimum wage in force in the survey's reference month (September for ",
      "PNAD, mid-year for PNAD Cont\\'{\\i}nua), deflated from the same date ",
      "as incomes. Y-axis truncated at R\\$", format(YLIM_MAX, big.mark = ","), "."
    ),
    apendice    = "sec-fignote-renda-media-centil",
    # A fonte descreve a FONTE, nao a selecao de anos (que vai na nota):
    # apontado pelo autor em 2026-08-09.
    fonte       = paste0(
      "IBGE — PNAD (1992–2015) and PNAD Contínua (2016–", max(ANOS_SEL),
      "), own harmonization; IPEADATA (minimum wage series ",
      "\\texttt{MTE12\\_SALMIN12})" # fignote e bloco {=latex}: crase nao vira \texttt
    ),
    script_path = SCRIPT_PATH,
    largura     = LARGURA_TEXTO,
    altura      = ALTURA_PADRAO
  )
} else {
  salvar_grafico(p, prefixo = "220B_Tese_Renda_Centil",
                 largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, formato = "png")
  cat("\nPROMOVER = FALSE: rascunho em graphs/.\n")
}

cat("\n===== 220B_Tese_Renda_Centil concluido =====\n")
