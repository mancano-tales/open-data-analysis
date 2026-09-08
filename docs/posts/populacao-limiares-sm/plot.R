# ==============================================================================
# SCRIPT: 233_Tese_Pop_Acima_Limiares_SM.R
#
# PARCELA DOS JOVENS 18-24 ACIMA DE LIMIARES DE SALARIO MINIMO: % de jovens
# cuja renda domiciliar per capita real e >= 1x, 1.5x e 3x o salario minimo
# REAL do proprio ano, 1992-2024. E a versao temporal dos circulos de SM do
# 220B: mostra o "pool" de familias de jovens em idade universitaria para
# quem uma mensalidade se tornou pagavel crescendo ao longo do tempo.
#
# METODO: SM nominal de JANEIRO de cada ano (ipeadatar MTE12_SALMIN12, serie
# ja convertida para R$) deflacionado para jan/2024 via deflateBR (mesma
# base D01 de renda_real; mesma convencao de janeiro do 220B, la declarada).
# Share ponderado por peso. Cache proprio com validacao de completude.
#
# DECISOES: D01, D02, D03. Fonte unica por ano (PNAD ate 2015, PNADC 2016+).
# Gaps quebrados: 1994, 2000, 2010, 2020-21.
#
# DADOS: output/Microdados_Jovens_18_24_1992_2025.parquet
# VER TAMBEM: 220B (circulos de SM na curva de centis), 230/231/232 (irmaos)
# ESTILO: utils/plot_theme.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(scales); library(here); library(lubridate)
  library(ipeadatar); library(deflateBR)
  library(patchwork) # composicao com a faixa de governos (banner_governos)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET  <- file.path(BASE_DIR, "output",
                       "Microdados_Jovens_18_24_1992_2025.parquet")
CACHE_SM <- file.path(BASE_DIR, "output", "233_sm_real_todos_anos.rds")

MULTS     <- c(1.0, 1.5, 3.0)
LBL_MULTS <- c("1" = "1×", "1.5" = "1.5×", "3" = "3×")  # completados por variante
LBL_FIXO  <- c("1"   = "Above 1× the 2002 minimum wage (constant reais)",
               "1.5" = "Above 1.5× the 2002 minimum wage",
               "3"   = "Above 3× the 2002 minimum wage")
LBL_CORR  <- c("1"   = "Above 1× the current-year minimum wage",
               "1.5" = "Above 1.5× the current-year minimum wage",
               "3"   = "Above 3× the current-year minimum wage")
COR_MULTS <- c("1" = "#0072B2", "1.5" = "#009E73", "3" = "#D55E00")

# GOVERNOS: tabela local e `gov_trans` REMOVIDOS em 2026-08-09 — vem tudo de
# banner_governos() / linhas_transicao_gov() (plot_theme.R). A copia local
# rotulava blocos vizinhos com o MESMO nome ("FHC", "FHC", "Lula", "Lula") e
# datava as transicoes pela eleicao, nao pela posse.

# ==============================================================================
# 1. MICRODADOS + ANOS DISPONIVEIS
# ==============================================================================
cat("Carregando parquet (jovens 18-24)...\n")
jovens <- arrow::read_parquet(
  PARQUET, col_select = c("ano", "fonte", "peso", "renda_real")
) |>
  filter(
    !(fonte == "PNAD Contínua" & ano <= 2015L),
    renda_real > 0, !is.na(renda_real), peso > 0, !is.na(peso)
  )
ANOS <- sort(unique(jovens$ano))

# ==============================================================================
# 2. SM REAL (jan/2024) PARA TODOS OS ANOS — cache com validacao
# ==============================================================================
sm_df <- if (file.exists(CACHE_SM)) readRDS(CACHE_SM) else NULL
if (is.null(sm_df) || !all(ANOS %in% sm_df$ano)) {
  cat("Buscando SM via ipeadatar (MTE12_SALMIN12) e deflacionando...\n")
  sm_df <- ipeadatar::ipeadata("MTE12_SALMIN12") |>
    filter(month(date) == 1L, year(date) %in% ANOS) |>
    transmute(ano = as.integer(year(date)), sm_nominal = value,
              date = as.Date(date)) |>
    arrange(ano) |>
    mutate(sm_real = deflateBR::deflate(sm_nominal, date, "01/2024",
                                         index = "ipca"))
  saveRDS(sm_df, CACHE_SM)
} else {
  cat("SM: cache ok\n")
}
sm_df <- sm_df |> filter(ano %in% ANOS) |> arrange(ano)
stopifnot(identical(sm_df$ano, ANOS))

# ==============================================================================
# 3. SHARES ACIMA DOS LIMIARES — DUAS VARIANTES CONCEITUAIS
#
# (A) LIMIAR FIXO (2002): multiplos do SM REAL DE 2002, constantes em R$ de
#     jan/2024. Mede o crescimento do "pool" de familias acima de um padrao
#     de vida fixo — a leitura certa para "quantos passaram a poder pagar
#     uma mensalidade" (o preco real da mensalidade ficou ~estavel ate 2020,
#     ver 2026-07_IPCA-Mensalidades/010).
# (B) LIMIAR CORRENTE: multiplos do SM real DO PROPRIO ANO. A regua sobe
#     +110% em termos reais no periodo — o share acima de 1x SM CAI de 49%
#     para 41% mesmo com renda real crescendo. Mede compressao da
#     distribuicao rumo ao piso, NAO acessibilidade. Mantida como leitura
#     complementar; nao usar como evidencia de "pool pagante".
# ==============================================================================
SM_REAL_2002 <- sm_df$sm_real[sm_df$ano == 2002L]
stopifnot(length(SM_REAL_2002) == 1)
cat(sprintf("SM real de 2002 (jan/2024 R$): %.0f\n", SM_REAL_2002))

calcular_shares <- function(limiar_por_ano) {
  # limiar_por_ano: funcao (ano, mult) -> limiar em R$ jan/2024
  tidyr::crossing(tibble(mult = MULTS), tibble(ano = ANOS)) |>
    left_join(sm_df |> select(ano, sm_real), by = "ano") |>
    rowwise() |>
    mutate(share = {
      d   <- jovens |> filter(ano == .env$ano)
      lim <- limiar_por_ano(sm_real, mult)
      100 * weighted.mean(d$renda_real >= lim, d$peso)
    }) |>
    ungroup() |>
    mutate(serie = factor(as.character(mult), levels = names(LBL_MULTS))) |>
    group_by(serie) |>
    arrange(ano, .by_group = TRUE) |>
    mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L)))) |>
    ungroup()
}

cat("Calculando shares (limiar fixo 2002)...\n")
shares_fixo <- calcular_shares(function(sm_ano, mult) mult * SM_REAL_2002)
cat("Calculando shares (limiar corrente)...\n")
shares_corr <- calcular_shares(function(sm_ano, mult) mult * sm_ano)

cat("\nSANIDADE — share (%) em anos-marco:\n")
tab_sanidade <- function(s, nome) {
  cat(sprintf("  [%s]\n", nome))
  print(s |>
          filter(ano %in% c(1992L, 2002L, 2014L, 2024L)) |>
          pivot_wider(id_cols = serie, names_from = ano, values_from = share) |>
          mutate(across(-serie, ~ round(.x, 1))) |> as.data.frame(),
        row.names = FALSE)
}
tab_sanidade(shares_fixo, "limiar FIXO (multiplos do SM real de 2002)")
tab_sanidade(shares_corr, "limiar CORRENTE (multiplos do SM real do ano)")

shares <- shares_fixo   # variante principal para a figura A

max_ano <- max(ANOS)
# O recorte da faixa passou para banner_governos() em 2026-08-09.

# ==============================================================================
# 4. FIGURAS (A = limiar fixo 2002; B = limiar corrente, complementar)
# ==============================================================================
# LEGENDA ABAIXO DO PAINEL (2026-08-09). Ela era uma caixa flutuante no canto
# superior esquerdo, e vivia no espaco que o headroom da faixa de governos
# abria; com a faixa promovida a grafico separado, esse espaco sumiu e a caixa
# passou a cobrir a serie de 1x entre 2005 e 2010. Descer a caixa nao resolve —
# tentado em y = 0.30, cobriu a serie de 3x nos anos 1990. Nao ha quadrante
# vazio nesta figura: as tres series varrem o painel de 11% a 78%. Fora do
# painel, em uma linha horizontal, a legenda nao custa area de plotagem
# nenhuma, e e a mesma solucao ja adotada em 041H e 041K.
# O argumento legenda_pos_y foi removido: nao havia chamada que o passasse.
fig_limiares <- function(shares, labels) {
  y_top <- max(shares$share)
  anos_disp <- sort(unique(shares$ano))
  escala_x <- scale_x_anos_tese(anos = anos_disp,
                                expand = expansion(add = c(0.8, 0.8)))

  p <- ggplot(shares, aes(x = ano, y = share, colour = serie,
                     group = interaction(serie, seg))) +
    # FAIXA DE GOVERNOS: saiu do painel em 2026-08-09 (migracao de
    # 097D/041H/…) e virou banner_governos(), composto por cima com patchwork
    # no fim desta funcao — dentro dela, e nao no ponto de chamada, para que
    # as DUAS variantes (limiar fixo e limiar corrente) recebam a mesma faixa.
    linhas_transicao_gov() +
    geom_line(linewidth = 0.9) +
    geom_point(size = 0.8, show.legend = FALSE) +
    scale_colour_manual(values = COR_MULTS, labels = labels, name = NULL,
                        # Duas linhas: os tres rotulos por extenso nao cabem
                        # numa so (o terceiro saia cortado em "the 2002
                        # minimu"), e encurta-los custaria a leitura de que o
                        # limiar e o SM de 2002 mantido constante.
                        guide = guide_legend(nrow = 2, byrow = TRUE,
                          override.aes = list(linewidth = 1.2))) +
    # Era seq(1992, 2024, 4) escrito a mao — grade fixa que nao acompanhou a
    # extensao da serie nem a norma de anos de mandato (WRITING-STYLE Sec 13.8).
    escala_x +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       breaks = seq(0, 90, 10)) +
    # Sem o headroom de 20%, que existia so para caber a faixa no painel.
    coord_cartesian(ylim = c(0, y_top * 1.03), clip = "off") +
    labs(x = NULL, y = "Share of 18–24-year-olds (%)") +
    theme(
      legend.position      = "bottom",
      legend.justification = "center",
      legend.background    = element_blank(),
      legend.box.margin    = margin(t = -2, r = 0, b = 0, l = 0),
      legend.margin        = margin(2, 2, 0, 2),
      legend.text          = element_text(size = 7.5, family = THESIS_FONT),
      legend.key.height    = unit(0.28, "cm"),
      legend.key.width     = unit(0.5, "cm"),
      legend.key           = element_blank(),
      legend.spacing.x     = unit(0.05, "cm"),
      axis.text.x          = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.major.x   = element_blank()
    )

  p_banner <- banner_governos(fim_dados = max(anos_disp), scale_x = escala_x)
  p_banner / p + patchwork::plot_layout(heights = c(1, 11))
}

finalizar_figura(
  plot        = fig_limiares(shares_fixo, LBL_FIXO),
  fig_label   = "populacao-limiares-sm",
  fig_cap     = "Share of 18--24-year-olds in households above fixed real income thresholds (multiples of the 2002 minimum wage), Brazil, 1992--2025.",
  fonte       = "IBGE --- PNAD (1992--2015) and PNAD Contínua (2016--2025), harmonized series; IPEADATA (minimum wage series \\texttt{MTE12\\_SALMIN12})",
  nota        = "Share of 18--24-year-olds in households with real per-capita income at or above 1\\texttimes, 1.5\\texttimes, and 3\\texttimes\\ the January-2002 real minimum wage (R\\$675 in Jan.\\ 2024 prices), held fixed across the whole period --- the reading appropriate for tracking the stock of families able to afford a roughly constant tuition bill. A complementary variant using the current-year minimum wage (a falling share, since the wage floor itself rose faster than the distribution compressed) is available but not used as evidence of the paying pool; \\hyperref[sec-decisions]{Appendix~\\ref*{sec-decisions}}, Decisions D01--D03.",
  apendice    = "sec-fignote-populacao-limiares-sm",
  script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                            "02_validation", "233_Tese_Pop_Acima_Limiares_SM.R"),
  largura = LARGURA_TEXTO, altura = ALTURA_PADRAO
)

salvar_grafico(fig_limiares(shares_corr, LBL_CORR),
               prefixo = "233_Tese_Pop_Acima_Limiares_SM_B_corrente",
               largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, formato = "pdf")

cat("\n═════ 233_Tese_Pop_Acima_Limiares_SM concluido ═════\n")
