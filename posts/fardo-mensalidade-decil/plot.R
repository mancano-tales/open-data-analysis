# ==============================================================================
# SCRIPT: 232_Tese_Fardo_Mensalidade_Decil.R
#
# FARDO DA MENSALIDADE POR DECIL: indice de mensalidade do ensino superior
# (IPCA subitem, real) dividido pelo indice de renda real media de cada
# decil, base 2002 = 100. Le-se: "quanto custa a mesma mensalidade em
# unidades da renda do decil d, relativo a 2002". Queda = a mensalidade
# ficou mais toleravel para aquele estrato.
#
# E a figura que FUNDE as duas evidencias do argumento de de-commodificacao:
# o preco real de tabela nao caiu (2026-07_IPCA-Mensalidades/010), mas a
# renda da base cresceu muito (230/231) — logo o fardo despenca para D1-D5
# e quase nao se move para D10.
#
# METODO: fardo_d(ano) = 100 * [M(ano)/M(2002)] / [R_d(ano)/R_d(2002)],
# onde M = media anual do indice real de mensalidade (mensal, jan/2024 via
# IPCA — mesma base D01 de renda_real) e R_d = renda_real media do decil d.
# Como mensalidade e renda sao deflacionadas pelo MESMO IPCA, a razao
# real/real e identica a nominal/nominal — o fardo nao depende do deflator.
#
# DECISOES: D01, D02, D08/D18 (decil por ano x fonte). Fonte unica por ano
# (PNAD ate 2015, PNADC 2016+). Gaps: 1994, 2000, 2010, 2020-21 (quebrados).
# CAVEAT: indice de reajuste de cesta fixa — nao captura barateamento por
# composicao (EAD); o fardo real de mercado deve ter caido AINDA mais.
#
# DADOS: output/Microdados_Todas_Idades_1992_2025.parquet
#        2026-07_IPCA-Mensalidades/output/serie_mensalidade_mensal.rds (010)
# VER TAMBEM: 230 (GIC), 231 (renda indexada), 233 (limiares SM)
# ESTILO: utils/plot_theme.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(scales); library(here); library(RColorBrewer); library(lubridate)
  library(patchwork) # composicao com a faixa de governos (banner_governos)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

BASE_DIR  <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET   <- file.path(BASE_DIR, "output",
                        "Microdados_Todas_Idades_1992_2025.parquet")
SERIE_MEN <- here::here("4-DA-Code", "2026-07_IPCA-Mensalidades", "output",
                         "serie_mensalidade_mensal.rds")
ANO_BASE  <- 2002L

if (!file.exists(SERIE_MEN))
  stop("Serie de mensalidade nao encontrada. Rodar antes: ",
       "4-DA-Code/2026-07_IPCA-Mensalidades/R/010_IPCA_Curso_Superior_Series.R")

# Paleta/labels identicos a 041H/231/245D
LEVELS_SERIE <- paste0("D", 10:1)
COR_SERIE <- setNames(RColorBrewer::brewer.pal(11, "RdBu")[-6], paste0("D", 1:10))
LBL_SERIE <- c(
  "D10" = "D10: top 10%",  "D9" = "D9: 80–90%", "D8" = "D8: 70–80%",
  "D7"  = "D7: 60–70%", "D6" = "D6: 50–60%", "D5" = "D5: 40–50%",
  "D4"  = "D4: 30–40%", "D3" = "D3: 20–30%", "D2" = "D2: 10–20%",
  "D1"  = "D1: bottom 10%"
)
LWT_SERIE <- setNames(rep(0.5, 10), LEVELS_SERIE)
LWT_SERIE[c("D10", "D1")] <- 1.1

# GOVERNOS: tabela local e `gov_trans` REMOVIDOS em 2026-08-09 — vem tudo de
# banner_governos() / linhas_transicao_gov() (plot_theme.R). Alem de duplicar
# codigo, esta copia rotulava blocos vizinhos com o MESMO nome ("FHC", "FHC",
# "Lula", "Lula") e datava as transicoes pela eleicao, nao pela posse.

# ==============================================================================
# 1. INDICE REAL DE MENSALIDADE, MEDIA ANUAL (comeca em 2000: primeiro ano
#    civil completo da serie do subitem, que inicia em ago/1999)
# ==============================================================================
cat("Carregando serie de mensalidade (010)...\n")
mens_anual <- readRDS(SERIE_MEN) |>
  mutate(ano = year(data)) |>
  group_by(ano) |>
  filter(n() == 12L) |>
  summarise(mens_real = mean(idx_real), .groups = "drop")
stopifnot(ANO_BASE %in% mens_anual$ano)

# ==============================================================================
# 2. RENDA REAL MEDIA POR DECIL E ANO
# ==============================================================================
cat("Carregando parquet...\n")
renda_dec <- arrow::read_parquet(
  PARQUET, col_select = c("ano", "fonte", "peso", "renda_real", "decil")
) |>
  filter(
    !(fonte == "PNAD Contínua" & ano <= 2015L),
    renda_real > 0, !is.na(renda_real), peso > 0, !is.na(peso), !is.na(decil)
  ) |>
  group_by(ano, decil = as.integer(decil)) |>
  summarise(renda = weighted.mean(renda_real, peso), .groups = "drop")

# ==============================================================================
# 3. FARDO = (mensalidade/renda_d), base 2002 = 100
# ==============================================================================
fardo <- renda_dec |>
  inner_join(mens_anual, by = "ano") |>
  group_by(decil) |>
  mutate(
    renda_idx = renda / renda[ano == ANO_BASE],
    mens_idx  = mens_real / mens_real[ano == ANO_BASE],
    fardo     = 100 * mens_idx / renda_idx
  ) |>
  ungroup() |>
  mutate(serie = factor(paste0("D", decil), levels = LEVELS_SERIE)) |>
  group_by(serie) |>
  arrange(ano, .by_group = TRUE) |>
  mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L)))) |>
  ungroup()

cat("\nSANIDADE — fardo (2002=100) em anos-marco:\n")
print(fardo |>
        filter(ano %in% c(2005L, 2014L, 2024L),
               decil %in% c(1L, 2L, 5L, 10L)) |>
        pivot_wider(id_cols = serie, names_from = ano, values_from = fardo) |>
        mutate(across(-serie, ~ round(.x, 1))) |> as.data.frame(),
      row.names = FALSE)

max_ano <- max(fardo$ano)
min_ano <- min(fardo$ano)
anos_disp <- sort(unique(fardo$ano))
# O recorte da faixa (aos dois extremos, porque esta serie so comeca em 2002)
# passou para banner_governos(), via `ini_dados`/`fim_dados`, em 2026-08-09.
y_top <- max(fardo$fardo)

# ==============================================================================
# 4. FIGURA
# ==============================================================================
p <- ggplot(fardo, aes(x = ano, y = fardo, colour = serie,
                       group = interaction(serie, seg))) +
  # FAIXA DE GOVERNOS: saiu do painel em 2026-08-09 (migracao de 097D/041H/…)
  # e virou banner_governos(), composto por cima com patchwork.
  linhas_transicao_gov() +
  geom_hline(yintercept = 100, colour = "grey60", linewidth = 0.35,
             linetype = "dashed") +
  geom_line(aes(linewidth = serie)) +
  geom_point(size = 0.6, show.legend = FALSE) +
  scale_colour_manual(values = COR_SERIE, labels = LBL_SERIE, name = NULL,
                      guide = guide_legend(ncol = 1,
                        override.aes = list(linewidth = 1.1))) +
  scale_linewidth_manual(values = LWT_SERIE, guide = "none") +
  # Era seq(2000, 2024, 4) escrito a mao: grade fixa, que nao acompanhou a
  # extensao da serie e nao seguia a norma de anos de mandato
  # (WRITING-STYLE.md Sec 13.8). Passa a usar o helper compartilhado.
  scale_x_anos_tese(anos = anos_disp,
                    expand = expansion(add = c(0.6, 0.6))) +
  # Sem o headroom de 15% que existia so para caber a faixa dentro do painel.
  coord_cartesian(ylim = c(min(fardo$fardo) * 0.93, y_top * 1.03),
                  clip = "off") +
  labs(x = NULL,
       y = "Tuition burden: tuition index / decile income index (2002 = 100)") +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.985, 0.93),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(fill = "white", colour = "grey85",
                                          linewidth = 0.3),
    legend.margin          = margin(3, 7, 4, 5),
    legend.text            = element_text(size = 7, family = THESIS_FONT),
    legend.key.height      = unit(0.34, "cm"),
    legend.key.width       = unit(0.5, "cm"),
    legend.key             = element_blank(),
    axis.text.x            = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x     = element_blank()
  )

# Composicao com a faixa de governos por cima (padrao 097D/041H, 2026-08-09).
escala_x_gov <- scale_x_anos_tese(anos = anos_disp,
                                  expand = expansion(add = c(0.6, 0.6)))
p_banner <- banner_governos(fim_dados = max_ano, ini_dados = min_ano,
                            scale_x = escala_x_gov)
p <- p_banner / p + patchwork::plot_layout(heights = c(1, 11))

finalizar_figura(
  plot        = p,
  fig_label   = "fardo-mensalidade-decil",
  fig_cap     = "Higher-education tuition burden relative to household income, by income decile, Brazil, 2002--2025.",
  fonte       = "IBGE --- PNAD (2002--2015) and PNAD Contínua (2016--2025), harmonized series; IBGE --- IPCA subitem-level series via SIDRA (real tuition index); IPEADATA (minimum wage series \\texttt{MTE12\\_SALMIN12})",
  nota        = "Burden$_d$(year) $= 100 \\times$ [tuition index(year)/tuition index(2002)] / [decile-$d$ income index(year)/decile-$d$ income index(2002)] --- the same tuition bill expressed in units of decile-$d$ income, 2002 = 100. A falling line means the bill became more affordable for that stratum. Deciles computed within each survey year and source (\\hyperref[sec-decisions]{Appendix~\\ref*{sec-decisions}}, Decisions D01, D02); single source per year (PNAD Anual through 2015, PNAD Contínua from 2016). The tuition index is a fixed-panel readjustment series and does not capture compositional cheapening via distance learning, so the true market-channel burden likely fell even further than shown.",
  apendice    = "sec-fignote-fardo-mensalidade-decil",
  script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                            "02_validation", "232_Tese_Fardo_Mensalidade_Decil.R"),
  largura = LARGURA_TEXTO, altura = ALTURA_ALTA
)

cat("\n═════ 232_Tese_Fardo_Mensalidade_Decil concluido ═════\n")
