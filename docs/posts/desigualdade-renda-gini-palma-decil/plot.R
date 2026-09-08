# ==============================================================================
# SCRIPT: 236_Tese_Desigualdade_Renda_Painel.R
#
# FIGURA UNICA EM TRES PAINEIS — a queda da desigualdade de renda, 1992-2024,
# do resumo escalar a anatomia por decil:
#   (A) Indice de Gini            — escala natural, [0,49; 0,60]
#   (B) Razao de Palma            — escala natural (top 10% / 40% mais pobres)
#   (C) Renda real media por decil, indexada (2002 = 100), 10 decis + media
#
# A e B em COLUNAS LADO A LADO, cada um na sua propria escala; C ocupa a
# largura inteira embaixo, na gramatica do 041H (taxa de acesso por decil),
# que e a figura-irma deste capitulo.
#
# HISTORICO DE DESENHO (importa para nao repetir o erro):
#   v1 (235) — Gini e P90/P10 em duas figuras separadas.
#   v2       — A e B empilhados e AMBOS INDEXADOS a 2002 = 100, para caberem
#              num painel so sem eixo y duplo (proibido no projeto). REJEITADO
#              pelo autor em 2026-08-08: indexar destroi a leitura do nivel, que
#              e justamente o que se quer ver num indice de Gini. A objecao e
#              boa — "0,58 caindo para 0,50" e informacao, "100 caindo para 85"
#              nao e.
#   v3 (esta) — colunas separadas em unidades naturais. Sem eixo duplo e sem
#              indexacao: o problema que a indexacao resolvia (duas escalas
#              incompativeis) some quando os dois deixam de dividir um painel.
#
# MUDANCA DE MEDIDA (autor, 2026-08-08): a segunda coluna passou de razao
# P90/P10 para RAZAO DE PALMA (participacao do decimo mais rico dividida pela
# participacao dos quatro decis mais pobres). Palma casa melhor com o argumento
# — e literalmente o topo contra a base, nao dois pontos de corte —, e a P90/P10
# continua calculada na serie (coluna `p90p10`) para quem quiser trocar: basta
# alternar MEDIDA_COL_B.
#
# METODO: Gini ponderado (covariancia, = gini_w do 234). Palma por
# participacoes de renda acumuladas sobre o rank ponderado. P90/P10 por
# quantis ponderados (Hmisc::wtd.quantile). Renda media por decil ponderada.
#
# VARIAVEL DE RENDA: `renda_real` = `renda_dom_pcta` deflacionada pelo IPCA
# para jan/2024 (D01). ATENCAO ao aviso do DICIONARIO_DADOS.md: o nome da
# coluna mente — ate 2015 a unidade e a FAMILIA (PNAD, V4722 / V4724) e de
# 2016 em diante o DOMICILIO (PNADC, VD5008). Coincidem em domicilio de
# familia unica; divergem onde ha familias conviventes (D04/D21).
#
# CAVEAT 1 (D02): parquet filtrado com renda_real > 0 na extracao — domicilios
# sem rendimento estao fora, e o Gini sai abaixo do publicado pelo IBGE. A
# comparabilidade com a serie oficial e de trajetoria, nao de nivel.
#
# CAVEAT 2 (EMENDA DE FONTE 2015|2016) — MEDIDO, nao suposto: a troca
# PNAD->PNADC coincide com a troca familia->domicilio, e cai exatamente no
# degrau de subida de 2015->2016. Nos anos de sobreposicao (2012-2015, ambas
# as fontes no parquet) a PNADC devolve, PARA O MESMO ANO, um Gini mais alto
# em +0,0102 na media (+0,0076 em 2015) e uma Palma mais alta em +0,208 na
# media (+0,140 em 2015). O degrau observado na serie emendada e de +0,021
# (Gini) e +0,26 (Palma): ou seja, algo entre um terco e metade do salto do
# Gini, e a maior parte do salto da Palma, e ARTEFATO DE EMENDA, nao aumento
# de desigualdade. A reversao de 2015-2019 e real (a recessao aumentou a
# desigualdade), mas a figura a exagera. Diagnostico completo: rodar a
# comparacao por fonte nos anos de overlap antes de promover.
#   IMPLICACAO METODOLOGICA: a concordancia entre Gini e Palma no degrau NAO
#   e evidencia de que o degrau e real — as duas medidas compartilham a mesma
#   emenda, entao o artefato as move juntas.
#
# DECISOES: D01 (renda_real jan/2024 via IPCA), D02, D08/D18 (decil por
# ano x fonte no parquet), D20. Fonte unica por ano: PNAD Anual 1992-2015,
# PNADC 2016+ (codebook B.3/B.4). Gaps: 1994, 2000, 2010, 2020-21.
#
# DADOS: output/Microdados_Todas_Idades_1992_2025.parquet
# VER TAMBEM: 041H (figura-irma, mesma gramatica), 230 (GIC), 234 (Gini como
#             painel entre seis), 235/231 (versoes separadas, superadas)
# ESTILO: utils/plot_theme.R
# ==============================================================================

# Promocao para final/: liberada em 2026-08-09 a pedido do autor, que pediu a
# insercao da figura no capitulo 2.
PROMOVER <- TRUE

# Medida da coluna B: "palma" (padrao, pedido do autor) ou "p90p10".
MEDIDA_COL_B <- "palma"

# Modo do painel C: "log_nivel" (padrao) ou "indexado".
#
# POR QUE O PADRAO DEIXOU DE SER "indexado" (autor, 2026-08-08): num grafico
# indexado todas as series passam por 100 no ano-base POR CONSTRUCAO, o que
# cria um estrangulamento visual em 2002 que nao e fato nenhum sobre o mundo
# — e pior, sugere que em 2002 os decis estavam proximos, quando 2002 estava
# no alto do plato de desigualdade (Gini 0,584; pico de 0,603 em 1993). O
# leitor le convergencia onde havia o maximo de dispersao.
#   "log_nivel" plota o nivel real em R$ de jan/2024 com eixo y logaritmico.
# Vantagens: (1) nao ha ano-base, logo nao ha estrangulamento artificial;
# (2) em escala log, inclinacoes iguais = crescimento proporcional igual,
# entao a leitura de "quem cresceu mais rapido" continua direta; (3) a
# DISTANCIA VERTICAL entre as linhas e a desigualdade relativa, de modo que
# o estreitamento do leque no painel C passa a ser a mesma coisa que a queda
# do Gini (A) e da Palma (B) — os tres paineis contam um fato so, em vez de
# tres recortes soltos.
MODO_PAINEL_C <- "log_nivel"

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(scales); library(here); library(Hmisc); library(patchwork)
  library(RColorBrewer)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET  <- file.path(BASE_DIR, "output",
                       "Microdados_Todas_Idades_1992_2025.parquet")
SCRIPT_PATH <- file.path(BASE_DIR, "R", "02_validation",
                          "236_Tese_Desigualdade_Renda_Painel.R")
FIG_LABEL <- "desigualdade-renda-gini-palma-decil"
ANO_BASE  <- 2002L

# ── Paleta ────────────────────────────────────────────────────────────────────
# Gini herda a cor que ja o identificava no 234 (#CC79A7). Palma recebe o
# laranja Okabe, que nao aparece saturado na rampa RdBu do painel C.
COR_GINI  <- "#CC79A7"
COR_PALMA <- "#D55E00"

LEVELS_SERIE <- c(paste0("D", 10:1), "National average")
COR_SERIE <- c(
  setNames(RColorBrewer::brewer.pal(11, "RdBu")[-6], paste0("D", 1:10)),
  "National average" = "#009E73"
)
LBL_SERIE <- c(
  "D10" = "D10: top 10%", "D9" = "D9: 80–90%", "D8" = "D8: 70–80%",
  "D7"  = "D7: 60–70%",   "D6" = "D6: 50–60%", "D5" = "D5: 40–50%",
  "D4"  = "D4: 30–40%",   "D3" = "D3: 20–30%", "D2" = "D2: 10–20%",
  "D1"  = "D1: bottom 10%", "National average" = "National average"
)
LWT_SERIE <- setNames(rep(0.5, 11), LEVELS_SERIE)
LWT_SERIE[c("D10", "D1")]     <- 1.1
LWT_SERIE["National average"] <- 1.3

# Governos (so no painel C, que tem a largura inteira): a tabela local e o
# vetor `gov_trans` foram removidos em 2026-08-09 — vem tudo de
# banner_governos() / linhas_transicao_gov() (plot_theme.R).

# ------------------------------------------------------------------------------
# 1. Helpers estatisticos
# ------------------------------------------------------------------------------
# Gini, Palma e P90/P10 NAO sao calculados aqui: vem prontos do 238, que os
# computa sobre o universo oficial (com renda zero). Manter uma segunda
# implementacao neste script so criaria a chance de as duas divergirem.

# Novo grupo a cada salto > 1 ano: nenhuma linha atravessa ano sem pesquisa.
add_seg <- function(df, chave) {
  df |>
    group_by(.data[[chave]]) |>
    arrange(ano, .by_group = TRUE) |>
    mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L)))) |>
    ungroup()
}

# ------------------------------------------------------------------------------
# 2. Series
# ------------------------------------------------------------------------------
cat("Carregando parquet (pos-D20)...\n")
todas <- read_parquet(
  PARQUET, col_select = c("ano", "fonte", "peso", "renda_real", "decil")
) |>
  filter(
    !(fonte == "PNAD Contínua" & ano <= 2015L),
    renda_real > 0, !is.na(renda_real),
    peso > 0, !is.na(peso)
  )

# Paineis A e B vem da SERIE OFICIAL (238), nao do parquet principal: ela
# inclui os domicilios de renda zero, como faz o IBGE, e por isso reproduz o
# Gini publicado (desvio medio +0,0008). O parquet principal exclui os zeros
# (D02) e rebaixava o Gini em ~0,009. Ver o cabecalho do 238 e o plano
# 2026-08-08_Plano_Gini_Oficial_e_Gap_Pandemia.md.
cat("Paineis A e B — lendo a serie oficial (238)...\n")
SERIE_OFICIAL <- file.path(BASE_DIR, "output", "238_serie_desigualdade_oficial.rds")
if (!file.exists(SERIE_OFICIAL)) {
  stop("Serie oficial ausente. Rode antes: R/02_validation/238_Serie_Desigualdade_Oficial.R")
}

serie_ab <- readRDS(SERIE_OFICIAL)$oficial |>
  select(ano, gini, palma, p90p10, retroponderado) |>
  arrange(ano) |>
  mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L))))

cat("Painel C — renda media por decil, indexada...\n")
renda_dec <- todas |>
  filter(!is.na(decil)) |>
  group_by(ano, decil = as.integer(decil)) |>
  summarise(renda = weighted.mean(renda_real, peso), .groups = "drop")
renda_nac <- todas |>
  group_by(ano) |>
  summarise(renda = weighted.mean(renda_real, peso), .groups = "drop")

df_c <- bind_rows(
  renda_dec |>
    group_by(decil) |>
    mutate(idx = 100 * renda / renda[ano == ANO_BASE]) |>
    ungroup() |>
    transmute(ano, renda, idx,
              serie = factor(paste0("D", decil), levels = LEVELS_SERIE)),
  renda_nac |>
    mutate(idx = 100 * renda / renda[ano == ANO_BASE],
           serie = factor("National average", levels = LEVELS_SERIE)) |>
    select(ano, renda, idx, serie)
) |>
  add_seg("serie") |>
  mutate(yval = if (MODO_PAINEL_C == "log_nivel") renda else idx)

max_ano   <- max(df_c$ano)
anos_disp <- sort(unique(df_c$ano))
# O recorte da faixa ao intervalo de dados e feito por banner_governos().

cat("\nSANIDADE — Gini, Palma e P90/P10 em anos-marco:\n")
print(serie_ab |>
        filter(ano %in% c(1992L, 1999L, 2002L, 2014L, 2019L, 2024L)) |>
        mutate(gini = round(gini, 3), palma = round(palma, 2),
               p90p10 = round(p90p10, 1)) |>
        select(ano, gini, palma, p90p10) |> as.data.frame(),
      row.names = FALSE)

cat(sprintf("\nVariacao 1992->2024: Gini %.3f -> %.3f (%.1f%%) | Palma %.2f -> %.2f (%.1f%%)\n",
            serie_ab$gini[serie_ab$ano == 1992],  serie_ab$gini[serie_ab$ano == 2024],
            100 * (serie_ab$gini[serie_ab$ano == 2024] / serie_ab$gini[serie_ab$ano == 1992] - 1),
            serie_ab$palma[serie_ab$ano == 1992], serie_ab$palma[serie_ab$ano == 2024],
            100 * (serie_ab$palma[serie_ab$ano == 2024] / serie_ab$palma[serie_ab$ano == 1992] - 1)))

cat("\nSANIDADE — painel C, indice em 2014 e no ultimo ano:\n")
print(df_c |>
        filter(ano %in% c(2014L, max_ano)) |>
        pivot_wider(id_cols = serie, names_from = ano, values_from = idx) |>
        mutate(across(-serie, ~ round(.x, 1))) |> as.data.frame(),
      row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. Paineis A e B — colunas estreitas, cada uma na sua escala natural
# ------------------------------------------------------------------------------
# Numa coluna de ~7,5cm a grade de mandatos do scale_x_anos_tese() (8 marcas)
# ficaria ilegivel; aqui a grade e esparsa e ancorada na narrativa: inicio da
# serie, ano-base, fundo do ciclo distributivo e fim.
# O ultimo elemento era 2024 escrito a mao; com a serie estendida a 2025 isso
# deixaria de marcar o FIM, que e o papel narrativo da marca.
BREAKS_ESTREITO <- c(1992, 2002, 2014, max_ano)

# Marca da emenda de fonte (decisao do autor, 2026-08-08): a linha NAO se
# quebra em 2015|2016 — quebrar divergiria de 041H/231/234/230, que emendam
# continuamente, e viraria mudanca de convencao do capitulo inteiro. Mas a
# emenda fica assinalada, porque parte do degrau de 2015->2016 e artefato dela
# (ver CAVEAT 2 no cabecalho). Estilo SOLIDO, para nao se confundir com as
# transicoes de governo, que sao tracejadas.
X_EMENDA <- 2015.5

painel_escalar <- function(yvar, cor, ylab, fmt, breaks_y, marcar = FALSE) {
  p <- ggplot(serie_ab, aes(x = ano, y = .data[[yvar]], group = seg)) +
    geom_vline(xintercept = X_EMENDA, colour = "#9A9A9A", linewidth = 0.4) +
    geom_line(colour = cor, linewidth = 0.75) +
    # 2020-2021 sao PONTOS VAZADOS: vieram da PNADC retroponderada, coletada
    # por telefone (CATI), que subenumera a renda domiciliar e exclui
    # domicilios sem telefone — vies contra pobres e rurais (D11, razao 2).
    # Plota-los identicos aos demais afirmaria uma comparabilidade que eles
    # nao tem; o marcador vazado e a contrapartida acordada com o autor para
    # incluir os dois anos (2026-08-08).
    geom_point(data = ~ filter(.x, !retroponderado),
               colour = cor, size = 0.85) +
    geom_point(data = ~ filter(.x, retroponderado),
               colour = cor, fill = "white", shape = 21,
               size = 1.15, stroke = 0.55) +
    scale_x_continuous(breaks = BREAKS_ESTREITO,
                       labels = function(x) sprintf("%d", as.integer(x)),
                       expand = expansion(add = c(1.0, 1.0))) +
    scale_y_continuous(breaks = breaks_y, labels = fmt,
                       expand = expansion(mult = c(0.05, 0.16))) +
    labs(x = NULL, y = ylab) +
    theme(
      axis.title.y       = element_text(size = 8.5),
      axis.text          = element_text(size = 8),
      panel.grid.major.x = element_blank(),
      plot.margin        = margin(4, 8, 2, 4)
    )
  # Rotulo so na coluna A: a marca e a mesma nas tres, e repetir o texto tres
  # vezes numa figura ja densa nao acrescenta leitura. Ancorado a ESQUERDA da
  # linha (hjust = 1) porque a direita dela so restam ~8 anos de painel.
  if (marcar) {
    p <- p + annotate("text", x = X_EMENDA - 0.6, y = Inf,
                      label = "PNAD → PNADC", hjust = 1, vjust = 1.4,
                      size = 6.5 / .pt, colour = "#6E6E6E",
                      family = THESIS_FONT)
  }
  p
}

pA <- painel_escalar("gini", COR_GINI, "Gini index",
                     function(x) sprintf("%.2f", x),
                     seq(0.48, 0.62, 0.02), marcar = TRUE)

if (MEDIDA_COL_B == "palma") {
  pB <- painel_escalar("palma", COR_PALMA, "Palma ratio",
                       function(x) sprintf("%.1f", x),
                       scales::breaks_pretty(n = 6))
} else {
  pB <- painel_escalar("p90p10", COR_PALMA, "P90/P10 ratio",
                       function(x) sprintf("%.0f×", x),
                       scales::breaks_pretty(n = 6))
}

# ------------------------------------------------------------------------------
# 4. Painel C — anatomia por decil, largura inteira
# ------------------------------------------------------------------------------
# O headroom no topo do painel C existia para acomodar a faixa de governos
# DENTRO da area de plotagem — que era o arranjo ate 2026-08-09, e a razao de
# toda a ginastica de ancoras relativas em escala log (1.05 x topo, em log, e
# uma distancia muito maior do que em escala linear). Com a faixa promovida a
# grafico separado via banner_governos(), sobra so a folga visual normal.
y_c_min <- min(df_c$yval)
y_c_top <- max(df_c$yval)
if (MODO_PAINEL_C == "log_nivel") {
  .lo <- log10(y_c_min); .hi <- log10(y_c_top); .amp <- .hi - .lo
  y_lim   <- c(10^(.lo - 0.03 * .amp), 10^(.hi + 0.04 * .amp))
  y_marca <- 10^.hi
} else {
  y_lim   <- c(y_c_min * 0.95, y_c_top * 1.04)
  y_marca <- y_c_top * 1.00
}

pC <- ggplot(df_c, aes(x = ano, y = yval, colour = serie,
                       group = interaction(serie, seg))) +
  # Transicoes de governo. A primeira tentativa (2026-08-09, manha) foi um
  # segmento que PARAVA na altura da faixa, para nao cortar os rotulos. O autor
  # observou que aquilo ainda nao era o que a 097D faz, e tinha razao: a 097D
  # nao precisa parar a linha em lugar nenhum porque a faixa esta FORA do
  # painel. Adotada a mesma arquitetura aqui, a vertical volta a ser inteira e
  # o vetor de anos passa a ser o mesmo do banner (posse, nao eleicao).
  linhas_transicao_gov() +
  annotate("segment", x = X_EMENDA, xend = X_EMENDA,
           y = y_lim[1], yend = y_marca,
           colour = "#9A9A9A", linewidth = 0.4) +
  # A referencia horizontal em 100 so faz sentido no modo indexado; em nivel
  # nao ha ano-base e portanto nao ha linha de 100.
  (if (MODO_PAINEL_C == "indexado") {
    geom_hline(yintercept = 100, colour = "grey60",
               linewidth = 0.35, linetype = "dashed")
  } else NULL) +
  geom_line(aes(linewidth = serie)) +
  geom_point(size = 0.6, show.legend = FALSE) +
  scale_colour_manual(
    values = COR_SERIE, labels = LBL_SERIE, name = NULL,
    guide = guide_legend(nrow = 2, byrow = TRUE,
                         override.aes = list(linewidth = 1.1))
  ) +
  scale_linewidth_manual(values = LWT_SERIE, guide = "none") +
  scale_x_anos_tese(anos = anos_disp, expand = expansion(add = c(1.0, 1.0))) +
  (if (MODO_PAINEL_C == "log_nivel") {
    # Escala log: em nivel a serie vai de ~R$ 130 (D1, 1992) a ~R$ 6.000
    # (D10, 2024) — quase duas ordens de grandeza, ilegivel em escala linear.
    scale_y_log10(breaks = c(100, 200, 500, 1000, 2000, 5000),
                  labels = function(x) paste0("R$ ", format(x, big.mark = ",",
                                                            trim = TRUE)))
  } else {
    # Ate 250: a marca de 300 caia em cima da faixa de governos (max dos dados
    # e 269, entao a grade de 300 nao servia a leitura, so poluia o topo).
    scale_y_continuous(breaks = seq(50, 250, 50))
  }) +
  coord_cartesian(ylim = y_lim, clip = "off") +
  labs(x = NULL,
       y = if (MODO_PAINEL_C == "log_nivel") {
         "Mean real income by decile (Jan 2024 R$, log scale)"
       } else {
         "Mean real income by decile (2002 = 100)"
       }) +
  theme(
    axis.title.y       = element_text(size = 8.5),
    axis.text          = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    # Legenda embaixo em duas linhas — mesma correcao que o autor aplicou ao
    # 041H em 2026-08-03: dentro do painel ela cobria as series dos anos 1990.
    legend.position      = "bottom",
    legend.justification = "center",
    legend.background    = element_blank(),
    legend.box.margin    = margin(t = -2, r = 0, b = 0, l = 0),
    legend.margin        = margin(2, 2, 0, 2),
    legend.text          = element_text(size = 7, family = THESIS_FONT),
    legend.key.height    = unit(0.28, "cm"),
    legend.key.width     = unit(0.42, "cm"),
    legend.key           = element_blank(),
    legend.spacing.x     = unit(0.05, "cm"),
    # Margem superior a 0: os 10pt existiam para afastar o painel dos rotulos
    # de governo desenhados dentro dele. Com a faixa por cima, viravam um vao
    # entre o sublinhado e o topo da area de plotagem.
    plot.margin          = margin(0, 8, 4, 4)
  )

# ------------------------------------------------------------------------------
# 5. Composicao: (A | B) sobre C
# ------------------------------------------------------------------------------
# A faixa de governos so vale para o painel C (os paineis A e B sao estreitos e
# usam a grade esparsa BREAKS_ESTREITO), entao ela e composta DENTRO da linha
# de baixo, aninhada, para herdar a escala x de C.
escala_x_gov <- scale_x_anos_tese(anos = anos_disp,
                                  expand = expansion(add = c(1.0, 1.0)))
p_banner <- banner_governos(fim_dados = max_ano, scale_x = escala_x_gov)
pC_faixa <- p_banner / pC + plot_layout(heights = c(1, 14))

# TAGS EXPLICITAS. Com tag_levels = "A" o patchwork DESCE no aninhamento em vez
# de tratar o bloco como um elemento so: a faixa recebia "C" e o painel virava
# "D" — figura de tres paineis rotulada ate D. A lista da o rotulo de cada
# elemento na ordem de composicao (pA, pB, faixa, pC), com string vazia para a
# faixa, que nao e painel.
p <- (pA | pB) / pC_faixa +
  plot_layout(heights = c(1, 1.9)) +
  plot_annotation(tag_levels = list(c("A", "B", "", "C"))) &
  theme(plot.tag = element_text(size = 9.5, face = "bold",
                                family = THESIS_FONT))

ALTURA_FIG <- 6.10
salvar_grafico(p, prefixo = "236_Tese_Desigualdade_Renda_Painel",
               largura = LARGURA_TEXTO, altura = ALTURA_FIG, formato = "png")
salvar_grafico(p, prefixo = "236_Tese_Desigualdade_Renda_Painel",
               largura = LARGURA_TEXTO, altura = ALTURA_FIG, formato = "pdf")

# ------------------------------------------------------------------------------
# 6. Promocao (so a pedido do autor)
# ------------------------------------------------------------------------------
if (PROMOVER) {
  finalizar_figura(
    plot      = p,
    fig_label = FIG_LABEL,
    fig_cap   = paste0("Income inequality and real income growth by decile, ",
                       "Brazil, 1992\\textendash{}", max_ano, "."),
    fonte     = paste0("IBGE --- PNAD (1992--2015) and PNAD Cont\\'{i}nua ",
                       "(2016--", max_ano, "), harmonized series"),
    # NOTA REESCRITA EM 2026-08-09, antes da insercao no capitulo 2. A versao
    # anterior estava errada em dois pontos, ambos herdados de quando os tres
    # paineis saiam do mesmo parquet:
    #   (1) descrevia o painel C como "indexed to 2002 = 100" — mas o modo
    #       padrao virou log_nivel em 2026-08-08, por decisao do autor;
    #   (2) dizia que os domicilios sem rendimento estavam excluidos e que por
    #       isso o Gini ficava ABAIXO do publicado pelo IBGE. Deixou de valer
    #       para A e B quando esses paineis passaram a vir do 238, que roda no
    #       universo oficial: o desvio contra a serie do IBGE e de -0,0001. A
    #       ressalva do D02 continua valendo, mas so para o painel C.
    nota      = paste0(
      "Panel A: Gini index of real per-capita income. Panel B: Palma ratio, ",
      "the income share of the richest decile divided by that of the poorest ",
      "four deciles. Panels A and B follow the official universe, which ",
      "retains units reporting no income, and reproduce IBGE's published ",
      "series (mean deviation $-$0.0001). Panel C: mean real per-capita ",
      "income by decile, in January-2024 reais on a logarithmic axis; here ",
      "the deciles rest on the working universe of positive incomes ",
      "(decision D02), so panel C's levels are not strictly the official ",
      "ones. Lines are interrupted in years without a survey (1994, 2000, ",
      "2010, 2020--2021); hollow markers in panels A and B are 2020 and ",
      "2021, taken from the fifth visit of PNAD Cont\\'{i}nua, collected by ",
      "telephone. The income unit is the family up to 2015 (PNAD) and the ",
      "household from 2016 (PNAD Cont\\'{i}nua); the two coincide except ",
      "where several families share a dwelling. The solid vertical rule ",
      "marks that change of source. Both surveys run in 2012--2015, so the ",
      "effect of the instrument can be measured rather than assumed: for the ",
      "same year, PNAD Cont\\'{i}nua returns a Gini higher by 0.008 and a ",
      "Palma ratio higher by 0.15 on average. The observed 2015--2016 step ",
      "is 0.020 and 0.44, so roughly a third of it is an artefact of the ",
      "splice rather than a rise in inequality. Unlike the dashed rules in panel C, ",
      "which mark changes of government, it is not a break in the series. ",
      "The 2015--2019 block joins Dilma Rousseff's second term to Michel ",
      "Temer's, so the August-2016 impeachment does not appear as a boundary."
    ),
    apendice    = NULL,
    script_path = SCRIPT_PATH,
    largura = LARGURA_TEXTO, altura = ALTURA_FIG, unidades = "in"
  )
} else {
  cat("\nPROMOVER = FALSE: rascunho em graphs/. Definir TRUE para promover.\n")
}

cat("\n═════ 236_Tese_Desigualdade_Renda_Painel concluido ═════\n")
