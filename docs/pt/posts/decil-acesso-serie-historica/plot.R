# ==============================================================================
# SCRIPT: 041H_Tese_Decil_Lines_Censo.R
#
# Versao TESE do 041C/041D: taxa de acesso ao ES (ens_sup, 18-24) por decil de
# renda domiciliar per capita + media nacional, 1992-2024, com IC 95% e
# diamantes de validacao censitaria (1991, 2000, 2010).
#
# Diferencas vs 041C/041D:
#   - Splice de fonte alinhado a serie de referencia validada (Apendice B.4)
#     e ao 245D: PNAD Anual 1992-2015, PNADC 2016-2024 (041C/D usavam
#     2011/2012). Fonte unica por ano — sem dupla contagem no overlap.
#   - IC da PNAD Anual: Wilson score com n_eff de Kish (DEFF = 2.0), nao Wald
#     (Wald degenera quando p ~ 0, caso do D1 nos anos 1990).
#   - PNADC: desenho complexo exato (svydesign, ids=~upa, strata=~estrato).
#     O cache NAO recebe filtro de rural Norte: a serie de referencia retem
#     o rural Norte na PNADC (codebook B.3; D03 vale so para PNAD 2004-2015).
#     Com isso o merge das variaveis de desenho fecha exato — stop() se nao.
#   - Linhas/pontos NAO atravessam anos sem pesquisa (2000, 2010 censitarios;
#     2020-21 excluidos por D11): segmentos quebrados por serie.
#   - Todas as 10 series coloridas com a paleta de decis do 245D
#     (RdBu[-6]: D1 = vermelho-escuro -> D10 = navy); nacional = verde Okabe.
#   - Governos: underline cinza + incumbente abreviado (padrao 245D), sem
#     caixas coloridas. Sem titulo/caption na imagem (WRITING-STYLE 13.1).
#   - DUAS variantes de geometria salvas em graphs/ para comparacao:
#       A = linhas quebradas + pontos + ribbons cinza
#       B = so pontos com IC colorido (pointrange, estilo dotplot 042C)
#     Promocao via finalizar_figura() apenas da escolhida (VARIANTE_FINAL).
#
# DECISOES: D02 (renda > 0), D03 (rural Norte PNAD 2004-2015), D08/D18 (decil
#           por ano x fonte, pre-computado no parquet), D11 (sem 2020-21),
#           D19 (ens_sup = acesso acumulado), D21-B/D26/D27 (Censo 1991 proxy).
# FONTES:   output/Microdados_Jovens_18_24_1992_2025.parquet
#           output/Censo_Pontos_Validacao_1991_2000_2010.parquet
#           5-data/pnadc_consolidado_2012_2025_interview1.rds (upa, estrato)
# VER TAMBEM: 041C/041D (rascunhos), 245D_Tese (composicao, mesma paleta),
#             042C_Tese (deltas por governo). ESTILO: utils/plot_theme.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(Hmisc)
  library(ggplot2)
  library(scales)
  library(here)
  library(survey)
  library(RColorBrewer)
  library(patchwork) # composicao com a faixa de governos (banner_governos)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999, survey.lonely.psu = "adjust")

# ── CONFIGURACAO ──────────────────────────────────────────────────────────────
# Promocao para 6-images-tables/final/: NULL enquanto se compara; depois "A"
# (linhas + pontos) ou "B" (pointrange) para gerar o bundle da escolhida.
# Promovido 2026-09-03: diamantes do Censo 2022 incluidos (validado -- ver
# docs/audits/2026-09-03_Auditoria_Ingestao_Censo_2022.md e
# docs/harmonization_decisions.md D12).
VARIANTE_FINAL <- "A"

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET_PNAD <- file.path(BASE_DIR, "output", "Microdados_Jovens_18_24_1992_2025.parquet")
PARQUET_CENSO <- file.path(BASE_DIR, "output", "Censo_Pontos_Validacao_1991_2022.parquet")
CACHED_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2025_interview1.rds")
if (!file.exists(CACHED_PNADC)) {
  # Fallback: 5-data e um diretorio irmao desta tese, um nivel acima na arvore.
  CACHED_PNADC <- file.path(
    dirname(here::here()), "5-data",
    "pnadc_consolidado_2012_2025_interview1.rds"
  )
}
SCRIPT_PATH <- here::here(
  "4-DA-Code", "2026-06_Harmonizing-BR-Data",
  "R", "02_validation", "041H_Tese_Decil_Lines_Censo.R"
)
FIG_LABEL <- "decil-acesso-serie-historica"

ANO_SPLICE <- 2015L # PNAD Anual ate ANO_SPLICE; PNADC a partir de ANO_SPLICE+1
DEFF <- 2.0
Z95 <- qnorm(0.975)

# ── PALETA — mesma codificacao de decis do 245D (RdBu[-6]) ───────────────────
# Divergente calibrada: D1 = vermelho-escuro (#67001F) -> D10 = navy (#053061).
# Healy (cap. 8): divergente quando os extremos tem identidade semantica
# distinta (baixa vs alta renda); consistencia de encoding com a figura de
# composicao por decil (245D) do mesmo capitulo.
LEVELS_SERIE <- c(paste0("D", 10:1), "National Average")

COR_SERIE <- c(
  setNames(RColorBrewer::brewer.pal(11, "RdBu")[-6], paste0("D", 1:10)),
  "National Average" = "#009E73" # verde Okabe
)

LBL_SERIE <- c(
  "D10" = "D10: top 10%", "D9" = "D9: 80–90%", "D8" = "D8: 70–80%",
  "D7" = "D7: 60–70%", "D6" = "D6: 50–60%", "D5" = "D5: 40–50%",
  "D4" = "D4: 30–40%", "D3" = "D3: 20–30%", "D2" = "D2: 10–20%",
  "D1" = "D1: bottom 10%", "National Average" = "National access rate"
)
# NOTA TERMINOLOGICA (decisao 2026-07-04): "National access rate", nao "Net
# enrollment rate" — a serie nacional e ens_sup (ja ingressou alguma vez,
# acesso acumulado, D19), nao matricula corrente. "Net enrollment rate" so
# caberia numa versao com ens_sup_a (cf. 041F).

# Hierarquia de espessura (variante A): extremos + nacional mais grossos
LWT_SERIE <- setNames(rep(0.5, 11), LEVELS_SERIE)
LWT_SERIE[c("D10", "D1")] <- 1.1
LWT_SERIE["National Average"] <- 1.3

# ── GOVERNOS — incumbente abreviado + underline (padrao 245D) ─────────────────
GOVERNOS <- data.frame(
  nome = c(
    "Itamar", "FHC I", "FHC II", "Lula I", "Lula II",
    "Dilma I", "Dilma II", "Temer", "Bolsonaro", "Lula III"
  ),
  # Numerais de mandato explicitos (2026-07-06): a versao anterior repetia
  # "FHC"/"Lula" sem distinguir I de II -- a 8pt/retrato ha folga para o
  # numeral por extenso em todos os periodos, exceto Dilma II (curto,
  # 2015-2016.5), que mantem a abreviacao "D. II".
  label_gov = c(
    "Itamar", "FHC I", "FHC II", "Lula I", "Lula II", "Dilma I",
    "D. II", "Temer", "Bolsonaro", "Lula III"
  ),
  ini = c(1992L, 1995L, 1999L, 2003L, 2007L, 2011L, 2015L, 2016L, 2019L, 2023L),
  fim = c(1994L, 1998L, 2002L, 2006L, 2010L, 2014L, 2016L, 2018L, 2022L, 2026L),
  stringsAsFactors = FALSE
)
GOVERNOS$xband_min <- GOVERNOS$ini - 0.5
GOVERNOS$xband_max <- GOVERNOS$fim + 0.5
GOVERNOS$xband_max[GOVERNOS$nome == "Dilma II"] <- 2016.5
GOVERNOS$xband_min[GOVERNOS$nome == "Temer"] <- 2016.5
gov_trans <- c(1994.5, 1998.5, 2002.5, 2006.5, 2010.5, 2014.5, 2016.5, 2018.5, 2022.5)

# ── IC de Wilson (score) para proporcao com n efetivo ─────────────────────────
# Respeita [0, 1] por construcao; nao degenera quando p ~ 0 (Wald tem largura
# ~zero nesse caso, um equivoco para o D1 nos anos 1990).
wilson_lo <- function(p, n, z = Z95) {
  denom <- 1 + z^2 / n
  (p + z^2 / (2 * n)) / denom -
    (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
}
wilson_hi <- function(p, n, z = Z95) {
  denom <- 1 + z^2 / n
  (p + z^2 / (2 * n)) / denom +
    (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
}

# ==============================================================================
# 1. PNAD ANUAL (1992-2015) — Kish n_eff (DEFF = 2.0) + Wilson
# ==============================================================================
cat("Carregando parquet (jovens 18-24)...\n")
pnad_all <- arrow::read_parquet(
  PARQUET_PNAD,
  col_select = c(
    "ano", "fonte", "idade", "peso", "renda_dom_pcta",
    "ens_sup", "decil"
  )
) |>
  filter(
    # Fonte unica por ano (overlap 2012-2015 nas duas fontes, codebook B.3):
    # PNAD Anual ate 2015, PNADC de 2016 em diante — splice da serie de
    # referencia validada no Apendice B.4 e da figura-irma 245D.
    !(fonte == "PNAD Contínua" & ano <= ANO_SPLICE),
    !is.na(decil), !is.na(ens_sup), !is.na(peso), peso > 0
  ) |>
  mutate(decil_num = as.integer(decil))

pnad_anual <- pnad_all |> filter(fonte == "PNAD Anual")
pnad_cont <- pnad_all |> filter(fonte == "PNAD Contínua")
cat(sprintf(
  "  PNAD Anual: %d anos | PNADC: %d anos\n",
  length(unique(pnad_anual$ano)), length(unique(pnad_cont$ano))
))

cat("Processando PNAD Anual (Kish n_eff + Wilson)...\n")
agrega_anual <- function(df, ...) {
  df |>
    group_by(ano, ...) |>
    summarise(
      p = Hmisc::wtd.mean(ens_sup, weights = peso, na.rm = TRUE),
      n_eff = (sum(peso)^2 / sum(peso^2)) / DEFF,
      .groups = "drop"
    ) |>
    mutate(
      prop  = p * 100,
      lower = wilson_lo(p, pmax(n_eff, 1)) * 100,
      upper = wilson_hi(p, pmax(n_eff, 1)) * 100
    ) |>
    select(-p)
}
df_dec_anual <- agrega_anual(pnad_anual, decil_num)
df_nac_anual <- agrega_anual(pnad_anual)

# ==============================================================================
# 2. PNADC (2016-2024) — desenho complexo exato (upa, estrato)
# ==============================================================================
cat("Carregando cache PNADC (variaveis de desenho)...\n")
if (!file.exists(CACHED_PNADC)) stop("Cache PNADC nao encontrado: ", CACHED_PNADC)
raw_pnadc <- readRDS(CACHED_PNADC)
names(raw_pnadc) <- tolower(names(raw_pnadc))

# NAO excluir rural Norte do cache: a serie de referencia retem o rural Norte
# na PNADC (codebook B.3); D03 aplica-se apenas a PNAD Anual 2004-2015 e ja
# esta embutida no parquet. Excluir aqui (como fazia o 041D) quebra o merge.
raw_pnadc_f <- raw_pnadc |>
  filter(
    !is.na(peso), peso > 0, !is.na(renda_dom_pcta), renda_dom_pcta > 0,
    idade >= 18L, idade <= 24L, !is.na(ens_sup),
    ano > ANO_SPLICE
  )

cat("Merge das variaveis de desenho (chave + indexador de ocorrencia)...\n")
pnad_cont_p <- pnad_cont |>
  group_by(ano, idade, peso, renda_dom_pcta, ens_sup) |>
  mutate(occ = row_number()) |>
  ungroup()
raw_pnadc_p <- raw_pnadc_f |>
  select(ano, idade, peso, renda_dom_pcta, ens_sup, estrato, upa) |>
  group_by(ano, idade, peso, renda_dom_pcta, ens_sup) |>
  mutate(occ = row_number()) |>
  ungroup()

pnadc_d <- inner_join(pnad_cont_p, raw_pnadc_p,
  by = c("ano", "idade", "peso", "renda_dom_pcta", "ens_sup", "occ")
) |>
  select(-occ)

# Merge parcial silencioso enviesaria os SEs — abortar, nao avisar
if (nrow(pnadc_d) != nrow(pnad_cont)) {
  stop(sprintf(
    "Mismatch no merge das variaveis de desenho: esperado %d, obtido %d.",
    nrow(pnad_cont), nrow(pnadc_d)
  ))
}
cat(sprintf(
  "  Merge exato: %s linhas\n",
  format(nrow(pnadc_d), big.mark = ".", decimal.mark = ",")
))

cat("Calculando SE com desenho complexo por ano...\n")
pnadc_results <- lapply(sort(unique(pnadc_d$ano)), function(yr) {
  df_yr <- filter(pnadc_d, ano == yr)
  des <- svydesign(
    ids = ~upa, strata = ~estrato, weights = ~peso,
    data = df_yr, nest = TRUE
  )
  p_dec <- svyby(~ens_sup, ~decil_num, des, svymean, na.rm = TRUE)
  p_nac <- svymean(~ens_sup, des, na.rm = TRUE)
  list(
    dec = data.frame(
      ano = yr,
      decil_num = as.integer(as.character(p_dec$decil_num)),
      prop = p_dec$ens_sup * 100, se = p_dec$se * 100
    ) |>
      mutate(lower = pmax(prop - Z95 * se, 0), upper = pmin(prop + Z95 * se, 100)) |>
      select(-se),
    nac = data.frame(
      ano = yr, prop = as.numeric(p_nac) * 100,
      se = as.numeric(SE(p_nac)) * 100
    ) |>
      mutate(lower = pmax(prop - Z95 * se, 0), upper = pmin(prop + Z95 * se, 100)) |>
      select(-se)
  )
})
df_dec_pnadc <- bind_rows(lapply(pnadc_results, `[[`, "dec"))
df_nac_pnadc <- bind_rows(lapply(pnadc_results, `[[`, "nac"))

# ==============================================================================
# 3. CONSOLIDAR + SEGMENTOS (quebra nos anos sem pesquisa)
# ==============================================================================
df_dec <- bind_rows(
  df_dec_anual |> select(ano, decil_num, prop, lower, upper),
  df_dec_pnadc
) |>
  mutate(serie = factor(paste0("D", decil_num), levels = LEVELS_SERIE))

df_nac <- bind_rows(
  df_nac_anual |> select(ano, prop, lower, upper),
  df_nac_pnadc
) |>
  mutate(serie = factor("National Average", levels = LEVELS_SERIE))

# Segmento: dentro de cada serie, novo grupo a cada salto > 1 ano.
# Nenhuma linha ou ribbon deve atravessar 2000, 2010 ou 2020-21.
add_seg <- function(df) {
  df |>
    group_by(serie) |>
    arrange(ano, .by_group = TRUE) |>
    mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L)))) |>
    ungroup()
}
df_dec <- add_seg(df_dec)
df_nac <- add_seg(df_nac)

anos_disp <- sort(unique(df_dec$ano))
max_ano <- max(anos_disp)
cat(sprintf(
  "Anos com dados: %d a %d (%d anos; gaps: %s)\n",
  min(anos_disp), max_ano, length(anos_disp),
  paste(setdiff(seq(min(anos_disp), max_ano), anos_disp), collapse = ", ")
))

# Sanidade vs benchmarks da serie de referencia (ens_sup 18-24)
chk <- df_dec |> filter(ano == 2024L, decil_num %in% c(1L, 10L))
cat(sprintf(
  "Sanidade 2024: D1 = %.1f%% (ref ~9.6) | D10 = %.1f%% (ref ~72.7) | Nac = %.1f%% (ref ~28.5)\n",
  chk$prop[chk$decil_num == 1L], chk$prop[chk$decil_num == 10L],
  df_nac$prop[df_nac$ano == 2024L]
))
cat("IC Wilson D1 anos 1990 (largura > 0 mesmo com p ~ 0):\n")
print(df_dec |> filter(decil_num == 1L, ano <= 1999L) |>
  transmute(ano,
    prop = round(prop, 2), lower = round(lower, 2),
    upper = round(upper, 2)
  ), n = Inf)

# Filtrar GOVERNOS ao intervalo de dados
GOVERNOS <- GOVERNOS[GOVERNOS$ini <= max_ano, ]
GOVERNOS$xband_max[nrow(GOVERNOS)] <- pmin(
  GOVERNOS$xband_max[nrow(GOVERNOS)],
  max_ano + 0.5
)
gov_trans <- gov_trans[gov_trans <= max_ano + 0.5]

# ==============================================================================
# 4. PONTOS DE VALIDACAO CENSITARIA
# ==============================================================================
cat("Carregando pontos censitarios...\n")
censo_all <- arrow::read_parquet(PARQUET_CENSO)
censo_dec <- censo_all |>
  filter(decil > 0L) |>
  mutate(serie = factor(paste0("D", as.integer(decil)), levels = LEVELS_SERIE))
censo_nac <- censo_all |>
  filter(decil == 0L) |>
  mutate(serie = factor("National Average", levels = LEVELS_SERIE))

# ==============================================================================
# 5. ELEMENTOS COMUNS AS DUAS VARIANTES
# ==============================================================================
camadas_comuns <- function() {
  list(
    # FAIXA DE GOVERNOS: saiu daqui em 2026-08-09. Antes era desenhada DENTRO
    # do painel (geom_segment em y=103, geom_text em y=106, com o ylim
    # esticado ate 108 para abrir espaco). Agora vem de banner_governos() do
    # plot_theme.R — o mesmo codigo da 097D —, composto POR CIMA com patchwork.
    # Dois defeitos morrem com isso: as verticais tracejadas nao alcancam mais
    # os rotulos (a faixa esta fora do painel), e os tracos deixam de se
    # encostar (o banner recua 0.08 ano de cada lado, em vez de ir de
    # ini-0.5 a fim+0.5, que fazia segmentos vizinhos colidirem e produzia a
    # linha quase-continua com falhas de antialiasing que o autor apontou).
    # As verticais de transicao seguem no painel, agora sem nada para cortar E
    # ancoradas nos MESMOS anos das fronteiras do banner. Antes vinham do vetor
    # `gov_trans` local, em meios-anos (1994.5, 1998.5, ...), enquanto o banner
    # usa anos inteiros: a linha caia meio ano ao lado da emenda entre os
    # mandatos em vez de sobre ela. Apontado pelo autor em 2026-08-09.
    linhas_transicao_gov(),
    # Diamantes censitarios (1991, 2000, 2010) — 2000/2010 preenchem os gaps
    geom_point(
      data = censo_dec,
      aes(x = ano, y = prop, fill = serie),
      shape = 23, colour = "white", size = 2.4, stroke = 0.55
    ),
    geom_point(
      data = censo_nac,
      aes(x = ano, y = prop, fill = serie),
      shape = 23, colour = "white", size = 4.2, stroke = 0.9
    ),
    scale_colour_manual(
      values = COR_SERIE, labels = LBL_SERIE, name = NULL,
      guide = guide_legend(
        nrow = 2, byrow = TRUE, order = 1,
        override.aes = list(
          linewidth = 1.1,
          linetype = "solid"
        )
      )
    ),
    scale_fill_manual(values = COR_SERIE, guide = "none"),
    # Grade de anos padronizada (WRITING-STYLE.md Sec 13.8) em vez da grade
    # manual seq(1992, 2024, 4) usada ate 2026-07-06
    scale_x_anos_tese(
      anos = anos_disp,
      expand = expansion(add = c(1.0, 0.8))
    ),
    scale_y_continuous(
      breaks = seq(0, 100, 10),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.01, 0.01))
    ),
    # ylim volta a 0-100: o headroom ate 108 existia so para caber a faixa de
    # governos dentro do painel, e a faixa agora e um grafico separado.
    coord_cartesian(ylim = c(0, 100), clip = "off"),
    labs(x = NULL, y = "Access rate (%)"),
    theme(
      # LEGENDA ABAIXO DO PAINEL (decisao do autor, 2026-08-03).
      #
      # Historico: ate aqui a legenda ficava DENTRO do painel, no canto
      # superior esquerdo, sob o argumento de que aquele quadrante era espaco
      # em branco (so o D10 passa de ~45% antes de 2000) e de que uma legenda
      # externa a direita custaria ~18% da largura util. Os dois pontos eram
      # verdadeiros em 2026-07-06, mas a premissa do "espaco em branco" nao
      # era: a caixa cobria o trecho 1992-1999 da serie D9 (41-51%) e o
      # diamante censitario de 2000 do D9, ou seja, escondia dado, nao fundo.
      # O alpha = 0.75 do fundo atenuava, mas nao resolvia.
      #
      # Solucao adotada: legenda horizontal em DUAS LINHAS sob o eixo x. Nao
      # custa largura nenhuma (ao contrario da legenda a direita, que era o
      # motivo original de coloca-la dentro) e libera 100% do painel. O custo
      # e vertical (~0,5in), compensado com folga pelo aumento de ALTURA na
      # chamada de promocao, de modo que a area de plotagem cresce em vez de
      # encolher.
      legend.position        = "bottom",
      legend.justification   = "center",
      legend.background      = element_blank(),
      legend.box.margin      = margin(t = -2, r = 0, b = 0, l = 0),
      legend.margin          = margin(2, 2, 0, 2),
      # 7pt / chave 0.42cm: a 7.5pt e 0.50cm os seis itens da primeira fila
      # estouravam por alguns milimetros a largura do texto e o "D5: 40-50%"
      # saia cortado na borda direita.
      legend.text            = element_text(size = 7, family = THESIS_FONT),
      legend.key.height      = unit(0.28, "cm"),
      legend.key.width       = unit(0.42, "cm"),
      legend.key             = element_blank(),
      legend.spacing.x       = unit(0.05, "cm"),
      axis.text.x            = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.major.x     = element_blank(),
      plot.margin            = margin(8, 8, 4, 8)
    )
  )
}

# ==============================================================================
# 6. VARIANTE A — linhas quebradas + pontos + ribbons cinza
# ==============================================================================
p_A <- ggplot() +
  # Ribbons de IC: cinza translucido uniforme (10 ribbons coloridos sobre-
  # postos viram poluicao; o cinza comunica incerteza sem competir com a cor)
  geom_ribbon(
    data = df_dec,
    aes(
      x = ano, ymin = lower, ymax = upper,
      group = interaction(serie, seg)
    ),
    fill = "grey82", alpha = 0.4, inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = df_nac,
    aes(
      x = ano, ymin = lower, ymax = upper,
      group = interaction(serie, seg)
    ),
    fill = "grey82", alpha = 0.4, inherit.aes = FALSE
  ) +
  # Linhas quebradas nos gaps + ponto em cada ano observado
  geom_line(
    data = df_dec,
    aes(
      x = ano, y = prop, colour = serie, linewidth = serie,
      group = interaction(serie, seg)
    )
  ) +
  geom_line(
    data = df_nac,
    aes(
      x = ano, y = prop, colour = serie, linewidth = serie,
      group = interaction(serie, seg)
    ),
    linetype = "longdash"
  ) +
  geom_point(
    data = df_dec,
    aes(x = ano, y = prop, colour = serie),
    size = 0.7, show.legend = FALSE
  ) +
  geom_point(
    data = df_nac,
    aes(x = ano, y = prop, colour = serie),
    size = 0.9, show.legend = FALSE
  ) +
  scale_linewidth_manual(values = LWT_SERIE, guide = "none") +
  camadas_comuns()

# ==============================================================================
# 7. VARIANTE B — so pontos com IC colorido (estilo dotplot 042C)
# ==============================================================================
p_B <- ggplot() +
  geom_pointrange(
    data = df_dec,
    aes(
      x = ano, y = prop, ymin = lower, ymax = upper,
      colour = serie
    ),
    size = 0.16, linewidth = 0.45
  ) +
  geom_pointrange(
    data = df_nac,
    aes(
      x = ano, y = prop, ymin = lower, ymax = upper,
      colour = serie
    ),
    size = 0.22, linewidth = 0.55
  ) +
  camadas_comuns()

# ==============================================================================
# 8. SALVAR RASCUNHOS (graphs/) — comparacao A vs B
# ==============================================================================
# Decisao 2026-07-03: geometria de LINHAS (A) escolhida; B mantida no codigo
# como referencia, sem salvar. Formato RETRATO (decisao do autor 2026-07-06:
# nenhuma pagina paisagem no meio da tese) -- testado contra paisagem antes
# dessa decisao; corrigido o tamanho de fonte da faixa de governos e da
# legenda para a coluna de texto normal (ver comentarios em camadas_comuns()).
GERAR_B <- FALSE
cat("\nSalvando rascunho (A, retrato)...\n")
# Altura: com a legenda movida para baixo do painel (2026-08-03), a figura
# cresce ~0,6in para que a AREA DE PLOTAGEM fique maior do que era com a
# legenda por dentro — o objetivo da mudanca era ganhar painel, nao trocar
# oclusao por encolhimento.
ALTURA_FIG_2_1 <- 4.45

# Composicao com a faixa de governos por cima (padrao 097D, 2026-08-09). A
# escala x precisa ser a MESMA nos dois para que as colunas fiquem alinhadas —
# o patchwork casa as larguras dos paineis, nao os dominios.
escala_x_gov <- scale_x_anos_tese(
  anos = anos_disp,
  expand = expansion(add = c(1.0, 0.8))
)
p_banner <- banner_governos(fim_dados = max_ano, scale_x = escala_x_gov)
p_A <- p_banner / p_A + patchwork::plot_layout(heights = c(1, 11))

salvar_grafico(p_A,
  prefixo = "041H_Tese_Decil_Lines_Censo_A_linhas_retrato",
  largura = LARGURA_TEXTO, altura = ALTURA_FIG_2_1, unidades = "in",
  formato = "pdf"
)
if (GERAR_B) {
  salvar_grafico(p_B,
    prefixo = "041H_Tese_Decil_Lines_Censo_B_pontos",
    largura = LARGURA_TEXTO, altura = ALTURA_ALTA, formato = "pdf"
  )
}

# ==============================================================================
# 9. PROMOCAO (apenas a variante escolhida; VARIANTE_FINAL = "A" ou "B")
# ==============================================================================
if (!is.null(VARIANTE_FINAL)) {
  stopifnot(VARIANTE_FINAL %in% c("A", "B"))
  p_final <- if (VARIANTE_FINAL == "A") p_A else p_B

  frase_geometria <- if (VARIANTE_FINAL == "A") {
    "Lines are interrupted in years without a survey (2000, 2010, 2020--2021). "
  } else {
    "Shown as points with 95\\% confidence bars; years without a survey carry no point. "
  }

  # Fignote enxuta (WRITING-STYLE.md Sec 13.7, politica 2026-07-06): o
  # detalhe de decisoes de harmonizacao (D02, D03, D08, D11, D18, D19, D26,
  # D27) e dos metodos de IC migrou para a entrada da figura em
  # "Extended Figure Notes" (apendice), linkada via `apendice=` abaixo.
  nota <- paste0(
    "Each series is the share of 18--24-year-olds ever enrolled in ",
    "tertiary education, by income decile. ", frase_geometria,
    "95\\% CIs: Wilson/Kish for PNAD Anual, design-based for PNAD Cont\\'{\\i}nua. ",
    "Diamonds: census validation points."
  )

  finalizar_figura(
    plot = p_final,
    fig_label = FIG_LABEL,
    fig_cap = paste0(
      "Tertiary education access rate by income decile, Brazil 1992–",
      max_ano, "."
    ),
    nota = nota,
    apendice = "sec-fignote-decil-acesso-serie-historica",
    fonte = paste0(
      "IBGE — PNAD (1992–2015) and PNAD Contínua (2016–",
      max_ano, "), harmonized series; Demographic Censuses 1991, 2000, ",
      "2010, and 2022 (`censobr`; 2022 via `import_microdata22_controlado()`, ",
      "dev branch)"
    ),
    script_path = SCRIPT_PATH,
    largura = LARGURA_TEXTO,
    altura = ALTURA_FIG_2_1,
    unidades = "in"
  )
} else {
  cat(
    "\nVARIANTE_FINAL = NULL: rascunhos A e B salvos em graphs/;",
    "definir \"A\" ou \"B\" e rodar novamente para promover.\n"
  )
}

cat("\n═════ Script 041H_Tese_Decil_Lines_Censo concluido ═════\n")
