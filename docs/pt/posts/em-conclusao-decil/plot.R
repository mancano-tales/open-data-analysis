# ==============================================================================
# SCRIPT: 041K_Tese_EM_Conclusao_Decil_Lines.R
#
# Figura nova (plano 2026-07-06, Parte D): taxa de conclusao do ensino medio
# (18-24 anos) por decil de renda domiciliar per capita + media nacional,
# 1992-2024, com IC 95%. Mesma receita do 041H_Tese_Decil_Lines_Censo.R
# (splice PNAD Anual/PNADC, IC Wilson/Kish para PNAD Anual, desenho complexo
# exato para PNADC, decis D02/D08/D18, interrupcoes 2000/2010/2020-21,
# faixa de mandatos completa), trocando a variavel de ens_sup (acesso ao
# ensino superior) para medio_completo (conclusao do ensino medio).
#
# PAPEL NO ARGUMENTO (Rascunho-v2, secao "Evidence II: The Eligibility
# Precondition"): mostra a barreira PRECEDENTE caindo -- as linhas dos decis
# de baixo convergindo para as de cima na conclusao do EM antes e durante a
# equalizacao do acesso terciario documentada em @fig-decil-acesso-serie-historica.
#
# DIFERENCA vs 041H: SEM diamantes de validacao censitaria. O parquet
# Censo_Pontos_Validacao_1991_2000_2010.parquet so contem pontos para
# ens_sup (validado no Apendice B.4); nao existe uma extracao censitaria
# equivalente para medio_completo neste projeto. Construir uma exigiria uma
# nova harmonizacao de proxy censitario (na linha de D21-B/D26/D27) fora do
# escopo desta figura -- registrado como limitacao na fignote/apendice.
#
# DECISOES: D02 (renda > 0), D03 (rural Norte PNAD 2004-2015), D08/D18 (decil
#           por ano x fonte, pre-computado no parquet), D11 (sem 2020-21).
# FONTES:   output/Microdados_Jovens_18_24_1992_2025.parquet
#           5-data/pnadc_consolidado_2012_2025_interview1.rds (upa, estrato)
# VER TAMBEM: 041H_Tese_Decil_Lines_Censo.R (receita original, com diamantes
#             censitarios). ESTILO: utils/plot_theme.R
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

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET_PNAD <- file.path(BASE_DIR, "output", "Microdados_Jovens_18_24_1992_2025.parquet")
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
  "R", "02_validation", "041K_Tese_EM_Conclusao_Decil_Lines.R"
)
FIG_LABEL <- "em-conclusao-decil"

ANO_SPLICE <- 2015L # PNAD Anual ate ANO_SPLICE; PNADC a partir de ANO_SPLICE+1
DEFF <- 2.0
Z95 <- qnorm(0.975)

# ── PALETA — mesma codificacao de decis do 041H/245D (RdBu[-6]) ──────────────
LEVELS_SERIE <- c(paste0("D", 10:1), "National Average")

COR_SERIE <- c(
  setNames(RColorBrewer::brewer.pal(11, "RdBu")[-6], paste0("D", 1:10)),
  "National Average" = "#009E73" # verde Okabe
)

LBL_SERIE <- c(
  "D10" = "D10: top 10%", "D9" = "D9: 80–90%", "D8" = "D8: 70–80%",
  "D7" = "D7: 60–70%", "D6" = "D6: 50–60%", "D5" = "D5: 40–50%",
  "D4" = "D4: 30–40%", "D3" = "D3: 20–30%", "D2" = "D2: 10–20%",
  "D1" = "D1: bottom 10%", "National Average" = "National completion rate"
)

LWT_SERIE <- setNames(rep(0.5, 11), LEVELS_SERIE)
LWT_SERIE[c("D10", "D1")] <- 1.1
LWT_SERIE["National Average"] <- 1.3

# GOVERNOS: a tabela local (nomes, ini/fim, xband_min/max) e o vetor
# `gov_trans` foram REMOVIDOS em 2026-08-09. A faixa de governos agora vem
# inteira de banner_governos() (plot_theme.R), que le ANOS_TRANSICAO_GOV e
# NOMES_GOV_CURTO. Manter a copia local seria pior do que codigo morto: ela
# ainda separava Dilma II de Temer e datava as transicoes pela eleicao
# (1994.5, 2010.5...), duas convencoes que o autor aposentou nesta mesma data
# em favor da POSSE e do bloco colapsado "D.II/Tem.".

# ── IC de Wilson (score) para proporcao com n efetivo ─────────────────────────
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
# ens_sup e mantida so como chave de merge com o cache de desenho da PNADC
# (que nao tem medio_completo pre-computado) -- mesma estrategia de merge
# exato do 041H, trocando apenas a variavel de interesse.
pnad_all <- arrow::read_parquet(
  PARQUET_PNAD,
  col_select = c(
    "ano", "fonte", "idade", "peso", "renda_dom_pcta",
    "medio_completo", "ens_sup", "decil"
  )
) |>
  filter(
    !(fonte == "PNAD Contínua" & ano <= ANO_SPLICE),
    !is.na(decil), !is.na(medio_completo), !is.na(ens_sup),
    !is.na(peso), peso > 0
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
      p = Hmisc::wtd.mean(medio_completo, weights = peso, na.rm = TRUE),
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
  p_dec <- svyby(~medio_completo, ~decil_num, des, svymean, na.rm = TRUE)
  p_nac <- svymean(~medio_completo, des, na.rm = TRUE)
  list(
    dec = data.frame(
      ano = yr,
      decil_num = as.integer(as.character(p_dec$decil_num)),
      prop = p_dec$medio_completo * 100, se = p_dec$se * 100
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

cat("Sanidade 2024 (conclusao do EM, 18-24):\n")
# Diagnostico do ultimo ano: era `ano == 2024L` fixo, o que passou a mentir
# quando a serie foi estendida para 2025 (2026-08-09).
chk <- df_dec |> filter(ano == max_ano, decil_num %in% c(1L, 10L))
cat(sprintf(
  "  [%d] D1 = %.1f%% | D10 = %.1f%% | Nacional = %.1f%%\n",
  max_ano,
  chk$prop[chk$decil_num == 1L], chk$prop[chk$decil_num == 10L],
  df_nac$prop[df_nac$ano == max_ano]
))

# O recorte da faixa ao intervalo de dados agora e feito por banner_governos()
# a partir de `fim_dados`; nao ha mais tabela local para filtrar aqui.

# ==============================================================================
# 4. GRAFICO — linhas quebradas + pontos + ribbons cinza (variante A do 041H;
#    legenda no mesmo tamanho ampliado adotado na correcao 2026-07-06 da 4.1)
# ==============================================================================
p <- ggplot() +
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
  # FAIXA DE GOVERNOS: saiu do painel em 2026-08-09, pelo mesmo motivo e da
  # mesma forma que na 041H — geom_segment em y=103 e geom_text em y=106 (com
  # o ylim esticado ate 108 so para abrir espaco) deram lugar a
  # banner_governos(), composto por cima com patchwork no bloco 5. As
  # verticais continuam no painel, mas agora vem de linhas_transicao_gov(), que
  # le o mesmo vetor de anos do banner: o `gov_trans` local estava em
  # meios-anos (1994.5, 1998.5, ...) e caia meio ano ao lado da emenda entre
  # mandatos.
  linhas_transicao_gov() +
  scale_colour_manual(
    values = COR_SERIE, labels = LBL_SERIE, name = NULL,
    guide = guide_legend(
      nrow = 2, byrow = TRUE, order = 1,
      override.aes = list(
        linewidth = 1.1,
        linetype = "solid"
      )
    )
  ) +
  scale_x_anos_tese(anos = anos_disp, expand = expansion(add = c(1.0, 0.8))) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  # ylim volta a 0-100: o headroom ate 108 existia so para caber a faixa de
  # governos dentro do painel, e a faixa agora e um grafico separado.
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  labs(x = NULL, y = "Secondary completion rate (%)") +
  theme(
    # LEGENDA ABAIXO DO PAINEL (decisao do autor, 2026-08-03) — mesma mudanca
    # aplicada a 041H, e aqui ainda mais necessaria: como observado no
    # comentario que esta linha substitui, a conclusao do EM ja e alta
    # (~70-90%) nos decis superiores desde o inicio da serie, de modo que o
    # quadrante superior-esquerdo, longe de ser espaco em branco, era o
    # trecho com MAIS linhas passando por baixo da caixa. O fundo com
    # alpha = 0.7 apenas suavizava a oclusao. Em duas linhas horizontais sob
    # o eixo x, a legenda nao custa largura e libera o painel inteiro.
    legend.position        = "bottom",
    legend.justification   = "center",
    legend.background      = element_blank(),
    legend.box.margin      = margin(t = -2, r = 0, b = 0, l = 0),
    legend.margin          = margin(2, 2, 0, 2),
    # 7pt / chave 0.42cm: mesma correcao da 041H — a 7.5pt os seis itens da
    # primeira fila estouravam a largura do texto.
    legend.text            = element_text(size = 7, family = THESIS_FONT),
    legend.key.height      = unit(0.28, "cm"),
    legend.key.width       = unit(0.42, "cm"),
    legend.key             = element_blank(),
    legend.spacing.x       = unit(0.05, "cm"),
    axis.text.x            = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x     = element_blank(),
    plot.margin            = margin(8, 8, 4, 8)
  )

# ==============================================================================
# 5. SALVAR RASCUNHO (graphs/) + PROMOCAO (RETRATO, decisao do autor
#    2026-07-06: sem paginas paisagem no meio da tese -- mesma correcao
#    aplicada a 041H)
# ==============================================================================
# Altura: mesma logica da 041H — com a legenda fora do painel a figura cresce
# ~0,6in para que a area de plotagem termine MAIOR do que era com a legenda
# por dentro.
ALTURA_FIG_EM <- 4.45

# Composicao com a faixa de governos por cima (padrao 097D/041H, 2026-08-09). A
# escala x precisa ser a MESMA nos dois graficos para que as colunas fiquem
# alinhadas — o patchwork casa as larguras dos paineis, nao os dominios.
escala_x_gov <- scale_x_anos_tese(
  anos = anos_disp,
  expand = expansion(add = c(1.0, 0.8))
)
p_banner <- banner_governos(fim_dados = max_ano, scale_x = escala_x_gov)
p <- p_banner / p + patchwork::plot_layout(heights = c(1, 11))

cat("\nSalvando rascunho (retrato)...\n")
salvar_grafico(p,
  prefixo = "041K_Tese_EM_Conclusao_Decil_Lines",
  largura = LARGURA_TEXTO, altura = ALTURA_FIG_EM, unidades = "in",
  formato = "pdf"
)

nota <- paste0(
  "Each series is the share of 18--24-year-olds who completed secondary ",
  "education, by income decile. Lines are interrupted in years without a ",
  "survey (2000, 2010, 2020--2021). 95\\% CIs: Wilson/Kish for PNAD Anual, ",
  "design-based for PNAD Cont\\'{\\i}nua. No independent census validation ",
  "points are available for this variable (see appendix)."
)

finalizar_figura(
  plot = p,
  fig_label = FIG_LABEL,
  fig_cap = paste0(
    "Secondary education completion rate by income decile, Brazil 1992–",
    max_ano, "."
  ),
  nota = nota,
  apendice = "sec-fignote-em-conclusao-decil",
  fonte = paste0(
    "IBGE — PNAD (1992–2015) and PNAD Contínua (2016–", max_ano,
    "), harmonized series"
  ),
  script_path = SCRIPT_PATH,
  largura = LARGURA_TEXTO,
  altura = ALTURA_FIG_EM,
  unidades = "in"
)

cat("\n═════ Script 041K_Tese_EM_Conclusao_Decil_Lines concluido ═════\n")
