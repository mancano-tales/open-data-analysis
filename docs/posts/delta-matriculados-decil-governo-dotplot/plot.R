# ==============================================================================
# SCRIPT: 042C_Tese_Dotplot.R
#
# Versão dotplot de Cleveland do 042C_Tese.R — MESMOS dados, MESMO grid 3x3,
# trocando geom_col()+geom_errorbar() por geom_pointrange() (ponto + linha de
# IC). Criado a pedido do autor para comparar as duas convenções visuais
# lado a lado; após comparação, o autor preferiu esta versão (2026-06-30) —
# é a que está citada em 3-texts/0202-...qmd a partir de agora. A versão em
# barras (042C_Tese.R) NÃO foi apagada — fica como referência/comparação.
#
# Por que o dotplot: Healy (2026, Caps. 5-6, via Cleveland & McGill 1984)
# recomenda dotplots em vez de barras para comparação categórica com
# incerteza — a extremidade da barra e o ponto carregam a mesma informação
# posicional, mas o dotplot não precisa da área da barra (menos tinta) e
# separa mais claramente a ESTIMATIVA (o ponto) do INTERVALO (a linha),
# enquanto na barra os dois se misturam visualmente com a própria extensão
# da barra.
#
# SPLICE POR MANDATO (autor, 2026-08-07) — ver a nota extensa no bloco GOVERNOS.
# Em resumo: cada painel usa UMA única survey nos dois extremos, aproveitando
# que 2012–2015 existe nas duas fontes do parquet. Antes, o corte único em
# 2011/2012 fazia o painel de Dilma I comparar PNAD 2011 com PNADC 2015 —
# somando à mudança real a descontinuidade de instrumento.
#
# PARA INSERÇÃO NO .qmd:
#   fig-cap: "Total change in higher education enrollment rate (ages 18–24)
#             by income decile and presidential term, Brazil 1992–2023.
#             Source: IBGE — PNAD (1992–2015) and PNAD Contínua (2015–2023)."
#   fig-label: fig-delta-matriculados-decil-governo (mesmo label da versão em
#              barras — não há referência cruzada @fig-... em outro lugar do
#              capítulo a quebrar ao trocar o arquivo apontado)
#
# VER TAMBÉM: 042C_Tese.R (versão em barras — mantida para comparação)
# ==============================================================================

MOSTRAR_TITULO <- FALSE
MOSTRAR_LEGENDA <- TRUE
MOSTRAR_FONTE <- FALSE

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(here)
  library(survey)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 14))
options(scipen = 999, survey.lonely.psu = "adjust")

ANO_FIM <- 2023L

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET_PNAD <- file.path(BASE_DIR, "output", "Microdados_Jovens_18_24_1992_2024.parquet")
CACHED_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2024_interview1.rds")
if (!file.exists(CACHED_PNADC)) {
  # Fallback: 5-data e um diretorio irmao desta tese, um nivel acima na arvore.
  CACHED_PNADC <- file.path(
    dirname(here::here()), "5-data",
    "pnadc_consolidado_2012_2024_interview1.rds"
  )
}

# ==============================================================================
# 1-4b. CARREGAR E PREPARAR DADOS — idêntico a 042C_Tese.R (ver lá para o
# detalhe metodológico de cada etapa: Kish/DEFF para PNAD Anual, svydesign
# exato para PNADC, deltas por governo, painel "Full period").
# ==============================================================================
cat("Carregando parquet...\n")
# NOTA (2026-08-07): o filtro `!(fonte == "PNAD Anual" & ano >= 2012)` que ficava
# aqui foi REMOVIDO de propósito. Ele resolvia a sobreposição 2012–2015 (anos
# presentes nas duas fontes) descartando a PNAD Anual, o que forçava o painel de
# Dilma I a atravessar as duas surveys. Agora o overlap é preservado e a escolha
# da fonte é feita por mandato, mais abaixo. Consequência: `df_dec` passa a ter
# DUAS linhas por (ano, decil) em 2012–2015 — toda leitura dele precisa filtrar
# também por `fonte`.
pnad_all <- arrow::read_parquet(PARQUET_PNAD) |>
  filter(!is.na(renda_dom_pcta), !is.na(peso)) |>
  mutate(decil_num = as.integer(decil)) |>
  filter(!is.na(decil_num), !is.na(ens_sup_a))

pnad_anual <- pnad_all |> filter(fonte == "PNAD Anual")
pnad_cont <- pnad_all |> filter(fonte == "PNAD Contínua")

cat("Processando PNAD Anual (1992-2011)...\n")
df_dec_anual <- pnad_anual |>
  group_by(ano, decil_num) |>
  summarise(
    prop = Hmisc::wtd.mean(ens_sup_a, weights = peso, na.rm = TRUE) * 100,
    n_eff = (sum(peso)^2 / sum(peso^2)) / 2.0,
    .groups = "drop"
  ) |>
  mutate(
    se = sqrt((prop / 100) * (1 - prop / 100) / pmax(n_eff, 1)) * 100,
    fonte = "PNAD Anual"
  )

cat("Carregando cache PNADC...\n")
raw_pnadc <- readRDS(CACHED_PNADC)
names(raw_pnadc) <- tolower(names(raw_pnadc))
raw_pnadc <- raw_pnadc |>
  filter(!(as.integer(uf) %in% c(11L, 12L, 13L, 14L, 15L, 16L) & as.integer(local) == 2L))
raw_pnadc_f <- raw_pnadc |>
  filter(
    !is.na(peso), peso > 0, !is.na(renda_dom_pcta), renda_dom_pcta > 0,
    idade >= 18, idade <= 24, !is.na(ens_sup_a)
  )

pnad_cont_p <- pnad_cont |>
  group_by(ano, idade, peso, renda_dom_pcta, ens_sup_a) |>
  mutate(occ = row_number()) |>
  ungroup()
raw_pnadc_p <- raw_pnadc_f |>
  select(ano, idade, peso, renda_dom_pcta, ens_sup_a, estrato, upa) |>
  group_by(ano, idade, peso, renda_dom_pcta, ens_sup_a) |>
  mutate(occ = row_number()) |>
  ungroup()
pnadc_d <- inner_join(pnad_cont_p, raw_pnadc_p,
  by = c("ano", "idade", "peso", "renda_dom_pcta", "ens_sup_a", "occ")
) |>
  select(-occ)

cat("Calculando SE com desenho complexo para PNADC...\n")
pnadc_decil <- lapply(sort(unique(pnadc_d$ano)), function(yr) {
  df_yr <- filter(pnadc_d, ano == yr)
  if (nrow(df_yr) == 0) {
    return(NULL)
  }
  des <- svydesign(
    ids = ~upa, strata = ~estrato, weights = ~peso,
    data = df_yr, nest = TRUE
  )
  p <- svyby(~ens_sup_a, ~decil_num, des, svymean, na.rm = TRUE)
  data.frame(
    ano = yr, decil_num = as.integer(as.character(p$decil_num)),
    prop = p$ens_sup_a * 100, se = p$se * 100,
    fonte = "PNAD Contínua"
  )
})
df_dec <- bind_rows(df_dec_anual, bind_rows(pnadc_decil)) |>
  arrange(ano, fonte, decil_num)

# ------------------------------------------------------------------------------
# Classificação do sinal para a cor do ponto (autor, 2026-08-03).
#
# Antes, a cor codificava apenas o sinal do delta, o que dava a MESMA saliência
# visual a uma variação de +15pp e a uma de +0,5pp cujo IC de 95% atravessa o
# zero. O leitor lia como "ganho" o que os dados não distinguem de "nada
# aconteceu". Agora o ponto fica CINZA quando o IC contém o zero — isto é,
# quando a mudança não é estatisticamente distinguível de zero ao nível de 5% —
# e só recebe azul/laranja quando o intervalo inteiro está de um lado do zero.
#
# Nota: o teste é o do IC do próprio delta (ci_low > 0 ou ci_high < 0), com
# se_delta = sqrt(se1^2 + se0^2), que trata os dois anos como independentes.
# É conservador contra a PNAD Anual (Kish/DEFF 2.0) e ignora a correlação entre
# ondas do painel da PNADC, de modo que o IC tende a ser largo demais — logo,
# um ponto que sai colorido sai colorido com folga.
# ------------------------------------------------------------------------------
NIVEIS_SINAL <- c("Gain", "Loss", "Not significant")

# Rótulos NOMEADOS (não posicionais). Importante: quando um dos três níveis não
# ocorre na figura, o ggplot o descarta da legenda (ver a nota em `scale_colour_
# manual`, mais abaixo) — e um vetor posicional passaria a rotular a chave errada
# silenciosamente. Com o vetor nomeado, cada rótulo segue o seu nível.
ROTULOS_SINAL <- c(
  "Gain"            = "Gain (pp total)",
  "Loss"            = "Loss (pp total)",
  "Not significant" = "Not significant (95% CI crosses 0)"
)

classificar_sinal <- function(delta, ci_low, ci_high) {
  factor(
    ifelse(ci_low > 0, "Gain",
      ifelse(ci_high < 0, "Loss", "Not significant")
    ),
    levels = NIVEIS_SINAL
  )
}

# ------------------------------------------------------------------------------
# Splice PNAD/PNADC POR MANDATO (autor, 2026-08-07).
#
# O problema: cada ponto desta figura é uma DIFERENÇA entre dois anos. Quando os
# dois anos vêm de surveys diferentes, o número exibido soma a mudança real a
# três descontinuidades de instrumento — desenho amostral e questionário; a
# unidade da renda per capita (familiar na PNAD Anual via V4722/V4724, domiciliar
# na PNADC via VD5008 — decisão D21, registrada em 2026-08-06); e a cobertura do
# Norte rural (D03, esta sim neutralizada por filtro logo acima). Numa série
# contínua a emenda aparece como um degrau que o leitor vê e desconta; dentro de
# um delta ela some.
#
# A solução: como 2012–2015 existe nas DUAS fontes do parquet (PNAD Anual n≈37
# mil/ano, PNADC n≈51 mil/ano) e esta figura nunca desenha linha atravessando a
# emenda, a fonte é escolhida por MANDATO, não por ano. Assim Dilma I (2011–15)
# fica inteiro na PNAD Anual e Dilma II/Temer (2015–19) inteiro na PNADC, e
# nenhum painel de mandato atravessa as duas surveys.
#
# O que se paga por isso: 2015 entra como PNAD Anual num painel e como PNADC no
# seguinte, de modo que o NÍVEL do fim de Dilma I não é o mesmo do início de
# Dilma II/Temer. Não há descontinuidade visual (não há linha), mas isso está
# declarado na fignote — é a contrapartida honesta da escolha.
#
# O painel "Full period" (1992→2023) segue cross-survey por necessidade: não
# existe fonte única cobrindo as duas pontas.
#
# Para voltar ao comportamento antigo (corte único), basta pôr "PNAD Contínua"
# na coluna `fonte` a partir de Dilma I — mas leia o parágrafo acima antes.
# ------------------------------------------------------------------------------
GOVERNOS <- data.frame(
  nome = c(
    "Itamar", "FHC I", "FHC II", "Lula I", "Lula II",
    "Dilma I", "Dilma II/Temer", "Bolsonaro"
  ),
  ini = c(1992, 1995, 1999, 2003, 2007, 2011, 2015, 2019),
  fim = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  fonte = c(rep("PNAD Anual", 6), rep("PNAD Contínua", 2)),
  stringsAsFactors = FALSE
)

# `nearest()` agora é relativo à fonte: cada survey tem seu próprio calendário
# (a PNAD Anual não tem 2000 nem 2010 e termina em 2015; a PNADC começa em 2012
# e não tem 2020–2021).
anos_disp <- lapply(
  split(df_dec$ano, df_dec$fonte),
  function(x) sort(unique(x))
)
nearest <- function(y, f) {
  a <- anos_disp[[f]]
  stopifnot(!is.null(a), length(a) > 0)
  a[which.min(abs(a - y))]
}

delta_dec <- GOVERNOS |>
  (\(govs) lapply(seq_len(nrow(govs)), function(i) {
    g <- govs[i, ]
    y0 <- nearest(g$ini, g$fonte)
    y1 <- nearest(g$fim, g$fonte)
    if (y0 == y1) {
      return(NULL)
    }
    t0 <- filter(df_dec, ano == y0, fonte == g$fonte) |>
      select(decil_num, t0 = prop, se0 = se)
    t1 <- filter(df_dec, ano == y1, fonte == g$fonte) |>
      select(decil_num, t1 = prop, se1 = se)
    left_join(t1, t0, by = "decil_num") |>
      mutate(
        delta     = t1 - t0,
        se_delta  = sqrt(se1^2 + se0^2),
        ci_low    = delta - 1.96 * se_delta,
        ci_high   = delta + 1.96 * se_delta,
        governo   = paste0(g$nome, " (", y0, "–", sprintf("%02d", y1 %% 100), ")"),
        gov_nome  = g$nome,
        fonte     = g$fonte,
        sinal     = classificar_sinal(delta, ci_low, ci_high)
      )
  }))() |>
  bind_rows()

# Painel de referência: único que atravessa as duas surveys, por necessidade.
y0_full <- nearest(1992, "PNAD Anual")
y1_full <- nearest(ANO_FIM, "PNAD Contínua")
t0_f <- filter(df_dec, ano == y0_full, fonte == "PNAD Anual") |>
  select(decil_num, t0 = prop, se0 = se)
t1_f <- filter(df_dec, ano == y1_full, fonte == "PNAD Contínua") |>
  select(decil_num, t1 = prop, se1 = se)
full_delta <- left_join(t1_f, t0_f, by = "decil_num") |>
  mutate(
    delta     = t1 - t0,
    se_delta  = sqrt(se1^2 + se0^2),
    ci_low    = delta - 1.96 * se_delta,
    ci_high   = delta + 1.96 * se_delta,
    governo   = paste0("Full period (", y0_full, "–", y1_full, ")"),
    gov_nome  = "Full period",
    fonte     = "PNAD Anual → PNAD Contínua",
    sinal     = classificar_sinal(delta, ci_low, ci_high)
  )

delta_dec <- bind_rows(delta_dec, full_delta) |>
  mutate(governo = factor(governo, levels = unique(governo)))

# Registro em console de qual survey sustentou cada painel — a checagem mais
# barata contra uma emenda silenciosa voltar a se instalar aqui.
cat("\nFonte por painel (splice por mandato):\n")
print(distinct(delta_dec, governo, fonte), n = 20)

all_x <- c(delta_dec$ci_low, delta_dec$ci_high)
xlim_shared <- c(
  floor(min(all_x, na.rm = TRUE) / 5) * 5 - 1,
  ceiling(max(all_x, na.rm = TRUE) / 5) * 5 + 2
)

# ==============================================================================
# 5. GRÁFICO — mesma grade 3x3, mesma tema_grade_densa(), mas geom_pointrange()
# (ponto = estimativa, linha = IC 95%) no lugar de geom_col()+geom_errorbar().
# `colour` no lugar de `fill` (pointrange não tem preenchimento de área) —
# usa scale_colour_delta(), o par exato de scale_fill_delta() para este caso
# (plot_theme.R, NEWS.md 2026-06-30).
# ==============================================================================
Y_DEC <- paste0("D", 1:10)

panel_theme <- theme(panel.grid.major.y = element_blank()) +
  tema_grade_densa()
scale_x_comp <- scale_x_continuous(
  breaks = scales::breaks_width(10),
  labels = function(x) paste0(ifelse(x > 0, "+", ""), x),
  expand = expansion(add = c(0.5, 0.5))
)
geoms_dotplot <- list(
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high),
    orientation = "y",
    size = 0.42, linewidth = 0.6, fatten = 2.4
  ),
  geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.55),
  geom_hline(
    yintercept = 5.5, colour = "#888888",
    linewidth = 0.3, linetype = "dotted"
  )
)

p_combined <- ggplot(delta_dec, aes(x = delta, y = factor(decil_num), colour = sinal)) +
  geoms_dotplot +
  facet_wrap(~governo, ncol = 3, scales = "fixed") +
  # `drop` fica no padrão (TRUE): um nível que não ocorre na figura sai da
  # legenda. Não é preferência estética — no ggplot2 4.x, forçar a chave de um
  # nível ausente (com `breaks` + `drop = FALSE`) desenha o rótulo SEM o ponto,
  # porque o glifo é montado a partir dos dados da camada, que estão vazios para
  # aquele nível. Verificado em 2026-08-07 na gêmea 042C_Tese_Dotplot_Acesso.R,
  # que não tem nenhuma perda significativa: a legenda saía com um "Loss" órfão,
  # sem marcador. O mapeamento de cor continua idêntico entre as duas figuras;
  # o que se adapta é só a lista de chaves, que passa a descrever a própria
  # figura.
  scale_colour_manual(
    values = c(
      "Gain" = COR_POSITIVO,
      "Loss" = COR_NEGATIVO,
      "Not significant" = COR_MEDIANA
    ),
    labels = ROTULOS_SINAL,
    name = NULL,
    guide = if (MOSTRAR_LEGENDA) "legend" else "none"
  ) +
  scale_x_comp +
  scale_y_discrete(labels = Y_DEC) +
  coord_cartesian(xlim = xlim_shared, clip = "off") +
  labs(
    x = "Total change in enrollment rate (pp over term)",
    y = "Income decile (D1 = poorest → D10 = richest)",
    title = if (MOSTRAR_TITULO) "Total change in higher education enrollment rate by income decile (ages 18–24)" else NULL,
    subtitle = if (MOSTRAR_TITULO) "Total pp change per term" else NULL,
    caption = if (MOSTRAR_FONTE) "Source: IBGE — PNAD (1992–2015) and PNAD Contínua (2015–2023)." else NULL
  ) +
  panel_theme +
  theme(
    legend.position = if (MOSTRAR_LEGENDA) "bottom" else "none",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(
      size = 18, face = "bold", family = THESIS_FONT,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 13, colour = "#444444", family = THESIS_FONT,
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      size = 10, colour = "#666666", family = THESIS_FONT,
      hjust = 0, margin = margin(t = 8)
    )
  )

salvar_grafico(p_combined,
  prefixo = "042C_Tese_Dotplot_Delta_Matriculados_Decil_Governo",
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm",
  formato = "pdf"
)

# ──────────────────────────────────────────────────────────────────────────
# Pacote em 6-images-tables/final/ — pasta própria (-dotplot) por
# rastreabilidade do gerador, mas esta é, a partir de 2026-06-30, a versão
# efetivamente citada no capítulo 0202 (#fig-delta-matriculados-decil-governo
# — o autor preferiu o dotplot à versão em barras após comparar as duas).
# Rode este bloco de novo sempre que finalizar uma revisão; depois, copie o
# trecho do .qmd gerado para o capítulo (mesmo fig-label da versão em barras,
# que permanece em 042C_Tese.R para referência/comparação futura).
# ──────────────────────────────────────────────────────────────────────────
finalizar_figura(
  plot = p_combined,
  fig_label = "delta-matriculados-decil-governo-dotplot",
  fig_cap = "Total change in higher education enrollment rate (ages 18–24) by income decile and presidential term, Brazil 1992–2023.",
  fonte = "IBGE — PNAD (1992–2015) and PNAD Contínua (2015–2023)",
  nota = "Each point is the pp change in the 18--24 active-enrollment rate from the first year of a presidential term to the next, with its 95\\% confidence interval. Points are grey where that interval contains zero, so the change is not distinguishable from none at the 5\\% level; blue and orange mark gains and losses whose intervals lie entirely on one side of zero. Each term panel draws on a single survey at both endpoints --- PNAD through 2015, PNAD Cont\\'{\\i}nua from 2015 --- so that no difference straddles the change of instrument; 2015, covered by both, therefore enters as PNAD in the Dilma I panel and as PNAD Cont\\'{\\i}nua in the next. Bottom-right panel: full 1992--2023 period, the one comparison no single survey spans.",
  apendice = "sec-fignote-delta-matriculados-decil-governo",
  script_path = here::here(
    "4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
    "02_validation", "042C_Tese_Dotplot.R"
  ),
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm"
)

cat("══════ Script 042C_Tese_Dotplot concluído ══════\n")
