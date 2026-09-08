# ==============================================================================
# SCRIPT: 042C_Tese_Dotplot_Acesso.R
#
# Versão "acesso" de 042C_Tese_Dotplot.R — MESMOS dados, MESMO grid 3x3,
# MESMA estética (dotplot de Cleveland), trocando a variável de interesse:
#
#   042C_Tese_Dotplot.R       -> ens_sup_a (matriculado AGORA, 18-24 anos)
#   042C_Tese_Dotplot_Acesso.R -> ens_sup    (ingressou no superior ALGUMA
#                                              VEZ, 18-24 anos — inclui
#                                              matriculados, graduados e
#                                              quem já saiu sem concluir)
#
# Pedido do autor (2026-06-30): "reaproveitar o mesmo script e mudar a
# variável dos matriculados para todos aqueles de 18-24 anos que acessaram a
# educação superior alguma vez".
#
# IMPORTANTE — o que NÃO foi trocado: a chave de casamento entre o parquet
# harmonizado (pnad_cont) e o cache raw da PNADC (raw_pnadc, usado só para
# recuperar estrato/upa para o desenho amostral) continua usando ens_sup_a,
# não ens_sup. Essa chave serve apenas para deduplicar/re-identificar linhas
# entre os dois datasets (não há ID de pessoa compartilhado) — trocá-la para
# ens_sup arriscaria alterar o comportamento da junção sem necessidade, já
# que ens_sup já está disponível em pnad_cont (vem do parquet harmonizado)
# e atravessa a junção intacto. A variável efetivamente usada na análise
# (média ponderada, svyby) é ens_sup em todo o resto do script.
#
# ATUALIZAÇÃO 2026-08-07 — duas mudanças, ambas para reencostar esta figura na
# gêmea do capítulo, de onde ela havia se descolado:
#   (1) cinza para pontos cujo IC de 95% atravessa o zero (convenção adotada em
#       042C_Tese_Dotplot.R em 2026-08-03 e não replicada aqui na ocasião);
#   (2) splice PNAD/PNADC por mandato, para que nenhum delta atravesse as duas
#       surveys (ver o bloco GOVERNOS).
#
# PARA INSERÇÃO NO .qmd:
#   fig-cap: "Total change in tertiary education access rate (ages 18–24,
#             ever enrolled) by income decile and presidential term, Brazil
#             1992–2023. Source: IBGE — PNAD (1992–2015) and PNAD Contínua
#             (2015–2023)."
#   fig-label: fig-delta-acesso-decil-governo
#
# VER TAMBÉM: 042C_Tese_Dotplot.R (mesma figura para matrícula ativa, ens_sup_a)
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
  # (Ate 2026-08-07 esta linha trazia o caminho absoluto da maquina do autor,
  # que so funcionava nela e travava o pre-commit ao ser copiada para final/.)
  CACHED_PNADC <- file.path(
    dirname(here::here()), "5-data",
    "pnadc_consolidado_2012_2024_interview1.rds"
  )
}

# ==============================================================================
# 1-4b. CARREGAR E PREPARAR DADOS — idêntico a 042C_Tese_Dotplot.R, exceto a
# variável de interesse (ens_sup no lugar de ens_sup_a). A chave de junção
# PNAD x PNADC permanece ens_sup_a (ver nota no cabeçalho).
# ==============================================================================
cat("Carregando parquet...\n")
# NOTA (2026-08-07): o filtro `!(fonte == "PNAD Anual" & ano >= 2012)` foi
# REMOVIDO de propósito — ver o bloco GOVERNOS (splice por mandato). O overlap
# 2012–2015 fica preservado e `df_dec` passa a ter duas linhas por (ano, decil)
# nesses anos, de modo que toda leitura dele precisa filtrar também por `fonte`.
pnad_all <- arrow::read_parquet(PARQUET_PNAD) |>
  filter(!is.na(renda_dom_pcta), !is.na(peso)) |>
  mutate(decil_num = as.integer(decil)) |>
  filter(!is.na(decil_num), !is.na(ens_sup))

pnad_anual <- pnad_all |> filter(fonte == "PNAD Anual")
pnad_cont <- pnad_all |> filter(fonte == "PNAD Contínua")

cat("Processando PNAD Anual (1992-2015)...\n")
df_dec_anual <- pnad_anual |>
  group_by(ano, decil_num) |>
  summarise(
    prop = Hmisc::wtd.mean(ens_sup, weights = peso, na.rm = TRUE) * 100,
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

# Chave de junção: ens_sup_a (não trocar — ver nota no cabeçalho). ens_sup
# vem junto em pnad_cont_p (já está em pnad_cont, herdado do parquet
# harmonizado) e atravessa a junção sem necessidade de selecioná-lo aqui.
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
  p <- svyby(~ens_sup, ~decil_num, des, svymean, na.rm = TRUE)
  data.frame(
    ano = yr, decil_num = as.integer(as.character(p$decil_num)),
    prop = p$ens_sup * 100, se = p$se * 100,
    fonte = "PNAD Contínua"
  )
})
df_dec <- bind_rows(df_dec_anual, bind_rows(pnadc_decil)) |>
  arrange(ano, fonte, decil_num)

# ------------------------------------------------------------------------------
# Classificação do sinal para a cor do ponto — portada de 042C_Tese_Dotplot.R
# em 2026-08-07, para que as duas figuras gêmeas digam a mesma coisa com a mesma
# cor (esta ficou para trás quando a do capítulo mudou, em 2026-08-03).
#
# Antes, a cor codificava apenas o sinal do delta, o que dava a MESMA saliência
# visual a uma variação de +15pp e a uma de +0,5pp cujo IC de 95% atravessa o
# zero. Agora o ponto fica CINZA quando o IC contém o zero — isto é, quando a
# mudança não é estatisticamente distinguível de zero ao nível de 5%.
#
# O teste é o do IC do próprio delta, com se_delta = sqrt(se1^2 + se0^2), que
# trata os dois anos como independentes: conservador contra a PNAD Anual
# (Kish/DEFF 2.0) e cego à correlação entre ondas do painel da PNADC, de modo
# que um ponto que sai colorido sai colorido com folga.
# ------------------------------------------------------------------------------
NIVEIS_SINAL <- c("Gain", "Loss", "Not significant")

# Rótulos NOMEADOS, não posicionais — esta figura não tem nenhuma perda
# significativa, então o nível "Loss" é descartado da legenda (ver a nota em
# `scale_colour_manual`) e um vetor posicional rotularia a chave errada.
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
# Splice PNAD/PNADC POR MANDATO (autor, 2026-08-07) — mesma regra do
# 042C_Tese_Dotplot.R, onde ela está justificada por extenso. Em resumo: cada
# ponto é uma DIFERENÇA, e um delta entre surveys diferentes soma à mudança real
# a descontinuidade de instrumento (desenho amostral, e a unidade da renda per
# capita — familiar na PNAD Anual, domiciliar na PNADC, decisão D21). Como
# 2012–2015 existe nas duas fontes, a fonte é escolhida por mandato e nenhum
# painel de mandato atravessa a emenda. Custo: 2015 entra como PNAD num painel e
# como PNADC no seguinte. O painel "Full period" segue cross-survey por
# necessidade.
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

cat("\nFonte por painel (splice por mandato):\n")
print(distinct(delta_dec, governo, fonte), n = 20)

all_x <- c(delta_dec$ci_low, delta_dec$ci_high)
xlim_shared <- c(
  floor(min(all_x, na.rm = TRUE) / 5) * 5 - 1,
  ceiling(max(all_x, na.rm = TRUE) / 5) * 5 + 2
)

# ==============================================================================
# 5. GRÁFICO — idêntico a 042C_Tese_Dotplot.R (grade 3x3, tema_grade_densa(),
# geom_pointrange()); só os textos (eixo, fig-cap, nota) refletem "acesso"
# em vez de "matrícula".
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
  # `drop` no padrão (TRUE) — ver a nota equivalente em 042C_Tese_Dotplot.R.
  # No ggplot2 4.x, forçar a chave de um nível ausente desenha o rótulo sem o
  # marcador; nesta figura, que não tem nenhuma perda significativa, isso
  # produzia um "Loss" órfão na legenda.
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
    x = "Total change in access rate (pp over term)",
    y = "Income decile (D1 = poorest → D10 = richest)",
    title = if (MOSTRAR_TITULO) "Total change in tertiary education access rate by income decile (ages 18–24)" else NULL,
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
  prefixo = "042C_Tese_Dotplot_Delta_Acesso_Decil_Governo",
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm",
  formato = "pdf"
)

# ──────────────────────────────────────────────────────────────────────────
# Pacote em 6-images-tables/final/delta-acesso-decil-governo/ — citado no
# apêndice de dados (§sec-access-alt, especificação alternativa), como
# contraponto à versão "matrícula ativa" do 042C_Tese_Dotplot.R, citada em
# 0202. As duas precisam evoluir juntas: qualquer mudança de convenção visual
# ou de estimador feita lá tem de vir para cá na mesma rodada.
# ──────────────────────────────────────────────────────────────────────────
finalizar_figura(
  plot = p_combined,
  fig_label = "delta-acesso-decil-governo",
  fig_cap = "Total change in tertiary education access rate (ages 18–24, ever enrolled) by income decile and presidential term, Brazil 1992–2023.",
  fonte = "IBGE — PNAD (1992–2015) and PNAD Contínua (2015–2023)",
  nota = "Each point is the pp change in the share of 18--24-year-olds who had ever accessed tertiary education (currently enrolled, graduated, or withdrawn), from the first year of a presidential term to the next, with its 95\\% confidence interval. Points are grey where that interval contains zero, so the change is not distinguishable from none at the 5\\% level; blue and orange mark gains and losses whose intervals lie entirely on one side of zero. Each term panel draws on a single survey at both endpoints --- PNAD through 2015, PNAD Cont\\'{\\i}nua from 2015 --- so that no difference straddles the change of instrument; 2015, covered by both, therefore enters as PNAD in the Dilma I panel and as PNAD Cont\\'{\\i}nua in the next. Bottom-right panel: full 1992--2023 period, the one comparison no single survey spans.",
  apendice = "sec-fignote-delta-matriculados-decil-governo",
  script_path = here::here(
    "4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
    "02_validation", "042C_Tese_Dotplot_Acesso.R"
  ),
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm"
)

cat("══════ Script 042C_Tese_Dotplot_Acesso concluído ══════\n")
