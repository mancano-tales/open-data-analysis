# ==============================================================================
# SCRIPT: 042C_Tese.R
#
# Versão para dissertação do 042C — matrícula ativa (ens_sup_a) por decil,
# 18-24 anos, com as seguintes diferenças estruturais:
#
#   1. Sem título nem fonte (Quarto fornece fig-cap e note no .qmd)
#   2. Dilma II + Temer colapsados em um único bloco de 4 anos (2015→2019)
#   3. Termina em 2023 — Lula III não incluído
#   4. Layout 2×4 (8 painéis de governo + full period na linha de baixo)
#
# Razão para colapso Dilma II / Temer:
#   O impeachment é parte do mecanismo político analisado; individualmente
#   o mandato de Dilma II (1 ano) e Temer (3 anos) não permitem isolar
#   efeitos de política pública. O bloco unificado facilita comparação
#   visual com os demais mandatos de 4 anos.
#
# Razão para término em 2023:
#   Evitar truncamento assimétrico do governo Lula III (apenas 1-2 anos
#   disponíveis vs. 4 anos completos dos demais).
#
# PARA INSERÇÃO NO .qmd:
#   fig-cap: "Total change in higher education enrollment rate (ages 18–24)
#             by income decile and presidential term, Brazil 1992–2023.
#             Source: IBGE — PNAD (1992–2011) and PNAD Contínua (2012–2023)."
#   fig-label: fig-delta-matriculados-decil-governo
#   Note: Each bar = total pp change in share of 18–24-year-olds actively
#         enrolled, first year of term → first year of next term.
#         Pink = center-left (PT); blue = center-right (PSDB/PL);
#         lavender = Dilma II / Temer transition (impeachment). 95% CIs.
#
# VER TAMBÉM: 042C (versão completa 1992-2024, 10 governos)
# ==============================================================================

MOSTRAR_TITULO  <- FALSE  # FALSE = sem título/subtítulo (usar fig-cap do .qmd)
MOSTRAR_LEGENDA <- TRUE   # TRUE  = mostrar legenda Gain/Loss no gráfico
MOSTRAR_FONTE   <- FALSE  # FALSE = sem caption/source (usar note do .qmd)

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(ggplot2); library(scales)
  library(here); library(survey); library(ggh4x); library(patchwork)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 14))
options(scipen = 999, survey.lonely.psu = "adjust")

ANO_FIM <- 2023L   # último ano de dados usados (exclui Lula III)

BASE_DIR     <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
PARQUET_PNAD <- file.path(BASE_DIR, "output", "Microdados_Jovens_18_24_1992_2024.parquet")
CACHED_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2024_interview1.rds")
if (!file.exists(CACHED_PNADC))
  CACHED_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2024_interview1.rds")

# ==============================================================================
# 1. CARREGAR MICRODADOS
# ==============================================================================
cat("Carregando parquet...\n")
pnad_all <- arrow::read_parquet(PARQUET_PNAD) |>
  filter(!(fonte == "PNAD Anual" & ano >= 2012),
         !is.na(renda_dom_pcta), !is.na(peso)) |>
  mutate(decil_num = as.integer(decil)) |>
  filter(!is.na(decil_num), !is.na(ens_sup_a))

pnad_anual <- pnad_all |> filter(fonte == "PNAD Anual")
pnad_cont  <- pnad_all |> filter(fonte == "PNAD Contínua")

# ==============================================================================
# 2. PNAD ANUAL — SE via Kish (DEFF = 2.0)
# ==============================================================================
cat("Processando PNAD Anual (1992-2011)...\n")
df_dec_anual <- pnad_anual |>
  group_by(ano, decil_num) |>
  summarise(
    prop  = Hmisc::wtd.mean(ens_sup_a, weights = peso, na.rm = TRUE) * 100,
    n_eff = (sum(peso)^2 / sum(peso^2)) / 2.0,
    .groups = "drop"
  ) |>
  mutate(se = sqrt((prop / 100) * (1 - prop / 100) / pmax(n_eff, 1)) * 100)

# ==============================================================================
# 3. PNADC — SE via svydesign exato
# ==============================================================================
cat("Carregando cache PNADC...\n")
raw_pnadc <- readRDS(CACHED_PNADC)
names(raw_pnadc) <- tolower(names(raw_pnadc))
raw_pnadc <- raw_pnadc |>
  filter(!(as.integer(uf) %in% c(11L,12L,13L,14L,15L,16L) & as.integer(local) == 2L))
raw_pnadc_f <- raw_pnadc |>
  filter(!is.na(peso), peso > 0, !is.na(renda_dom_pcta), renda_dom_pcta > 0,
         idade >= 18, idade <= 24, !is.na(ens_sup_a))

pnad_cont_p <- pnad_cont |>
  group_by(ano, idade, peso, renda_dom_pcta, ens_sup_a) |>
  mutate(occ = row_number()) |> ungroup()
raw_pnadc_p <- raw_pnadc_f |>
  select(ano, idade, peso, renda_dom_pcta, ens_sup_a, estrato, upa) |>
  group_by(ano, idade, peso, renda_dom_pcta, ens_sup_a) |>
  mutate(occ = row_number()) |> ungroup()
pnadc_d <- inner_join(pnad_cont_p, raw_pnadc_p,
                      by = c("ano","idade","peso","renda_dom_pcta","ens_sup_a","occ")) |>
  select(-occ)

cat("Calculando SE com desenho complexo para PNADC...\n")
pnadc_decil <- lapply(sort(unique(pnadc_d$ano)), function(yr) {
  df_yr <- filter(pnadc_d, ano == yr)
  if (nrow(df_yr) == 0) return(NULL)
  des <- svydesign(ids = ~upa, strata = ~estrato, weights = ~peso,
                   data = df_yr, nest = TRUE)
  p   <- svyby(~ens_sup_a, ~decil_num, des, svymean, na.rm = TRUE)
  data.frame(ano = yr, decil_num = as.integer(as.character(p$decil_num)),
             prop = p$ens_sup_a * 100, se = p$se * 100)
})
df_dec <- bind_rows(df_dec_anual, bind_rows(pnadc_decil)) |> arrange(ano, decil_num)

# ==============================================================================
# 4. DELTAS POR GOVERNO — Dilma II + Temer colapsados; termina em 2023
# ==============================================================================
GOVERNOS <- data.frame(
  nome = c("Itamar", "FHC I", "FHC II", "Lula I", "Lula II",
           "Dilma I", "Dilma II/Temer", "Bolsonaro"),
  ini  = c(1992, 1995, 1999, 2003, 2007, 2011, 2015, 2019),
  fim  = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  stringsAsFactors = FALSE
)

anos_disp <- sort(unique(df_dec$ano))
nearest   <- function(y) anos_disp[which.min(abs(anos_disp - y))]

delta_dec <- GOVERNOS |>
  (\(govs) lapply(seq_len(nrow(govs)), function(i) {
    g  <- govs[i, ]
    y0 <- nearest(g$ini); y1 <- nearest(g$fim)
    if (y0 == y1) return(NULL)
    n  <- y1 - y0
    t0 <- filter(df_dec, ano == y0) |> select(decil_num, t0 = prop, se0 = se)
    t1 <- filter(df_dec, ano == y1) |> select(decil_num, t1 = prop, se1 = se)
    left_join(t1, t0, by = "decil_num") |>
      mutate(
        delta     = t1 - t0,
        se_delta  = sqrt(se1^2 + se0^2),
        ci_low    = delta - 1.96 * se_delta,
        ci_high   = delta + 1.96 * se_delta,
        # Uma linha só (era 2 linhas + "N yrs"), ano final com 2 dígitos —
        # libera altura E largura da caixa do cabeçalho, reinvestida em fonte
        # maior nos eixos (NEWS.md 2026-06-30). "Dilma II/Temer" era o nome
        # mais largo e estava vazando pro painel vizinho antes deste ajuste.
        governo   = paste0(g$nome, " (", y0, "–", sprintf("%02d", y1 %% 100), ")"),
        gov_nome  = g$nome,
        positivo  = delta >= 0
      )
  }))() |> bind_rows()

# ==============================================================================
# 4b. PAINEL DE REFERÊNCIA: PERÍODO COMPLETO 1992–2023
# ==============================================================================
y0_full <- nearest(1992); y1_full <- nearest(ANO_FIM)
n_full  <- y1_full - y0_full
t0_f <- filter(df_dec, ano == y0_full) |> select(decil_num, t0 = prop, se0 = se)
t1_f <- filter(df_dec, ano == y1_full) |> select(decil_num, t1 = prop, se1 = se)
full_delta <- left_join(t1_f, t0_f, by = "decil_num") |>
  mutate(
    delta     = t1 - t0,
    se_delta  = sqrt(se1^2 + se0^2),
    ci_low    = delta - 1.96 * se_delta,
    ci_high   = delta + 1.96 * se_delta,
    governo   = paste0("Full period (", y0_full, "–", y1_full, ")"),
    gov_nome  = "Full period",
    positivo  = delta >= 0
  )

delta_dec <- bind_rows(delta_dec, full_delta) |>
  mutate(governo = factor(governo, levels = unique(governo)))

# ==============================================================================
# 5. (Removido 2026-06-30: caixa colorida por coalizão nos cabeçalhos dos
# painéis — o autor decidiu que o nome do governo + anos basta, sem fundo
# colorido. Ver histórico do git para a versão anterior com ORIENTACAO_GOV /
# make_strip() / ggh4x::strip_themed() caso essa codificação visual volte a
# ser necessária.)
# ==============================================================================
# `label_x` (usado para posicionar o rótulo de texto que já não existe mais
# em nenhum painel) inflava o eixo até +40 mesmo o maior valor real (com IC)
# sendo ~+25 (FHC II) — observação do autor direto no .qmd, 2026-06-30.
# Usar ci_high (não label_x) reflete o intervalo de dados real.
all_x       <- c(delta_dec$ci_low, delta_dec$ci_high)
xlim_shared <- c(floor(min(all_x, na.rm=TRUE)/5)*5 - 1,
                 ceiling(max(all_x, na.rm=TRUE)/5)*5 + 2)

# ==============================================================================
# 6. GRÁFICO — grade única 3x3 (8 governos + "Full period" no 9º painel)
# Decisão do autor (2026-06-30): abandonar o desenho anterior (8 painéis em
# 2x4 + painel "Full period" em largura total via patchwork) por um grid
# único de 9 painéis iguais em 3 colunas. Cada painel ganha ~33% mais
# largura (24cm/3 = 8cm vs 24cm/4 = 6cm antes), o que resolveu o vazamento
# do cabeçalho "Dilma II/Temer". Efeito colateral aceito: o painel "Full
# period" perde o espaço extra que tinha (era largura total); por isso ele
# passa a usar o mesmo tratamento sem rótulo de valor que os outros 8 já
# usavam, para manter consistência visual. A ordem cronológica do factor
# `governo` já posiciona os governos na ordem certa por linha (3 por linha)
# sem precisar reordenar nada.
# ==============================================================================
# Rótulos de uma linha só: com o canvas redimensionado para o tamanho real de
# exibição (ver NEWS.md 2026-06-30), labels de 2 linhas ("D10\n(richest)")
# colidem com a categoria vizinha por falta de altura por decil. A indicação
# poorest/median/richest passa a ser dita UMA VEZ no título do eixo (labs)
# em vez de repetida em cada tick.
Y_DEC <- paste0("D", 1:10)

# tema_grade_densa() (plot_theme.R) codifica a receita desta figura para
# reuso em outras grades densas — ver WRITING-STYLE.md §13.6.
panel_theme <- theme(panel.grid.major.y = element_blank()) +
  tema_grade_densa()
scale_x_comp <- scale_x_continuous(
  breaks = scales::breaks_width(10),
  labels = function(x) paste0(ifelse(x > 0, "+", ""), x),
  expand = expansion(add = c(0.5, 0.5))
)
# Sem rótulo de valor em nenhum painel (inclusive "Full period", que antes
# tinha largura total e espaço de sobra — agora é só mais um painel do grid
# 3x3). Posição (extremidade da barra) já é o canal de leitura mais preciso
# (Cleveland & McGill 1984); o valor exato fica disponível na tabela/nota.
geoms_sem_label <- list(
  geom_col(width = 0.72, show.legend = FALSE),
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                orientation = "y", width = 0.35,
                colour = "#444444", linewidth = 0.45),
  geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.55),
  geom_hline(yintercept = 5.5, colour = "#888888",
             linewidth = 0.3, linetype = "dotted")
)

# Grade única 3x3 — os 9 níveis do factor `governo` (8 governos + "Full
# period", já na ordem cronológica) preenchem o grid em ordem de leitura.
p_combined <- ggplot(delta_dec, aes(x = delta, y = factor(decil_num), fill = positivo)) +
  geoms_sem_label +
  facet_wrap(~governo, ncol = 3, scales = "fixed") +
  scale_fill_delta(labels = c("TRUE" = "Gain (pp total)", "FALSE" = "Loss (pp total)"),
                    guide  = if (MOSTRAR_LEGENDA) "legend" else "none") +
  scale_x_comp +
  scale_y_discrete(labels = Y_DEC) +
  coord_cartesian(xlim = xlim_shared, clip = "off") +
  labs(x = "Total change in enrollment rate (pp over term)",
       y = "Income decile (D1 = poorest → D10 = richest)",
       title    = if (MOSTRAR_TITULO) "Total change in higher education enrollment rate by income decile (ages 18–24)" else NULL,
       subtitle = if (MOSTRAR_TITULO) "Total pp change per term" else NULL,
       caption  = if (MOSTRAR_FONTE) "Source: IBGE — PNAD (1992–2011) and PNAD Contínua (2012–2023)." else NULL) +
  panel_theme +
  theme(legend.position = if (MOSTRAR_LEGENDA) "bottom" else "none",
        axis.text.y   = element_text(size = 10),
        plot.title    = element_text(size = 18, face = "bold", family = THESIS_FONT,
                                     margin = margin(b = 4)),
        plot.subtitle = element_text(size = 13, colour = "#444444", family = THESIS_FONT,
                                     margin = margin(b = 8)),
        plot.caption  = element_text(size = 10, colour = "#666666", family = THESIS_FONT,
                                     hjust = 0, margin = margin(t = 8)))

# Canvas = LARGURA_PAISAGEM/ALTURA_PAISAGEM (plot_theme.R): tamanho FÍSICO
# REAL de exibição no PDF — esta figura é citada como página paisagem cheia
# (\begin{landscape} no .qmd, ver Mancano2026-0202-...qmd). ANTES: 44x24cm
# salvos mas exibidos a 6,27in (coluna de texto normal) = encolhimento de
# ~2,8x = texto ilegível. Ver WRITING-STYLE.md §13.6 e NEWS.md 2026-06-30.
salvar_grafico(p_combined, prefixo = "042C_Tese_Delta_Matriculados_Decil_Governo",
               largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm",
               formato = "pdf")

# ──────────────────────────────────────────────────────────────────────────
# PROMOÇÃO MANUAL PARA O TEXTO — pacote PDF + .R + .qmd em 6-images-tables/final/
# Esta é a figura efetivamente referenciada em
# 3-texts/0202-MA-Distinguishing-Policy-Impacts/Mancano2026-0202-MA-Distinguishing-Policy-Impacts.qmd
# via #fig-delta-matriculados-decil-governo. Rode este bloco de novo sempre
# que finalizar uma revisão desta figura. Depois, copie o trecho do .qmd
# gerado para o capítulo (substituindo a versão anterior) — ver o .qmd
# resultante para o caminho exato do PDF com timestamp.
# ──────────────────────────────────────────────────────────────────────────
finalizar_figura(
  plot        = p_combined,
  fig_label   = "delta-matriculados-decil-governo",
  fig_cap     = "Total change in higher education enrollment rate (ages 18–24) by income decile and presidential term, Brazil 1992–2023.",
  fonte       = "IBGE — PNAD (1992–2011) and PNAD Contínua (2012–2023)",
  nota        = "Each bar shows the total percentage-point change in the share of 18–24-year-olds actively enrolled in tertiary education, from the first year of each presidential term to the first year of the next (blue = gain, orange = loss). Panels are ordered chronologically left to right, top to bottom; the bottom-right panel shows the full 1992–2023 period as reference. Error bars are 95% confidence intervals computed via Kish design-effect approximation (PNAD Anual, 1992–2011) and exact complex-survey estimation via `survey::svydesign()` (PNAD Contínua, 2012–2023). For full harmonization methodology linking the two survey instruments, see the Technical Appendix [@sec-decisions] and @sec-p2-datamethods above.",
  script_path = here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data", "R",
                            "02_validation", "042C_Tese.R"),
  largura = LARGURA_PAISAGEM, altura = ALTURA_PAISAGEM, unidades = "cm"
)

cat("══════ Script 042C_Tese concluído ══════\n")
cat("Toggles: MOSTRAR_TITULO =", MOSTRAR_TITULO,
    "| MOSTRAR_LEGENDA =", MOSTRAR_LEGENDA,
    "| MOSTRAR_FONTE =", MOSTRAR_FONTE, "\n")
cat("Governo final: Bolsonaro (2019 → 2023) | Full period: 1992 → 2023\n")
