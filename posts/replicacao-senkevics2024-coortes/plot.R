# ==============================================================================
# 085_Fig_Coortes_2012_2019_Apendice.R
# ODA: portado de 6-images-tables/final/replicacao-senkevics2024-coortes/2026-09-14_2215_085_Fig_Coortes_2012_2019_Apendice.R
#      em 2026-09-20; le as matrizes redistribuidas em data/ (ou a reextracao em data-raw/).
# ==============================================================================
# A Figura 3 replicada (055) estendida a DUAS coortes — 2012 e 2019 — no mesmo
# vocabulario visual da figura promovida: amarelo = 1o decil de renda, azul =
# 10o decil; claro = 2012, escuro = 2019. Destino nas colunas, bolha = peso da
# celula, como nos autores.
#
# Por que so duas coortes e nao quatro (como o 080): a cor volta a carregar o
# estrato de renda, que e o que o leitor do apendice ja aprendeu a ler na 055.
# A coorte fica na luminosidade, e quatro tons de amarelo nao sao
# distinguiveis — dois sao. As coortes intermediarias (2014, 2017) ficam na
# figura de hiato (070), que as carrega sem esse custo.
#
# Decisoes que vieram do diagnostico de 2026-09-14 (scratchpad diag_080*.R):
#   - SEM faixas de IC: com celulas >= 749, o IC de 95% e mais estreito que
#     +-3,3 pp, quase a espessura da linha; numa figura de bolhas viraria uma
#     terceira marca. A fignote declara a largura maxima. A incerteza relevante
#     e nao amostral (selecao para o ENEM, regra D3) e faixa nenhuma a cobre.
#   - HISTORICO: ate 2026-09-14 a celula 10D x desempenho 1 era desenhada
#     vazada, porque violava a ordem monotona em renda. A verificacao contra o
#     SEDAP+ (170, V4) mostrou que 83% dela na edicao tardia era renda NULL
#     codificada como 10D pelo ELSE do CASE. Corrigido na fonte (desvio D7 em
#     015_Especificacao.R; matrizes reextraidas), a celula voltou a curva e o
#     simbolo especial saiu. Rodape explicativo dentro da imagem foi rejeitado
#     pelo autor de todo modo ("tema de apendice ou fignote").
#   - Linhas finas ligando os pontos de uma serie: auxilio Gestalt de
#     agrupamento (4 series por painel), nao canal de dado. Mesmo argumento do 080.
#
# PROMOVER = FALSE grava em graphs/ (rascunho); TRUE grava o trio em final/.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
})

PROMOVER <- as.logical(Sys.getenv("PROMOVER", "TRUE"))

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

# ODA: as matrizes agregadas (300 celulas renda x desempenho x destino por
# coorte) sao redistribuidas em data/ — a figura reproduz sem credencial.
# Quem tiver SEDAP_TOKEN e reextrair com shared-pipeline/sedap/senkevics/
# (COORTE_ANO=2012 e 2019) tem a reextracao lida de preferencia, de data-raw/.
DIR_DATA <- here::here("posts", "replicacao-senkevics2024-coortes", "data")
DIR_RAW  <- here::here("data-raw", "sedap", "senkevics")
COORTES <- c(2012, 2019)
NIVEIS_D <- paste0(1:10, "D")

# col_types explicito: "1D"/"10D" viram numero se o readr adivinhar (D e
# marcador de expoente) — ver 040/050/080.
matriz <- map_dfr(COORTES, function(ano) {
  f_raw <- file.path(DIR_RAW, sprintf("coorte_%d", ano), "030_matriz_final.csv")
  f_pkg <- file.path(DIR_DATA, sprintf("coorte_%d_030_matriz_final.csv", ano))
  f <- if (file.exists(f_raw)) f_raw else f_pkg
  if (!file.exists(f)) stop("Falta a matriz da coorte ", ano, " (esperada em ", f_pkg, ")", call. = FALSE)
  cat("  coorte", ano, "<-", f, "\n")
  read_csv(f, col_types = cols(
    income_D = col_character(), performance = col_character(),
    choice = col_character(), n = col_double()
  )) %>% mutate(coorte = ano)
})

dados <- matriz %>%
  filter(income_D %in% c("1D", "10D")) %>%
  group_by(coorte, income_D, performance) %>%
  mutate(n_cel = sum(n)) %>%
  ungroup() %>%
  group_by(coorte, income_D) %>%
  mutate(I = n_cel / sum(n_cel) * 3) %>% # /3 porque n_cel repete nos 3 destinos
  ungroup() %>%
  mutate(
    P = n / n_cel,
    performance_num = match(performance, NIVEIS_D),
    destination = factor(
      recode(choice,
        "No Access" = "No access", "Public" = "Public sector", "Private" = "Private sector"
      ),
      levels = c("No access", "Public sector", "Private sector")
    ),
    serie = factor(
      paste(income_D, coorte),
      levels = c("1D 2012", "1D 2019", "10D 2012", "10D 2019"),
      labels = c(
        "1st income decile, 2012", "1st income decile, 2019",
        "10th income decile, 2012", "10th income decile, 2019"
      )
    )
  )

# I soma 1 dentro de cada coorte x decil de renda (uma vez, nao tres).
stopifnot(all(abs(
  dados %>%
    filter(destination == "No access") %>%
    group_by(coorte, income_D) %>%
    summarise(s = sum(I), .groups = "drop") %>%
    pull(s)
) - 1 < 1e-8))
if (nrow(dados) != 120L) stop("Esperava 120 linhas (2 coortes x 2 decis x 10 x 3), obtive ", nrow(dados))

# Okabe-Ito da 055: amarelo (1o decil) e azul (10o decil). Claro = 2012,
# escuro = 2019. lighten() em HCL preserva o matiz, ao contrario de alpha,
# que mistura com o fundo e some sobre a grade.
AMARELO <- "#E69F00"
AZUL <- "#0072B2"
CORES <- c(
  "1st income decile, 2012"  = colorspace::lighten(AMARELO, 0.45),
  "1st income decile, 2019"  = AMARELO,
  "10th income decile, 2012" = colorspace::lighten(AZUL, 0.45),
  "10th income decile, 2019" = AZUL
)

fig <- ggplot(dados, aes(x = performance_num, y = P, colour = serie, group = serie)) +
  geom_line(linewidth = 0.3, alpha = 0.7) +
  geom_point(aes(size = I), alpha = 0.85) +
  facet_wrap(~destination) +
  scale_size_area(
    max_size = 7, breaks = c(.05, .15, .30),
    labels = scales::percent_format(accuracy = 1), name = "Cell weight"
  ) +
  scale_colour_manual(values = CORES, name = NULL) +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.8, 0.2), minor_breaks = NULL
  ) +
  # coord_cartesian recorta a vista SEM descartar observacao — ao contrario de
  # limits=, que remove pontos silenciosamente.
  coord_cartesian(ylim = c(0, 0.9)) +
  labs(
    x = "ENEM performance decile",
    y = "Predicted probability"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 2, override.aes = list(size = 2.4, linewidth = 0)),
    size = guide_legend(order = 2)
  ) +
  theme(
    legend.position = "bottom", legend.box = "vertical",
    legend.margin = margin(t = 0, b = 0)
  )

DIR_RASCUNHO <- here::here("output", "graphs")

if (!PROMOVER) {
  salvar_grafico(fig,
    prefixo = "085_Fig_Coortes_2012_2019_Apendice",
    largura = LARGURA_TEXTO, altura = 4.4, dir = DIR_RASCUNHO, formato = "png"
  )
} else {
  finalizar_figura(
    plot = fig,
    fig_label = "replicacao-senkevics2024-coortes",
    fig_cap = paste(
      "Predicted probability of no access, public-sector and private-sector entry",
      "by ENEM performance decile, for the poorest and richest income deciles,",
      "Brazil, secondary-school cohorts of 2012 and 2019."
    ),
    nota = paste(
      "Each circle is the predicted probability that a student in the final year",
      "of secondary school in 2012 (lighter shade) or 2019 (darker shade) had,",
      "within five years, entered no tertiary institution, a public one or a",
      "private one, by decile of ENEM performance (horizontal axis) and family",
      "income decile (colour), from a saturated multinomial logit fitted",
      "separately to each cohort. Deciles are computed within each cohort among",
      "students who sat the ENEM. Circle area is the share of that income decile",
      "in that performance decile, so circles of one colour and shade sum to one",
      "within each panel. Students with no income declaration are excluded",
      "(0.03\\% of the 2012 sample, 0.28\\% of 2019). The two cohorts are not",
      "equally covered:",
      "the students observed in the ENEM are 45\\% of the 2012 school cohort and",
      "35\\% of the 2019 cohort, so part of the change may reflect who sat the",
      "examination rather than who gained access. Every 95\\% confidence interval",
      "is narrower than $\\pm$3.3 percentage points and is not drawn. Departures",
      "from the original specification and the coverage diagnostics are in"
    ),
    fonte = paste(
      "INEP --- Censo Escolar, ENEM and Censo da Educação Superior, linked by",
      "masked CPF via SEDAP+; specification from Senkevics et al. (2024)"
    ),
    apendice = "sec-fignote-replicacao-senkevics2024-coortes",
    script_path = here::here("posts", "replicacao-senkevics2024-coortes", "plot.R"),
    largura = LARGURA_TEXTO, altura = 4.4
  )
}

cat("\n[OK] 085 concluido (PROMOVER =", PROMOVER, ").\n")
