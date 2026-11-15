# ==============================================================================
# 085_Fig_Coortes_2012_2019_Apendice.R
# ODA: portado de 4-DA-Code/2026-08_Replicacao_Senkevics2024/
#      085_Fig_Coortes_2012_2019_Apendice.R em 2026-10-20.
# ==============================================================================
# A Figura 3 de Senkevics et al. (2024) replicada para DUAS coortes — 2012 e
# 2019 — com amarelo = 1o decil de renda, azul = 10o decil; claro = 2012,
# escuro = 2019. Destino nas colunas, bolha = peso da celula, como nos autores.
# E a UNICA figura promovida da replicacao: a versao so-2012 (055) foi
# despromovida em 2026-09-15 porque esta a contem (a serie clara). A versao
# em portugues de uma coorte segue no 050, como rascunho.
#
# Por que so duas coortes e nao quatro (como o 080): a cor carrega o estrato
# de renda, como na figura dos autores, que e o que o leitor ja sabe ler.
# A coorte fica na luminosidade, e quatro tons de amarelo nao sao
# distinguiveis — dois sao. As coortes intermediarias (2014, 2017) ficam na
# figura de hiato (070), que as carrega sem esse custo.
#
# Decisoes que vieram do diagnostico de 2026-09-14 (scratchpad diag_080*.R):
#   - SEM faixas de IC: com celulas >= 1.219, o IC de 95% e mais estreito que
#     +-2,7 pp, quase a espessura da linha; numa figura de bolhas viraria uma
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
# FIG_IDIOMA=pt ou en; o post publicado usa en.
# Cobertura e IC da nota sao calculados dos CSV; totais D7 vêm da tese.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
})

IDIOMA <- Sys.getenv("FIG_IDIOMA", "en")
stopifnot(IDIOMA %in% c("pt", "en"))

TXT <- list(
  pt = list(
    destinos = c("Sem acesso", "Setor público", "Setor privado"),
    series = c(
      "1º decil de renda, 2012", "1º decil de renda, 2019",
      "10º decil de renda, 2012", "10º decil de renda, 2019"
    ),
    peso = "Peso da célula", x = "Decil de desempenho no ENEM",
    y = "Probabilidade prevista", pct = function(x) scales::percent(x, accuracy = 1, decimal.mark = ",")
  ),
  en = list(
    destinos = c("No access", "Public sector", "Private sector"),
    series = c(
      "1st income decile, 2012", "1st income decile, 2019",
      "10th income decile, 2012", "10th income decile, 2019"
    ),
    peso = "Cell weight", x = "ENEM performance decile",
    y = "Predicted probability", pct = function(x) scales::percent(x, accuracy = 1)
  )
)[[IDIOMA]]

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

RAIZ <- here::here("posts", "replicacao-senkevics2024-coortes", "data")
COORTES <- c(2012, 2019)
NIVEIS_D <- paste0(1:10, "D")

# col_types explicito: "1D"/"10D" viram numero se o readr adivinhar (D e
# marcador de expoente) — ver 040/050/080.
matriz <- map_dfr(COORTES, function(ano) {
  f <- file.path(RAIZ, sprintf("coorte_%d_030_matriz_final.csv", ano))
  if (!file.exists(f)) stop("Falta a matriz da coorte ", ano)
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
    destination = factor(choice,
      levels = c("No Access", "Public", "Private"), labels = TXT$destinos
    ),
    serie = factor(
      paste(income_D, coorte),
      levels = c("1D 2012", "1D 2019", "10D 2012", "10D 2019"),
      labels = TXT$series
    )
  )
stopifnot(!anyNA(dados$destination), !anyNA(dados$serie))

# I soma 1 dentro de cada coorte x decil de renda (uma vez, nao tres).
stopifnot(all(abs(
  dados %>%
    filter(choice == "No Access") %>%
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
CORES <- setNames(
  c(colorspace::lighten(AMARELO, 0.45), AMARELO, colorspace::lighten(AZUL, 0.45), AZUL),
  TXT$series
)

fig <- ggplot(dados, aes(x = performance_num, y = P, colour = serie, group = serie)) +
  geom_line(linewidth = 0.3, alpha = 0.7) +
  geom_point(aes(size = I), alpha = 0.85) +
  facet_wrap(~destination) +
  scale_size_area(
    max_size = 7, breaks = c(.05, .15, .30),
    labels = TXT$pct, name = TXT$peso
  ) +
  scale_colour_manual(values = CORES, name = NULL) +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous(
    labels = TXT$pct,
    breaks = seq(0, 0.8, 0.2), minor_breaks = NULL
  ) +
  # coord_cartesian recorta a vista SEM descartar observacao — ao contrario de
  # limits=, que remove pontos silenciosamente.
  coord_cartesian(ylim = c(0, 0.9)) +
  labs(x = TXT$x, y = TXT$y) +
  guides(
    colour = guide_legend(order = 1, nrow = 2, override.aes = list(size = 2.4, linewidth = 0)),
    size = guide_legend(order = 2)
  ) +
  theme(
    legend.position = "bottom", legend.box = "vertical",
    legend.margin = margin(t = 0, b = 0)
  )

# ------------------------------------------------------------------------------
# Numeros da nota, calculados dos CSV (antes eram digitados e envelheceram
# quando as matrizes foram reextraidas em 2026-10-20)
# ------------------------------------------------------------------------------
num_coorte <- map_dfr(COORTES, function(ano) {
  tam <- read_csv(file.path(RAIZ, sprintf("coorte_%d_010_coorte_tamanho.csv", ano)), show_col_types = FALSE)
  # Totais D7 conferidos na tese em 2026-10-20; os CSVs de exclusao nao
  # acompanham o PR da tese. Preservar estes totais ate a proxima extracao.
  excluidos_d7 <- c(`2012` = 364, `2019` = 2579)
  n_amostra <- sum(matriz$n[matriz$coorte == ano])
  tibble(
    coorte = ano,
    cobertura = 100 * n_amostra / tam$N_MATRICULAS_SERIE_FINAL,
    d7 = 100 * excluidos_d7[as.character(ano)] / (excluidos_d7[as.character(ano)] + n_amostra)
  )
})
# Meia-largura maxima do IC de Wilson de 95% entre as 120 celulas da figura.
z <- 1.96
ic_max <- with(dados, max(z * sqrt(P * (1 - P) / n_cel + z^2 / (4 * n_cel^2)) / (1 + z^2 / n_cel)))
ic_teto <- ceiling(1000 * ic_max) / 10 # em pp, arredondado para cima a 0,1
print(num_coorte)
cat(sprintf("Meia-largura maxima do IC 95%%: %.2f pp (nota: %.1f)\n", 100 * ic_max, ic_teto))

f1 <- function(x) {
  s <- formatC(x, format = "f", digits = 1)
  if (IDIOMA == "pt") sub(".", ",", s, fixed = TRUE) else s
}
f2 <- function(x) {
  s <- formatC(x, format = "f", digits = 2)
  if (IDIOMA == "pt") sub(".", ",", s, fixed = TRUE) else s
}
cob <- num_coorte$cobertura
d7 <- num_coorte$d7

NOTA <- list(
  pt = paste0(
    "Cada círculo é a probabilidade prevista de que um estudante do último ano ",
    "do ensino médio em 2012 (tom mais claro) ou em 2019 (tom mais escuro) tenha, ",
    "em até cinco anos, ficado fora da educação terciária, ingressado numa ",
    "instituição pública ou numa privada, por decil de desempenho no ENEM (eixo ",
    "horizontal) e decil de renda familiar (cor), estimada por um logit ",
    "multinomial saturado ajustado separadamente para cada coorte. Os decis são ",
    "calculados dentro de cada coorte, entre os estudantes que fizeram o ENEM. ",
    "A área do círculo é a participação daquele decil de renda naquele decil de ",
    "desempenho, de modo que os círculos de uma mesma cor e tom somam um em cada ",
    "painel. Os estudantes sem declaração de renda são excluídos (", f2(d7[1]),
    "\\% da amostra de 2012 e ", f2(d7[2]), "\\% da de 2019). As duas coortes não ",
    "têm a mesma cobertura: os estudantes observados no ENEM são ", f1(cob[1]),
    "\\% da coorte escolar de 2012 e ", f1(cob[2]), "\\% da de 2019, e parte da ",
    "mudança pode refletir quem fez o exame, e não quem ingressou. Todos os ",
    "intervalos de confiança de 95\\% são menores que $\\pm$", f1(ic_teto),
    " pontos percentuais e não são desenhados. Os desvios em relação à ",
    "especificação original e os diagnósticos de cobertura estão no"
  ),
  en = paste0(
    "Each circle is the predicted probability that a student in the final year ",
    "of secondary school in 2012 (lighter shade) or 2019 (darker shade) had, ",
    "within five years, entered no tertiary institution, a public one or a ",
    "private one, by decile of ENEM performance (horizontal axis) and family ",
    "income decile (colour), from a saturated multinomial logit fitted ",
    "separately to each cohort. Deciles are computed within each cohort among ",
    "students who sat the ENEM. Circle area is the share of that income decile ",
    "in that performance decile, so circles of one colour and shade sum to one ",
    "within each panel. Students with no income declaration are excluded (",
    f2(d7[1]), "\\% of the 2012 sample, ", f2(d7[2]), "\\% of 2019). The two ",
    "cohorts are not equally covered: the students observed in the ENEM are ",
    f1(cob[1]), "\\% of the 2012 school cohort and ", f1(cob[2]), "\\% of the 2019 ",
    "cohort, so part of the change may reflect who sat the examination rather ",
    "than who gained access. Every 95\\% confidence interval is narrower than ",
    "$\\pm$", f1(ic_teto), " percentage points and is not drawn. Departures ",
    "from the original specification and the coverage diagnostics are in"
  )
)[[IDIOMA]]

CAP <- list(
  pt = paste(
    "Probabilidade prevista de não acesso e de ingresso nos setores público e",
    "privado por decil de desempenho no ENEM, para os decis de renda mais pobre",
    "e mais rico, Brasil, coortes de concluintes do ensino médio de 2012 e 2019."
  ),
  en = paste(
    "Predicted probability of no access, public-sector and private-sector entry",
    "by ENEM performance decile, for the poorest and richest income deciles,",
    "Brazil, secondary-school cohorts of 2012 and 2019."
  )
)[[IDIOMA]]

FONTE <- list(
  pt = paste(
    "INEP --- Censo Escolar, ENEM e Censo da Educação Superior, vinculados por",
    "CPF mascarado via SEDAP+; especificação de Senkevics et al. (2024)"
  ),
  en = paste(
    "INEP --- Censo Escolar, ENEM and Censo da Educação Superior, linked by",
    "masked CPF via SEDAP+; specification from Senkevics et al. (2024)"
  )
)[[IDIOMA]]

ggsave(file.path(dirname(RAIZ), "thumbnail.png"), fig,
  width = LARGURA_TEXTO, height = 4.4, units = "in", dpi = 300, bg = "white"
)
cat("\n[OK] 085 concluido (IDIOMA =", IDIOMA, ").\n")
