# ==============================================================================
# 090_Fig_Perfil_Renda_Modalidade.R
#
# Figura: perfil de renda familiar dos ingressantes por modalidade, setor
# privado e publico, a partir do vinculo ENEM x CENSUP no SEDAP+.
#
# Substitui a versao anterior (`075_Analise_Perfil_Renda.R` e o pacote
# `perfil-renda-ead-presencial/`), que tinha tres problemas:
#   1. rotulos em portugues, num corpo de tese em ingles;
#   2. pacote salvo a mao, so com .pdf e .png — sem o .R e o .qmd que
#      `finalizar_figura()` produz, logo sem snapshot do gerador nem bloco
#      de nota;
#   3. os anos contaminados entravam no pipeline sem diagnostico.
#
# ------------------------------------------------------------------------------
# JANELA 2015-2023, E POR QUE OS OUTROS DOIS ANOS SAEM
# ------------------------------------------------------------------------------
# Diagnostico por contagem de categorias de Q006 e pela fracao concentrada nas
# duas primeiras (A+B), que num ano saudavel fica entre 15% e 39%:
#
#   ano   n_cat privado / publico   % em A+B privado / publico
#   2014        17 / 4                    38,5 / 99,7      <- publico quebrado
#   2015-2023   17 / 17                   15,7-38,5 / 19,3-37,6   <- saudavel
#   2024        2 / 2                     100 / 100        <- nao e renda
#
# Em 2024 so existem as categorias A e B (66,9% e 33,1% no privado presencial).
# Isso nao e a pergunta de renda do questionario socioeconomico: e uma variavel
# binaria ocupando o mesmo nome de coluna. Usar 2024 como renda produziria
# "100% dos ingressantes abaixo de 1,5 SM" em todos os grupos — que foi
# exatamente o que a extracao devolveu.
#
# Em 2014 o setor PUBLICO tem 4 categorias e 99,7% em A+B, enquanto o privado
# do MESMO ANO tem as 17 com distribuicao plausivel. Mesma tabela, mesmo ano,
# resultados incompativeis por setor — anomalia nao explicada, que precisa de
# diagnostico proprio antes de o ano ser usado. Ate la, fora.
#
# NAO reintroduza esses anos sem antes rodar a checagem de n_cat e %A+B.
# ------------------------------------------------------------------------------
#
# LEITURA DA VARIAVEL: Q006 e renda TOTAL do domicilio em faixas de salario
# minimo, nao renda per capita. "Ate 1,5 SM" significa a familia inteira
# vivendo com ate um salario e meio. O tamanho do domicilio (Q005) foi extraido
# em separado (085_) justamente para verificar se difere entre modalidades; se
# diferir, a comparacao em renda familiar precisa ser lida com essa ressalva.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(here)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

ANOS_VALIDOS <- 2015:2023

CSV <- here::here(
  "4-DA-Code", "2026-08_SEDAP", "060_Analise_ENEM_Renda",
  "extraido_perfil_renda_modalidade_2014_2024.csv"
)
stopifnot(file.exists(CSV))
d <- fread(CSV)

# Guarda-rail: se um ano fora da janela entrar por engano, o script para em vez
# de plotar numero sem sentido.
chk <- d[, .(n_cat = uniqueN(Q006)), by = .(ano, setor)]
if (any(chk[ano %in% ANOS_VALIDOS]$n_cat != 17)) {
  stop("Ano dentro da janela com numero de categorias Q006 != 17. Rediagnostique.", call. = FALSE)
}

d <- d[ano %in% ANOS_VALIDOS]
d[, grupo := fifelse(setor == "Privado" & Modalidade == "EaD", "Private, distance",
  fifelse(setor == "Privado", "Private, in person", "Public, in person")
)]
d <- d[grupo != "Public, in person" | Modalidade == "Presencial"]

# A, B, C = sem renda, ate 1 SM, de 1 a 1,5 SM (codebook do ENEM)
d[, baixa := Q006 %in% c("A", "B", "C")]

res <- d[, .(N = sum(N)), by = .(ano, grupo, baixa)][
  , .(pct = 100 * sum(N[baixa]) / sum(N)), by = .(ano, grupo)
]

CORES <- c(
  "Private, distance" = "#D55E00",
  "Private, in person" = "#0072B2",
  "Public, in person" = "#009E73"
)

# Rotulos diretos: as duas linhas de baixo terminam a 4 pontos uma da outra,
# entao o deslocamento vertical e definido por grupo em vez de uniforme.
fim <- res[ano == max(ano)]
fim[, dy := fifelse(grupo == "Private, distance", 3.0,
  fifelse(grupo == "Public, in person", 3.4, -6.5)
)]

p <- ggplot(res, aes(x = ano, y = pct, colour = grupo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  geom_text(
    data = fim, aes(y = pct + dy, label = sprintf("%s: %.0f%%", grupo, pct)),
    hjust = 1, size = 2.7, fontface = "bold", show.legend = FALSE
  ) +
  scale_colour_manual(values = CORES, guide = "none") +
  scale_x_continuous(
    breaks = ANOS_VALIDOS,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA), expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    x = NULL,
    y = "Share of entrants",
    subtitle = "Entrants whose household income was 1.5 minimum wages or less"
  ) +
  theme(plot.subtitle = element_text(size = 8.5, colour = "#444444",
                                     margin = margin(b = 6)))

salvar_grafico(p,
  prefixo = "090_Perfil_Renda_Modalidade",
  largura = LARGURA_TEXTO, altura = 3.2, unidades = "in"
)

cat("\n=== % com renda familiar ate 1,5 SM ===\n")
print(as.data.frame(dcast(res, ano ~ grupo, value.var = "pct")[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)
]), row.names = FALSE)

finalizar_figura(
  plot = p,
  fig_label = "perfil-renda-modalidade",
  fig_cap = "Household income of entrants by sector and mode of delivery, Brazil, 2015--2023.",
  fonte = "INEP — Censo da Educação Superior linked to ENEM by masked CPF, via SEDAP+ (2015–2023)",
  nota = paste(
    "Each line is the share of that year's entrants whose household income, as declared on the ENEM",
    "questionnaire, was 1.5 minimum wages or less --- for the household as a whole, not per capita.",
    "Entrants are matched to their ENEM record of the same year by masked CPF, so the figure covers",
    "only those who took the exam that year: about a third of entrants, and not a random third.",
    "Coverage is highest where the exam is required for admission and lowest where it is not, which",
    "over-represents public in-person study by roughly 1.7 times and under-represents private distance",
    "study by about a third. Comparisons are therefore read within each year, between modes, and not",
    "as levels for the system as a whole. 2014 and 2024 are excluded: in 2024 the income item returns",
    "only two categories rather than seventeen and is not the income question, and in 2014 the",
    "public-sector records show the same defect while the private-sector records of that year do not."
  ),
  apendice = "sec-fignote-perfil-renda-modalidade",
  script_path = here::here(
    "4-DA-Code", "2026-08_SEDAP", "060_Analise_ENEM_Renda",
    "090_Fig_Perfil_Renda_Modalidade.R"
  ),
  largura = LARGURA_TEXTO, altura = 3.2
)
