# ==============================================================================
# posts/gasto-estudante-educacao-basica/plot.R
#
# Investimento publico direto por estudante na educacao basica e no ensino
# medio, Brasil, 2000-2021, em reais de 2024. Portado em 2026-11-14 do script
# 030_Plot_Gasto_Estudante_Basica_en.R da dissertacao (figura do working paper
# do capitulo 0202).
#
# ENTRADAS (data/, redistribuidas):
#   inep_investimento_estudante_nominal.csv  INEP, Indicadores Financeiros
#       Educacionais, planilha "Investimento_estudante_valor_nominal.xlsx"
#       (versao de 10/09/2024), em reais correntes: educacao basica, educacao
#       infantil, anos iniciais e finais do fundamental e ensino medio.
#       Investimento direto da Uniao, estados e municipios: pessoal ativo,
#       outras despesas correntes e investimento; sem aposentadorias, bolsas,
#       financiamento estudantil, divida e transferencias ao setor privado.
#   ipca_media_anual.csv  IPCA, media anual do numero-indice (IBGE, SIDRA,
#       tabela 1737).
#
# POR QUE A PLANILHA NOMINAL: a planilha "valor real" do INEP mistura precos de
# 2018 e de 2021 entre as edicoes; aqui a nominal e deflacionada pelo IPCA
# medio anual, base 2024.
#
# SAIDAS: fig-en.png, fig-pt.png e thumbnail.png (a figura em ingles).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(here)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

POST <- here::here("posts", "gasto-estudante-educacao-basica")
if (!file.exists(file.path(POST, "data", "inep_investimento_estudante_nominal.csv"))) {
  stop("Dados ausentes em data/. Rode antes: ",
       "shared-pipeline/gasto-educacao-basica/010_INEP_Investimento_Estudante.R", call. = FALSE)
}
inep <- read.csv(file.path(POST, "data", "inep_investimento_estudante_nominal.csv"))
ipca <- read.csv(file.path(POST, "data", "ipca_media_anual.csv"))
fator <- function(ano) ipca$indice[ipca$ano == 2024] / ipca$indice[match(ano, ipca$ano)]

d <- inep |>
  select(ano, basica, em) |>
  pivot_longer(-ano, names_to = "etapa", values_to = "nominal") |>
  mutate(valor = nominal * fator(ano) / 1000)
stopifnot(!anyNA(d$valor))

INI <- min(d$ano)
FIM <- max(d$ano)
escala_x <- scale_x_anos_tese(anos = INI:FIM, limits = c(INI, FIM),
                              expand = expansion(mult = c(0.02, 0.02)))

TXT <- list(
  en = list(basica = "Basic education (all stages)", em = "Upper secondary education",
            y = "R$ thousand per student\n(2024 prices)"),
  pt = list(basica = "Educação básica (todas as etapas)", em = "Ensino médio",
            y = "R$ mil por estudante\n(preços de 2024)")
)
COR <- c(basica = "grey30", em = PALETA_QUALITATIVA[[6]])
TIPO <- c(basica = "22", em = "solid")

figura <- function(txt) {
  rot <- c(basica = txt$basica, em = txt$em)
  p <- ggplot(d, aes(ano, valor, colour = etapa, linetype = etapa)) +
    linhas_transicao_gov() +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.9, show.legend = FALSE) +
    scale_colour_manual(name = NULL, values = COR, labels = rot, breaks = names(rot)) +
    scale_linetype_manual(name = NULL, values = TIPO, labels = rot, breaks = names(rot)) +
    escala_x +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = txt$y) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.margin = margin(t = 0))
  faixa <- banner_governos(fim_dados = FIM, scale_x = escala_x, ini_dados = INI)
  faixa / p + plot_layout(heights = c(0.6, 9))
}

for (idioma in names(TXT)) {
  ggsave(file.path(POST, paste0("fig-", idioma, ".png")), figura(TXT[[idioma]]),
         width = LARGURA_TEXTO, height = 4, units = "in", dpi = DPI_IMPRESSAO)
}
file.copy(file.path(POST, "fig-en.png"), file.path(POST, "thumbnail.png"), overwrite = TRUE)

print(as.data.frame(d |> filter(ano %in% c(2000, 2014, FIM)) |>
  transmute(ano, etapa, valor = round(valor, 2))))
cat("OK\n")
