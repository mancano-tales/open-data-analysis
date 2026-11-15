# ==============================================================================
# posts/gini-renda-mediana/plot.R
#
# Indice de Gini e renda domiciliar per capita mediana, Brasil, 1992-2025.
# Portado em 2026-11-14 do script 097J_WP_Gini_Renda_Mediana_en.R da dissertacao
# (figura do working paper do capitulo 0202), que repete o painel B da figura 1
# da introducao (097I).
#
# ENTRADAS (data/, redistribuidas):
#   gini_oficial_anual.csv   Gini da renda domiciliar per capita no universo
#                            oficial (com renda zero). Mesma serie de
#                            shared-pipeline/pnadc/238_Serie_Desigualdade_Oficial.R
#                            e do post desigualdade-renda-gini-palma-decil.
#   renda_mediana_anual.csv  renda domiciliar per capita mediana, R$ de jan/2024.
#                            Mesma serie do post renda-mediana-domiciliar-pc.
#   Em ambas, visita5 = TRUE marca 2020-2021 (PNADC visita 5, por telefone).
#
# SAIDAS: fig-en.png, fig-pt.png e thumbnail.png (a figura em ingles).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

POST <- here::here("posts", "gini-renda-mediana")
segmentos <- function(d) d |> arrange(ano) |> mutate(seg = cumsum(c(1L, as.integer(diff(ano) > 1L))))
gini <- read.csv(file.path(POST, "data", "gini_oficial_anual.csv")) |> segmentos()
renda <- read.csv(file.path(POST, "data", "renda_mediana_anual.csv")) |> segmentos()
stopifnot(!anyNA(gini$gini), !anyNA(renda$mediana))

FIM <- max(gini$ano)
INI <- min(gini$ano)
escala_x <- scale_x_anos_tese(anos = c(ANOS_TRANSICAO_GOV, FIM),
                              expand = expansion(mult = c(0.015, 0), add = c(0, 1.8)))

COR <- c(gini = PALETA_QUALITATIVA[[3]], renda = "#0072B2")
esc_b <- diff(range(gini$gini)) / diff(range(renda$mediana))
esc_a <- min(gini$gini) - esc_b * min(renda$mediana)
renda <- renda |> mutate(y = esc_a + esc_b * mediana)

TXT <- list(
  en = list(gini = "Gini index", renda = "Median income (right axis)",
            y = "Gini index\n(household income)",
            y2 = "Median per-capita household\nincome (R$, Jan. 2024 prices)",
            dec = ".", mil = ","),
  pt = list(gini = "Índice de Gini", renda = "Renda mediana (eixo direito)",
            y = "Índice de Gini\n(renda domiciliar)",
            y2 = "Renda domiciliar per capita\nmediana (R$ de jan./2024)",
            dec = ",", mil = ".")
)

figura <- function(txt) {
  p <- ggplot(mapping = aes(x = ano, group = seg)) +
    geom_vline(xintercept = 2015.5, colour = "#9A9A9A", linewidth = 0.4) +
    geom_line(data = gini, aes(y = gini, colour = "gini"), linewidth = 0.9) +
    geom_line(data = renda, aes(y = y, colour = "renda"), linewidth = 0.9) +
    geom_point(data = filter(gini, !visita5), aes(y = gini, colour = "gini"), size = 1.8) +
    geom_point(data = filter(gini, visita5), aes(y = gini, colour = "gini"),
               fill = "white", shape = 21, size = 2, stroke = 0.7) +
    geom_point(data = filter(renda, !visita5), aes(y = y, colour = "renda"), shape = 15, size = 1.7) +
    geom_point(data = filter(renda, visita5), aes(y = y, colour = "renda"),
               fill = "white", shape = 22, size = 1.9, stroke = 0.7) +
    scale_colour_manual(name = NULL, values = COR, breaks = names(COR),
                        labels = c(gini = txt$gini, renda = txt$renda)) +
    guides(colour = guide_legend(override.aes = list(shape = c(16, 15), fill = NA, size = 2))) +
    escala_x +
    scale_y_continuous(breaks = seq(0.48, 0.62, 0.02),
                       labels = function(x) formatC(x, format = "f", digits = 2, decimal.mark = txt$dec),
                       expand = expansion(mult = c(0.05, 0.08)),
                       sec.axis = sec_axis(~ (. - esc_a) / esc_b, name = txt$y2,
                                           labels = function(x) format(round(x), big.mark = txt$mil, decimal.mark = txt$dec, trim = TRUE))) +
    labs(x = NULL, y = txt$y) +
    theme(panel.grid.minor   = element_blank(),
          axis.title.y       = element_text(colour = COR[["gini"]]),
          axis.title.y.right = element_text(colour = COR[["renda"]]),
          legend.position    = "bottom",
          legend.margin      = margin(t = 0))
  faixa <- banner_governos(fim_dados = FIM, ini_dados = INI, scale_x = escala_x, tamanho_pt = 6.5) +
    theme(plot.margin = margin(2, 5.5, 1, 5.5))
  faixa / p + plot_layout(heights = c(0.6, 9))
}

for (idioma in names(TXT)) {
  ggsave(file.path(POST, paste0("fig-", idioma, ".png")), figura(TXT[[idioma]]),
         width = LARGURA_TEXTO, height = 4.2, units = "in", dpi = DPI_IMPRESSAO)
}
file.copy(file.path(POST, "fig-en.png"), file.path(POST, "thumbnail.png"), overwrite = TRUE)
cat("OK\n")
