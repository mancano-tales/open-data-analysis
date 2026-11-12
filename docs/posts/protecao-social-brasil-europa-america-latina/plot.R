# ==============================================================================
# posts/protecao-social-brasil-europa-america-latina/plot.R
#
# Grade pais x ano do gasto do governo geral em protecao social (funcao 710 da
# COFOG), em % do PIB, 2010-2023, com todos os paises que tem o dado. Os paises
# ficam agrupados por continente e ordenados pelo gasto medio no periodo; o
# Brasil fica em destaque. Sem medias por grupo: a cobertura muda muito de um
# ano para outro, e a media mudaria de composicao.
#
# ENTRADA (data/, redistribuida):
#   protecao_social.csv  iso3, ano, pct_pib, fonte (FMI, OCDE, CEPAL ou
#                        CEPAL-SPNF). Gerado por
#                        shared-pipeline/cofog/010_COFOG_Protecao_Social.R
#
# SAIDAS: fig-en.png, fig-pt.png e thumbnail.png (a figura em ingles).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

POST <- here::here("posts", "protecao-social-brasil-europa-america-latina")

dados <- read.csv(file.path(POST, "data", "protecao_social.csv")) |>
  # continente pelo countrycode; o FMI usa KOS para o Kosovo
  mutate(continente = countrycode::countrycode(iso3, "iso3c", "continent",
                                               custom_match = c(KOS = "Europe")))
stopifnot(!anyNA(dados$continente))

TXT <- list(
  pt = list(
    continentes = c(Europe = "Europa", Americas = "Américas", Asia = "Ásia",
                    Oceania = "Oceania", Africa = "África"),
    legenda = "% do PIB", nomes = "cldr.name.pt", decimal = ",", sufixo = "%"
  ),
  en = list(
    continentes = c(Europe = "Europe", Americas = "Americas", Asia = "Asia",
                    Oceania = "Oceania", Africa = "Africa"),
    legenda = "% of GDP", nomes = "cldr.name.en", decimal = ".", sufixo = "%"
  )
)
# marca no nome do pais: * OCDE; † CEPAL, governo geral; ‡ CEPAL, setor
# publico nao financeiro. Cada pais vem de uma so fonte.
MARCA <- c(FMI = "", OCDE = "*", CEPAL = "†", `CEPAL-SPNF` = "‡")
# vermelho (gasto baixo) -> amarelo -> verde -> azul (gasto alto)
PALETA <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4", "#2B83BA")

montar <- function(txt) {
  grade <- dados |>
    group_by(iso3) |>
    mutate(linha = paste0(countrycode::countrycode(iso3[1], "iso3c", txt$nomes,
                                                   custom_match = c(KOS = "Kosovo")),
                          paste(unique(MARCA[fonte]), collapse = "")),
           linha = if (iso3[1] == "BRA") toupper(linha) else linha,
           m = mean(pct_pib)) |>
    ungroup() |>
    mutate(bloco = factor(txt$continentes[continente], levels = txt$continentes))
  ordem <- grade |> distinct(bloco, linha, m) |> arrange(bloco, desc(m))
  grade |>
    mutate(linha_id = factor(paste(bloco, linha), levels = rev(paste(ordem$bloco, ordem$linha))))
}

figura <- function(txt) {
  grade <- montar(txt)
  rotulos <- setNames(as.character(grade$linha), as.character(grade$linha_id))
  numero <- function(x) sub(".", txt$decimal, sprintf("%.1f", x), fixed = TRUE)

  ggplot(grade, aes(factor(ano), linha_id, fill = pct_pib)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_tile(data = filter(grade, iso3 == "BRA"), fill = NA, colour = "#222222",
              linewidth = 0.5) +
    geom_text(aes(label = numero(pct_pib), fontface = if_else(iso3 == "BRA", "bold", "plain"),
                  colour = pct_pib < 3 | pct_pib > 23),
              size = 1.6, family = THESIS_FONT, show.legend = FALSE) +
    facet_grid(bloco ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_y_discrete(labels = rotulos) +
    scale_x_discrete(position = "top") +
    scale_fill_gradientn(colours = PALETA, limits = c(0, 28), breaks = seq(0, 25, 5),
                         labels = function(x) paste0(x, txt$sufixo), name = txt$legenda) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#222222"), guide = "none") +
    labs(x = NULL, y = NULL) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
    theme(
      panel.grid         = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.x.top    = element_text(size = 6.5),
      axis.text.y        = element_text(size = 5.8, hjust = 1),
      strip.placement    = "outside",
      strip.background   = element_blank(),
      strip.text.y.left  = element_text(angle = 0, hjust = 1, face = "bold", size = 7),
      panel.spacing.y    = unit(4, "pt"),
      legend.title       = element_text(size = 7, face = "plain"),
      legend.text        = element_text(size = 6.5),
      legend.key.width   = unit(1.4, "cm"),
      legend.key.height  = unit(0.22, "cm")
    )
}

n_paises <- n_distinct(dados$iso3)
for (idioma in names(TXT)) {
  ggsave(file.path(POST, paste0("fig-", idioma, ".png")), figura(TXT[[idioma]]),
         width = LARGURA_TEXTO, height = 1.2 + 0.155 * n_paises, units = "in",
         dpi = DPI_IMPRESSAO, bg = "white")
}
file.copy(file.path(POST, "fig-en.png"), file.path(POST, "thumbnail.png"),
          overwrite = TRUE)
