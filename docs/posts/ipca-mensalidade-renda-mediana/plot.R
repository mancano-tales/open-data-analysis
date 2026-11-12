# ==============================================================================
# posts/ipca-mensalidade-renda-mediana/plot.R
#
# Mensalidade do ensino superior em relacao ao IPCA geral e a renda domiciliar
# per capita mediana, Brasil. Portado de 4-DA-Code/2026-07_IPCA-Mensalidades/R/
# 011_IPCA_Mensalidade_vs_Renda_Mediana.R, da dissertacao, em 2026-11-06.
#
# ENTRADAS (data/, redistribuidas):
#   mensalidade_mensal.csv  subitem 8101005 do IPCA e IPCA geral. Mesma serie de
#                           shared-pipeline/ipca/010_IPCA_Curso_Superior_Series.R
#   renda_mediana_anual.csv renda domiciliar per capita mediana em R$ de jan/2024.
#                           Mesma serie de posts/renda-mediana-domiciliar-pc/
#
# CONSTRUCAO DA SERIE LARANJA: a mediana existe por ano de pesquisa. Cada ponto
# usa o mes de referencia da pesquisa, em que ela foi deflacionada (setembro na
# PNAD, junho na PNADC). A mediana volta ao valor nominal pelo IPCA, na convencao
# do deflateBR (indice do mes anterior a data nominal). Mensalidade e renda valem
# 100 em set/1999.
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

POST <- here::here("posts", "ipca-mensalidade-renda-mediana")
ANO_BASE <- 1999L
ANO_SPLICE <- 2015L # PNAD ate aqui; PNADC depois
DATA_DEFLATOR <- as.Date("2024-01-01")

serie <- read.csv(file.path(POST, "data", "mensalidade_mensal.csv")) |>
  mutate(data = as.Date(data))
renda <- read.csv(file.path(POST, "data", "renda_mediana_anual.csv"))

mes_anterior <- function(d) {
  as.Date(format(as.Date(format(d, "%Y-%m-01")) - 1, "%Y-%m-01"))
}
ipca_mes_anterior <- function(d) {
  serie$ipca_idx[match(mes_anterior(d), serie$data)]
}
ipca_destino <- serie$ipca_idx[serie$data == DATA_DEFLATOR]
stopifnot(length(ipca_destino) == 1)

df_renda <- renda |>
  filter(ano >= ANO_BASE) |>
  mutate(data = as.Date(sprintf("%d-%02d-01", ano,
                                ifelse(ano <= ANO_SPLICE, 9L, 6L)))) |>
  inner_join(select(serie, data, sub_idx), by = "data") |>
  mutate(mediana_nominal = mediana * ipca_mes_anterior(data) / ipca_destino) |>
  arrange(ano)

base <- filter(df_renda, ano == ANO_BASE)
stopifnot(nrow(base) == 1)

df_renda <- df_renda |>
  mutate(
    idx_renda = 100 * (sub_idx / base$sub_idx) /
      (mediana_nominal / base$mediana_nominal),
    seg = cumsum(c(1L, as.integer(diff(ano) > 1L)))
  )

print(as.data.frame(df_renda |> transmute(ano, idx_renda = round(idx_renda, 1))),
      row.names = FALSE)

TXT <- list(
  pt = list(real = "Mensalidade em relação ao IPCA geral",
            renda = "Mensalidade em relação à renda mediana",
            y = "Índice (1999 = 100)"),
  en = list(real = "Tuition relative to headline IPCA",
            renda = "Tuition relative to median income",
            y = "Index (1999 = 100)")
)
COR <- c(real = "#0072B2", renda = "#D55E00")

figura <- function(txt) {
  ggplot() +
    geom_hline(yintercept = 100, colour = "grey60", linewidth = 0.35,
               linetype = "dashed") +
    geom_line(data = serie, aes(data, idx_real, colour = "real"), linewidth = 0.8) +
    geom_line(data = df_renda, aes(data, idx_renda, colour = "renda", group = seg),
              linewidth = 0.8) +
    geom_point(data = filter(df_renda, !visita5),
               aes(data, idx_renda, colour = "renda"), size = 1.3) +
    geom_point(data = filter(df_renda, visita5),
               aes(data, idx_renda, colour = "renda"),
               fill = "white", shape = 21, size = 1.7, stroke = 0.7) +
    scale_colour_manual(values = COR, breaks = names(COR), name = NULL,
                        labels = c(real = txt$real, renda = txt$renda)) +
    scale_x_anos_tese_date(datas = serie$data,
                           expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(breaks = seq(0, 140, 20)) +
    labs(x = NULL, y = txt$y) +
    guides(colour = guide_legend(override.aes = list(linewidth = 1.1,
                                                     shape = c(NA, 16)))) +
    theme(
      legend.position      = "bottom",
      legend.justification = "center",
      legend.background    = element_blank(),
      legend.key           = element_blank(),
      legend.margin        = margin(0, 0, 0, 0),
      legend.key.width     = unit(0.8, "cm"),
      legend.text          = element_text(size = 8, family = THESIS_FONT),
      panel.grid.major.x   = element_blank()
    )
}

for (idioma in names(TXT)) {
  ggsave(file.path(POST, paste0("fig-", idioma, ".png")), figura(TXT[[idioma]]),
         width = LARGURA_TEXTO, height = ALTURA_PADRAO, units = "in",
         dpi = DPI_IMPRESSAO)
}
file.copy(file.path(POST, "fig-en.png"), file.path(POST, "thumbnail.png"),
          overwrite = TRUE)
