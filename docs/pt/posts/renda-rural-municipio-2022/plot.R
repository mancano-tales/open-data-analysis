# ==============================================================================
# plot.R -- renda-rural-municipio-2022
# ODA: portado de 4-DA-Code/2026-11_Renda-Rural-Municipios/R/
#      020_Mapa_Renda_Rural_Municipio.R (repositorio da dissertacao) em
#      2026-11-11.
#
# Mapa coropletico do rendimento domiciliar per capita medio da populacao
# rural por municipio, Censo Demografico 2022. Le a tabela agregada de
# data/renda_rural_municipio_2022.csv (gerada por
# shared-pipeline/censo/050_Renda_Rural_Municipio_2022.R).
#
# Estetica inspirada nos mapas do Atlas da Questao Agraria Brasileira
# (Eduardo Paulon Girardi, NERA/UNESP), conferida em Girardi (2014, Revista
# do Departamento de Geografia -- USP, vol. especial Cartogeo, p. 302-331,
# Mapas 2-3 e 8) e Girardi (2025, Revista NERA, v. 28, n. 3, Mapa 3):
# mapa emoldurado com titulo em caixa-alta sobre um fio; municipios sem
# contorno e limites estaduais finos; paleta ordenada do amarelo ao
# vermelho-escuro (variavel visual "valor", da semiologia grafica de Bertin,
# que o autor adota); caixa "LEGENDA" no canto inferior esquerdo, com as
# classes e um histograma; cruzetas de coordenadas, seta de orientacao,
# escala grafica e bloco de creditos; projecao policonica (SIRGAS 2000 /
# Brazil Polyconic, EPSG:5880).
#
# Classes: quantis (7 classes) sobre os municipios com estimativa aceitavel
# (coeficiente de variacao <= 30%), arredondados a dezenas de reais.
# Municipios com CV > 30% ficam em cinza medio; os sem domicilio rural na
# amostra, em cinza claro.
#
# Malhas: {geobr} (IPEA) -- municipios de 2022 e UFs de 2020.
#
# SAIDAS: fig-pt.png, fig-en.png e thumbnail.png (a figura em ingles), na
# pasta do post.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(here)
})

POST <- here::here("posts", "renda-rural-municipio-2022")
ARQ <- file.path(POST, "data", "renda_rural_municipio_2022.csv")
if (!file.exists(ARQ)) {
  stop("Tabela nao encontrada: rode shared-pipeline/censo/050_Renda_Rural_Municipio_2022.R")
}

FONTE <- if ("Arial" %in% systemfonts::system_fonts()$family) "Arial" else "sans"
CV_MAX <- 0.30
N_CLASSES <- 7
CRS_MAPA <- 5880 # SIRGAS 2000 / Brazil Polyconic

TXT <- list(
  pt = list(
    titulo = "RENDIMENTO MÉDIO DA POPULAÇÃO RURAL POR MUNICÍPIO - 2022",
    legenda = "LEGENDA",
    descr = "Rendimento domiciliar per capita médio da\npopulação rural (R$ de jan. 2024; entre\nparênteses, número de municípios)",
    a = " a ", imprecisa = "Estimativa imprecisa (CV > 30%)",
    sem = "Sem domicílio rural na amostra",
    hist = "Histograma", municipios = "Municípios",
    oeste = "O", big = ".", dec = ",",
    creditos = paste(
      "Dados: IBGE, Censo Demográfico 2022, questionário da amostra",
      "(microdados de acesso controlado).",
      "Média ponderada por pessoa em domicílio particular rural;",
      "R$ de janeiro de 2024 (IPCA). Malhas: geobr (IPEA).",
      "Projeção Policônica, SIRGAS 2000.",
      "Elaboração: Tales Mancano, 2026. Estética inspirada no",
      "Atlas da Questão Agrária Brasileira (E. P. Girardi).",
      sep = "\n"
    )
  ),
  en = list(
    titulo = "AVERAGE INCOME OF THE RURAL POPULATION BY MUNICIPALITY - 2022",
    legenda = "LEGEND",
    descr = "Mean per capita household income of the\nrural population (R$ of Jan. 2024; number\nof municipalities in parentheses)",
    a = " to ", imprecisa = "Imprecise estimate (CV > 30%)",
    sem = "No rural household in the sample",
    hist = "Histogram", municipios = "Municipalities",
    oeste = "W", big = ",", dec = ".",
    creditos = paste(
      "Data: IBGE, 2022 Demographic Census, long-form sample",
      "(controlled-access microdata).",
      "Person-weighted mean over rural private households;",
      "R$ of January 2024 (IPCA). Boundaries: geobr (IPEA).",
      "Polyconic projection, SIRGAS 2000.",
      "By Tales Mancano, 2026. Styled after the Atlas da",
      "Questão Agrária Brasileira (E. P. Girardi).",
      sep = "\n"
    )
  )
)

renda <- read.csv(ARQ, encoding = "UTF-8") |>
  mutate(code_muni = as.integer(code_muni))

# 1. Malhas -----------------------------------------------------------------------
mun <- geobr::read_municipality(year = 2022, showProgress = FALSE) |>
  st_transform(CRS_MAPA) |>
  mutate(code_muni = as.integer(code_muni))
uf <- geobr::read_state(year = 2020, showProgress = FALSE) |>
  st_transform(CRS_MAPA)
brasil <- st_union(uf)

sem_malha <- setdiff(renda$code_muni, mun$code_muni)
if (length(sem_malha) > 0) {
  warning("Municipios da tabela sem poligono na malha: ", paste(sem_malha, collapse = ", "))
}

# 2. Classes (iguais nas duas linguas) -----------------------------------------
valido <- renda |> filter(!is.na(cv), cv <= CV_MAX)
brks <- round(quantile(valido$renda_media_jan2024,
                       probs = seq(0, 1, length.out = N_CLASSES + 1)), -1)
brks[1] <- floor(min(valido$renda_media_jan2024) / 10) * 10
brks[length(brks)] <- ceiling(max(valido$renda_media_jan2024) / 10) * 10
names(brks) <- NULL

renda <- renda |>
  mutate(classe_id = if_else(
    !is.na(cv) & cv <= CV_MAX,
    as.integer(cut(renda_media_jan2024, brks, include.lowest = TRUE)),
    N_CLASSES + 1L
  ))

mapa_base <- mun |>
  left_join(renda |> select(code_muni, classe_id), by = "code_muni") |>
  mutate(classe_id = coalesce(classe_id, N_CLASSES + 2L))

cores_id <- c(RColorBrewer::brewer.pal(N_CLASSES, "YlOrRd"), "#a6a6a6", "#e3e3e3")

# 3. Moldura e elementos fixos (coordenadas projetadas) -----------------------
bb <- st_bbox(brasil)
larg <- unname(bb["xmax"] - bb["xmin"])
alt <- unname(bb["ymax"] - bb["ymin"])
xlim <- c(unname(bb["xmin"]) - 0.20 * larg, unname(bb["xmax"]) + 0.03 * larg)
ylim <- c(unname(bb["ymin"]) - 0.03 * alt, unname(bb["ymax"]) + 0.13 * alt)

cruz <- data.frame(lon = c(-72, -40, -40), lat = c(0, 0, -25)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(CRS_MAPA)
cruz_xy <- cbind(st_drop_geometry(cruz), st_coordinates(cruz))

esc_x0 <- xlim[2] - 0.30 * diff(xlim)
esc_y0 <- ylim[1] + 0.13 * diff(ylim)
esc <- data.frame(
  xmin = esc_x0 + c(0, 200e3, 400e3), xmax = esc_x0 + c(200e3, 400e3, 600e3),
  ymin = esc_y0, ymax = esc_y0 + 22e3, fill = c("black", "white", "black")
)
esc_rot <- data.frame(x = esc_x0 + c(0, 200e3, 400e3, 600e3),
                      lab = c("0", "200", "400", "600 km"))
norte_x <- xlim[2] - 0.06 * diff(xlim)
norte_y <- ylim[1] + 0.40 * diff(ylim)
tit_x <- xlim[1] + 0.025 * diff(xlim)
tit_y <- ylim[2] - 0.035 * diff(ylim)
fio_y <- ylim[2] - 0.062 * diff(ylim)

TETO <- ceiling(quantile(valido$renda_media_jan2024, 0.99) / 100) * 100

# 4. Figura ---------------------------------------------------------------------
figura <- function(t) {
  fmt <- function(x) format(x, big.mark = t$big, decimal.mark = t$dec,
                            scientific = FALSE, trim = TRUE)
  rot_classes <- paste0(fmt(head(brks, -1)), t$a, fmt(tail(brks, -1)))
  niveis <- c(rot_classes, t$imprecisa, t$sem)
  cores <- setNames(cores_id, niveis)

  mapa_df <- mapa_base |> mutate(classe = factor(niveis[classe_id], levels = niveis))
  cz <- cruz_xy |>
    mutate(rot_lon = paste0(abs(lon), "° ", t$oeste),
           rot_lat = if_else(lat == 0, "0°", paste0(abs(lat), "° S")))

  mapa <- ggplot() +
    geom_sf(data = mapa_df, aes(fill = classe, colour = classe), linewidth = 0.04) +
    geom_sf(data = uf, fill = NA, colour = "grey20", linewidth = 0.22) +
    geom_sf(data = brasil, fill = NA, colour = "grey5", linewidth = 0.4) +
    geom_point(data = cz, aes(X, Y), shape = 3, size = 3.2, stroke = 0.35) +
    geom_text(data = cz, aes(X, Y, label = rot_lon), nudge_y = 75e3, size = 2.1, family = FONTE) +
    geom_text(data = cz, aes(X, Y, label = rot_lat), nudge_x = 85e3, hjust = 0, size = 2.1, family = FONTE) +
    geom_rect(data = esc, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = esc$fill, colour = "black", linewidth = 0.25) +
    geom_text(data = esc_rot, aes(x = x, y = esc_y0 - 40e3, label = lab),
              size = 2.1, family = FONTE, vjust = 1) +
    annotate("segment", x = norte_x, xend = norte_x, y = norte_y - 110e3, yend = norte_y + 110e3,
             linewidth = 0.5, arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
    annotate("text", x = norte_x, y = norte_y + 175e3, label = "N",
             fontface = "bold", size = 3.2, family = FONTE) +
    annotate("text", x = esc_x0, y = esc_y0 - 110e3, label = t$creditos,
             hjust = 0, vjust = 1, size = 1.85, family = FONTE, lineheight = 1.05, colour = "grey15") +
    annotate("text", x = tit_x, y = tit_y, label = t$titulo,
             hjust = 0, fontface = "bold", size = 4.3, family = FONTE) +
    annotate("segment", x = tit_x, xend = xlim[2] - 0.025 * diff(xlim), y = fio_y, yend = fio_y,
             linewidth = 0.35, colour = "grey30") +
    scale_fill_manual(values = cores, drop = FALSE, guide = "none") +
    scale_colour_manual(values = cores, drop = FALSE, guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA, crs = CRS_MAPA) +
    theme_void(base_family = FONTE) +
    theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.6),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(4, 4, 4, 4))

  leg_df <- mapa_df |>
    st_drop_geometry() |>
    count(classe, .drop = FALSE) |>
    mutate(y = rev(seq_along(classe)) + if_else(classe %in% c(t$imprecisa, t$sem), 0, 0.6),
           rot = paste0(classe, "  (", fmt(n), ")"))

  leg_classes <- ggplot(leg_df) +
    geom_tile(aes(x = 0, y = y, fill = classe), width = 0.9, height = 0.78,
              colour = "grey35", linewidth = 0.2) +
    geom_text(aes(x = 0.75, y = y, label = rot), hjust = 0, size = 2.15, family = FONTE) +
    annotate("text", x = -0.45, y = max(leg_df$y) + 4.1, label = t$legenda,
             hjust = 0, vjust = 1, fontface = "bold", size = 2.5, family = FONTE) +
    annotate("text", x = -0.45, y = max(leg_df$y) + 3.0, label = t$descr,
             hjust = 0, vjust = 1, size = 2.05, family = FONTE, lineheight = 0.95) +
    scale_fill_manual(values = cores, guide = "none") +
    scale_x_continuous(limits = c(-0.5, 9)) +
    scale_y_continuous(limits = c(0.4, max(leg_df$y) + 4.3)) +
    theme_void(base_family = FONTE)

  hist_df <- valido |>
    mutate(x = pmin(renda_media_jan2024, TETO - 1),
           faixa = floor(x / 50) * 50 + 25,
           classe = cut(pmin(faixa, max(brks)), brks, labels = rot_classes, include.lowest = TRUE)) |>
    count(faixa, classe)

  leg_hist <- ggplot(hist_df, aes(x = faixa, y = n, fill = classe)) +
    geom_col(width = 50, colour = "grey30", linewidth = 0.08) +
    annotate("text", x = TETO - 25, y = hist_df$n[which.max(hist_df$faixa)],
             label = paste0(fmt(TETO), "+"), vjust = -0.5, hjust = 0.8,
             size = 1.7, family = FONTE) +
    scale_fill_manual(values = cores, guide = "none") +
    scale_x_continuous(breaks = scales::breaks_pretty(5), labels = fmt,
                       expand = expansion(mult = 0.01)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = t$hist, x = NULL, y = t$municipios) +
    theme_classic(base_family = FONTE, base_size = 6) +
    theme(plot.title = element_text(face = "bold", size = 6.3, margin = margin(b = 2)),
          axis.line = element_line(linewidth = 0.25),
          axis.ticks = element_line(linewidth = 0.25),
          axis.text = element_text(size = 5.2, colour = "grey20"),
          axis.title.y = element_text(size = 5.4),
          plot.background = element_rect(fill = NA, colour = NA))

  legenda <- (leg_classes / leg_hist) + plot_layout(heights = c(1.55, 1)) &
    theme(plot.background = element_rect(fill = "white", colour = NA))
  caixa <- wrap_elements(full = legenda) +
    theme(plot.background = element_rect(fill = "white", colour = "grey30", linewidth = 0.3))

  mapa + inset_element(caixa, left = 0.012, bottom = 0.012, right = 0.36, top = 0.47,
                       align_to = "panel")
}

# 5. Salvamento -----------------------------------------------------------------
LARG <- 8.0
ALT <- LARG * diff(ylim) / diff(xlim)
for (idioma in c("pt", "en")) {
  ggsave(file.path(POST, paste0("fig-", idioma, ".png")), figura(TXT[[idioma]]),
         width = LARG, height = ALT, dpi = 300, device = ragg::agg_png, bg = "white")
}
file.copy(file.path(POST, "fig-en.png"), file.path(POST, "thumbnail.png"), overwrite = TRUE)
cat("Quebras das classes (R$ jan/2024):", paste(brks, collapse = " | "), "\n")
cat("Salvo: fig-pt.png, fig-en.png, thumbnail.png em", POST, "\n")
