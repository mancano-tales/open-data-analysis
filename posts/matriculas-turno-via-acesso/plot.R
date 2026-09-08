# ==============================================================================
# 050_Plot_Nicho_100pct_SEDAP.R
#
# Reproducao 100% SEDAP da figura de nicho de matriculas (Setor x Turno x Via de Acesso)
# para a serie historica COMPLETA de 15 anos (2010--2024).
#
# Elimina 100% qualquer dependencia dos microdados abertos antigos, consumindo
# diretamente o dataset derivado extraido via API do SEDAP (INEP/RNP):
#   5-data/INEP/derived/nicho_agregado_sedap_2010_2024.csv
#
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(readr)
  library(scales)
  library(colorspace)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

# 1. Carregar Dados 100% SEDAP (2010–2024) -------------------------------------
csv_sedap <- here::here("data-raw", "INEP", "derived", "nicho_agregado_sedap_2010_2024.csv")

if (!file.exists(csv_sedap)) {
  stop("O arquivo 'nicho_agregado_sedap_2010_2024.csv' nao foi encontrado em 5-data/INEP/derived/.", call. = FALSE)
}

df_raw <- read_csv(csv_sedap, show_col_types = FALSE)

# Padronizar nomes de colunas e categorias
df_clean <- df_raw %>%
  rename(
    ANO = ano,
    Setor = setor,
    Grupo_Turno = turno,
    Subsidio_Tipo = via,
    QT_MATRICULAS = QT_MATRICULAS
  ) %>%
  filter(Setor %in% c("Publico", "Público", "Privado")) %>%
  mutate(
    Setor = ifelse(Setor %in% c("Publico", "Público"), "Público", "Privado"),
    Grupo_Turno = factor(Grupo_Turno, levels = c("Day", "Evening", "Distance"))
  )

prefixo <- "050_Plot_Nicho_100pct_SEDAP"

# 2. Grade HCL de 15 cores (Idêntica ao sistema de design 028) ------------------
base_hcl <- tibble::tribble(
  ~SetorSubsidio,    ~h0,  ~c0, ~l0,
  "Público|Open",    215,   32,  80,
  "Público|Cotas",   268,   70,  42,
  "Privado|Open",    100,   30,  84,
  "Privado|FIES",     35,   78,  62,
  "Privado|ProUni",  330,   78,  38
)

gerar_cor <- function(h0, c0, l0, turno) {
  dplyr::case_when(
    turno == "Day" ~ colorspace::hex(
      colorspace::polarLUV(L = pmin(l0 + (100 - l0) * 0.45, 95), C = c0 * 0.70, H = h0 + 22),
      fixup = TRUE
    ),
    turno == "Evening" ~ colorspace::hex(colorspace::polarLUV(L = l0, C = c0, H = h0), fixup = TRUE),
    turno == "Distance" ~ colorspace::hex(
      colorspace::polarLUV(L = pmax(l0 - l0 * 0.45, 15), C = pmin(c0 * 1.15, 95), H = h0 - 22),
      fixup = TRUE
    )
  )
}

grade_cores <- tidyr::expand_grid(base_hcl, Turno = c("Day", "Evening", "Distance")) %>%
  rowwise() %>%
  mutate(cor = gerar_cor(h0, c0, l0, Turno)) %>%
  ungroup() %>%
  mutate(
    Setor    = sub("\\|.*", "", SetorSubsidio),
    Subsidio = sub(".*\\|", "", SetorSubsidio),
    Perfil   = paste(SetorSubsidio, Turno, sep = "|")
  )

cores_15 <- setNames(grade_cores$cor, grade_cores$Perfil)

texto_contraste <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(lum > 0.55, "#222222", "white")
}
cor_texto_15 <- setNames(texto_contraste(grade_cores$cor), grade_cores$Perfil)

niveis_perfil <- c(
  "Público|Open|Day", "Público|Open|Evening", "Público|Open|Distance",
  "Público|Cotas|Day", "Público|Cotas|Evening", "Público|Cotas|Distance",
  "Privado|Open|Day", "Privado|Open|Evening", "Privado|Open|Distance",
  "Privado|FIES|Day", "Privado|FIES|Evening", "Privado|FIES|Distance",
  "Privado|ProUni|Day", "Privado|ProUni|Evening", "Privado|ProUni|Distance"
)

# 3. Agregação e Rótulos --------------------------------------------------------
df_plot <- df_clean %>%
  mutate(Perfil = paste(Setor, Subsidio_Tipo, Grupo_Turno, sep = "|")) %>%
  group_by(ANO, Perfil) %>%
  summarise(Matriculas = sum(QT_MATRICULAS, na.rm = TRUE), .groups = "drop") %>%
  group_by(ANO) %>%
  mutate(
    Total_Ano  = sum(Matriculas),
    Share      = Matriculas / Total_Ano * 100,
    Label_Prop = if_else(Share >= 2.5, sprintf("%.1f%%", Share), ""),
    Label_Abs  = if_else(Share >= 2.5, sprintf("%.2f M", Matriculas / 1e6), "")
  ) %>%
  ungroup() %>%
  mutate(Perfil = factor(Perfil, levels = niveis_perfil))

# 4. Legenda em Mapa de Calor HCL -----------------------------------------------
grade_cores$Setor    <- factor(grade_cores$Setor, levels = c("Público", "Privado"))
grade_cores$Subsidio <- factor(grade_cores$Subsidio, levels = c("Open", "Cotas", "FIES", "ProUni"))
grade_cores$Turno    <- factor(grade_cores$Turno, levels = c("Distance", "Evening", "Day"))

p_legenda <- ggplot(grade_cores, aes(x = Subsidio, y = Turno, fill = cor)) +
  geom_tile(colour = "white", linewidth = 1.1, width = 0.94, height = 0.9) +
  facet_grid(~Setor,
    scales = "free_x", space = "free_x",
    labeller = as_labeller(c("Público" = "Public", "Privado" = "Private"))
  ) +
  scale_fill_identity() +
  scale_x_discrete(labels = c("Cotas" = "Quotas")) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9, base_family = THESIS_FONT) +
  theme(
    strip.text        = element_text(face = "bold", size = 10, margin = margin(b = 2)),
    strip.background  = element_blank(),
    panel.grid        = element_blank(),
    axis.text.x       = element_text(size = 8.5),
    axis.text.y       = element_text(size = 8.5),
    panel.spacing.x   = unit(0.7, "lines"),
    plot.margin       = margin(2, 8, 0, 8)
  )

# 5. Gráfico Principal (Série Completa 2010--2024) -----------------------------
anos_completos <- scale_x_continuous(breaks = min(df_plot$ANO):max(df_plot$ANO))

# A) Proporção (%)
p_prop <- ggplot(df_plot, aes(x = ANO, y = Matriculas, fill = Perfil, group = Perfil)) +
  geom_col(position = "fill", width = 0.85, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = Label_Prop, colour = Perfil),
    position = position_fill(vjust = 0.5), size = 2.4, fontface = "plain", show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_15) +
  scale_colour_manual(values = cor_texto_15) +
  scale_y_continuous(labels = scales::percent) +
  anos_completos +
  labs(x = "Year", y = "Composition of enrollments (%)") +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

# B) Valores Absolutos (milhões)
p_abs <- ggplot(df_plot, aes(x = ANO, y = Matriculas, fill = Perfil, group = Perfil)) +
  geom_col(width = 0.85, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = Label_Abs, colour = Perfil),
    position = position_stack(vjust = 0.5), size = 2.4, fontface = "plain", show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_15) +
  scale_colour_manual(values = cor_texto_15) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
  anos_completos +
  labs(x = "Year", y = "Total enrollments (millions)") +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

p_fig_prop <- p_prop / p_legenda + patchwork::plot_layout(heights = c(6.5, 1.0))
p_fig_abs  <- p_abs / p_legenda + patchwork::plot_layout(heights = c(6.5, 1.0))

# 6. Salvando Rascunhos e Promoção Final ---------------------------------------
ALTURA_FIG <- 8.0

for (fmt in c("png", "pdf")) {
  salvar_grafico(p_fig_prop, paste0(prefixo, "_A_Proporcao"),
    largura = LARGURA_TEXTO, altura = ALTURA_FIG, formato = fmt
  )
  salvar_grafico(p_fig_abs, paste0(prefixo, "_B_Absoluto"),
    largura = LARGURA_TEXTO, altura = ALTURA_FIG, formato = fmt
  )
}

# Promoção canônica para 6-images-tables/final/
finalizar_figura(
  plot        = p_fig_abs,
  fig_label   = "matriculas-turno-via-acesso",
  fig_cap     = "Enrolment by sector, access route and time of day, Brazil, 2010--2024.",
  fonte       = "INEP — SEDAP (Plataforma IDE / RNP), microdata (2010–2024)",
  nota        = paste(
    "Each year's column is the whole system, split three ways at once: sector (public or private),",
    "the route through which the seat was reached (open admission, reserved seats, FIES, ProUni),",
    "and time of day (daytime, evening, distance). Hue encodes sector and route; within each hue,",
    "lightness encodes time of day --- except distance learning, which is given a colour family of",
    "its own rather than a darker shade. The legend is a map of that two-way scheme. Labels are",
    "enrolment counts in millions. Reconstructed 100% from INEP SEDAP API microdata (2010--2024)."
  ),
  apendice    = "sec-fignote-matriculas-turno-via-acesso",
  script_path = here::here("4-DA-Code", "2026-08_SEDAP", "050_Plot_Nicho_100pct_SEDAP.R"),
  largura     = LARGURA_TEXTO, altura = ALTURA_FIG
)

cat("Figura 050 (100% SEDAP, 2010-2024) gerada com sucesso e promovida em 6-images-tables/final/\n")
