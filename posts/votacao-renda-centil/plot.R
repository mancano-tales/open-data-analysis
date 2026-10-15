# Plot only observed output from the spatial pipeline; never trace the screenshot.
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
})
path <- here("data-raw", "eleicoes-renda", "centis.csv")
if (!file.exists(path)) path <- here("posts", "votacao-renda-centil", "data", "centis.csv")
if (!file.exists(path)) stop("Run shared-pipeline/eleicoes/020_Cruzar_Setores_Votos.R first.")
d <- fread(path, encoding = "UTF-8")
stopifnot(all(c("centil", "candidato", "votacao", "renda_sm", "n_setores") %in% names(d)),
          all(is.finite(d$votacao)), all(d$votacao >= 0 & d$votacao <= 100),
          all(d$centil %in% 1:100), !anyDuplicated(d[, .(centil, candidato)]))
colors <- setNames(c("#E83D67", "#B3871A"), c("Lula (PT)", "Fl\u00e1vio Bolsonaro (PL)"))
if (!setequal(unique(d$candidato), names(colors))) stop("Unexpected candidate labels.")
ends <- d[centil == max(centil)]
ends[, label := fifelse(candidato == "Fl\u00e1vio Bolsonaro (PL)", "Fl\u00e1vio\nBolsonaro (PL)", candidato)]
p <- ggplot(d, aes(centil, votacao, colour = candidato, group = candidato)) +
  geom_line(linewidth = 0.8) +
  geom_text(data = ends, aes(x = 101.5, label = label), hjust = 0,
            fontface = "bold", size = 3.8, lineheight = 0.95) +
  scale_colour_manual(values = colors, guide = "none") +
  scale_x_continuous(position = "top", breaks = seq(0, 100, 10),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = seq(0, 80, 20), limits = c(0, 90), expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  labs(x = "Grupos por renda (mais pobres \u2192 mais ricos)", y = "Vota\u00e7\u00e3o (%)",
       caption = paste("Centis de setores pela renda m\u00e9dia dos respons\u00e1veis com rendimentos (Censo 2022).",
                       "Vota\u00e7\u00e3o estimada por proximidade dos locais de vota\u00e7\u00e3o; m\u00e9dia dos percentuais setoriais.",
                       "Fonte: elabora\u00e7\u00e3o pr\u00f3pria com TSE e IBGE (censobr/geobr). M\u00e9todo espacial pr\u00f3prio; ver post.", sep = "\n")) +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "#EEEEEE"),
        axis.title = element_text(face = "bold", colour = "#555555"),
        axis.title.x = element_text(margin = margin(b = 12)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(colour = "#666666"),
        plot.caption = element_text(hjust = 0, colour = "#555555", margin = margin(t = 20)),
        plot.margin = margin(15, 135, 12, 15))
out <- here("output", "figures", "votacao-renda-centil")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out, "votacao-renda-centil.png"), p, width = 9.2, height = 5.8, dpi = 150, bg = "white")
ggsave(file.path(out, "votacao-renda-centil.pdf"), p, width = 9.2, height = 5.8, device = cairo_pdf)
message("Figure saved in output/figures/votacao-renda-centil/. Promote thumbnail only after reviewing coverage.")
