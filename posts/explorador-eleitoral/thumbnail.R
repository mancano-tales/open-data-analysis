# Static preview only; the interactive explorer includes all certified candidates.
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
suppressPackageStartupMessages({library(ggplot2); library(jsonlite)})
snapshot <- read_json("posts/explorador-eleitoral/data/series/2026-1-1-BR.json", simplifyVector = TRUE)
stopifnot(snapshot$metadata$vote_audit$status == "reconciled", snapshot$metadata$vote_audit$difference == 0)
points <- subset(snapshot$points, candidate_id %in% c("13", "22"))
points$candidate <- ifelse(points$candidate_id == "13", "Lula", "Flávio Bolsonaro")
labels <- subset(points, centil == 100)
figure <- ggplot(points, aes(centil, votacao, color = candidate)) +
  geom_line(linewidth = 1) +
  geom_text(data = labels, aes(label = candidate), hjust = 0, nudge_x = 1.5,
            size = 3.4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Lula" = "#e83f66", "Flávio Bolsonaro" = "#b48718")) +
  scale_x_continuous(position = "top", breaks = c(1,20,40,60,80,100), limits = c(1,120)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  labs(title = "Votação presidencial por renda dos setores",
    subtitle = "1º turno de 2026 · centis nacionais fixos do Censo 2022",
    x = "Setores por renda: mais pobres → mais ricos", y = "Votação estimada (%)",
    caption = "TSE e Censo 2022 (IBGE), via geobr e censobr. Estimativa espacial independente.\nMédia simples por setor; local mais próximo no mesmo município, até 5 km.") +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#ececec"),
    plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0),
    plot.margin = margin(16,20,14,14), axis.title = element_text(face = "bold"))
ggsave("posts/explorador-eleitoral/thumbnail.png", figure, width = 10, height = 5.6,
       dpi = 180, bg = "white")
