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
                       "Associa\u00e7\u00e3o territorial: n\u00e3o identifica a renda individual dos eleitores.",
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

# Additional figures use computed diagnostics only. Permit the original plot
# alone when redrawing an older snapshot that predates the audit.
metric <- function(name) {
  candidate <- file.path(dirname(path), name)
  if (file.exists(candidate)) fread(candidate, encoding = "UTF-8") else NULL
}
fixed <- metric("sensibilidade-cortes-fixos.csv")
methods <- list(original = metric("centis-originais-comparacao.csv"),
                fixed = fixed, weighted = metric("centis-ponderados.csv"),
                ratio = metric("centis-razao.csv"))
linkage <- metric("sensibilidade-ligacao.csv")
if (!is.null(linkage)) methods$idw3 <- linkage[regra == "idw3"]
if (!is.null(fixed) && all(vapply(methods, Negate(is.null), logical(1)))) {
  for (language in c("pt", "en")) {
    is_pt <- language == "pt"
    distance_labels <- ifelse(is.infinite(fixed$distancia_maxima_m),
      if (is_pt) "Sem limite" else "Unlimited", paste0(fixed$distancia_maxima_m / 1000, " km"))
    fixed_long <- melt(copy(fixed)[, limite := distance_labels],
      id.vars = c("centil", "limite"), measure.vars = c("lula", "flavio"),
      variable.name = "candidato", value.name = "votacao")
    fixed_long[, candidato := ifelse(candidato == "lula", "Lula (PT)", "Fl\u00e1vio Bolsonaro (PL)")]
    sensitivity_plot <- ggplot(fixed_long, aes(centil, votacao, colour = limite)) +
      geom_line(linewidth = .6, na.rm = TRUE) + facet_wrap(~candidato) +
      labs(x = if (is_pt) "Centil de renda (cortes fixos)" else "Income percentile (fixed cutoffs)",
        y = if (is_pt) "Vota\u00e7\u00e3o estimada (%)" else "Estimated vote share (%)",
        colour = if (is_pt) "Dist\u00e2ncia" else "Distance",
        caption = if (is_pt) "Mesmo universo e mesmos cortes em todos os limites; setores sem dados ficam ausentes. Associa\u00e7\u00e3o territorial." else
          "Same universe and cutoffs at every limit; tracts without data remain missing. Territorial association.") +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    ggsave(file.path(out, paste0("sensibilidade-cortes-fixos-", language, ".png")),
      sensitivity_plot, width = 10, height = 5.2, dpi = 150, bg = "white")
    labels <- if (is_pt) c(original = "Original: centis ap\u00f3s corte", fixed = "Centis fixos",
      weighted = "Peso por respons\u00e1veis", ratio = "Raz\u00e3o de votos alocados", idw3 = "Tr\u00eas vizinhos IDW") else
      c(original = "Original: ranks after cutoff", fixed = "Fixed percentiles",
        weighted = "Weighted by household heads", ratio = "Allocated vote ratio", idw3 = "Three-neighbour IDW")
    comparisons <- rbindlist(lapply(names(methods), function(method) {
      z <- copy(methods[[method]])[distancia_maxima_m == 5000]
      z[, metodo := labels[[method]]]
      z[, .(centil, metodo, lula, flavio)]
    }))
    comparison_long <- melt(comparisons, id.vars = c("centil", "metodo"),
      measure.vars = c("lula", "flavio"), variable.name = "candidato", value.name = "votacao")
    comparison_long[, candidato := ifelse(candidato == "lula", "Lula (PT)", "Fl\u00e1vio Bolsonaro (PL)")]
    comparison_plot <- ggplot(comparison_long, aes(centil, votacao, colour = metodo)) +
      geom_line(linewidth = .6, na.rm = TRUE) + facet_wrap(~candidato) +
      labs(x = if (is_pt) "Centil de renda" else "Income percentile",
        y = if (is_pt) "Vota\u00e7\u00e3o estimada (%)" else "Estimated vote share (%)", colour = NULL,
        caption = if (is_pt) "Limite de 5 km. Os centis ponderados e originais representam grupos diferentes; comparar o formato, n\u00e3o os mesmos eleitores." else
          "5 km limit. Weighted and original percentiles represent different groups; compare shapes, not the same voters.") +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    ggsave(file.path(out, paste0("comparacao-metodos-", language, ".png")),
      comparison_plot, width = 10, height = 5.5, dpi = 150, bg = "white")
  }
}
message("Figure saved in output/figures/votacao-renda-centil/. Promote thumbnail only after reviewing coverage.")
