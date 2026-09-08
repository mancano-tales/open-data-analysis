# 300_educabr2_appendix_figures.R
#
# Figuras do Apêndice D (pacote educabr2) — conjunto redesenhado em 2026-07-11
# ("repensar do zero", decisão do autor) segundo o catálogo Healy (socviz 2ª ed.):
# um achado por figura; rotulagem direta no fim das linhas em vez de legenda
# (Gestalt/proximidade, cap. 5/8); Layer-Highlight-Repeat para séries com
# protagonista (cap. 8); anotação parcimoniosa de marcos apenas quando o marco
# É o achado; ribbons materializando gaps; títulos-frase vivem no fig-cap/
# fignote do .qmd, nunca no canvas (plot_titles = FALSE).
#
# Estética: sistema visual do próprio pacote (theme_educabr, Okabe-Ito,
# scale_x_year_educabr) — identidade de software paper. plot_theme.R entra
# para finalizar_figura() e dimensões A4.
#
# Uso: Rscript 4-DA-Code/2026-07_educabr2-appendix/300_educabr2_appendix_figures.R

suppressPackageStartupMessages({
  library(educabr2)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))

script_path <- here::here("4-DA-Code", "2026-07_educabr2-appendix", "300_educabr2_appendix_figures.R")

# Cores semânticas fixas (Okabe-Ito) usadas em todo o conjunto
COR_SERIE    <- "#0072B2"  # série protagonista / azul
COR_CONTEXTO <- "grey62"   # fontes de fundo
COR_MULHER   <- "#E69F00"  # laranja
COR_HOMEM    <- "#56B4E9"  # azul-céu
COR_SP       <- "#56B4E9"
COR_BA       <- "#E69F00"

# ──────────────────────────────────────────────────────────────────────────────
# D1: Expansão secular do ensino superior — série harmonizada destacada sobre
#     as fontes concorrentes (1907-2024), eixo log10
# ──────────────────────────────────────────────────────────────────────────────
cat("Gerando D1: expansão secular multi-fonte...\n")

df_es <- get_enrollment(
  level = "superior", network = "total", institution_type = "total",
  modality = "total", indicator = "count", include_derived = FALSE, lang = "en"
) %>%
  filter(source %in% c("ibge_seculo_xx", "durham_2005", "maduro_junior_2007",
                       "kang_paese_felix_2021", "inep_sinopse_censup",
                       "inep_microdados_censup")) %>%
  distinct(year, source, value, .keep_all = TRUE)

# Série harmonizada: hierarquia de deduplicação documentada do pacote
precedencia <- c(inep_microdados_censup = 1, inep_sinopse_censup = 2,
                 kang_paese_felix_2021 = 3, maduro_junior_2007 = 4,
                 durham_2005 = 5, ibge_seculo_xx = 6)
df_spliced <- df_es %>%
  mutate(prec = precedencia[source]) %>%
  group_by(year) %>% slice_min(prec, n = 1, with_ties = FALSE) %>% ungroup() %>%
  arrange(year)

p1 <- ggplot() +
  geom_point(data = df_es, aes(x = year, y = value),
             colour = COR_CONTEXTO, size = 1.0, alpha = 0.55) +
  geom_line(data = df_spliced, aes(x = year, y = value),
            colour = COR_SERIE, linewidth = 0.9) +
  annotate("text", x = 1990, y = 210000, label = "Harmonized series",
           colour = COR_SERIE, size = 3.1, hjust = 1, family = "serif") +
  annotate("segment", x = 1991, xend = 1999, y = 260000, yend = 990000,
           colour = COR_SERIE, linewidth = 0.25) +
  annotate("text", x = 1962, y = 3.2e6, label = "Grey points: all competing sources",
           colour = "grey40", size = 3.1, hjust = 0.5, family = "serif") +
  scale_x_year_educabr(df_es$year) +
  scale_y_log10(breaks = c(1e4, 1e5, 1e6, 1e7),
                labels = c("10 thousand", "100 thousand", "1 million", "10 million")) +
  theme_educabr(plot_titles = FALSE) +
  labs(x = "Year", y = "Enrollments (log scale)")

finalizar_figura(
  plot = p1,
  fig_label = "educabr2-enrollment-sources",
  fig_cap = "Tertiary education enrollment in Brazil, 1907-2024: harmonized series and competing sources.",
  fonte = "educabr2 R package (v0.1.0), drawing from IBGE, Durham, Maduro Jr., Kang et al., and INEP.",
  nota = "The blue line splices the six sources by the package's deduplication hierarchy (most disaggregated official source wins); grey points show every competing estimate. Their overlap makes the cross-source agreement, and the few divergent stretches, directly visible. The vertical axis is logarithmic: enrollment grew roughly a thousandfold across the century.",
  script_path = script_path
)

# ──────────────────────────────────────────────────────────────────────────────
# D2: O subregistro do EAD e os totais reconstruídos (2000-2008)
# ──────────────────────────────────────────────────────────────────────────────
cat("Gerando D2: totais reconstruídos EAD...\n")

# A partir de 2000, o "total" publicado pela sinopse é rotulado
# modality = "presencial" (o EAD ficava em tabela separada) — este É o
# subregistro que a figura documenta. Original = presencial publicado;
# reconstruído = linha derivada (presencial + EAD).
df_recon <- get_enrollment(
  level = "superior", year = c(2000, 2008), network = "total",
  institution_type = "total", modality = c("total", "presencial"),
  indicator = "count", include_derived = TRUE, lang = "en"
) %>%
  filter(source %in% c("inep_sinopse_censup",
                       "inep_sinopse_censup+inep_sinopse_censup")) %>%
  distinct(year, source, value, .keep_all = TRUE) %>%
  mutate(serie = if_else(is_derived, "reconstructed", "original")) %>%
  group_by(year, serie) %>% slice_max(value, n = 1, with_ties = FALSE) %>% ungroup()

df_recon_wide <- df_recon %>%
  select(year, serie, value) %>%
  pivot_wider(names_from = serie, values_from = value)

gap_2008 <- with(df_recon_wide[df_recon_wide$year == 2008, ],
                 reconstructed - original)

p2 <- ggplot(df_recon_wide, aes(x = year)) +
  geom_ribbon(aes(ymin = original / 1e6, ymax = reconstructed / 1e6),
              fill = "#E69F00", alpha = 0.25) +
  geom_line(aes(y = original / 1e6), colour = COR_SERIE, linewidth = 0.9) +
  geom_line(aes(y = reconstructed / 1e6), colour = COR_SERIE,
            linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = original / 1e6), colour = COR_SERIE, size = 1.6) +
  geom_point(aes(y = reconstructed / 1e6), colour = COR_SERIE, size = 1.6,
             shape = 21, fill = "white") +
  annotate("text", x = 2007.9, y = df_recon_wide$original[df_recon_wide$year == 2008] / 1e6 - 0.13,
           label = "Published total\n(in-person only)", hjust = 1, vjust = 1,
           size = 3.0, colour = COR_SERIE, family = "serif", lineheight = 0.95) +
  annotate("text", x = 2006.6, y = df_recon_wide$reconstructed[df_recon_wide$year == 2007] / 1e6 + 0.28,
           label = "Reconstructed total\n(+ separately-tracked EAD)", hjust = 1, vjust = 0,
           size = 3.0, colour = COR_SERIE, family = "serif", lineheight = 0.95) +
  annotate("text", x = 2008.05, y = mean(unlist(df_recon_wide[df_recon_wide$year == 2008, c("original", "reconstructed")])) / 1e6,
           label = sprintf("+%s", format(round(gap_2008 / 1e3) * 1e3, big.mark = ",")),
           hjust = 0, size = 3.0, colour = "#B07800", family = "serif") +
  scale_x_continuous(breaks = 2000:2008,
                     expand = expansion(mult = c(0.03, 0.12))) +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  theme_educabr(plot_titles = FALSE) +
  labs(x = "Year", y = "Enrollments (millions)")

finalizar_figura(
  plot = p2,
  fig_label = "educabr2-enrollment-reconstructed",
  fig_cap = "The distance-education undercount and the reconstructed tertiary totals, 2000-2008.",
  fonte = "educabr2 R package, combining the INEP CENSUP in-person and distance-learning tables.",
  nota = "Between 2000 and 2008 INEP tracked distance enrollment (EAD) in a separate table and excluded it from published headline totals. The shaded band is the resulting undercount, which grows to roughly 700 thousand students by 2008; the dashed line shows the package's reconstructed totals (is\\_derived = TRUE).",
  script_path = script_path
)

# ──────────────────────────────────────────────────────────────────────────────
# D3: Reversão do hiato de gênero na escolaridade (1925-2015)
# ──────────────────────────────────────────────────────────────────────────────
cat("Gerando D3: reversão de gênero...\n")

df_sch <- get_schooling(geo_level = "BR", dimension = "sex", lang = "en") %>%
  select(year, dim_sex, value) %>%
  pivot_wider(names_from = dim_sex, values_from = value)

ano_cruz <- min(df_sch$year[df_sch$female > df_sch$male], na.rm = TRUE)
val_cruz <- df_sch$female[df_sch$year == ano_cruz]

p3 <- ggplot(df_sch, aes(x = year)) +
  geom_ribbon(aes(ymin = pmin(female, male), ymax = pmax(female, male),
                  fill = female > male), alpha = 0.20, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = COR_MULHER, `FALSE` = COR_HOMEM)) +
  geom_line(aes(y = female), colour = COR_MULHER, linewidth = 0.9) +
  geom_line(aes(y = male),   colour = COR_HOMEM,  linewidth = 0.9) +
  annotate("point", x = ano_cruz, y = val_cruz, size = 2.2, colour = "grey25") +
  annotate("text", x = ano_cruz - 2, y = val_cruz + 0.85,
           label = sprintf("Women overtake men (%d)", ano_cruz),
           hjust = 1, size = 3.1, colour = "grey25", family = "serif") +
  annotate("text", x = max(df_sch$year) + 1.5,
           y = df_sch$female[df_sch$year == max(df_sch$year)],
           label = "Women", colour = COR_MULHER, hjust = 0, size = 3.2, family = "serif") +
  annotate("text", x = max(df_sch$year) + 1.5,
           y = df_sch$male[df_sch$year == max(df_sch$year)] - 0.15,
           label = "Men", colour = COR_HOMEM, hjust = 0, size = 3.2, family = "serif") +
  scale_x_year_educabr(df_sch$year, expand = expansion(mult = c(0.03, 0.10))) +
  theme_educabr(plot_titles = FALSE) +
  labs(x = "Year", y = "Mean years of schooling")

finalizar_figura(
  plot = p3,
  fig_label = "educabr2-schooling-sex",
  fig_cap = "Average years of schooling of the population aged 15-64 in Brazil by sex, 1925-2015.",
  fonte = "educabr2 R package, compiled from Walter and Kang (2024).",
  nota = "The shaded band is the schooling gap between the sexes; its colour switches when the sign flips. Brazilian women moved from a persistent schooling deficit to a durable advantage over the twentieth century — the crossover is marked directly on the series.",
  script_path = script_path
)

# ──────────────────────────────────────────────────────────────────────────────
# D4: Queda da regressividade fiscal — razão dupla ES/EF1 (1933-2010), log10
# ──────────────────────────────────────────────────────────────────────────────
cat("Gerando D4: regressividade fiscal...\n")

df_exp <- get_expenditure(indicator = "double_ratio_es_ef1", lang = "en") %>%
  arrange(year)
v_ini <- df_exp$value[1];              a_ini <- df_exp$year[1]
v_fim <- df_exp$value[nrow(df_exp)];   a_fim <- df_exp$year[nrow(df_exp)]

p4 <- ggplot(df_exp, aes(x = year, y = value)) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey45") +
  annotate("text", x = 2009, y = 1.28, label = "Equal spending per student",
           hjust = 1, size = 2.9, colour = "grey45", family = "serif") +
  geom_line(colour = COR_SERIE, linewidth = 0.8) +
  geom_point(colour = COR_SERIE, size = 1.1) +
  annotate("text", x = a_ini + 1.5, y = v_ini * 1.35,
           label = sprintf("%.0f× (%d)", v_ini, a_ini),
           hjust = 0, size = 3.2, colour = COR_SERIE, family = "serif") +
  annotate("text", x = a_fim - 1.5, y = v_fim * 0.72,
           label = sprintf("%.0f× (%d)", v_fim, a_fim),
           hjust = 1, size = 3.2, colour = COR_SERIE, family = "serif") +
  scale_x_year_educabr(df_exp$year) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100),
                labels = c("1×", "3×", "10×", "30×", "100×")) +
  theme_educabr(plot_titles = FALSE) +
  labs(x = "Year", y = "Tertiary / primary spending per student (log scale)")

finalizar_figura(
  plot = p4,
  fig_label = "educabr2-expenditure-regressivity",
  fig_cap = "Double ratio of per-student public spending, tertiary over primary education, Brazil, 1933-2010.",
  fonte = "educabr2 R package, compiled from Kang and Menetrier (2024).",
  nota = "The double ratio divides public spending per tertiary student by spending per pupil in early primary education (EF1); the dotted reference line marks equal per-student spending. On the log scale, the fall from 66\\texttimes{} in 1933 to under 9\\texttimes{} in 2010 spans nearly an order of magnitude, yet spending never approached parity — the elitist bias diagnosed by Kang and Menetrier (2024), in the tradition of Lindert's indicators, narrowed without disappearing.",
  script_path = script_path
)

# ──────────────────────────────────────────────────────────────────────────────
# D5: Desigualdade regional na progressão escolar — GDR6, SP vs BA (1955-2010)
# ──────────────────────────────────────────────────────────────────────────────
cat("Gerando D5: GDR6 SP vs BA...\n")

df_prog <- get_progression(geo_level = "UF", geo = c("SP", "BA"), lang = "en") %>%
  select(year, geo_code, value) %>%
  pivot_wider(names_from = geo_code, values_from = value) %>%
  filter(!is.na(SP) & !is.na(BA))

ano_gap <- df_prog$year[which.max(df_prog$SP - df_prog$BA)]
gap_max <- max(df_prog$SP - df_prog$BA)

p5 <- ggplot(df_prog, aes(x = year)) +
  geom_ribbon(aes(ymin = BA, ymax = SP), fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = SP), colour = COR_SP, linewidth = 0.9) +
  geom_line(aes(y = BA), colour = COR_BA, linewidth = 0.9) +
  annotate("text", x = max(df_prog$year) + 1,
           y = df_prog$SP[df_prog$year == max(df_prog$year)],
           label = "São Paulo", colour = COR_SP, hjust = 0, size = 3.2, family = "serif") +
  annotate("text", x = max(df_prog$year) + 1,
           y = df_prog$BA[df_prog$year == max(df_prog$year)],
           label = "Bahia", colour = COR_BA, hjust = 0, size = 3.2, family = "serif") +
  annotate("segment", x = ano_gap, xend = ano_gap,
           y = df_prog$BA[df_prog$year == ano_gap] + 0.015,
           yend = df_prog$SP[df_prog$year == ano_gap] - 0.015,
           colour = "grey35", linewidth = 0.3,
           arrow = arrow(ends = "both", length = unit(0.12, "cm"))) +
  annotate("text", x = ano_gap - 1.5,
           y = mean(c(df_prog$SP[df_prog$year == ano_gap], df_prog$BA[df_prog$year == ano_gap])),
           label = sprintf("Widest gap (%d)", ano_gap),
           hjust = 1, size = 2.9, colour = "grey35", family = "serif") +
  scale_x_year_educabr(df_prog$year, expand = expansion(mult = c(0.03, 0.12))) +
  theme_educabr(plot_titles = FALSE) +
  labs(x = "Year", y = "GDR6 ratio")

finalizar_figura(
  plot = p5,
  fig_label = "educabr2-progression-gdr6",
  fig_cap = "Grade progression ratio (GDR6) in São Paulo and Bahia, 1955-2010.",
  fonte = "educabr2 R package, compiled from Kang, Paese and Felix (2021).",
  nota = "GDR6 proxies early-primary retention (enrollment in grades 4-6 relative to grades 1-3). The grey band is the S\\~ao Paulo--Bahia gap: it widens through the developmentalist decades and only narrows substantially after the 1990s reforms — half a century of regional educational inequality in a single band.",
  script_path = script_path
)

# ──────────────────────────────────────────────────────────────────────────────
# D6: Mapa de cobertura das fontes — por que o educabr2 existe
# ──────────────────────────────────────────────────────────────────────────────
cat("Gerando D6: mapa de cobertura das fontes...\n")

fontes_pacote <- c("ibge_seculo_xx", "durham_2005", "maduro_junior_2007",
                   "kang_paese_felix_2021", "kang_menetrier_2024",
                   "kang_menetrier_comim_2024", "walter_kang_2023",
                   "inep_sinopse_censup", "inep_microdados_censup",
                   "inep_censup_powerbi", "lee_lee_2016")

src <- list_sources() %>%
  filter(key %in% fontes_pacote) %>%
  mutate(
    year_end = ifelse(is.na(year_end), 2024L, year_end),
    tipo = case_when(
      key %in% c("inep_sinopse_censup", "inep_microdados_censup",
                 "inep_censup_powerbi") ~ "INEP registers",
      key == "ibge_seculo_xx" ~ "IBGE compilations",
      TRUE ~ "Academic reconstructions"
    ),
    rotulo = case_when(
      key == "walter_kang_2023" ~ "Walter & Kang (2024)",
      TRUE ~ short_name
    )
  ) %>%
  arrange(year_start, year_end) %>%
  mutate(rotulo = factor(rotulo, levels = rev(unique(rotulo))))

p6 <- ggplot(src, aes(y = rotulo)) +
  geom_segment(aes(x = year_start, xend = year_end, yend = rotulo, colour = tipo),
               linewidth = 3.4, lineend = "butt") +
  scale_colour_manual(name = NULL,
                      values = c("Academic reconstructions" = "#E69F00",
                                 "INEP registers"           = "#0072B2",
                                 "IBGE compilations"        = "#009E73")) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_year_educabr(c(src$year_start, src$year_end)) +
  theme_educabr(plot_titles = FALSE) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8)) +
  labs(x = "Year", y = NULL)

finalizar_figura(
  plot = p6,
  fig_label = "educabr2-source-coverage",
  fig_cap = "Temporal coverage of the eleven sources harmonized by educabr2, 1870-2024.",
  fonte = "educabr2 R package, inst/dict/vocabularies/sources.yaml.",
  nota = "Each bar spans the years covered by one bundled source. No single source covers the century: official registers begin only in 1995, historical compilations stop in 1980, and the gap between them is bridged by academic reconstructions. Harmonizing these overlapping, partial series into one continuous panel is the package's reason to exist.",
  script_path = script_path
)

cat("✓ Conjunto D1-D6 gerado.\n")
