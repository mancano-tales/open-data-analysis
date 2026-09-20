# ==============================================================================
# SCRIPT: 097G_Tese_Wagstaff_Transicao_Condicional.R
# ODA: portado de 6-images-tables/final/wagstaff-transicao-condicional/2026-09-16_1649_097G_Tese_Wagstaff_Transicao_Condicional.R
#      em 2026-09-20; caminhos relativos a data-raw/.
#
# Versao para dissertacao do painel A do diagnostico 097F, restrita a 18-24
# anos (pedido do autor, 2026-09-16). Tres series de Wagstaff, 1992-2025:
#   1. conclusao do ensino medio (medio_completo)            -- o funil
#   2. acesso ao ensino superior (ens_sup, ingressou alguma vez) -- a serie da 097D
#   3. acesso ao superior ENTRE quem concluiu o medio, com o rank de renda
#      recomputado dentro dos elegiveis -- a transicao condicional
# A distancia entre 2 e 3 e a contribuicao do funil do EM para a desigualdade
# de acesso ao superior; 3 sozinha e o que restou de seletividade por renda
# no proprio degrau terciario (ver 097F para a decomposicao exata e o
# raciocinio completo).
#
# O QUE E IGUAL AO 097D (copia, sem alterar): banner de governos, escala X
# compartilhada (scale_x_anos_tese), ribbons de IC 95% em cinza, legenda
# unica na base com cor + tracejado + simbolo. SEM o painel B de Gini
# (decisao do autor, 2026-09-16: "vamos sem o Gini nessa versao") -- layout
# banner + painel unico, heights = c(1, 11) e LARGURA_TEXTO x ALTURA_ALTA,
# exatamente o que a 097D usava antes de ganhar o painel B (commit d81c5f2^).
#
# O QUE MUDA:
#   - Fonte dos numeros: cache do 097F (output/097F_diag_wagstaff_transicao_
#     decomposicao.rds), que ja contem as tres series com SE pelo mesmo
#     estimador do 097D (GLM ponderado, Kish DEFF = 2.0). Este script NAO
#     recalcula; se o cache nao existir, para e manda rodar o 097F -- mesmo
#     padrao da dependencia 097D -> 238.
#   - Codificacao visual: cor = desfecho (nao mais faixa etaria, ja que so ha
#     uma). Azul-ceu (PALETA[2]) para acesso ao superior, a MESMA cor que a
#     serie 18-24 tem na 097D, para que a serie identica leia igual nas duas
#     figuras; laranja (PALETA[1]) para conclusao do EM, como na 097E; verde
#     (PALETA[3]) para a condicional. Tracejado so na condicional, marcando
#     que e uma serie derivada (subpopulacao), nao um desfecho da populacao.
#
# RESSALVAS (fignote): condicionar a conclusao do EM e condicionar a um
# desfecho que depende da renda; a composicao dos elegiveis muda muito
# (17,6% -> 73,6% da coorte), entao a serie 3 mistura mudanca na transicao
# com mudanca em quem e elegivel -- descritiva, nao causal. 1992-1995: ~21%
# de NA em anos de estudo (NA => nao concluiu) contamina o denominador da
# condicional; ler com cautela. Emenda 2015/2016 visivel como degrau.
#
# VER TAMBEM: 097D (acesso/matricula), 097E (conclusao do EM, 2 faixas),
#             097F (diagnostico completo, 2 faixas, decomposicao)
# ==============================================================================

MOSTRAR_TITULO <- FALSE
MOSTRAR_FONTE  <- FALSE

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(patchwork)

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 14))

BASE_DIR <- here::here("data-raw", "harmonizing-br-data")

# ==============================================================================
# 1. DADOS -- cache do 097F, so 18-24, so W
# ==============================================================================
CACHE_097F <- file.path(BASE_DIR, "output", "097F_diag_wagstaff_transicao_decomposicao.rds")
if (!file.exists(CACHE_097F)) {
  stop("Cache do 097F nao encontrado em data-raw/. Rode antes: shared-pipeline/pnadc/097F_Diag_Wagstaff_Transicao_Decomposicao.R", call. = FALSE)
}
res <- readRDS(CACHE_097F)

NIVEIS_SERIE <- c(
  "Upper secondary completion (all)",
  "Tertiary access (all)",
  "Tertiary access | completed US"
)
data_lines <- res |>
  filter(faixa == "Ages 18-24", serie %in% NIVEIS_SERIE) |>
  transmute(ano, serie_w = factor(serie, levels = NIVEIS_SERIE),
            mu, valor = W, se = se_W,
            lower = W - 1.96 * se_W, upper = W + 1.96 * se_W) |>
  arrange(serie_w, ano)

ANO_MAX_SERIE <- max(data_lines$ano, na.rm = TRUE)
cat(sprintf("Serie cobre ate %d\n", ANO_MAX_SERIE))

cat("\nSANIDADE -- W (18-24) em anos-marco:\n")
print(data_lines |>
        filter(ano %in% c(1992L, 2002L, 2014L, 2019L, ANO_MAX_SERIE)) |>
        mutate(across(c(mu, valor, se), ~ round(.x, 3))) |>
        select(ano, serie_w, mu, W = valor, se_W = se) |>
        pivot_wider(names_from = serie_w, values_from = c(mu, W, se_W)) |>
        as.data.frame(),
      row.names = FALSE)

# ==============================================================================
# 2. BANNER DE GOVERNO -- identico ao 097D
# ==============================================================================
FIM_DADOS <- ANO_MAX_SERIE
ANOS_EIXO <- ANOS_TRANSICAO_GOV

GOV <- data.frame(
  nome      = NOMES_GOV_CURTO,
  xband_min = ANOS_EIXO,
  xband_max = c(ANOS_EIXO[-1], FIM_DADOS),
  stringsAsFactors = FALSE
)
GOV$x_label     <- (GOV$xband_min + GOV$xband_max) / 2
GOV$hjust_label <- 0.5
idx_ultimo <- nrow(GOV)
GOV$x_label[idx_ultimo]     <- GOV$xband_min[idx_ultimo]
GOV$hjust_label[idx_ultimo] <- 0
GOV$seg_min <- GOV$xband_min + 0.08
GOV$seg_max <- GOV$xband_max - 0.08

scale_x_compartilhada <- scale_x_anos_tese(
  anos   = c(ANOS_EIXO, FIM_DADOS),
  expand = expansion(mult = c(0.015, 0), add = c(0, 1.8))
)

TAMANHO_EIXO_PT <- 8
TAMANHO_EIXO_MM <- TAMANHO_EIXO_PT / .pt

p_banner <- ggplot(GOV) +
  geom_segment(aes(x = seg_min, xend = seg_max, y = 0.12, yend = 0.12),
               colour = "grey75", linewidth = 0.5) +
  geom_text(data = GOV[-idx_ultimo, ], aes(x = x_label, y = 0.55, label = nome),
            hjust = 0.5, size = TAMANHO_EIXO_MM, fontface = "bold",
            colour = "#222222", family = THESIS_FONT) +
  geom_text(data = GOV[idx_ultimo, ], aes(x = x_label, y = 0.55, label = nome),
            hjust = 0, size = TAMANHO_EIXO_MM, fontface = "bold",
            colour = "#222222", family = THESIS_FONT) +
  scale_x_compartilhada +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = THESIS_FONT) +
  theme(plot.margin = margin(2, 5.5, 1, 5.5))

# ==============================================================================
# 3. PAINEL A -- tres series, 18-24
# ==============================================================================
LBL_SERIE_W <- c(
  "Upper secondary completion (all)" = "Completed upper secondary",
  "Tertiary access (all)"            = "Entered tertiary",
  "Tertiary access | completed US"   = "Entered tertiary, among completers"
)
COR_SERIE_W <- setNames(c(PALETA_QUALITATIVA[[1]], PALETA_QUALITATIVA[[2]],
                          PALETA_QUALITATIVA[[3]]), NIVEIS_SERIE)
LTY_SERIE_W <- setNames(c("solid", "solid", "dashed"), NIVEIS_SERIE)
SHP_SERIE_W <- setNames(c(16, 17, 15), NIVEIS_SERIE)

p_main <- ggplot() +
  geom_ribbon(data = data_lines, aes(x = ano, ymin = lower, ymax = upper, group = serie_w),
              fill = "grey50", alpha = 0.3) +
  geom_line(data = data_lines,
            aes(x = ano, y = valor, colour = serie_w, linetype = serie_w),
            linewidth = 1.0) +
  geom_point(data = data_lines,
             aes(x = ano, y = valor, colour = serie_w, shape = serie_w),
             size = 2.4) +
  scale_colour_manual(name = NULL, values = COR_SERIE_W, labels = LBL_SERIE_W) +
  scale_linetype_manual(name = NULL, values = LTY_SERIE_W, labels = LBL_SERIE_W) +
  scale_shape_manual(name = NULL, values = SHP_SERIE_W, labels = LBL_SERIE_W) +
  # Linha unica (2026-09-16, a pedido do autor). Descartadas antes: 2x2 como
  # a 097D (a largura de cada coluna e ditada pelo rotulo mais longo dela, e
  # o rotulo da condicional sozinho na segunda linha abria um vao entre as
  # chaves da primeira) e coluna unica de 3 linhas (custa altura). Para as
  # tres caberem em LARGURA_TEXTO o rotulo da condicional foi encurtado de
  # "among upper secondary completers" para "among completers" -- a fignote
  # define o grupo por extenso.
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_compartilhada +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = "Wagstaff index (W)",
    title    = if (MOSTRAR_TITULO) "Where does income select? Upper secondary funnel vs tertiary transition, ages 18-24" else NULL,
    subtitle = if (MOSTRAR_TITULO) "Grey bands = 95% CIs. Top banner = government term timeline." else NULL,
    caption  = if (MOSTRAR_FONTE) "Calculated via complex survey GLM regression (Kish DEFF=2.0)." else NULL
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.7, "lines"),
    legend.spacing.x = unit(0.15, "cm"),
    plot.margin      = margin(6, 5.5, 2, 5.5)
  )

# ==============================================================================
# 4. COMPOSICAO -- banner + painel unico (sem Gini), como a 097D pre-d81c5f2
# ==============================================================================
p_combined <- p_banner / p_main + plot_layout(heights = c(1, 11))

ALTURA_FIG <- ALTURA_ALTA
salvar_grafico(p_combined, prefixo = "097G_Tese_Wagstaff_Transicao_Condicional",
               largura = LARGURA_TEXTO, altura = ALTURA_FIG, unidades = "in",
               formato = "png")
salvar_grafico(p_combined, prefixo = "097G_Tese_Wagstaff_Transicao_Condicional",
               largura = LARGURA_TEXTO, altura = ALTURA_FIG, unidades = "in",
               formato = "pdf")

# ------------------------------------------------------------------------------
# PROMOVER = TRUE (2026-09-16, aprovado pelo autor apos legenda em linha unica).
# Enquanto FALSE ficava so em 6-images-tables/graphs/ (gitignored).
# ------------------------------------------------------------------------------
PROMOVER <- TRUE
if (PROMOVER) {
  finalizar_figura(
    plot        = p_combined,
    fig_label   = "wagstaff-transicao-condicional",
    fig_cap     = paste0("Relative inequality in upper secondary completion, tertiary access, ",
                         "and the tertiary transition among upper secondary completers, ",
                         "ages 18–24, Brazil 1992–", ANO_MAX_SERIE,
                         " (Wagstaff concentration index)."),
    fonte       = paste0("IBGE — PNAD (1992–2015) and PNAD Contínua (2016–",
                         ANO_MAX_SERIE, ")"),
    # Sec 13.7 WRITING-STYLE.md: ate ~5 linhas impressas, nesta ordem --
    # (1) o que cada elemento visual mostra, (2) variavel e corte, (3) metodo
    # do IC, (4) link ao apendice (apendice=, automatico), (5) Source (fonte=).
    # O que saiu daqui e vai para a entrada da figura em "Extended Figure
    # Notes" (sec-fignote-wagstaff-transicao-condicional): composicao dos
    # elegiveis (17,6% -> 73,6% da coorte) e a leitura descritiva/nao causal
    # da condicional; NA de anos de estudo 1992-1995 (D19, caveat 3); emenda
    # PNAD/PNADC 2015-2016; estimador (GLM ponderado, Kish DEFF = 2,0);
    # decomposicao exata de C(EM) no 097F.
    nota        = paste0(
      "Wagstaff index ($W$): a Gini-like scalar bounded for binary outcomes; $W$ near zero ",
      "means the outcome is independent of household income, high $W$ means it remains ",
      "concentrated among richer households. Solid lines, full cohort aged 18--24: ",
      "completed upper secondary (circles) and ever entered tertiary (triangles). ",
      "Dashed/squares: ever entered tertiary computed only among upper secondary completers, ",
      "with income ranks recomputed within that group; the gap to the solid tertiary line is ",
      "the part of access inequality that runs through the upper secondary funnel. ",
      "Grey bands: 95\\% CIs. Labels along top mark presidential terms."),
    apendice    = "sec-fignote-wagstaff-transicao-condicional",
    script_path = here::here("posts", "wagstaff-transicao-condicional", "plot.R"),
    largura = LARGURA_TEXTO, altura = ALTURA_FIG, unidades = "in"
  )
} else {
  cat("\nPROMOVER = FALSE: rascunho em graphs/. Nada em final/ ou nos .qmd foi tocado.\n")
}

cat("====== Script 097G_Tese_Wagstaff_Transicao_Condicional concluido ======\n")
