# ==============================================================================
# SCRIPT: 052_Fig_Portas_Renda_Empilhado.R
# ODA: portado de 4-DA-Code/2026-08_SEDAP/080_Wagstaff_Cotas/052_Fig_Portas_Renda_Empilhado.R (dissertacao)
#      em 2026-09-23; le as matrizes redistribuidas em data/ (ou a reextracao
#      em data-raw/sedap/portas/). Figuras em output/graphs/ (rascunho) e
#      output/figures/<fig_label>/ (PROMOVER = TRUE, padrao).
#
# Mesma pergunta de 051 (probabilidade de ingresso por via de acesso e decil
# de renda per capita, ProUni e FIES separados), mas em barras empilhadas em
# vez de linhas: para cada decil, por qual via as pessoas daquele decil
# entraram na faculdade. A altura da barra e a probabilidade TOTAL de
# ingresso do decil (soma das cinco vias); os segmentos coloridos mostram a
# composicao. E o mesmo numero "p" que 051 ja calculava por decil x porta,
# so que empilhado em vez de tracado como linhas separadas -- pedido do autor
# (2026-09-22) para ver a composicao da entrada, nao so o nivel de cada via.
#
# PROMOVIDA (decisao do autor, 2026-09-22): substitui 051 (linhas) no
# capitulo. O autor tambem avaliou 053_Fig_Portas_Renda_Empilhado100.R
# (empilhado a 100%, so composicao) e preferiu esta, que preserva o nivel
# total de acesso na altura da barra.
#
# DUAS VERSOES (decisao do autor, 2026-09-23). A revisao de 2026-09-23 achou
# que o filtro IN_MATRICULA = '1' da extracao original descartava quem
# ingressou e evadiu/trancou/se transferiu no proprio ano de ingresso (ver
# 031). O script agora gera as duas:
#   - principal (corpo do 0202): matrizes SEM o filtro, rotulo
#     portas-renda-coortes-empilhado;
#   - apendice: matrizes COM o filtro (sufixo _com_in_matricula), rotulo
#     portas-renda-coortes-empilhado-in-matricula, para documentar a
#     diferenca.
#
# FONTE DOS DADOS: saidas/matriz_<ano>_fina_prouni_fies[_com_in_matricula].csv,
# geradas por 031_Extrair_ProUni_FIES.R (a segunda com FILTRO_IN_MATRICULA=TRUE).
#
# SAIDAS: 6-images-tables/graphs/<timestamp>_052_Fig_Portas_Renda_Empilhado[_com_in_matricula].png/.pdf
# ==============================================================================

MOSTRAR_TITULO <- FALSE
MOSTRAR_FONTE  <- FALSE

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(here)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme(base_size = 14))

# ODA: as matrizes agregadas sao redistribuidas em data/ -- a figura reproduz
# sem credencial. Quem reextrair com shared-pipeline/sedap/portas/ tem a
# reextracao, gravada em data-raw/sedap/portas/, lida de preferencia.
DIR_DATA <- here::here("posts", "portas-renda-coortes-empilhado", "data")
DIR_RAW  <- here::here("data-raw", "sedap", "portas")
COORTES    <- c(2013L, 2020L)

# Ordem de empilhamento: position_stack() poe o PRIMEIRO nivel no TOPO da
# barra, entao a via da primeira posicao (reserva de vagas) fica em cima e a
# privada sem subsidio na base. NIVEIS_PORTA tambem fixa a ordem da legenda
# (esquerda -> direita), que segue a MESMA ordem (topo -> base).
LBL_PORTA <- c(
  "1" = "Public, affirmative action",
  "4" = "FIES",
  "3" = "ProUni",
  "2" = "Public, open competition",
  "5" = "Private, without subsidy"
)
NIVEIS_PORTA <- unname(LBL_PORTA)

COR_PORTA <- setNames(
  c(PALETA_QUALITATIVA[[1]], PALETA_QUALITATIVA[[5]],
    PALETA_QUALITATIVA[[3]], PALETA_QUALITATIVA[[4]], PALETA_QUALITATIVA[[2]]),
  NIVEIS_PORTA
)

# ==============================================================================
# 1. PONTOS MEDIOS  (mesmos de 040/050/051)
# ==============================================================================
SM_17 <- c(A = 0, B = 0.5, C = 1.25, D = 1.75, E = 2.25, F = 2.75, G = 3.5,
           H = 4.5, I = 5.5, J = 6.5, K = 7.5, L = 8.5, M = 9.5, N = 11,
           O = 13.5, P = 17.5, Q = 25)
MOR_MEDIO <- c("1_2" = 1.8, "3_4" = 3.5, "5_6" = 5.4, "7_mais" = 7.8)

# ==============================================================================
# 2. LEITURA, RENDA PER CAPITA E DECIS  (identico a 051)
# ==============================================================================
ler_coorte <- function(ano, sufixo = "") {
  nome <- sprintf("matriz_%d_fina_prouni_fies%s.csv", ano, sufixo)
  arq <- file.path(DIR_RAW, nome)
  if (!file.exists(arq)) arq <- file.path(DIR_DATA, nome)
  if (!file.exists(arq)) {
    stop("Matriz ausente: ", arq,
         "\nRode shared-pipeline/sedap/portas/031_Extrair_ProUni_FIES.R antes deste script",
         if (nzchar(sufixo)) " (com FILTRO_IN_MATRICULA=TRUE)." else ".")
  }
  read.csv(arq, stringsAsFactors = FALSE) |>
    mutate(ano = ano, COD_PORTA = as.character(COD_PORTA))
}

celulas <- function(m, sm = SM_17, mor = MOR_MEDIO) {
  m |>
    mutate(rpc = unname(sm[FAIXA_RENDA]) / unname(mor[MORADORES_GRUPO])) |>
    pivot_wider(id_cols = c(FAIXA_RENDA, MORADORES_GRUPO, rpc),
                names_from = COD_PORTA, values_from = N,
                values_fill = 0, names_prefix = "p") |>
    mutate(n_cel = rowSums(across(starts_with("p")))) |>
    arrange(rpc, desc(unname(MOR_MEDIO[MORADORES_GRUPO])))
}

por_decil <- function(cel, k = 10) {
  tot <- sum(cel$n_cel)
  ini <- c(0, cumsum(cel$n_cel)[-nrow(cel)]) / tot
  fim <- cumsum(cel$n_cel) / tot
  cortes <- seq(0, 1, length.out = k + 1)
  portas <- grep("^p[1-5]$", names(cel), value = TRUE)
  out <- lapply(seq_len(k), function(d) {
    w <- pmax(0, pmin(fim, cortes[d + 1]) - pmax(ini, cortes[d])) /
         pmax(fim - ini, .Machine$double.eps)
    n_d <- sum(w * cel$n_cel)
    vals <- vapply(portas, function(p) sum(w * cel[[p]]), numeric(1))
    data.frame(decil = d, n = n_d, porta = portas,
               p = 100 * vals / n_d, row.names = NULL)
  })
  bind_rows(out)
}

montar <- function(ano, sufixo = "", sm = SM_17, mor = MOR_MEDIO) {
  por_decil(celulas(ler_coorte(ano, sufixo), sm, mor)) |> mutate(ano = ano)
}

montar_dados <- function(sufixo) {
  dados <- bind_rows(lapply(COORTES, montar, sufixo = sufixo)) |>
    mutate(cod = sub("^p", "", porta),
           porta = factor(LBL_PORTA[cod], levels = NIVEIS_PORTA)) |>
    filter(!is.na(porta)) |>
    arrange(ano, decil, porta)
  stopifnot(!any(is.na(dados$porta)), all(dados$p >= 0), all(dados$p <= 100))
  dados
}

# ==============================================================================
# 3. FIGURA — BARRAS EMPILHADAS
# ==============================================================================
rot_painel <- c("2013" = "Cohort of 2013", "2020" = "Cohort of 2020")

# Rotulos de composicao (pedido do autor, 2026-09-23): dentro de cada segmento,
# a fracao que aquela via representa NA COLUNA (segmento / total do decil), nao
# o p absoluto -- o p absoluto ja se le no eixo. Em negrito e no mesmo corpo da
# 055 (pedido do autor: a versao menor ficava ilegivel). Segmentos baixos demais
# para caber o texto ficam sem rotulo (LIMIAR_ROTULO, em pontos percentuais de
# altura). Cor do texto pela luminancia do preenchimento.
TAM_ROTULO    <- 2.5
LIMIAR_ROTULO <- 4.5
cor_texto <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  ifelse(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ] > 0.55, "#222222", "white")
}

fazer_figura <- function(dados) {
  # A posicao y de cada rotulo e calculada na ordem de position_stack()
  # (ultimo nivel embaixo, primeiro em cima) ANTES de filtrar -- empilhar so
  # os rotulos filtrados deslocaria todos.
  rotulos <- dados |>
    arrange(ano, decil, desc(as.integer(porta))) |>
    group_by(ano, decil) |>
    mutate(share = 100 * p / sum(p),
           y_mid = cumsum(p) - p / 2) |>
    ungroup() |>
    filter(p >= LIMIAR_ROTULO) |>
    mutate(cor = cor_texto(COR_PORTA[as.character(porta)]),
           rotulo = sprintf("%.0f%%", share))

  ggplot(dados, aes(x = factor(decil), y = p, fill = porta)) +
    geom_col(position = "stack", width = 0.8) +
    geom_text(data = rotulos, aes(y = y_mid, label = rotulo, colour = cor),
              size = TAM_ROTULO, fontface = "bold", show.legend = FALSE) +
    scale_colour_identity() +
    facet_wrap(~ ano, nrow = 1, labeller = as_labeller(rot_painel)) +
    scale_fill_manual(name = NULL, values = COR_PORTA) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(x = "Decile of per-capita family income (1 = poorest)",
         y = "Entered within three years (%)") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position  = "bottom",
      legend.text      = element_text(size = 7.8),
      legend.key.size  = unit(0.7, "lines"),
      legend.spacing.x = unit(3, "pt"),
      plot.margin      = margin(6, 5.5, 2, 5.5)
    )
}

# ==============================================================================
# 4. VERSOES, RASCUNHO E PROMOCAO
# ==============================================================================
NOTA_BASE <- paste0(
  "segments show the route used, so bar height is the decile's total entry probability ",
  "and the percentage printed in each segment is that route's share of the decile's ",
  "entrants (segments under 4.5 points are left unlabelled). ",
  "ProUni and FIES are shown separately; when the same person is flagged for both within ",
  "the window, ProUni takes priority by convention. Deciles are of per-capita family ",
  "income, obtained by dividing the midpoint of the ENEM declared family-income bracket ",
  "by the midpoint of the declared household-size group; the denominator is everyone in ",
  "the decile, including those who never entered. Counts are exact up to the platform's ",
  "suppression of small cells, so no confidence bands are drawn. Everything is ",
  "conditional on having sat the ENEM in the year of completion.")

VERSOES <- list(
  principal = list(
    sufixo = "",
    fig_label = "portas-renda-coortes-empilhado",
    fig_cap = paste0("Composition of tertiary entry by access route and ",
                     "decile of per-capita family income, Brazil, secondary-school ",
                     "cohorts of 2013 and 2020."),
    nota = paste0(
      "Each bar is the share of a cohort's income decile with an enrolment record whose ",
      "year of entry falls within three years of the final year of secondary school, ",
      "whatever the record's later status (including those who dropped out, suspended or ",
      "transferred in their first year); ", NOTA_BASE,
      " Suppressed cells: 0.49\\% of cells in 2013, 2.24\\% in 2020."),
    apendice = "sec-fignote-portas-renda-coortes-empilhado"),
  apendice = list(
    sufixo = "_com_in_matricula",
    fig_label = "portas-renda-coortes-empilhado-in-matricula",
    fig_cap = paste0("Composition of tertiary entry by access route and decile of ",
                     "per-capita family income, counting only enrolments active at the ",
                     "census date, Brazil, secondary-school cohorts of 2013 and 2020."),
    nota = paste0(
      "Earlier definition of entry, kept for comparison: only enrolment records flagged ",
      "as active or completed at the census date (\\texttt{IN\\_MATRICULA} = 1) count, so ",
      "anyone who entered and dropped out, suspended or transferred within the entry ",
      "year is counted as not having entered. Otherwise identical to the figure in the ",
      "chapter; ", NOTA_BASE,
      " Suppressed cells: 0.49\\% of cells in 2013, 2.0\\% in 2020."),
    apendice = "sec-fignote-portas-renda-coortes-in-matricula")
)

# ------------------------------------------------------------------------------
# GUARDA DE TIPOGRAFIA. Usa `.lm_ok` (calculado em plot_theme.R via
# match_fonts()), nao `systemfonts::system_fonts()$family` -- ver NEWS.md
# 2026-09-22 para o porque (o alias "LM Roman 10" que o GDI/Cairo usa nao
# aparece nesse indice, e a checagem antiga dava falso negativo sempre).
# ------------------------------------------------------------------------------
fonte_pdf_disponivel <- function() {
  tryCatch(isTRUE(.lm_ok), error = function(e) NA)
}

# PROMOVIDO (decisao do autor, 2026-09-22; versao de apendice 2026-09-23).
PROMOVER <- as.logical(Sys.getenv("PROMOVER", "TRUE"))
.ok_fonte <- fonte_pdf_disponivel()
if (PROMOVER && isFALSE(.ok_fonte)) {
  cat(sprintf(paste0("\nPROMOCAO RECUSADA: a fonte '%s' nao esta instalada nesta maquina.\n",
                     "O PDF sairia com fallback silencioso. Figura gerada apenas em",
                     " 6-images-tables/graphs/.\n"), FONTE_PDF))
  PROMOVER <- FALSE
}
SCRIPT_PATH <- here::here("posts", "portas-renda-coortes-empilhado", "plot.R")

for (v in VERSOES) {
  dados <- montar_dados(v$sufixo)
  cat(sprintf("\n-- [%s] probabilidade total de ingresso por decil (soma das 5 vias, %%) --\n",
              v$fig_label))
  print(as.data.frame(dados |> group_by(ano, decil) |>
                        summarise(total = sum(p), .groups = "drop")))

  p_fig <- fazer_figura(dados)
  prefixo <- paste0("052_Fig_Portas_Renda_Empilhado", v$sufixo)
  for (fmt in c("png", "pdf")) {
    salvar_grafico(p_fig, prefixo = prefixo,
                   largura = LARGURA_TEXTO, altura = ALTURA_PADRAO,
                   unidades = "in", formato = fmt)
  }

  if (PROMOVER) {
    finalizar_figura(
      plot        = p_fig,
      fig_label   = v$fig_label,
      fig_cap     = v$fig_cap,
      fonte       = paste0("INEP --- Censo Escolar, ENEM and Censo da Educação ",
                           "Superior, linked by masked CPF via SEDAP+"),
      nota        = v$nota,
      apendice    = v$apendice,
      script_path = SCRIPT_PATH,
      largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, unidades = "in"
    )
  }
}
if (!PROMOVER) {
  cat("\nPROMOVER = FALSE: figuras em 6-images-tables/graphs/ apenas (rascunho para avaliacao).\n")
}

cat("====== 052 concluido ======\n")
