# ==============================================================================
# SCRIPT: 055_Fig_Portas_Renda_EaD.R
# ODA: portado de 4-DA-Code/2026-08_SEDAP/080_Wagstaff_Cotas/055_Fig_Portas_Renda_EaD.R (dissertacao)
#      em 2026-09-23; le as matrizes redistribuidas em data/ (ou a reextracao
#      em data-raw/sedap/portas/). Figuras em output/graphs/ (rascunho) e
#      output/figures/<fig_label>/ (PROMOVER = TRUE, padrao).
#
# Figura irma de 052_Fig_Portas_Renda_Empilhado.R, mas para MODALIDADE de
# ensino (EaD x presencial) em vez de via de financiamento: para cada decil
# de renda per capita, que fracao da populacao entrou em EaD e que fracao
# entrou presencial, coortes 2013 e 2020. Pedido do autor (2026-09-22):
# "vamos fazer a 052 so para EaD vs presencial e depois vemos como fazer" --
# ou seja, esta figura fica standalone por enquanto; combinar com as vias de
# 051/052/053 fica para decisao posterior.
#
# ***** RESSALVA CRITICA (repetida de 054_Extrair_EaD.R -- LEIA LA) *****
# EaD nao exige ENEM para ingresso. O vinculo ENEM x SUP_ALUNO que esta
# figura usa e uma amostra autosselecionada de quem prestou o ENEM, e essa
# selecao SUB-REPRESENTA EaD privado (medido em 0,68x em 2022 no plano
# 9-vers/plan/2026-08-06_Plano_Perfil_Renda_EaD_ENEM_CENSUP.md, WP3). Esta
# figura mede "EaD entre quem prestou o ENEM", nao "EaD na populacao" -- e a
# distancia entre as duas e pior aqui do que nas vias de financiamento de
# 051/052/053, que tambem sao amostras do ENEM mas nao tem esse gradiente de
# cobertura documentado tao grande.
#
# DUAS VERSOES (decisao do autor, 2026-09-23) -- mesma logica de 052:
#   - principal (corpo do 0202): matrizes SEM o filtro IN_MATRICULA, rotulo
#     portas-renda-coortes-ead;
#   - apendice: matrizes COM o filtro (sufixo _com_in_matricula), rotulo
#     portas-renda-coortes-ead-in-matricula.
# Aqui a diferenca e a maior da familia: EaD de 2020 no decil mais pobre vai
# de 15,5% (com filtro) para 19,5% (sem filtro).
#
# ROW_LOSS EXATO (fracao de celulas suprimidas, nao de pessoas):
#   2013: 0,4901960784%   2020: 0%   (identico nas duas versoes)
#
# FONTE DOS DADOS: saidas/matriz_<ano>_fina_ead[_com_in_matricula].csv,
# geradas por 054_Extrair_EaD.R (a segunda com FILTRO_IN_MATRICULA=TRUE).
#
# SAIDAS: 6-images-tables/graphs/<timestamp>_055_Fig_Portas_Renda_EaD[_com_in_matricula].png/.pdf
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
DIR_DATA <- here::here("posts", "portas-renda-coortes-ead", "data")
DIR_RAW  <- here::here("data-raw", "sedap", "portas")
COORTES    <- c(2013L, 2020L)

# Ordem de empilhamento: position_stack() poe o primeiro nivel (EaD) no topo;
# a legenda segue a mesma ordem (esquerda -> direita).
LBL_MODALIDADE <- c(
  "1" = "Distance learning (EaD)",
  "2" = "In-person"
)
NIVEIS_MODALIDADE <- unname(LBL_MODALIDADE)

COR_MODALIDADE <- setNames(
  c(PALETA_QUALITATIVA[[5]], PALETA_QUALITATIVA[[2]]),
  NIVEIS_MODALIDADE
)

# ==============================================================================
# 1. PONTOS MEDIOS  (mesmos de 040/050/051/052/053)
# ==============================================================================
SM_17 <- c(A = 0, B = 0.5, C = 1.25, D = 1.75, E = 2.25, F = 2.75, G = 3.5,
           H = 4.5, I = 5.5, J = 6.5, K = 7.5, L = 8.5, M = 9.5, N = 11,
           O = 13.5, P = 17.5, Q = 25)
MOR_MEDIO <- c("1_2" = 1.8, "3_4" = 3.5, "5_6" = 5.4, "7_mais" = 7.8)

# ==============================================================================
# 2. LEITURA, RENDA PER CAPITA E DECIS
# ==============================================================================
ler_coorte <- function(ano, sufixo = "") {
  nome <- sprintf("matriz_%d_fina_ead%s.csv", ano, sufixo)
  arq <- file.path(DIR_RAW, nome)
  if (!file.exists(arq)) arq <- file.path(DIR_DATA, nome)
  if (!file.exists(arq)) {
    stop("Matriz ausente: ", arq,
         "\nRode shared-pipeline/sedap/portas/054_Extrair_EaD.R antes deste script",
         if (nzchar(sufixo)) " (com FILTRO_IN_MATRICULA=TRUE)." else ".")
  }
  read.csv(arq, stringsAsFactors = FALSE) |>
    mutate(ano = ano, COD_MODALIDADE = as.character(COD_MODALIDADE))
}

celulas <- function(m, sm = SM_17, mor = MOR_MEDIO) {
  m |>
    mutate(rpc = unname(sm[FAIXA_RENDA]) / unname(mor[MORADORES_GRUPO])) |>
    pivot_wider(id_cols = c(FAIXA_RENDA, MORADORES_GRUPO, rpc),
                names_from = COD_MODALIDADE, values_from = N,
                values_fill = 0, names_prefix = "p") |>
    mutate(n_cel = rowSums(across(starts_with("p")))) |>
    arrange(rpc, desc(unname(MOR_MEDIO[MORADORES_GRUPO])))
}

por_decil <- function(cel, k = 10) {
  tot <- sum(cel$n_cel)
  ini <- c(0, cumsum(cel$n_cel)[-nrow(cel)]) / tot
  fim <- cumsum(cel$n_cel) / tot
  cortes <- seq(0, 1, length.out = k + 1)
  modalidades <- grep("^p[1-2]$", names(cel), value = TRUE)
  out <- lapply(seq_len(k), function(d) {
    w <- pmax(0, pmin(fim, cortes[d + 1]) - pmax(ini, cortes[d])) /
         pmax(fim - ini, .Machine$double.eps)
    n_d <- sum(w * cel$n_cel)
    vals <- vapply(modalidades, function(p) sum(w * cel[[p]]), numeric(1))
    data.frame(decil = d, n = n_d, modalidade = modalidades,
               p = 100 * vals / n_d, row.names = NULL)
  })
  bind_rows(out)
}

montar <- function(ano, sufixo = "", sm = SM_17, mor = MOR_MEDIO) {
  por_decil(celulas(ler_coorte(ano, sufixo), sm, mor)) |> mutate(ano = ano)
}

montar_dados <- function(sufixo) {
  dados <- bind_rows(lapply(COORTES, montar, sufixo = sufixo)) |>
    mutate(cod = sub("^p", "", modalidade),
           modalidade = factor(LBL_MODALIDADE[cod], levels = NIVEIS_MODALIDADE)) |>
    filter(!is.na(modalidade)) |>
    arrange(ano, decil, modalidade)
  stopifnot(!any(is.na(dados$modalidade)), all(dados$p >= 0), all(dados$p <= 100))
  dados
}

# ==============================================================================
# 3. FIGURA — BARRAS EMPILHADAS
# ==============================================================================
rot_painel <- c("2013" = "Cohort of 2013", "2020" = "Cohort of 2020")

fazer_figura <- function(dados) {
  # Rotulo de composicao (pedido do autor, 2026-09-23): so a fracao EaD da
  # coluna (a presencial e o complemento), em preto e negrito, ACIMA da barra.
  rotulos <- dados |>
    group_by(ano, decil) |>
    summarise(share_ead = 100 * sum(p[cod == "1"]) / sum(p),
              topo = sum(p), .groups = "drop") |>
    mutate(rotulo = sprintf("%.0f%%", share_ead))

  ggplot(dados, aes(x = factor(decil), y = p, fill = modalidade)) +
    geom_col(position = "stack", width = 0.75) +
    geom_text(data = rotulos, aes(x = factor(decil), y = topo, label = rotulo),
              inherit.aes = FALSE, vjust = -0.5, size = 2.5, fontface = "bold",
              colour = "black") +
    facet_wrap(~ ano, nrow = 1, labeller = as_labeller(rot_painel)) +
    scale_fill_manual(name = NULL, values = COR_MODALIDADE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(x = "Decile of per-capita family income (1 = poorest)",
         y = "Entered within three years (%)") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position  = "bottom",
      legend.text      = element_text(size = 9),
      legend.key.size  = unit(0.7, "lines"),
      plot.margin      = margin(6, 5.5, 2, 5.5)
    )
}

# ==============================================================================
# 4. VERSOES, RASCUNHO E PROMOCAO
# ==============================================================================
NOTA_BASE <- paste0(
  "by mode of delivery; the percentage above each bar is distance learning's share of ",
  "the decile's entrants. When the same person has both an in-person and a ",
  "distance-learning enrolment within the window, distance learning takes priority by ",
  "convention. Distance learning does not require the ENEM for admission, so this figure ",
  "--- like every other in this series --- measures entry among ENEM takers, not the ",
  "population: coverage is known to under-represent private distance learning by roughly ",
  "a third (0.68$\\times$ in 2022, see Appendix). Deciles are of per-capita family income, ",
  "obtained by dividing the midpoint of the ENEM declared family-income bracket by the ",
  "midpoint of the declared household-size group; the denominator is everyone in the ",
  "decile, including those who never entered. Suppressed cells: 0.49\\% (2013), 0\\% ",
  "(2020); counts are exact up to that suppression, so no confidence bands are drawn.")

VERSOES <- list(
  principal = list(
    sufixo = "",
    fig_label = "portas-renda-coortes-ead",
    fig_cap = paste0("Probability of tertiary entry by mode of delivery ",
                     "(distance learning vs. in-person) and decile of per-capita ",
                     "family income, Brazil, secondary-school cohorts of 2013 and 2020."),
    nota = paste0(
      "Each bar is the share of a cohort's income decile with an enrolment record whose ",
      "year of entry falls within three years of the final year of secondary school, ",
      "whatever the record's later status (including those who dropped out, suspended or ",
      "transferred in their first year), ", NOTA_BASE),
    apendice = "sec-fignote-portas-renda-coortes-ead"),
  apendice = list(
    sufixo = "_com_in_matricula",
    fig_label = "portas-renda-coortes-ead-in-matricula",
    fig_cap = paste0("Probability of tertiary entry by mode of delivery and decile of ",
                     "per-capita family income, counting only enrolments active at the ",
                     "census date, Brazil, secondary-school cohorts of 2013 and 2020."),
    nota = paste0(
      "Earlier definition of entry, kept for comparison: only enrolment records flagged ",
      "as active or completed at the census date (\\texttt{IN\\_MATRICULA} = 1) count, so ",
      "anyone who entered and dropped out, suspended or transferred within the entry ",
      "year is counted as not having entered. Otherwise identical to the figure in the ",
      "chapter: each bar is the share of a cohort's income decile that entered within ",
      "three years, ", NOTA_BASE),
    apendice = "sec-fignote-portas-renda-coortes-in-matricula")
)

# ------------------------------------------------------------------------------
# GUARDA DE TIPOGRAFIA. Usa `.lm_ok`, nao `systemfonts::system_fonts()$family`
# -- ver NEWS.md 2026-09-22 (051) para o porque.
# ------------------------------------------------------------------------------
fonte_pdf_disponivel <- function() {
  tryCatch(isTRUE(.lm_ok), error = function(e) NA)
}

# PROMOVIDA (decisao do autor, 2026-09-22): entra ao fim da secao "Crescimento
# da renda" do 0202, ao lado da 052 na secao de politicas. Versao de apendice
# promovida em 2026-09-23.
PROMOVER <- as.logical(Sys.getenv("PROMOVER", "TRUE"))
.ok_fonte <- fonte_pdf_disponivel()
if (PROMOVER && !isTRUE(.ok_fonte)) {
  cat(sprintf(paste0("\nPROMOCAO RECUSADA: a fonte '%s' nao esta instalada nesta maquina.\n",
                     "O PDF sairia com fallback silencioso. Figura gerada apenas em",
                     " 6-images-tables/graphs/.\n"), FONTE_PDF))
  PROMOVER <- FALSE
}
SCRIPT_PATH <- here::here("posts", "portas-renda-coortes-ead", "plot.R")

for (v in VERSOES) {
  dados <- montar_dados(v$sufixo)
  cat(sprintf("\n-- [%s] probabilidade de ingresso por modalidade e decil (%%) --\n",
              v$fig_label))
  for (a in COORTES) {
    cat("\ncoorte", a, "\n")
    print(round(xtabs(p ~ decil + modalidade, data = subset(dados, ano == a)), 2))
  }

  p_fig <- fazer_figura(dados)
  prefixo <- paste0("055_Fig_Portas_Renda_EaD", v$sufixo)
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

cat("====== 055 concluido ======\n")
