# ==============================================================================
# ODA: portado de 4-DA-Code/2026-08_Replicacao_Senkevics2024/ em 2026-09-20; saidas em
#      data-raw/sedap/senkevics/coorte_<ano>/. Exige SEDAP_TOKEN (Tier B).
# 010_Coorte.R — Replicacao de Senkevics et al. (2024), Figura 3
# ==============================================================================
# Dimensiona a coorte de concluintes do EM (ano definido em 015) e mede quanto
# dela sobrevive ao vinculo com o ENEM.
#
# CRITERIO DOS AUTORES (1. Codes in SAS/Code_SAS.sas, linhas 28-47):
#   FROM BAS_SITUACAO
#   WHERE IN_CONCLUINTE = 1 AND TP_SITUACAO IN (5, 9)
#     AND TP_ETAPA_ENSINO IN (27,28,29,32,33,34,37,38)
#     AND NU_IDADE BETWEEN 15 AND 29
#
# DESVIO D1: BAS_SITUACAO nao esta no projeto SEDAP+; so BAS_MATRICULA. Faltam
# IN_CONCLUINTE e TP_SITUACAO, entao a coorte aqui e de MATRICULADOS NA SERIE
# FINAL, nao de concluintes aprovados. Etapa e idade sao identicas as dos
# autores (idade 16-22, o filtro da amostra analitica final no Stata).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))
source(here::here("shared-pipeline", "sedap", "senkevics", "015_Especificacao.R"))

# ------------------------------------------------------------------------------
# 1. Tamanho da coorte, com e sem CPF
# ------------------------------------------------------------------------------
# Sem CPF nao ha vinculo com ENEM nem com o Censo Superior — a cobertura mede
# diretamente quanto da coorte e sequer elegivel ao acompanhamento.

sql_coorte <- sprintf(
  "
SELECT COUNT(*) AS N_MATRICULAS_SERIE_FINAL, COUNT(CPF_MASC) AS N_COM_CPF
FROM %s b
WHERE %s
", TABELA_COORTE, filtro_coorte("b")
)

cat(sprintf(
  "=== 1. Coorte %d (serie final do EM, %d-%d anos) ===
",
  COORTE_ANO, IDADE_MIN, IDADE_MAX
))
coorte <- sedap_query(sql_coorte)
print(coorte)

if (!is.null(coorte)) {
  write_csv(coorte, file.path(DIR_DADOS, "010_coorte_tamanho.csv"))
  cat(sprintf(
    "Cobertura de CPF: %.1f%%
",
    100 * coorte$N_COM_CPF / coorte$N_MATRICULAS_SERIE_FINAL
  ))
  if (COORTE_ANO == 2012) {
    cat(sprintf(
      "Artigo: 1.690.000 concluintes. Aqui: %s matriculados na serie final (%.0f%% do artigo).
",
      format(coorte$N_MATRICULAS_SERIE_FINAL, big.mark = " "),
      100 * coorte$N_MATRICULAS_SERIE_FINAL / 1690000
    ))
  }
}

# ------------------------------------------------------------------------------
# 2. Vinculo com cada edicao do ENEM
# ------------------------------------------------------------------------------
# Edicao a edicao, SEM deduplicar: quem fez mais de uma aparece em varias
# linhas. A deduplicacao ("ultima edicao feita") entra no 020, por anti-join.

cat("
=== 2. Vinculo coorte x ENEM, por edicao (com repeticao) ===
")

vinculo <- lapply(ANOS_ENEM, function(ano) {
  sql <- sprintf(
    "
SELECT COUNT(*) AS N_VINCULADOS
FROM %s b
INNER JOIN raw.ENEM_%d_SEDAP e ON b.CPF_MASC = e.CPF_MASC
WHERE %s
", TABELA_COORTE, ano, filtro_coorte("b")
  )
  cat(sprintf("  ENEM %d ... ", ano))
  r <- sedap_query(sql, verbose = FALSE)
  if (is.null(r)) {
    cat("FALHOU
")
    return(NULL)
  }
  cat(sprintf("%s vinculados
", format(r$N_VINCULADOS, big.mark = " ")))
  tibble(ano_enem = ano, n_vinculados = r$N_VINCULADOS)
})

vinculo <- bind_rows(vinculo)
if (nrow(vinculo)) {
  write_csv(vinculo, file.path(DIR_DADOS, "010_vinculo_enem_por_edicao.csv"))
  if (COORTE_ANO == 2012) {
    cat("
Referencia do artigo (individuos unicos): 1 133 027
")
  }
}

cat(sprintf("
[OK] 010 concluido para a coorte %d.
", COORTE_ANO))
