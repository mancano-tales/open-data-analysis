# ==============================================================================
# ODA: portado de 4-DA-Code/2026-08_Replicacao_Senkevics2024/ em 2026-09-20; saidas em
#      data-raw/sedap/senkevics/coorte_<ano>/. Exige SEDAP_TOKEN (Tier B).
# 020_Distribuicoes_Marginais.R
# ==============================================================================
# Extrai as distribuicoes necessarias para calcular LOCALMENTE os pontos de
# corte dos decis de renda e de desempenho, na coorte definida em 015.
#
# POR QUE LOCALMENTE: a API do SEDAP+ nao oferece quantis. NTILE() e rejeitado
# ("Name CPF_MASC not found inside t") e APPROX_QUANTILES nao existe na versao
# com privacidade diferencial ("Function not found:
# $differential_privacy_approx_quantiles"). Sondado em 2026-08-07.
#
# Estrategia: pedir a distribuicao em granularidade fina (renda e discreta —
# 17 faixas x ate 20 moradores; desempenho em bins de 1 ponto) e calcular os
# quantis ponderados no 025, com a mesma aritmetica do quantile() que os
# autores usam sobre o microdado.
#
# Toda a parametrizacao (coorte, anos, schema por edicao, helpers de SQL) vem
# de 015_Especificacao.R. Este script nao hardcoda ano nem nome de coluna.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))
source(here::here("shared-pipeline", "sedap", "senkevics", "015_Especificacao.R"))

# ------------------------------------------------------------------------------
# 1. Distribuicao conjunta renda x moradores, por edicao
# ------------------------------------------------------------------------------
# Renda e discreta (17 faixas x ate 20 moradores), entao esta tabela reconstroi
# a distribuicao exata de rfpc localmente, sem perda por binagem.
# Agregacao DIRETA, sem subquery aninhada: o reescritor de privacidade
# diferencial rejeita a forma aninhada quando ha JOIN ("Unrecognized name: b").

cat("=== 1. Distribuicao renda x moradores, por edicao do ENEM ===\n")

renda_dist <- map_dfr(ANOS_ENEM, function(ano) {
  sch <- SCHEMA_ENEM[[as.character(ano)]]
  ue <- join_ultima_edicao(ano)
  sql <- sprintf(
    "
SELECT e.%s AS faixa_renda, e.%s AS qtde_hab, COUNT(*) AS N
FROM %s b
INNER JOIN raw.ENEM_%d_SEDAP e ON b.CPF_MASC = e.CPF_MASC
%s
WHERE %s
  AND %s
%s
GROUP BY faixa_renda, qtde_hab
",
    sch$renda, sch$hab, TABELA_COORTE, ano, ue$join,
    filtro_coorte("b"), expr_presenca(ano, "e"), ue$where
  )

  cat(sprintf("  ENEM %d ... ", ano))
  r <- sedap_query(sql, verbose = FALSE)
  if (is.null(r) || !nrow(r)) {
    cat("FALHOU\n")
    return(NULL)
  }
  cat(sprintf("%d celulas, N = %s\n", nrow(r), format(sum(r$N), big.mark = " ")))
  r %>% mutate(ano_enem = ano)
})

if (nrow(renda_dist)) {
  write_csv(renda_dist, file.path(DIR_DADOS, "020_dist_renda_moradores.csv"))
}

# ------------------------------------------------------------------------------
# 2. Histograma de desempenho, bins de 1 ponto, por edicao
# ------------------------------------------------------------------------------

cat("\n=== 2. Histograma de desempenho (bins de 1 ponto) ===\n")

nota_dist <- map_dfr(ANOS_ENEM, function(ano) {
  ue <- join_ultima_edicao(ano)
  sql <- sprintf(
    "
SELECT CAST(FLOOR(%s) AS INT64) AS bin_nota, COUNT(*) AS N
FROM %s b
INNER JOIN raw.ENEM_%d_SEDAP e ON b.CPF_MASC = e.CPF_MASC
%s
WHERE %s
  AND %s
%s
GROUP BY bin_nota
",
    expr_media(ano, "e"), TABELA_COORTE, ano, ue$join,
    filtro_coorte("b"), expr_presenca(ano, "e"), ue$where
  )

  cat(sprintf("  ENEM %d ... ", ano))
  r <- sedap_query(sql, verbose = FALSE)
  if (is.null(r) || !nrow(r)) {
    cat("FALHOU\n")
    return(NULL)
  }
  cat(sprintf("%d bins, N = %s\n", nrow(r), format(sum(r$N), big.mark = " ")))
  r %>% mutate(ano_enem = ano)
})

if (nrow(nota_dist)) {
  write_csv(nota_dist, file.path(DIR_DADOS, "020_dist_nota.csv"))
}

cat(sprintf(
  "\n[OK] 020 concluido para a coorte %d. N deduplicado: %s\n",
  COORTE_ANO, format(sum(nota_dist$N), big.mark = " ")
))
