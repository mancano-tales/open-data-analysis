# ==============================================================================
# 020_Extrair_Coortes_Anuais.R
#
# Estende as vias de ingresso dos posts portas-renda-coortes-empilhado e
# portas-renda-coortes-ead (coortes de 2013 e 2020) a TODAS as coortes de 2012
# a 2021, ano a ano, como pediu a orientadora (2026-10-20). O desenho e o de
# shared-pipeline/sedap/portas/031 e 054, com uma mudanca:
#
# PRESENCA NO ENEM (nova). Os scripts 031/054 vinculam a coorte a INSCRICAO no
# ENEM do mesmo ano, nao a presenca: a tabela ENEM_<ano>_SEDAP tem uma linha
# por inscrito. A abstencao varia muito entre edicoes (55% faltaram a prova de
# Matematica em 2020, contra 27% em 2012; 010_Sondagem_Anual.R) e tem gradiente
# de renda, entao numa serie anual a populacao "quem fez o ENEM" mudaria de
# definicao de um ano para o outro. Aqui o aluno so entra se esteve presente
# nas quatro provas objetivas da edicao do ano da coorte ('1' em
# IN_PRESENCA_* ate 2014 e em TP_PRESENCA_* desde 2015).
#
# O que a sondagem (010) fixou:
#   - Coortes 2012-2021. 2011 fica fora: a renda do ENEM 2011 (Q004) tem 11
#     faixas, outra escala, cujos pontos medios nao foram conferidos.
#   - Renda e moradores: Q003/Q004 em 2012-2014; Q006/Q005 desde 2015.
#   - IN_RESERVA_VAGAS e 100% NULL no Censo Superior de 2024: na coorte de
#     2021, quem ingressou em 2024 nao tem a via "publica com reserva"
#     identificavel e cai em "publica ampla". O plot.R marca essa coorte.
#   - Cobertura de CPF da serie final varia de 47% (2016) a 87% (2012, 2017).
#
# Duas consultas por coorte, como em 031 e 054: via de ingresso (porta) e
# modalidade. 20 consultas no total.
#
# Pre-requisito: SEDAP_TOKEN no ambiente.
# Uso: Rscript shared-pipeline/sedap/portas-anuais/020_Extrair_Coortes_Anuais.R
#      COORTES=2012,2013 Rscript ...   (subconjunto)
# ==============================================================================

suppressPackageStartupMessages({ library(here) })
source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))

OUT <- here::here("data-raw", "sedap", "portas-anuais")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

COORTES <- as.integer(strsplit(Sys.getenv("COORTES", paste(2012:2021, collapse = ",")), ",")[[1]])
ETAPAS <- paste(sprintf("'%d'", c(27, 28, 29, 32, 33, 34, 37, 38)), collapse = ", ")
COND_COTA <- "CAST(IN_RESERVA_VAGAS AS STRING) = '1'"

mapa_enem <- function(ano) {
  stopifnot(ano >= 2012)
  pres <- if (ano <= 2014) "IN_PRESENCA_%s" else "TP_PRESENCA_%s"
  list(
    renda = if (ano <= 2014) "Q003" else "Q006",
    moradores = if (ano <= 2014) "Q004" else "Q005",
    presenca = paste(sprintf(paste0("CAST(", pres, " AS STRING) = '1'"), c("CN", "CH", "LC", "MT")), collapse = " AND ")
  )
}

# Uma linha por registro do Censo Superior com ingresso na janela, com o codigo
# de classificacao; a pessoa recebe o MIN() entre os seus registros.
bloco_ingresso <- function(janela, expr_codigo) {
  partes <- vapply(seq_along(janela), function(i) {
    anos_ok <- paste(sprintf("'%d'", janela[1:i]), collapse = ", ")
    sprintf(
      "SELECT CPF_MASC, %s AS COD FROM raw.SUP_ALUNO_%d WHERE CAST(NU_ANO_INGRESSO AS STRING) IN (%s) AND CPF_MASC IS NOT NULL",
      expr_codigo, janela[i], anos_ok
    )
  }, character(1))
  paste(partes, collapse = "\n        UNION ALL\n        ")
}

# Prioridade: 1 publica com reserva < 2 publica ampla < 3 ProUni < 4 FIES <
# 5 privada sem subsidio (a mesma de 031).
COD_PORTA <- paste0(
  "CASE ",
  "WHEN CAST(TP_CATEGORIA_ADMINISTRATIVA AS STRING) IN ('1','2','3') AND ", COND_COTA, " THEN 1 ",
  "WHEN CAST(TP_CATEGORIA_ADMINISTRATIVA AS STRING) IN ('1','2','3') THEN 2 ",
  "WHEN CAST(IN_FIN_NAOREEMB_PROUNI_INTEGR AS STRING) = '1' THEN 3 ",
  "WHEN CAST(IN_FIN_NAOREEMB_PROUNI_PARCIAL AS STRING) = '1' THEN 3 ",
  "WHEN CAST(IN_FIN_REEMB_FIES AS STRING) = '1' THEN 4 ",
  "ELSE 5 END"
)
# Prioridade: 1 EaD < 2 presencial (a mesma de 054).
COD_MODALIDADE <- "CASE WHEN CAST(TP_MODALIDADE_ENSINO AS STRING) = '2' THEN 1 ELSE 2 END"

sql_coorte <- function(ano, expr_codigo) {
  m <- mapa_enem(ano)
  sprintf("
WITH concluintes AS (
    SELECT CPF_MASC
    FROM raw.BAS_MATRICULA_%d
    WHERE CAST(TP_ETAPA_ENSINO AS STRING) IN (%s)
      AND CAST(NU_IDADE AS INT64) >= 16
      AND CAST(NU_IDADE AS INT64) <= 22
      AND CPF_MASC IS NOT NULL
    GROUP BY CPF_MASC
),
enem AS (
    SELECT CPF_MASC,
           CAST(%s AS STRING) AS FAIXA_RENDA,
           SAFE_CAST(%s AS INT64) AS MORADORES
    FROM raw.ENEM_%d_SEDAP
    WHERE CPF_MASC IS NOT NULL
      AND %s IS NOT NULL
      AND %s
    GROUP BY CPF_MASC, %s, %s
),
ingresso_linhas AS (
        %s
),
ingresso AS (
    SELECT CPF_MASC, MIN(COD) AS COD
    FROM ingresso_linhas
    GROUP BY CPF_MASC
)
SELECT
    e.FAIXA_RENDA,
    CASE WHEN e.MORADORES IS NULL THEN 'desconhecido'
         WHEN e.MORADORES <= 2 THEN '1_2'
         WHEN e.MORADORES <= 4 THEN '3_4'
         WHEN e.MORADORES <= 6 THEN '5_6'
         ELSE '7_mais' END AS MORADORES_GRUPO,
    IFNULL(s.COD, 0) AS COD,
    COUNT(*) AS N
FROM concluintes c
INNER JOIN enem e ON c.CPF_MASC = e.CPF_MASC
LEFT JOIN ingresso s ON c.CPF_MASC = s.CPF_MASC
GROUP BY e.FAIXA_RENDA, MORADORES_GRUPO, COD",
    ano, ETAPAS,
    m$renda, m$moradores, ano, m$renda, m$presenca, m$renda, m$moradores,
    bloco_ingresso((ano + 1):(ano + 3), expr_codigo)
  )
}

log_rl <- list()
executar <- function(sql, rotulo, arquivo) {
  cat("\n>>>", rotulo, "\n")
  r <- sedap_query(sql, epsilon = 1.0, delta = 1e-6, verbose = FALSE)
  if (is.null(r) || !nrow(r)) {
    cat("    FALHOU, nada gravado\n")
    return(invisible(NULL))
  }
  dp <- attr(r, "dp_stats")$differential_privacy
  rl <- suppressWarnings(as.numeric(dp$row_loss %||% NA))
  cat(sprintf("    linhas=%d  N=%s  row_loss=%s\n", nrow(r),
    format(sum(as.numeric(r$N)), big.mark = ","),
    if (is.na(rl)) "NA" else format(100 * rl, digits = 6)))
  log_rl[[arquivo]] <<- rl
  utils::write.csv(r, file.path(OUT, arquivo), row.names = FALSE)
  invisible(r)
}

for (ano in COORTES) {
  cat("\n", strrep("=", 70), "\nCOORTE ", ano, " (ingresso ", ano + 1, "-", ano + 3, ")\n", strrep("=", 70), "\n", sep = "")
  executar(sql_coorte(ano, COD_PORTA), sprintf("%d via de ingresso", ano), sprintf("matriz_%d_porta.csv", ano))
  executar(sql_coorte(ano, COD_MODALIDADE), sprintf("%d modalidade", ano), sprintf("matriz_%d_modalidade.csv", ano))
}

rl <- data.frame(arquivo = names(log_rl), row_loss = unlist(log_rl), row.names = NULL)
f_rl <- file.path(OUT, "row_loss.csv")
if (file.exists(f_rl)) rl <- rbind(utils::read.csv(f_rl)[!utils::read.csv(f_rl)$arquivo %in% rl$arquivo, ], rl)
utils::write.csv(rl, f_rl, row.names = FALSE)
cat("\n[OK] 020 concluido.\n")
