# ==============================================================================
# 031_Extrair_ProUni_FIES.R
# ODA: portado de 4-DA-Code/2026-08_SEDAP/080_Wagstaff_Cotas/031_Extrair_ProUni_FIES.R (dissertacao)
#      em 2026-09-23; grava em data-raw/sedap/portas/, de onde os plot.R dos
#      posts portas-renda-coortes-* leem de preferencia a copia redistribuida.
#
# Variante de 030_Extrair_Coortes.R que separa a porta "privada subsidiada" em
# ProUni (integral+parcial) e FIES, para uma figura que mostra as duas
# separadamente. Script a parte, nao um parametro em 030, DE PROPOSITO: 030
# alimenta o estimador de Wagstaff (010_Estimador_Wagstaff.R via 040), que
# espera exatamente 4 codigos de porta (ver PORTAS em 040_Estimar_Reportar.R).
# Mudar o esquema de codigos ali quebraria a leitura de matriz_<ano>_*.csv ja
# usada e auditada. Este script grava em ARQUIVOS SEPARADOS
# (matriz_<ano>_fina_prouni_fies.csv), sem tocar nos originais.
#
# PRIORIDADE ProUni ANTES de FIES (decisao do autor, 2026-09-22): testado em
# sondagem ad-hoc que ~9% dos ingressantes na porta subsidiada (2020, grade
# primaria: 3413 de 38659) tem sinalizacao para as duas ao mesmo tempo — ou na
# mesma matricula, ou em matriculas diferentes dentro da janela — e o MIN()
# usado para resolver CPFs com mais de uma matricula na janela precisa de UM
# desempate. A ordem so importa para esse grupo; nao ha "resposta certa"
# empirica, e uma convencao, escrita aqui para poder ser contestada.
#
# GOTCHA JA CORRIGIDO NUM TESTE ANTERIOR: os codigos de ProUni e FIES tem que
# ficar os DOIS abaixo do codigo de "privada de mercado" (senao o MIN() por
# CPF inverte a prioridade so para quem tiver codigo de porta subsidiada MAIOR
# que o de mercado). Por isso o mercado e o CODIGO MAIS ALTO (5), nao o 4 como
# em 030 -- aqui a ordem e 1=publica reservada, 2=publica ampla, 3=ProUni,
# 4=FIES, 5=privada de mercado.
#
# Pre-requisito: SEDAP_TOKEN no ambiente (ver 010_SEDAP_Cliente.R). Cada
# consulta consome orcamento de privacidade diferencial (epsilon); este script
# roda so as duas coortes com grade fina (2013, 2020) -- a grade que
# 051_Fig_Portas_Renda_ProUni_FIES.R consome -- para nao gastar orcamento em
# 2010 (sem variavel de moradores, fora da figura) nem na grade primaria
# (nao usada por 051).
#
# Uso: Rscript shared-pipeline/sedap/portas/031_Extrair_ProUni_FIES.R
# ==============================================================================

suppressPackageStartupMessages({ library(here) })
source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))

OUT <- here::here("data-raw", "sedap", "portas")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

LIMITE_ROW_LOSS <- 0.05

# Mesmo mapa de 030_Extrair_Coortes.R para as duas coortes com moradores.
MAPA_ANO <- list(
  `2013` = list(renda = "Q003", moradores = "Q004",
                etapas = c("27", "28", "29", "32", "33", "34", "37", "38"),
                janela = 2014:2016),
  `2020` = list(renda = "Q006", moradores = "Q005",
                etapas = c("27", "28", "29", "32", "33", "34", "37", "38"),
                janela = 2021:2023)
)

COND_COTA <- "CAST(IN_RESERVA_VAGAS AS STRING) = '1'"

# FILTRO DE VINCULO ATIVO (revisao de 2026-09-23). A versao original exigia
# IN_MATRICULA = '1', que so vale para matricula cursando ou formada na data
# do censo: quem ingressou e trancou, evadiu ou se transferiu no proprio ano
# de ingresso sumia e contava como "nao ingressou". O padrao agora e SEM o
# filtro -- ingresso e qualquer registro com NU_ANO_INGRESSO na janela. O
# filtro fica disponivel (FILTRO_IN_MATRICULA=TRUE no ambiente) para
# reproduzir a versao antiga, gravada com sufixo _com_in_matricula; e ela que
# alimenta a figura de comparacao do apendice. Diferenca medida: +8,7%
# ingressantes em 2013, +8,2% em 2020, concentrada na privada sem subsidio.
FILTRO_IN_MATRICULA <- identical(Sys.getenv("FILTRO_IN_MATRICULA"), "TRUE")
COND_MATRICULA <- if (FILTRO_IN_MATRICULA) "CAST(IN_MATRICULA AS STRING) = '1' AND " else ""
SUFIXO <- if (FILTRO_IN_MATRICULA) "_com_in_matricula" else ""

# Prioridade do MIN(): 1 publica reservada < 2 publica ampla <
#                      3 ProUni < 4 FIES < 5 privada de mercado
bloco_ingresso <- function(janela) {
  partes <- vapply(seq_along(janela), function(i) {
    ano <- janela[i]
    anos_ok <- paste(sprintf("'%d'", janela[1:i]), collapse = ", ")
    sprintf(paste0(
      "SELECT CPF_MASC, ",
      "CASE ",
      "WHEN CAST(TP_CATEGORIA_ADMINISTRATIVA AS STRING) IN ('1','2','3') AND %s THEN 1 ",
      "WHEN CAST(TP_CATEGORIA_ADMINISTRATIVA AS STRING) IN ('1','2','3') THEN 2 ",
      "WHEN CAST(IN_FIN_NAOREEMB_PROUNI_INTEGR AS STRING) = '1' THEN 3 ",
      "WHEN CAST(IN_FIN_NAOREEMB_PROUNI_PARCIAL AS STRING) = '1' THEN 3 ",
      "WHEN CAST(IN_FIN_REEMB_FIES AS STRING) = '1' THEN 4 ",
      "ELSE 5 END AS COD_PORTA ",
      "FROM raw.SUP_ALUNO_%d ",
      "WHERE %sCAST(NU_ANO_INGRESSO AS STRING) IN (%s) ",
      "AND CPF_MASC IS NOT NULL"),
      COND_COTA, ano, COND_MATRICULA, anos_ok)
  }, character(1))
  paste(partes, collapse = "\n        UNION ALL\n        ")
}

# Grade fina (renda per capita x moradores x porta), a unica que
# 051_Fig_Portas_Renda_ProUni_FIES.R consome.
sql_coorte_fina <- function(ano) {
  m <- MAPA_ANO[[as.character(ano)]]
  etapas <- paste(sprintf("'%s'", m$etapas), collapse = ", ")
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
    GROUP BY CPF_MASC, %s, %s
),
ingresso_linhas AS (
        %s
),
ingresso AS (
    SELECT CPF_MASC, MIN(COD_PORTA) AS COD_PORTA
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
    IFNULL(s.COD_PORTA, 0) AS COD_PORTA,
    COUNT(*) AS N
FROM concluintes c
INNER JOIN enem e ON c.CPF_MASC = e.CPF_MASC
LEFT JOIN ingresso s ON c.CPF_MASC = s.CPF_MASC
GROUP BY e.FAIXA_RENDA, MORADORES_GRUPO, COD_PORTA",
    ano, etapas,
    m$renda, m$moradores, ano, m$renda, m$renda, m$moradores,
    bloco_ingresso(m$janela))
}

executar <- function(sql, rotulo, arquivo) {
  cat("\n>>>", rotulo, "\n")
  r <- sedap_query(sql, epsilon = 1.0, delta = 1e-6, verbose = TRUE)
  if (is.null(r)) { cat("    FALHOU — nada gravado\n"); return(invisible(NULL)) }
  dp <- attr(r, "dp_stats")$differential_privacy
  rl <- suppressWarnings(as.numeric(dp$row_loss %||% NA))
  cat(sprintf("    linhas=%d  row_loss=%s\n", nrow(r),
              if (is.na(rl)) "NA" else sprintf("%.2f%%", 100 * rl)))
  if (!is.na(rl) && rl > LIMITE_ROW_LOSS) {
    cat(sprintf("    [ATENCAO] row_loss acima de %.0f%%: use como robustez, nao como primario.\n",
                100 * LIMITE_ROW_LOSS))
  }
  utils::write.csv(r, file.path(OUT, arquivo), row.names = FALSE)
  cat("    gravado:", arquivo, "\n")
  invisible(r)
}

for (ano in c(2013, 2020)) {
  cat("\n", strrep("=", 70), "\nCOORTE ", ano, " (ProUni/FIES separados)\n",
      strrep("=", 70), "\n", sep = "")
  executar(sql_coorte_fina(ano), sprintf("%d grade fina (renda per capita x porta, 5 codigos)", ano),
           sprintf("matriz_%d_fina_prouni_fies%s.csv", ano, SUFIXO))
}

cat("\nRegistrado em data-raw/sedap/log_consultas_sedap.tsv\n")
