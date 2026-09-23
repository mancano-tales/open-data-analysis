# ==============================================================================
# 054_Extrair_EaD.R
# ODA: portado de 4-DA-Code/2026-08_SEDAP/080_Wagstaff_Cotas/054_Extrair_EaD.R (dissertacao)
#      em 2026-09-23; grava em data-raw/sedap/portas/, de onde os plot.R dos
#      posts portas-renda-coortes-* leem de preferencia a copia redistribuida.
#
# Extracao paralela a 031_Extrair_ProUni_FIES.R, mas classificando por
# MODALIDADE DE ENSINO (presencial x EaD) em vez de via de financiamento.
# Mesmo funil de populacao de 030/031 (concluintes do medio -> ENEM -> SUP_
# ALUNO na janela), para que os decis de renda per capita sejam comparaveis
# com os de 051/052/053 -- script separado, nao reaproveita 070_Perfil_
# Renda_Modalidade.R (4-DA-Code/2026-08_SEDAP/060_Analise_ENEM_Renda/),
# que usa uma populacao MAIS AMPLA (qualquer ingressante casado com ENEM do
# MESMO ano, sem exigir conclusao do medio nem decis de renda per capita).
#
# VARIAVEL: TP_MODALIDADE_ENSINO em raw.SUP_ALUNO_<ano>. Codificacao herdada
# de 070_Perfil_Renda_Modalidade.R (WP1 do plano
# 9-vers/plan/2026-08-06_Plano_Perfil_Renda_EaD_ENEM_CENSUP.md, ja rodado com
# sucesso 2014-2024): '1' = Presencial, qualquer outro valor no INEP e EaD
# (nao ha terceira modalidade corrente no Censo da Educacao Superior).
#
# ***** RESSALVA CRITICA HERDADA DO PLANO ACIMA (secao 2, "Por que nao
# replicar Wagstaff com estes dados") *****
# EaD NAO EXIGE ENEM para ingresso (processo seletivo proprio da IES). O
# vinculo ENEM x SUP_ALUNO usado aqui e em TODA a familia 030/031/050/051/
# 052/053 e uma amostra AUTOSSELECIONADA de quem prestou o ENEM, e essa
# selecao e DIFERENCIAL por modalidade: medido em 2022 (WP3 do plano acima),
# a cobertura do vinculo era 0,68x para privado EaD (sub-representado) e
# 1,71x para publico presencial (sobre-representado). Isso significa que
# esta figura NAO mede "quantas pessoas do decil fazem EaD" -- mede "quantas
# pessoas do decil que PRESTARAM O ENEM fazem EaD", e a segunda quantidade
# subestima a primeira de forma pior para EaD do que para as outras vias.
# Declarar isso no fignote nao e opcional.
#
# PRIORIDADE DO MIN() POR CPF (convencao, ver 031 para o precedente):
# EaD tem prioridade sobre presencial -- se a mesma pessoa tem uma matricula
# presencial e outra em EaD na janela, ela conta como EaD. Racional: a
# pergunta e "quem faz EaD", entao qualquer contato com EaD marca a pessoa;
# nao ha razao substantiva para o oposto, mas e convencao, nao fato, e fica
# registrada aqui para ser contestada.
#
# RIGOR DE ROW_LOSS (pedido do autor, 2026-09-22): reportar o valor EXATO de
# cada consulta, nao arredondado a 1-2 casas -- ver `executar()` abaixo e o
# cabecalho de 055_Fig_Portas_Renda_EaD.R para onde esses numeros sao usados.
#
# Pre-requisito: SEDAP_TOKEN no ambiente. Roda so 2013 e 2020 (grade fina,
# com moradores), a mesma dupla de 031 -- nao reextrai 2010 (sem moradores).
#
# Uso: Rscript shared-pipeline/sedap/portas/054_Extrair_EaD.R
# ==============================================================================

suppressPackageStartupMessages({ library(here) })
source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))

OUT <- here::here("data-raw", "sedap", "portas")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

LIMITE_ROW_LOSS <- 0.05

# FILTRO DE VINCULO ATIVO -- mesma revisao e mesma chave de 031 (ver la).
# Padrao SEM IN_MATRICULA; FILTRO_IN_MATRICULA=TRUE reproduz a versao
# antiga, gravada com sufixo _com_in_matricula. Aqui o efeito e o maior da
# familia: o EaD de 2020 no decil mais pobre passa de 15,5% para 19,5%.
FILTRO_IN_MATRICULA <- identical(Sys.getenv("FILTRO_IN_MATRICULA"), "TRUE")
COND_MATRICULA <- if (FILTRO_IN_MATRICULA) "CAST(IN_MATRICULA AS STRING) = '1' AND " else ""
SUFIXO <- if (FILTRO_IN_MATRICULA) "_com_in_matricula" else ""

MAPA_ANO <- list(
  `2013` = list(renda = "Q003", moradores = "Q004",
                etapas = c("27", "28", "29", "32", "33", "34", "37", "38"),
                janela = 2014:2016),
  `2020` = list(renda = "Q006", moradores = "Q005",
                etapas = c("27", "28", "29", "32", "33", "34", "37", "38"),
                janela = 2021:2023)
)

# Prioridade do MIN(): 1 EaD < 2 presencial (ver nota de convencao acima).
bloco_ingresso <- function(janela) {
  partes <- vapply(seq_along(janela), function(i) {
    ano <- janela[i]
    anos_ok <- paste(sprintf("'%d'", janela[1:i]), collapse = ", ")
    sprintf(paste0(
      "SELECT CPF_MASC, ",
      "CASE ",
      "WHEN CAST(TP_MODALIDADE_ENSINO AS STRING) = '2' THEN 1 ",
      "ELSE 2 END AS COD_MODALIDADE ",
      "FROM raw.SUP_ALUNO_%d ",
      "WHERE %sCAST(NU_ANO_INGRESSO AS STRING) IN (%s) ",
      "AND CPF_MASC IS NOT NULL"),
      ano, COND_MATRICULA, anos_ok)
  }, character(1))
  paste(partes, collapse = "\n        UNION ALL\n        ")
}

# Grade fina, identica em estrutura a 031 (MORADORES_GRUPO calculado no
# SELECT final, na MESMA consulta que le a tabela bruta -- ver 031 para o
# gotcha de API que exige isso).
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
    SELECT CPF_MASC, MIN(COD_MODALIDADE) AS COD_MODALIDADE
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
    IFNULL(s.COD_MODALIDADE, 0) AS COD_MODALIDADE,
    COUNT(*) AS N
FROM concluintes c
INNER JOIN enem e ON c.CPF_MASC = e.CPF_MASC
LEFT JOIN ingresso s ON c.CPF_MASC = s.CPF_MASC
GROUP BY e.FAIXA_RENDA, MORADORES_GRUPO, COD_MODALIDADE",
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
  # RIGOR: valor exato, nao arredondado (pedido do autor 2026-09-22).
  cat(sprintf("    linhas=%d  row_loss_exato=%s\n", nrow(r),
              if (is.na(rl)) "NA" else format(100 * rl, digits = 10)))
  if (!is.na(rl) && rl > LIMITE_ROW_LOSS) {
    cat(sprintf("    [ATENCAO] row_loss acima de %.0f%%: use como robustez, nao como primario.\n",
                100 * LIMITE_ROW_LOSS))
  }
  utils::write.csv(r, file.path(OUT, arquivo), row.names = FALSE)
  cat("    gravado:", arquivo, "\n")
  invisible(r)
}

for (ano in c(2013, 2020)) {
  cat("\n", strrep("=", 70), "\nCOORTE ", ano, " (EaD x presencial)\n",
      strrep("=", 70), "\n", sep = "")
  executar(sql_coorte_fina(ano), sprintf("%d grade fina (renda per capita x modalidade)", ano),
           sprintf("matriz_%d_fina_ead%s.csv", ano, SUFIXO))
}

cat("\nRegistrado em data-raw/sedap/log_consultas_sedap.tsv\n")
