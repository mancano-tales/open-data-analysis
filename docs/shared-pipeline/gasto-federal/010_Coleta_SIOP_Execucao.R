# ==============================================================================
# SCRIPT: shared-pipeline/gasto-federal/010_Coleta_SIOP_Execucao.R
# ODA: portado em 2026-11-14 de 4-DA-Code/2026-10_Gasto-Federal-ES/R/
#      010_Coleta_SIOP_Execucao.R, da dissertacao; caminhos relativos a data-raw/.
#
# Coleta a execucao orcamentaria da Uniao, 2000-2025, para as unidades
# orcamentarias (UO) do Ministerio da Educacao (codigos 26xxx) e para a UO do
# FIES a partir de 2004 (74902, orgao 74000 Operacoes Oficiais de Credito).
# Antes de 2004 o FIES estava na UO 26907, ja coberta pelo filtro 26xxx.
#
# FONTE: SIOP (Secretaria de Orcamento Federal), dados abertos do orcamento em
# RDF, consultados pelo endpoint SPARQL publico
#   http://orcamento.dados.gov.br/sparql/
# (o mesmo conteudo responde em https://www1.siop.planejamento.gov.br/sparql/).
# Vocabulario: http://vocab.e.gov.br/2013/09/loa# . Cada loa:ItemDespesa traz
# exercicio, UO, acao, GND e os valores de dotacao inicial, lei mais creditos,
# empenhado, liquidado e pago no exercicio. Os restos a pagar pagos NAO estao
# no item: o "pago" e so o do exercicio, por isso as series usam o liquidado.
#
# SAIDA (bruta): data-raw/gasto-federal/siop/, uma tabela por ano com
# UO x acao x GND e os cinco valores, mais tabelas de rotulos de UO e de acao.
# Valores em reais correntes. A coleta usada na dissertacao e no post foi feita
# em 2026-10-10; o SIOP atualiza os anos recentes, entao uma coleta nova pode
# diferir nos ultimos anos.
#
# USO: Rscript shared-pipeline/gasto-federal/010_Coleta_SIOP_Execucao.R
#      Leva de 20 a 40 minutos. Anos ja gravados sao pulados (REFAZER = FALSE);
#      para coletar so alguns anos: SIOP_ANOS=2010:2012 antes do comando.
# PROXIMO: 030_Monta_Series_Gasto.R
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(here)
})

ENDPOINT <- "http://orcamento.dados.gov.br/sparql/"
ANOS <- if (nzchar(Sys.getenv("SIOP_ANOS"))) eval(parse(text = Sys.getenv("SIOP_ANOS"))) else 2000:2025
DIR_DADOS <- here::here("data-raw", "gasto-federal", "siop")
dir.create(DIR_DADOS, recursive = TRUE, showWarnings = FALSE)

PREFIXOS <- paste(
  "PREFIX loa: <http://vocab.e.gov.br/2013/09/loa#>",
  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
  sep = "\n"
)

consulta_sparql <- function(query) {
  resp <- request(ENDPOINT) |>
    req_url_query(query = paste(PREFIXOS, query, sep = "\n")) |>
    req_headers(Accept = "text/csv") |>
    req_timeout(600) |>
    req_retry(max_tries = 4, backoff = function(i) 10 * i) |>
    req_perform()
  read_csv(I(resp_body_string(resp)), col_types = cols(.default = "c"))
}

# REFAZER = FALSE pula os anos ja gravados (retomada apos falha de rede).
REFAZER <- FALSE

# Filtro de UO: todo o MEC (26xxx) mais o FIES em Operacoes Oficiais de Credito.
FILTRO_UO <- 'FILTER(STRSTARTS(?uoc, "26") || ?uoc = "74902")'

q_execucao <- function(ano) sprintf('
SELECT ?uoc ?acao ?gnd
       (SUM(?dot) AS ?dotacao_inicial) (SUM(?lei) AS ?lei_mais_creditos)
       (SUM(?emp) AS ?empenhado) (SUM(?liq) AS ?liquidado) (SUM(?pag) AS ?pago)
WHERE {
  ?i loa:temExercicio ?ex . ?ex loa:identificador %d .
  ?i loa:temUnidadeOrcamentaria ?uo . ?uo loa:codigo ?uoc . %s
  ?i loa:temAcao ?a . ?a loa:codigo ?acao .
  ?i loa:temGND ?g . ?g loa:codigo ?gnd .
  ?i loa:valorDotacaoInicial ?dot ; loa:valorLeiMaisCredito ?lei ;
     loa:valorEmpenhado ?emp ; loa:valorLiquidado ?liq ; loa:valorPago ?pag .
} GROUP BY ?uoc ?acao ?gnd', ano, FILTRO_UO)

q_rotulos_uo <- function(ano) sprintf('
SELECT DISTINCT ?uoc ?uo_nome ?orgao WHERE {
  ?i loa:temExercicio ?ex . ?ex loa:identificador %d .
  ?i loa:temUnidadeOrcamentaria ?uo . ?uo loa:codigo ?uoc . %s
  ?uo rdfs:label ?uo_nome . OPTIONAL { ?uo loa:temOrgao ?og . ?og loa:codigo ?orgao }
}', ano, FILTRO_UO)

q_rotulos_acao <- function(ano) sprintf('
SELECT DISTINCT ?acao ?acao_nome WHERE {
  ?i loa:temExercicio ?ex . ?ex loa:identificador %d .
  ?i loa:temUnidadeOrcamentaria ?uo . ?uo loa:codigo ?uoc . %s
  ?i loa:temAcao ?a . ?a loa:codigo ?acao . ?a rdfs:label ?acao_nome .
}', ano, FILTRO_UO)

for (ano in ANOS) {
  arq_exe <- file.path(DIR_DADOS, sprintf("siop_execucao_mec_fies_%d.csv", ano))
  if (!REFAZER && file.exists(arq_exe)) next
  message("SIOP ", ano)
  exe <- consulta_sparql(q_execucao(ano))
  uo <- consulta_sparql(q_rotulos_uo(ano))
  ac <- consulta_sparql(q_rotulos_acao(ano))
  stopifnot(nrow(exe) > 0, !anyDuplicated(exe[c("uoc", "acao", "gnd")]),
            all(exe$uoc %in% uo$uoc))
  # Algumas acoes vem sem rdfs:label no RDF (ex.: 2014). Nao impede a serie,
  # que e montada por UO e GND; so registra.
  sem_rotulo <- setdiff(exe$acao, ac$acao)
  if (length(sem_rotulo)) {
    message("  acoes sem rotulo em ", ano, ": ", paste(sem_rotulo, collapse = ", "))
  }
  write_csv(mutate(exe, ano = ano, .before = 1), arq_exe)
  write_csv(mutate(uo, ano = ano, .before = 1),
            file.path(DIR_DADOS, sprintf("siop_rotulos_uo_%d.csv", ano)))
  write_csv(mutate(ac, ano = ano, .before = 1),
            file.path(DIR_DADOS, sprintf("siop_rotulos_acao_%d.csv", ano)))
}

writeLines(c(
  paste("Coleta:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste("Endpoint:", ENDPOINT),
  paste("Anos:", min(ANOS), "-", max(ANOS)),
  "Script: shared-pipeline/gasto-federal/010_Coleta_SIOP_Execucao.R"
), file.path(DIR_DADOS, "_coleta.txt"))
message("Concluido: ", DIR_DADOS)
