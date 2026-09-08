# ==============================================================================
# 010_SEDAP_Cliente.R
#
# Cliente para a API do SEDAP+ (INEP/RNP), o canal de acesso protegido aos
# microdados que sairam da publicacao aberta.
#
#   Plataforma:  https://plataformasedap.inep.gov.br/
#   Anuncio:     https://www.rnp.br/2026/04/30/sedap-nova-plataforma-amplia-
#                acesso-seguro-a-dados-educacionais-e-marca-avanco-na-pesquisa-
#                no-pais/
#
# ATENCAO AO NOME: a plataforma e o SEDAP+, nao "SEDAP". O acesso e concedido
# por projeto de pesquisa, com lista de bases deferidas — mas veja o limite 6
# abaixo: a lista nao e confiavel como inventario do que responde.
#
# Motivo de existir: o INEP
# deixou de publicar o microdado de ALUNO do Censo da Educacao Superior
# depois de 2019, e e dele que depende o cruzamento
# setor x via de acesso x turno/modalidade usado em
# @fig-matriculas-turno-via-acesso, hoje limitada a 2010-2019.
#
# ------------------------------------------------------------------------------
# CREDENCIAL — LEIA ANTES DE USAR
# ------------------------------------------------------------------------------
# O token do SEDAP+ e um JWT pessoal que carrega nome, e-mail e CPF do
# pesquisador em claro (base64, nao criptografia) e vale ~7 dias. Ele NUNCA
# entra neste arquivo nem em qualquer arquivo versionado. Defina no ambiente
# antes de rodar:
#
#   Sys.setenv(SEDAP_TOKEN = "eyJ...")        # no console, sessao a sessao
#   ou usethis::edit_r_environ()               # persistente, em ~/.Renviron
#
# Se um token vazar (colado em conversa, commitado por engano), gere outro na
# interface do SEDAP+ — revogar e mais barato que auditar o estrago.
# ------------------------------------------------------------------------------
#
# PROTOCOLO DA API (descoberto por sondagem em 2026-08-05, nao documentado
# em lugar nenhum que eu tenha achado):
#
#   1. O corpo do POST aceita UM unico campo, "content", com o SQL. Qualquer
#      outro campo e rejeitado com 400 ("<campo> is not allowed") — o
#      validador tem allowlist estrita.
#
#   2. Os parametros de privacidade diferencial vao DENTRO do SQL, num hint
#      de comentario antes do SELECT:
#          /*+ epsilon=1.0, delta=1e-6 */ SELECT ...
#      Sem ele: 422 "Os parametros de calculo da privacidade diferencial
#      precisam estar definidos."
#
#   3. Toda consulta precisa conter funcao de agregacao. SELECT de linha crua
#      e recusado com 422 ("A consulta nao contem funcoes de agregacao
#      validas"). Isso tambem inutiliza information_schema — nao ha como
#      listar tabelas pelo caminho normal; descoberta e por tentativa com
#      COUNT(*).
#
#   4. A resposta traz, alem de `rows`, um bloco `bqconnector_statistics`
#      com noise_rate, row_loss, group_loss e intervalo de confianca por
#      coluna agregada. LEIA ESSE BLOCO: e ele que diz se o resultado e
#      utilizavel ou se o ruido comeu o sinal.
#
#   5. Tabela nao habilitada no projeto responde "Acesso negado. Voce esta
#      tentando acessar uma base nao selecionada em seu projeto" — o que e
#      diferente de tabela inexistente. Habilitacao se faz na interface web.
#
#   6. NOMES DAS TABELAS: a familia ENEM leva sufixo (ENEM_2022_SEDAP), a do
#      superior NAO (SUP_ALUNO_2024, SUP_CURSO_2023, SUP_IES_2024). Sondar
#      com o sufixo errado devolve a mesma mensagem de "base nao selecionada"
#      de uma tabela realmente ausente do projeto — as duas situacoes sao
#      indistinguiveis pela resposta. Confira a lista de bases deferidas na
#      interface antes de concluir que falta acesso.
#
#   7. LIMITES DO PARSER (empiricos, todos devolvem o mesmo erro inutil
#      "TypeError: Cannot read properties of undefined (reading 'type')"):
#        - GROUP BY aceita SO colunas cruas. Nada de posicional (GROUP BY 1,2),
#          nada de expressao ou CASE, nada de alias.
#        - GROUP BY suporta ~5 colunas; 7 quebra.
#        - WHERE aceita cadeia de AND com igualdade simples. NAO aceita OR,
#          IS NULL nem parenteses.
#      Consequencia pratica: categorias derivadas (setor, via de acesso,
#      turno agrupado) tem de ser montadas LOCALMENTE, depois do download, ou
#      fatiadas em varias consultas com WHERE. Ver 020_.
#
# ESTADO DO ACESSO (reverificado 2026-08-08 por COUNT(*) em cada tabela):
# SUP_ALUNO responde em TODOS os anos de 2009 a 2024, INCLUSIVE 2020-2023.
#
#   SUP_ALUNO_2020  12.565.383   SUP_ALUNO_2022  14.398.996
#   SUP_ALUNO_2021  13.204.606   SUP_ALUNO_2023  15.196.572
#
# SOBRE A LISTA DE BASES DEFERIDAS: a lista exibida na interface do SEDAP+
# (conferida em 2026-08-08) vai de SUP_ALUNO_2009 a _2019, mais _2024, e nao
# mostra 2020-2023. Segundo o autor, esses anos FORAM solicitados junto com os
# demais — a lista da interface e que esta desatualizada, nao o acesso. O autor
# comunicou o INEP e vai atualizar o cadastro. O log em log_consultas_sedap.tsv
# registra toda consulta executada e serve de rastro para a conferencia
# posterior do INEP.
#
# A @fig-matriculas-turno-via-acesso NAO estava travada: 020_Extrair_Nicho_
# 2020_2024.R ja cobria 2010-2024 desde 2026-08-05, tendo verificado por conta
# propria que essas tabelas respondem (ver a nota na linha ~303 daquele script).
# A redacao anterior deste cabecalho, que dava a figura como limitada a
# 2010-2019, ficou defasada em relacao ao trabalho ja feito.
#
# LICAO QUE PERMANECE: a lista de bases deferidas nao e inventario confiavel do
# que responde. Sonde com COUNT(*) antes de concluir que falta acesso — foi
# assim que os quatro anos apareceram.
#
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

SEDAP_URL <- Sys.getenv(
  "SEDAP_URL",
  paste0(
    "https://backend.rpinep2.prd.app.rnp.br/ide/send-process/",
    "46074741-17fd-4ccc-8222-500fccc66b1e"
  )
)
SEDAP_PROFILE <- Sys.getenv("SEDAP_PROFILE", "1a5854d4-1764-4ac2-9adc-1d173362cfa8")

sedap_token <- function() {
  tk <- Sys.getenv("SEDAP_TOKEN", "")
  if (!nzchar(tk)) {
    stop(
      "SEDAP_TOKEN nao definido. Ver o cabecalho deste arquivo: o token e ",
      "pessoal, vale ~7 dias e nunca deve ser escrito em arquivo versionado.",
      call. = FALSE
    )
  }
  tk
}

#' Executa uma consulta no SEDAP+ sob privacidade diferencial.
#'
#' @param sql        SQL com agregacao. O hint de DP e inserido automaticamente
#'                   se ainda nao estiver presente.
#' @param epsilon    Orcamento de privacidade. Menor = mais ruido, mais
#'                   protecao. Cada consulta consome orcamento; nao varra
#'                   parametros a esmo.
#' @param delta      Probabilidade de falha da garantia.
#' @param verbose    Imprime o bloco de estatisticas de privacidade.
#' @return data.frame com as linhas, e o bloco de estatisticas no atributo
#'         "dp_stats". NULL em caso de erro (a mensagem da API e impressa).
#' Caminho do log de consultas.
#'
#' POR QUE ESTE LOG EXISTE: os dados do SEDAP+ nao podem ser reabertos e
#' conferidos como um arquivo local — a plataforma so responde a consultas
#' agregadas, e o resultado depende do recorte exato pedido (a supressao de
#' celulas pequenas muda com a granularidade). Sem o SQL literalmente
#' executado, um numero publicado na tese e irreproduzivel: nem o autor
#' consegue voltar e verificar de onde veio. O script sozinho nao basta,
#' porque as consultas sao montadas por sprintf em tempo de execucao.
#'
#' O log grava, para cada chamada: timestamp, SQL exato, parametros de
#' privacidade, e as estatisticas de perda devolvidas pela API.
SEDAP_LOG <- here::here("4-DA-Code", "2026-08_SEDAP", "log_consultas_sedap.tsv")

.sedap_registrar <- function(sql, epsilon, delta, linhas, row_loss, group_loss) {
  dir.create(dirname(SEDAP_LOG), showWarnings = FALSE, recursive = TRUE)
  novo <- !file.exists(SEDAP_LOG)
  reg <- data.frame(
    executado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    epsilon = epsilon,
    delta = delta,
    linhas_retornadas = linhas %||% NA_integer_,
    row_loss = row_loss %||% NA_real_,
    group_loss = group_loss %||% NA_real_,
    # O SQL vai numa celula so; quebras de linha viram \n literal para o TSV
    # continuar com uma consulta por linha.
    sql = gsub("[\r\n]+", "\\\\n", trimws(sql)),
    stringsAsFactors = FALSE
  )
  utils::write.table(reg, SEDAP_LOG,
    sep = "\t", row.names = FALSE, col.names = novo,
    append = !novo, quote = TRUE, fileEncoding = "UTF-8"
  )
}

sedap_query <- function(sql, epsilon = 1.0, delta = 1e-6, verbose = TRUE) {
  if (!grepl("/\\*\\+", sql)) {
    sql <- sprintf("/*+ epsilon=%s, delta=%s */\n%s", epsilon, format(delta, scientific = TRUE), sql)
  }

  res <- POST(
    url = SEDAP_URL,
    body = list(content = sql),
    encode = "json",
    add_headers(
      Authorization = paste("Bearer", sedap_token()),
      "Content-Type" = "application/json",
      "profile-id" = SEDAP_PROFILE
    ),
    timeout(600)
  )

  txt <- content(res, "text", encoding = "UTF-8")

  if (http_error(res)) {
    msg <- tryCatch(fromJSON(txt)$message, error = function(e) substr(txt, 1, 400))
    message("SEDAP+ HTTP ", status_code(res), ": ", msg)
    return(invisible(NULL))
  }

  parsed <- fromJSON(txt)
  if (verbose && !is.null(parsed$bqconnector_statistics)) {
    dp <- parsed$bqconnector_statistics$differential_privacy
    message(
      "[DP] row_loss = ", dp$row_loss %||% NA,
      " | group_loss = ", dp$group_loss %||% NA
    )
  }

  out <- as.data.frame(parsed$rows)
  attr(out, "dp_stats") <- parsed$bqconnector_statistics

  dp <- parsed$bqconnector_statistics$differential_privacy
  .sedap_registrar(sql, epsilon, delta, nrow(out), dp$row_loss, dp$group_loss)

  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Testa se uma tabela esta habilitada no projeto.
#' Distingue os tres estados: habilitada, nao selecionada, inexistente.
sedap_tem_tabela <- function(tbl) {
  r <- suppressMessages(sedap_query(sprintf("SELECT COUNT(*) AS N FROM %s", tbl), verbose = FALSE))
  if (is.null(r)) FALSE else TRUE
}
