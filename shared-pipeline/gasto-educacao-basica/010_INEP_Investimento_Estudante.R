# ==============================================================================
# SCRIPT: shared-pipeline/gasto-educacao-basica/010_INEP_Investimento_Estudante.R
# ODA: novo em 2026-11-14, para o post gasto-estudante-educacao-basica. Refaz
#      a leitura usada em 4-DA-Code/2026-11_Gasto-Educacao-Basica/R/
#      030_Plot_Gasto_Estudante_Basica_en.R, da dissertacao.
#
# Baixa do INEP o indicador "Investimento publico direto em educacao por
# estudante, por nivel de ensino, em valores nominais" (Indicadores Financeiros
# Educacionais, 2000-2021) e o IPCA medio anual do SIDRA (tabela 1737).
#
# FONTES
#   INEP: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/
#         indicadores-educacionais/indicadores-financeiros-educacionais
#         arquivo Investimento_estudante_valor_nominal.zip (uma planilha .xlsx).
#         Investimento direto da Uniao, estados e municipios: pessoal ativo,
#         outras despesas correntes e investimento; sem aposentadorias, bolsas,
#         financiamento estudantil, divida e transferencias ao setor privado.
#   IBGE: SIDRA, tabela 1737, IPCA numero-indice (variavel 2266).
#
# POR QUE A PLANILHA NOMINAL: a planilha "valor real" do INEP mistura precos de
# 2018 e de 2021 entre as edicoes; aqui a nominal e deflacionada depois, no
# plot.R do post, pelo IPCA medio anual, base 2024.
#
# SAIDAS (redistribuidas com o post):
#   posts/gasto-estudante-educacao-basica/data/inep_investimento_estudante_nominal.csv
#   posts/gasto-estudante-educacao-basica/data/ipca_media_anual.csv
# Bruto em data-raw/gasto-educacao-basica/.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(readxl)
  library(dplyr)
  library(here)
})

URL_INEP <- paste0("https://download.inep.gov.br/informacoes_estatisticas/",
                   "investimentos_publicos_em_educacao/indicadores_financeiros_educacionais/",
                   "Investimento_estudante_valor_nominal.zip")
DIR_RAW <- here::here("data-raw", "gasto-educacao-basica")
DIR_POST <- here::here("posts", "gasto-estudante-educacao-basica", "data")
dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_POST, recursive = TRUE, showWarnings = FALSE)

# ── 1. INEP ─────────────────────────────────────────────────────────────────
zip <- file.path(DIR_RAW, "Investimento_estudante_valor_nominal.zip")
if (!file.exists(zip)) {
  tmp <- tempfile(fileext = ".zip")
  request(URL_INEP) |>
    req_user_agent("open-data-analysis (R httr2)") |>
    req_timeout(300) |>
    req_retry(max_tries = 4, backoff = function(i) 10 * i) |>
    req_perform(path = tmp)
  stopifnot(identical(readBin(tmp, "raw", 2), charToRaw("PK")))
  file.copy(tmp, zip)
  unlink(tmp)
}
xlsx <- unzip(zip, exdir = DIR_RAW)
xlsx <- xlsx[grepl("\\.xlsx$", xlsx, ignore.case = TRUE)]
stopifnot(length(xlsx) == 1)

m <- as.data.frame(suppressMessages(read_excel(xlsx, col_names = FALSE,
                                               .name_repair = "minimal")))
m <- m[grepl("^20[0-9]{2}", m[[1]]), ]
x <- as.data.frame(lapply(m, function(v) suppressWarnings(as.numeric(gsub("[^0-9.]", "", v)))))
# Colunas da planilha: ano, todos os niveis, educacao basica, educacao infantil,
# anos iniciais e anos finais do fundamental, ensino medio, educacao superior.
stopifnot(ncol(x) == 8)
names(x) <- c("ano", "todos", "basica", "infantil", "ef1", "ef2", "em", "superior")
x$ano <- as.integer(substr(m[[1]], 1, 4))
stopifnot(!anyNA(x[c("ano", "basica", "em")]), !anyDuplicated(x$ano))
write.csv(x[c("ano", "basica", "infantil", "ef1", "ef2", "em")],
          file.path(DIR_POST, "inep_investimento_estudante_nominal.csv"), row.names = FALSE)

# ── 2. IPCA medio anual ─────────────────────────────────────────────────────
# API do SIDRA chamada direto (o pacote sidrar falhou ao conectar em 2026-11-14).
CACHE_IPCA <- file.path(DIR_RAW, "sidra_1737_v2266.json")
if (!file.exists(CACHE_IPCA)) {
  request(paste0("https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2266/p/199101-",
                 format(Sys.Date(), "%Y%m"))) |>
    req_timeout(300) |>
    req_retry(max_tries = 4, backoff = function(i) 10 * i) |>
    req_perform(path = CACHE_IPCA)
}
sidra <- jsonlite::fromJSON(CACHE_IPCA)[-1, ]  # a primeira linha e o cabecalho
ipca <- data.frame(mes = sidra$D3C, indice = as.numeric(sidra$V)) |>
  transmute(ano = as.integer(substr(mes, 1, 4)), indice) |>
  group_by(ano) |>
  filter(n() == 12) |>
  summarise(indice = mean(indice), .groups = "drop")
stopifnot(all(2000:2024 %in% ipca$ano))
write.csv(as.data.frame(ipca), file.path(DIR_POST, "ipca_media_anual.csv"), row.names = FALSE)
message("Dados do post gravados em ", DIR_POST)
