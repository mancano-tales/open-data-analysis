# ==============================================================================
# SCRIPT: shared-pipeline/gasto-federal/030_Monta_Series_Gasto.R
# ODA: portado em 2026-11-14 de 4-DA-Code/2026-10_Gasto-Federal-ES/R/
#      030_Monta_Series_Gasto.R, da dissertacao; caminhos relativos a data-raw/.
#      O IPCA passa a vir do SIDRA aqui mesmo (com cache em data-raw/), em vez
#      do cache do projeto de mensalidades da dissertacao.
#
# Monta as series anuais de gasto federal com educacao terciaria,
# deflacionadas pelo IPCA (media anual, reais de 2024), com o governo de cada ano:
#
#   1. FIES: concessao de financiamento (acoes 0579 e 00IG), liquidado.
#      UO 26907 em 2000-2003 e 74902 de 2004 em diante. Exclui a administracao
#      do fundo (4556/20RZ) e a integralizacao de cotas dos fundos garantidores
#      (00M2), que nao sao credito ao estudante.
#   2. Universidades federais: soma das UOs de universidade (nome atual contendo
#      "Universidade", sem hospitais), liquidado, por grupo de despesa:
#      pessoal ativo, aposentadorias e pensoes (acoes 0181, 0182, 0C05, 0711,
#      00S6 dentro das UOs de universidade), outras despesas correntes (GND 3)
#      e investimentos e inversoes (GND 4 e 5).
#      QUEBRA EM 2009: os hospitais universitarios viram UOs proprias em 2009 e
#      a EBSERH entra em 2012. Por isso ha duas versoes: so universidades e
#      universidades + hospitais universitarios + EBSERH (comparavel a 2000-2008,
#      quando o gasto hospitalar estava dentro das universidades). O Hospital de
#      Clinicas de Porto Alegre (empresa publica, UO 26294 desde 2000) fica fora
#      das duas.
#      Conceito de Carvalho (2011, Tabela VII), a serie da figura: ver secao
#      abaixo.
#      1995-1999: TCU, Relatorio sobre as Contas do Governo de 1999, Anexo VI
#      ("IFES - Despesas Totais", valores correntes), IFES exceto CEFETs. O
#      salto de 1995 para 1996 (+85% nominal) nao e explicado pelo TCU e o ano
#      de 1995 fica marcado como nao comparavel.
#   3. ProUni: renuncia fiscal (020_ProUni_Renuncia_RFB.R), 2006-2023.
#
# ENTRADAS: data-raw/gasto-federal/siop/ (010) e
#           data-raw/gasto-federal/series/prouni_renuncia_rfb.csv (020).
# SAIDAS:   data-raw/gasto-federal/series/ (todas as series, detalhe por UO,
#           media por governo e o IPCA medio anual) e, redistribuidos com o post,
#           posts/gasto-federal-educacao-superior/data/gasto_federal_es_anual.csv
#           (as tres series da figura) e .../data/ipca_media_anual.csv.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(here)
})

DIR_BASE <- here::here("data-raw", "gasto-federal")
DIR_SIOP <- file.path(DIR_BASE, "siop")
DIR_SERIES <- file.path(DIR_BASE, "series")
DIR_POST <- here::here("posts", "gasto-federal-educacao-superior", "data")
ANO_BASE_PRECOS <- 2024
dir.create(DIR_SERIES, recursive = TRUE, showWarnings = FALSE)
if (length(list.files(DIR_SIOP, "^siop_execucao_mec_fies_\\d{4}\\.csv$")) < 26) {
  stop("Execucao do SIOP incompleta em data-raw/gasto-federal/siop/. Rode antes: ",
       "shared-pipeline/gasto-federal/010_Coleta_SIOP_Execucao.R", call. = FALSE)
}
if (!file.exists(file.path(DIR_SERIES, "prouni_renuncia_rfb.csv"))) {
  stop("Serie do ProUni ausente. Rode antes: ",
       "shared-pipeline/gasto-federal/020_ProUni_Renuncia_RFB.R", call. = FALSE)
}

# ── Deflator: IPCA medio anual (SIDRA, tabela 1737, numero-indice) ──────────
# API do SIDRA chamada direto (o pacote sidrar falhou ao conectar em 2026-11-14).
CACHE_IPCA <- file.path(DIR_BASE, "sidra_1737_v2266.json")
if (!file.exists(CACHE_IPCA)) {
  # Ate o mes corrente: a serie do SIOP vai ate 2025, que precisa do IPCA de 2025.
  httr2::request(paste0("https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2266/p/199101-",
                        format(Sys.Date(), "%Y%m"))) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 10 * i) |>
    httr2::req_perform(path = CACHE_IPCA)
}
sidra <- jsonlite::fromJSON(CACHE_IPCA)[-1, ]  # a primeira linha e o cabecalho
ipca <- data.frame(mes = sidra$D3C, indice = as.numeric(sidra$V)) |>
  transmute(ano = as.integer(substr(mes, 1, 4)), indice) |>
  group_by(ano) |>
  filter(n() == 12) |>   # so anos completos
  summarise(indice = mean(indice), .groups = "drop")
stopifnot(all(1995:ANO_BASE_PRECOS %in% ipca$ano))
fator_real <- function(ano) {
  ipca$indice[ipca$ano == ANO_BASE_PRECOS] / ipca$indice[match(ano, ipca$ano)]
}
write_csv(ipca, file.path(DIR_SERIES, "ipca_media_anual_sidra1737.csv"))

# ── Governo de cada ano (ano de execucao do orcamento) ──────────────────────
# 2016 vai para Temer (no cargo de maio em diante); 2015 e Dilma II.
governo_do_ano <- function(ano) {
  cut(ano, breaks = c(1994, 1998, 2002, 2006, 2010, 2014, 2015, 2018, 2022, 2026),
      labels = c("FHC I", "FHC II", "Lula I", "Lula II", "Dilma I", "Dilma II",
                 "Temer", "Bolsonaro", "Lula III"))
}

# ── SIOP ────────────────────────────────────────────────────────────────────
le_siop <- function(padrao) {
  bind_rows(lapply(list.files(DIR_SIOP, padrao, full.names = TRUE), read_csv,
                   col_types = cols(.default = "c")))
}
exe <- le_siop("^siop_execucao_mec_fies_\\d{4}\\.csv$") |>
  mutate(ano = as.integer(ano),
         across(c(dotacao_inicial, lei_mais_creditos, empenhado, liquidado, pago),
                as.numeric))
uo_nomes <- le_siop("^siop_rotulos_uo_\\d{4}\\.csv$") |>
  mutate(ano = as.integer(ano)) |>
  arrange(uoc, ano) |>
  group_by(uoc) |>
  summarise(nome_atual = last(uo_nome), .groups = "drop")
stopifnot(identical(sort(unique(exe$ano)), 2000:2025))

exe <- left_join(exe, uo_nomes, by = "uoc")
stopifnot(!anyNA(exe$nome_atual))

eh_hospital <- function(nome) {
  str_detect(nome, regex("hospital|maternidade|complexo hospitalar", ignore_case = TRUE))
}
exe <- exe |>
  mutate(grupo_uo = case_when(
    uoc %in% c("26907", "74902") ~ "FIES",
    uoc == "26443" ~ "EBSERH",
    uoc == "26294" ~ "HCPA",
    eh_hospital(nome_atual) ~ "Hospital universitario",
    str_detect(nome_atual, regex("universidade", ignore_case = TRUE)) ~ "Universidade",
    TRUE ~ "Outros MEC"
  ))

ACOES_FIES_CONCESSAO <- c("0579", "00IG")
ACOES_INATIVOS <- c("0181", "0182", "0C05", "0711", "00S6")

# FIES
fies <- exe |>
  filter(grupo_uo == "FIES") |>
  group_by(ano) |>
  summarise(concessao = sum(liquidado[acao %in% ACOES_FIES_CONCESSAO]),
            uo_total = sum(liquidado), .groups = "drop")
stopifnot(nrow(fies) == 26, all(fies$concessao > 0))

# Universidades (e hospitais), por componente
componente <- function(acao, gnd) {
  case_when(
    acao %in% ACOES_INATIVOS ~ "Aposentadorias e pensoes",
    gnd == "1" ~ "Pessoal ativo",
    gnd == "3" ~ "Outras despesas correntes",
    gnd %in% c("4", "5") ~ "Investimentos",
    TRUE ~ "Outros (juros, amortizacao, reserva)"
  )
}
univ_comp <- exe |>
  filter(grupo_uo %in% c("Universidade", "Hospital universitario", "EBSERH")) |>
  mutate(componente = componente(acao, gnd)) |>
  group_by(ano, grupo_uo, componente) |>
  summarise(liquidado = sum(liquidado), empenhado = sum(empenhado),
            .groups = "drop")

# Conceito de Carvalho (2011, Tabela VII): universidades, sem inativos, sem a
# contribuicao patronal intraorcamentaria ao RPPS (09HB, desde 2006), sem
# precatorios (0005) e, no GND 3, sem beneficios a servidores (2004, 2010, 2011,
# 2012) e servicos de hospitais de ensino (4086). Na sobreposicao 2000-2008,
# esta serie fica proxima da de Carvalho, como a figura mostra.
ACOES_FORA_GND1 <- c("09HB", "0005")
ACOES_FORA_GND3 <- c("2004", "2010", "2011", "2012", "4086")
univ_carvalho <- exe |>
  filter(grupo_uo == "Universidade", !acao %in% ACOES_INATIVOS,
         (gnd == "1" & !acao %in% ACOES_FORA_GND1) |
           (gnd == "3" & !acao %in% ACOES_FORA_GND3) |
           gnd %in% c("4", "5")) |>
  group_by(ano) |>
  summarise(valor_corrente = sum(liquidado), .groups = "drop")
stopifnot(nrow(univ_carvalho) == 26)

por_uo <- exe |>
  filter(grupo_uo %in% c("Universidade", "Hospital universitario", "EBSERH")) |>
  group_by(ano, uoc, nome_atual, grupo_uo) |>
  summarise(liquidado = sum(liquidado), empenhado = sum(empenhado),
            .groups = "drop")
write_csv(por_uo, file.path(DIR_SERIES, "universidades_por_uo_anual.csv"))

# ── TCU, 1995-1999: IFES exceto CEFETs, valores correntes ───────────────────
# Relatorio e Parecer Previo sobre as Contas do Governo da Republica, exercicio
# de 1999, Anexo VI "IFES* - Despesas Totais, 1995-1999", linha Total, colunas
# de valores correntes (pagina com o quadro na secao 6.6.4). O valor de 1998
# (6.261.432.907) coincide com o do Relatorio de 1998, secao 6.7.3. Acervo:
# https://sites.tcu.gov.br/acervo-de-contas-do-presidente/
tcu <- tibble(
  ano = 1995:1999,
  valor_corrente = c(3103746541, 5739294330, 6155633643, 6261432907, 7446244012)
)

# ── Montagem em formato longo ───────────────────────────────────────────────
soma_comp <- function(grupos, comps = NULL) {
  univ_comp |>
    filter(grupo_uo %in% grupos, is.null(comps) | componente %in% comps) |>
    group_by(ano) |>
    summarise(valor_corrente = sum(liquidado), .groups = "drop")
}
COMP_DISCRICIONARIO <- c("Outras despesas correntes", "Investimentos")

series <- bind_rows(
  fies |> transmute(ano, serie = "fies_concessao", valor_corrente = concessao,
                    fonte = "SIOP"),
  read_csv(file.path(DIR_SERIES, "prouni_renuncia_rfb.csv"), show_col_types = FALSE) |>
    transmute(ano, serie = "prouni_renuncia", valor_corrente, fonte = "RFB"),
  soma_comp("Universidade") |>
    mutate(serie = "univ_total", fonte = "SIOP"),
  tcu |> mutate(serie = "univ_total", fonte = "TCU"),
  soma_comp(c("Universidade", "Hospital universitario", "EBSERH")) |>
    mutate(serie = "univ_hu_total", fonte = "SIOP"),
  bind_rows(lapply(unique(univ_comp$componente), function(cp) {
    soma_comp("Universidade", cp) |> mutate(serie = paste0("univ_", cp), fonte = "SIOP")
  })),
  soma_comp("Universidade", COMP_DISCRICIONARIO) |>
    mutate(serie = "univ_discricionario", fonte = "SIOP"),
  soma_comp("Universidade", c("Pessoal ativo", COMP_DISCRICIONARIO)) |>
    mutate(serie = "univ_sem_inativos", fonte = "SIOP"),
  univ_carvalho |> mutate(serie = "univ_conceito_carvalho", fonte = "SIOP")
) |>
  mutate(valor_real = valor_corrente * fator_real(ano),
         preco_base = ANO_BASE_PRECOS,
         governo = governo_do_ano(ano),
         nao_comparavel = fonte == "TCU" & ano == 1995) |>
  arrange(serie, ano)

stopifnot(!anyNA(series$valor_real), !anyNA(series$governo),
          !anyDuplicated(series[c("serie", "ano")]))

write_csv(series, file.path(DIR_SERIES, "gasto_federal_es_anual.csv"))

# Resumo por governo: media anual em reais de 2024 (bilhoes)
resumo <- series |>
  filter(serie %in% c("fies_concessao", "prouni_renuncia", "univ_total",
                      "univ_hu_total", "univ_discricionario"), !nao_comparavel) |>
  group_by(serie, governo) |>
  summarise(anos = paste(range(ano), collapse = "-"),
            media_real_bi = round(mean(valor_real) / 1e9, 2), .groups = "drop") |>
  pivot_wider(id_cols = governo, names_from = serie, values_from = media_real_bi)
write_csv(resumo, file.path(DIR_SERIES, "gasto_federal_es_media_por_governo.csv"))
print(resumo)

# ── Dados redistribuidos com o post ─────────────────────────────────────────
dir.create(DIR_POST, recursive = TRUE, showWarnings = FALSE)
series |>
  filter(serie %in% c("univ_conceito_carvalho", "fies_concessao", "prouni_renuncia")) |>
  select(ano, serie, fonte, valor_corrente, valor_real, preco_base) |>
  as.data.frame() |>
  write.csv(file.path(DIR_POST, "gasto_federal_es_anual.csv"), row.names = FALSE)
write.csv(as.data.frame(ipca), file.path(DIR_POST, "ipca_media_anual.csv"), row.names = FALSE)
message("Dados do post gravados em ", DIR_POST)
