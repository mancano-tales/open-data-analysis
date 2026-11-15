# ==============================================================================
# SCRIPT: shared-pipeline/gasto-federal/020_ProUni_Renuncia_RFB.R
# ODA: portado em 2026-11-14 de 4-DA-Code/2026-10_Gasto-Federal-ES/R/
#      020_ProUni_Renuncia_RFB.R, da dissertacao; caminhos relativos a data-raw/.
#
# Serie anual da renuncia fiscal do ProUni (IRPJ, CSLL, PIS-Pasep e Cofins das
# instituicoes que aderem ao programa), 2006-2023, em reais correntes.
#
# FONTE: Receita Federal (CETAD), "Demonstrativo dos Gastos Tributarios -
# Bases Efetivas", edicoes de 2008 a 2023, baixadas por
# 015_Download_RFB_Gastos_Tributarios.R.
#
# REGRA DE ESCOLHA: a edicao X traz o ano-base X como estimativa efetiva (os
# anos X+1 a X+3 sao projecoes). Para cada ano usa-se a edicao do proprio ano.
# As revisoes posteriores sao pequenas: 2014 e 2015 aparecem com o mesmo valor
# nas edicoes de 2014/2015 (PDF) e de 2016/2017 (planilha), o que o script
# confere. 2006 e 2007 vem da edicao de 2008, a primeira publicada. 2005 (ano de
# criacao do programa) nao aparece em nenhuma edicao. 2024 so existe como
# projecao da edicao de 2023 e fica de fora da serie.
#
# EDICOES EM PDF (2008-2015): o texto extraido desalinha as colunas das tabelas,
# entao os valores foram transcritos dos quadros de descricao legal por tributo
# (uma linha "PROUNI - Programa Universidade para Todos" por tributo) e, para
# 2006-2007, dos quadros regionais por ano. A transcricao, com pagina e quadro
# de cada valor, esta versionada ao lado deste script (prouni_transcricao_pdf.csv)
# e pode ser conferida contra os PDFs baixados.
# EDICOES EM PLANILHA (2016-2023): leitura automatica dos quadros regionais
# por ano ("POR FUNCAO ORCAMENTARIA ... REGIONALIZADO - <ano>" ou "BASES
# EFETIVAS <ano>"), linha PROUNI, ultima coluna numerica (Total).
#
# SAIDA: data-raw/gasto-federal/series/prouni_renuncia_rfb.csv (serie escolhida)
# e prouni_renuncia_rfb_todas_edicoes.csv (todas as safras, para conferencia).
# PROXIMO: 030_Monta_Series_Gasto.R
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

DIR_RFB <- here::here("data-raw", "gasto-federal", "rfb")
DIR_SERIES <- here::here("data-raw", "gasto-federal", "series")
TRANSCRICAO <- here::here("shared-pipeline", "gasto-federal", "prouni_transcricao_pdf.csv")
dir.create(DIR_SERIES, recursive = TRUE, showWarnings = FALSE)
if (length(list.files(DIR_RFB, pattern = "quadros\\.xlsx$")) < 8) {
  stop("Edicoes da Receita nao encontradas em data-raw/gasto-federal/rfb/. Rode antes: ",
       "shared-pipeline/gasto-federal/015_Download_RFB_Gastos_Tributarios.R", call. = FALSE)
}

# ── 1. Edicoes em PDF: transcricao documentada ──────────────────────────────
transc <- read_csv(TRANSCRICAO, col_types = cols(valor = "d", .default = "c")) |>
  mutate(edicao = as.integer(edicao), ano_base = as.integer(ano_base))
stopifnot(all(transc$tributo[transc$ano_base >= 2008] %in%
              c("IRPJ", "PIS", "CSLL", "COFINS")))
# Cada ano-base de 2008 a 2015 precisa dos quatro tributos.
stopifnot(all(table(transc$ano_base[transc$ano_base >= 2008]) == 4))

pdf_anos <- transc |>
  group_by(edicao, ano = ano_base) |>
  summarise(valor = sum(valor), .groups = "drop") |>
  mutate(formato = "pdf")

# ── 2. Edicoes em planilha: leitura dos quadros regionais por ano ───────────
ler_edicao_xlsx <- function(arq) {
  edicao <- as.integer(str_match(basename(arq), "efetivas-(\\d{4})")[, 2])
  out <- list()
  for (s in excel_sheets(arq)) {
    x <- suppressMessages(read_excel(arq, sheet = s, col_names = FALSE,
                                     col_types = "text", n_max = 400))
    titulo <- paste(na.omit(unlist(x[1:4, ])), collapse = " ")
    # So os quadros por funcao orcamentaria regionalizados: um ano por quadro,
    # com a linha PROUNI e o total na ultima coluna numerica.
    if (!str_detect(titulo, "REGIONALIZADO") ||
        !str_detect(titulo, "FUN[CÇ][AÃ]O OR[CÇ]AMENT[AÁ]RIA")) next
    ano <- as.integer(str_match(titulo, "(?:EFETIVAS|REGIONALIZADO -) ?(20\\d{2})")[, 2])
    linha <- which(apply(x, 1, function(r) any(str_trim(r) == "PROUNI", na.rm = TRUE)))
    if (is.na(ano) || length(linha) != 1) next
    nums <- suppressWarnings(as.numeric(unlist(x[linha, ])))
    nums <- nums[!is.na(nums)]
    # Cinco regioes + total; o total precisa bater com a soma das regioes.
    stopifnot(length(nums) == 6, abs(sum(nums[1:5]) - nums[6]) < 10)
    out[[length(out) + 1]] <- tibble(edicao = edicao, ano = ano, valor = nums[6],
                                     quadro = s)
  }
  bind_rows(out) |> distinct(edicao, ano, .keep_all = TRUE)
}

arqs_xlsx <- list.files(DIR_RFB, pattern = "quadros\\.xlsx$", full.names = TRUE)
xlsx_anos <- bind_rows(lapply(arqs_xlsx, ler_edicao_xlsx)) |>
  mutate(formato = "xlsx")
# Toda edicao em planilha precisa trazer o proprio ano-base.
stopifnot(all(2016:2023 %in% xlsx_anos$edicao),
          all(sapply(2016:2023, function(e) e %in% xlsx_anos$ano[xlsx_anos$edicao == e])))

todas <- bind_rows(pdf_anos, xlsx_anos) |> arrange(ano, edicao)

# ── 3. Conferencia entre safras ─────────────────────────────────────────────
# 2014 e 2015: edicao do proprio ano (PDF, soma dos tributos) contra a planilha
# de 2016 (quadro regional), que os traz como efetivos.
for (a in 2014:2015) {
  v_pdf <- todas$valor[todas$ano == a & todas$edicao == a]
  v_xls <- todas$valor[todas$ano == a & todas$edicao == 2016]
  stopifnot(length(v_pdf) == 1, length(v_xls) == 1, abs(v_pdf - v_xls) < 10)
}

# ── 4. Serie escolhida ──────────────────────────────────────────────────────
serie <- todas |>
  filter((ano <= 2007 & edicao == 2008) | (ano >= 2008 & edicao == ano)) |>
  select(ano, edicao, valor_corrente = valor, formato) |>
  arrange(ano)
stopifnot(identical(serie$ano, 2006:2023), !anyDuplicated(serie$ano))

write_csv(todas, file.path(DIR_SERIES, "prouni_renuncia_rfb_todas_edicoes.csv"))
write_csv(serie, file.path(DIR_SERIES, "prouni_renuncia_rfb.csv"))
print(serie, n = Inf)
