# ==============================================================================
# SCRIPT: shared-pipeline/gasto-federal/015_Download_RFB_Gastos_Tributarios.R
# ODA: novo em 2026-11-14. Na dissertacao, as edicoes foram baixadas a mao em
#      2026-10-10; este script refaz o download pelos mesmos enderecos.
#
# Baixa as edicoes de 2008 a 2023 do "Demonstrativo dos Gastos Tributarios -
# Bases Efetivas" da Receita Federal (CETAD), de onde sai a renuncia fiscal do
# ProUni. Pagina:
#   https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/
#   relatorios/renuncia/gastos-tributarios-bases-efetivas
# Cada arquivo e servido em <pagina>/<identificador>/@@download/file. O
# identificador e o nome do arquivo sem extensao ate a edicao de 2018 e com a
# extensao .xlsx a partir de 2019 (conferido na pagina em 2026-11-14). Os
# arquivos sao gravados com a extensao, como na dissertacao.
# As edicoes de 2008 a 2015 sao PDF; as de 2016 a 2023, planilhas de quadros.
#
# SAIDA: data-raw/gasto-federal/rfb/ (16 arquivos, cerca de 33 MB).
# PROXIMO: 020_ProUni_Renuncia_RFB.R
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(here)
})

PAGINA <- paste0("https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/",
                 "publicacoes/relatorios/renuncia/gastos-tributarios-bases-efetivas")
# nome do arquivo local = identificador na pagina
ARQUIVOS <- c(
  "dgt-bases-efetivas-2008-serie-2006-a-2010.pdf" = "dgt-bases-efetivas-2008-serie-2006-a-2010",
  "dgt-bases-efetivas-2009-serie-2007-a-2011.pdf" = "dgt-bases-efetivas-2009-serie-2007-a-2011",
  "dgt-bases-efetivas-2010-serie-2008-a-2012.pdf" = "dgt-bases-efetivas-2010-serie-2008-a-2012",
  "dgt-bases-efetivas-2011-serie-2009-a-2013-final-ret-1-29_09_2014.pdf" =
    "dgt-bases-efetivas-2011-serie-2009-a-2013-final-ret-1-29_09_2014",
  "dgt-bases-efetivas-2012-serie-2010-a-2014.pdf" = "dgt-bases-efetivas-2012-serie-2010-a-2014",
  "dgt-bases-efetivas-2013-serie-2011-a-2016.pdf" = "dgt-bases-efetivas-2013-serie-2011-a-2016",
  "dgt-bases-efetivas-2014-serie-2012-a-2017.pdf" = "dgt-bases-efetivas-2014-serie-2012-a-2017",
  "dgt-bases-efetivas-2015-serie-2013-a-2018.pdf" = "dgt-bases-efetivas-2015-serie-2013-a-2018",
  "dgt-bases-efetivas-2016-serie-2014-a-2019-quadros.xlsx" = "dgt-bases-efetivas-2016-serie-2014-a-2019-quadros",
  "dgt-bases-efetivas-2017-serie-2015-a-2020-quadros.xlsx" = "dgt-bases-efetivas-2017-serie-2015-a-2020-quadros",
  "dgt-bases-efetivas-2018-serie-2016-a-2021-quadros.xlsx" = "dgt-bases-efetivas-2018-serie-2016-a-2021-quadros",
  "dgt-bases-efetivas-2019-serie-2017-a-2022-quadros.xlsx" = "dgt-bases-efetivas-2019-serie-2017-a-2022-quadros.xlsx",
  "dgt-bases-efetivas-2020-serie-2018-a-2023-quadros.xlsx" = "dgt-bases-efetivas-2020-serie-2018-a-2023-quadros.xlsx",
  "dgt-bases-efetivas-2021-serie-2019-a-2024-quadros.xlsx" = "dgt-bases-efetivas-2021-serie-2019-a-2024-quadros.xlsx",
  "dgt-bases-efetivas-2022-serie-2020-a-2025-quadros.xlsx" = "dgt-bases-efetivas-2022-serie-2020-a-2025-quadros.xlsx",
  "dgt-bases-efetivas-2023-serie-2021-a-2026-quadros.xlsx" = "dgt-bases-efetivas-2023-serie-2021-a-2026-quadros.xlsx"
)
DIR_RFB <- here::here("data-raw", "gasto-federal", "rfb")
dir.create(DIR_RFB, recursive = TRUE, showWarnings = FALSE)

for (arq in names(ARQUIVOS)) {
  destino <- file.path(DIR_RFB, arq)
  if (file.exists(destino) && file.size(destino) > 1000) next
  message("Baixando ", arq)
  # Baixa num temporario: um erro HTTP nao deixa arquivo invalido no destino.
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(arq)))
  request(paste0(PAGINA, "/", ARQUIVOS[[arq]], "/@@download/file")) |>
    req_user_agent("open-data-analysis (R httr2)") |>
    req_timeout(300) |>
    req_retry(max_tries = 4, backoff = function(i) 10 * i) |>
    req_perform(path = tmp)
  # Confere que veio o arquivo, e nao uma pagina de erro.
  cabeca <- readBin(tmp, "raw", 4)
  esperado <- if (grepl("\\.pdf$", arq)) charToRaw("%PDF") else as.raw(c(0x50, 0x4b, 0x03, 0x04))
  if (!identical(cabeca, esperado)) {
    stop("O download de ", arq, " nao trouxe o arquivo esperado.", call. = FALSE)
  }
  file.copy(tmp, destino, overwrite = TRUE)
  unlink(tmp)
}
message("Concluido: ", DIR_RFB)
