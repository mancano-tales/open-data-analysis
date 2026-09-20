# ==============================================================================
# SCRIPT: 022_PNADC_Visita5_2020_2021.R
# ODA: portado de 4-DA-Code/2026-06_Harmonizing-BR-Data/R/01_pipeline/ em 2026-09-20;
#      caminhos relativos a data-raw/ e bloco de download do FTP do IBGE (a versao
#      da tese espera os zips ja em disco). Upstream do post renda-mediana-domiciliar-pc
#      (pontos vazados de 2020-21) e do 238 (Gini/Palma 2020-21).
#
# OBJETIVO: extrair renda domiciliar per capita da PNADC Anual VISITA 5, para
#   cobrir 2020 e 2021 — os dois anos que a serie da tese nao tem porque o
#   IBGE nunca publicou `visita1` para eles (D11, razao 1; verificado no FTP
#   em 2026-08-09: Visita_1/Dados/ vai de 2012-2019 e 2022-2025, sem 2020-21).
#
# POR QUE A VISITA 5 RESOLVE (achado de 2026-08-09, a partir de uma objecao do
#   autor): a Visita_5 TEM `PNADC_2020_visita5` e `PNADC_2021_visita5`, e o
#   dicionario confirma que ela traz a VD5008 — "rendimento domiciliar per
#   capita (habitual de todos os trabalhos e efetivo de outras fontes)", que e
#   exatamente a variavel usada pela nossa pipeline. Isso e melhor que o
#   enxerto anterior via Salata et al. (2025), cuja renda ja vinha deflacionada
#   para uma base NAO DECLARADA e por isso servia so para indices
#   escala-invariantes, nunca para nivel. Aqui o valor e nominal e o deflator
#   e o nosso (IPCA -> jan/2024, D01), entao 2020-2021 entram tambem na serie
#   de RENDA MEDIA.
#
# 🚨 DOIS VIESES SE SOMAM NESSES DOIS ANOS, e a nota da figura deve dizer:
#   1. EFEITO DE MODO (D11, razao 2): em 2020 a coleta virou telefonica
#      (CATI), o que subenumera renda domiciliar e exclui domicilios sem
#      telefone — vies contra pobres e rurais.
#   2. EFEITO DE VISITA: a serie e construida sobre a visita 1; usar a visita 5
#      em dois anos mistura visitas. Este script MEDE esse efeito em 2019 e
#      2022, anos em que as duas visitas coexistem, antes de qualquer enxerto.
#   Por isso os dois anos entram marcados (ponto vazado), nunca como
#   observacoes de mesma qualidade.
#
# VARIAVEIS: V1032 (peso anual com calibracao pela projecao) e VD5008.
#   Atencao: na base ANUAL o peso NAO e V1028 (esse e o trimestral).
#   As posicoes fwf mudam de ano para ano — sao lidas do dicionario de cada
#   ano, nunca hardcoded.
#
# SAIDA: output/PNADC_Visita5_2019_2022.parquet (ano, peso, renda_dom_pcta)
# PLANO: 9-vers/plan/2026-08-08_Plano_Gini_Oficial_e_Gap_Pandemia.md (WP3)
# VER TAMBEM: 238 (serie oficial que consome isto), 021 (renda zero na PNAD)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(arrow); library(here)
})
options(scipen = 999)

RAW <- here::here("data-raw", "pnadc_raw")
OUT <- here::here("data-raw", "harmonizing-br-data", "output")
ANOS <- c(2019L, 2020L, 2021L, 2022L)

# ── ODA: download dos zips da visita 5 e dos dicionarios, se ausentes ──────
# Os nomes no FTP carregam um sufixo de data (ex.: PNADC_2020_visita5_20250822.zip)
# que muda a cada reponderacao; por isso lista-se o diretorio e escolhe-se por
# padrao, em vez de hardcodar o nome.
FTP_V5 <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/"
dir.create(RAW, showWarnings = FALSE, recursive = TRUE)

listar_ftp <- function(url) {
  html <- httr2::request(url) |> httr2::req_perform() |> httr2::resp_body_string()
  hrefs <- regmatches(html, gregexpr('href="[^"]+"', html))[[1]]
  unique(sub('"$', "", sub('^href="', "", hrefs)))
}

baixar_se_ausente <- function(ano) {
  tem_zip <- length(list.files(RAW, pattern = sprintf("PNADC_%d_visita5.*\\.zip$", ano))) > 0
  tem_dic <- length(list.files(RAW, pattern = sprintf("dicionario.*%d_visita5.*\\.xls$", ano))) > 0
  if (tem_zip && tem_dic) return(invisible(NULL))
  dados <- listar_ftp(paste0(FTP_V5, "Dados/"))
  docs  <- listar_ftp(paste0(FTP_V5, "Documentacao/"))
  zip_nome <- grep(sprintf("^PNADC_%d_visita5.*\\.zip$", ano), dados, value = TRUE)
  dic_nome <- grep(sprintf("^dicionario_PNADC_microdados_%d_visita5.*\\.xls$", ano), docs, value = TRUE)
  if (!length(zip_nome) || !length(dic_nome)) {
    stop("Visita 5 de ", ano, " nao encontrada no FTP do IBGE (", FTP_V5, ")", call. = FALSE)
  }
  if (!tem_zip) {
    cat(sprintf("  %d: baixando %s\n", ano, zip_nome[1]))
    httr2::request(paste0(FTP_V5, "Dados/", zip_nome[1])) |>
      httr2::req_timeout(3600) |>
      httr2::req_perform(path = file.path(RAW, zip_nome[1]))
  }
  if (!tem_dic) {
    cat(sprintf("  %d: baixando %s\n", ano, dic_nome[1]))
    httr2::request(paste0(FTP_V5, "Documentacao/", dic_nome[1])) |>
      httr2::req_perform(path = file.path(RAW, dic_nome[1]))
  }
  invisible(NULL)
}
invisible(lapply(ANOS, baixar_se_ausente))

# Le as posicoes fwf do dicionario .xls do IBGE. O layout e: col 1 = posicao
# inicial, col 2 = largura, col 3 = nome da variavel.
posicoes_do_dicionario <- function(dic, vars) {
  d <- suppressMessages(readxl::read_excel(dic, col_names = FALSE))
  linhas <- apply(d, 1, function(l) paste(l, collapse = " "))
  out <- lapply(vars, function(v) {
    i <- grep(paste0("(^|\\s)", v, "(\\s|$)"), linhas)
    if (!length(i)) return(NULL)
    campos <- suppressWarnings(as.numeric(unlist(d[i[1], 1:2])))
    if (any(is.na(campos))) return(NULL)
    data.frame(var = v, inicio = campos[1], largura = campos[2])
  })
  faltando <- vars[vapply(out, is.null, logical(1))]
  if (length(faltando)) {
    stop("variaveis nao localizadas no dicionario ", basename(dic), ": ",
         paste(faltando, collapse = ", "))
  }
  bind_rows(out)
}

extrair_ano <- function(ano) {
  zipf <- list.files(RAW, pattern = sprintf("PNADC_%d_visita5.*\\.zip$", ano),
                     full.names = TRUE)
  dic  <- list.files(RAW, pattern = sprintf("dicionario.*%d_visita5.*\\.xls$", ano),
                     full.names = TRUE)
  if (!length(zipf) || !length(dic)) {
    cat(sprintf("  %d: arquivos ausentes — pulando\n", ano)); return(NULL)
  }

  pos <- posicoes_do_dicionario(dic[1], c("V1032", "VD5008"))
  cat(sprintf("  %d: V1032 @%d(%d) | VD5008 @%d(%d)\n", ano,
              pos$inicio[1], pos$largura[1], pos$inicio[2], pos$largura[2]))

  # O .txt vive dentro do zip; extrair para tempdir e ler so as 2 colunas
  interno <- unzip(zipf[1], list = TRUE)
  alvo <- interno$Name[grepl("\\.txt$", interno$Name, ignore.case = TRUE)][1]
  tmp <- file.path(tempdir(), sprintf("v5_%d", ano))
  dir.create(tmp, showWarnings = FALSE)
  unzip(zipf[1], files = alvo, exdir = tmp, overwrite = TRUE)

  col_pos <- readr::fwf_positions(
    start     = pos$inicio,
    end       = pos$inicio + pos$largura - 1,
    col_names = pos$var
  )
  df <- read_fwf(file.path(tmp, alvo), col_pos,
                 col_types = cols(.default = col_double()), progress = FALSE)
  unlink(tmp, recursive = TRUE)

  df |>
    transmute(ano = ano, peso = V1032, renda_dom_pcta = VD5008) |>
    filter(!is.na(peso), peso > 0)
}

cat("Extraindo PNADC visita 5...\n")
v5 <- lapply(ANOS, extrair_ano) |> bind_rows()

if (!nrow(v5)) stop("nada extraido — conferir se o download terminou")

cat("\nLinhas por ano:\n")
print(as.data.frame(v5 |> count(ano)), row.names = FALSE)
cat("\nRenda zero (share ponderado):\n")
print(as.data.frame(v5 |> group_by(ano) |>
  summarise(pct_zero = round(100 * sum(peso[renda_dom_pcta == 0], na.rm = TRUE) /
                             sum(peso), 2),
            na = sum(is.na(renda_dom_pcta)), .groups = "drop")), row.names = FALSE)

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
write_parquet(v5, file.path(OUT, "PNADC_Visita5_2019_2022.parquet"))
cat("\nSalvo: output/PNADC_Visita5_2019_2022.parquet\n")
cat("\n═════ 022_PNADC_Visita5_2020_2021 concluido ═════\n")
