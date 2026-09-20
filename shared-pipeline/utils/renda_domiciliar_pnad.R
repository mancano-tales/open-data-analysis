# ==============================================================================
# renda_domiciliar_pnad.R
#
# Leitura do bruto da PNAD Anual (1992-2015) para reconstruir a renda
# DOMICILIAR per capita no universo OFICIAL (com renda zero), a partir do
# dicionario SAS do IBGE. Uma linha por domicilio, com o peso igual a SOMA dos
# pesos de pessoa dos moradores contados no denominador — o que equivale a
# ponderar por pessoa em qualquer estatistica calculada sobre a saida.
#
#   renda_dom_pcta = V4721 / (moradores com V0401 fora de {6,7,8})
#   V4721 = rendimento mensal domiciliar, exclusive pensionista, empregado
#   domestico e parente do empregado domestico — por isso o denominador
#   exclui exatamente essas tres condicoes (mesma construcao de Souza &
#   Hecksher, harmoniza_pnad.R: tamdom = membros com v0401 <= 5).
#
# Historico: maquinaria escrita para 101_Sensitivity_Renda_Familiar_vs_
# Domiciliar.R, copiada para 242_Tese_Renda_Mediana_Domiciliar.R em
# 2026-09-16 e extraida para este arquivo no mesmo dia, quando o 220B passou
# a precisar da mesma leitura (decisao do autor: "quero tudo domiciliar").
# O codigo e o do 242, sem alteracao alem do parametro `cache_dir`.
#
# Uso: source() este arquivo com dplyr, readr e stringr ja anexados, e chamar
#   extrai_pnad(ano, cache_dir = <pasta data-raw/pnad_anual_raw>)
# que devolve tibble(ano, renda_dom_pcta, peso) em moeda NOMINAL do ano (ja
# convertida de Cruzeiros/Cruzeiros Reais para Reais em 1992/1993), ou NULL
# se o ano nao tiver microdados no cache. Deflacionar no chamador (D01).
# Consumidores: 242_Tese_Renda_Mediana_Domiciliar.R, 220B_Tese_Renda_Centil.R.
# ==============================================================================

CACHE_PNAD_PADRAO <- here::here("data-raw", "pnad_anual_raw")   # escrito por shared-pipeline/pnadc/020

# `CONTROL` e o nome do numero de controle do domicilio em 1992-1993; de 1995
# em diante o IBGE passou a chama-lo `V0102`. Sem isso, 1992/93 caem fora por
# "dicionario sem as variaveis necessarias".
VARS <- c("V0102", "CONTROL", "V0103", "V0401", "V4721", "V4729", "UF")

parse_sas_dict <- function(sas_file, vars_of_interest) {
  linhas <- readLines(sas_file, encoding = "latin1", warn = FALSE)
  linhas <- iconv(linhas, from = "latin1", to = "ASCII", sub = "")
  linhas <- str_squish(linhas)
  out <- data.frame(var = character(), start = integer(), width = integer())
  for (v in vars_of_interest) {
    idx <- grep(paste0("^@\\s*[0-9]+\\s+", v, "(\\s+|$)"), linhas, ignore.case = TRUE)
    if (length(idx) == 0) next
    l <- linhas[idx[1]]
    start <- as.integer(str_extract(l, "(?<=@)[0-9]+"))
    parte <- strsplit(l, v, fixed = TRUE)[[1]]
    width <- as.integer(str_extract(parte[2], "[0-9]+"))
    if (!is.na(start) && !is.na(width)) {
      out <- bind_rows(out, data.frame(var = v, start = start, width = width))
    }
  }
  out %>%
    mutate(end = start + width - 1) %>%
    arrange(start)
}

# O IBGE variou os nomes ao longo da serie e os zips foram extraidos em
# subpastas diferentes. Nomes ja vistos no cache local:
#   dados : PES92.DAT, P96BR.TXT, Pessoas97 (sem extensao), PES2001.TXT, PES2013.txt
#   input : LAYOUT/sas/SAS_PES.TXT, SAS_PE96.TXT, SasPes97.txt, "INPUT PES2007.txt"
# Regra: o input e o arquivo de texto que esta numa pasta LAYOUT ou cujo nome
# comeca por input/sas; o dado e o arquivo de pessoas que sobra.
localiza <- function(ano, cache_dir = CACHE_PNAD_PADRAO) {
  todos <- list.files(file.path(cache_dir, ano),
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )
  todos <- todos[!grepl("\\.(zip|xls|xlsx|doc|docx|pdf)$", todos, ignore.case = TRUE)]
  todos <- todos[!grepl("temp_dados|temp_layout", todos, ignore.case = TRUE)]
  bn <- basename(todos)
  eh_input <- grepl("layout", todos, ignore.case = TRUE) |
    grepl("^(input|sas)", bn, ignore.case = TRUE)
  input <- todos[eh_input & grepl("\\.(txt|sas)$", bn, ignore.case = TRUE)]
  # Varios anos trazem o input de DOMICILIOS ao lado do de PESSOAS (ex.: 2013
  # tem "input DOM2013.txt" e "input PES2013.txt"). O de domicilios nao tem
  # V0401/V4729 e faria o ano cair fora silenciosamente — descartar.
  input <- input[!grepl("dom", basename(input), ignore.case = TRUE)]
  input <- c(
    input[grepl("\\.txt$", input, ignore.case = TRUE)],
    input[grepl("\\.sas$", input, ignore.case = TRUE)]
  )
  dados <- todos[!eh_input & grepl("^(pes|pessoa|p[0-9]{2})", bn, ignore.case = TRUE)]
  # Maior arquivo primeiro: o de pessoas e sempre o maior do ano.
  if (length(dados) > 1) dados <- dados[order(file.size(dados), decreasing = TRUE)]
  list(dados = dados, input = input)
}

extrai_pnad <- function(ano, cache_dir = CACHE_PNAD_PADRAO) {
  arq <- localiza(ano, cache_dir)
  if (length(arq$dados) == 0 || length(arq$input) == 0) {
    cat(sprintf("  [%s] microdados nao encontrados — ano pulado\n", ano))
    return(NULL)
  }
  pos <- parse_sas_dict(arq$input[1], VARS)
  tem_controle <- any(c("V0102", "CONTROL") %in% pos$var)
  if (!all(c("V4721", "V0401", "V0103", "V4729") %in% pos$var) || !tem_controle) {
    cat(sprintf("  [%s] dicionario sem as variaveis necessarias (%s) — ano pulado\n",
                ano, paste(setdiff(c("V4721", "V0401", "V0103", "V4729"), pos$var),
                           collapse = ", ")))
    return(NULL)
  }
  df <- read_fwf(arq$dados[1], fwf_positions(pos$start, pos$end, pos$var),
    col_types = cols(.default = col_character()), progress = FALSE
  ) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
  if (!"UF" %in% names(df)) df$UF <- NA_real_
  # 1992-1993: o numero de controle chama-se CONTROL
  if (!"V0102" %in% names(df)) df$V0102 <- df$CONTROL

  # ⚠ GOTCHA (ver 101): a chave do domicilio muda de largura no meio da serie.
  # Ate 1999, V0102 tem 6 digitos e a UF vem a parte; de 2001, as duas
  # primeiras posicoes de V0102 JA sao a UF. Chavear sem a UF funde
  # domicilios de estados diferentes nos anos 90.
  df <- df %>% mutate(dom_id = paste(UF, V0102, V0103, sep = "-"))

  # D20 — sentinela de renda nao declarada (doze noves) -> NA
  df <- df %>% mutate(V4721 = ifelse(V4721 >= 999999999999, NA_real_, V4721))

  # Denominador: moradores fora das condicoes ja excluidas do numerador V4721.
  # Peso do domicilio = SOMA dos pesos de pessoa dos moradores contados no
  # denominador — e o que Souza & Hecksher fazem ao colapsar por domicilio
  # (`pesopop = sum(pesopop)`), e equivale a ponderar por PESSOA. Nao usar
  # `first(V4729)`: a mediana passaria a ser a do domicilio tipico, nao a da
  # pessoa tipica, que e a convencao do IBGE e a da NT 120.
  dom <- df %>%
    group_by(dom_id) %>%
    summarise(
      n_dom = sum(!V0401 %in% c(6, 7, 8)),
      renda_dom = first(V4721),
      peso = sum(V4729[!V0401 %in% c(6, 7, 8)], na.rm = TRUE),
      .groups = "drop"
    )

  # Guarda contra chave malformada (ver 101): a PNAD tem 3-4 moradores/domicilio.
  tam_medio <- mean(dom$n_dom)
  if (tam_medio < 2 || tam_medio > 6) {
    stop(sprintf("[%s] tamanho medio de domicilio implausivel (%.2f) — chave dom_id malformada",
                 ano, tam_medio))
  }

  out <- dom %>%
    filter(n_dom > 0, !is.na(renda_dom), !is.na(peso), peso > 0) %>%
    mutate(
      ano = as.integer(ano),
      renda_dom_pcta = renda_dom / n_dom,
      # Conversao cambial (D01): Cruzeiros/Cruzeiros Reais -> Reais
      renda_dom_pcta = case_when(
        ano == 1992L ~ renda_dom_pcta / 2750000,
        ano == 1993L ~ renda_dom_pcta / 2750,
        TRUE ~ renda_dom_pcta
      ),
    ) %>%
    select(ano, renda_dom_pcta, peso)
  cat(sprintf("  [%s] %s domicilios | mediana nominal R$ %.0f\n", ano,
              format(nrow(out), big.mark = "."),
              as.numeric(Hmisc::wtd.quantile(out$renda_dom_pcta, weights = out$peso,
                                      probs = 0.5, normwt = FALSE))))
  out
}

