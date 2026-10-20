# ==============================================================================
# 010_Sondagem_Anual.R
#
# Antes de estender as vias de ingresso (portas-renda-coortes-*) de duas coortes
# para todas as coortes possiveis, confere contra a base, ano a ano, o que o
# desenho exige:
#
#   1. ENEM_<ano>: a variavel de renda familiar (faixas A..Q), a de numero de
#      moradores (numerico) e a de presenca (prova de Matematica), cujos nomes
#      mudam entre edicoes.
#   2. BAS_MATRICULA_<ano>: tamanho da coorte da serie final do medio (16-22
#      anos) e quantos tem CPF, sem o qual nao ha vinculo.
#   3. SUP_ALUNO_<ano>: entre os ingressantes do ano, se as flags de reserva de
#      vagas, ProUni, FIES e a modalidade estao preenchidas. IN_RESERVA_VAGAS e
#      100% NULL em 2024 (dicionario empirico da dissertacao).
#
# Coortes candidatas: 2011 a 2021. A ultima e 2021 porque a janela de tres anos
# (2022-2024) termina no ultimo Censo da Educacao Superior disponivel.
#
# Mapa de nomes de partida (catalogo do INEP, conferido aqui, nao presumido):
#   renda: 2011 Q004; 2012-2014 Q003; 2015+ Q006
#   moradores: 2011 Q001; 2012-2014 Q004; 2015+ Q005
#   presenca: 2011-2014 IN_PRESENCA_MT; 2015+ TP_PRESENCA_MT
#
# Custo: 11 x 3 consultas ao ENEM, 11 ao Censo Escolar, 13 ao Censo Superior.
# Pre-requisito: SEDAP_TOKEN no ambiente.
# Uso: Rscript shared-pipeline/sedap/portas-anuais/010_Sondagem_Anual.R
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(purrr)
})
source(here::here("shared-pipeline", "sedap", "010_SEDAP_Cliente.R"))

OUT <- here::here("data-raw", "sedap", "portas-anuais")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

COORTES <- 2011:2021
ANOS_SUP <- 2012:2024
ETAPAS <- paste(sprintf("'%d'", c(27, 28, 29, 32, 33, 34, 37, 38)), collapse = ", ")

mapa_enem <- function(ano) {
  if (ano == 2011) {
    list(renda = "Q004", moradores = "Q001", presenca = "IN_PRESENCA_MT")
  } else if (ano <= 2014) {
    list(renda = "Q003", moradores = "Q004", presenca = "IN_PRESENCA_MT")
  } else {
    list(renda = "Q006", moradores = "Q005", presenca = "TP_PRESENCA_MT")
  }
}

consulta <- function(sql, rotulo) {
  r <- suppressMessages(sedap_query(sql, verbose = FALSE))
  if (is.null(r) || !nrow(r)) {
    cat(sprintf("  %-40s FALHOU/VAZIA\n", rotulo))
    return(NULL)
  }
  r
}

# ------------------------------------------------------------------------------
# 1. ENEM
# ------------------------------------------------------------------------------
cat("\n######## 1. ENEM: renda, moradores, presenca ########\n")
enem <- map_dfr(COORTES, function(ano) {
  m <- mapa_enem(ano)
  map_dfr(c("renda", "moradores", "presenca"), function(papel) {
    col <- m[[papel]]
    r <- consulta(
      sprintf("SELECT CAST(%s AS STRING) AS V, COUNT(*) AS N FROM raw.ENEM_%d_SEDAP GROUP BY V", col, ano),
      sprintf("ENEM %d %s (%s)", ano, papel, col)
    )
    if (is.null(r)) {
      return(NULL)
    }
    r <- r %>% mutate(V = as.character(V), N = as.numeric(N)) %>% arrange(V)
    cat(sprintf(
      "  ENEM %d %-9s %-15s ncat=%-3d tot=%-10s | %s\n", ano, papel, col, nrow(r),
      format(sum(r$N), big.mark = ","),
      paste0(head(paste0(r$V, "=", round(r$N / 1000), "k"), 20), collapse = " ")
    ))
    r %>% mutate(ano = ano, papel = papel, coluna = col)
  })
})
write.csv(enem, file.path(OUT, "sondagem_enem.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 2. Censo Escolar: coorte e cobertura de CPF
# ------------------------------------------------------------------------------
cat("\n######## 2. BAS_MATRICULA: serie final, 16-22 anos ########\n")
bas <- map_dfr(COORTES, function(ano) {
  r <- consulta(sprintf(
    "SELECT COUNT(*) AS N_SERIE_FINAL, COUNT(CPF_MASC) AS N_COM_CPF
     FROM raw.BAS_MATRICULA_%d
     WHERE CAST(TP_ETAPA_ENSINO AS STRING) IN (%s)
       AND CAST(NU_IDADE AS INT64) >= 16 AND CAST(NU_IDADE AS INT64) <= 22",
    ano, ETAPAS
  ), sprintf("BAS_MATRICULA %d", ano))
  if (is.null(r)) {
    return(NULL)
  }
  r <- r %>% mutate(across(everything(), as.numeric), ano = ano, pct_cpf = 100 * N_COM_CPF / N_SERIE_FINAL)
  cat(sprintf(
    "  %d  serie final=%-10s com CPF=%-10s (%.1f%%)\n", ano,
    format(r$N_SERIE_FINAL, big.mark = ","), format(r$N_COM_CPF, big.mark = ","), r$pct_cpf
  ))
  r
})
write.csv(bas, file.path(OUT, "sondagem_bas.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. Censo Superior: flags entre os ingressantes do ano
# ------------------------------------------------------------------------------
cat("\n######## 3. SUP_ALUNO: flags entre ingressantes do ano ########\n")
sup <- map_dfr(ANOS_SUP, function(ano) {
  r <- consulta(sprintf(
    "SELECT CAST(IN_RESERVA_VAGAS AS STRING) AS RESERVA,
            CAST(IN_FIN_REEMB_FIES AS STRING) AS FIES,
            CAST(IN_FIN_NAOREEMB_PROUNI_INTEGR AS STRING) AS PROUNI_INT,
            CAST(IN_FIN_NAOREEMB_PROUNI_PARCIAL AS STRING) AS PROUNI_PARC,
            CAST(TP_MODALIDADE_ENSINO AS STRING) AS MODALIDADE,
            COUNT(*) AS N
     FROM raw.SUP_ALUNO_%d
     WHERE CAST(NU_ANO_INGRESSO AS STRING) = '%d'
     GROUP BY RESERVA, FIES, PROUNI_INT, PROUNI_PARC, MODALIDADE",
    ano, ano
  ), sprintf("SUP_ALUNO %d", ano))
  if (is.null(r)) {
    return(NULL)
  }
  r <- r %>% mutate(N = as.numeric(N), ano = ano)
  tot <- sum(r$N)
  pct <- function(cond) sprintf("%5.1f", 100 * sum(r$N[cond]) / tot)
  cat(sprintf(
    "  %d  ingressantes=%-10s NULL: reserva=%s fies=%s prouni=%s modal=%s | sim: reserva=%s fies=%s prouni=%s ead=%s\n",
    ano, format(tot, big.mark = ","),
    pct(is.na(r$RESERVA)), pct(is.na(r$FIES)), pct(is.na(r$PROUNI_INT)), pct(is.na(r$MODALIDADE)),
    pct(r$RESERVA %in% "1"), pct(r$FIES %in% "1"),
    pct(r$PROUNI_INT %in% "1" | r$PROUNI_PARC %in% "1"),
    pct(!is.na(r$MODALIDADE) & r$MODALIDADE != "1")
  ))
  r
})
write.csv(sup, file.path(OUT, "sondagem_sup.csv"), row.names = FALSE)

cat("\nSondagem gravada em data-raw/sedap/portas-anuais/\n")
