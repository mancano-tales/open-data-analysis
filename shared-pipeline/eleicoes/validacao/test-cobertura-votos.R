suppressPackageStartupMessages(library(data.table))
source("shared-pipeline/eleicoes/parametros.R")
source("shared-pipeline/eleicoes/validacao/cobertura-votos.R")
sections <- data.table(SG_UF = c("AC", "AC", "AC", "SP"),
  CD_MUNICIPIO = c(1L, 1L, 1L, 2L), NR_ZONA = 1L,
  NR_LOCAL_VOTACAO = c(10L, 10L, NA_integer_, 20L),
  validos = c(30, 20, 10, 40), lula = c(10, 5, 5, 10),
  flavio = c(10, 10, 5, 20))
places <- sections[!is.na(NR_LOCAL_VOTACAO),
  lapply(.SD, sum), by = .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO),
  .SDcols = c("validos", "lula", "flavio")]
places[, place_key := paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = ":")]
result <- calcular_cobertura_votos(sections, places, places$place_key,
  places$place_key, "AC:1:1:10")
stopifnot(result$br[etapa == "resultado_nacional", validos] == 100,
  result$br[etapa == "locais_com_votos", validos] == 90,
  result$br[etapa == "locais_usados", validos] == 50,
  result$br[etapa == "locais_usados", locais] == 1L,
  result$br[etapa == "locais_usados", validos_fracao_br] == 0.5,
  result$uf[SG_UF == "AC" & etapa == "locais_usados", validos_fracao_territorio] == 50 / 60,
  result$uf[SG_UF == "SP" & etapa == "locais_usados", validos] == 0,
  is.na(result$uf[SG_UF == "SP" & etapa == "locais_usados", lula_pct_validos]))
bad <- try(calcular_cobertura_votos(sections, places, character(), character(), "AC:1:1:10"), silent = TRUE)
stopifnot(inherits(bad, "try-error"))
candidates <- data.table(NR_VOTAVEL = c(13L, 22L),
  NM_VOTAVEL = c("LULA", "FLAVIO BOLSONARO"), QT_VOTOS = 1)
validar_candidatos_presidenciais(candidates)
candidates[NR_VOTAVEL == 22L, NM_VOTAVEL := "JAIR BOLSONARO"]
stopifnot(inherits(try(validar_candidatos_presidenciais(candidates), silent = TRUE), "try-error"))
cat("PASS: vote coverage, missing location, UF denominators and candidate identity.\n")
