# Census income refers to July 2022; do not substitute the election-year wage.
ELEICAO_ANO <- 2026L
ELEICAO_TURNO <- 1L
ELEICAO_CARGO <- 1L
SALARIO_MINIMO_CENSO_2022 <- 1212

validar_candidatos_presidenciais <- function(votos) {
  required <- c("NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS")
  if (!all(required %in% names(votos))) stop("Candidate identity columns missing.")
  patterns <- c(`13` = "\\bLULA\\b", `22` = "FLAVIO.*BOLSONARO")
  for (number in names(patterns)) {
    selected <- votos$NR_VOTAVEL == as.integer(number) & votos$QT_VOTOS > 0
    labels <- unique(as.character(votos$NM_VOTAVEL[selected]))
    normalized <- toupper(iconv(labels, from = "", to = "ASCII//TRANSLIT"))
    if (!length(labels) || anyNA(normalized) ||
        !all(grepl(patterns[[number]], normalized, perl = TRUE))) {
      stop("Unexpected candidate identity for number ", number, ": ",
           paste(labels, collapse = ", "))
    }
  }
  invisible(TRUE)
}
