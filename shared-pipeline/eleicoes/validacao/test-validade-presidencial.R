if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/validacao/validade-presidencial.R", encoding = "UTF-8")
library(data.table); setDTthreads(2L)
reject <- function(expr, pattern) {
  message <- tryCatch({force(expr); NULL}, error = conditionMessage)
  stopifnot(is.character(message), grepl(pattern, message))
}
b <- data.table(ANO_ELEICAO = 2026L, NR_TURNO = 1L, CD_ELEICAO = 6257L, CD_CARGO = 1L,
  SG_UF = "AC", CD_MUNICIPIO = 1L, NR_ZONA = 1L)
raw <- b[rep(1L, 12L)][, `:=`(NR_ZONA = c(rep(1L, 8L), rep(2L, 4L)),
  NR_VOTAVEL = c(13L, 13L, 22L, 28L, 15L, 95L, 96L, 97L, 13L, 22L, 28L, 15L),
  QT_VOTOS = c(30, 20, 40, 7, 8, 3, 4, 0, 10, 20, 2, 0))]
outside <- rbind(copy(raw[1])[, `:=`(SG_UF = "ZZ", QT_VOTOS = 999)],
  copy(raw[1])[, `:=`(NR_TURNO = 2L, QT_VOTOS = 777)],
  copy(raw[1])[, `:=`(ANO_ELEICAO = 2022L, QT_VOTOS = 555)],
  copy(raw[1])[, `:=`(CD_CARGO = 3L, QT_VOTOS = 333)])
raw <- rbind(raw, outside)[c(9, 1, 13, 10, 2, 14, 11, 3, 15, 12, 4, 16, 5, 6, 7, 8)]
candidates <- b[rep(1L, 7L)][, `:=`(NR_ZONA = c(1L, 1L, 1L, 1L, 2L, 2L, 2L),
  NR_CANDIDATO = c(13L, 13L, 22L, 15L, 13L, 22L, 15L),
  QT_VOTOS_NOMINAIS = c(30, 20, 40, 8, 10, 20, 0),
  QT_VOTOS_NOMINAIS_VALIDOS = c(30, 20, 40, 0, 10, 20, 0))]
data.table::set(candidates, j = "CD_MUNICIPIO", value = rep("00001", nrow(candidates)))
detail <- b[rep(1L, 2L)][, `:=`(NR_ZONA = 1:2,
  QT_TOTAL_VOTOS_VALIDOS = c(90, 30), QT_VOTOS_NOMINAIS_VALIDOS = c(90, 30),
  QT_TOTAL_VOTOS_LEG_VALIDOS = 0, QT_VOTOS_NULOS_TECNICOS = c(7, 2))]
run <- function(r = raw, c = candidates, d = detail) filtrar_validade_presidencial(r, c, d)
before <- as.data.frame(raw)
x <- run()
scope <- raw$ANO_ELEICAO == 2026L & raw$NR_TURNO == 1L & raw$CD_CARGO == 1L & raw$SG_UF != "ZZ"
stopifnot(identical(as.data.frame(raw), before), identical(x$keep, scope & raw$NR_VOTAVEL %in% c(13L, 22L)),
  length(x$keep) == nrow(raw), all(x$checks$reconciled), sum(raw$QT_VOTOS[x$keep]) == 120,
  all(x$checks$computed_party_valid == 0), sum(x$checks$technical_computed) == 9,
  x$excluidos[NR_VOTAVEL == 28L, votos_excluidos] == 9,
  x$excluidos[NR_VOTAVEL == 15L, votos_excluidos] == 8,
  x$candidate_checks[NR_ZONA == 1L & NR_VOTAVEL == 13L, official_nominal] == 50,
  identical(x$metadata$election_ids, 6257L))
# No hardcoded candidate28: any unregistered number must close technical nulls.
different_number <- copy(raw)[NR_VOTAVEL == 28L, NR_VOTAVEL := 31L]
stopifnot(identical(run(r = different_number)$keep, x$keep),
  run(r = different_number)$excluidos[NR_VOTAVEL == 31L, votos_excluidos] == 9)
# The ID is derived from the explicit ordinary raw scope, not hardcoded6257.
r2 <- copy(raw)[, CD_ELEICAO := 7001L]
c2 <- copy(candidates)[, CD_ELEICAO := 7001L]
d2 <- copy(detail)[, CD_ELEICAO := 7001L]
stopifnot(identical(run(r2, c2, d2)$metadata$election_ids, 7001L))
partial <- copy(candidates);partial[1, QT_VOTOS_NOMINAIS_VALIDOS := 29]
reject(run(c = partial), "Partial nominal validity")
absent <- rbind(candidates, copy(candidates[1])[, `:=`(NR_CANDIDATO = 44L, QT_VOTOS_NOMINAIS = 3, QT_VOTOS_NOMINAIS_VALIDOS = 3)])
reject(run(c = absent), "positive candidate votes absent")
wrong_raw <- copy(raw);wrong_raw[which(scope & NR_VOTAVEL == 13L)[1L], QT_VOTOS := QT_VOTOS + 1]
reject(run(r = wrong_raw), "candidate nominal totals differ")
technical <- copy(detail);technical[1, QT_VOTOS_NULOS_TECNICOS := 8]
reject(run(d = technical), "zone reconciliation failed")
# Equal national technical totals cannot conceal differences between zones.
compensated <- copy(detail)[, QT_VOTOS_NULOS_TECNICOS := c(8, 1)]
reject(run(d = compensated), "zone reconciliation failed")
valid_total <- copy(detail);valid_total[1, QT_TOTAL_VOTOS_VALIDOS := 91]
reject(run(d = valid_total), "zone reconciliation failed")
compensated_valid <- copy(detail)[, QT_TOTAL_VOTOS_VALIDOS := c(91, 29)]
reject(run(d = compensated_valid), "zone reconciliation failed")
nominal_total <- copy(detail);nominal_total[1, QT_VOTOS_NOMINAIS_VALIDOS := 89]
reject(run(d = nominal_total), "zone reconciliation failed")
legend <- copy(detail);legend[1, QT_TOTAL_VOTOS_LEG_VALIDOS := 1]
reject(run(d = legend), "legend votes must be zero")
missing_zone <- detail[NR_ZONA == 1L]
reject(run(d = missing_zone), "detail zone keys differ")
missing_candidate_header <- copy(candidates)[, QT_VOTOS_NOMINAIS_VALIDOS := NULL]
reject(run(c = missing_candidate_header), "Missing required official candidate columns: QT_VOTOS_NOMINAIS_VALIDOS")
missing_detail_header <- copy(detail)[, QT_VOTOS_NULOS_TECNICOS := NULL]
reject(run(d = missing_detail_header), "Missing required official detail columns: QT_VOTOS_NULOS_TECNICOS")
two_ids <- rbind(raw, copy(raw[1])[, CD_ELEICAO := 7001L])
reject(run(r = two_ids), "one explicit ordinary presidential election ID")
nonordinary <- copy(raw)[, CD_TIPO_ELEICAO := 1L]
reject(run(r = nonordinary), "ordinary election type 2")
reject(filtrar_validade_presidencial(raw, candidates, detail, ano = 2022L), "only 2026")
reject(validade_presidencial(raw, candidates, detail, election_ids = c(6257L, 6257L)), "explicit, unique")
# Real CSV-path reader/header contract, still with tiny synthetic tables only.
tmp <- tempfile("presidential-validity-");dir.create(tmp)
cp <- file.path(tmp, "candidate.csv");dp <- file.path(tmp, "detail.csv")
fwrite(candidates, cp);fwrite(detail, dp)
from_csv <- filtrar_validade_presidencial(raw, cp, dp)
stopifnot(identical(from_csv$keep, x$keep), all(from_csv$checks$reconciled))
fwrite(missing_candidate_header, cp)
reject(filtrar_validade_presidencial(raw, cp, dp), "Missing required official candidate columns")
unlink(c(cp, dp));unlink(tmp)
message("PASS: raw-order legal mask, duplicate nominal aggregation, dynamic election ID, unknown-number technical nulls, all-or-zero validity, every zone ledger and mandatory headers.")
