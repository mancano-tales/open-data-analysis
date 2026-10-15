# Entire reconciliation function exercised with tiny mocked local sources.
source("shared-pipeline/eleicoes/agregacao/validacao/legal-party-technical-wrapper.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(2L)
env <- legal_party_technical_environment()
folder <- tempfile("party-validity-fixture-")
dir.create(folder)
zip <- file.path(folder, "source.zip")
writeLines("synthetic checksum fixture; never downloaded/unzipped", zip)
md5 <- unname(tools::md5sum(zip))
raw <- data.table(year = 2026L, round = 1L, election_id = 6259L,
  election_date = "04/10/2026", uf = "PB", municipality_tse = "1", zone = 1L,
  place = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
  place_key = paste0("PB:1:1:", c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L)), office = 6L,
  candidate_id = c(1313L, 1313L, 13L, 13L, 70L, 70L, 4444L, 4444L),
  candidate_name = c("Candidate", "Candidate", "Party", "Party", "Unknown party", "Unknown party", "Unknown nominal", "Unknown nominal"),
  vote_type = rep(c("candidate", "party", "party", "candidate"), each = 2L),
  votes = c(30, 20, 5, 5, 2, 3, 1, 2))
save_raw <- function(x) {
  attr(x, "source_md5") <- md5; attr(x, "normalizer_version") <- 2L
  saveRDS(x, file.path(folder, "places-2026-PB-6.rds"))
}
save_raw(raw)
base <- data.table(ANO_ELEICAO = 2026L, NR_TURNO = 1L, CD_ELEICAO = 6259L,
  SG_UF = "PB", CD_MUNICIPIO = 1L, NR_ZONA = 1L, CD_CARGO = 6L)
candidate <- copy(base)[, `:=`(SQ_CANDIDATO = "100", NR_CANDIDATO = 1313L, NR_PARTIDO = 13L,
  SG_PARTIDO = "PT", NM_TIPO_DESTINACAO_VOTOS = "Válido", QT_VOTOS_NOMINAIS = 50, QT_VOTOS_NOMINAIS_VALIDOS = 50)]
party <- copy(base)[, `:=`(NR_PARTIDO = 13L, QT_VOTOS_LEGENDA_VALIDOS = 10,
  QT_VOTOS_LEGENDA_ANUL_SUBJUD = 0, QT_VOTOS_LEGENDA_ANULADOS = 0)]
detail <- copy(base)[, `:=`(QT_TOTAL_VOTOS_VALIDOS = 60, QT_VOTOS_NOMINAIS_VALIDOS = 50,
  QT_TOTAL_VOTOS_LEG_VALIDOS = 10, QT_VOTOS_NULOS_TECNICOS = 8)]
fwrite(candidate, file.path(folder, "votacao_candidato_munzona.csv"))
fwrite(party, file.path(folder, "votacao_partido_munzona.csv"))
fwrite(detail, file.path(folder, "detalhe_votacao_munzona.csv"))
env$tse_download <- function(...) zip
env$legal_resource_csv <- function(year, dataset_prefix, uf, cache_dir) file.path(folder, paste0(dataset_prefix, ".csv"))
pass <- env$legal_reconcile_places(2026L, "PB", 6L, folder)
stopifnot(sum(pass$votes) == 60, sum(pass$votes[pass$vote_type == "party"]) == 10,
  !any(pass$candidate_id %in% c(70L, 4444L)), attr(pass, "vote_audit")$checks$difference == 0)
expect_rejection <- function(x, message_pattern) {
  save_raw(x)
  result <- tryCatch({env$legal_reconcile_places(2026L, "PB", 6L, folder); NULL}, error = conditionMessage)
  stopifnot(is.character(result), grepl(message_pattern, result))
}
bad_known <- copy(raw); bad_known[candidate_id == 13L & place == 1L, votes := votes + 1]
expect_rejection(bad_known, "Raw section and official party totals differ")
bad_unknown <- copy(raw); bad_unknown[candidate_id == 70L & place == 1L, votes := votes + 1]
expect_rejection(bad_unknown, "No reconciled contest")
resolved <- normalizePath(folder, winslash = "/", mustWork = TRUE)
temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
stopifnot(startsWith(resolved, paste0(temp_root, "/")), startsWith(basename(resolved), "party-validity-fixture-"))
unlink(resolved, recursive = TRUE)
message("PASS: unknown party excluded and exactly audited as technical null; recognized-party and technical-null discrepancies rejected.")
