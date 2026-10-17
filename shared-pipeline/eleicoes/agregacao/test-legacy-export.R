# Isolated schema-1 fixtures only. No driver/TSE helper sourcing, downloads or
# production cache reads/writes. Four frozen implementation files are hashed.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/agregacao/build-explorer.R", encoding = "UTF-8")
library(data.table); setDTthreads(1L)
reject <- function(expr, pattern = NULL) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(error)))
}
root <- tempfile("legacy-export-fixture-"); dir.create(root)
cache <- file.path(root, "historico"); legacy <- file.path(cache, "legacy-canonic")
spatial <- file.path(root, "espacial"); output <- file.path(root, "output")
staging <- file.path(legacy, "staging", "2014", "AC", "fixture-1")
dir.create(cache); dir.create(legacy); dir.create(spatial); dir.create(staging, recursive = TRUE)
raw <- file.path(cache, "votacao_secao_2014_AC.zip"); writeLines("TEST raw archive", raw)
prefixes <- c("votacao_candidato_munzona", "votacao_partido_munzona", "detalhe_votacao_munzona")
members <- list(); member_paths <- character()
for (prefix in prefixes) {
  writeLines(paste("TEST official ZIP", prefix), file.path(cache, paste0(prefix, "_2014.zip")))
  folder <- file.path(staging, "extracted", paste0(prefix, "_2014")); dir.create(folder, recursive = TRUE)
  file <- file.path(folder, paste0(prefix, "_2014_AC.csv")); writeLines(paste("TEST selected CSV", prefix), file)
  member_paths <- c(member_paths, file)
  members[[prefix]] <- list(archive = paste0(prefix, "_2014.zip"), member = basename(file), csv_md5 = unname(tools::md5sum(file)))
}
keys <- eleicao_legacy_keys
expected <- data.frame(year = 2014L, round = c(1L, 1L, 1L, 2L), election_id = c(143L, 143L, 143L, 144L), office = c(6L, 7L, 3L, 3L), uf = "AC")
row <- function(office, round, ids, counts, type = "candidate") data.table(year = 2014L, round = as.integer(round),
  election_id = if (round == 1L) 143L else 144L, office = as.integer(office), uf = "AC", municipality_tse = "1", zone = 1L,
  place = 1L, candidate_id = ids, candidate_name = paste("TEST", ids), vote_type = type, votes = counts)
votes <- rbindlist(list(row(6L, 1L, c(1313, 2222, 13), c(30, 50, 20), c("candidate", "candidate", "party")),
                       row(3L, 1L, c(13, 22), c(60, 40)), row(3L, 2L, c(13, 22), c(55, 45))))
checks <- votes[, .(official_valid_votes = sum(votes), computed_valid_votes = sum(votes), difference = 0,
                    reconciled = TRUE, zones = 1L), by = keys]
blocked <- transform(expected[expected$office == 7L, ], reason = "TEST ambiguous legend allocation remains unavailable")
identity_table <- unique(as.data.frame(votes[vote_type == "candidate", c(keys, "candidate_id", "candidate_name"), with = FALSE]))
identity_table$identified_candidate_uid <- paste(identity_table$election_id, identity_table$office, identity_table$candidate_id, sep = ":")
attr(votes, "normalizer_version") <- 2L; attr(votes, "source_md5") <- unname(tools::md5sum(raw))
attr(votes, "vote_audit") <- list(status = "reconciled", publishable = TRUE, method = "TEST legacy partial exact certificate",
  expected_contests = expected, checks = as.data.frame(checks), unavailable = blocked, candidate_identity = identity_table,
  provenance = list(raw_md5 = attr(votes, "source_md5"), official_members = members))
target <- file.path(legacy, "places-2014-AC-3_5_6_7_8-legal.rds")
checkpoint <- file.path(legacy, "status", "2014", "AC.json"); dir.create(dirname(checkpoint), recursive = TRUE)
persist <- function(value = votes, status = NULL, edit = identity) {
  saveRDS(value, target)
  audit <- attr(value, "vote_audit")
  paths <- c(eleicao_legacy_sources, raw, file.path(cache, paste0(prefixes, "_2014.zip")), member_paths, target)
  record <- list(schema_version = 1L, year = 2014L, uf = "AC", run_id = "fixture-1", output = target,
    status = if (is.null(status)) if (nrow(audit$unavailable)) "partial" else "complete" else status,
    audit_status = audit$status, publishable = audit$publishable, rows = nrow(value),
    certified = audit$checks, unavailable = audit$unavailable, staging = staging,
    fingerprints = data.frame(path = paths, bytes = unname(file.info(paths)$size), md5 = unname(tools::md5sum(paths))))
  jsonlite::write_json(edit(record), checkpoint, auto_unbox = TRUE, dataframe = "rows", pretty = TRUE)
  invisible(record)
}
read_guard <- function(value = readRDS(target)) eleicao_legacy_certificate(value, target, legacy, 2014L, "AC")
persist(); certificate <- read_guard()
stopifnot(certificate$status == "partial", nrow(certificate$checks) == 3L,
  eleicao_legacy_target(certificate, 2014L, 1L, 6L, "AC")$available,
  grepl("ambiguous legend", eleicao_legacy_target(certificate, 2014L, 1L, 7L, "AC")$reason))

# Income always remains Census 2022; both references use the full income universe.
income <- data.frame(code_tract = sprintf("12%013d", 1:200), V06001 = 1, V06004 = (1:200) * 1212)
income_path <- file.path(root, "renda2022.rds"); saveRDS(income, income_path)
links <- data.table(code_tract = income$code_tract, place_key = "AC:1:1:1", distancia_m = 10)
saveRDS(links, file.path(spatial, "links-2014-1.rds")); saveRDS(links, file.path(spatial, "links-2014-2.rds"))
# A normal 2018 fixture has no LEGACY checkpoint and must keep working unchanged.
old <- copy(votes[office == 3L & round == 1L]); old[, `:=`(year = 2018L, election_id = 297L)]
old_checks <- as.data.frame(checks[office == 3L & round == 1L]); old_checks$year <- 2018L; old_checks$election_id <- 297L
attr(old, "normalizer_version") <- 2L; attr(old, "source_md5") <- "TEST2018"
attr(old, "vote_audit") <- list(status = "reconciled", method = "TEST normal 2018", checks = old_checks)
saveRDS(old, file.path(cache, "places-2018-AC-3-legal.rds")); saveRDS(links, file.path(spatial, "links-2018-1.rds"))
previous <- eleicao_build_explorer(output, cache, spatial, income_path, data.table(year = 2018L, round = 1L, office = 3L, uf = "AC"))
previous_file <- file.path(output, "series", "2018-1-3-AC.json"); previous_hash <- unname(tools::md5sum(previous_file))
# Root-level 2014 caches exist but explicit LEGACY must never fall back to them.
saveRDS(votes, file.path(cache, basename(target)))
targets <- rbindlist(lapply(c("national", "state"), function(reference) data.table(expected[, c("year", "round", "office", "uf")], income_reference = reference)))
calls <- 0L; guard <- eleicao_legacy_certificate
eleicao_legacy_certificate <- function(...) { calls <<- calls + 1L; guard(...) }
build <- function() eleicao_build_explorer(output, cache, spatial, income_path, targets, incremental = TRUE, flush_progress = TRUE, legacy_cache_dir = legacy)
result <- build(); stopifnot(calls == 1L, length(result$series) == 7L,
  identical(unname(tools::md5sum(previous_file)), previous_hash), any(vapply(result$series, function(x) x$year == 2018L, logical(1))))
json <- jsonlite::read_json(file.path(output, "series", "2014-1-6-AC-state.json"))
stopifnot(json$metadata$income_year == 2022L, json$metadata$percentile_scope == "AC",
  json$metadata$vote_cache_protocol == "legacy-canonic-v1", json$metadata$vote_certificate$status == "partial",
  json$metadata$vote_checkpoint_md5 == unname(tools::md5sum(checkpoint)),
  json$metadata$vote_audit$difference == 0, json$metadata$vote_audit$computed_valid_votes == 100,
  json$metadata$vote_certificate$unavailable_contests == 1L,
  length(json$metadata$vote_certificate$official_members) == 3L)
aggregate <- eleicao_aggregate_contest; eleicao_aggregate_contest <- function(...) stop("Unexpected reaggregation")
calls <- 0L; reused <- build(); stopifnot(calls == 1L, length(reused$series) == 7L)
# The initial gate cannot authorize a source that changes later in the same
# invocation. Even flush_progress must leave the old manifest untouched.
manifest_path <- file.path(output, "manifest.json"); before_manifest <- readBin(manifest_path, "raw", file.info(manifest_path)$size)
target_guard <- eleicao_legacy_target; changed <- FALSE
eleicao_legacy_target <- function(...) {
  result <- target_guard(...)
  if (!changed) { writeLines("TEST raw archivX", raw); changed <<- TRUE }
  result
}
reject(build(), "snapshot changed")
stopifnot(identical(readBin(manifest_path, "raw", file.info(manifest_path)$size), before_manifest))
eleicao_legacy_target <- target_guard; writeLines("TEST raw archive", raw); persist()
before_manifest <- readBin(manifest_path, "raw", file.info(manifest_path)$size)
saved_rds <- readBin(target, "raw", file.info(target)$size); changed <- FALSE
eleicao_legacy_target <- function(...) {
  result <- target_guard(...)
  if (!changed) {
    corrupted <- saved_rds; corrupted[[length(corrupted)]] <- as.raw(bitwXor(as.integer(corrupted[[length(corrupted)]]), 1L))
    writeBin(corrupted, target); changed <<- TRUE
  }
  result
}
reject(build(), "snapshot changed")
stopifnot(identical(readBin(manifest_path, "raw", file.info(manifest_path)$size), before_manifest))
eleicao_legacy_target <- target_guard; writeBin(saved_rds, target); persist()
one <- list(fingerprints = data.frame(path = raw, bytes = file.info(raw)$size, md5 = unname(tools::md5sum(raw))))
stopifnot(eleicao_legacy_verify_snapshot(list(one, one)) == 1L)
other <- one; other$fingerprints$md5 <- paste(rep("0", 32), collapse = "")
reject(eleicao_legacy_verify_snapshot(list(one, other)), "conflicting expected")
# Checkpoint gate must execute BEFORE reuse of numerically identical cached JSON.
persist(status = "running"); unavailable <- build(); stopifnot(length(unavailable$series) == 1L,
  all(vapply(Filter(function(x) x$year == 2014L, unavailable$availability), function(x) grepl("schema/status", x$reason), logical(1))))
eleicao_aggregate_contest <- aggregate; persist()
eleicao_legacy_certificate <- guard

# Invalid protocol, stale inputs and insufficient partitions all fail closed.
persist(edit = function(x) { x$schema_version <- 2L; x }); reject(read_guard(), "schema/status")
persist(edit = function(x) { x$rows <- x$rows + 1L; x }); reject(read_guard(), "row count")
persist(); original_raw <- readLines(raw); raw_bytes <- file.info(raw)$size
writeLines(sub("archive", "archivX", original_raw), raw); stopifnot(file.info(raw)$size == raw_bytes); reject(read_guard(), "fingerprint")
writeLines(original_raw, raw); persist()
mutate_audit <- function(change) { value <- copy(votes); attr(value, "vote_audit") <- change(attr(value, "vote_audit")); persist(value); value }
value <- mutate_audit(function(a) { a$unavailable <- rbind(a$unavailable, transform(a$expected_contests[1, ], reason = "overlap")); a }); reject(read_guard(), "partition")
value <- mutate_audit(function(a) { a$expected_contests <- a$expected_contests[-1, ]; a }); reject(read_guard(), "partition")
value <- mutate_audit(function(a) { extra <- a$expected_contests[1, ]; extra$election_id <- 999L; a$expected_contests <- rbind(a$expected_contests, extra); a$unavailable <- rbind(a$unavailable, transform(extra, reason = "other election")); a }); reject(read_guard(), "duplicate contest")
value <- copy(votes); value[1L, votes := votes + 1]; persist(value); reject(read_guard(), "total differs")
value <- mutate_audit(function(a) { a$candidate_identity <- rbind(a$candidate_identity, transform(a$candidate_identity[1, ], identified_candidate_uid = "CONFLICT")); a }); reject(read_guard(), "conflicting candidate")
value <- mutate_audit(function(a) { i <- a$checks$office == 6L; a$checks$official_valid_votes[i] <- a$checks$computed_valid_votes[i] <- 0; a }); reject(read_guard(), "positive exact denominator")
# Producer-converted whole-zero contests stay unavailable with their own reason.
value <- copy(votes)[office != 6L]; audit <- attr(votes, "vote_audit")
audit$checks <- audit$checks[audit$checks$office != 6L, ]; audit$unavailable <- rbind(audit$unavailable,
  transform(expected[expected$office == 6L, ], reason = "Exact zero valid votes: no percentage denominator"))
audit$candidate_identity <- audit$candidate_identity[audit$candidate_identity$office != 6L, ]; attr(value, "vote_audit") <- audit
persist(value); stopifnot(grepl("Exact zero", eleicao_legacy_target(read_guard(), 2014L, 1L, 6L, "AC")$reason))
# Investigation complete with every contest blocked: valid diagnostic cache,
# never a reconciled or publicable percentage series.
empty <- copy(votes)[0]; audit <- attr(votes, "vote_audit")
audit$checks <- audit$checks[0, ]; audit$unavailable <- transform(expected, reason = "TEST all contests blocked")
audit$status <- "unavailable"; audit$publishable <- FALSE; audit$candidate_identity <- audit$candidate_identity[0, ]
attr(empty, "vote_audit") <- audit; persist(empty)
stopifnot(nrow(read_guard()$checks) == 0L,
  !eleicao_legacy_target(read_guard(), 2014L, 1L, 6L, "AC")$available)
persist()
without_checkpoint <- file.path(root, "missing-checkpoint"); dir.create(without_checkpoint)
uncertified_target <- file.path(without_checkpoint, basename(target)); saveRDS(votes, uncertified_target)
reject(eleicao_legacy_certificate(votes, uncertified_target, without_checkpoint, 2014L, "AC"), "checkpoint missing")
probes_only <- file.path(root, "probes-only"); dir.create(file.path(probes_only, "probes"), recursive = TRUE)
saveRDS(votes, file.path(probes_only, "probes", basename(target)))
missing <- eleicao_build_explorer(output, cache, spatial, income_path, targets, incremental = TRUE, legacy_cache_dir = probes_only)
stopifnot(length(missing$series) == 1L,
  all(vapply(Filter(function(x) x$year == 2014L, missing$availability), function(x) grepl("nenhum fallback", x$reason), logical(1))))
stopifnot(identical(unname(tools::md5sum(previous_file)), previous_hash))
# No opt-in keeps the old root selection, including ordinary 2014 caches;
# presidency ignores the LEGACY source even when opt-in is supplied.
eleicao_legacy_certificate <- function(...) stop("LEGACY guard called outside explicit legacy state scope")
normal_target <- data.table(year = 2014L, round = 1L, office = 6L, uf = "AC")
normal <- eleicao_build_explorer(file.path(root, "old-root-output"), cache, spatial, income_path, normal_target)
stopifnot(length(normal$series) == 1L)
pres <- copy(votes[office == 3L & round == 1L]); pres[, office := 1L]
pres_checks <- as.data.frame(checks[office == 3L & round == 1L]); pres_checks$office <- 1L
attr(pres, "normalizer_version") <- 2L; attr(pres, "source_md5") <- "TESTpres2014"
attr(pres, "vote_audit") <- list(status = "reconciled", method = "TEST root presidential source", checks = pres_checks)
saveRDS(pres, file.path(cache, "places-2014-BR-1-legal.rds"))
pres_result <- eleicao_build_explorer(file.path(root, "pres-output"), cache, spatial, income_path,
  data.table(year = 2014L, round = 1L, office = 1L, uf = "AC"), legacy_cache_dir = legacy)
stopifnot(length(pres_result$series) == 1L)
eleicao_legacy_certificate <- guard
message("PASS: isolated LEGACY scope/schema/partition/totals/identities; partial/zero/blocked reasons; gate before cached reuse; one certificate pass per UF; final unique-source barrier detects late same-size ZIP/RDS changes and conflicting hashes, preserves previous manifest despite flush_progress; Census2022 dual references; unchanged normal2018/presidential/root behavior.")
