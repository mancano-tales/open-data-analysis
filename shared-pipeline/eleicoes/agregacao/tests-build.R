# Small fixtures only: no downloads, no production spatial data/exports.
source("shared-pipeline/eleicoes/agregacao/tests.R")
source("shared-pipeline/eleicoes/agregacao/build-explorer.R")
source("shared-pipeline/eleicoes/agregacao/prepare-2026-all-ufs.R")

folder <- tempfile("explorer-fixture-")
cache <- file.path(folder, "historico"); spatial <- file.path(folder, "espacial")
output <- file.path(folder, "data")
dir.create(cache, recursive = TRUE); dir.create(spatial)
saveRDS(renda, file.path(folder, "renda.rds"))
attr(votes, "normalizer_version") <- 2L
attr(votes, "source_md5") <- "fixture"
fixture_checks <- votes[, .(computed_valid_votes = sum(votes)), by = .(year, round, office, uf)]
fixture_checks[, `:=`(official_valid_votes = computed_valid_votes, difference = 0)]
fixture_checks <- rbind(fixture_checks, data.table(year = 2026L, round = 1L, office = 1L,
  uf = setdiff(unname(eleicao_ufs), fixture_checks$uf), computed_valid_votes = 0,
  official_valid_votes = 0, difference = 0))
attr(votes, "vote_audit") <- list(status = "reconciled", method = "Synthetic fixture only", checks = fixture_checks)
saveRDS(votes, file.path(cache, "places-2026-BR-1.rds"))
attr(links, "metadata") <- list(year = 2026L, coordinate_year = 2026L)
saveRDS(links, file.path(spatial, "links-2026-1.rds"))
saveRDS(links, file.path(spatial, "links-2026-2.rds"))
targets <- data.table(year = c(2026L, 2026L, 2026L, 2026L, 2022L),
  round = c(1L, 1L, 1L, 2L, 1L), office = c(1L, 1L, 3L, 1L, 1L),
  uf = c("BR", "SP", "SP", "BR", "BR"))
manifest <- eleicao_build_explorer(output, cache, spatial, file.path(folder, "renda.rds"), targets)
stopifnot(manifest$schema_version == 1L, length(manifest$series) == 2L,
          length(manifest$availability) == 5L,
          sum(vapply(manifest$availability, function(x) x$status == "unavailable", logical(1))) == 3L,
          file.exists(file.path(output, "series", "2026-1-1-BR.json")),
          file.exists(file.path(output, "series", "2026-1-1-SP.json")),
          file.exists(file.path(output, "availability.csv")))
parsed <- jsonlite::read_json(file.path(output, "manifest.json"), simplifyVector = TRUE)
stopifnot(nrow(parsed$series) == 2L, parsed$metadata$percentile_scope == "BR")
# Incremental export preserves unrelated published series and reuses an exact
# input fingerprint. A changed vote cache must invalidate that shortcut.
original_aggregate <- eleicao_aggregate_contest
eleicao_aggregate_contest <- function(...) stop("Unexpected aggregation on unchanged inputs")
incremental <- eleicao_build_explorer(output,cache,spatial,file.path(folder,"renda.rds"),
  targets[year==2026 & round==1 & office==1 & uf=="SP"],incremental=TRUE,flush_progress=TRUE)
eleicao_aggregate_contest <- original_aggregate
stopifnot(length(incremental$series)==2L)
changed_votes <- copy(votes);changed_votes[1L,votes:=votes+1]
saveRDS(changed_votes,file.path(cache,"places-2026-BR-1.rds"))
invalidated <- eleicao_build_explorer(output,cache,spatial,file.path(folder,"renda.rds"),
  targets[year==2026 & round==1 & office==1 & uf=="SP"],incremental=TRUE)
stopifnot(length(invalidated$series)==1L,invalidated$series[[1L]]$uf=="BR")
saveRDS(votes,file.path(cache,"places-2026-BR-1.rds"))
planned <- eleicao_prepare_ufs(c("SP", "DF", "SP"), dry_run = TRUE)
stopifnot(nrow(planned) == 2L, all(planned$status == "planned"), all(is.na(planned$exit_code)))
# A missing national link cannot be promoted from fragments to available.
unlink(file.path(spatial, "links-2026-1.rds"))
saveRDS(links, file.path(spatial, "links-2026-1-UF35.rds"))
missing_map <- eleicao_build_explorer(file.path(folder, "missing"), cache, spatial,
  file.path(folder, "renda.rds"), targets[year == 2026 & round == 1 & office == 1 & uf == "BR"])
stopifnot(length(missing_map$series) == 0L,
          missing_map$availability[[1L]]$status == "unavailable",
          grepl("fragmentos", missing_map$availability[[1L]]$reason))
# Official certification is mandatory for production, including presidency.
uncertified <- copy(votes); attr(uncertified, "vote_audit") <- NULL
expect_error(eleicao_aggregate_contest(uncertified, universe, links, 2026, 1, 1))
tampered <- copy(votes)
bad_audit <- attr(tampered, "vote_audit"); bad_audit$checks$difference[[1L]] <- 1
attr(tampered, "vote_audit") <- bad_audit
expect_error(eleicao_aggregate_contest(tampered, universe, links, 2026, 1, 1))
incomplete <- copy(votes)
incomplete_audit <- attr(incomplete, "vote_audit")
incomplete_audit$checks <- incomplete_audit$checks[uf != "AC"]
attr(incomplete, "vote_audit") <- incomplete_audit
expect_error(eleicao_aggregate_contest(incomplete, universe, links, 2026, 1, 1))
resolved_folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
resolved_temp <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
stopifnot(startsWith(resolved_folder, paste0(resolved_temp, "/")),
          startsWith(basename(resolved_folder), "explorer-fixture-"))
unlink(resolved_folder, recursive = TRUE)
message("PASS: export manifest, BR/UF series, missing source/round/map reasons, partial-map rejection and sequential CLI dry run.")
