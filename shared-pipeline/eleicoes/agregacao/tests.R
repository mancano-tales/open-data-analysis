if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/agregacao/centis.R", encoding = "UTF-8")
library(data.table)
setDTthreads(2L)
fixture_aggregate <- function(...) eleicao_aggregate_contest(..., require_reconciled = FALSE)

expect_error <- function(expr) {
  failed <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(failed)
}

# Two tracts per national percentile. State exports retain national ranks.
renda <- data.frame(code_tract = c(sprintf("35%013d", 1:100), sprintf("33%013d", 101:200)),
                    V06001 = 1, V06004 = seq_len(200) * 1212)
universe <- eleicao_income_universe(renda)
links <- universe[, .(code_tract, place_key = paste(uf, 1, 1, rep(1:2, 100), sep = ":"), distancia_m = 100)]
make_rows <- function(uf, place, ids, counts, office = 1L, type = "candidate") {
  data.table(year = 2026L, round = 1L, uf = uf, municipality_tse = "1", zone = 1L,
    section = place, place = place, office = office, candidate_id = ids,
    candidate_name = paste("Nome", ids), votes = counts, vote_type = type)
}
votes <- rbindlist(list(make_rows("SP", 1L, c(13, 22), c(80, 20)),
                        make_rows("SP", 2L, c(22, 30), c(10, 90)),
                        make_rows("RJ", 1L, c(13, 22), c(60, 40))))
result <- fixture_aggregate(votes, universe, links, 2026, 1, 1)
p <- as.data.table(result$points)
stopifnot(nrow(p) == 300L, nrow(result$candidates) == 3L,
          result$metadata$coverage$included_tracts == 150L,
          result$metadata$coverage$total_tracts == 200L,
          p[centil == 1 & candidate_id == "13", votacao] == 40,
          p[centil == 1 & candidate_id == "30", votacao] == 45,
          p[centil == 1 & candidate_id == "13", renda_sm] == 1.5,
          p[centil == 51 & candidate_id == "13", n_setores] == 1L,
          p[centil == 51 & candidate_id == "13", n_setores_total] == 2L,
          p[centil == 51 & candidate_id == "13", votacao] == 60)

# Multiple tracts at the same place/percentile must contribute their full weight.
replicated_links <- copy(links)
replicated_links[code_tract %in% universe[centil == 1, code_tract], place_key := "SP:1:1:1"]
replicated <- fixture_aggregate(votes, universe, replicated_links, 2026, 1, 1)
rp <- as.data.table(replicated$points)
stopifnot(rp[centil == 1 & candidate_id == "13", votacao] == 80,
          rp[centil == 1 & candidate_id == "13", n_setores] == 2L)

# A missing local/cargo changes coverage; an absent candidate at an observed
# local is zero. Neither operation changes income ranks or census mean income.
filtered_links <- links[place_key != "RJ:1:1:1"]
missing <- fixture_aggregate(votes, universe, filtered_links, 2026, 1, 1)
pm <- as.data.table(missing$points)
stopifnot(pm[centil == 51 & candidate_id == "13", n_setores] == 0L,
          is.na(pm[centil == 51 & candidate_id == "13", votacao]),
          pm[centil == 51 & candidate_id == "13", renda_sm] == 101.5)

# Independently brute-force the small fixture at tract level.
v <- copy(votes)[, place_key := paste(uf, municipality_tse, zone, place, sep = ":")]
den <- v[, .(denominator = sum(votes)), by = place_key]
matched <- merge(universe, links, by = "code_tract")
matched <- merge(matched, den, by = "place_key")
for (candidate in c(13, 22, 30)) {
  vv <- v[candidate_id == candidate, .(numerator = sum(votes)), by = place_key]
  brute <- merge(matched, vv, by = "place_key", all.x = TRUE)
  brute[is.na(numerator), numerator := 0]
  brute <- brute[, .(expected = mean(100 * numerator / denominator)), by = centil]
  actual <- p[candidate_id == as.character(candidate)]
  comparison <- merge(actual, brute, by = "centil")
  stopifnot(max(abs(comparison$votacao - comparison$expected)) < 1e-12)
}

# Senate shares use valid votes, even when two votes per elector are allowed.
senate <- make_rows("SP", 1L, c(111, 222), c(100, 100), office = 5L)
sen <- fixture_aggregate(senate, universe, links, 2026, 1, 5, uf = "SP")
sp <- as.data.table(sen$points)
stopifnot(all(sp$votacao[sp$n_setores > 0] == 50),
          sen$metadata$coverage$total_tracts == 100L,
          all(sp$n_setores_total[sp$centil > 50] == 0L),
          !is.null(sen$metadata$senate_note))

# Deputies: two nominal candidates plus party-only votes in the denominator.
deputies <- make_rows("SP", 1L, c(1313, 2222, 13, 95), c(30, 50, 20, 1000), office = 6L,
                      type = c("candidate", "candidate", "party", "special"))
dep <- fixture_aggregate(deputies, universe, links, 2026, 1, 6, uf = "SP")
dp <- as.data.table(dep$points)
stopifnot(nrow(dep$candidates) == 2L, dp[centil == 1 & candidate_id == "1313", votacao] == 30,
          dp[centil == 1 & candidate_id == "2222", votacao] == 50,
          sum(dp[centil == 1, votacao]) == 80)
dep_party <- fixture_aggregate(deputies, universe, links, 2026, 1, 6, uf = "SP", include_party_series = TRUE)
stopifnot(nrow(dep_party$candidates) == 3L, sum(as.data.table(dep_party$points)[centil == 1, votacao]) == 100)

# A provisional official snapshot remains provisional after successful vote
# reconciliation, and its provenance survives aggregation/JSON export.
certified_deputies <- copy(deputies)
snapshot <- list(office = 6L, status = "provisional", generation_date = "13/10/2026",
                 generation_time = "16:26:47", final_totalization = "n", progress = "p",
                 url = "https://resultados.tse.jus.br/test-only.json", md5 = "fixture-md5")
attr(certified_deputies, "vote_audit") <- list(status = "reconciled", method = "test fixture",
  checks = data.frame(year = 2026L, round = 1L, office = 6L, uf = "SP",
    official_valid_votes = 100, computed_valid_votes = 100, difference = 0),
  provisional = TRUE, snapshots = list(`6` = snapshot))
provisional_result <- eleicao_aggregate_contest(certified_deputies, universe, links, 2026, 1, 6, uf = "SP")
stopifnot(isTRUE(provisional_result$metadata$vote_audit$provisional),
          identical(provisional_result$metadata$vote_audit$snapshots[[1]], snapshot))
provisional_path <- tempfile(fileext = ".json")
eleicao_write_dataset(provisional_result, provisional_path, "test-only.json")
provisional_json <- jsonlite::read_json(provisional_path)
stopifnot(isTRUE(provisional_json$metadata$vote_audit$provisional),
          provisional_json$metadata$vote_audit$snapshots[[1]]$md5 == "fixture-md5")
unlink(provisional_path)

# Documented destinations survive aggregation and JSON, without leaking the
# evidence of another year, round, office or state from a combined cache.
sourced_votes <- copy(certified_deputies)
sourced_audit <- attr(sourced_votes, "vote_audit")
source_rows <- data.frame(year = c(2026L, 2018L, 2026L, 2026L, 2026L),
  round = c(1L, 1L, 2L, 1L, 1L), office = c(6L, 6L, 6L, 5L, 6L),
  uf = c("SP", "SP", "SP", "SP", "RO"),
  source_id = c("selected", "other-year", "other-round", "other-office", "other-state"),
  url = "https://example.invalid/official-report.pdf", source_date = "2026-10-01",
  source_page = 43L, source_total_page = 83L, sha256 = strrep("a", 64), bytes = 12345L)
sourced_audit$official_destination_sources <- source_rows
attr(sourced_votes, "vote_audit") <- sourced_audit
sourced_result <- eleicao_aggregate_contest(sourced_votes, universe, links, 2026, 1, 6, uf = "SP")
stopifnot(identical(sourced_result$metadata$vote_audit$official_destination_sources$source_id, "selected"),
  nrow(attr(sourced_votes, "vote_audit")$official_destination_sources) == 5L)
sourced_path <- tempfile(fileext = ".json")
eleicao_write_dataset(sourced_result, sourced_path, "test-only.json")
sourced_json <- jsonlite::read_json(sourced_path)
source_json <- sourced_json$metadata$vote_audit$official_destination_sources
stopifnot(length(source_json) == 1L, source_json[[1L]]$source_id == "selected",
  source_json[[1L]]$sha256 == strrep("a", 64), source_json[[1L]]$source_page == 43,
  source_json[[1L]]$url == source_rows$url[[1L]])
unlink(sourced_path)
sourced_audit$official_destination_sources <- data.frame()
attr(sourced_votes, "vote_audit") <- sourced_audit
stopifnot(is.null(eleicao_vote_audit(sourced_votes, 2026, 1, 6, "SP")$official_destination_sources))
sourced_audit$official_destination_sources <- source_rows[, setdiff(names(source_rows), "uf")]
attr(sourced_votes, "vote_audit") <- sourced_audit
expect_error(eleicao_vote_audit(sourced_votes, 2026, 1, 6, "SP"))

# The same combined-cache flag must not mark an unrelated office provisional.
certified_senate <- copy(senate)
senate_audit <- attr(certified_deputies, "vote_audit")
senate_audit$checks$office <- 5L
senate_audit$checks$official_valid_votes <- 200
senate_audit$checks$computed_valid_votes <- 200
attr(certified_senate, "vote_audit") <- senate_audit
senate_certified <- eleicao_vote_audit(certified_senate, 2026, 1, 5, "SP")
stopifnot(!senate_certified$provisional, !length(senate_certified$snapshots))

# Domestic national audits must exclude overseas votes even when the source
# cache legitimately certifies them. Missing domestic states still fail.
domestic <- data.table(year=2010L, round=1L, office=1L,
  uf=c(unname(eleicao_ufs), "ZZ"), votes=c(rep(10,27), 999), vote_type="candidate")
domestic_checks <- domestic[, .(year, round, office, uf,
  official_valid_votes=votes, computed_valid_votes=votes, difference=0)]
attr(domestic,"vote_audit") <- list(status="reconciled", method="test fixture", checks=domestic_checks)
domestic_audit <- eleicao_vote_audit(domestic,2010,1,1,"BR")
stopifnot(domestic_audit$official_valid_votes==270, domestic_audit$computed_valid_votes==270,
  domestic_audit$checked_units==27, !"ZZ" %in% domestic_audit$ufs)
incomplete_domestic <- copy(domestic)
attr(incomplete_domestic,"vote_audit") <- list(status="reconciled", method="test fixture",
  checks=domestic_checks[uf!="AC"])
expect_error(eleicao_vote_audit(incomplete_domestic,2010,1,1,"BR"))

# National and state income references share votes/audit/coverage, but use
# different COMPLETE income universes. SP incomes 1..100 and RJ 101..200
# deliberately differ, so a national percentile is not a state percentile.
universe_before_reference <- copy(universe)
explicit_national <- fixture_aggregate(votes, universe, links, 2026, 1, 1,
                                      income_reference = "national")
stopifnot(identical(result$points, explicit_national$points),
          identical(result$candidates, explicit_national$candidates),
          identical(result$metadata$coverage, explicit_national$metadata$coverage))
state_result <- eleicao_aggregate_contest(certified_deputies, universe, links, 2026, 1, 6,
                                         uf = "SP", income_reference = "state")
state_points <- as.data.table(state_result$points)
stopifnot(identical(universe, universe_before_reference),
          identical(state_result$metadata$vote_audit, provisional_result$metadata$vote_audit),
          identical(state_result$metadata$coverage, provisional_result$metadata$coverage),
          state_result$metadata$income_reference == "state", state_result$metadata$percentile_scope == "SP",
          provisional_result$metadata$income_reference == "national", provisional_result$metadata$percentile_scope == "BR",
          state_points[centil == 2 & candidate_id == "1313", n_setores] == 0L,
          state_points[centil == 2 & candidate_id == "1313", n_setores_total] == 1L,
          state_points[centil == 2 & candidate_id == "1313", renda_sm] == 2,
          is.na(state_points[centil == 2 & candidate_id == "1313", votacao]),
          state_points[centil == 100 & candidate_id == "1313", renda_sm] == 100)

# Independent base-R rank and tract-level calculation for the state reference.
state_universe <- as.data.frame(universe[uf == "SP"])
state_universe <- state_universe[order(state_universe$income, state_universe$code_tract), ]
state_universe$centil <- floor((seq_len(nrow(state_universe)) - 1L) * 100 / nrow(state_universe)) + 1L
state_votes <- as.data.table(certified_deputies)[vote_type %in% c("candidate", "party")]
state_votes[, place_key := paste(uf, municipality_tse, zone, place, sep = ":")]
state_denominator <- state_votes[, .(denominator = sum(votes)), by = place_key]
state_matched <- merge(state_universe, as.data.frame(links), by = "code_tract")
state_matched <- merge(state_matched, as.data.frame(state_denominator), by = "place_key")
for (candidate in c(1313, 2222)) {
  nominal <- state_votes[candidate_id == candidate, .(numerator = sum(votes)), by = place_key]
  brute_state <- merge(state_matched, as.data.frame(nominal), by = "place_key", all.x = TRUE)
  brute_state$numerator[is.na(brute_state$numerator)] <- 0
  expected <- tapply(100 * brute_state$numerator / brute_state$denominator, brute_state$centil, mean)
  actual <- state_points[candidate_id == as.character(candidate)]
  stopifnot(max(abs(actual$votacao[match(as.integer(names(expected)), actual$centil)] - as.numeric(expected))) < 1e-12)
}
# Excluding the poorest observed tract must not move the second/third tract.
state_filtered_links <- copy(links)
state_filtered_links[code_tract == state_universe$code_tract[[1L]], distancia_m := 6000]
state_filtered <- eleicao_aggregate_contest(certified_deputies, universe, state_filtered_links,
  2026, 1, 6, uf = "SP", income_reference = "state")
state_filtered_points <- as.data.table(state_filtered$points)
stopifnot(state_filtered$metadata$coverage$included_tracts == 49L,
          state_filtered_points[centil == 1 & candidate_id == "1313", n_setores] == 0L,
          state_filtered_points[centil == 1 & candidate_id == "1313", renda_sm] == 1,
          state_filtered_points[centil == 3 & candidate_id == "1313", renda_sm] == 3,
          state_filtered_points[centil == 3 & candidate_id == "1313", votacao] == 30)

national_item <- eleicao_manifest_item(provisional_result, "national.json")
state_item <- eleicao_manifest_item(state_result, "state.json")
legacy_metadata <- provisional_result; legacy_metadata$metadata$income_reference <- NULL
legacy_item <- eleicao_manifest_item(legacy_metadata, "legacy.json")
stopifnot(national_item$id == "2026-1-6-SP", national_item$income_reference == "national",
          state_item$id == "2026-1-6-SP-state", state_item$income_reference == "state",
          legacy_item$id == national_item$id, legacy_item$income_reference == "national")
expect_error(fixture_aggregate(votes, universe, links, 2026, 1, 1, income_reference = "state"))
expect_error(fixture_aggregate(votes, universe, links, 2026, 1, 1, uf = "SP", income_reference = "municipality"))

# Essential rejection paths avoid pooling UFs or duplicating census weights.
expect_error(fixture_aggregate(deputies, universe, links, 2026, 1, 6))
expect_error(fixture_aggregate(votes, universe, rbind(links, links[1]), 2026, 1, 1))
bad <- copy(votes); bad[, votes := -1]
expect_error(fixture_aggregate(bad, universe, links, 2026, 1, 1))
pooled <- copy(votes); pooled[, election_id := seq_len(.N)]
expect_error(fixture_aggregate(pooled, universe, links, 2026, 1, 1))

# JSON must retain arrays with one candidate and explicitly null missing values.
path <- tempfile(fileext = ".json")
item <- eleicao_write_dataset(missing, path, "data/2026-1-1-BR.json")
roundtrip <- jsonlite::read_json(path, simplifyVector = TRUE)
stopifnot(identical(roundtrip$metadata$percentile_scope, "BR"),
          item$office == "1", item$status == "available", nrow(roundtrip$points) == 300L,
          anyNA(roundtrip$points$votacao), is.data.frame(roundtrip$candidates))
unlink(path)
message("PASS: national/state fixed ranks, audit/coverage invariance, complete denominators, zero versus missing, brute-force equivalence, senate, party votes and JSON.")
