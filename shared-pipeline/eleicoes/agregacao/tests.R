source("shared-pipeline/eleicoes/agregacao/centis.R")
library(data.table)
setDTthreads(2L)

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
result <- eleicao_aggregate_contest(votes, universe, links, 2026, 1, 1)
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
replicated <- eleicao_aggregate_contest(votes, universe, replicated_links, 2026, 1, 1)
rp <- as.data.table(replicated$points)
stopifnot(rp[centil == 1 & candidate_id == "13", votacao] == 80,
          rp[centil == 1 & candidate_id == "13", n_setores] == 2L)

# A missing local/cargo changes coverage; an absent candidate at an observed
# local is zero. Neither operation changes income ranks or census mean income.
filtered_links <- links[place_key != "RJ:1:1:1"]
missing <- eleicao_aggregate_contest(votes, universe, filtered_links, 2026, 1, 1)
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
sen <- eleicao_aggregate_contest(senate, universe, links, 2026, 1, 5, uf = "SP")
sp <- as.data.table(sen$points)
stopifnot(all(sp$votacao[sp$n_setores > 0] == 50),
          sen$metadata$coverage$total_tracts == 100L,
          all(sp$n_setores_total[sp$centil > 50] == 0L),
          !is.null(sen$metadata$senate_note))

# Deputies: two nominal candidates plus party-only votes in the denominator.
deputies <- make_rows("SP", 1L, c(1313, 2222, 13, 95), c(30, 50, 20, 1000), office = 6L,
                      type = c("candidate", "candidate", "party", "special"))
dep <- eleicao_aggregate_contest(deputies, universe, links, 2026, 1, 6, uf = "SP")
dp <- as.data.table(dep$points)
stopifnot(nrow(dep$candidates) == 2L, dp[centil == 1 & candidate_id == "1313", votacao] == 30,
          dp[centil == 1 & candidate_id == "2222", votacao] == 50,
          sum(dp[centil == 1, votacao]) == 80)
dep_party <- eleicao_aggregate_contest(deputies, universe, links, 2026, 1, 6, uf = "SP", include_party_series = TRUE)
stopifnot(nrow(dep_party$candidates) == 3L, sum(as.data.table(dep_party$points)[centil == 1, votacao]) == 100)

# Essential rejection paths avoid pooling UFs or duplicating census weights.
expect_error(eleicao_aggregate_contest(deputies, universe, links, 2026, 1, 6))
expect_error(eleicao_aggregate_contest(votes, universe, rbind(links, links[1]), 2026, 1, 1))
bad <- copy(votes); bad[, votes := -1]
expect_error(eleicao_aggregate_contest(bad, universe, links, 2026, 1, 1))
pooled <- copy(votes); pooled[, election_id := seq_len(.N)]
expect_error(eleicao_aggregate_contest(pooled, universe, links, 2026, 1, 1))

# JSON must retain arrays with one candidate and explicitly null missing values.
path <- tempfile(fileext = ".json")
item <- eleicao_write_dataset(missing, path, "data/2026-1-1-BR.json")
roundtrip <- jsonlite::read_json(path, simplifyVector = TRUE)
stopifnot(identical(roundtrip$metadata$percentile_scope, "BR"),
          item$office == "1", item$status == "available", nrow(roundtrip$points) == 300L,
          anyNA(roundtrip$points$votacao), is.data.frame(roundtrip$candidates))
unlink(path)
message("PASS: fixed ranks, complete denominators, zero versus missing, state scope, brute-force equivalence, senate, party votes and JSON.")
