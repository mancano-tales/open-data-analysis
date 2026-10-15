source("shared-pipeline/eleicoes/agregacao/validacao/candidate-group-fastpath.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(2L)
# Interleave first appearances to test stable group order as well as values.
x <- data.table(municipality = c("a", "b", "a", "c", "d", "c", "e", "f", "e", "g"),
  candidate = c(13L, 22L, 13L, 30L, 44L, 30L, 55L, 66L, 55L, 77L),
  official_nominal = c(10L, 5L, 20L, 0L, NA_integer_, 3L, 7L, 0L, 8L, 1L),
  official_nominal_valid = c(10L, 5L, 0L, 0L, NA_integer_, 0L, 7L, 0L, 8L, 1L),
  party_id = c(13L, 22L, 13L, 30L, NA_integer_, 31L, 55L, 66L, NA_integer_, 77L),
  party = c("PT", "PL", "PT", "A", NA_character_, "B", "X", "Y", NA_character_, "Z"),
  destination = c("Válido", NA_character_, "Anulado", "Legenda", "Válido", "Legenda", NA_character_, "", "Válido", "Válido"))
keys <- c("municipality", "candidate")
before <- copy(x)
compare <- function(input) {
  expected <- legal_group_candidates_reference(input, keys)
  actual <- legal_group_candidates_fast(input, keys)
  stopifnot(isTRUE(all.equal(as.data.frame(actual), as.data.frame(expected), check.attributes = TRUE)))
  actual
}
actual <- compare(x)
stopifnot(identical(x, before), actual[municipality == "b", destination] == "",
          actual[municipality == "a", destination] == "Anulado|Válido",
          is.na(actual[municipality == "c", party_id]), is.na(actual[municipality == "c", party]),
          is.na(actual[municipality == "d", official_nominal]),
          is.na(actual[municipality == "e", party_id]))
invisible(compare(x[!duplicated(x, by = keys)]))
invisible(compare(x[municipality %in% c("a", "c", "e")]))
invisible(compare(x[0]))
# Missing key values retain the same grouping semantics.
with_na <- copy(x); with_na[municipality == "a", municipality := NA_character_]
invisible(compare(with_na))
# Small randomized cases cover zero/one/many duplicates and NA combinations.
set.seed(20261015)
for (iteration in 1:20) {
  fixture <- x[sample(seq_len(nrow(x)), 100L, replace = TRUE)]
  invisible(compare(fixture))
}
env <- legal_candidate_fast_environment()
stopifnot(is.function(env$legal_reconcile_places),
  grepl("legal_group_candidates_fast", paste(deparse(body(env$legal_reconcile_places)), collapse = " ")))
# Small in-memory benchmark only. No production files or national jobs.
n <- 20000L
benchmark <- data.table(municipality = as.character(seq_len(n)), candidate = seq_len(n),
  official_nominal = 1L, official_nominal_valid = 1L, party_id = 13L, party = "PT", destination = "Válido")
reference_time <- system.time(expected <- legal_group_candidates_reference(benchmark, keys))["elapsed"]
fast_time <- system.time(fast <- legal_group_candidates_fast(benchmark, keys))["elapsed"]
stopifnot(isTRUE(all.equal(as.data.frame(expected), as.data.frame(fast))))
print(data.frame(keys = n, reference_seconds = as.numeric(reference_time), fast_seconds = as.numeric(fast_time)))
message("PASS: unique/duplicate/NA keys, missing values, multiple destinations, ambiguous parties, ordering and private adapter.")
