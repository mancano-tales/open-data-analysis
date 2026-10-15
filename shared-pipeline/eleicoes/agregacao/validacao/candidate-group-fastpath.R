# Exact replacement for candidate-key aggregation, without callbacks per
# already-unique key. No downloads; does not modify the active legal helper.
legal_group_candidates_reference <- function(cand, keys) {
  x <- data.table::as.data.table(cand)
  x[, .(official_nominal = sum(as.numeric(official_nominal)),
    official_nominal_valid = sum(as.numeric(official_nominal_valid)),
    party_id = if (data.table::uniqueN(party_id) == 1L) party_id[1L] else NA_integer_,
    party = if (data.table::uniqueN(party) == 1L) party[1L] else NA_character_,
    destination = paste(sort(unique(destination)), collapse = "|")), by = keys]
}

legal_group_candidates_fast <- function(cand, keys) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
  x <- data.table::as.data.table(cand)
  columns <- c("official_nominal", "official_nominal_valid", "party_id", "party", "destination")
  if (!length(keys) || anyDuplicated(keys) || !all(c(keys, columns) %in% names(x))) stop("Missing candidate grouping columns")
  if (".candidate_first_row" %in% names(x)) stop("Reserved internal column")
  if (!nrow(x)) return(legal_group_candidates_reference(x, keys))
  first_duplicate <- duplicated(x, by = keys)
  repeated <- first_duplicate | duplicated(x, by = keys, fromLast = TRUE)
  # Copy only selected output columns. The caller's table remains unchanged.
  single_index <- which(!repeated)
  singles <- x[single_index, c(keys, columns), with = FALSE]
  singles[, `:=`(official_nominal = as.numeric(official_nominal),
                 official_nominal_valid = as.numeric(official_nominal_valid),
                 # sort(unique(NA_character_)) drops NA, then paste gives "".
                 destination = ifelse(is.na(destination), "", as.character(destination)),
                 .candidate_first_row = single_index)]
  if (any(repeated)) {
    duplicates <- data.table::copy(x[repeated, c(keys, columns), with = FALSE])
    duplicates[, .candidate_first_row := which(repeated)]
    grouped <- duplicates[, .(official_nominal = sum(as.numeric(official_nominal)),
      official_nominal_valid = sum(as.numeric(official_nominal_valid)),
      party_id = if (data.table::uniqueN(party_id) == 1L) party_id[1L] else NA_integer_,
      party = if (data.table::uniqueN(party) == 1L) party[1L] else NA_character_,
      destination = paste(sort(unique(destination)), collapse = "|"),
      .candidate_first_row = .candidate_first_row[1L]), by = keys]
    result <- data.table::rbindlist(list(singles, grouped), use.names = TRUE)
  } else result <- singles
  data.table::setorder(result, .candidate_first_row)
  result[, .candidate_first_row := NULL]
  data.table::setcolorder(result, c(keys, columns))
  result[]
}

# Optional private-environment adapter. It refuses to patch a changed source
# shape and leaves all legal validity/reconciliation assertions untouched.
legal_candidate_fast_environment <- function(source_path = "shared-pipeline/eleicoes/historico/legal-validity.R",
                                              base_environment = NULL) {
  env <- if (is.null(base_environment)) new.env(parent = globalenv()) else base_environment
  if (!is.environment(env)) stop("Expected private helper environment")
  if (is.null(base_environment)) sys.source(source_path, envir = env)
  if (!is.function(env$legal_reconcile_places)) stop("Missing legal reconciliation function")
  env$legal_group_candidates_fast <- legal_group_candidates_fast
  fn <- env$legal_reconcile_places
  replacements <- 0L
  walk <- function(expr) {
    if (!is.call(expr)) return(expr)
    target <- identical(expr[[1L]], as.name("<-")) && identical(expr[[2L]], as.name("cand")) &&
      is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("[")) &&
      identical(expr[[3L]][[2L]], as.name("cand")) &&
      all(c("official_nominal", "official_nominal_valid", "party_id", "party", "destination", "uniqueN") %in%
          all.names(expr[[3L]]))
    if (target) {
      replacements <<- replacements + 1L
      return(quote(cand <- legal_group_candidates_fast(cand, keys)))
    }
    as.call(lapply(as.list(expr), walk))
  }
  body(fn) <- walk(body(fn))
  if (replacements != 1L) stop("Expected exactly one candidate grouping expression; source changed")
  environment(fn) <- env
  env$legal_reconcile_places <- fn
  env
}
