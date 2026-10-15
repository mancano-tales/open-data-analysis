# Narrow fix applied to a private function environment, leaving the active
# historical helper on disk untouched. Run CLI with PB,SE after fixture tests.
legal_party_technical_environment <- function(source_path = "shared-pipeline/eleicoes/historico/legal-validity.R") {
  env <- new.env(parent = globalenv())
  sys.source(source_path, envir = env)
  original <- env$legal_reconcile_places
  if (!is.function(original)) stop("Missing legal reconciliation function")
  replacements <- 0L
  transform <- function(expr) {
    if (!is.call(expr)) return(expr)
    target <- identical(expr[[1L]], as.name("<-")) && identical(expr[[2L]], as.name("party_valid")) &&
      is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("merge"))
    if (target) {
      replacements <<- replacements + 1L
      return(substitute({
        ORIGINAL
        # Unknown two-digit ballots are not valid party votes. Their exact
        # original place counts enter the technical-null audit; no allocation.
        party_technical <- party_valid[is.na(party_total),
          .(technical_computed = sum(votes)), by = basekeys]
        if (nrow(party_technical)) {
          technical <- data.table::rbindlist(list(technical, party_technical), use.names = TRUE)[,
            .(technical_computed = sum(technical_computed)), by = basekeys]
        }
        party_valid <- party_valid[!is.na(party_total)]
      }, list(ORIGINAL = expr)))
    }
    as.call(lapply(as.list(expr), transform))
  }
  body(original) <- transform(body(original))
  if (replacements != 1L) stop("Expected exactly one party join; source changed, refusing to patch")
  environment(original) <- env
  env$legal_reconcile_places <- original
  env
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  states <- if (length(args)) strsplit(toupper(args[[1L]]), ",", fixed = TRUE)[[1L]] else c("PB", "SE")
  if (any(!states %in% c("PB", "SE"))) stop("This diagnosis wrapper is restricted to PB and SE")
  env <- legal_party_technical_environment()
  data.table::setDTthreads(2L)
  for (state in states) {
    result <- env$legal_reconcile_places(2026L, state, c(3L, 5L, 6L, 7L, 8L))
    audit <- attr(result, "vote_audit")
    stopifnot(identical(audit$status, "reconciled"), all(audit$checks$difference == 0))
    print(audit$checks)
    rm(result); invisible(gc())
  }
}
