# Sequential resumption with exact candidate fastpath and technical-party fix.
# Does not change the original helper or start on source().
# CLI: Rscript .../resume-2026-optimized.R [UF,UF,...] [--dry-run]
source("shared-pipeline/eleicoes/agregacao/validacao/legal-party-technical-wrapper.R")
source("shared-pipeline/eleicoes/agregacao/validacao/candidate-group-fastpath.R")

legal_optimized_environment <- function() {
  env <- legal_party_technical_environment()
  legal_candidate_fast_environment(base_environment = env)
}

eleicao_resume_optimized <- function(ufs = c("MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
                                     dry_run = FALSE, cache_dir = "data-raw/eleicoes-renda/historico") {
  ufs <- unique(toupper(ufs))
  allowed <- c("MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
  if (!length(ufs) || anyNA(ufs) || any(!ufs %in% allowed)) stop("Unexpected state in optimized resumption")
  states <- vector("list", length(ufs))
  env <- if (!dry_run) legal_optimized_environment() else NULL
  if (!dry_run) data.table::setDTthreads(2L)
  for (i in seq_along(ufs)) {
    state <- ufs[[i]]
    message(if (dry_run) "Would reconcile optimized 2026/" else "Reconciling optimized 2026/", state)
    if (dry_run) {
      states[[i]] <- data.frame(year = 2026L, uf = state, status = "planned", reason = "", stringsAsFactors = FALSE)
      next
    }
    # One state at a time. Errors are reported and later states still proceed.
    outcome <- tryCatch({
      places <- env$legal_reconcile_places(2026L, state, c(3L, 5L, 6L, 7L, 8L), cache_dir)
      audit <- attr(places, "vote_audit")
      stopifnot(identical(audit$status, "reconciled"), all(audit$checks$difference == 0))
      print(audit$checks)
      rm(places)
      unavailable <- audit$unavailable
      partial <- is.data.frame(unavailable) && nrow(unavailable) > 0L
      list(status = if (partial) "partial" else "reconciled",
           reason = if (partial) paste(nrow(unavailable), "disputas excluídas por divergência; consultar CSV de auditoria") else "")
    }, error = function(e) list(status = "failed", reason = conditionMessage(e)))
    states[[i]] <- data.frame(year = 2026L, uf = state, status = outcome$status,
                              reason = outcome$reason, stringsAsFactors = FALSE)
    if (outcome$status == "failed") message("FAILED ", state, ": ", outcome$reason)
    invisible(gc())
    # Save resumable status after each state; cache provenance remains owned by
    # the original helper and every certified output retains its official audit.
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(do.call(rbind, states[seq_len(i)]), file.path(cache_dir, "resume-2026-optimized-status.csv"), row.names = FALSE)
  }
  do.call(rbind, states)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  dry_run <- "--dry-run" %in% args
  state_args <- args[!grepl("^--", args)]
  states <- if (length(state_args)) strsplit(state_args[[1L]], ",", fixed = TRUE)[[1L]] else
    c("MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
  result <- eleicao_resume_optimized(states, dry_run = dry_run)
  print(result, row.names = FALSE)
  if (any(result$status == "failed")) quit(status = 1L)
}
