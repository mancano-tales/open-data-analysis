# Sequential wrapper around the historical acquisition CLI; never parallelizes downloads.
# CLI: Rscript .../prepare-2026-all-ufs.R [UF,UF,...] [--dry-run]
source("shared-pipeline/eleicoes/agregacao/centis.R")

eleicao_prepare_ufs <- function(ufs = unname(eleicao_ufs), dry_run = FALSE, year = 2026L,
                                offices = c(3L, 5L, 6L, 7L, 8L),
                                prepare_script = "shared-pipeline/eleicoes/historico/legal-validity.R") {
  ufs <- unique(toupper(ufs)); year <- as.integer(year); offices <- unique(as.integer(offices))
  if (anyNA(ufs) || !length(ufs) || any(!ufs %in% unname(eleicao_ufs))) stop("Invalid state list")
  if (length(year) != 1L || is.na(year) || year %% 4L != 2L) stop("Expected general-election year")
  if (anyNA(offices) || !length(offices) || any(!offices %in% c(1L, 3L, 5L, 6L, 7L, 8L))) stop("Invalid offices")
  if (!file.exists(prepare_script)) stop("Missing historical prepare CLI")
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  result <- vector("list", length(ufs))
  for (i in seq_along(ufs)) {
    state <- ufs[[i]]
    arguments <- c("--vanilla", shQuote(prepare_script), as.character(year), state, paste(offices, collapse = ","))
    message(if (dry_run) "Would prepare " else "Preparing ", year, "/", state)
    status <- if (dry_run) NA_integer_ else system2(executable, arguments, stdout = "", stderr = "")
    result[[i]] <- data.frame(year = year, uf = state, offices = paste(offices, collapse = ","),
      status = if (dry_run) "planned" else if (identical(status, 0L)) "prepared" else "failed",
      exit_code = status, stringsAsFactors = FALSE)
    # A failure in one state does not prevent resuming or preparing later states.
    invisible(gc())
  }
  do.call(rbind, result)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  dry_run <- "--dry-run" %in% args
  state_arg <- args[!grepl("^--", args)]
  ufs <- if (length(state_arg)) strsplit(state_arg[[1L]], ",", fixed = TRUE)[[1L]] else unname(eleicao_ufs)
  result <- eleicao_prepare_ufs(ufs, dry_run = dry_run)
  print(result, row.names = FALSE)
  if (!dry_run) {
    write.csv(result, "data-raw/eleicoes-renda/historico/prepare-2026-all-ufs-status.csv", row.names = FALSE)
    if (any(result$status == "failed")) quit(status = 1L)
  }
}
