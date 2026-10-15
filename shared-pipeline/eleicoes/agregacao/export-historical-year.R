# Export one historical election while retaining previously published years.
# CLI: Rscript .../export-historical-year.R 2022 [--presidency-only]
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/agregacao/build-explorer.R", encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Choose one historical year: 2022, 2018, 2014 or 2010")
year <- suppressWarnings(as.integer(args[[1L]]))
if (length(year) != 1L || is.na(year) || !year %in% c(2022L, 2018L, 2014L, 2010L))
  stop("Choose one historical year: 2022, 2018, 2014 or 2010")
presidency_only <- "--presidency-only" %in% args
if (any(!args[-1L] %in% "--presidency-only")) stop("Unknown option")
targets <- eleicao_targets(years = year,
  state_years = if (presidency_only) integer() else year,
  income_references = c("national", "state"))
manifest <- eleicao_build_explorer(targets = targets, incremental = TRUE, flush_progress = TRUE)
selected <- Filter(function(item) as.integer(item$year) == year, manifest$series)
message("Historical year ", year, ": ", length(selected), " available series; inspect availability.csv for unavailable contests")
