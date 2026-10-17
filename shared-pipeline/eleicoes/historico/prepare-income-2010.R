# Prepare a separate census input; this does not change the live explorer's
# income reference or reuse Census 2022 geometry. Run from the repository root.
prepare_income_2010 <- function(destination = "data-raw/eleicoes-renda/censo2010") {
  stopifnot(requireNamespace("censobr", quietly = TRUE),
    requireNamespace("arrow", quietly = TRUE), requireNamespace("dplyr", quietly = TRUE),
    requireNamespace("jsonlite", quietly = TRUE))
  options(timeout = max(600, getOption("timeout")))
  if ("set_cpu_count" %in% getNamespaceExports("arrow")) arrow::set_cpu_count(2L)
  data <- censobr::read_tracts(year = 2010, dataset = "Basico",
    as_data_frame = FALSE, showProgress = FALSE)
  required <- c("code_tract", "code_muni", "code_state", "V001", "V005", "V007")
  if (!all(required %in% names(data))) stop("Unexpected Census 2010 Basico schema")
  wanted <- unique(c(required, intersect(c("situacao", "code_situacao"), names(data))))
  income <- as.data.frame(dplyr::collect(dplyr::select(data, dplyr::all_of(wanted))))
  income$code_tract <- if (is.numeric(income$code_tract)) sprintf("%.0f", income$code_tract) else as.character(income$code_tract)
  if (!nrow(income) || anyNA(income$code_tract) || anyDuplicated(income$code_tract)) stop("Invalid Census 2010 tract keys")
  if (any(substr(income$code_tract, 1L, 7L) != sprintf("%.0f", income$code_muni)) ||
      any(substr(income$code_tract, 1L, 2L) != sprintf("%.0f", income$code_state))) stop("Census 2010 geographic identifiers differ")
  if (any(is.finite(income$V001) & (income$V001 < 0 | income$V001 %% 1 != 0))) stop("Invalid Census 2010 household/head counts")
  income$eligible_income <- is.finite(income$V001) & income$V001 > 0 &
    is.finite(income$V007) & income$V007 > 0
  if (!any(income$eligible_income)) stop("No eligible 2010 income observations")
  files <- data$files
  if (!is.character(files) || !length(files) || any(!file.exists(files))) stop("Cannot fingerprint downloaded Census 2010 input")
  hashes <- unname(tools::md5sum(files))
  if (anyNA(hashes)) stop("Cannot hash downloaded Census 2010 input")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  output <- file.path(destination, "renda2010-basico.rds")
  saveRDS(income, output)
  release <- get("censobr_env", asNamespace("censobr"))$data_release
  metadata <- list(schema_version = 1L, census_year = 2010L, dataset = "Basico",
    status = "prepared_not_integrated", income_variable = "V007", head_count_variable = "V001",
    income_definition = "Mean nominal monthly income of responsible people WITH income in permanent private households",
    other_income_variable = "V005 includes responsible people both with and without income; not interchangeable with V007",
    rows = nrow(income), eligible_rows = sum(income$eligible_income),
    censobr_version = as.character(utils::packageVersion("censobr")),
    data_release = release,
    downloaded_files = data.frame(file = basename(files), md5 = hashes),
    output_md5 = unname(tools::md5sum(output)),
    prepared_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    dictionary = "https://github.com/ipea/censobr_prep_data/releases/download/censo_docs/2010_dictionary_tracts.pdf",
    dictionary_pages = c(43L, 44L),
    limitations = c("Census 2010 geometry and an explicit year/reference selector are still required before publication",
      "Tract income and nearby polling-place votes do not identify individual voter income",
      "Income is nominal in 2010 reais; no minimum-wage conversion is applied by this preparation step"))
  jsonlite::write_json(metadata, file.path(destination, "metadata.json"), pretty = TRUE, auto_unbox = TRUE, dataframe = "rows")
  message("Prepared Census 2010 input: ", nrow(income), " tracts, ", sum(income$eligible_income), " income-eligible; not integrated into published curves.")
  invisible(metadata)
}

if (sys.nframe() == 0L) prepare_income_2010()
