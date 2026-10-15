# Generic acquisition layer. Source this file from the repository root.
# The CKAN API lists downloadable files; it does not offer row-level SQL on them.
tse_offices <- c(`1` = "Presidente", `3` = "Governador", `5` = "Senador",
                 `6` = "Deputado federal", `7` = "Deputado estadual",
                 `8` = "Deputado distrital", `11` = "Prefeito", `13` = "Vereador")

tse_require <- function() {
  for (pkg in c("data.table", "httr2", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install package: ", pkg)
  }
}

tse_metadata <- function(year, cache_dir = "data-raw/eleicoes-renda/historico", refresh = FALSE) {
  tse_require()
  year <- as.integer(year)
  if (length(year) != 1L || is.na(year) || year < 1989L || year %% 2L != 0L) stop("Invalid election year")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("resultados-", year, "-metadata.json"))
  if (!file.exists(path) || refresh) {
    response <- httr2::request("https://dadosabertos.tse.jus.br/api/3/action/package_show") |>
      httr2::req_url_query(id = paste0("resultados-", year)) |>
      httr2::req_retry(max_tries = 3) |> httr2::req_perform()
    metadata <- httr2::resp_body_json(response, simplifyVector = TRUE)
    if (!isTRUE(metadata$success)) stop("TSE metadata failed for ", year)
    jsonlite::write_json(metadata, path, auto_unbox = TRUE, pretty = TRUE)
  }
  metadata <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!isTRUE(metadata$success)) stop("Invalid cached metadata: ", path)
  metadata
}

tse_resource <- function(metadata, year, uf = "BR") {
  resources <- data.table::as.data.table(metadata$result$resources)
  uf <- toupper(uf)
  if (length(uf) != 1L || !grepl("^[A-Z]{2}$", uf)) stop("Invalid UF")
  pattern <- paste0("/votacao_secao_", as.integer(year), "_", uf, "[.]zip$")
  hit <- resources[grepl(pattern, url)]
  if (nrow(hit) != 1L) stop("Expected one section-vote resource for ", year, "/", uf,
                            "; found ", nrow(hit), ". No coarser-data substitution is allowed.")
  hit
}

tse_download <- function(year, uf = "BR", cache_dir = "data-raw/eleicoes-renda/historico") {
  resource <- tse_resource(tse_metadata(year, cache_dir), year, uf)
  path <- file.path(cache_dir, basename(resource$url))
  # Reuse the existing national file rather than download a second copy.
  legacy <- file.path(dirname(cache_dir), paste0("votos", year, ".zip"))
  if (uf == "BR" && file.exists(legacy)) path <- legacy
  if (!file.exists(path)) {
    partial <- paste0(path, ".part")
    httr2::request(resource$url) |> httr2::req_timeout(1800) |>
      httr2::req_retry(max_tries = 3) |> httr2::req_perform(path = partial)
    if (!length(utils::unzip(partial, list = TRUE)$Name)) stop("Invalid ZIP: ", partial)
    if (!file.rename(partial, path)) stop("Cannot finish download: ", path)
  }
  if (!length(utils::unzip(path, list = TRUE)$Name)) stop("Invalid cached ZIP: ", path)
  manifest <- data.table::data.table(year = as.integer(year), uf = uf, url = resource$url,
    file = basename(path), md5 = unname(tools::md5sum(path)), checked_at = as.character(Sys.time()))
  data.table::fwrite(manifest, file.path(cache_dir, paste0("source-", year, "-", uf, ".csv")))
  path
}

# Special ballots are exactly 95 (blank), 96 (null), 97 (annulled/pending).
# Do NOT use candidate number < 95: deputies have four/five-digit numbers.
tse_vote_type <- function(number, office) {
  number <- as.integer(number)
  office <- as.integer(office)
  type <- rep("candidate", length(number))
  type[is.na(number) | number <= 0L] <- "invalid_code"
  type[number %in% c(95L, 96L, 97L)] <- "special"
  proportional <- office %in% c(6L, 7L, 8L, 13L)
  type[proportional & number >= 10L & number <= 99L & !number %in% c(95L, 96L, 97L)] <- "party"
  type
}

tse_normalize <- function(x, expected_year = NULL) {
  tse_require()
  x <- data.table::as.data.table(x)
  aliases <- list(year = c("ANO_ELEICAO", "AA_ELEICAO"), round = "NR_TURNO",
    uf = "SG_UF", municipality_tse = "CD_MUNICIPIO", zone = "NR_ZONA", section = "NR_SECAO",
    place = "NR_LOCAL_VOTACAO", office = "CD_CARGO", candidate_id = "NR_VOTAVEL",
    candidate_name = "NM_VOTAVEL", candidate_seq = "SQ_CANDIDATO", votes = "QT_VOTOS", election_id = "CD_ELEICAO",
    election_type = "CD_TIPO_ELEICAO", election_date = "DT_ELEICAO")
  required <- c("year", "round", "uf", "municipality_tse", "zone", "section", "office", "candidate_id", "votes")
  output <- list()
  for (column in names(aliases)) {
    hit <- intersect(aliases[[column]], names(x))
    if (!length(hit)) {
      if (column %in% required) stop("Missing section-vote column: ", paste(aliases[[column]], collapse = "/"))
      output[[column]] <- rep(NA, nrow(x))
    } else output[[column]] <- x[[hit[1L]]]
  }
  output <- data.table::as.data.table(output)
  numeric_columns <- c("year", "round", "zone", "section", "place", "office", "candidate_id", "election_id", "election_type")
  for (column in numeric_columns) data.table::set(output, j = column, value = as.integer(output[[column]]))
  output[, `:=`(uf = toupper(as.character(uf)), municipality_tse = as.character(municipality_tse),
    candidate_name = as.character(candidate_name), candidate_seq = as.character(candidate_seq), votes = as.numeric(votes))]
  if (anyNA(output$votes) || any(!is.finite(output$votes)) || any(output$votes < 0) || any(output$votes %% 1 != 0)) {
    stop("Negative, fractional, missing or nonfinite votes")
  }
  if (anyNA(output[, ..required])) stop("Missing required section-vote values")
  if (!is.null(expected_year) && any(output$year != as.integer(expected_year))) stop("Unexpected election year")
  output[, vote_type := tse_vote_type(candidate_id, office)]
  output[, section_key := paste(uf, municipality_tse, zone, section, sep = ":")]
  output[, place_key := ifelse(!is.na(place) & place > 0L,
    paste(uf, municipality_tse, zone, place, sep = ":"), NA_character_)]
  output[]
}

tse_read_votes <- function(path, year, offices = 1L, rounds = c(1L, 2L), ufs = NULL,
                           election_type = 2L) {
  tse_require()
  members <- utils::unzip(path, list = TRUE)$Name
  # Avoid README, auxiliary files and accidentally reading several UF copies.
  members <- members[grepl(paste0("(^|/)votacao_secao_", year, "_[A-Z]{2}[.]csv$"), members,
                          ignore.case = TRUE)]
  if (length(members) != 1L) stop("Expected exactly one section-vote CSV, found ", length(members))
  if (grepl("_BR[.]csv$", members, ignore.case = TRUE) && any(!offices %in% 1L)) {
    stop("The BR file contains presidency only; use a UF ZIP for other offices")
  }
  folder <- sub("[.]zip$", "", path, ignore.case = TRUE)
  csv <- file.path(folder, members)
  if (!file.exists(csv)) utils::unzip(path, files = members, exdir = folder)
  header <- names(data.table::fread(csv, nrows = 0L, encoding = "Latin-1"))
  wanted <- c("ANO_ELEICAO", "AA_ELEICAO", "NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA",
    "NR_SECAO", "NR_LOCAL_VOTACAO", "CD_CARGO", "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS",
    "CD_ELEICAO", "CD_TIPO_ELEICAO", "DT_ELEICAO", "SQ_CANDIDATO")
  x <- data.table::fread(csv, select = intersect(wanted, header), encoding = "Latin-1",
                        showProgress = interactive())
  # Restrict before normalizing to keep the duplicated long table small.
  x <- x[CD_CARGO %in% offices & NR_TURNO %in% rounds]
  if (!is.null(ufs)) x <- x[SG_UF %in% ufs]
  # General elections are code 2; supplements must not be pooled with them.
  if (!is.null(election_type) && "CD_TIPO_ELEICAO" %in% names(x)) x <- x[CD_TIPO_ELEICAO %in% election_type]
  tse_normalize(x, expected_year = year)
}

tse_place_votes <- function(votes) {
  # Preserve election identity to avoid accidentally pooling supplemental races.
  x <- data.table::as.data.table(votes)
  if (anyNA(x$place_key)) stop("Some sections have no place. Join a SAME-YEAR section register before aggregation.")
  x[vote_type %in% c("candidate", "party"), .(votes = sum(votes)),
    by = .(year, round, election_id, election_date, uf, municipality_tse, zone,
           place, place_key, office, candidate_id, candidate_name, vote_type)]
}

tse_geobr_years <- function() {
  if (!requireNamespace("geobr", quietly = TRUE)) stop("Install geobr")
  catalog <- geobr::list_geobr()
  # geobr releases have used both name and function_name for the function field.
  matched <- apply(as.data.frame(catalog), 1L, function(z) any(grepl("polling", z, ignore.case = TRUE)))
  catalog[matched, , drop = FALSE]
}
