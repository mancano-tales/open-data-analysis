# Isolated Acre pilot: Census 2010 income/geometry and 2010 polling places.
# No candidate aggregation, public exports or changes to the Census2022 pipeline.
pilot2010_require <- function() {
  for (package in c("data.table", "sf", "s2", "jsonlite", "arrow", "duckdb", "DBI", "geobr", "digest"))
    if (!requireNamespace(package, quietly = TRUE)) stop("Existing package required: ", package)
}
pilot2010_code <- function(x) {
  if (inherits(x, "integer64")) return(as.character(x))
  if (is.numeric(x)) return(ifelse(is.finite(x), sprintf("%.0f", x), NA_character_))
  as.character(x)
}
pilot2010_free_bytes <- function() {
  if (.Platform$OS.type == "windows") {
    command <- "([System.IO.DriveInfo]::new((Get-Item .).PSDrive.Root)).AvailableFreeSpace"
    value <- suppressWarnings(as.numeric(system2("powershell.exe", c("-NoProfile", "-NonInteractive", "-Command", shQuote(command)), stdout = TRUE)))
    if (length(value) != 1L || !is.finite(value)) stop("Cannot verify free disk space")
    value
  } else {
    lines <- system2("df", c("-Pk", "."), stdout = TRUE)
    as.numeric(strsplit(trimws(tail(lines, 1)), "[[:space:]]+")[[1]][4]) * 1024
  }
}
pilot2010_hash <- function(path, algorithm = "md5") {
  if (algorithm == "md5") return(unname(tools::md5sum(path)))
  if (!requireNamespace("digest", quietly = TRUE)) stop("Existing digest package required for SHA256")
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
pilot2010_download <- function(url, path, size, sha256 = NULL) {
  if (!startsWith(url, "https://github.com/ipea/geobr_prep_data/")) stop("Nonofficial geometry source rejected")
  if (!is.finite(size) || size <= 0 || size > 2 * 1024^3) stop("Geometry asset exceeds the 2GiB cap")
  if (file.exists(path)) {
    if (file.info(path)$size != size || (!is.null(sha256) && pilot2010_hash(path, "sha256") != sha256)) stop("Cached geometry differs from official asset")
    return(invisible(path))
  }
  if (pilot2010_free_bytes() - size < 20 * 1024^3) stop("Download would violate the 20GiB free-space reserve")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  args <- c("--fail", "--location", "--silent", "--show-error", "--max-time", "1200", "--max-filesize", as.character(size),
    "--output", shQuote(paste0(path, ".part")), shQuote(url))
  status <- system2(if (.Platform$OS.type == "windows") "curl.exe" else "curl", args)
  if (status != 0L || !file.exists(paste0(path, ".part")) || file.info(paste0(path, ".part"))$size != size ||
      (!is.null(sha256) && pilot2010_hash(paste0(path, ".part"), "sha256") != sha256)) stop("Bounded geometry download failed verification")
  if (!file.rename(paste0(path, ".part"), path)) stop("Cannot install geometry cache")
  if (pilot2010_free_bytes() < 20 * 1024^3) stop("Disk reserve violated after geometry download")
  invisible(path)
}

# DuckDB scans locally with a 1GB memory limit; only AC WKB is collected.
# The R materialization is refused above 128MiB of selected WKB geometry.
pilot2010_read_ac <- function(path) {
  pilot2010_require()
  schema <- arrow::ParquetFileReader$create(path)$GetSchema()
  geo <- schema$metadata[["geo"]]
  if (is.null(geo)) stop("Missing GeoParquet geometry metadata")
  geo <- jsonlite::fromJSON(geo, simplifyVector = FALSE)
  geom <- geo$primary_column
  geometry_metadata <- geo$columns[[geom]]
  crs <- geometry_metadata$crs
  if ("crs" %in% names(geometry_metadata) && is.null(crs)) stop("Unknown explicit-null GeoParquet source CRS")
  if (is.null(crs)) crs <- "OGC:CRS84" # GeoParquet's specified default, recorded below.
  if (is.list(crs)) crs <- as.character(jsonlite::toJSON(crs, auto_unbox = TRUE))
  source_crs <- sf::st_crs(crs)
  if (is.na(source_crs)) stop("Unknown GeoParquet source CRS")
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", config = list(memory_limit = "1GB", threads = "2"))
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(connection, "SET enable_geoparquet_conversion=false")
  table <- paste0("read_parquet(", DBI::dbQuoteString(connection, normalizePath(path, winslash = "/")), ")")
  columns <- DBI::dbGetQuery(connection, paste("DESCRIBE SELECT * FROM", table))$column_name
  if (!all(c("code_state", geom) %in% columns)) stop("Unexpected GeoParquet state/geometry schema")
  geometry <- DBI::dbQuoteIdentifier(connection, geom)
  size <- DBI::dbGetQuery(connection, paste0("SELECT count(*) AS n, sum(octet_length(", geometry, ")) AS wkb_bytes FROM ", table, " WHERE code_state=12"))
  if (!size$n || !is.finite(size$wkb_bytes) || size$wkb_bytes > 128 * 1024^2) stop("AC geometry materialization exceeds pilot memory cap")
  rows <- DBI::dbGetQuery(connection, paste("SELECT * FROM", table, "WHERE code_state=12"))
  wkb <- structure(lapply(rows[[geom]], identity), class = "WKB")
  rows[[geom]] <- NULL
  result <- sf::st_sf(rows, geometry = sf::st_as_sfc(wkb, crs = source_crs))
  attr(result, "pilot_source") <- list(file = basename(path), source_crs = source_crs$wkt,
    selected_rows = nrow(result), selected_wkb_bytes = as.numeric(size$wkb_bytes), selection = "code_state=12 before collection", duckdb_memory_limit = "1GB")
  result
}

pilot2010_rank_income <- function(income) {
  x <- data.table::as.data.table(income)
  if (!all(c("code_tract", "code_muni", "code_state", "V001", "V007") %in% names(x))) stop("Unexpected Census2010 income schema")
  x <- data.table::copy(x)
  x[, `:=`(code_tract = pilot2010_code(code_tract), code_muni = pilot2010_code(code_muni), code_state = pilot2010_code(code_state))]
  if (anyNA(x$code_tract) || anyDuplicated(x$code_tract) || any(!grepl("^[0-9]{15}$", x$code_tract)) ||
      anyNA(x$code_muni) || anyNA(x$code_state) || any(substr(x$code_tract, 1, 7) != x$code_muni) ||
      any(substr(x$code_tract, 1, 2) != x$code_state)) stop("Census2010 sector/municipality/state identifiers differ")
  eligible <- is.finite(x$V001) & x$V001 > 0 & is.finite(x$V007) & x$V007 > 0
  if ("eligible_income" %in% names(x) && !identical(as.logical(x$eligible_income), eligible)) stop("Stored Census2010 income eligibility differs")
  x <- x[eligible]
  data.table::setorder(x, V007, code_tract)
  x[, centil_national := as.integer(floor((seq_len(.N) - 1L) * 100 / .N) + 1L)]
  x[, centil_state := as.integer(floor((seq_len(.N) - 1L) * 100 / .N) + 1L), by = code_state]
  x[, income_brl2010 := V007]
  attr(x, "rank_universe") <- nrow(x)
  x[]
}

pilot2010_link_ac <- function(income, tracts, municipalities, polling, max_distance_m = 5000) {
  pilot2010_require(); data.table::setDTthreads(2L)
  if (!identical(as.numeric(max_distance_m), 5000)) stop("This isolated pilot fixes the distance cutoff at 5000 meters")
  source_tracts <- nrow(tracts)
  previous_s2 <- sf::sf_use_s2(); on.exit(sf::sf_use_s2(previous_s2), add = TRUE); sf::sf_use_s2(TRUE)
  ranked <- pilot2010_rank_income(income); national_n <- nrow(ranked); ranked <- ranked[code_state == "12"]
  if (!nrow(ranked)) stop("No AC Census2010 eligible income sectors")
  if (is.na(sf::st_crs(tracts)) || is.na(sf::st_crs(municipalities)) || is.na(sf::st_crs(polling))) stop("Missing input geometry CRS")
  municipalities$code_muni <- pilot2010_code(municipalities$code_muni)
  tracts$code_tract <- pilot2010_code(tracts$code_tract)
  if (anyNA(municipalities$code_muni) || anyDuplicated(municipalities$code_muni) ||
      any(!grepl("^12[0-9]{5}$", municipalities$code_muni)) || anyNA(tracts$code_tract) ||
      anyDuplicated(tracts$code_tract) || any(!grepl("^12[0-9]{13}$", tracts$code_tract))) stop("AC2010 geometry identifiers invalid")
  if ("code_muni" %in% names(tracts) && any(pilot2010_code(tracts$code_muni) != substr(tracts$code_tract, 1L, 7L))) stop("Census2010 geometry sector/municipality disagreement")
  repair <- function(x) sf::st_transform(sf::st_make_valid(sf::st_transform(x, 5880)), 4326)
  invalid_municipalities <- sum(!sf::st_is_valid(municipalities)); invalid_tracts <- sum(!sf::st_is_valid(tracts))
  municipalities <- repair(municipalities); tracts <- repair(tracts)
  if (any(sf::st_is_empty(municipalities)) || any(!sf::st_is_valid(municipalities)) || any(!sf::st_is_valid(tracts))) stop("Invalid repaired Census2010 geometry")
  fields <- c("code_muni", "code_muni_tse", "nr_zona", "nr_local_votacao", "abbrev_state", "dt_eleicao")
  if (!all(fields %in% names(polling)) || any(sf::st_geometry_type(polling) != "POINT")) stop("Unexpected polling2010 schema/geometry")
  polling <- sf::st_transform(polling[polling$abbrev_state == "AC" & !is.na(polling$abbrev_state), ], 4326)
  rows <- data.table::as.data.table(sf::st_drop_geometry(polling)); rows[, source_row := seq_len(.N)]
  xy <- sf::st_coordinates(polling)
  if (nrow(xy) != nrow(rows)) stop("Polling coordinate/row mismatch")
  rows[, `:=`(source_code_muni = pilot2010_code(code_muni), longitude = xy[, 1], latitude = xy[, 2])]
  dates <- as.Date(as.character(rows$dt_eleicao), "%d/%m/%Y")
  missing <- is.na(dates); dates[missing] <- as.Date(as.character(rows$dt_eleicao[missing]), "%Y-%m-%d")
  available_dates <- sort(unique(dates[!is.na(dates) & format(dates, "%Y") == "2010"]))
  if (!length(available_dates)) stop("No polling date in election2010")
  reference <- available_dates[[1L]]
  rows[, reason := "candidate"]
  rows[is.na(dates) | dates != reference, reason := "not_2010_reference_date"]
  rows[reason == "candidate" & (!is.finite(longitude) | !is.finite(latitude) | abs(longitude) > 180 | abs(latitude) > 90), reason := "invalid_coordinate"]
  rows[reason == "candidate" & (is.na(source_code_muni) | !grepl("^12[0-9]{5}$", source_code_muni) | !source_code_muni %in% municipalities$code_muni), reason := "not_an_exact_IBGE2010_municipality"]
  rows[, `:=`(code_muni_tse = as.character(code_muni_tse), nr_zona = as.character(nr_zona), nr_local_votacao = as.character(nr_local_votacao))]
  rows[reason == "candidate" & (is.na(code_muni_tse) | is.na(nr_zona) | is.na(nr_local_votacao) |
      !grepl("^[0-9]+$", code_muni_tse) | !grepl("^[0-9]+$", nr_zona) | !grepl("^[0-9]+$", nr_local_votacao)), reason := "invalid_original_TSE_key"]
  mapping <- unique(rows[reason == "candidate", .(code_muni_tse, ibge2010 = source_code_muni)])
  if (!nrow(mapping) || anyDuplicated(mapping$code_muni_tse) || anyDuplicated(mapping$ibge2010)) stop("AC TSE-to-IBGE2010 municipality mapping is not bijective; no silent aliases allowed")
  rows[, place_key := paste("AC", code_muni_tse, nr_zona, nr_local_votacao, sep = ":")]
  distinct <- unique(rows[reason == "candidate", .(place_key, source_code_muni, longitude, latitude)])
  conflicts <- distinct[, .N, by = place_key][N > 1L, place_key]
  rows[reason == "candidate" & place_key %in% conflicts, reason := "conflicting_place_key_coordinates"]
  duplicates <- rows[reason == "candidate", .(source_rows = .N), by = place_key]
  duplicate_rows <- rows[reason == "candidate"][duplicated(place_key), source_row]
  rows[source_row %in% duplicate_rows, reason := "identical_duplicate_collapsed"]
  candidates <- rows[reason == "candidate"]
  points <- sf::st_as_sf(as.data.frame(candidates), coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  inside <- sf::st_within(points, municipalities)
  geometric <- vapply(seq_len(nrow(points)), function(i) {
    codes <- municipalities$code_muni[inside[[i]]]
    if (!length(codes)) "outside_declared_2010_municipality" else if (length(codes) != 1L) "ambiguous_2010_municipality" else if (codes != points$source_code_muni[[i]]) "different_2010_municipality" else "eligible"
  }, character(1))
  rows[match(points$source_row, rows$source_row), reason := geometric]
  points <- points[geometric == "eligible", ]; points <- points[order(points$place_key), ]
  if (!nrow(points)) stop("No geometrically validated polling2010 places")
  tracts <- tracts[tracts$code_tract %in% ranked$code_tract & !sf::st_is_empty(tracts), ]
  interiors <- sf::st_transform(sf::st_point_on_surface(sf::st_transform(tracts, 5880)), 4326)
  own_inside <- sf::st_within(interiors, tracts)
  muni_inside <- sf::st_within(interiors, municipalities)
  tract_reason <- vapply(seq_len(nrow(interiors)), function(i) {
    if (!i %in% own_inside[[i]]) return("interior_point_not_inside_2010_tract")
    code <- municipalities$code_muni[muni_inside[[i]]]
    if (length(code) != 1L || code != substr(interiors$code_tract[[i]], 1L, 7L)) "interior_point_not_in_declared_2010_municipality" else "eligible"
  }, character(1))
  sectors <- ranked[, .(code_tract, code_muni, V001, V007, income_brl2010, centil_national, centil_state)]
  sectors[, `:=`(place_key = NA_character_, distance_m = NA_real_, nearest_ties = 0L, reason = "missing_2010_tract_geometry")]
  for (i in seq_len(nrow(interiors))) sectors[code_tract == interiors$code_tract[[i]], reason := tract_reason[[i]]]
  for (municipality in unique(sectors$code_muni)) {
    take <- which(tract_reason == "eligible" & substr(interiors$code_tract, 1L, 7L) == municipality)
    places <- which(points$source_code_muni == municipality)
    if (!length(take)) next
    if (!length(places)) { sectors[code_tract %in% interiors$code_tract[take], reason := "no_validated_place_in_2010_municipality"]; next }
    distances <- as.matrix(sf::st_distance(interiors[take, ], points[places, ]))
    for (j in seq_along(take)) {
      meters <- as.numeric(distances[j, ]); nearest <- which(abs(meters - min(meters)) <= 1e-6)
      if (any(!is.finite(meters)) || any(meters < 0)) stop("Invalid S2 distance")
      code <- interiors$code_tract[[take[[j]]]]
      sectors[code_tract == code, `:=`(distance_m = min(meters), nearest_ties = length(nearest),
        place_key = if (length(nearest) == 1L) points$place_key[[places[[nearest]]]] else NA_character_,
        reason = if (length(nearest) > 1L) "ambiguous_nearest_tie" else if (min(meters) > max_distance_m) "beyond_5km" else "linked_within_5km")]
    }
  }
  sectors[, within_5km := !is.na(place_key) & is.finite(distance_m) & distance_m <= max_distance_m]
  list(links = sectors, places = points, tract_points = interiors, mapping = mapping, polling_audit = rows,
    duplicate_places = duplicates, metadata = list(status = "prepared_not_integrated", election_year = 2010L, census_year = 2010L,
      uf = "AC", polling_reference_round = 1L, polling_reference_date = as.character(reference),
      income_variable = "V007", head_count_variable = "V001", income_unit = "BRL2010 nominal; no minimum-wage conversion",
      rank_universe_national = national_n, rank_universe_state = nrow(ranked), ranking_before_spatial_exclusions = TRUE,
      max_distance_m = max_distance_m, geometry_vintage = 2010L, distance_engine = "sf/S2 spherical EPSG4326 meters",
      interior_method = "point_on_surface of repaired Census2010 polygons in EPSG5880; checked inside own sector and 2010 municipality",
      nearest_method = "minimum S2 distance within original declared IBGE2010 municipality after strict point-inside validation; ties remain unmatched",
      invalid_municipalities_repaired = invalid_municipalities, invalid_tracts_repaired = invalid_tracts,
      source_tract_geometries = source_tracts, eligible_tract_interior_points = nrow(interiors),
      polling_source_rows = nrow(rows), eligible_polling_places = nrow(points), mapped_municipalities = nrow(mapping),
      geometrically_validated_municipalities = length(unique(points$source_code_muni)),
      polling_exclusions = as.data.frame(rows[, .(rows = .N), by = reason]),
      sector_coverage = as.data.frame(sectors[, .(sectors = .N), by = reason]),
      linked_within_5km = sum(sectors$within_5km), conflicting_place_keys = length(conflicts),
      identical_duplicate_rows_collapsed = length(duplicate_rows)))
}

prepare_spatial_2010_pilot <- function(download = FALSE, destination = "data-raw/eleicoes-renda/censo2010/spatial-pilot") {
  pilot2010_require(); options(timeout = max(1200, getOption("timeout")))
  release_path <- file.path(destination, "geobr-release-v2.0.0.json")
  if (!file.exists(release_path)) stop("Save the official geobr release metadata before this bounded pilot: https://api.github.com/repos/ipea/geobr_prep_data/releases/tags/v2.0.0")
  release <- jsonlite::read_json(release_path, simplifyVector = FALSE)
  wanted <- c("censustracts_2010.parquet", "municipalities_2010.parquet")
  assets <- Filter(function(x) x$name %in% wanted, release$assets)
  if (length(assets) != 2L || !setequal(vapply(assets, function(x) x$name, character(1)), wanted)) stop("Official geobr geometry assets unavailable")
  geometry <- list(); provenance <- list()
  for (asset in assets) {
    expected_url <- paste0("https://github.com/ipea/geobr_prep_data/releases/download/v2.0.0/", asset$name)
    hash <- sub("^sha256:", "", asset$digest)
    if (!identical(asset$browser_download_url, expected_url) || !grepl("^[a-f0-9]{64}$", hash) || asset$size > 2 * 1024^3) stop("Unbounded/unverified geometry asset")
    file <- file.path(destination, "geometry-source", asset$name)
    if (!file.exists(file) && !download) stop("Geometry not cached; explicit download=TRUE required")
    pilot2010_download(expected_url, file, asset$size, hash)
    geometry[[asset$name]] <- pilot2010_read_ac(file)
    provenance[[asset$name]] <- c(list(url = expected_url, bytes = asset$size, sha256 = hash, md5 = pilot2010_hash(file)), attr(geometry[[asset$name]], "pilot_source"))
  }
  input_paths <- c(income = "data-raw/eleicoes-renda/censo2010/renda2010-basico.rds", polling = "data-raw/eleicoes-renda/espacial/polling-2010.rds")
  hashes <- unname(tools::md5sum(input_paths)); if (anyNA(hashes)) stop("Missing prepared income2010/polling2010 inputs")
  result <- pilot2010_link_ac(readRDS(input_paths[["income"]]), geometry[["censustracts_2010.parquet"]], geometry[["municipalities_2010.parquet"]], readRDS(input_paths[["polling"]]))
  if (!identical(hashes, unname(tools::md5sum(input_paths)))) stop("Pilot input changed during spatial calculation")
  result$metadata$input_md5 <- stats::setNames(as.list(hashes), names(input_paths))
  result$metadata$geography_sources <- provenance
  result$metadata$geobr_release_metadata <- list(url = "https://api.github.com/repos/ipea/geobr_prep_data/releases/tags/v2.0.0", file = basename(release_path), md5 = pilot2010_hash(release_path))
  result$metadata$polling_source <- "geobr::read_polling_places(year=2010), existing untouched polling-2010.rds"
  result$metadata$census_source <- "censobr::read_tracts(year=2010,dataset='Basico'), existing untouched income preparation"
  legal <- "data-raw/eleicoes-renda/historico/legacy-canonic/places-2010-AC-3_5_6_7_8-legal.rds"
  if (file.exists(legal)) result$metadata$legal_cache_reference <- list(file = basename(legal), md5 = pilot2010_hash(legal), role = "provenance only; candidates/votes not aggregated in this pilot")
  result$metadata$prepared_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  result$metadata$versions <- lapply(c("sf", "s2", "arrow", "duckdb", "geobr"), function(x) as.character(getNamespaceVersion(x)))
  names(result$metadata$versions) <- c("sf", "s2", "arrow", "duckdb", "geobr")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  attr(result$links, "metadata") <- result$metadata
  saveRDS(result$links, file.path(destination, "links-2010-AC-censo2010.rds"))
  saveRDS(result$places, file.path(destination, "polling-2010-AC-validated.rds")); saveRDS(result$tract_points, file.path(destination, "tract-points-2010-AC.rds"))
  data.table::fwrite(result$mapping, file.path(destination, "mapping-TSE-to-IBGE2010-AC.csv"))
  data.table::fwrite(result$polling_audit, file.path(destination, "polling-audit-2010-AC.csv"))
  data.table::fwrite(result$duplicate_places, file.path(destination, "duplicate-places-2010-AC.csv"))
  data.table::fwrite(result$links, file.path(destination, "tract-links-audit-2010-AC.csv"))
  jsonlite::write_json(result$metadata, file.path(destination, "metadata.json"), auto_unbox = TRUE, pretty = TRUE, dataframe = "rows", na = "null", null = "null")
  message("Prepared AC Census2010 spatial pilot: ", nrow(result$links), " income-eligible sectors; ", sum(result$links$within_5km), " uniquely linked within5km; NOT integrated.")
  invisible(result$metadata)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!identical(args, "--prepare-ac")) stop("Usage: Rscript prepare-spatial-2010-pilot.R --prepare-ac (isolated; bounded official downloads)")
  prepare_spatial_2010_pilot(download = TRUE)
}
