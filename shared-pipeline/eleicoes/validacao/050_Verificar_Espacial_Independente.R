# Independent geometric audit: original polygons/register, never cached tract points.
# Spherical reference uses base-R haversine, not sf/S2's distance/index routine.
# EPSG5880 remains a diagnostic: its national scale distortion exceeded 1%.
verificacao_haversine <- function(point, sites, radius=6371010) {
  radians <- pi/180
  dlat <- (sites[,2]-point[2])*radians
  dlon <- (sites[,1]-point[1])*radians
  a <- sin(dlat/2)^2+cos(point[2]*radians)*cos(sites[,2]*radians)*sin(dlon/2)^2
  2*radius*atan2(sqrt(pmax(0,pmin(1,a))),sqrt(pmax(0,1-a)))
}
verificacao_amostra_uf <- function(tracts, size = 2000L, seed = 20261016L) {
  x <- as.data.frame(tracts, stringsAsFactors = FALSE)
  x$code_tract <- if (is.numeric(x$code_tract)) sprintf("%.0f", x$code_tract) else as.character(x$code_tract)
  x <- x[order(x$code_tract), , drop = FALSE]
  x$uf <- substr(x$code_tract, 1L, 2L)
  if (anyDuplicated(x$code_tract) || nrow(x) < size) stop("Insufficient/distinct tracts for spatial sample")
  strata <- split(seq_len(nrow(x)), x$uf)
  if (size < length(strata)) stop("Spatial sample must include every UF")
  quota <- setNames(integer(length(strata)), names(strata))
  for (i in seq_len(size)) {
    eligible <- which(quota < lengths(strata))
    next_uf <- eligible[which.min(quota[eligible])]
    quota[next_uf] <- quota[next_uf] + 1L
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  take <- unlist(lapply(names(strata), function(uf) {
    ix <- strata[[uf]]
    ix[sample.int(length(ix), quota[[uf]], replace = FALSE)]
  }), use.names = FALSE)
  x[take, , drop = FALSE]
}

verificar_espacial_independente <- function(tracts, place_votes,
    dest = "data-raw/eleicoes-renda", size = 2000L, seed = 20261016L,
    tie_tolerance_m = 1e-6, cache_tolerance_m = 0.05, projection_relative_tolerance = 0.01,
    knn_links = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install sf")
  tracts <- as.data.frame(tracts, stringsAsFactors = FALSE)
  place_votes <- as.data.frame(place_votes, stringsAsFactors = FALSE)
  required <- c("code_tract", "code_muni", "place_key", "distancia_m", "motivo")
  if (!all(required %in% names(tracts)) || !all(c("place_key", "validos") %in% names(place_votes)))
    stop("Missing original spatial-audit inputs")
  observed <- as.data.frame(tracts[as.character(tracts$motivo) == "incluido", required, drop = FALSE])
  if (!nrow(observed) || anyNA(observed$place_key) || any(!is.finite(observed$distancia_m)))
    stop("Invalid included spatial links")
  sample <- verificacao_amostra_uf(observed, size, seed)
  if (!is.null(knn_links)) {
    knn_links <- as.data.frame(knn_links, stringsAsFactors = FALSE)
    knn_links$code_tract <- if(is.numeric(knn_links$code_tract))sprintf("%.0f",knn_links$code_tract) else as.character(knn_links$code_tract)
  }
  poll_path <- file.path(dest, "polling2026.rds")
  municipality_path <- file.path(dest, "municipios2022.rds")
  input_paths <- c(poll_path, municipality_path)
  polling <- sf::st_transform(readRDS(poll_path), 4326)
  need <- c("abbrev_state", "code_muni_tse", "nr_zona", "nr_local_votacao", "code_muni", "dt_eleicao")
  if (!all(need %in% names(polling))) stop("Original polling-register schema changed")
  date <- as.character(polling$dt_eleicao)
  polling <- polling[date %in% c("2026-10-04", "04/10/2026"), ]
  if (!nrow(polling)) stop("No first-round 2026 polling coordinates")
  xy <- sf::st_coordinates(polling)
  p <- as.data.frame(sf::st_drop_geometry(polling))
  p$longitude <- xy[, 1L]; p$latitude <- xy[, 2L]
  p$place_key <- with(p, paste(abbrev_state, code_muni_tse, nr_zona, nr_local_votacao, sep = ":"))
  p$code_muni <- as.character(p$code_muni)
  # Collapse only identical locations, rejecting conflicts independently of 020.
  location_rows <- unique(p[, c("place_key", "code_muni", "longitude", "latitude")])
  conflict <- names(which(table(location_rows$place_key) > 1L))
  p <- p[!p$place_key %in% conflict & !duplicated(p$place_key), , drop = FALSE]
  raw_keys <- unique(as.character(place_votes$place_key[is.finite(place_votes$validos) & place_votes$validos > 0]))
  keep <- p$place_key %in% raw_keys & is.finite(p$longitude) & is.finite(p$latitude) &
    p$longitude >= -74 & p$longitude <= -28 & p$latitude >= -34 & p$latitude <= 6
  p <- p[keep, c("place_key", "code_muni", "longitude", "latitude"), drop = FALSE]
  polling <- sf::st_as_sf(p, coords = c("longitude", "latitude"), crs = 4326)
  municipalities <- sf::st_make_valid(sf::st_transform(readRDS(municipality_path), 5880))
  polling_projected <- sf::st_transform(polling, 5880)
  overlap <- sf::st_intersects(polling_projected, municipalities)
  contained <- vapply(seq_len(nrow(polling)), function(i) {
    polling$code_muni[i] %in% as.character(municipalities$code_muni[overlap[[i]]])
  }, logical(1))
  coordinate_keys <- as.character(polling$place_key)
  outside <- as.data.frame(sf::st_drop_geometry(polling[!contained, c("place_key", "code_muni")]))
  polling <- polling[contained, ]; polling_projected <- polling_projected[contained, ]
  ord <- order(polling$place_key)
  polling <- polling[ord, ]; polling_projected <- polling_projected[ord, ]
  rm(municipalities, overlap, p, xy, polling_projected)
  invisible(gc())
  membership <- data.frame(included_tracts = nrow(observed),
    missing_raw_place = sum(!observed$place_key %in% raw_keys),
    missing_eligible_place = sum(!observed$place_key %in% polling$place_key),
    independently_eligible_places = nrow(polling), seed = seed, sample_size = size)
  write.csv(membership, file.path(dest, "verificacao-espacial-chaves.csv"), row.names = FALSE)
  saveRDS(list(coordinate_keys=coordinate_keys,eligible_keys=as.character(polling$place_key),outside=outside),
    file.path(dest,"verificacao-espacial-locais.rds"))
  old_s2 <- sf::sf_use_s2(); on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(TRUE)
  records <- list()
  for (uf in sort(unique(sample$uf))) {
    message("Independent spatial audit UF ",uf," (",sum(sample$uf==uf)," tracts)")
    shape_path <- file.path(dest, "malhas", paste0("setores2022-", uf, ".rds"))
    input_paths <- c(input_paths, shape_path)
    polygons <- readRDS(shape_path)
    codes <- if (is.numeric(polygons$code_tract)) sprintf("%.0f", polygons$code_tract) else as.character(polygons$code_tract)
    part <- sample[sample$uf == uf, , drop = FALSE]
    ix <- match(part$code_tract, codes)
    if (anyNA(ix) || anyDuplicated(codes[ix])) stop("Sample tract missing/duplicated in original polygons: ", uf)
    polygons <- polygons[ix, ]
    if (any(as.character(polygons$code_muni) != as.character(part$code_muni))) stop("Original tract municipality mismatch")
    # Recreate interior points from the original polygons, not pontos2022 caches.
    points_projected <- suppressWarnings(sf::st_point_on_surface(sf::st_make_valid(sf::st_transform(polygons, 5880))))
    points_geographic <- sf::st_transform(points_projected, 4326)
    for (muni in unique(as.character(part$code_muni))) {
      local_rows <- which(as.character(part$code_muni) == muni)
      sites <- polling[as.character(polling$code_muni) == muni, ]
      if (!nrow(sites)) stop("Included sample has no independently eligible municipal place")
      site_xy <- sf::st_coordinates(sf::st_transform(sites, 5880))
      point_xy <- sf::st_coordinates(points_projected[local_rows, ])
      # Reconstruct the spherical metric independently using elementary trig.
      site_degrees <- sf::st_coordinates(sites)
      point_degrees <- sf::st_coordinates(points_geographic[local_rows,])
      geographic <- t(matrix(vapply(seq_len(nrow(point_degrees)),function(i)
        verificacao_haversine(point_degrees[i,],site_degrees),numeric(nrow(sites))),
        nrow=nrow(sites),ncol=nrow(point_degrees)))
      for (j in seq_along(local_rows)) {
        i <- local_rows[j]
        euclidean <- sqrt((site_xy[, 1L] - point_xy[j, 1L])^2 + (site_xy[, 2L] - point_xy[j, 2L])^2)
        nearest <- which.min(euclidean)
        ties <- which(abs(euclidean - euclidean[nearest]) <= tie_tolerance_m)
        stored <- match(part$place_key[i], sites$place_key)
        if (is.na(stored)) stop("Cached included place outside independently reconstructed municipality")
        geod <- as.numeric(geographic[j, stored])
        projected <- euclidean[stored]
        relative <- if (max(geod, projected) == 0) 0 else abs(projected - geod) / max(geod, 1e-9)
        same_or_tied <- stored %in% ties
        geodesic_nearest <- abs(geod - min(as.numeric(geographic[j, ]))) <= tie_tolerance_m
        cache_error <- abs(as.numeric(part$distancia_m[i]) - geod)
        knn_pass <- TRUE;knn_expected <- knn_saved <- NA_character_
        if(!is.null(knn_links)) {
          expected_top <- head(order(as.numeric(geographic[j, ]),as.character(sites$place_key)),3L)
          saved_top <- knn_links[knn_links$code_tract==part$code_tract[i],,drop=FALSE]
          saved_top <- saved_top[order(saved_top$rank),,drop=FALSE]
          knn_expected <- paste(sites$place_key[expected_top],collapse="|")
          knn_saved <- paste(saved_top$place_key,collapse="|")
          knn_pass <- identical(knn_saved,knn_expected) && nrow(saved_top)==length(expected_top) &&
            all(abs(saved_top$distancia_m-as.numeric(geographic[j,expected_top]))<=cache_tolerance_m)
        }
        records[[length(records) + 1L]] <- data.frame(code_tract = part$code_tract[i], uf = uf,
          code_muni = muni, place_key_saved = part$place_key[i], place_key_bruteforce = sites$place_key[nearest],
          municipal_places = nrow(sites), projected_tie_count = length(ties),
          tie_keys = if (length(ties) > 1L) paste(sites$place_key[ties], collapse = "|") else "",
          same_or_documented_tie = same_or_tied, geodesic_nearest = geodesic_nearest,
          distance_saved_m = part$distancia_m[i], distance_geodesic_m = geod,
          distance_projected_m = projected, projection_relative_error = relative,
          projected_diagnostic_pass=same_or_tied && relative<projection_relative_tolerance,
          reference_method="base_R_haversine_radius_6371010_m",
          saved_geodesic_error_m = cache_error, seed = seed,
          knn3_expected_keys=knn_expected,knn3_saved_keys=knn_saved,knn3_pass=knn_pass,
          pass = geodesic_nearest && cache_error <= cache_tolerance_m && knn_pass)
      }
    }
    rm(polygons, points_projected, points_geographic)
    invisible(gc())
  }
  result <- do.call(rbind, records)
  write.csv(result, file.path(dest, "verificacao-espacial.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(file = input_paths, md5 = unname(tools::md5sum(input_paths))),
    file.path(dest, "verificacao-espacial-insumos.csv"), row.names = FALSE)
  if (nrow(result) != size || membership$missing_raw_place || membership$missing_eligible_place || any(!result$pass))
    stop("Independent spatial audit failed; inspect verificacao-espacial.csv and verificacao-espacial-chaves.csv")
  message("PASS spatial: ", size, " fixed UF-stratified tracts; independent base-R haversine, k3 and global place membership. EPSG5880 disagreement retained as diagnostic.")
  invisible(result)
}

if (sys.nframe() == 0L) {
  dest <- "data-raw/eleicoes-renda"
  verificar_espacial_independente(as.data.frame(readRDS(file.path(dest, "setores-votos.rds"))),
    as.data.frame(readRDS(file.path(dest, "votos-locais-verificacao.rds"))), dest = dest)
}
