# Auxiliary methods for the original post. No downloads, implicit IO or CLI.
# Does not source or modify the explorer's historical aggregation pipeline.
post_table <- function(x, required) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
  x <- data.table::copy(data.table::as.data.table(x))
  if (!all(required %in% names(x))) stop("Missing post columns: ", paste(setdiff(required, names(x)), collapse = ", "))
  x
}

post_code <- function(x) if (is.numeric(x)) sprintf("%.0f", x) else as.character(x)

post_universe <- function(tracts, links, universe = c("linked", "income")) {
  universe <- match.arg(universe)
  x <- post_table(tracts, c("code_tract", "code_muni", "code_state", "V06001", "V06004"))
  keep <- intersect(c("code_tract", "code_muni", "code_state", "V06001", "V06004", "situacao", "code_situacao"), names(x))
  x <- x[, ..keep]
  x[, code_tract := post_code(code_tract)]
  if (anyNA(x$code_tract) || anyDuplicated(x$code_tract)) stop("Missing or duplicate census tract")
  x <- x[is.finite(V06001) & V06001 > 0 & is.finite(V06004) & V06004 > 0]
  if (any(x$V06001 %% 1 != 0)) stop("V06001 must contain counts")
  if (!"situacao" %in% names(x)) x[, situacao := "nao_disponivel"]
  x[is.na(situacao) | situacao == "", situacao := "nao_disponivel"]
  if (universe == "linked") x <- x[code_tract %in% links$code_tract]
  if (!nrow(x)) stop("Empty fixed income universe")
  data.table::setorder(x, V06004, code_tract)
  x[, centil := as.integer(floor((seq_len(.N) - 1) * 100 / .N) + 1)]
  # No splitting of a tract: a large tract may span several 1% intervals.
  x[, centil_ponderado := pmin(100L, as.integer(floor(100 * (cumsum(V06001) - V06001) / sum(V06001)) + 1))]
  x[, decil := as.integer((centil - 1L) %/% 10L + 1L)]
  x
}

post_links <- function(links, places) {
  l <- post_table(links, c("code_tract", "place_key", "distancia_m"))
  l <- l[, .(code_tract, place_key, distancia_m)]
  p <- post_table(places, c("place_key", "validos", "lula", "flavio"))
  l[, code_tract := post_code(code_tract)]
  if (anyDuplicated(p$place_key) || anyNA(p$place_key)) stop("Duplicate or missing place key")
  for (field in c("validos", "lula", "flavio")) {
    if (anyNA(p[[field]]) || any(!is.finite(p[[field]])) || any(p[[field]] < 0)) stop("Invalid place votes")
  }
  if (any(p$lula + p$flavio > p$validos)) stop("Candidate votes exceed denominator")
  l <- l[!is.na(place_key) & is.finite(distancia_m)]
  if (any(l$distancia_m < 0) || anyDuplicated(l[, .(code_tract, place_key)])) stop("Invalid or repeated tract/place edge")
  if (any(!l$place_key %in% p$place_key)) stop("Link references a place absent from election totals")
  l <- merge(l, p[, .(place_key, validos, lula, flavio)], by = "place_key", all.x = TRUE, sort = FALSE)
  l <- l[validos > 0]
  data.table::setorder(l, code_tract, distancia_m, place_key)
  l[, rank := seq_len(.N), by = code_tract]
  l[rank <= 3L]
}

post_idw <- function(links, limit = Inf, rule = c("nearest", "idw3")) {
  rule <- match.arg(rule)
  x <- data.table::copy(links[distancia_m <= limit & (rule != "nearest" | rank == 1L)])
  if (!nrow(x)) { x[, peso := numeric()]; return(x) }
  # Zero distance means exactly zero metres. Share equally among coincident
  # retained sites; nonzero sites receive zero mass in that case.
  x[, peso := if (any(distancia_m == 0)) as.numeric(distancia_m == 0) / sum(distancia_m == 0)
    else (1 / distancia_m) / sum(1 / distancia_m), by = code_tract]
  if (any(abs(x[, sum(peso), by = code_tract]$V1 - 1) > 1e-12)) stop("IDW weights do not sum to one")
  x
}

post_assigned <- function(links, limit, rule) {
  w <- post_idw(links, limit, rule)
  w[, .(p_lula = sum(peso * 100 * lula / validos),
         p_flavio = sum(peso * 100 * flavio / validos)), by = code_tract]
}

post_curve <- function(universe, assigned, weighted = FALSE, minimum_wage = 1212) {
  u <- data.table::copy(universe)
  u[, grupo := if (weighted) centil_ponderado else centil]
  full <- u[, .(n_setores_total = .N, peso_V06001_total = sum(V06001),
    renda_sm = sum(V06004 * if (weighted) V06001 else 1) / sum(if (weighted) V06001 else rep(1, .N)) / minimum_wage), by = .(centil = grupo)]
  observed <- merge(u, assigned, by = "code_tract", all = FALSE, sort = FALSE)
  counts <- observed[, .(n_setores = .N, peso_V06001 = sum(V06001),
    renda_sm_observada = sum(V06004 * if (weighted) V06001 else 1) / sum(if (weighted) V06001 else rep(1, .N)) / minimum_wage,
    lula = sum(p_lula * if (weighted) V06001 else 1) / sum(if (weighted) V06001 else rep(1, .N)),
    flavio = sum(p_flavio * if (weighted) V06001 else 1) / sum(if (weighted) V06001 else rep(1, .N))), by = .(centil = grupo)]
  ans <- merge(data.table::data.table(centil = 1:100), full, by = "centil", all.x = TRUE)
  ans <- merge(ans, counts, by = "centil", all.x = TRUE)
  for (name in c("n_setores", "n_setores_total", "peso_V06001", "peso_V06001_total"))
    data.table::set(ans, which(is.na(ans[[name]])), name, 0)
  ans
}

post_ratio <- function(universe, links, limit, minimum_wage = 1212) {
  e <- merge(universe, links[rank == 1L & distancia_m <= limit], by = "code_tract", all = FALSE, sort = FALSE)
  e[, allocation := V06001 / sum(V06001), by = place_key]
  e[, `:=`(validos_alocados = validos * allocation, lula_alocados = lula * allocation,
            flavio_alocados = flavio * allocation)]
  local <- if (nrow(e)) e[, .(massa = sum(allocation), validos_originais = validos[[1L]],
    lula_originais = lula[[1L]], flavio_originais = flavio[[1L]],
    validos_alocados = sum(validos_alocados), lula_alocados = sum(lula_alocados),
    flavio_alocados = sum(flavio_alocados)), by = place_key] else data.table::data.table(
      place_key = character(), massa = numeric(), validos_originais = numeric(), lula_originais = numeric(),
      flavio_originais = numeric(), validos_alocados = numeric(), lula_alocados = numeric(), flavio_alocados = numeric())
  for (name in c("validos", "lula", "flavio")) {
    if (any(abs(local[[paste0(name, "_originais")]] - local[[paste0(name, "_alocados")]]) >
      1e-9 * pmax(1, local[[paste0(name, "_originais")]]))) stop("Local vote conservation failed")
  }
  allocated <- e[, .(validos_alocados = sum(validos_alocados), lula_alocados = sum(lula_alocados),
    flavio_alocados = sum(flavio_alocados)), by = centil]
  baseline <- post_curve(universe, post_assigned(links, limit, "nearest"), FALSE, minimum_wage)
  baseline[, c("lula", "flavio") := NULL]
  baseline <- merge(baseline, allocated, by = "centil", all.x = TRUE)
  baseline[, `:=`(lula = 100 * lula_alocados / validos_alocados, flavio = 100 * flavio_alocados / validos_alocados)]
  baseline[!is.finite(lula), lula := NA_real_]; baseline[!is.finite(flavio), flavio := NA_real_]
  conservation <- data.table::data.table(locais_usados = nrow(local),
    erro_maximo_massa_local = if (nrow(local)) max(abs(local$massa - 1)) else 0)
  for (name in c("validos", "lula", "flavio")) for (kind in c("originais", "alocados"))
    conservation[, (paste0(name, "_", kind)) := sum(local[[paste0(name, "_", kind)]])]
  list(curve = baseline, conservation = conservation)
}

post_crossing <- function(curve) {
  data.table::setorder(curve, centil)
  over <- is.finite(curve$lula) & is.finite(curve$flavio) & curve$flavio > curve$lula
  sustained <- vapply(seq_len(nrow(curve)), function(i) all(over[i:nrow(curve)]), logical(1))
  data.table::data.table(primeiro_centil = if (any(over)) curve$centil[which(over)[1L]] else NA_integer_,
    primeiro_centil_sustentado = if (any(sustained)) curve$centil[which(sustained)[1L]] else NA_integer_,
    n_intervalos_superacao = sum(over & !c(FALSE, head(over, -1))))
}

post_income_diagnostic <- function(universe) {
  # ResponsavelRenda V06001..V06006 contains no count of heads WITH income.
  ans <- universe[, .(n_setores_total = .N, peso_V06001_total = sum(V06001)), by = centil]
  ans[, `:=`(fracao_sem_rendimento = NA_real_,
    status = "indisponivel_sem_contagem_de_responsaveis_com_rendimento")]
  ans
}

post_methods <- function(tracts, links, places, limits = c(1000, 2000, 5000, 10000, Inf),
                         universe = c("linked", "income"), minimum_wage = 1212) {
  universe <- match.arg(universe)
  if (anyNA(limits) || any(limits <= 0) || !length(limits)) stop("Invalid sensitivity distance limits")
  if (length(minimum_wage) != 1L || !is.finite(minimum_wage) || minimum_wage <= 0) stop("Invalid income unit")
  limits <- sort(unique(limits))
  l <- post_links(links, places)
  u <- post_universe(tracts, l, universe)
  # Exclusion table always includes ALL income-eligible tracts, including no-link.
  all_income <- post_universe(tracts, l, "income")
  nearest <- l[rank == 1L, .(code_tract, distancia_m)]
  exclusions <- merge(all_income, nearest, by = "code_tract", all.x = TRUE)
  fixed <- weighted <- ratio <- conservation <- alternative <- excluded <- crossings <- original <- list()
  for (i in seq_along(limits)) {
    limit <- limits[[i]]
    assigned <- post_assigned(l, limit, "nearest")
    observed_universe <- u[code_tract %in% assigned$code_tract]
    if (nrow(observed_universe)) {
      reranked <- post_universe(observed_universe, l, "income")
      original[[i]] <- post_curve(reranked, assigned, FALSE, minimum_wage)
    } else original[[i]] <- post_curve(u[0], assigned, FALSE, minimum_wage)
    fixed[[i]] <- post_curve(u, assigned, FALSE, minimum_wage)
    weighted[[i]] <- post_curve(u, assigned, TRUE, minimum_wage)
    r <- post_ratio(u, l, limit, minimum_wage)
    ratio[[i]] <- r$curve; conservation[[i]] <- r$conservation
    alternative[[i]] <- post_curve(u, post_assigned(l, limit, "idw3"), FALSE, minimum_wage)
    for (name in c("fixed", "weighted", "ratio", "conservation", "alternative", "original")) {
      value <- get(name); value[[i]][, distancia_maxima_m := limit]; assign(name, value)
    }
    exclusions[, motivo := ifelse(!is.finite(distancia_m), "sem_local_ou_malha",
      ifelse(distancia_m > limit, "alem_distancia_maxima", "incluido"))]
    excluded[[i]] <- exclusions[, .(n_setores = .N, peso_V06001 = sum(V06001)),
      by = .(code_state, decil, situacao, motivo)]
    excluded[[i]][, distancia_maxima_m := limit]
    c1 <- post_crossing(fixed[[i]]); c1[, `:=`(distancia_maxima_m = limit, regra = "nearest")]
    c3 <- post_crossing(alternative[[i]]); c3[, `:=`(distancia_maxima_m = limit, regra = "idw3")]
    crossings[[i]] <- data.table::rbindlist(list(c1, c3))
  }
  stack <- function(x) data.table::rbindlist(x, use.names = TRUE, fill = TRUE)
  fixed <- stack(fixed); weighted <- stack(weighted); ratio <- stack(ratio); original <- stack(original)
  idw <- stack(alternative)
  linkage <- data.table::rbindlist(list(data.table::copy(fixed)[, regra := "nearest"],
    data.table::copy(idw)[, regra := "idw3"]))
  comparisons <- list(); pos <- 0L
  for (reference in c("original_reranked", "nearest_fixed")) {
    ref <- if (reference == "original_reranked") original else fixed
    for (method in c("nearest_fixed", "weighted", "ratio", "idw3")) {
      if (method == reference) next
      other <- switch(method, nearest_fixed = fixed, weighted = weighted, ratio = ratio, idw3 = idw)
      pair <- merge(ref[, .(centil, distancia_maxima_m, lula, flavio)],
        other[, .(centil, distancia_maxima_m, lula, flavio)], by = c("centil", "distancia_maxima_m"), suffixes = c("_ref", "_other"))
      for (candidate in c("lula", "flavio")) {
        pair[, difference := abs(get(paste0(candidate, "_ref")) - get(paste0(candidate, "_other")))]
        pos <- pos + 1L
        comparisons[[pos]] <- pair[, .(n_centis_comparados = sum(is.finite(difference)),
          diferenca_maxima_pp = if (any(is.finite(difference))) max(difference[is.finite(difference)]) else NA_real_), by = distancia_maxima_m]
        comparisons[[pos]][, `:=`(metodo_referencia = reference, metodo_comparado = method, candidato = candidate)]
      }
    }
  }
  list(universo_centis = u, links = l[, .(code_tract, place_key, distancia_m, rank)],
    centis_originais_comparacao = original, confronto_metodos = stack(comparisons),
    sensibilidade_cortes_fixos = fixed, centis_ponderados = weighted, centis_razao = ratio,
    conservacao_votos = stack(conservation), sensibilidade_ligacao = linkage,
    exclusoes_distancia = stack(excluded), cruzamentos = stack(crossings),
    diagnostico_renda = post_income_diagnostic(u),
    metadata = list(universe = universe, percentile_scope = "BR", weight = "V06001: pessoas responsaveis em domicilios particulares permanentes ocupados",
      weighted_cut = "floor(100*cumulative preceding V06001/total)+1; no tract splitting",
      ratio_allocation = "nearest linked included tracts; V06001 share within each place and distance scenario",
      idw = "three nearest places; within-limit renormalization; exact zeros share weight and exclude positive distances"))
}

post_write_methods <- function(tracts, links, places, destination,
                               limits = c(1000, 2000, 5000, 10000, Inf),
                               universe = c("linked", "income"), minimum_wage = 1212) {
  universe <- match.arg(universe)
  result <- post_methods(tracts, links, places, limits, universe, minimum_wage)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  filenames <- c(universo_centis = "universo-centis.csv", links = "links-metodos.csv",
    sensibilidade_cortes_fixos = "sensibilidade-cortes-fixos.csv", centis_ponderados = "centis-ponderados.csv",
    centis_razao = "centis-razao.csv", conservacao_votos = "conservacao-votos.csv",
    sensibilidade_ligacao = "sensibilidade-ligacao.csv", exclusoes_distancia = "exclusoes-distancia.csv",
    cruzamentos = "cruzamentos-cortes-fixos.csv", diagnostico_renda = "diagnostico-renda.csv",
    centis_originais_comparacao = "centis-originais-comparacao.csv", confronto_metodos = "confronto-metodos.csv")
  for (name in names(filenames)) data.table::fwrite(result[[name]], file.path(destination, filenames[[name]]), na = "NA")
  p <- post_table(places, c("place_key", "validos", "lula", "flavio"))
  data.table::fwrite(p[, .(place_key, validos, lula, flavio)], file.path(destination, "locais-metodos.csv"))
  result$metadata$schema_version <- 1L
  actual_limits <- sort(unique(limits))
  result$metadata$limits_m <- ifelse(is.infinite(actual_limits), "Inf", as.character(actual_limits))
  result$metadata$minimum_wage_2022_brl <- minimum_wage
  result$metadata$income_dictionary <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel/dicionario_de_dados_renda_responsavel_20260508.xlsx"
  result$metadata$income_dictionary_md5 <- "634dd1e839200c1724d6caca75626c69"
  result$metadata$income_count_status <- "unavailable: ResponsavelRenda has no count of responsible people WITH income; V06002 counts residents"
  result$metadata$situation_source <- if ("situacao" %in% names(tracts)) "Census 2022 ResponsavelRenda situacao attribute supplied by censobr" else "unavailable"
  jsonlite::write_json(result$metadata, file.path(destination, "metodos-metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(result)
}

# Exact spherical index, with a bounded brute-force alternative for validation.
# S2 does not promise key ordering at the third-neighbour tie boundary. Search
# one extra neighbour and expand only unresolved boundaries, then sort by key.
post_k_nearest <- function(points, places_sf, k = 3L, chunk_size = 64L,
                           algorithm = c("s2", "bruteforce")) {
  algorithm <- match.arg(algorithm)
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install sf")
  if (!requireNamespace("s2", quietly = TRUE)) stop("Install s2")
  if (k != 3L || chunk_size < 1L) stop("Expected k=3 and positive chunk size")
  if (!all(c("code_tract", "code_muni") %in% names(points)) ||
      !all(c("place_key", "code_muni") %in% names(places_sf))) stop("Missing spatial keys")
  if (anyDuplicated(points$code_tract) || anyDuplicated(places_sf$place_key)) stop("Duplicate spatial key")
  previous_s2 <- sf::sf_use_s2(); on.exit(suppressMessages(sf::sf_use_s2(previous_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  points <- sf::st_transform(points, 4326); places_sf <- sf::st_transform(places_sf, 4326)
  rows <- list(); pos <- 0L
  for (muni in unique(as.character(points$code_muni))) {
    a <- points[as.character(points$code_muni) == muni, ]
    b <- places_sf[as.character(places_sf$code_muni) == muni, ]
    if (!nrow(b)) next
    b <- b[order(b$place_key), ]
    if (algorithm == "s2") {
      a_geo <- sf::st_as_s2(a); b_geo <- sf::st_as_s2(b)
      unresolved <- seq_len(nrow(a)); request_k <- min(k + 1L, nrow(b))
      chosen_all <- vector("list", nrow(a)); distances_all <- vector("list", nrow(a))
      while (length(unresolved)) {
        found <- s2::s2_closest_edges(a_geo[unresolved], b_geo, k = request_k)
        lengths_found <- lengths(found)
        if (any(lengths_found != request_k)) stop("Incomplete S2 neighbour search")
        # Vectorised paired distances, not an all-pairs matrix.
        distances <- as.numeric(s2::s2_distance(a_geo[rep(unresolved, lengths_found)], b_geo[unlist(found)]))
        start <- cumsum(c(1L, head(lengths_found, -1L)))
        next_unresolved <- integer()
        for (j in seq_along(unresolved)) {
          candidate <- found[[j]]
          d <- distances[start[[j]] + seq_len(lengths_found[[j]]) - 1L]
          ord <- order(d, b$place_key[candidate]); candidate <- candidate[ord]; d <- d[ord]
          desired <- min(k, length(candidate))
          # A tiny expansion guard only decides whether to request MORE exact
          # candidates; it does not round distances or alter their final order.
          guard <- max(1e-7, d[[desired]] * 1e-12)
          boundary_open <- request_k < nrow(b) && d[[length(d)]] - d[[desired]] <= guard
          if (boundary_open) next_unresolved <- c(next_unresolved, unresolved[[j]])
          else {
            chosen_all[[unresolved[[j]]]] <- head(candidate, k)
            distances_all[[unresolved[[j]]]] <- head(d, k)
          }
        }
        unresolved <- next_unresolved
        request_k <- min(nrow(b), request_k * 2L)
      }
      for (j in seq_len(nrow(a))) {
        chosen <- chosen_all[[j]]; pos <- pos + 1L
        rows[[pos]] <- data.table::data.table(code_tract = post_code(a$code_tract[[j]]),
          place_key = as.character(b$place_key[chosen]), distancia_m = distances_all[[j]], rank = seq_along(chosen))
      }
      next
    }
    for (start in seq.int(1L, nrow(a), by = chunk_size)) {
      indices <- start:min(nrow(a), start + chunk_size - 1L)
      distance <- as.matrix(sf::st_distance(a[indices, ], b))
      for (j in seq_along(indices)) {
        chosen <- head(order(as.numeric(distance[j, ]), b$place_key), k)
        pos <- pos + 1L
        rows[[pos]] <- data.table::data.table(code_tract = post_code(a$code_tract[indices[[j]]]),
          place_key = as.character(b$place_key[chosen]), distancia_m = as.numeric(distance[j, chosen]), rank = seq_along(chosen))
      }
    }
  }
  if (length(rows)) data.table::rbindlist(rows) else data.table::data.table(code_tract = character(), place_key = character(), distancia_m = numeric(), rank = integer())
}
