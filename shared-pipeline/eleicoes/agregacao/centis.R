# Fixed Census-2022 income ranks and memory-bounded electoral aggregation.
# Source from the repository root. No downloads or side effects on source().
source("shared-pipeline/eleicoes/agregacao/censo-referencia.R", encoding="UTF-8")
eleicao_require <- function() {
  for (pkg in c("data.table", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install package: ", pkg)
  }
}

eleicao_ufs <- c(`11` = "RO", `12` = "AC", `13` = "AM", `14` = "RR", `15` = "PA", `16` = "AP", `17` = "TO",
  `21` = "MA", `22` = "PI", `23` = "CE", `24` = "RN", `25` = "PB", `26` = "PE", `27` = "AL", `28` = "SE", `29` = "BA",
  `31` = "MG", `32` = "ES", `33` = "RJ", `35` = "SP", `41` = "PR", `42` = "SC", `43` = "RS",
  `50` = "MS", `51` = "MT", `52` = "GO", `53` = "DF")
eleicao_office_labels <- c(`1` = "Presidente", `3` = "Governador", `5` = "Senador",
  `6` = "Deputado federal", `7` = "Deputado estadual", `8` = "Deputado distrital")

eleicao_tract_code <- function(x) {
  if (is.numeric(x)) sprintf("%.0f", x) else as.character(x)
}

eleicao_income_universe <- function(renda) {
  eleicao_require()
  x <- data.table::as.data.table(renda)
  if (!all(c("code_tract", "V06001", "V06004") %in% names(x))) stop("Missing census income columns")
  x <- x[, .(code_tract, V06001, V06004)]
  x[, code_tract := eleicao_tract_code(code_tract)]
  if (anyNA(x$code_tract) || anyDuplicated(x$code_tract)) stop("Missing/duplicate census tract")
  x <- x[!is.na(V06001) & V06001 > 0 & is.finite(V06004) & V06004 > 0,
         .(code_tract, income = V06004)]
  if (nrow(x) < 100L) stop("At least 100 eligible tracts required")
  x[, uf := unname(eleicao_ufs[substr(code_tract, 1L, 2L)])]
  if (anyNA(x$uf)) stop("Unknown IBGE state code in census tracts")
  data.table::setorder(x, income, code_tract)
  x[, centil := as.integer(floor((seq_len(.N) - 1L) * 100 / .N) + 1L)]
  x[]
}

eleicao_reference_universe <- function(renda, census_year = 2022L) {
  if (as.integer(census_year) == 2022L) return(eleicao_income_universe(renda))
  x <- eleicao_censo_universe(renda, census_year)[, .(code_tract, income)]
  if (nrow(x) < 100L) stop("At least 100 eligible tracts required")
  x[, uf := unname(eleicao_ufs[substr(code_tract, 1L, 2L)])]
  if (anyNA(x$uf)) stop("Unknown IBGE state code in census tracts")
  data.table::setorder(x, income, code_tract)
  x[, centil := as.integer(floor((seq_len(.N) - 1L) * 100 / .N) + 1L)]
  x[]
}

eleicao_vote_audit <- function(votes, year, round, office, uf = "BR") {
  audit <- attr(votes, "vote_audit")
  if (is.null(audit) || !identical(audit$status, "reconciled")) stop("Sem auditoria oficial reconciliada para esta disputa")
  checks <- data.table::as.data.table(audit$checks)
  required <- c("year", "round", "office", "uf", "official_valid_votes", "computed_valid_votes", "difference")
  if (!all(required %in% names(checks))) stop("Auditoria oficial incompleta")
  yr <- as.integer(year); tr <- as.integer(round); oc <- as.integer(office); state <- toupper(uf)
  checks <- checks[year == yr & round == tr & office == oc]
  expected <- unique(as.character(votes$uf[votes$year == yr & votes$round == tr & votes$office == oc & votes$uf != "ZZ"]))
  if (state == "BR") expected <- unname(eleicao_ufs)
  else { checks <- checks[uf == state]; expected <- intersect(expected, state) }
  # The spatial explorer covers domestic tracts only. Historical audits may
  # also certify overseas votes (ZZ); do not add those to the displayed scope.
  checks <- checks[uf %in% expected]
  if (!length(expected) || !nrow(checks) || !all(expected %in% checks$uf)) stop("Auditoria não cobre todas as UFs desta disputa")
  if (anyNA(checks[, ..required]) || any(!is.finite(checks$difference)) ||
      any(abs(checks$difference) > 1e-8) ||
      any(abs(checks$computed_valid_votes - checks$official_valid_votes) > 1e-8)) {
    stop("Auditoria oficial apresenta divergência de votos válidos")
  }
  selected <- votes$year == yr & votes$round == tr & votes$office == oc &
    votes$uf %in% expected & votes$vote_type %in% c("candidate", "party")
  for (state_checked in expected) {
    actual <- sum(votes$votes[selected & votes$uf == state_checked])
    certified <- sum(checks$computed_valid_votes[checks$uf == state_checked])
    if (!is.finite(actual) || abs(actual - certified) > 1e-8) stop("Votos do cache divergem da auditoria oficial anexada")
  }
  # Combined caches can contain EA20 provisional snapshots for deputies and
  # final/ordinary checks for other offices. Carry only this contest's evidence.
  snapshots <- audit$snapshots
  if (!is.null(snapshots)) {
    snapshots <- Filter(function(snapshot) {
      is.list(snapshot) &&
        (is.null(snapshot$office) || identical(as.integer(snapshot$office), oc)) &&
        (is.null(snapshot$year) || identical(as.integer(snapshot$year), yr)) &&
        (is.null(snapshot$round) || identical(as.integer(snapshot$round), tr)) &&
        (is.null(snapshot$uf) || state == "BR" || toupper(snapshot$uf) == state)
    }, snapshots)
    provisional <- length(snapshots) > 0L && any(vapply(snapshots, function(snapshot) {
      isTRUE(snapshot$provisional) || identical(snapshot$status, "provisional") ||
        (!is.null(snapshot$final_totalization) && !identical(snapshot$final_totalization, "s")) ||
        (is.null(snapshot$status) && is.null(snapshot$final_totalization) && isTRUE(audit$provisional))
    }, logical(1)))
  } else provisional <- isTRUE(audit$provisional)
  result <- list(status = "reconciled", scope = "official-valid-votes", method = audit$method,
       source = list(label = "TSE — resultados oficiais", url = "https://dadosabertos.tse.jus.br/"),
       official_valid_votes = sum(checks$official_valid_votes),
       computed_valid_votes = sum(checks$computed_valid_votes),
       difference = sum(checks$difference), checked_units = nrow(checks), ufs = sort(unique(checks$uf)),
       provisional = provisional, snapshots = snapshots)
  # Explicit historical destinations can be needed to reconstruct a missing
  # nominal record. Keep their document evidence only for the displayed race.
  destination_sources <- audit$official_destination_sources
  if (!is.null(destination_sources)) {
    destination_sources <- data.table::copy(data.table::as.data.table(destination_sources))
    source_keys <- c("year", "round", "office", "uf")
    if (nrow(destination_sources) && (!all(source_keys %in% names(destination_sources)) ||
        anyNA(destination_sources[, ..source_keys]))) stop("Proveniência de destinação oficial sem escopo completo")
    if (nrow(destination_sources)) {
      destination_sources <- destination_sources[year == yr & round == tr & office == oc &
        toupper(uf) %in% expected]
      if (nrow(destination_sources)) result$official_destination_sources <- as.data.frame(destination_sources)
    }
  }
  result
}

eleicao_aggregate_contest <- function(votes, universe, links, year, round, office,
                                      uf = "BR", max_distance_m = 5000,
                                      sources = list(), include_party_series = FALSE,
                                      require_reconciled = TRUE, income_reference = "national",
                                      census_year = 2022L) {
  eleicao_require()
  census <- eleicao_censo(census_year)
  if (!eleicao_censo_allowed(census_year, year)) stop("Census reference unavailable for election")
  year_arg <- as.integer(year); round_arg <- as.integer(round); office_arg <- as.integer(office)
  uf_arg <- toupper(uf)
  income_reference <- as.character(income_reference)
  if (length(income_reference) != 1L || is.na(income_reference) ||
      !income_reference %in% c("national", "state")) stop("Invalid income reference: use national or state")
  if (length(uf_arg) != 1L || !uf_arg %in% c("BR", unname(eleicao_ufs))) stop("Invalid UF")
  if (income_reference == "state" && uf_arg == "BR") stop("State income reference requires an explicit UF")
  if (!office_arg %in% as.integer(names(eleicao_office_labels))) stop("Unsupported office")
  if (uf_arg == "BR" && office_arg != 1L) stop("State offices require an explicit UF")
  if (length(max_distance_m) != 1L || !is.finite(max_distance_m) || max_distance_m <= 0) stop("Invalid distance")
  vote_audit <- if (require_reconciled) eleicao_vote_audit(votes, year_arg, round_arg, office_arg, uf_arg) else
    list(status = "unreconciled", method = "Desativado explicitamente para verificação de fixtures; não publicar")
  x <- data.table::as.data.table(votes)
  required <- c("year", "round", "uf", "municipality_tse", "zone", "place", "office",
                "candidate_id", "candidate_name", "votes", "vote_type")
  if (!all(required %in% names(x))) stop("Missing normalized vote columns")
  x <- x[year == year_arg & round == round_arg & office == office_arg & uf != "ZZ" &
           (uf_arg == "BR" | uf == uf_arg)]
  if (!nrow(x)) stop("No results for requested contest")
  if ("election_id" %in% names(x) && data.table::uniqueN(x$election_id[!is.na(x$election_id)]) > 1L) {
    stop("Multiple election identities: do not pool supplemental elections")
  }
  if (anyNA(x$votes) || any(!is.finite(x$votes)) || any(x$votes < 0) || any(x$votes %% 1 != 0)) stop("Invalid vote count")
  if (anyNA(x$vote_type) || any(!x$vote_type %in% c("candidate", "party", "special", "invalid_code"))) stop("Unknown vote type")
  if (any(x$vote_type == "invalid_code" & x$votes > 0)) stop("Unclassified positive votes")
  proportional <- office_arg %in% c(6L, 7L, 8L)
  if (!proportional && any(x$vote_type == "party" & x$votes > 0)) stop("Party votes in majoritarian race")
  # Missing place rows remain part of input-coverage diagnostics, never a synthetic local.
  if (!"place_key" %in% names(x)) x[, place_key := ifelse(!is.na(place) & place > 0,
    paste(uf, municipality_tse, zone, place, sep = ":"), NA_character_)]
  unlocated_votes <- sum(x$votes[is.na(x$place_key) & x$vote_type %in% c("candidate", "party")])
  x <- x[!is.na(place_key) & vote_type %in% c("candidate", "party")]
  if (!nrow(x)) stop("No valid located votes")
  x[, candidate_id := as.character(candidate_id)]
  denominator <- x[, .(valid_votes = sum(votes)), by = place_key][valid_votes > 0]
  if (!nrow(denominator)) stop("No positive valid-vote denominators")
  displayed <- x[if (include_party_series) vote_type %in% c("candidate", "party") else vote_type == "candidate"]
  # Name variants do not split the same candidate number in a single contest.
  candidate_info <- displayed[, .(name = {
    known <- sort(unique(candidate_name[!is.na(candidate_name) & nzchar(candidate_name)]))
    if (length(known)) known[[1L]] else paste("Candidato", candidate_id[[1L]])
  }, vote_type = vote_type[[1L]]), by = candidate_id]
  data.table::setorder(candidate_info, candidate_id)
  if (!nrow(candidate_info)) stop("No candidate series")
  candidates <- candidate_info[, .(id = candidate_id, name, vote_type)]
  u <- data.table::copy(data.table::as.data.table(universe))
  if (!all(c("code_tract", "income", "uf", "centil") %in% names(u)) || anyDuplicated(u$code_tract)) stop("Invalid fixed income universe")
  if (uf_arg != "BR") u <- u[uf == uf_arg]
  if (!nrow(u)) stop("No income tracts in requested UF")
  if (income_reference == "state") {
    # Re-rank the COMPLETE state income universe before any distance/vote
    # exclusions. National ranks remain untouched in the default reference.
    data.table::setorder(u, income, code_tract)
    u[, centil := as.integer(floor((seq_len(.N) - 1L) * 100 / .N) + 1L)]
  }
  # Census labels are computed from the COMPLETE group, before vote/spatial exclusions.
  census_groups <- u[, .(renda_sm = mean(income) / census$minimum_wage_brl, n_setores_total = .N), by = centil]
  census_groups <- merge(data.table::data.table(centil = 1:100), census_groups, by = "centil", all.x = TRUE)
  census_groups[is.na(n_setores_total), n_setores_total := 0L]
  l <- data.table::copy(data.table::as.data.table(links))
  if (!all(c("code_tract", "place_key", "distancia_m") %in% names(l))) stop("Invalid spatial links")
  l[, code_tract := eleicao_tract_code(code_tract)]
  if (anyDuplicated(l$code_tract)) stop("Duplicate spatial tract links")
  l <- l[!is.na(place_key) & is.finite(distancia_m) & distancia_m <= max_distance_m & distancia_m >= 0,
         .(code_tract, place_key)]
  linked <- merge(u[, .(code_tract, uf, centil)], l, by = "code_tract", all = FALSE)
  if (any(sub(":.*$", "", linked$place_key) != linked$uf)) stop("Cross-state spatial link")
  weights <- linked[, .(n = .N), by = .(place_key, centil)]
  weights <- merge(weights, denominator, by = "place_key", all = FALSE)
  coverage <- weights[, .(n_setores = sum(n)), by = centil]
  base <- merge(census_groups, coverage, by = "centil", all.x = TRUE)
  base[is.na(n_setores), n_setores := 0L]
  if (any(base$n_setores > base$n_setores_total)) stop("Coverage exceeds census universe")
  if (sum(base$n_setores) == 0L) stop("No observed tracts for requested contest")
  # This is candidate x PLACE x percentile, never candidate x census tract.
  local_votes <- displayed[, .(votes = sum(votes)), by = .(place_key, candidate_id)]
  weighted <- merge(local_votes, weights, by = "place_key", allow.cartesian = TRUE)
  numerator <- weighted[, .(weighted_sum = sum(100 * votes / valid_votes * n)), by = .(centil, candidate_id)]
  grid <- data.table::CJ(centil = 1:100, candidate_id = candidates$id, sorted = TRUE)
  points <- merge(grid, base, by = "centil", all.x = TRUE)
  points <- merge(points, numerator, by = c("centil", "candidate_id"), all.x = TRUE)
  # An absent candidate at an observed local has zero votes. An absent local/cargo
  # contributes no denominator; an uncovered percentile is NA, not zero.
  points[is.na(weighted_sum) & n_setores > 0, weighted_sum := 0]
  points[, votacao := ifelse(n_setores > 0, weighted_sum / n_setores, NA_real_)]
  points[, weighted_sum := NULL]
  data.table::setcolorder(points, c("centil", "renda_sm", "n_setores", "n_setores_total", "candidate_id", "votacao"))
  data.table::setorder(points, candidate_id, centil)
  if (any(points$votacao < -1e-8 | points$votacao > 100 + 1e-8, na.rm = TRUE)) stop("Invalid percentage")
  totals <- points[, .(total = sum(votacao)), by = centil]
  if (any(totals$total > 100 + 1e-7, na.rm = TRUE)) stop("Displayed shares exceed complete denominator")
  metadata <- list(method = "Média simples dos percentuais do local mais próximo atribuídos aos setores; universo de renda fixo antes das exclusões.",
    income_definition = "Rendimento nominal médio mensal dos responsáveis pelos domicílios do setor (V06004), Censo 2022; salários mínimos de 2022 (R$ 1.212).",
    income_reference = income_reference,
    percentile_scope = if (income_reference == "state") uf_arg else "BR",
    income_year = census$census_year, minimum_wage_brl = census$minimum_wage_brl,
    year = year_arg, round = round_arg, office = as.character(office_arg), uf = uf_arg,
    max_distance_m = max_distance_m,
    denominator = if (proportional) "Soma dos votos nominais e de legenda do cargo/local, excluídos os códigos especiais 95/96/97" else "Soma dos votos dos candidatos do cargo/local, excluídos os códigos especiais 95/96/97",
    vote_status_note = "O arquivo de votação por seção não certifica sozinho a situação jurídica dos votos; os denominadores devem ser reconciliados com os totais oficiais.",
    vote_audit = vote_audit,
    senate_note = if (office_arg == 5L) "Participação em votos válidos; não corresponde a percentual de eleitores, especialmente em eleições com dois votos." else NULL,
    coverage = list(included_tracts = sum(base$n_setores), total_tracts = sum(base$n_setores_total),
      observed_places = data.table::uniqueN(weights$place_key), unlocated_valid_votes = unlocated_votes),
    sources = sources)
  if (census$census_year != 2022L) metadata$income_definition <- census$income_definition
  list(metadata = metadata, candidates = as.data.frame(candidates), points = as.data.frame(points))
}

eleicao_manifest_item <- function(dataset, url) {
  m <- dataset$metadata
  income_reference <- if (is.null(m$income_reference)) "national" else m$income_reference
  id <- paste(m$year, m$round, m$office, m$uf, sep = "-")
  if (income_reference == "state") id <- paste0(id, "-state")
  if (!is.null(m$income_year) && m$income_year != 2022L) id <- eleicao_censo_series_id(id, m$income_year)
  office_label <- unname(eleicao_office_labels[m$office])
  item <- list(id = id, year = m$year,
       round = m$round, office = m$office, office_label = office_label, uf = m$uf,
       income_reference = income_reference,
       label = paste(m$year, paste0(m$round, "º turno"), office_label, m$uf, sep = " · "),
       url = url, status = "available")
  if (!is.null(m$income_year) && m$income_year != 2022L) item$census_year <- m$income_year
  if (isTRUE(m$party_series_complete)) item$party_series_complete <- TRUE
  item
}

eleicao_write_dataset <- function(dataset, path, url = basename(path)) {
  eleicao_require()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(dataset, path, auto_unbox = TRUE, dataframe = "rows", na = "null",
                       null = "null", digits = 10, pretty = FALSE)
  eleicao_manifest_item(dataset, url)
}
