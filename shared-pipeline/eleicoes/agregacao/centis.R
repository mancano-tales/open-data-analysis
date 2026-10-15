# Fixed Census-2022 income ranks and memory-bounded electoral aggregation.
# Source from the repository root. No downloads or side effects on source().
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
  x <- data.table::copy(data.table::as.data.table(renda))
  if (!all(c("code_tract", "V06001", "V06004") %in% names(x))) stop("Missing census income columns")
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
  list(status = "reconciled", scope = "official-valid-votes", method = audit$method,
       source = list(label = "TSE — resultados oficiais", url = "https://dadosabertos.tse.jus.br/"),
       official_valid_votes = sum(checks$official_valid_votes),
       computed_valid_votes = sum(checks$computed_valid_votes),
       difference = sum(checks$difference), checked_units = nrow(checks), ufs = sort(unique(checks$uf)))
}

eleicao_aggregate_contest <- function(votes, universe, links, year, round, office,
                                      uf = "BR", max_distance_m = 5000,
                                      sources = list(), include_party_series = FALSE,
                                      require_reconciled = TRUE) {
  eleicao_require()
  year_arg <- as.integer(year); round_arg <- as.integer(round); office_arg <- as.integer(office)
  uf_arg <- toupper(uf)
  if (length(uf_arg) != 1L || !uf_arg %in% c("BR", unname(eleicao_ufs))) stop("Invalid UF")
  if (!office_arg %in% as.integer(names(eleicao_office_labels))) stop("Unsupported office")
  if (uf_arg == "BR" && office_arg != 1L) stop("State offices require an explicit UF")
  if (length(max_distance_m) != 1L || !is.finite(max_distance_m) || max_distance_m <= 0) stop("Invalid distance")
  vote_audit <- if (require_reconciled) eleicao_vote_audit(votes, year_arg, round_arg, office_arg, uf_arg) else
    list(status = "unreconciled", method = "Desativado explicitamente para verificação de fixtures; não publicar")
  x <- data.table::copy(data.table::as.data.table(votes))
  required <- c("year", "round", "uf", "municipality_tse", "zone", "place", "office",
                "candidate_id", "candidate_name", "votes", "vote_type")
  if (!all(required %in% names(x))) stop("Missing normalized vote columns")
  x <- x[year == year_arg & round == round_arg & office == office_arg & uf != "ZZ"]
  if (uf_arg != "BR") x <- x[uf == uf_arg]
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
  # Census labels are computed from the COMPLETE group, before vote/spatial exclusions.
  census_groups <- u[, .(renda_sm = mean(income) / 1212, n_setores_total = .N), by = centil]
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
    percentile_scope = "BR", income_year = 2022L, minimum_wage_brl = 1212,
    year = year_arg, round = round_arg, office = as.character(office_arg), uf = uf_arg,
    max_distance_m = max_distance_m,
    denominator = if (proportional) "Soma dos votos nominais e de legenda do cargo/local, excluídos os códigos especiais 95/96/97" else "Soma dos votos dos candidatos do cargo/local, excluídos os códigos especiais 95/96/97",
    vote_status_note = "O arquivo de votação por seção não certifica sozinho a situação jurídica dos votos; os denominadores devem ser reconciliados com os totais oficiais.",
    vote_audit = vote_audit,
    senate_note = if (office_arg == 5L) "Participação em votos válidos; não corresponde a percentual de eleitores, especialmente em eleições com dois votos." else NULL,
    coverage = list(included_tracts = sum(base$n_setores), total_tracts = sum(base$n_setores_total),
      observed_places = data.table::uniqueN(weights$place_key), unlocated_valid_votes = unlocated_votes),
    sources = sources)
  list(metadata = metadata, candidates = as.data.frame(candidates), points = as.data.frame(points))
}

eleicao_manifest_item <- function(dataset, url) {
  m <- dataset$metadata
  office_label <- unname(eleicao_office_labels[m$office])
  list(id = paste(m$year, m$round, m$office, m$uf, sep = "-"), year = m$year,
       round = m$round, office = m$office, office_label = office_label, uf = m$uf,
       label = paste(m$year, paste0(m$round, "º turno"), office_label, m$uf, sep = " · "),
       url = url, status = "available")
}

eleicao_write_dataset <- function(dataset, path, url = basename(path)) {
  eleicao_require()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(dataset, path, auto_unbox = TRUE, dataframe = "rows", na = "null",
                       null = "null", digits = 10, pretty = FALSE)
  eleicao_manifest_item(dataset, url)
}
