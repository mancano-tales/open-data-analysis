# Independent legacy ledger. Does not edit/patch the active reconciler or runner.
# Uses all-or-zero ballots only; never allocates a zone total across places.
legacy14_base <- c("year", "round", "election_id", "uf", "municipality_tse", "zone", "office")

legacy14_require <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
}

legacy14_table <- function(value, required) {
  legacy14_require()
  x <- data.table::copy(data.table::as.data.table(value))
  if (!all(required %in% names(x))) stop("Missing legacy columns: ", paste(setdiff(required, names(x)), collapse = ","))
  if (anyNA(x[, ..legacy14_base])) stop("Missing legacy contest key")
  x[, municipality_tse := as.character(as.integer(municipality_tse))]
  if (anyNA(x$municipality_tse)) stop("Invalid TSE municipality code")
  x[]
}

legacy14_counts <- function(x, fields) {
  for (name in fields) {
    z <- x[[name]]
    if (anyNA(z) || any(!is.finite(z)) || any(z < 0) || any(z %% 1 != 0)) stop("Invalid legacy count: ", name)
  }
}

legacy14_party_number <- function(number, office) {
  as.integer(number %/% ifelse(office == 6L, 100L, ifelse(office %in% c(7L, 8L), 1000L, ifelse(office == 5L, 10L, 1L))))
}

legacy14_reconcile_contest <- function(raw, candidates, parties, detail) {
  b <- legacy14_base; ck <- c(b, "candidate_id"); pk <- c(b, "party_id")
  raw <- legacy14_table(raw, c(b, "candidate_id", "votes", "vote_type", "place_key"))
  candidates <- legacy14_table(candidates, c(b, "candidate_id", "party_id", "candidate_uid", "official_name", "official_nominal"))
  parties <- legacy14_table(parties, c(b, "party_id", "official_nominal", "official_legend"))
  detail_fields <- c("official_nominal", "official_legend", "official_apurados", "official_nulls", "official_blank",
                     "official_annulled", "official_pending", "official_separate")
  detail <- legacy14_table(detail, c(b, detail_fields))
  for (x in list(raw, candidates, parties, detail)) {
    if (nrow(x) && (any(!x$year %in% c(2010L, 2014L)) || any(!x$office %in% c(3L, 5L, 6L, 7L, 8L)))) stop("Unsupported legacy scope")
  }
  contest <- unique(detail[, .(year, round, election_id, uf, office)])
  if (nrow(contest) != 1L) stop("Expected one legacy contest")
  for (x in list(raw, candidates, parties)) {
    if (nrow(x) && !data.table::fsetequal(unique(x[, .(year, round, election_id, uf, office)]), contest)) stop("Mixed or mismatched legacy contest")
  }
  legacy14_counts(raw, "votes"); legacy14_counts(candidates, "official_nominal")
  legacy14_counts(parties, c("official_nominal", "official_legend")); legacy14_counts(detail, detail_fields)
  if (contest$office %in% c(3L, 5L) && (any(detail$official_legend > 0) || any(raw$vote_type == "party" & raw$votes > 0))) stop("Legacy majoritarian contest cannot contain party votes")
  if (any(raw$vote_type == "invalid_code" & raw$votes > 0) || any(!raw$vote_type %in% c("candidate", "party", "special", "invalid_code"))) stop("Unclassified raw legacy votes")
  if (any(raw$votes > 0 & is.na(raw$place_key))) stop("Legacy positive votes lack a polling place")
  if (any(detail$official_annulled > 0 | detail$official_pending > 0 | detail$official_separate > 0)) stop("Legacy annulled/pending/separate categories need independent separability evidence")
  if (any(raw$candidate_id == 97L & raw$votes > 0)) stop("Legacy code 97 is not separable by this helper")
  if (any(raw$vote_type == "special" & !raw$candidate_id %in% c(95L, 96L, 97L) & raw$votes > 0)) stop("Unsupported special legacy ballot")
  raw[, candidate_id := as.integer(candidate_id)]
  candidates[, candidate_id := as.integer(candidate_id)]
  if (anyNA(raw$candidate_id) || anyNA(candidates$candidate_id) || anyNA(candidates$party_id) || anyNA(parties$party_id)) stop("Invalid candidate/party identifiers")
  candidates[, candidate_uid := as.character(candidate_uid)]
  candidates[, official_name := trimws(as.character(official_name))]
  identity_scope <- c("year", "round", "election_id", "uf", "office", "candidate_id")
  positive <- candidates[official_nominal > 0]
  if (anyNA(positive$candidate_uid) || anyNA(positive$official_name) || any(positive$candidate_uid == "" | positive$official_name == "")) stop("Missing positive official candidate identity")
  identities <- positive[, .(uids = data.table::uniqueN(candidate_uid), names = data.table::uniqueN(official_name),
    parties = data.table::uniqueN(party_id)), by = identity_scope]
  if (any(identities$uids != 1L | identities$names != 1L | identities$parties != 1L)) stop("Ambiguous legacy substitution/candidate identity; raw ballot number cannot distinguish official candidates")
  if (anyDuplicated(positive[, c(ck, "candidate_uid"), with = FALSE])) stop("Duplicate positive official candidate key")
  if (!"candidate_name" %in% names(raw)) raw[, candidate_name := NA_character_]
  raw[, identified_candidate_uid := NA_character_]
  # Every positive final nominal count must match the exact same raw ballot key.
  # A zero or absent official count never makes a positive raw ballot nominally valid.
  c <- candidates[, .(official_nominal = sum(as.numeric(official_nominal)),
    party_id = if (data.table::uniqueN(party_id) == 1L) party_id[[1L]] else NA_integer_,
    official_candidate_uid = if (any(official_nominal > 0)) candidate_uid[official_nominal > 0][[1L]] else NA_character_,
    official_name = if (any(official_nominal > 0)) official_name[official_nominal > 0][[1L]] else NA_character_), by = ck]
  if (anyNA(c$party_id)) stop("Ambiguous legacy candidate party")
  actual <- raw[vote_type == "candidate", .(raw_nominal = sum(votes)), by = ck]
  candidate_check <- merge(actual, c, by = ck, all = TRUE)
  candidate_check[is.na(raw_nominal), raw_nominal := 0]
  candidate_check[, official_present := !is.na(official_nominal)]
  candidate_check[is.na(official_nominal), official_nominal := 0]
  if (any(candidate_check$official_nominal > 0 & candidate_check$official_nominal != candidate_check$raw_nominal)) stop("Positive official nominal is missing or partially differs from raw ballots")
  candidate_check[is.na(party_id), party_id := legacy14_party_number(candidate_id, office)]
  candidate_check[, keep_nominal := official_nominal > 0]
  candidate_check[, excluded_nominal := raw_nominal - official_nominal]
  if (any(candidate_check$excluded_nominal < 0)) stop("Negative excluded legacy nominal votes")
  nominal_rows <- merge(raw[vote_type == "candidate"], candidate_check[, c(ck, "party_id", "keep_nominal", "official_candidate_uid", "official_name"), with = FALSE], by = ck, all.x = TRUE, sort = FALSE)
  nominal_rows[keep_nominal == TRUE, `:=`(candidate_name = official_name, identified_candidate_uid = official_candidate_uid)]
  if (anyNA(nominal_rows$keep_nominal)) stop("Unclassified nominal ballot key")
  p <- parties[, .(official_party_nominal = sum(as.numeric(official_nominal)),
                   official_party_legend = sum(as.numeric(official_legend))), by = pk]
  cparty <- c[, .(candidate_nominal = sum(official_nominal)), by = pk]
  direct <- raw[vote_type == "party", .(raw_direct_legend = sum(votes)), by = c(b, "candidate_id")]
  data.table::setnames(direct, "candidate_id", "party_id")
  excluded <- candidate_check[, .(excluded_nominal = sum(excluded_nominal)), by = pk]
  party_check <- Reduce(function(a, z) merge(a, z, by = pk, all = TRUE), list(p, cparty, direct, excluded))
  count_fields <- c("official_party_nominal", "official_party_legend", "candidate_nominal", "raw_direct_legend", "excluded_nominal")
  for (name in count_fields) data.table::set(party_check, which(is.na(party_check[[name]])), name, 0)
  if (any(party_check$official_party_nominal != party_check$candidate_nominal)) stop("Candidate and party official nominal totals differ")
  party_check[, `:=`(keep_direct = FALSE, convert_all_excluded = FALSE, rule = "unclassified")]
  # The old party dictionary does not distinguish direct and final legend.
  # Equality with raw directs cannot rule out offset conversion/invalidation.
  zone_bounds <- party_check[, .(pool = sum(raw_direct_legend + excluded_nominal),
    excluded_pool = sum(excluded_nominal)), by = b]
  zone_bounds <- merge(zone_bounds, detail[, .(zone_final_legend = sum(official_legend)), by = b], by = b, all = TRUE)
  if (anyNA(zone_bounds) || any(zone_bounds$zone_final_legend > zone_bounds$pool)) stop("Legacy final legend exceeds observed ballot pool")
  if (contest$office %in% c(3L, 5L)) {
    party_check[, `:=`(keep_direct = FALSE, rule = "majoritarian_excluded_null")]
  } else {
    party_check <- merge(party_check, zone_bounds, by = b, all.x = TRUE, sort = FALSE)
    # Never anchor a party group to the generic legacy party count: it
    # might be raw direct rather than valid direct/final. Cross-party offset
    # conversions would then conceal invalid directs even in a no-excess party.
    # Only independent FINAL zone endpoints identify every ballot destination.
    party_check[zone_final_legend == 0,
      `:=`(keep_direct = FALSE, convert_all_excluded = FALSE, rule = "zone_final_zero")]
    party_check[zone_final_legend == pool,
      `:=`(keep_direct = TRUE, convert_all_excluded = TRUE, rule = "zone_entire_pool_valid")]

  }
  if (any(party_check$rule == "unclassified")) stop("Partial or ambiguous legacy legend conversion; direct/final semantics cannot identify polling-place votes")
  party_check[, `:=`(computed_party_legend = ifelse(keep_direct, raw_direct_legend, 0) + ifelse(convert_all_excluded, excluded_nominal, 0),
                    computed_conversion = ifelse(convert_all_excluded, excluded_nominal, 0))]
  # Generic party count may mean raw direct, valid direct OR final count;
  # this compatibility is not used as evidence to resolve interior totals.
  computed_direct <- ifelse(party_check$keep_direct, party_check$raw_direct_legend, 0)
  if (any(!((party_check$official_party_legend == computed_direct) |
            (party_check$official_party_legend == party_check$computed_party_legend) |
            (party_check$official_party_legend == party_check$raw_direct_legend)))) stop("Legacy generic party legend incompatible with identified endpoints")
  keep_columns <- names(raw)
  direct_rows <- data.table::copy(raw[vote_type == "party"])
  data.table::setnames(direct_rows, "candidate_id", "party_id")
  direct_rows <- merge(direct_rows, party_check[, c(pk, "keep_direct"), with = FALSE], by = pk, all.x = TRUE, sort = FALSE)
  direct_rows <- direct_rows[keep_direct == TRUE]
  data.table::setnames(direct_rows, "party_id", "candidate_id")
  converted <- merge(nominal_rows[keep_nominal == FALSE], party_check[, c(pk, "convert_all_excluded"), with = FALSE], by = pk, all.x = TRUE, sort = FALSE)
  converted <- converted[convert_all_excluded == TRUE]
  converted[, `:=`(candidate_id = party_id, vote_type = "party")]
  if ("candidate_name" %in% names(converted)) converted[, candidate_name := paste("Legacy party", party_id)]
  legal <- data.table::rbindlist(list(nominal_rows[keep_nominal == TRUE, ..keep_columns],
    direct_rows[, ..keep_columns], converted[, ..keep_columns]), use.names = TRUE)
  d <- detail[, lapply(.SD, function(z) sum(as.numeric(z))), by = b, .SDcols = detail_fields]
  raw_zone <- raw[, .(raw_apurados = sum(votes), raw_nominal = sum(votes[vote_type == "candidate"]),
    raw_direct_legend = sum(votes[vote_type == "party"]), raw_blank = sum(votes[candidate_id == 95L]),
    raw_null96 = sum(votes[candidate_id == 96L])), by = b]
  computed <- legal[, .(computed_nominal = sum(votes[vote_type == "candidate"]),
    computed_legend = sum(votes[vote_type == "party"]), computed_valid_votes = sum(votes)), by = b]
  conversions <- party_check[, .(computed_conversion = sum(computed_conversion)), by = b]
  if (!data.table::fsetequal(d[, ..b], raw_zone[, ..b])) stop("Legacy raw and detail zone keys do not match")
  checks <- Reduce(function(a, z) merge(a, z, by = b, all.x = TRUE), list(d, raw_zone, computed, conversions))
  # Classified ballots can yield zero valid votes in a present, observed zone.
  # This does not manufacture a missing raw/detail zone, checked just above.
  for (name in c("computed_nominal", "computed_legend", "computed_valid_votes", "computed_conversion"))
    data.table::set(checks, which(is.na(checks[[name]])), name, 0)
  if (anyNA(checks)) stop("Legacy raw, final and detail zone keys do not match")
  checks[, official_valid_votes := official_nominal + official_legend]
  checks[, excluded_nominal := raw_nominal - computed_nominal]
  checks[, excluded_direct_legend := raw_direct_legend - (computed_legend - computed_conversion)]
  checks[, computed_nulls := raw_null96 + excluded_nominal - computed_conversion + excluded_direct_legend]
  checks[, ledger_apurados := official_valid_votes + official_blank + official_nulls + official_annulled + official_pending + official_separate]
  checks[, difference := computed_valid_votes - official_valid_votes]
  checks[, reconciled := difference == 0 & computed_nominal == official_nominal & computed_legend == official_legend &
    computed_nulls == official_nulls & raw_blank == official_blank & raw_apurados == official_apurados & ledger_apurados == official_apurados]
  if (any(!checks$reconciled)) stop("Legacy exact nominal/party/null/blank/apurados zone reconciliation failed")
  list(legal = legal, checks = checks, party_checks = party_check, candidate_checks = candidate_check)
}

legacy14_cached_csv <- function(prefix, year, uf, cache_dir, probe_dir) {
  archive <- file.path(cache_dir, paste0(prefix, "_", year, if (prefix == "votacao_secao") paste0("_", uf) else "", ".zip"))
  if (!file.exists(archive)) stop("Required official ZIP is not cached: ", basename(archive))
  members <- utils::unzip(archive, list = TRUE)$Name
  member <- members[grepl(paste0("(^|/)", prefix, "_", year, "_", uf, "[.]csv$"), members, ignore.case = TRUE)]
  if (length(member) != 1L) stop("Expected one official CSV for ", prefix, "/", year, "/", uf)
  folder <- file.path(probe_dir, "extracted", paste0(prefix, "_", year))
  target <- file.path(folder, member)
  # Never reuse a stale CSV after a ZIP replacement under the same filename.
  # Only the selected UF member is refreshed; no national CSV is extracted.
  utils::unzip(archive, files = member, exdir = folder, overwrite = TRUE)
  target
}

# Includes special ballots. The ordinary places cache and tse_place_votes()
# intentionally discard those codes and must NEVER be used for this ledger.
legacy14_place_all_types <- function(raw) {
  raw <- data.table::as.data.table(raw)
  keys <- c("year", "round", "election_id", "election_date", "uf", "municipality_tse", "zone",
    "place", "place_key", "office", "candidate_id", "candidate_name", "vote_type")
  if ("candidate_seq" %in% names(raw)) keys <- c(keys, "candidate_seq")
  if (!all(c(keys, "votes") %in% names(raw))) stop("Incomplete all-types polling-place input")
  if (any(raw$votes > 0 & is.na(raw$place_key))) stop("Positive raw votes lack polling place")
  raw[, .(votes = sum(votes)), by = keys]
}

legacy14_read_all_types <- function(path, year, uf, offices, probe_dir) {
  source_file <- "shared-pipeline/eleicoes/historico/tse.R"
  scope <- list(year = as.integer(year), uf = toupper(uf), offices = sort(unique(as.integer(offices))),
    rounds = c(1L, 2L), election_type = 2L)
  identity <- list(version = 1L, includes_special = TRUE, scope = scope,
    zip_md5 = unname(tools::md5sum(path)), normalizer_md5 = unname(tools::md5sum(source_file)))
  if (anyNA(c(identity$zip_md5, identity$normalizer_md5))) stop("Cannot fingerprint legacy all-types source")
  dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
  cache <- file.path(probe_dir, paste0("all-types-", year, "-", toupper(uf), "-", paste(scope$offices, collapse = "_"), "-v1.rds"))
  if (file.exists(cache)) {
    cached <- readRDS(cache)
    if (identical(attr(cached, "all_types_identity"), identity)) return(cached)
    rm(cached)
  }
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[grepl(paste0("(^|/)votacao_secao_", year, "_", toupper(uf), "[.]csv$"), members, ignore.case = TRUE)]
  if (length(member) != 1L) stop("Expected one raw legacy UF member")
  # Content-specific extraction prevents a replaced ZIP from reusing stale CSV.
  folder <- file.path(probe_dir, "raw-extracted", paste0(year, "-", toupper(uf), "-", identity$zip_md5))
  utils::unzip(path, files = member, exdir = folder, overwrite = TRUE)
  csv <- file.path(folder, member)
  header <- names(data.table::fread(csv, nrows = 0L, encoding = "Latin-1"))
  wanted <- c("ANO_ELEICAO", "AA_ELEICAO", "NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO",
    "NR_LOCAL_VOTACAO", "CD_CARGO", "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS", "CD_ELEICAO", "CD_TIPO_ELEICAO", "DT_ELEICAO", "SQ_CANDIDATO")
  input <- data.table::fread(csv, select = intersect(wanted, header), encoding = "Latin-1", showProgress = FALSE)
  if (!all(c("CD_CARGO", "NR_TURNO", "SG_UF", "CD_TIPO_ELEICAO") %in% names(input))) stop("Missing raw election scope")
  input <- input[CD_CARGO %in% scope$offices & NR_TURNO %in% scope$rounds & SG_UF == scope$uf & CD_TIPO_ELEICAO == scope$election_type]
  normalizer <- new.env(parent = environment())
  sys.source(source_file, envir = normalizer)
  normalized <- normalizer$tse_normalize(input, expected_year = year)
  rm(input)
  raw <- legacy14_place_all_types(normalized)
  rm(normalized)
  attr(raw, "all_types_identity") <- identity
  attr(raw, "all_types_source") <- list(archive = basename(path), member = member,
    zip_md5 = identity$zip_md5, archive_bytes = unname(file.info(path)$size))
  saveRDS(raw, cache)
  raw
}

legacy14_reconcile_cached <- function(year, uf, offices = c(3L, 5L, 6L, 7L, 8L),
                                      cache_dir = "data-raw/eleicoes-renda/historico",
                                      probe_dir = file.path(cache_dir, "legacy14-isolated")) {
  legacy14_require()
  ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  if (length(year) != 1L || !year %in% c(2010L, 2014L) || length(uf) != 1L || !toupper(uf) %in% ufs) stop("Expected one legacy year and Brazilian UF")
  offices <- sort(unique(as.integer(offices)))
  if (!length(offices) || anyNA(offices) || any(!offices %in% c(3L, 5L, 6L, 7L, 8L))) stop("Unsupported legacy offices")
  uf <- toupper(uf)
  source("shared-pipeline/eleicoes/historico/tse.R", local = environment(), encoding = "UTF-8")
  year_arg <- as.integer(year); uf_arg <- toupper(uf)
  dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
  # Read only the requested UF archive; the function never downloads archives.
  vote_zip <- file.path(cache_dir, paste0("votacao_secao_", year, "_", uf, ".zip"))
  if (!file.exists(vote_zip)) stop("Raw UF section ZIP not cached")
  raw <- legacy14_read_all_types(vote_zip, year, uf, offices, probe_dir)
  raw_source <- attr(raw, "all_types_source")
  aliases <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "CD_CARGO")
  official_inputs <- list()
  read_official <- function(prefix, columns, renamed) {
    path <- legacy14_cached_csv(prefix, year, uf, cache_dir, probe_dir)
    official_inputs[[prefix]] <<- list(archive = paste0(prefix, "_", year, ".zip"),
      member = basename(path), csv_md5 = unname(tools::md5sum(path)), csv_bytes = unname(file.info(path)$size))
    x <- data.table::fread(path, select = c(aliases, "CD_TIPO_ELEICAO", columns), encoding = "Latin-1")
    data.table::setnames(x, c(aliases, columns), c(legacy14_base, renamed))
    x <- x[year == year_arg & uf == uf_arg & CD_TIPO_ELEICAO == 2L & office %in% offices]
    x[, municipality_tse := as.character(as.integer(municipality_tse))]
    x[]
  }
  candidates <- read_official("votacao_candidato_munzona", c("NR_CANDIDATO", "NR_PARTIDO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "QT_VOTOS_NOMINAIS"), c("candidate_id", "party_id", "candidate_uid", "official_name", "official_nominal"))
  parties <- read_official("votacao_partido_munzona", c("NR_PARTIDO", "QT_VOTOS_NOMINAIS", "QT_VOTOS_LEGENDA"), c("party_id", "official_nominal", "official_legend"))
  detail_columns <- c("QT_VOTOS_NOMINAIS_VALIDOS", "QT_TOTAL_VOTOS_LEG_VALIDOS", "QT_VOTOS", "QT_TOTAL_VOTOS_NULOS", "QT_VOTOS_BRANCOS", "QT_TOTAL_VOTOS_ANULADOS", "QT_TOTAL_VOTOS_ANUL_SUBJUD", "QT_VOTOS_ANULADOS_APU_SEP")
  detail <- read_official("detalhe_votacao_munzona", detail_columns,
    c("official_nominal", "official_legend", "official_apurados", "official_nulls", "official_blank", "official_annulled", "official_pending", "official_separate"))
  scope <- c("year", "round", "election_id", "uf", "office")
  contests <- unique(data.table::rbindlist(list(detail[, ..scope], raw[, ..scope])))
  good <- list(); checks <- list(); blocked <- list()
  for (i in seq_len(nrow(contests))) {
    row <- contests[i]
    take <- function(x) merge(x, row, by = scope, all = FALSE, sort = FALSE)
    outcome <- tryCatch(legacy14_reconcile_contest(take(raw), take(candidates), take(parties), take(detail)), error = function(e) e)
    if (inherits(outcome, "error")) { blocked[[length(blocked) + 1L]] <- cbind(row, reason = conditionMessage(outcome)); next }
    good[[length(good) + 1L]] <- outcome$legal
    checks[[length(checks) + 1L]] <- outcome$checks
    tag <- paste(year, uf, row$round, row$office, sep = "-")
    data.table::fwrite(outcome$party_checks, file.path(probe_dir, paste0(tag, "-party-checks.csv")))
    data.table::fwrite(outcome$candidate_checks, file.path(probe_dir, paste0(tag, "-candidate-checks.csv")))
  }
  if (!nrow(contests)) stop("No official ordinary-election contest in the requested scope")
  accepted_rows <- if (length(good)) data.table::rbindlist(good, use.names = TRUE) else raw[0]
  places <- tse_place_votes(accepted_rows)
  candidate_identity <- if (nrow(accepted_rows)) unique(accepted_rows[vote_type == "candidate",
    .(year, round, election_id, uf, office, candidate_id, candidate_name, identified_candidate_uid)]) else data.table::data.table()
  zone_checks <- data.table::rbindlist(checks, use.names = TRUE)
  summary <- if (nrow(zone_checks)) zone_checks[, .(official_valid_votes = sum(official_valid_votes), computed_valid_votes = sum(computed_valid_votes),
    difference = sum(difference), zones = .N, reconciled = all(reconciled)), by = scope] else
      cbind(contests[0], official_valid_votes = numeric(), computed_valid_votes = numeric(), difference = numeric(), zones = integer(), reconciled = logical())
  unavailable <- if (length(blocked)) data.table::rbindlist(blocked, fill = TRUE) else data.table::data.table()
  represented <- unique(data.table::rbindlist(list(summary[, ..scope],
    if (nrow(unavailable)) unavailable[, ..scope] else contests[0]), use.names = TRUE))
  if (!data.table::fsetequal(contests, represented)) stop("Legacy expected contest disappeared from accepted/unavailable partition")
  if (nrow(unavailable) && nrow(merge(summary[, ..scope], unavailable[, ..scope], by = scope))) stop("Legacy contest is both accepted and unavailable")
  attr(places, "normalizer_version") <- 2L
  attr(places, "source_md5") <- raw_source$zip_md5
  attr(places, "vote_audit") <- list(status = "diagnostic_only", publishable = FALSE, method = "Unreviewed 2010/2014 prototype: candidate all-or-zero, legend endpoint identification, exact null and full zone ledger; no allocation",
    checks = as.data.frame(summary), unavailable = as.data.frame(unavailable), election_ids = unique(summary$election_id),
    expected_contests = as.data.frame(contests),
    provenance = list(raw_archive = basename(vote_zip), raw_member = raw_source$member, raw_md5 = attr(places, "source_md5"),
      raw_bytes = unname(file.info(vote_zip)$size), official_members = official_inputs),
    candidate_identity = as.data.frame(candidate_identity),
    source_assumptions = c("Official positive candidate nominal votes equal the same full raw candidate key; independently match final nominal per zone",
      "Final zone legend means valid direct plus converted nominal; generic historical party legend is not used to identify destinations",
      "Majoritarian contests contain no legend; proportional accepted only at final zone zero/full observed pool endpoints"),
    election_scope = "ordinary elections; isolated diagnostic; independent review required")
  target <- file.path(probe_dir, paste0("places-", year, "-", uf, "-", paste(offices, collapse = "_"), "-diagnostic.rds"))
  saveRDS(places, target)
  if (nrow(zone_checks)) data.table::fwrite(zone_checks, sub("[.]rds$", "-audit.csv", target))
  if (nrow(unavailable)) data.table::fwrite(unavailable, sub("[.]rds$", "-unavailable.csv", target))
  list(path = target, places = places, checks = summary, zone_checks = zone_checks, unavailable = unavailable)
}

legacy14_probe_cached <- function(year, uf = "AC", cache_dir = "data-raw/eleicoes-renda/historico",
                                  probe_dir = file.path(cache_dir, "legacy14-isolated")) {
  if (!identical(toupper(uf), "AC")) stop("Named AC probe accepts only AC; use the independent diagnostic entry point for other UFs")
  legacy14_reconcile_cached(year, uf, cache_dir = cache_dir, probe_dir = probe_dir)
}
