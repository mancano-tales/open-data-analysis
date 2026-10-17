# Contemporary presidential validity from independent official candidate and
# municipality/zone detail sources. No cache, downloads, rateio or legal text
# heuristics. The returned keep mask always follows the ORIGINAL raw row order.
presidential_base_keys <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "CD_CARGO",
  "SG_UF", "CD_MUNICIPIO", "NR_ZONA")

presidential_read <- function(value, required, label) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table")
  if (is.character(value) && length(value) == 1L) {
    if (!file.exists(value)) stop("Missing ", label, " CSV: ", value)
    header <- names(data.table::fread(value, nrows = 0L, encoding = "Latin-1"))
    missing <- setdiff(required, header)
    if (length(missing)) stop("Missing required ", label, " columns: ", paste(missing, collapse = ", "))
    return(data.table::fread(value, select = required, encoding = "Latin-1", showProgress = FALSE))
  }
  if (!is.data.frame(value)) stop("Expected ", label, " CSV path or table")
  missing <- setdiff(required, names(value))
  if (length(missing)) stop("Missing required ", label, " columns: ", paste(missing, collapse = ", "))
  data.table::copy(data.table::as.data.table(value)[, ..required])
}

presidential_count <- function(x, columns, label) {
  for (name in columns) {
    values <- suppressWarnings(as.numeric(x[[name]]))
    if (anyNA(values) || any(!is.finite(values)) || any(values < 0 | values %% 1 != 0))
      stop("Invalid ", label, " count: ", name)
    data.table::set(x, j = name, value = values)
  }
  invisible(x)
}

presidential_scope <- function(x, election_ids, label) {
  # These scope fields must be explicit, even on rows that are later excluded.
  scope_keys <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "CD_CARGO", "SG_UF")
  if (anyNA(x[, ..scope_keys])) stop("Missing ", label, " election scope")
  x$ANO_ELEICAO == 2026L & x$NR_TURNO == 1L & x$CD_CARGO == 1L &
    x$CD_ELEICAO %in% election_ids & toupper(as.character(x$SG_UF)) != "ZZ"
}

presidential_normalize_keys <- function(x, ballot_column = NULL) {
  numeric <- c(setdiff(presidential_base_keys, "SG_UF"), ballot_column)
  for (name in numeric) {
    values <- suppressWarnings(as.numeric(as.character(x[[name]])))
    if (anyNA(values) || any(!is.finite(values)) || any(values %% 1 != 0)) stop("Invalid presidential key: ", name)
    if (name == "CD_MUNICIPIO") data.table::set(x, j = name, value = sprintf("%.0f", values))
    else data.table::set(x, j = name, value = as.integer(values))
  }
  x[, SG_UF := toupper(trimws(as.character(SG_UF)))]
  if (any(!grepl("^[A-Z]{2}$", x$SG_UF))) stop("Invalid presidential UF")
  invisible(x)
}

validade_presidencial <- function(raw, official_candidate_csv, official_detail_csv, election_ids) {
  if (!is.numeric(election_ids) || !length(election_ids) || anyNA(election_ids) ||
      any(!is.finite(election_ids)) || any(election_ids <= 0 | election_ids %% 1 != 0 | election_ids > .Machine$integer.max) || anyDuplicated(election_ids))
    stop("election_ids must be an explicit, unique vector of positive integer ordinary-election IDs")
  election_ids <- as.integer(election_ids)
  b <- presidential_base_keys; k <- c(b, "NR_VOTAVEL")
  r <- presidential_read(raw, c(k, "QT_VOTOS"), "raw")
  selected <- presidential_scope(r, election_ids, "raw")
  if (!any(selected)) stop("No 2026 first-round presidential rows for requested election_ids")
  source_rows <- which(selected)
  r <- r[selected]
  r[, source_row := source_rows]
  presidential_normalize_keys(r, "NR_VOTAVEL"); presidential_count(r, "QT_VOTOS", "raw")
  if (!setequal(unique(r$CD_ELEICAO), election_ids)) stop("Requested election_ids absent from raw presidential scope")
  c <- presidential_read(official_candidate_csv,
    c(b, "NR_CANDIDATO", "QT_VOTOS_NOMINAIS", "QT_VOTOS_NOMINAIS_VALIDOS"), "official candidate")
  c <- c[presidential_scope(c, election_ids, "official candidate")]
  data.table::setnames(c, "NR_CANDIDATO", "NR_VOTAVEL")
  presidential_normalize_keys(c, "NR_VOTAVEL")
  presidential_count(c, c("QT_VOTOS_NOMINAIS", "QT_VOTOS_NOMINAIS_VALIDOS"), "official candidate")
  if (any(c$NR_VOTAVEL <= 0 | c$NR_VOTAVEL %in% c(95L, 96L, 97L))) stop("Invalid official presidential candidate number")
  candidate <- c[, .(official_nominal = sum(QT_VOTOS_NOMINAIS),
    official_nominal_valid = sum(QT_VOTOS_NOMINAIS_VALIDOS)), by = k]
  if (any(candidate$official_nominal_valid > candidate$official_nominal)) stop("Official valid nominal exceeds total nominal")
  if (any(candidate$official_nominal_valid != 0 & candidate$official_nominal_valid != candidate$official_nominal))
    stop("Partial nominal validity: cannot identify valid raw rows without allocation")
  actual <- r[!NR_VOTAVEL %in% c(95L, 96L, 97L), .(raw_nominal = sum(QT_VOTOS)), by = k]
  candidate_check <- merge(actual, candidate, by = k, all = TRUE, sort = FALSE)
  candidate_check[, official_present := !is.na(official_nominal)]
  if (any(candidate_check$official_nominal > 0 & is.na(candidate_check$raw_nominal), na.rm = TRUE))
    stop("Official positive candidate votes absent from raw rows")
  candidate_check[is.na(raw_nominal), raw_nominal := 0]
  if (any(candidate_check$official_present & candidate_check$raw_nominal != candidate_check$official_nominal, na.rm = TRUE))
    stop("Raw and official candidate nominal totals differ")
  candidate_check[, keep_nominal := official_present & !is.na(official_nominal_valid) &
    official_nominal_valid > 0 & official_nominal_valid == official_nominal]
  candidate_check[, motivo := ifelse(!official_present, "nulo_tecnico_sem_candidato",
    ifelse(keep_nominal, "nominal_valido", "nominal_nao_valido"))]
  # One mask decision per candidate municipality/zone key, never one decision
  # for a national candidate total and never a proportional allocation.
  decisions <- candidate_check[r, on = k,
    .(source_row = i.source_row, keep = keep_nominal, motivo = motivo)]
  keep <- rep(FALSE, nrow(raw))
  keep[decisions$source_row[!is.na(decisions$keep) & decisions$keep]] <- TRUE
  r[, motivo := decisions$motivo]
  r[NR_VOTAVEL == 95L, motivo := "branco_codigo_95"]
  r[NR_VOTAVEL == 96L, motivo := "nulo_codigo_96"]
  r[NR_VOTAVEL == 97L, motivo := "anulado_codigo_97"]
  if (anyNA(r$motivo)) stop("Unclassified presidential raw rows")
  detail_columns <- c("QT_TOTAL_VOTOS_VALIDOS", "QT_VOTOS_NOMINAIS_VALIDOS", "QT_TOTAL_VOTOS_LEG_VALIDOS", "QT_VOTOS_NULOS_TECNICOS")
  d <- presidential_read(official_detail_csv, c(b, detail_columns), "official detail")
  d <- d[presidential_scope(d, election_ids, "official detail")]
  presidential_normalize_keys(d); presidential_count(d, detail_columns, "official detail")
  detail <- d[, .(official_valid_votes = sum(QT_TOTAL_VOTOS_VALIDOS),
    official_nominal_valid = sum(QT_VOTOS_NOMINAIS_VALIDOS), official_party_valid = sum(QT_TOTAL_VOTOS_LEG_VALIDOS),
    technical_official = sum(QT_VOTOS_NULOS_TECNICOS)), by = b]
  if (!data.table::fsetequal(unique(r[, ..b]), detail[, ..b])) stop("Raw and official detail zone keys differ")
  if (any(detail$official_party_valid != 0)) stop("Presidential final legend votes must be zero")
  computed <- r[, .(computed_valid_votes = sum(QT_VOTOS[keep[source_row]]),
    computed_nominal_valid = sum(QT_VOTOS[keep[source_row]]), computed_party_valid = 0,
    technical_computed = sum(QT_VOTOS[motivo == "nulo_tecnico_sem_candidato"]),
    excluded_known_nominal = sum(QT_VOTOS[motivo == "nominal_nao_valido"])), by = b]
  checks <- merge(detail, computed, by = b, all = TRUE)
  checks[, difference := computed_valid_votes - official_valid_votes]
  checks[, reconciled := difference == 0 & computed_nominal_valid == official_nominal_valid &
    computed_party_valid == official_party_valid & technical_computed == technical_official]
  if (anyNA(checks) || any(!checks$reconciled)) stop("Presidential valid/nominal/legend/technical zone reconciliation failed")
  excluded <- r[!keep[source_row], .(linhas = .N, votos_excluidos = sum(QT_VOTOS)), by = .(NR_VOTAVEL, motivo)]
  data.table::setorder(excluded, NR_VOTAVEL, motivo)
  list(keep = keep, scope = selected, checks = checks, excluidos = excluded, candidate_checks = candidate_check,
    metadata = list(year = 2026L, round = 1L, office = 1L, election_ids = election_ids, exterior = "excluded ZZ",
      method = "Official candidate municipality/zone all-or-zero nominal validity; unknown numbers reconcile exactly to technical nulls; every final zone total/nominal/zero-legend count matches; no allocation"))
}

# Integration API: caller has already selected ordinary election type 2.
# The election ID is obtained from that raw scope, never inferred independently
# in each official file. Header/count checks remain in the independent core.
filtrar_validade_presidencial <- function(raw, candidatos, detalhe, ano = 2026L, turno = 1L, cargo = 1L) {
  if (length(ano) != 1L || length(turno) != 1L || length(cargo) != 1L ||
      is.na(ano) || is.na(turno) || is.na(cargo) || ano != 2026L || turno != 1L || cargo != 1L)
    stop("This helper supports only 2026 first-round presidential validity")
  required <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "CD_CARGO", "SG_UF")
  if (!is.data.frame(raw) || !all(required %in% names(raw))) stop("Missing required raw scope columns: ", paste(setdiff(required, names(raw)), collapse = ", "))
  selected <- raw$ANO_ELEICAO == ano & raw$NR_TURNO == turno & raw$CD_CARGO == cargo & toupper(as.character(raw$SG_UF)) != "ZZ"
  if (anyNA(selected)) stop("Missing raw presidential scope")
  if ("CD_TIPO_ELEICAO" %in% names(raw) && any(is.na(raw$CD_TIPO_ELEICAO[selected]) | raw$CD_TIPO_ELEICAO[selected] != 2L))
    stop("Presidential raw input must be filtered to ordinary election type 2")
  ids <- suppressWarnings(as.numeric(as.character(unique(raw$CD_ELEICAO[selected]))))
  if (length(ids) != 1L || is.na(ids) || !is.finite(ids) || ids <= 0 || ids %% 1 != 0 || ids > .Machine$integer.max)
    stop("Expected one explicit ordinary presidential election ID in raw scope")
  validade_presidencial(raw, candidatos, detalhe, election_ids = as.integer(ids))
}
