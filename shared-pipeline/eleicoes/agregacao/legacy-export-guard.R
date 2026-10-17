# Read-only consumer of LEGACY schema 1 certificates. No driver/helper sourcing.
eleicao_legacy_protocol <- "legacy-canonic-v1"
eleicao_legacy_keys <- c("year", "round", "election_id", "office", "uf")
eleicao_legacy_sources <- c(
  "shared-pipeline/eleicoes/historico/prepare-legacy-all-ufs.R",
  "shared-pipeline/eleicoes/agregacao/validacao/legal-2010-2014.R",
  "shared-pipeline/eleicoes/agregacao/validacao/legacy-2010-2014-environment.R",
  "shared-pipeline/eleicoes/historico/tse.R")

eleicao_legacy_path <- function(path, must_work = TRUE) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) stop("LEGACY: invalid path")
  result <- normalizePath(path, winslash = "/", mustWork = must_work)
  if (.Platform$OS.type == "windows") tolower(result) else result
}

eleicao_legacy_signature <- function(frame, keys = eleicao_legacy_keys) {
  frame <- as.data.frame(frame)
  if (!nrow(frame)) return(character())
  if (!all(keys %in% names(frame)) || anyNA(frame[, keys, drop = FALSE])) stop("LEGACY: incomplete contest identity")
  signature <- do.call(paste, c(frame[, keys, drop = FALSE], sep = ":"))
  if (anyDuplicated(signature)) stop("LEGACY: duplicate contest identity")
  signature
}

eleicao_legacy_same_table <- function(left, right, columns) {
  normalize <- function(x) {
    x <- as.data.frame(x)
    if (!nrow(x)) return(data.frame())
    if (!all(columns %in% names(x))) stop("LEGACY: checkpoint table incomplete")
    x <- x[order(eleicao_legacy_signature(x)), columns, drop = FALSE]
    x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    rownames(x) <- NULL; x
  }
  identical(normalize(left), normalize(right))
}

eleicao_legacy_read_cache <- function(path) {
  before <- unname(tools::md5sum(path)); votes <- readRDS(path); after <- unname(tools::md5sum(path))
  if (anyNA(c(before, after)) || !identical(before, after)) stop("LEGACY: RDS changed while being read")
  list(votes = votes, md5 = after)
}

eleicao_legacy_certificate <- function(votes, cache_path, cache_dir, year, uf, cache_md5 = NULL) {
  year <- as.integer(year); uf <- toupper(uf)
  if (!year %in% c(2010L, 2014L) || !uf %in% unname(eleicao_ufs)) stop("LEGACY: unsupported requested scope")
  expected_path <- file.path(cache_dir, paste0("places-", year, "-", uf, "-3_5_6_7_8-legal.rds"))
  selected_path <- eleicao_legacy_path(cache_path)
  if (!identical(selected_path, eleicao_legacy_path(expected_path)) ||
      !identical(dirname(selected_path), eleicao_legacy_path(cache_dir))) stop("LEGACY: cache must be the direct canonical output")
  checkpoint_path <- file.path(cache_dir, "status", as.character(year), paste0(uf, ".json"))
  if (!file.exists(checkpoint_path)) stop("LEGACY: canonical checkpoint missing")
  checkpoint_md5 <- unname(tools::md5sum(checkpoint_path))
  record <- jsonlite::read_json(checkpoint_path, simplifyVector = TRUE)
  if (!is.numeric(record$schema_version) || !isTRUE(record$schema_version == 1L) || !isTRUE(record$status %in% c("complete", "partial")) ||
      !is.numeric(record$year) || !isTRUE(record$year == year) || !identical(record$uf, uf) ||
      length(record$run_id) != 1L || !grepl("^[0-9A-Za-z.-]+$", record$run_id) ||
      !identical(eleicao_legacy_path(record$output), selected_path)) stop("LEGACY: checkpoint schema/status/scope invalid")
  fingerprints <- as.data.frame(record$fingerprints)
  if (!nrow(fingerprints) || !all(c("path", "bytes", "md5") %in% names(fingerprints)) || anyNA(fingerprints) ||
      any(!is.finite(fingerprints$bytes)) || any(fingerprints$bytes < 0) ||
      any(!grepl("^[a-fA-F0-9]{32}$", fingerprints$md5))) stop("LEGACY: incomplete fingerprints")
  fingerprint_paths <- vapply(fingerprints$path, eleicao_legacy_path, character(1))
  if (anyDuplicated(fingerprint_paths) || !selected_path %in% fingerprint_paths ||
      !all(vapply(eleicao_legacy_sources, eleicao_legacy_path, character(1)) %in% fingerprint_paths)) stop("LEGACY: unbound output or implementation fingerprint")
  if (!is.null(cache_md5) && !identical(cache_md5, fingerprints$md5[[match(selected_path, fingerprint_paths)]])) stop("LEGACY: loaded RDS differs from checkpoint output")
  if (any(unname(file.info(fingerprints$path)$size) != fingerprints$bytes) ||
      any(unname(tools::md5sum(fingerprints$path)) != fingerprints$md5)) stop("LEGACY: stale checkpoint fingerprints")
  audit <- attr(votes, "vote_audit")
  if (!identical(attr(votes, "normalizer_version"), 2L) || is.null(audit) ||
      !isTRUE(audit$status %in% c("reconciled", "unavailable"))) stop("LEGACY: canonical audited cache required")
  raw <- which(basename(fingerprints$path) == paste0("votacao_secao_", year, "_", uf, ".zip"))
  prefixes <- c("votacao_candidato_munzona", "votacao_partido_munzona", "detalhe_votacao_munzona")
  if (length(raw) != 1L || !identical(attr(votes, "source_md5"), fingerprints$md5[[raw]]) ||
      !identical(audit$provenance$raw_md5, attr(votes, "source_md5")) ||
      !all(paste0(prefixes, "_", year, ".zip") %in% basename(fingerprints$path)) ||
      !setequal(names(audit$provenance$official_members), prefixes)) stop("LEGACY: raw/official provenance mismatch")
  staging <- eleicao_legacy_path(record$staging)
  if (!startsWith(staging, paste0(eleicao_legacy_path(cache_dir), "/staging/"))) stop("LEGACY: selected-member staging outside canonical tree")
  for (prefix in prefixes) {
    member <- audit$provenance$official_members[[prefix]]
    member_path <- eleicao_legacy_path(file.path(record$staging, "extracted", paste0(prefix, "_", year), member$member))
    index <- match(member_path, fingerprint_paths)
    if (!startsWith(member_path, paste0(staging, "/")) || is.na(index) ||
        !identical(member$csv_md5, fingerprints$md5[[index]])) stop("LEGACY: selected official member not bound to checkpoint")
  }
  expected <- as.data.frame(audit$expected_contests)
  checks <- as.data.frame(audit$checks); blocked <- as.data.frame(audit$unavailable)
  e <- eleicao_legacy_signature(expected); a <- eleicao_legacy_signature(checks); b <- eleicao_legacy_signature(blocked)
  if (!length(e) || length(intersect(a, b)) || !setequal(e, c(a, b))) stop("LEGACY: incomplete/overlapping expected partition")
  if (any(expected$year != year) || any(expected$uf != uf) ||
      any(!expected$office %in% c(3L, 5L, 6L, 7L, 8L)) ||
      any(!expected$round %in% c(1L, 2L)) || any(expected$round == 2L & expected$office != 3L) ||
      any(expected$office == 8L & uf != "DF") || any(expected$office == 7L & uf == "DF") ||
      any(!is.finite(expected$election_id)) || any(expected$election_id <= 0) || any(expected$election_id %% 1 != 0)) stop("LEGACY: unexpected ordinary contest scope")
  # A target without election_id must identify exactly one ordinary election.
  eleicao_legacy_signature(expected, setdiff(eleicao_legacy_keys, "election_id"))
  fields <- c("official_valid_votes", "computed_valid_votes", "difference", "reconciled")
  if (nrow(checks) && (!all(fields %in% names(checks)) || anyNA(checks[, fields, drop = FALSE]) ||
      any(!is.finite(checks$computed_valid_votes)) || any(checks$computed_valid_votes <= 0) ||
      any(checks$difference != 0) || any(checks$computed_valid_votes != checks$official_valid_votes) ||
      !is.logical(checks$reconciled) || any(!checks$reconciled))) stop("LEGACY: positive exact denominator required")
  if (nrow(blocked) && (!"reason" %in% names(blocked) || anyNA(blocked$reason) || any(!nzchar(trimws(blocked$reason))))) stop("LEGACY: unavailable contest missing reason")
  if (!eleicao_legacy_same_table(record$certified, checks, c(eleicao_legacy_keys, fields)) ||
      !eleicao_legacy_same_table(record$unavailable, blocked, c(eleicao_legacy_keys, "reason"))) stop("LEGACY: checkpoint/cache partition differs")
  has_accepted <- nrow(checks) > 0L
  if (!identical(audit$status, if (has_accepted) "reconciled" else "unavailable") ||
      !identical(audit$publishable, has_accepted) || !identical(record$publishable, has_accepted) ||
      !identical(record$audit_status, audit$status) || !identical(record$status, if (nrow(blocked)) "partial" else "complete") ||
      length(record$rows) != 1L || record$rows != nrow(votes)) stop("LEGACY: cache/checkpoint status or row count differs")
  if (!all(c(eleicao_legacy_keys, "votes", "vote_type") %in% names(votes)) || anyNA(votes$votes) ||
      any(!is.finite(votes$votes)) || any(votes$votes < 0) || any(votes$votes %% 1 != 0) ||
      anyNA(votes$vote_type) || any(!votes$vote_type %in% c("candidate", "party"))) stop("LEGACY: invalid normalized votes")
  rows <- as.data.frame(votes)
  if (nrow(rows) && anyNA(rows[, eleicao_legacy_keys, drop = FALSE])) stop("LEGACY: unscoped vote rows")
  row_keys <- if (nrow(rows)) do.call(paste, c(rows[, eleicao_legacy_keys, drop = FALSE], sep = ":")) else character()
  if (any(!row_keys %in% a)) stop("LEGACY: output contains uncertified contest")
  for (i in seq_len(nrow(checks))) if (sum(rows$votes[row_keys == a[[i]]]) != checks$computed_valid_votes[[i]]) stop("LEGACY: RDS contest total differs from certificate")
  if (sum(rows$votes) != sum(checks$computed_valid_votes)) stop("LEGACY: RDS total differs from certificate")
  positive_candidates <- rows[rows$vote_type == "candidate" & rows$votes > 0, , drop = FALSE]
  if (nrow(positive_candidates)) {
    identity <- as.data.frame(audit$candidate_identity)
    identity_keys <- c(eleicao_legacy_keys, "candidate_id")
    if (!all(c(identity_keys, "identified_candidate_uid") %in% names(identity)) ||
        anyNA(identity[, c(identity_keys, "identified_candidate_uid"), drop = FALSE]) ||
        any(!nzchar(as.character(identity$identified_candidate_uid)))) stop("LEGACY: candidate identities incomplete")
    id_scope <- do.call(paste, c(identity[, eleicao_legacy_keys, drop = FALSE], sep = ":"))
    id_keys <- do.call(paste, c(identity[, identity_keys, drop = FALSE], sep = ":"))
    if (any(!id_scope %in% a) || any(vapply(split(as.character(identity$identified_candidate_uid), id_keys), function(x) length(unique(x)) != 1L, logical(1)))) stop("LEGACY: conflicting candidate identities")
    actual_ids <- do.call(paste, c(positive_candidates[, identity_keys, drop = FALSE], sep = ":"))
    if (any(!actual_ids %in% id_keys)) stop("LEGACY: positive candidate absent from certified identities")
  }
  if (!identical(checkpoint_md5, unname(tools::md5sum(checkpoint_path)))) stop("LEGACY: checkpoint changed during validation")
  list(protocol = eleicao_legacy_protocol, checkpoint_md5 = checkpoint_md5,
       checkpoint = checkpoint_path, expected = expected, checks = checks, unavailable = blocked,
       status = record$status, method = audit$method, provenance = audit$provenance,
       fingerprints = rbind(fingerprints[, c("path", "bytes", "md5")], data.frame(
         path = checkpoint_path, bytes = unname(file.info(checkpoint_path)$size), md5 = checkpoint_md5)))
}

# Final read barrier: every unique byte source is hashed once across all UFs,
# including national ZIPs shared by multiple certificates. No manifest may be
# switched if a source, selected CSV, implementation, RDS or checkpoint changed.
eleicao_legacy_verify_snapshot <- function(certificates) {
  if (!length(certificates)) return(invisible(0L))
  fingerprints <- data.table::rbindlist(lapply(certificates, function(x) x$fingerprints), use.names = TRUE)
  if (!nrow(fingerprints)) stop("LEGACY: missing final snapshot fingerprints")
  fingerprints[, canonical_path := vapply(path, eleicao_legacy_path, character(1), must_work = FALSE)]
  identities <- unique(fingerprints[, .(canonical_path, bytes, md5)])
  if (any(identities[, .N, by = canonical_path]$N != 1L)) stop("LEGACY: conflicting expected fingerprints across certificates")
  fingerprints <- fingerprints[!duplicated(canonical_path)]
  actual_bytes <- unname(file.info(fingerprints$path)$size)
  if (anyNA(actual_bytes) || any(actual_bytes != fingerprints$bytes)) stop("LEGACY: snapshot changed before manifest publication")
  actual_md5 <- unname(tools::md5sum(fingerprints$path))
  if (anyNA(actual_md5) || any(actual_md5 != fingerprints$md5)) stop("LEGACY: snapshot changed before manifest publication")
  invisible(nrow(fingerprints))
}

eleicao_legacy_target <- function(certificate, year, round, office, uf) {
  select <- function(frame) {
    frame <- as.data.frame(frame)
    if (!nrow(frame)) return(frame)
    frame[frame$year == year & frame$round == round & frame$office == office & frame$uf == uf, , drop = FALSE]
  }
  blocked <- select(certificate$unavailable)
  if (nrow(blocked)) return(list(available = FALSE, reason = paste0("LEGACY: ", blocked$reason[[1L]])))
  checked <- select(certificate$checks)
  if (nrow(checked) != 1L) return(list(available = FALSE, reason = "LEGACY: no certified ordinary contest for this year/round/office/UF"))
  list(available = TRUE, election_id = checked$election_id[[1L]])
}
