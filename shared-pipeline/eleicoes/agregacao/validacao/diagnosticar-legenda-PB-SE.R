# Local-only diagnosis: no downloads, no changes to legal caches.
suppressPackageStartupMessages(library(data.table))
setDTthreads(2L)
cache <- "data-raw/eleicoes-renda/historico"
output <- file.path(cache, "diagnostico-legenda")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
find_csv <- function(prefix, uf) {
  catalog <- file.path(cache, "candidate-catalog-2026", paste0(uf, ".csv"))
  if (prefix == "votacao_candidato_munzona" && file.exists(catalog)) return(catalog)
  root <- file.path(cache, paste0(prefix, "_2026"))
  found <- list.files(root, pattern = paste0("_2026_", uf, "[.]csv$"), full.names = TRUE, recursive = TRUE)
  if (length(found) != 1L) stop("Expected local CSV for ", prefix, "/", uf)
  found[[1L]]
}
for (state in c("PB", "SE")) {
  message("Diagnosing ", state)
  raw <- readRDS(file.path(cache, paste0("places-2026-", state, "-3_5_6_7_8.rds")))
  raw <- as.data.table(raw)
  raw <- raw[office %in% c(6L, 7L, 8L)]
  keys <- c("year", "round", "election_id", "uf", "municipality_tse", "zone", "office", "candidate_id")
  actual <- raw[vote_type == "party", .(section_party = sum(votes)), by = keys]
  path <- find_csv("votacao_partido_munzona", state)
  header <- names(fread(path, nrows = 0L, encoding = "Latin-1"))
  writeLines(header, file.path(output, paste0(state, "-party-columns.txt")))
  columns <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "CD_CARGO", "NR_PARTIDO",
               "QT_VOTOS_LEGENDA_VALIDOS", "QT_VOTOS_LEGENDA_ANUL_SUBJUD", "QT_VOTOS_LEGENDA_ANULADOS")
  official <- fread(path, select = columns, encoding = "Latin-1")
  setnames(official, columns, c(keys, "valid", "pending", "void"))
  official[, municipality_tse := as.character(municipality_tse)]
  official <- official[office %in% c(6L, 7L, 8L), .(official_party = sum(as.numeric(valid) + pending + void),
     official_valid_party = sum(as.numeric(valid)), official_pending_party = sum(as.numeric(pending)), official_void_party = sum(as.numeric(void))), by = keys]
  comparison <- merge(actual, official, by = keys, all = TRUE)
  comparison[is.na(section_party), section_party := 0]
  comparison[, difference := section_party - official_party]
  mismatches <- comparison[is.na(official_party) | difference != 0]
  candpath <- find_csv("votacao_candidato_munzona", state)
  cc <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "CD_CARGO", "NR_CANDIDATO",
          "NR_PARTIDO", "NM_TIPO_DESTINACAO_VOTOS", "QT_VOTOS_NOMINAIS", "QT_VOTOS_NOMINAIS_VALIDOS")
  candidates <- fread(candpath, select = cc, encoding = "Latin-1")
  setnames(candidates, cc, c(keys, "party_id", "destination", "nominal", "nominal_valid"))
  candidates[, municipality_tse := as.character(municipality_tse)]
  converted <- candidates[office %in% c(6L, 7L, 8L) & grepl("legenda", destination, ignore.case = TRUE) & nominal_valid == 0,
    .(converted_nominal = sum(as.numeric(nominal))), by = c(keys[1:7], "party_id")]
  setnames(converted, "party_id", "candidate_id")
  comparison <- merge(comparison, converted, by = keys, all.x = TRUE)
  comparison[is.na(converted_nominal), converted_nominal := 0]
  comparison[, residual_after_conversion := section_party + converted_nominal - official_party]
  comparison[, classification := fifelse(is.na(official_party), "party_absent_official",
    fifelse(difference == 0, "matches",
    fifelse(residual_after_conversion == 0, "nominal_conversion_exact", "other")))]
  # Confirm whether unmatched two-digit ballots are technical nulls, using
  # an independent official zone/cargo total. Never move these to valid votes.
  basekeys <- keys[1:7]
  nominal <- raw[vote_type == "candidate", .(nominal_raw = sum(votes)), by = keys]
  known_candidates <- unique(candidates[, c(keys, "nominal"), with = FALSE])
  known_candidates <- known_candidates[, .(nominal_official = sum(nominal)), by = keys]
  nominal <- merge(nominal, known_candidates, by = keys, all.x = TRUE)
  technical_nominal <- nominal[is.na(nominal_official), .(nominal_technical = sum(nominal_raw)), by = basekeys]
  technical_party <- comparison[classification == "party_absent_official", .(party_technical = sum(section_party)), by = basekeys]
  detailpath <- find_csv("detalhe_votacao_munzona", state)
  dc <- c("ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "CD_CARGO", "QT_VOTOS_NULOS_TECNICOS")
  detail <- fread(detailpath, select = dc, encoding = "Latin-1")
  setnames(detail, dc, c(basekeys, "technical_official"))
  detail[, municipality_tse := as.character(municipality_tse)]
  detail <- detail[office %in% c(6L, 7L, 8L), .(technical_official = sum(as.numeric(technical_official))), by = basekeys]
  technical <- merge(detail, technical_nominal, by = basekeys, all = TRUE)
  technical <- merge(technical, technical_party, by = basekeys, all = TRUE)
  technical[is.na(nominal_technical), nominal_technical := 0]
  technical[is.na(party_technical), party_technical := 0]
  technical[, residual := nominal_technical + party_technical - technical_official]
  fwrite(technical, file.path(output, paste0(state, "-technical-null-by-zone.csv")))
  print(technical[, .(zones = .N, nominal_technical = sum(nominal_technical), party_technical = sum(party_technical),
    technical_official = sum(technical_official), mismatched_zones = sum(is.na(residual) | residual != 0)), by = office])
  print(comparison[classification == "party_absent_official", .(zones = .N, votes = sum(section_party)), by = .(office, candidate_id)])
  stopifnot(!anyNA(technical$residual), all(technical$residual == 0))
  fwrite(comparison[classification != "matches"], file.path(output, paste0(state, "-party-mismatches.csv")))
  summary <- comparison[, .(keys = .N, section_party = sum(section_party),
     official_party = sum(official_party, na.rm = TRUE), converted_nominal = sum(converted_nominal),
     difference = sum(difference, na.rm = TRUE)), by = .(office, classification)]
  fwrite(summary, file.path(output, paste0(state, "-summary.csv")))
  print(summary)
  print(head(comparison[classification != "matches"], 20L))
  message("Nonzero mismatched keys: ", nrow(mismatches))
  rm(raw, actual, official, comparison, candidates, converted, mismatches)
  invisible(gc())
}
