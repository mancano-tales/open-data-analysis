# Export only normalized, spatially supported series. Run from repository root.
# CLI: Rscript .../build-explorer.R [output_dir] [historical_cache] [spatial_cache]
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/agregacao/centis.R", encoding = "UTF-8")
source("shared-pipeline/eleicoes/agregacao/censo-referencia.R", encoding = "UTF-8")
source("shared-pipeline/eleicoes/agregacao/legacy-export-guard.R", encoding = "UTF-8")

eleicao_cache_index <- function(cache_dir) {
  paths <- list.files(cache_dir, pattern = "^places-[0-9]{4}-[A-Z]{2}-[0-9_]+(-legal)?[.]rds$", full.names = TRUE)
  if (!length(paths)) return(data.frame(path = character(), year = integer(), uf = character(), offices = character(), legal = logical()))
  parts <- strsplit(sub("[.]rds$", "", basename(paths)), "-", fixed = TRUE)
  data.frame(path = paths, year = as.integer(vapply(parts, `[[`, "", 2L)),
    uf = vapply(parts, `[[`, "", 3L), offices = vapply(parts, `[[`, "", 4L),
    legal = grepl("-legal[.]rds$", paths), stringsAsFactors = FALSE)
}

eleicao_targets <- function(years = c(2010L, 2014L, 2018L, 2022L, 2026L),
                            rounds = c(1L, 2L), state_years = years,
                            ufs = unname(eleicao_ufs), state_offices = c(3L, 5L, 6L, 7L, 8L),
                            income_references = "national") {
  pres <- expand.grid(year = as.integer(years), round = as.integer(rounds), office = 1L,
                       uf = c("BR", ufs), stringsAsFactors = FALSE)
  state <- expand.grid(year = as.integer(intersect(years, state_years)), round = as.integer(rounds),
                        office = as.integer(state_offices), uf = ufs, stringsAsFactors = FALSE)
  state <- state[(state$office != 8L | state$uf == "DF") &
    (state$office != 7L | state$uf != "DF") & (state$round == 1L | state$office == 3L), , drop = FALSE]
  base <- data.table::as.data.table(unique(rbind(pres, state)))
  if (!length(income_references) || any(!income_references %in% c("national", "state"))) stop("Invalid income references")
  data.table::rbindlist(lapply(unique(income_references), function(reference) {
    selected <- data.table::copy(base[reference != "state" | uf != "BR"])
    selected[, income_reference := reference]
    selected
  }))
}

eleicao_target_id <- function(year, round, office, uf, income_reference = "national") {
  paste0(paste(year, round, office, uf, sep = "-"), ifelse(income_reference == "state", "-state", ""))
}

eleicao_pick_cache <- function(index, year, office, uf) {
  match <- index[index$year == year & index$uf %in% if (office == 1L) c("BR", uf) else uf, , drop = FALSE]
  if (!nrow(match)) return(NULL)
  codes <- strsplit(match$offices, "_", fixed = TRUE)
  keep <- vapply(codes, function(x) as.character(office) %in% x, logical(1))
  match <- match[keep, , drop = FALSE]; codes <- codes[keep]
  if (!nrow(match)) return(NULL)
  # Prefer a national presidential source and the smallest matching office cache.
  preference <- if (office == 1L) match$uf != "BR" else rep(FALSE, nrow(match))
  match[order(!match$legal, preference, lengths(codes), basename(match$path)), , drop = FALSE][1L, , drop = FALSE]
}

eleicao_manifest_parties <- function(cache_dir, years) {
  result <- list()
  for (year in sort(unique(years))) {
    zip <- file.path(cache_dir,paste0("votacao_candidato_munzona_",year,".zip"))
    if (!file.exists(zip)) stop("Official candidate ZIP missing for party labels: ",zip)
    members <- unzip(zip,list=TRUE)$Name
    members <- members[grepl(paste0("(^|/)votacao_candidato_munzona_",year,"_[A-Z]{2}[.]csv$"),members)]
    if (!length(members)) stop("Official candidate tables missing in ",zip)
    scratch <- tempfile(paste0("parties-",year,"-")); dir.create(scratch)
    annual <- tryCatch(data.table::rbindlist(lapply(members,function(member) {
      unpacked <- unzip(zip,files=member,exdir=scratch,overwrite=FALSE)
      path <- file.path(scratch,member)
      if (!file.exists(path)) stop("Could not extract candidate table: ",member)
      rows <- data.table::fread(path,select=c("NR_PARTIDO","SG_PARTIDO"),encoding="Latin-1")
      unlink(path)
      rows
    }),use.names=TRUE),finally=unlink(scratch,recursive=TRUE))
    rows <- unique(annual[!is.na(NR_PARTIDO) & !is.na(SG_PARTIDO) & nzchar(SG_PARTIDO),
      .(number=as.character(NR_PARTIDO),acronym=as.character(SG_PARTIDO))])
    # A party renamed during the year (e.g. 51 in 2018) keeps every official
    # acronym; labels only, matching always uses the number.
    rows <- rows[, .(acronym=paste(sort(unique(acronym)),collapse="/")),by=number]
    result[[as.character(year)]] <- stats::setNames(as.list(rows$acronym),rows$number)
  }
  result
}

eleicao_build_explorer <- function(output_dir = "posts/explorador-eleitoral/data",
    cache_dir = "data-raw/eleicoes-renda/historico",
    spatial_dir = "data-raw/eleicoes-renda/espacial",
    income_path = "data-raw/eleicoes-renda/renda2022.rds",
    targets = NULL, max_distance_m = 5000, incremental = FALSE, flush_progress = FALSE,
    legacy_cache_dir = NULL) {
  eleicao_require(); data.table::setDTthreads(2L)
  if (is.null(targets)) targets <- eleicao_targets(income_references = c("national", "state"))
  targets <- data.table::copy(data.table::as.data.table(targets))
  if (!"income_reference" %in% names(targets)) targets[, income_reference := "national"]
  if (!"census_year" %in% names(targets)) targets[, census_year := 2022L]
  targets <- unique(targets[, .(year, round, office, uf, income_reference, census_year)])
  targets[, `:=`(year = as.integer(year), round = as.integer(round), office = as.integer(office), uf = toupper(uf), census_year = as.integer(census_year))]
  data.table::setorder(targets, year, round, office, uf)
  if (anyNA(targets) || any(!targets$uf %in% c("BR", unname(eleicao_ufs))) ||
      any(!targets$income_reference %in% c("national", "state")) ||
      any(targets$income_reference == "state" & targets$uf == "BR") ||
      any(!vapply(seq_len(nrow(targets)), function(i) eleicao_censo_allowed(targets$census_year[i],targets$year[i]), logical(1)))) stop("Invalid targets")
  index <- eleicao_cache_index(cache_dir)
  if (!is.null(legacy_cache_dir) && (length(legacy_cache_dir) != 1L || is.na(legacy_cache_dir) || !nzchar(legacy_cache_dir))) stop("Invalid explicit LEGACY cache directory")
  legacy_index <- if (!is.null(legacy_cache_dir)) eleicao_cache_index(legacy_cache_dir) else NULL
  if (!is.null(legacy_index)) legacy_index <- legacy_index[legacy_index$year %in% c(2010L, 2014L) &
    legacy_index$legal & legacy_index$offices == "3_5_6_7_8", , drop = FALSE]
  is_legacy <- function(year, office) !is.null(legacy_cache_dir) && year %in% c(2010L, 2014L) && office != 1L
  legacy_snapshot_required <- !is.null(legacy_cache_dir) && any(targets$year %in% c(2010L, 2014L) & targets$office != 1L)
  select_cache <- function(year, office, uf) eleicao_pick_cache(if (is_legacy(year, office)) legacy_index else index, year, office, uf)
  # Read each potentially large vote cache once per year/round, not per office.
  targets[, processing_source := vapply(seq_len(.N), function(i) {
    selected <- select_cache(year[[i]], office[[i]], uf[[i]])
    if (is.null(selected)) "" else selected$path[[1L]]
  }, character(1))]
  data.table::setorder(targets, year, round, processing_source, office, uf)
  universes <- new.env(parent=emptyenv())
  income_paths <- c(`2022`=income_path,`2010`="data-raw/eleicoes-renda/censo2010/renda2010-basico.rds")
  sources <- list(
    list(label = "TSE — resultados por seção", url = "https://dadosabertos.tse.jus.br/"),
    list(label = "Censo 2022 — renda dos responsáveis, censobr", url = "https://ipeagit.github.io/censobr/"),
    list(label = "Coordenadas dos locais, geobr/TSE", url = "https://ipeagit.github.io/geobr/"))
  manifest_path <- file.path(output_dir,"manifest.json")
  prior <- if(incremental && file.exists(manifest_path)) jsonlite::read_json(manifest_path) else NULL
  index_items <- function(items) {
    if(!length(items)) return(list())
    ids <- vapply(items,function(item)item$id,character(1))
    if(anyDuplicated(ids)) stop("Duplicate manifest identity")
    stats::setNames(items,ids)
  }
  series <- index_items(prior$series); availability <- index_items(prior$availability)
  valid_scope <- eleicao_targets(income_references = c("national", "state"))
  valid_ids <- with(valid_scope,eleicao_target_id(year,round,office,uf,income_reference))
  valid_ids <- c(valid_ids, paste0(valid_ids[grepl("^2010-|^2014-",valid_ids)],"-c2010"))
  series <- series[names(series) %in% valid_ids]
  availability <- availability[names(availability) %in% valid_ids]
  party_years <- unique(c(targets$year,vapply(series,function(item) as.integer(item$year),integer(1))))
  party_maps <- eleicao_manifest_parties(cache_dir,party_years)
  write_manifest <- function(manifest) {
    manifest$parties <- party_maps
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    temporary <- paste0(manifest_path,".part")
    jsonlite::write_json(manifest,temporary,auto_unbox=TRUE,dataframe="rows",
      null="null",na="null",pretty=TRUE,digits=10)
    if(!file.copy(temporary,manifest_path,overwrite=TRUE)) stop("Cannot update manifest")
    unlink(temporary)
    frame <- if(length(manifest$availability)) data.table::rbindlist(manifest$availability,fill=TRUE) else data.table::data.table()
    data.table::fwrite(frame,file.path(output_dir,"availability.csv"))
  }
  link_hash <- vote_hash <- NULL
  last_link_path <- NULL; links <- NULL; link_error <- NULL
  last_vote_path <- NULL; votes <- NULL; vote_error <- NULL
  # Certificate records are small; large vote frames retain the existing
  # one-source-at-a-time lifecycle. Hash ZIPs/selected CSVs once per UF per
  # invocation, binding reuse to the current checkpoint and RDS digests.
  legacy_certificates <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(targets))) {
    target <- targets[i]; yr <- target$year; tr <- target$round; oc <- target$office; state <- target$uf
    reference <- target$income_reference; census_year <- target$census_year
    census <- eleicao_censo(census_year)
    current_sources <- sources
    if (census_year != 2022L) current_sources[[2L]]$label <- "Censo 2010 — renda dos responsáveis, censobr"
    current_income_path <- unname(income_paths[as.character(census_year)])
    if (!exists(as.character(census_year),envir=universes,inherits=FALSE))
      assign(as.character(census_year),eleicao_reference_universe(readRDS(current_income_path),census_year),envir=universes)
    universe <- get(as.character(census_year),envir=universes)
    income_hash <- unname(tools::md5sum(current_income_path))
    id <- eleicao_censo_series_id(eleicao_target_id(yr, tr, oc, state, reference),census_year)
    status <- "unavailable"; reason <- NULL; item <- NULL
    selected <- select_cache(yr, oc, state)
    legacy <- is_legacy(yr, oc); legacy_context <- NULL; legacy_error <- NULL
    if (legacy && !is.null(selected)) {
      if (!identical(last_vote_path, selected$path)) {
        votes <- NULL; invisible(gc())
        vote_result <- tryCatch(list(value = eleicao_legacy_read_cache(selected$path), error = NULL), error = function(e) list(value = NULL, error = conditionMessage(e)))
        votes <- vote_result$value$votes; vote_hash <- vote_result$value$md5
        vote_error <- vote_result$error; last_vote_path <- selected$path
      }
      if (!is.null(vote_error)) legacy_error <- paste("LEGACY: falha ao ler votos:", vote_error)
      else {
        checkpoint_path <- file.path(legacy_cache_dir, "status", as.character(yr), paste0(state, ".json"))
        checkpoint_hash <- if (file.exists(checkpoint_path)) unname(tools::md5sum(checkpoint_path)) else NA_character_
        key <- selected$path[[1L]]
        certificate <- if (exists(key, envir = legacy_certificates, inherits = FALSE)) get(key, envir = legacy_certificates) else NULL
        if (is.null(certificate) || !identical(certificate$checkpoint_md5, checkpoint_hash) || !identical(certificate$vote_md5, vote_hash)) {
          checked <- tryCatch({
            if (!is.null(certificate) && !identical(certificate$checkpoint_md5, checkpoint_hash)) {
              # A new checkpoint can accompany a replaced canonical file at the
              # same path. Never certify the preceding in-memory vote frame.
              loaded <- eleicao_legacy_read_cache(key); votes <- loaded$votes; vote_hash <- loaded$md5
            }
            list(value = eleicao_legacy_certificate(votes, key, legacy_cache_dir, yr, state, cache_md5 = vote_hash), error = NULL)
          },
            error = function(e) list(value = NULL, error = conditionMessage(e)))
          certificate <- list(value = checked$value, error = checked$error, checkpoint_md5 = checkpoint_hash, vote_md5 = vote_hash)
          assign(key, certificate, envir = legacy_certificates)
        }
        if (!is.null(certificate$error)) legacy_error <- certificate$error
        else {
          legacy_context <- certificate$value
          legacy_target <- eleicao_legacy_target(legacy_context, yr, tr, oc, state)
          if (!legacy_target$available) legacy_error <- legacy_target$reason
        }
      }
    }
    link_path <- file.path(spatial_dir, paste0("links-", yr, "-", tr,
      if(census_year == 2022L) "" else paste0("-c",census_year), ".rds"))
    if (is.null(selected)) {
      reason <- if (legacy) "LEGACY: sem cache canônico isolado para esta disputa; nenhum fallback à raiz histórica" else "Sem cache normalizado de votação por local para esta disputa"
    } else if (!is.null(legacy_error)) {
      reason <- legacy_error
    } else if (!file.exists(link_path)) {
      reason <- if (yr < 2010L) "Sem georreferenciamento contemporâneo disponível antes de 2010" else "Mapa setorial completo do ano/turno ainda não disponível; fragmentos por UF não são apresentados como cobertura nacional"
    } else {
      if (!identical(last_link_path, link_path)) {
        links <- NULL; invisible(gc())
        link_result <- tryCatch(list(value = readRDS(link_path), error = NULL), error = function(e) list(value = NULL, error = conditionMessage(e)))
        links <- link_result$value; link_error <- link_result$error; last_link_path <- link_path
        link_hash <- unname(tools::md5sum(link_path))
      }
      if (!identical(last_vote_path, selected$path)) {
        votes <- NULL; invisible(gc())
        vote_result <- tryCatch(list(value = readRDS(selected$path), error = NULL), error = function(e) list(value = NULL, error = conditionMessage(e)))
        votes <- vote_result$value; vote_error <- vote_result$error; last_vote_path <- selected$path
        vote_hash <- unname(tools::md5sum(selected$path))
      }
      if (!is.null(link_error)) reason <- paste("Falha ao ler mapa:", link_error)
      else if (!is.null(vote_error)) reason <- paste("Falha ao ler votos:", vote_error)
      else if (!identical(attr(votes, "normalizer_version"), 2L)) reason <- "Cache requer normalizador versão 2 antes da exportação"
      else {
        outcome <- tryCatch({
          name <- paste0(id, ".json")
          destination <- file.path(output_dir,"series",name)
          fingerprint_version <- if (reference == "state") "state-income-valid-v1" else "fixed-income-valid-v1"
          if (oc %in% c(6L, 7L, 8L)) fingerprint_version <- paste0(fingerprint_version, "-party")
          fingerprint <- paste(fingerprint_version,income_hash,link_hash,vote_hash,max_distance_m,id,sep=":")
          if (legacy) fingerprint <- paste(fingerprint, legacy_context$protocol, legacy_context$checkpoint_md5, sep = ":")
          cached <- if(file.exists(destination))tryCatch(jsonlite::read_json(destination),error=function(e)NULL) else NULL
          if(!is.null(cached) && identical(cached$metadata$export_fingerprint,fingerprint) &&
             identical(cached$metadata$vote_audit$status,"reconciled") &&
             !"ZZ" %in% cached$metadata$vote_audit$ufs && (!legacy ||
               (identical(cached$metadata$vote_cache_protocol, legacy_context$protocol) &&
                identical(cached$metadata$vote_checkpoint_md5, legacy_context$checkpoint_md5)))) {
            item <- eleicao_manifest_item(cached,paste0("series/",name))
          } else {
            dataset <- eleicao_aggregate_contest(votes, universe, links, yr, tr, oc, state,
              max_distance_m = max_distance_m, sources = current_sources, income_reference = reference,
              census_year = census_year, include_party_series = oc %in% c(6L, 7L, 8L))
            dataset$metadata$vote_source_md5 <- attr(votes, "source_md5")
            dataset$metadata$vote_cache <- basename(selected$path)
            dataset$metadata$spatial <- attr(links, "metadata")
            dataset$metadata$export_fingerprint <- fingerprint
            if (oc %in% c(6L, 7L, 8L)) dataset$metadata$party_series_complete <- TRUE
            if (legacy) {
              dataset$metadata$vote_cache_protocol <- legacy_context$protocol
              dataset$metadata$vote_checkpoint_md5 <- legacy_context$checkpoint_md5
              dataset$metadata$vote_certificate <- list(protocol = legacy_context$protocol,
                checkpoint = paste0("status/", yr, "/", state, ".json"), checkpoint_md5 = legacy_context$checkpoint_md5,
                status = legacy_context$status, election_id = legacy_target$election_id,
                certified_contests = nrow(legacy_context$checks), unavailable_contests = nrow(legacy_context$unavailable),
                official_members = legacy_context$provenance$official_members)
            }
            item <- eleicao_write_dataset(dataset,destination,paste0("series/",name))
            rm(dataset);invisible(gc())
          }
          list(item = item, error = NULL)
        }, error = function(e) list(item = NULL, error = conditionMessage(e)))
        item <- outcome$item; reason <- outcome$error
        if (!is.null(item)) {
          series[[id]] <- item; status <- "available"
          message("Exported ", id)
        }
      }
    }
    availability[[id]] <- list(id = id, year = yr, round = tr, office = as.character(oc), uf = state, income_reference = reference,
      census_year = census_year,
      office_label = unname(eleicao_office_labels[as.character(oc)]), status = status,
      reason = if (is.null(reason)) "Dados processados com universo de renda fixo e cobertura documentada" else reason)
    if(status!="available") series[[id]] <- NULL
    boundary <- i==nrow(targets) || targets$processing_source[[i+1L]]!=targets$processing_source[[i]] ||
      targets$year[[i+1L]]!=yr || targets$round[[i+1L]]!=tr
    if(flush_progress && boundary && !legacy_snapshot_required) {
      progress_metadata <- if(!is.null(prior$metadata))prior$metadata else list(percentile_scope="BR",income_year=2022L,sources=sources)
      write_manifest(list(schema_version=1L,series=unname(series),availability=unname(availability),metadata=progress_metadata))
      message("Manifest updated: ",length(series)," available series")
    }
  }
  if (legacy_snapshot_required) {
    certificates <- lapply(as.list(legacy_certificates), function(x) x$value)
    certificates <- Filter(Negate(is.null), certificates)
    # Defer progress manifests for explicit LEGACY batches so a late snapshot
    # failure preserves the previous incremental manifest byte for byte.
    eleicao_legacy_verify_snapshot(certificates)
  }
  manifest <- list(schema_version = 1L, series = unname(series), availability = unname(availability),
    metadata = if(incremental && !is.null(prior$metadata)) prior$metadata else list(percentile_scope = if(any(vapply(series,function(item)identical(item$income_reference,"state"),logical(1)))) "selectable" else "BR", income_year = 2022L,
      income_references = c("national", "state"),
      method = "Setores com renda do Censo 2022 ordenados no Brasil ou na UF escolhida antes das exclusões; média simples de percentuais eleitorais atribuídos pelo local mais próximo no mesmo município.",
      income_definition = "V06004, renda nominal média dos responsáveis; salários mínimos de 2022 (R$ 1.212).",
      sources = sources,
      coverage = list(total_eligible_tracts = nrow(universe)),
      limitations = c("Associação territorial não identifica a renda individual nem o local onde cada morador votou.",
        "A renda é de 2022 também nas eleições anteriores e posteriores.",
        "Georreferenciamento contemporâneo pesquisado a partir de 2010; ausência de mapa impede publicação da série.",
        "O limite de distância produz exclusões seletivas; comparar cobertura e setores comuns entre anos.",
        "Votos por seção exigem reconciliação com totais oficiais para certificar a situação jurídica do denominador.",
        "Senadores: porcentagem dos votos do cargo, não dos eleitores; pode haver dois votos por eleitor.",
        "Deputados: votos de legenda entram no denominador, sem integrar as séries individuais.",
        "Estimador independente; detalhes publicados não permitem certificar reprodução exata da Folha.")))
  write_manifest(manifest)
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  eleicao_build_explorer(output_dir = if (length(args)) args[[1L]] else "posts/explorador-eleitoral/data",
    cache_dir = if (length(args) >= 2L) args[[2L]] else "data-raw/eleicoes-renda/historico",
    spatial_dir = if (length(args) >= 3L) args[[3L]] else "data-raw/eleicoes-renda/espacial")
}
