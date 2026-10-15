# Export only normalized, spatially supported series. Run from repository root.
# CLI: Rscript .../build-explorer.R [output_dir] [historical_cache] [spatial_cache]
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/agregacao/centis.R", encoding = "UTF-8")

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

eleicao_build_explorer <- function(output_dir = "posts/explorador-eleitoral/data",
    cache_dir = "data-raw/eleicoes-renda/historico",
    spatial_dir = "data-raw/eleicoes-renda/espacial",
    income_path = "data-raw/eleicoes-renda/renda2022.rds",
    targets = NULL, max_distance_m = 5000, incremental = FALSE, flush_progress = FALSE) {
  eleicao_require(); data.table::setDTthreads(2L)
  if (is.null(targets)) targets <- eleicao_targets(income_references = c("national", "state"))
  targets <- data.table::copy(data.table::as.data.table(targets))
  if (!"income_reference" %in% names(targets)) targets[, income_reference := "national"]
  targets <- unique(targets[, .(year, round, office, uf, income_reference)])
  targets[, `:=`(year = as.integer(year), round = as.integer(round), office = as.integer(office), uf = toupper(uf))]
  data.table::setorder(targets, year, round, office, uf)
  if (anyNA(targets) || any(!targets$uf %in% c("BR", unname(eleicao_ufs))) ||
      any(!targets$income_reference %in% c("national", "state")) ||
      any(targets$income_reference == "state" & targets$uf == "BR")) stop("Invalid targets")
  index <- eleicao_cache_index(cache_dir)
  # Read each potentially large vote cache once per year/round, not per office.
  targets[, processing_source := vapply(seq_len(.N), function(i) {
    selected <- eleicao_pick_cache(index, year[[i]], office[[i]], uf[[i]])
    if (is.null(selected)) "" else selected$path[[1L]]
  }, character(1))]
  data.table::setorder(targets, year, round, processing_source, office, uf)
  universe <- eleicao_income_universe(readRDS(income_path))
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
  series <- series[names(series) %in% valid_ids]
  availability <- availability[names(availability) %in% valid_ids]
  write_manifest <- function(manifest) {
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
    temporary <- paste0(manifest_path,".part")
    jsonlite::write_json(manifest,temporary,auto_unbox=TRUE,dataframe="rows",
      null="null",na="null",pretty=TRUE,digits=10)
    if(!file.copy(temporary,manifest_path,overwrite=TRUE)) stop("Cannot update manifest")
    unlink(temporary)
    frame <- if(length(manifest$availability)) data.table::rbindlist(manifest$availability,fill=TRUE) else data.table::data.table()
    data.table::fwrite(frame,file.path(output_dir,"availability.csv"))
  }
  income_hash <- unname(tools::md5sum(income_path))
  link_hash <- vote_hash <- NULL
  last_link_path <- NULL; links <- NULL; link_error <- NULL
  last_vote_path <- NULL; votes <- NULL; vote_error <- NULL
  for (i in seq_len(nrow(targets))) {
    target <- targets[i]; yr <- target$year; tr <- target$round; oc <- target$office; state <- target$uf
    reference <- target$income_reference
    id <- eleicao_target_id(yr, tr, oc, state, reference)
    status <- "unavailable"; reason <- NULL; item <- NULL
    selected <- eleicao_pick_cache(index, yr, oc, state)
    link_path <- file.path(spatial_dir, paste0("links-", yr, "-", tr, ".rds"))
    if (is.null(selected)) {
      reason <- "Sem cache normalizado de votação por local para esta disputa"
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
          fingerprint <- paste(if(reference == "state") "state-income-valid-v1" else "fixed-income-valid-v1",income_hash,link_hash,vote_hash,max_distance_m,id,sep=":")
          cached <- if(file.exists(destination))tryCatch(jsonlite::read_json(destination),error=function(e)NULL) else NULL
          if(!is.null(cached) && identical(cached$metadata$export_fingerprint,fingerprint) &&
             identical(cached$metadata$vote_audit$status,"reconciled")) {
            item <- eleicao_manifest_item(cached,paste0("series/",name))
          } else {
            dataset <- eleicao_aggregate_contest(votes, universe, links, yr, tr, oc, state,
              max_distance_m = max_distance_m, sources = sources, income_reference = reference)
            dataset$metadata$vote_source_md5 <- attr(votes, "source_md5")
            dataset$metadata$vote_cache <- basename(selected$path)
            dataset$metadata$spatial <- attr(links, "metadata")
            dataset$metadata$export_fingerprint <- fingerprint
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
      office_label = unname(eleicao_office_labels[as.character(oc)]), status = status,
      reason = if (is.null(reason)) "Dados processados com universo de renda fixo e cobertura documentada" else reason)
    if(status!="available") series[[id]] <- NULL
    boundary <- i==nrow(targets) || targets$processing_source[[i+1L]]!=targets$processing_source[[i]] ||
      targets$year[[i+1L]]!=yr || targets$round[[i+1L]]!=tr
    if(flush_progress && boundary) {
      progress_metadata <- if(!is.null(prior$metadata))prior$metadata else list(percentile_scope="BR",income_year=2022L,sources=sources)
      write_manifest(list(schema_version=1L,series=unname(series),availability=unname(availability),metadata=progress_metadata))
      message("Manifest updated: ",length(series)," available series")
    }
  }
  manifest <- list(schema_version = 1L, series = unname(series), availability = unname(availability),
    metadata = list(percentile_scope = if(any(vapply(series,function(item)identical(item$income_reference,"state"),logical(1)))) "selectable" else "BR", income_year = 2022L,
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
