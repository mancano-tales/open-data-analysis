# Fixed Census 2022 tracts, contemporary polling places, independent of votes.
# Rscript .../050_Mapear_Locais_Historicos.R 2026 1
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
suppressPackageStartupMessages({library(data.table); library(sf); library(geobr); library(here)})
source("shared-pipeline/eleicoes/agregacao/censo-referencia.R", encoding = "UTF-8")
source("shared-pipeline/eleicoes/agregacao/centis.R", encoding = "UTF-8")
setDTthreads(2L)
options(timeout = max(600, getOption("timeout")))

spatial_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}
spatial_save <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(path, ".part")
  saveRDS(x, part)
  if (!file.copy(part, path, overwrite = TRUE)) stop("Cannot save ", path)
  unlink(part)
}

spatial_2010_geometry <- function(root, kind, uf = NULL) {
  file <- file.path(root,"censo2010","spatial-pilot","geometry-source",
    if(kind=="tract") "censustracts_2010.parquet" else "municipalities_2010.parquet")
  if (!file.exists(file)) stop("Missing cached geobr 2010 geometry: ",file)
  table <- arrow::open_dataset(file)
  if (!is.null(uf)) table <- dplyr::filter(table,abbrev_state==uf)
  x <- dplyr::collect(table)
  if (!nrow(x)) stop("Empty cached geobr 2010 geometry: ",kind,"/",uf)
  x$geometry <- sf::st_as_sfc(x$geometry,EWKB=TRUE)
  sf::st_crs(x$geometry) <- 4674
  sf::st_as_sf(x)
}

map_election_places <- function(year, round = 1L, rebuild = FALSE, census_year = 2022L) {
  census <- eleicao_censo(census_year)
  if (!eleicao_censo_allowed(census_year, year)) stop("Census reference unavailable for election")
  root <- here("data-raw", "eleicoes-renda")
  cache <- file.path(root, "espacial")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  stopifnot(year %in% c(2010L,2012L,2014L,2016L,2018L,2020L,2022L,2024L,2026L), round %in% 1:2)
  suffix <- if (census$census_year == 2022L) "" else paste0("-c", census$census_year)
  income_path <- if (census$census_year == 2022L) file.path(root,"renda2022.rds") else
    file.path(root,"censo2010","renda2010-basico.rds")
  out <- file.path(cache, sprintf("links-%s-%s%s.rds", year, round, suffix))
  if (census$census_year == 2010L && file.exists(out) && (rebuild || is.null(spatial_read(out))))
    stop("Existing Census 2010 link cannot be overwritten: ",out)
  poll_file <- file.path(cache, sprintf("polling-%s.rds", year))
  if (!rebuild) {
    ready <- spatial_read(out)
    info <- attr(ready, "metadata")
    if (!is.null(ready) && !is.null(info$date) && file.exists(poll_file)) {
      current_identity <- paste(unname(tools::md5sum(c(poll_file,
        income_path))), info$date,
        "municipal-nearest-v1", collapse=":")
      if (identical(info$input_identity, current_identity)) return(ready)
    }
  }
  if (census$census_year == 2010L && file.exists(out)) stop("Existing Census 2010 link is stale and cannot be overwritten: ",out)
  poll <- spatial_read(poll_file)
  if (is.null(poll) && year == 2026L) poll <- spatial_read(file.path(root, "polling2026.rds"))
  if (is.null(poll)) poll <- geobr::read_polling_places(year = year)
  if (is.null(poll)) stop("Contemporary polling places unavailable: ", year)
  if (!file.exists(poll_file)) spatial_save(poll, poll_file)
  crs <- st_crs(poll)
  xy <- st_coordinates(poll)
  fields <- c("code_muni", "code_muni_tse", "nr_zona", "nr_local_votacao", "abbrev_state", "dt_eleicao")
  if (!all(fields %in% names(poll))) stop("Unexpected geobr columns for ", year)
  tab <- as.data.table(st_drop_geometry(poll))[, ..fields]
  tab[, `:=`(longitude = xy[,1], latitude = xy[,2])]
  dates <- as.Date(as.character(tab$dt_eleicao), format = "%d/%m/%Y")
  alternative <- as.Date(as.character(tab$dt_eleicao), format = "%Y-%m-%d")
  dates[is.na(dates)] <- alternative[is.na(dates)]
  available <- sort(unique(dates[!is.na(dates)]))
  if (!length(available) || any(as.integer(format(available, "%Y")) != year))
    stop("Polling-place election date unavailable for requested year/round: ", year, "/", round)
  reference_round <- if(length(available) >= round) round else 1L
  reference_date <- available[reference_round]
  reused_register <- reference_round != round
  if(reused_register) warning("Only first-round polling coordinates available; matching second-round votes to SAME-YEAR place keys. New/unmatched places remain uncovered.")
  if(reused_register && !rebuild) {
    first <- spatial_read(file.path(cache,sprintf("links-%s-1%s.rds",year,suffix)))
    first_info <- attr(first,"metadata")
    expected_identity <- paste(unname(tools::md5sum(c(poll_file,income_path))),
      as.character(reference_date),"municipal-nearest-v1",collapse=":")
    if(!is.null(first) && identical(first_info$input_identity,expected_identity)) {
      first_info$round <- round
      first_info$polling_reference_date <- as.character(reference_date)
      first_info$polling_reference_round <- reference_round
      first_info$within_year_register_reused <- TRUE
      attr(first,"metadata") <- first_info
      if (census$census_year == 2010L) {
        source_audit <- file.path(cache,sprintf("coverage-places-%s-1%s.csv",year,suffix))
        target_audit <- file.path(cache,sprintf("coverage-places-%s-%s%s.csv",year,round,suffix))
        if (file.exists(source_audit) && !file.exists(target_audit)) {
          audit <- fread(source_audit)
          requested_round <- round
          audit[,`:=`(round=requested_round,within_year_register_reused=TRUE)]
          fwrite(audit,target_audit)
        }
      }
      spatial_save(first,out)
      return(first)
    }
  }
  tab <- tab[dates == reference_date & !is.na(dates)]
  tab[, place_key := paste(abbrev_state, code_muni_tse, nr_zona, nr_local_votacao, sep = ":")]
  locations <- unique(tab[, .(place_key, code_muni, longitude, latitude)])
  conflicts <- locations[, .N, by = place_key][N > 1L, place_key]
  tab <- tab[!place_key %in% conflicts & !duplicated(place_key) &
    is.finite(longitude) & is.finite(latitude) &
    longitude >= -74 & longitude <= -28 & latitude >= -34 & latitude <= 6]
  poll <- st_transform(st_as_sf(tab[, .(place_key,code_muni,longitude,latitude)],
                                coords=c("longitude","latitude"), crs=crs), 4326)
  municipal_path <- file.path(root,sprintf("municipios%s-validos5880.rds",census$municipality_geometry_year))
  municipal <- spatial_read(municipal_path)
  if (is.null(municipal)) {
    if (census$census_year == 2010L && file.exists(municipal_path)) stop("Existing 2010 municipality cache cannot be overwritten")
    source_municipal <- if(census$census_year==2010L) spatial_2010_geometry(root,"municipality") else
      geobr::read_municipality(year=census$municipality_geometry_year,simplified=FALSE)
    municipal <- st_make_valid(st_transform(source_municipal,5880))
    spatial_save(municipal, municipal_path)
  }
  intersects <- st_intersects(st_transform(poll,5880),municipal)
  compatible <- vapply(seq_len(nrow(poll)), function(i)
    poll$code_muni[i] %in% municipal$code_muni[intersects[[i]]], logical(1))
  audit <- data.table(year=year, round=round, date=as.character(reference_date),
    polling_reference_round=reference_round,within_year_register_reused=reused_register,
    conflicting_keys=length(conflicts), valid_coordinates=nrow(poll),
    incompatible_municipality=sum(!compatible), eligible_places=sum(compatible))
  audit_path <- file.path(cache,sprintf("coverage-places-%s-%s%s.csv",year,round,suffix))
  if (census$census_year == 2010L && file.exists(audit_path)) {
    if (!isTRUE(all.equal(fread(audit_path,colClasses=list(character="date")),audit,check.attributes=FALSE)))
      stop("Existing 2010 place audit differs: ",audit_path)
  } else fwrite(audit,audit_path)
  poll <- poll[compatible,]
  poll <- poll[order(poll$place_key),]
  rm(municipal,intersects,compatible,xy,tab,locations,dates,alternative)
  invisible(gc())
  indices <- split(seq_len(nrow(poll)), as.character(poll$code_muni))
  income <- as.data.table(readRDS(income_path))[, c("code_tract","code_muni","code_state",
    census$head_count_variable,census$income_variable), with=FALSE]
  setnames(income,c(census$head_count_variable,census$income_variable),c("heads","income"))
  income[, code_tract := eleicao_tract_code(code_tract)]
  income <- income[!is.na(heads) & heads > 0 & is.finite(income) & income > 0]
  setorder(income,income,code_tract)
  income[, centil := as.integer(floor((seq_len(.N)-1)*100/.N)+1)]
  # Fingerprint every election input; per-state links survive interruptions.
  identity <- paste(unname(tools::md5sum(c(poll_file,income_path))),
                    as.character(reference_date),"municipal-nearest-v1",collapse=":")
  rows <- list()
  for (uf in sort(unique(income$code_state))) {
    state_file <- file.path(cache,sprintf("links-%s-%s-UF%s%s.rds",year,round,uf,
      if(census$census_year==2010L) "-c2010-v2" else suffix))
    state <- if (!rebuild) spatial_read(state_file) else NULL
    if (is.null(state) || !identical(attr(state,"input_identity"),identity)) {
      if (census$census_year == 2010L && file.exists(state_file)) stop("Existing 2010 state link cannot be overwritten: ",state_file)
      message("Spatial ",year,"/",round," UF ",uf)
      point_path <- file.path(root,"malhas",if(census$census_year==2010L)
        sprintf("pontos2010-v2-%s.rds",uf) else sprintf("pontos%s-%s.rds",census$tract_geometry_year,uf))
      points <- spatial_read(point_path)
      if (is.null(points) && census$census_year == 2010L) {
        if (file.exists(point_path)) stop("Existing 2010 tract point cache cannot be overwritten: ",point_path)
        shape <- spatial_2010_geometry(root,"tract",unname(eleicao_ufs[as.character(uf)]))
        points <- st_transform(st_point_on_surface(st_make_valid(st_transform(shape,5880))),4326)
        spatial_save(points,point_path)
      }
      if (is.null(points)) stop("Build census tract points first: run baseline script 020.")
      points <- points[points$code_tract %in% income$code_tract,]
      state_rows <- list()
      for (muni in unique(points$code_muni)) {
        tr <- points[points$code_muni == muni,]
        ix <- indices[[as.character(muni)]]
        entry <- data.table(code_tract=tr$code_tract,place_key=NA_character_,distancia_m=NA_real_)
        if (length(ix)) {
          pp <- poll[ix,]
          nearest <- st_nearest_feature(tr,pp)
          entry[, `:=`(place_key=pp$place_key[nearest],
            distancia_m=as.numeric(st_distance(tr,pp[nearest,],by_element=TRUE)))]
        }
        state_rows[[length(state_rows)+1L]] <- entry
      }
      state <- rbindlist(state_rows)
      attr(state,"input_identity") <- identity
      spatial_save(state,state_file)
    }
    rows[[length(rows)+1L]] <- state
  }
  links <- rbindlist(rows)
  # 2010 geobr geometry stores tract codes as doubles; normalize before joining.
  links[, code_tract := eleicao_tract_code(code_tract)]
  if (anyDuplicated(links$code_tract)) stop("Duplicate tract in spatial links")
  result <- merge(income[,.(code_tract,code_muni,code_state,centil,renda_sm=income/census$minimum_wage_brl)],
                  links,by="code_tract",all.x=TRUE,sort=FALSE)
  attr(result,"metadata") <- list(year=year,round=round,date=as.character(reference_date),
    polling_reference_date=as.character(reference_date),polling_reference_round=reference_round,
    within_year_register_reused=reused_register,
    method=paste("Nearest contemporary polling place to interior tract point, same",census$census_year,"municipality"),
    census_year=census$census_year,
    percentile_scope="BR",rank_universe=nrow(income),geometry_independent_of_votes=TRUE,
    input_identity=identity)
  spatial_save(result,out)
  result
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if (!length(args)) stop("Usage: Rscript 050_Mapear_Locais_Historicos.R YEAR ROUND")
  result <- map_election_places(as.integer(args[1]),if(length(args)>1) as.integer(args[2]) else 1L,
    census_year=if(length(args)>2) as.integer(args[3]) else 2022L)
  message("PASS spatial links: ",nrow(result)," fixed-income tracts")
}
