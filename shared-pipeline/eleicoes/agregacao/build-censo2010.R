# Census 2010 companion series for already certified 2010/2014 contests.
# Run from the repository root. Spatial cache writes add census-suffixed files.
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/espacial/050_Mapear_Locais_Historicos.R",encoding="UTF-8")
source("shared-pipeline/eleicoes/agregacao/build-explorer.R",encoding="UTF-8")
source("shared-pipeline/eleicoes/agregacao/verificar-censo2010.R",encoding="UTF-8")

build_censo2010 <- function(output_dir="posts/explorador-eleitoral/data") {
  for (year in c(2010L,2014L)) for (round in 1:2)
    map_election_places(year,round,census_year=2010L)
  manifest <- jsonlite::read_json(file.path(output_dir,"manifest.json"))
  existing <- Filter(function(s) s$year %in% c(2010L,2014L) &&
    identical(s$status,"available") && is.null(s$census_year),manifest$series)
  if (!length(existing)) stop("No certified Census 2022 counterparts for 2010/2014")
  targets <- data.table::rbindlist(lapply(existing,function(s) data.table::data.table(
    year=as.integer(s$year),round=as.integer(s$round),office=as.integer(s$office),uf=s$uf,
    income_reference=if(is.null(s$income_reference)) "national" else s$income_reference,
    census_year=2010L)))
  result <- eleicao_build_explorer(output_dir=output_dir,targets=targets,incremental=TRUE,
    legacy_cache_dir="data-raw/eleicoes-renda/historico/legacy-canonic")
  verify_census2010(output_dir)
  invisible(result)
}

if (sys.nframe()==0L) build_censo2010()
