# Run from the repository root. Defaults intentionally download presidency only.
# Example: Rscript --vanilla shared-pipeline/eleicoes/historico/prepare.R 2010,2014,2018,2022 BR 1
# State offices: ... prepare.R 2026 AC 3,5,6,7
source("shared-pipeline/eleicoes/historico/tse.R")
args <- commandArgs(trailingOnly = TRUE)
years <- if (length(args)) as.integer(strsplit(args[1], ",", fixed = TRUE)[[1]]) else c(2010L,2014L,2018L,2022L)
ufs <- if (length(args) >= 2) strsplit(toupper(args[2]), ",", fixed = TRUE)[[1]] else "BR"
offices <- if (length(args) >= 3) as.integer(strsplit(args[3], ",", fixed = TRUE)[[1]]) else 1L
if (anyNA(years) || anyNA(offices) || any(!offices %in% as.integer(names(tse_offices)))) stop("Invalid years/offices")
if ("BR" %in% ufs && any(offices != 1L)) stop("Use individual UFs for non-presidential offices")
cache_dir <- "data-raw/eleicoes-renda/historico"
data.table::setDTthreads(2L)
for (year in years) for (uf in ufs) {
  path <- tse_download(year, uf, cache_dir)
  target <- file.path(cache_dir, paste0("places-",year,"-",uf,"-",paste(offices,collapse="_"),".rds"))
  input_md5 <- unname(tools::md5sum(path))
  if (file.exists(target)) {
    cached <- readRDS(target)
    if (identical(attr(cached,"source_md5"),input_md5) &&
        identical(attr(cached,"normalizer_version"),2L)) {
      message("Cached: ", target)
      next
    }
  }
  votes <- tse_read_votes(path, year, offices)
  if (!nrow(votes)) stop("No votes found for ",year,"/",uf,"/",paste(offices,collapse=","))
  excluded <- votes[uf == "ZZ" | vote_type %in% c("special","invalid_code"),
    .(votes=sum(votes),rows=.N), by=.(year,round,office,uf,vote_type)]
  data.table::fwrite(excluded, sub("[.]rds$","-excluded.csv",target))
  places <- tse_place_votes(votes[uf != "ZZ"])
  attr(places,"source_md5") <- input_md5
  attr(places,"normalizer_version") <- 2L
  saveRDS(places,paste0(target,".part"))
  if (!file.rename(paste0(target,".part"),target)) {
    # Windows does not replace existing destinations on file.rename.
    if (!file.copy(paste0(target,".part"),target,overwrite=TRUE)) stop("Cannot save ",target)
    unlink(paste0(target,".part"))
  }
  message("Saved ", nrow(places)," place/candidate/round rows: ",target)
  rm(votes,places)
  invisible(gc())
}
