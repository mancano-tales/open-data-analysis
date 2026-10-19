# Independent checks against the pilot income table, before spatial exclusions.
# Run from the repository root after exporting Census 2010 series.
suppressPackageStartupMessages(library(data.table))
source("shared-pipeline/eleicoes/agregacao/censo-referencia.R",encoding="UTF-8")
source("shared-pipeline/eleicoes/agregacao/centis.R",encoding="UTF-8")

verify_census2010 <- function(output_dir="posts/explorador-eleitoral/data",
  income_path="data-raw/eleicoes-renda/censo2010/renda2010-basico.rds") {
  config <- eleicao_censo(2010L)
  raw <- as.data.table(readRDS(income_path))
  eligible <- raw[is.finite(get(config$income_variable)) & get(config$income_variable)>0 &
    is.finite(get(config$head_count_variable)) & get(config$head_count_variable)>0]
  eligible[,code_tract:=eleicao_tract_code(code_tract)]
  eligible[,uf:=unname(eleicao_ufs[substr(code_tract,1L,2L)])]
  stopifnot(!anyNA(eligible$uf),!anyDuplicated(eligible$code_tract))
  manifest <- jsonlite::read_json(file.path(output_dir,"manifest.json"))
  entries <- Filter(function(x) identical(as.integer(x$census_year),2010L),manifest$series)
  if (!length(entries)) stop("No Census 2010 series")
  for (entry in entries) {
    universe <- copy(eligible)
    if (entry$income_reference=="state") universe <- universe[uf==entry$uf]
    setorderv(universe,c(config$income_variable,"code_tract"))
    universe[,centil:=as.integer(floor((seq_len(.N)-1L)*100/.N)+1L)]
    if (entry$income_reference=="national" && entry$uf!="BR") universe <- universe[uf==entry$uf]
    expected <- universe[,.(n_setores_total=.N,
      renda_sm=mean(get(config$income_variable))/config$minimum_wage_brl),by=centil]
    data <- jsonlite::read_json(file.path(output_dir,entry$url),simplifyVector=TRUE)
    meta <- data$metadata; points <- as.data.table(data$points)
    stopifnot(identical(as.integer(meta$income_year),2010L),
      identical(as.numeric(meta$minimum_wage_brl),510),
      identical(meta$income_definition,config$income_definition),
      identical(meta$vote_audit$status,"reconciled"),
      identical(as.numeric(meta$vote_audit$difference),0))
    for (candidate in unique(points$candidate_id)) {
      p <- points[candidate_id==candidate]
      if (nrow(p)!=100L || !identical(sort(p$centil),1:100)) stop("Incomplete centiles: ",entry$id)
      if (sum(p$n_setores)!=meta$coverage$included_tracts) stop("Covered tract count mismatch: ",entry$id)
      # National groups may have no tracts in a small UF: expected count 0, no income.
      compare <- merge(p[,.(centil,n_setores_total,renda_sm)],expected,by="centil",all.x=TRUE,suffixes=c("","_expected"))
      compare[is.na(n_setores_total_expected),n_setores_total_expected:=0L]
      filled <- compare[n_setores_total_expected>0L]
      if (nrow(compare)!=100L || any(compare$n_setores_total!=compare$n_setores_total_expected) ||
          anyNA(filled$renda_sm) || any(abs(filled$renda_sm-filled$renda_sm_expected)>1e-7) ||
          any(!is.na(compare[n_setores_total_expected==0L]$renda_sm))) stop("Income universe mismatch: ",entry$id)
    }
  }
  message("Verified ",length(entries)," Census 2010 series; eligible pilot tracts: ",nrow(eligible))
  invisible(length(entries))
}

if (sys.nframe()==0L) verify_census2010()
