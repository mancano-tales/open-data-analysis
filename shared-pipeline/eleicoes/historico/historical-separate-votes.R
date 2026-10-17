# 2018 official section dictionary, leiame.pdf p5: 96=null; 97=annulled
# and counted separately. Category 97 NEVER enters the null-vote calculation.
historical_section_categories <- function(path,year,uf,offices,elections) {
  if(!identical(as.integer(year),2018L))stop("Separate-vote ledger is restricted to the 2018 dictionary")
  members<-utils::unzip(path,list=TRUE)$Name
  member<-members[grepl(paste0("(^|/)votacao_secao_",year,"_",uf,"[.]csv$"),members,ignore.case=TRUE)]
  if(length(member)!=1L)stop("No unique historical section member")
  folder<-sub("[.]zip$","",path);csv<-file.path(folder,member)
  source_md5<-unname(tools::md5sum(path))
  stamp<-file.path(folder,".historical-specials-source-md5")
  recorded_md5<-if(file.exists(stamp))tryCatch(readLines(stamp,warn=FALSE),error=function(e)NULL) else NULL
  if(!file.exists(csv)||!identical(recorded_md5,source_md5)) {
    # Bind the derived extraction to the archive bytes, not just its filename.
    # zip::unzip also rejects corrupt member payloads before recording a stamp.
    zip::unzip(path,files=member,exdir=folder,overwrite=TRUE)
    if(!file.exists(csv))stop("Cannot extract historical section member")
    writeLines(source_md5,stamp)
  }
  columns<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO","NR_VOTAVEL","QT_VOTOS")
  raw<-data.table::fread(csv,select=columns,encoding="Latin-1")
  raw<-raw[ANO_ELEICAO==year & CD_ELEICAO %in% elections & CD_CARGO %in% offices & SG_UF==uf & NR_VOTAVEL %in% c(96L,97L)]
  if(anyNA(raw)||any(!is.finite(raw$QT_VOTOS))||any(raw$QT_VOTOS<0)||any(raw$QT_VOTOS %% 1!=0))
    stop("Invalid observed section96/97 counts or keys")
  groups<-columns[1:7]
  result<-raw[,.(section_nulls=sum(as.numeric(QT_VOTOS[NR_VOTAVEL==96L])),
    section_separate=sum(as.numeric(QT_VOTOS[NR_VOTAVEL==97L]))),by=groups]
  data.table::setnames(result,groups,c("year","round","election_id","uf","municipality_tse","zone","office"))
  result[,municipality_tse:=as.character(municipality_tse)]
  result
}

historical_separate_vote_environment <- function(base_environment) {
  env<-base_environment
  if(!is.environment(env)||!is.function(env$legal_reconcile_places))stop("Expected historical Senate/null ledger environment")
  env$historical_null_sections<-historical_section_categories
  fn<-env$legal_reconcile_places;counts<-c(guard=0L,detail=0L,observed=0L,check=0L,summary=0L,method=0L)
  transform<-function(z) {
    if(!is.call(z))return(z)
    lhs<-if(identical(z[[1L]],as.name("<-")))z[[2L]] else NULL
    if(identical(lhs,as.name("blocked_columns"))) {
      counts["guard"]<<-counts["guard"]+1L
      return(quote({
        blocked_columns<-"QT_TOTAL_VOTOS_ANUL_SUBJUD"
        if(!"QT_VOTOS_ANULADOS_APU_SEP" %in% names(historical_detail))stop("Missing official separate-vote category")
        separate_counts<-historical_detail$QT_VOTOS_ANULADOS_APU_SEP
        if(anyNA(separate_counts)||any(!is.finite(separate_counts))||any(separate_counts<0)||any(separate_counts %% 1!=0))
          stop("Invalid official separate-vote category")
      }))
    }
    if(identical(lhs,as.name("detail"))&&is.call(z[[3L]])&&".SDcols" %in% names(as.list(z[[3L]]))) {
      counts["detail"]<<-counts["detail"]+1L
      return(substitute({
        ORIGINAL
        separate_aliases<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO")
        separate_official<-historical_category_detail[,.(official_separate_votes=sum(as.numeric(QT_VOTOS_ANULADOS_APU_SEP))),by=separate_aliases]
        data.table::setnames(separate_official,separate_aliases,basekeys)
        separate_official[,municipality_tse:=as.character(municipality_tse)]
        detail<-merge(detail,separate_official,by=basekeys,all=TRUE)
      },list(ORIGINAL=z)))
    }
    if(identical(lhs,as.name("checks"))&&is.call(z[[3L]])&&identical(z[[3L]][[1L]],as.name("merge"))&&
       length(z[[3L]])>=3L&&identical(z[[3L]][[3L]],as.name("technical"))) {
      counts["observed"]<<-counts["observed"]+1L
      return(substitute({
        ORIGINAL
        separate_observed<-data.table::copy(section_nulls[,c(basekeys,"section_separate"),with=FALSE])
        data.table::setnames(separate_observed,"section_separate","computed_separate_votes")
        checks<-merge(checks,separate_observed,by=basekeys,all=TRUE)
        checks[is.na(computed_separate_votes),computed_separate_votes:=0]
      },list(ORIGINAL=z)))
    }
    if(identical(z[[1L]],as.name("["))&&length(z)>=4L&&identical(z[[2L]],as.name("checks"))&&
       is.call(z[[4L]])&&identical(z[[4L]][[1L]],as.name(":="))&&identical(z[[4L]][[2L]],as.name("reconciled"))) {
      counts["check"]<<-counts["check"]+1L
      return(substitute({ORIGINAL;checks[,reconciled:=reconciled & !is.na(official_separate_votes) & computed_separate_votes==official_separate_votes]},list(ORIGINAL=z)))
    }
    if(identical(lhs,as.name("aggregate_checks"))) {
      counts["summary"]<<-counts["summary"]+1L
      return(substitute({
        ORIGINAL
        separate_summary<-checks[,.(official_separate_votes=sum(official_separate_votes),computed_separate_votes=sum(computed_separate_votes),
          separate_difference=sum(computed_separate_votes-official_separate_votes)),by=contest_keys]
        aggregate_checks<-merge(aggregate_checks,separate_summary,by=contest_keys,all=TRUE)
      },list(ORIGINAL=z)))
    }
    if(identical(lhs,as.name("audit_method"))) {
      counts["method"]<<-counts["method"]+1L
      return(substitute(audit_method<-paste(gsub("pending/separate categories blocked","pending category blocked; separate category independently audited",ORIGINAL_VALUE,fixed=TRUE),
        "Observed section97 exactly matches the official separately counted annulled category in every zone; excluded separately, NEVER added to nulls; pending category remains blocked",sep="; "),list(ORIGINAL_VALUE=z[[3L]])))
    }
    as.call(lapply(as.list(z),transform))
  }
  body(fn)<-transform(body(fn));if(any(counts!=1L))stop("Historical separate-vote adapter anchors changed: ",paste(names(counts),counts,collapse=", "))
  environment(fn)<-env;env$legal_reconcile_places<-fn;env
}
