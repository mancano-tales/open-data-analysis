# The official 2018 leiame.pdf p7 defines legend/conversion measures at
# party/cargo/municipality/zone grain, independent of coalition descriptors.
# Transit partitions remain additive. Nominal measures are NEVER deduplicated.
historical_party_split <- function(party) {
  p<-data.table::copy(data.table::as.data.table(party))
  keys<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO","NR_PARTIDO","ST_VOTO_EM_TRANSITO")
  measures<-c("QT_VOTOS_LEGENDA_VALIDOS","QT_VOTOS_NOMINAIS_CONVR_LEG","QT_TOTAL_VOTOS_LEG_VALIDOS","QT_VOTOS_LEGENDA_ANUL_SUBJUD")
  measures<-c(measures,intersect("QT_VOTOS_LEGENDA_ANULADOS",names(p)))
  if(!all(c(keys,measures) %in% names(p)))stop("Missing official 2018 party aggregate fields")
  if(anyNA(p[,..keys]))stop("Missing official party aggregate key")
  if(any(p$ANO_ELEICAO!=2018L))stop("Party aggregate projection is restricted to the 2018 dictionary")
  if(any(!p$ST_VOTO_EM_TRANSITO %in% c("N","S")))stop("Unknown official party transit partition")
  for(field in measures) {
    z<-p[[field]]
    if(anyNA(z)||any(!is.finite(z))||any(z<0)||any(z %% 1!=0))stop("Invalid explicit party measure: ",field)
  }
  counts<-p[,lapply(.SD,data.table::uniqueN),by=keys,.SDcols=measures]
  if(nrow(counts)&&any(unlist(counts[,..measures])!=1L))
    stop("Conflicting explicit party aggregates across coalition descriptors")
  projection_columns<-c(keys,measures)
  list(legend=unique(p[,..projection_columns]),nominal_source=p)
}

historical_party_projection_read <- function(path,year,uf,offices,elections,columns=NULL) {
  p<-data.table::fread(path,encoding="Latin-1")
  p<-p[p$ANO_ELEICAO %in% year & p$SG_UF %in% uf & p$CD_CARGO %in% offices & p$CD_ELEICAO %in% elections]
  split<-historical_party_split(p)
  if(!"QT_VOTOS_NOMINAIS_ANUL_SUBJUD" %in% names(split$nominal_source))stop("Missing original nominal pending control")
  if("QT_VOTOS_NOMINAIS_ANUL_SUBJUD" %in% names(split$nominal_source)) {
    pending<-split$nominal_source$QT_VOTOS_NOMINAIS_ANUL_SUBJUD
    if(anyNA(pending)||any(pending!=0))stop("Positive or unknown original nominal pending counts")
  }
  if(is.null(columns))split$legend else split$legend[,intersect(columns,names(split$legend)),with=FALSE]
}

historical_party_aggregate_environment <- function(base_environment) {
  env<-base_environment
  if(!is.environment(env)||!is.function(env$legal_reconcile_places)||!is.function(env$historical_unknown_conversions))
    stop("Expected private historical environment")
  env$historical_party_projection_read<-historical_party_projection_read
  env$historical_party_split<-historical_party_split
  env$.historical_original_unknown_conversions<-env$historical_unknown_conversions
  unknown<-function(nominal,converted,party,basekeys) {
    aliases<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO")
    scope<-unique(nominal[,..basekeys]);data.table::setnames(scope,basekeys,aliases)
    source<-data.table::copy(party);source[,CD_MUNICIPIO:=as.character(CD_MUNICIPIO)]
    source<-merge(source,scope,by=aliases,all=FALSE)
    split<-historical_party_split(source)
    if(!"QT_VOTOS_NOMINAIS_ANUL_SUBJUD" %in% names(split$nominal_source))stop("Missing original nominal pending control")
    if("QT_VOTOS_NOMINAIS_ANUL_SUBJUD" %in% names(split$nominal_source)) {
      pending<-split$nominal_source$QT_VOTOS_NOMINAIS_ANUL_SUBJUD
      if(anyNA(pending)||any(pending!=0))stop("Positive or unknown original nominal pending counts")
    }
    .historical_original_unknown_conversions(nominal,converted,split$legend,basekeys)
  }
  environment(unknown)<-env;env$historical_unknown_conversions<-unknown
  fn<-env$legal_reconcile_places;counts<-c(read=0L,method=0L)
  transform<-function(z) {
    if(!is.call(z))return(z)
    if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("party_official"))&&
       is.call(z[[3L]])&&is.call(z[[3L]][[1L]])&&identical(z[[3L]][[1L]][[1L]],as.name("::"))&&
       identical(z[[3L]][[1L]][[3L]],as.name("fread"))) {
      counts["read"]<<-counts["read"]+1L
      return(quote(party_official<-historical_party_projection_read(party_path,year,uf,offices,ordinary_elections,
        columns=intersect(party_cols,party_header))))
    }
    if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("audit_method"))) {
      counts["method"]<<-counts["method"]+1L
      return(substitute(audit_method<-paste(ORIGINAL_VALUE,
        "Explicit party-zone legend/conversion aggregates counted once per transit partition; coalition descriptor repeats must agree; original nominal rows retained separately",sep="; "),list(ORIGINAL_VALUE=z[[3L]])))
    }
    as.call(lapply(as.list(z),transform))
  }
  body(fn)<-transform(body(fn));if(any(counts!=1L))stop("Historical party aggregate adapter anchors changed")
  environment(fn)<-env;env$legal_reconcile_places<-fn;env
}
