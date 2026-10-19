# Opt-in 2018 Senate ledger. The sum NULL + ANNULLED is observed exactly;
# it does not identify a judicial category for each excluded candidate.
historical_senate_nonvalid_environment <- function(base_environment) {
  env<-base_environment
  if(!is.environment(env)||!is.function(env$legal_reconcile_places))stop("Expected historical null-total environment")
  fn<-env$legal_reconcile_places
  counts<-c(guard=0L,retain=0L,detail=0L,method=0L)
  transform<-function(z) {
    if(!is.call(z))return(z)
    if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("blocked_columns"))) {
      counts["guard"]<<-counts["guard"]+1L
      return(quote({
        blocked_columns<-c("QT_TOTAL_VOTOS_ANUL_SUBJUD","QT_VOTOS_ANULADOS_APU_SEP")
        if(anyNA(historical_detail$QT_TOTAL_VOTOS_ANULADOS)||any(historical_detail$QT_TOTAL_VOTOS_ANULADOS<0)||
           any(historical_detail$CD_CARGO!=5L & historical_detail$QT_TOTAL_VOTOS_ANULADOS!=0))
          stop("Annulled-category extension is restricted to identified 2018 Senate ledgers")
      }))
    }
    if(identical(z[[1L]],as.name("rm"))&&length(z)==2L&&identical(z[[2L]],as.name("historical_detail"))) {
      counts["retain"]<<-counts["retain"]+1L
      return(quote({historical_category_detail<-data.table::copy(historical_detail);rm(historical_detail)}))
    }
    if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("audit_method"))) {
      counts["method"]<<-counts["method"]+1L
      return(quote(audit_method<-"2018 official all-or-zero candidate validity and integral explicit party conversion; exact valid/nominal/party totals and COMBINED NULL PLUS ANNULLED nonvalid ledger (section96 plus excluded nominal/party ballots) per zone. Official null and annulled components retained separately; their computed individual allocation is NOT inferred. Annulled extension restricted to Senate; pending/separate categories blocked; no allocation"))
    }
    if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("detail"))&&
       is.call(z[[3L]])&&".SDcols" %in% names(as.list(z[[3L]]))) {
      counts["detail"]<<-counts["detail"]+1L
      return(substitute({
        ORIGINAL
        aliases<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO")
        components<-historical_category_detail[,.(official_annulled_component=sum(as.numeric(QT_TOTAL_VOTOS_ANULADOS))),by=aliases]
        data.table::setnames(components,aliases,basekeys)
        components[,municipality_tse:=as.character(municipality_tse)]
        detail[,official_null_component:=null_total_official]
        detail<-merge(detail,components,by=basekeys,all=TRUE)
        if(anyNA(detail$official_annulled_component))stop("Missing official historical annulled component")
        detail[,null_total_official:=null_total_official+official_annulled_component]
      },list(ORIGINAL=z)))
    }
    as.call(lapply(as.list(z),transform))
  }
  patched<-transform(body(fn))
  if(any(counts!=1L))stop("Historical Senate adapter anchors changed: ",paste(names(counts),counts,collapse=", "))
  rename<-function(z) {
    if(is.symbol(z)&&as.character(z) %in% c("null_total_computed","null_total_official"))
      return(as.name(sub("null_total_","nonvalid_combined_",as.character(z),fixed=TRUE)))
    if(is.character(z))return(gsub("^null_total_(computed|official)$","nonvalid_combined_\\1",z))
    if(is.call(z))return(as.call(lapply(as.list(z),rename)))
    z
  }
  body(fn)<-rename(patched);environment(fn)<-env;env$legal_reconcile_places<-fn
  env
}
