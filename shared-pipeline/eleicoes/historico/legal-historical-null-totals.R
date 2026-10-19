# Historical total-null reconciliation. Opt-in only; contemporary rules remain
# untouched. No candidate eligibility or proportional allocation is used.
source("shared-pipeline/eleicoes/historico/legal-validity.R")

historical_null_sections <- function(path,year,uf,offices,elections) {
  member<-utils::unzip(path,list=TRUE)$Name
  member<-member[grepl(paste0("(^|/)votacao_secao_",year,"_",uf,"[.]csv$"),member,ignore.case=TRUE)]
  if(length(member)!=1L)stop("No unique historical section member")
  csv<-file.path(sub("[.]zip$","",path),member)
  if(!file.exists(csv))utils::unzip(path,files=member,exdir=sub("[.]zip$","",path))
  cols<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO","NR_VOTAVEL","QT_VOTOS")
  x<-data.table::fread(csv,select=cols,encoding="Latin-1")
  groups<-cols[1:7]
  x<-x[CD_ELEICAO %in% elections & CD_CARGO %in% offices & SG_UF==uf & NR_VOTAVEL==96L,
    .(section_nulls=sum(as.numeric(QT_VOTOS))),by=groups]
  data.table::setnames(x,cols[1:7],c("year","round","election_id","uf","municipality_tse","zone","office"))
  x[,municipality_tse:=as.character(municipality_tse)]
  x
}

historical_unknown_conversions <- function(nominal,converted,party,basekeys) {
  pkeys<-c(basekeys,"party_id")
  needed<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO","NR_PARTIDO","QT_VOTOS_NOMINAIS_CONVR_LEG")
  if(!all(needed %in% names(party)))stop("Historical conversion source lacks required columns")
  pending_fields<-intersect(c("QT_VOTOS_LEGENDA_ANUL_SUBJUD","QT_VOTOS_NOMINAIS_ANUL_SUBJUD"),names(party))
  selected_columns<-c(needed,pending_fields)
  official<-party[,..selected_columns]
  data.table::setnames(official,needed,c(pkeys,"official_conversion"))
  official[,municipality_tse:=as.character(municipality_tse)]
  # Restrict scope to the exact ordinary contests in the section source.
  scope<-unique(nominal[,..basekeys])
  official<-merge(official,scope,by=basekeys,all=FALSE)
  if(length(pending_fields)&&(anyNA(official[,..pending_fields])||any(unlist(official[,..pending_fields])!=0)))
    stop("Positive or unknown historical party pending counts: no total-null allocation allowed")
  if(any(grepl("sub.?jud|pendente",nominal$destination,ignore.case=TRUE)&nominal$official_nominal>0,na.rm=TRUE))
    stop("Positive historical candidate pending counts: no total-null allocation allowed")
  official<-official[,.(official_conversion=sum(as.numeric(official_conversion))),by=pkeys]
  if(anyNA(official$official_conversion)||any(official$official_conversion<0))stop("Invalid historical conversion count")
  known<-converted[,.(known_conversion=sum(votes)),by=pkeys]
  unknown<-data.table::copy(nominal[is.na(official_nominal)])
  # Ballot prefixes determine only the party grouping, never legal validity.
  unknown[,party_id:=as.integer(substr(as.character(candidate_id),1L,2L))]
  groups<-unknown[,.(unknown_total=sum(votes)),by=pkeys]
  check<-merge(official,known,by=pkeys,all=TRUE)
  check<-merge(check,groups,by=pkeys,all=TRUE)
  check[is.na(official_conversion),official_conversion:=0]
  check[is.na(known_conversion),known_conversion:=0]
  check[is.na(unknown_total),unknown_total:=0]
  check[,remaining_conversion:=official_conversion-known_conversion]
  if(any(check$remaining_conversion<0)||any(check$remaining_conversion!=0 & check$remaining_conversion!=check$unknown_total))
    stop("Partial or unexplained historical conversion: cannot allocate to places")
  selected<-check[remaining_conversion>0,..pkeys]
  extra<-merge(unknown,selected,by=pkeys,all=FALSE,sort=FALSE)
  if(nrow(extra))extra[,`:=`(candidate_id=party_id,candidate_name=paste("Partido",party_id),vote_type="party")]
  data.table::rbindlist(list(converted,extra),use.names=TRUE,fill=TRUE)
}

historical_null_environment <- function() {
  # Work on a private function, with exact AST anchors. Fail if the reference
  # implementation changes instead of silently applying an obsolete patch.
  env<-new.env(parent=globalenv())
  sys.source("shared-pipeline/eleicoes/historico/legal-validity.R",envir=env)
  env$historical_null_sections<-historical_null_sections
  env$historical_unknown_conversions<-historical_unknown_conversions
  original<-env$legal_reconcile_places
  expressions<-as.list(body(original));counts<-c(scope=0L,conversion=0L,technical=0L,method=0L,party=0L)
  output<-list(expressions[[1L]],quote(if(!identical(as.integer(year),2018L))stop("Total-null historical rule is restricted to 2018; earlier ledgers require separate validation")))
  for(expr in expressions[-1L]) {
    lhs<-if(is.call(expr)&&identical(expr[[1L]],as.name("<-")))as.character(expr[[2L]]) else ""
    if(identical(lhs,"detail_cols")) {
      replace_field<-function(z) {
        if(is.character(z))return(replace(z,z=="QT_VOTOS_NULOS_TECNICOS","QT_TOTAL_VOTOS_NULOS"))
        if(is.call(z))return(as.call(lapply(as.list(z),replace_field)))
        z
      }
      expr<-replace_field(expr)
    }
    if(identical(lhs,"technical")) {
      counts["technical"]<-counts["technical"]+1L
      # Overwrite after legal votes have been built; known invalid candidates,
      # unknown candidates and invalid party ballots all remain in raw totals.
      next
    }
    if(identical(lhs,"audit_method")) {
      counts["method"]<-counts["method"]+1L
      expr<-quote(audit_method<-"Historical official all-or-zero candidate validity and integral party/zone conversion; exact valid, nominal, party and TOTAL NULL votes (section 96 plus excluded nominal/party ballots) in every municipality/zone; pending/annulled categories must be zero; no allocation")
    }
    if(identical(lhs,"keep_columns")) {
      counts["conversion"]<-counts["conversion"]+1L
      output<-c(output,list(quote({
        historical_party_path<-legal_resource_csv(year,"votacao_partido_munzona",uf,cache_dir)
        historical_party<-data.table::fread(historical_party_path,encoding="Latin-1")
        converted<-historical_unknown_conversions(nominal,converted,historical_party,basekeys)
        rm(historical_party)
      })))
    }
    # Nested party join: missing official party rows remain excluded and enter
    # the total-null equation through the original raw counts.
    filter_unknown_party<-function(z) {
      if(!is.call(z))return(z)
      if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("party_valid"))&&
         is.call(z[[3L]])&&identical(z[[3L]][[1L]],as.name("merge"))) {
        counts["party"]<<-counts["party"]+1L
        return(substitute({JOIN;party_valid<-party_valid[!is.na(party_total)]},list(JOIN=z)))
      }
      as.call(lapply(as.list(z),filter_unknown_party))
    }
    expr<-filter_unknown_party(expr)
    output<-c(output,list(expr))
    if(identical(lhs,"ordinary_elections")) {
      counts["scope"]<-counts["scope"]+1L
      output<-c(output,list(quote({
        section_nulls<-historical_null_sections(path,year,uf,offices,ordinary_elections)
        historical_detail_path<-legal_resource_csv(year,"detalhe_votacao_munzona",uf,cache_dir)
        historical_detail<-data.table::fread(historical_detail_path,encoding="Latin-1")
        historical_detail<-historical_detail[CD_ELEICAO %in% ordinary_elections & CD_CARGO %in% offices & SG_UF==uf]
        blocked_columns<-c("QT_TOTAL_VOTOS_ANUL_SUBJUD","QT_TOTAL_VOTOS_ANULADOS","QT_VOTOS_ANULADOS_APU_SEP")
        if(!all(blocked_columns %in% names(historical_detail)))stop("Missing historical pending/annulled controls")
        if(anyNA(historical_detail[,..blocked_columns])||any(unlist(historical_detail[,..blocked_columns])!=0))
          stop("Positive or unknown historical pending/annulled categories: no total-null allocation allowed")
        rm(historical_detail)
      })))
    }
    if(identical(lhs,"computed"))output<-c(output,list(quote({
      technical<-raw[,.(raw_non_special=sum(votes)),by=basekeys]
      technical<-merge(technical,computed[,c(basekeys,"computed_valid_votes"),with=FALSE],by=basekeys,all=TRUE)
      technical<-merge(technical,section_nulls,by=basekeys,all=TRUE)
      technical[is.na(section_nulls),section_nulls:=0]
      technical[is.na(computed_valid_votes),computed_valid_votes:=0]
      technical[is.na(raw_non_special),raw_non_special:=0]
      technical[,technical_computed:=section_nulls+raw_non_special-computed_valid_votes]
      if(any(technical$technical_computed<0))stop("Negative historical null count")
      technical<-technical[,c(basekeys,"technical_computed"),with=FALSE]
    })))
  }
  if(any(counts!=1L))stop("Historical adapter anchors changed: ",paste(names(counts),counts,collapse=", "))
  # Audit column names reflect TOTAL nulls; never label them technical nulls.
  rename_nulls<-function(z) {
    if(is.symbol(z)&&as.character(z) %in% c("technical_computed","technical_official"))
      return(as.name(sub("technical_","null_total_",as.character(z),fixed=TRUE)))
    if(is.character(z))return(gsub("^technical_(computed|official)$","null_total_\\1",z))
    if(is.call(z))return(as.call(lapply(as.list(z),rename_nulls)))
    z
  }
  body(original)<-rename_nulls(as.call(output));environment(original)<-env
  env$legal_reconcile_places<-original
  env
}
