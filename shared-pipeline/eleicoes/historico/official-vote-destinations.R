# Explicit primary-source vote destination only. Never solve a subset-sum or
# infer a destination from candidacy status or from an aggregate fit.
official_destination_table <- "shared-pipeline/eleicoes/historico/official-vote-destinations.json"
official_destination_load <- function(year,uf,offices,rounds,cache_dir,mapping_path=official_destination_table) {
  info<-jsonlite::read_json(mapping_path,simplifyVector=TRUE)
  if(!identical(as.integer(info$schema_version),1L))stop("Unknown official destination schema")
  mappings<-data.table::as.data.table(info$mappings)
  required<-c("year","uf","round","office","candidate_id","party_id","destination","source_destination","official_total_votes","source_id","source_page","source_total_page")
  if(!all(required %in% names(mappings)))stop("Missing official destination fields")
  # Avoid data.table column shadowing: caller arguments are retained explicitly.
  select_year<-as.integer(year);select_uf<-as.character(uf)
  mappings<-mappings[mappings$year %in% select_year & mappings$uf %in% select_uf & mappings$office %in% offices & mappings$round %in% rounds]
  if(!nrow(mappings))return(mappings)
  keys<-c("year","uf","round","office","candidate_id")
  if(anyDuplicated(mappings,by=keys)||any(!mappings$destination %in% c("party","null"))||
     anyNA(mappings[,..required])||any(mappings$official_total_votes<=0)||any(mappings$official_total_votes %% 1!=0)||
     any(mappings$source_page<1)||any(mappings$source_total_page<1))stop("Invalid or ambiguous official destination mapping")
  sources<-data.table::as.data.table(info$sources)
  if(anyDuplicated(sources$id))stop("Duplicate official destination document identity")
  mappings<-merge(mappings,sources,by.x="source_id",by.y="id",all.x=TRUE,sort=FALSE)
  source_fields<-c("url","source_date","cache_path","sha256","bytes")
  if(!all(source_fields %in% names(mappings))||anyNA(mappings[,..source_fields]))stop("Missing official destination provenance")
  root<-normalizePath(cache_dir,winslash="/",mustWork=TRUE)
  for(i in seq_len(nrow(mappings))) {
    relative<-mappings$cache_path[i]
    if(grepl("^([A-Za-z]:|[/\\\\])",relative)||any(strsplit(relative,"[/\\\\]")[[1L]]==".."))stop("Unsafe official document cache path")
    path<-normalizePath(file.path(cache_dir,relative),winslash="/",mustWork=TRUE)
    if(!startsWith(tolower(path),paste0(tolower(root),"/")))stop("Official document outside requested cache")
    if(!identical(readBin(path,"raw",5L),charToRaw("%PDF-"))||file.info(path)$size!=mappings$bytes[i]||
       !identical(digest::digest(file=path,algo="sha256"),mappings$sha256[i]))stop("Official destination PDF signature/size/SHA256 mismatch")
  }
  mappings
}

official_destination_apply <- function(nominal,converted,mappings) {
  x<-data.table::copy(nominal);extra<-list();evidence<-list()
  keys<-c("year","uf","round","office","candidate_id")
  if(!nrow(mappings))return(list(nominal=x,converted=converted,evidence=data.frame()))
  for(i in seq_len(nrow(mappings))) {
    m<-mappings[i]
    hit<-which(x$year==m$year & x$uf==m$uf & x$round==m$round & x$office==m$office & x$candidate_id==m$candidate_id)
    if(!length(hit)||sum(x$votes[hit])!=m$official_total_votes)stop("Explicit destination total does not match complete candidate ballots")
    if(any(!is.na(x$official_nominal[hit])))stop("Explicit destination cannot override an existing nominal allocation")
    if("candidate_name" %in% names(x)&&data.table::uniqueN(x$candidate_name[hit],na.rm=TRUE)>1L)
      stop("Multiple candidate identities under explicit destination ballot number")
    if("candidate_seq" %in% names(x)) {
      sequences<-unique(as.character(x$candidate_seq[hit]));sequences<-sequences[!is.na(sequences)&sequences!="NA"&nzchar(sequences)]
      if(length(sequences)>1L)stop("Multiple candidate sequences under explicit destination ballot number")
    }
    if(as.integer(substr(as.character(m$candidate_id),1L,2L))!=m$party_id)stop("Explicit destination party differs from ballot prefix")
    if(!m$destination %in% c("party","null"))stop("Unsupported explicit vote destination")
    if(m$destination=="party") {
      rows<-data.table::copy(x[hit])
      rows[,`:=`(party_id=m$party_id,candidate_id=m$party_id,candidate_name=paste("Partido",m$party_id),vote_type="party")]
      extra[[length(extra)+1L]]<-rows
    }
    # Known-null rows are removed from the pool available to any remaining
    # unknown conversion just as known-party rows are, preventing reuse.
    x[hit,`:=`(official_nominal=0,official_nominal_valid=0,party_id=m$party_id,
      destination=m$source_destination,keep_nominal=FALSE,convert_party=m$destination=="party")]
    evidence[[length(evidence)+1L]]<-as.data.frame(m)
  }
  converted<-data.table::rbindlist(c(list(converted),extra),use.names=TRUE,fill=TRUE)
  list(nominal=x,converted=converted,evidence=as.data.frame(data.table::rbindlist(evidence,use.names=TRUE,fill=TRUE)))
}

official_destination_environment <- function(base_environment,mapping_path=official_destination_table,source_cache=NULL) {
  env<-base_environment
  if(!is.environment(env)||!is.function(env$legal_reconcile_places))stop("Expected private historical reconciliation environment")
  env$official_destination_load<-official_destination_load;env$official_destination_apply<-official_destination_apply
  env$.official_destination_mapping_path<-mapping_path
  env$.official_destination_source_cache<-source_cache
  fn<-env$legal_reconcile_places;counts<-c(conversion=0L,audit=0L)
  transform<-function(z) {
    if(!is.call(z))return(z)
    if(identical(z[[1L]],as.name("<-"))&&identical(z[[2L]],as.name("converted"))&&is.call(z[[3L]])&&
       identical(z[[3L]][[1L]],as.name("historical_unknown_conversions"))) {
      counts["conversion"]<<-counts["conversion"]+1L
      return(substitute({
        evidence_cache<-if(is.null(.official_destination_source_cache))cache_dir else .official_destination_source_cache
        explicit_destinations<-official_destination_load(year,uf,offices,unique(nominal$round),evidence_cache,.official_destination_mapping_path)
        explicit_result<-official_destination_apply(nominal,converted,explicit_destinations)
        nominal<-explicit_result$nominal;converted<-explicit_result$converted
        official_destination_evidence<-explicit_result$evidence
        ORIGINAL
      },list(ORIGINAL=z)))
    }
    if(identical(z[[1L]],as.name("<-"))&&is.call(z[[2L]])&&identical(z[[2L]][[1L]],as.name("attr"))&&
       identical(z[[2L]][[2L]],as.name("places"))&&identical(z[[2L]][[3L]],"vote_audit")) {
      counts["audit"]<<-counts["audit"]+1L
      return(substitute({
        ORIGINAL
        if(nrow(official_destination_evidence)) {
          audit<-attr(places,"vote_audit")
          audit$official_destination_sources<-official_destination_evidence
          audit$method<-paste(audit$method,"Absent candidate destinations explicitly documented by official PDF, verified signature/bytes/SHA256 and exact candidate totals; converted vectors checked against official party/zone conversion, no subset inference",sep="; ")
          attr(places,"vote_audit")<-audit
        }
      },list(ORIGINAL=z)))
    }
    as.call(lapply(as.list(z),transform))
  }
  body(fn)<-transform(body(fn));if(any(counts!=1L))stop("Official destination adapter anchors changed")
  environment(fn)<-env;env$legal_reconcile_places<-fn;env
}
