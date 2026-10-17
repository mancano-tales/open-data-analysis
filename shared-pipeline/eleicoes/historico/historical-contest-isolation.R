# Isolate historical disputes without weakening any legal reconciliation.
# Reuse normalized places and the selective section-96 scan across contests.
historical_isolation_environment <- function(base_environment) {
  env<-base_environment
  if(!is.environment(env)||!is.function(env$legal_reconcile_places)||!is.function(env$historical_null_sections))
    stop("Expected historical legal environment with section-null reader")
  env$.historical_single_contest<-env$legal_reconcile_places
  wrapper<-function(year,uf,offices=c(3L,5L,6L,7L,8L),cache_dir="data-raw/eleicoes-renda/historico") {
    if(!identical(as.integer(year),2018L))stop("Historical isolation currently supports 2018 only")
    path<-tse_download(year,uf,cache_dir)
    source_md5<-unname(tools::md5sum(path))
    stem<-paste0("places-",year,"-",uf,"-",paste(offices,collapse="_"))
    raw_path<-file.path(cache_dir,paste0(stem,".rds"))
    raw<-if(file.exists(raw_path))readRDS(raw_path) else NULL
    if(is.null(raw)||!identical(attr(raw,"normalizer_version"),2L)||!identical(attr(raw,"source_md5"),source_md5))
      stop("Prepare matching normalized raw all-office cache before contest isolation")
    keys<-c("year","round","election_id","office","uf")
    detail_path<-legal_resource_csv(year,"detalhe_votacao_munzona",uf,cache_dir)
    detail<-data.table::fread(detail_path,select=c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","CD_CARGO","SG_UF","CD_TIPO_ELEICAO"),encoding="Latin-1")
    expected<-unique(detail[ANO_ELEICAO==year & SG_UF==uf & CD_TIPO_ELEICAO==2L & CD_CARGO %in% offices,
      .(year=ANO_ELEICAO,round=NR_TURNO,election_id=CD_ELEICAO,office=CD_CARGO,uf=SG_UF)])
    if(!nrow(expected))stop("No ordinary historical contests in official detail")
    nulls<-historical_null_sections(path,year,uf,offices,unique(expected$election_id))
    source_cache<-cache_dir
    original_resource<-get("legal_resource_csv",envir=environment(),inherits=TRUE)
    # These overrides are private to this environment and restored on exit.
    private<-environment(.historical_single_contest)
    previous<-lapply(c("tse_download","legal_resource_csv","historical_null_sections"),function(n)
      if(exists(n,envir=private,inherits=FALSE))get(n,envir=private) else NULL)
    on.exit({
      for(i in seq_along(previous)) {
        n<-c("tse_download","legal_resource_csv","historical_null_sections")[i]
        if(is.null(previous[[i]]))rm(list=n,envir=private) else assign(n,previous[[i]],envir=private)
      }
    },add=TRUE)
    download_override<-function(year,uf,cache_dir)path
    resource_override<-function(year,dataset_prefix,uf,cache_dir)original_resource(year,dataset_prefix,uf,source_cache)
    null_override<-function(path,year,uf,offices,elections)nulls[office %in% offices & election_id %in% elections]
    assign("tse_download",download_override,envir=private)
    assign("legal_resource_csv",resource_override,envir=private)
    assign("historical_null_sections",null_override,envir=private)
    results<-checks<-zone_audits<-blocked<-destination_sources<-list();method<-character()
    run_id<-paste0("run-",Sys.getpid(),"-",format(Sys.time(),"%Y%m%d%H%M%OS6"))
    for(i in seq_len(nrow(expected))) {
      contest<-expected[i]
      work<-file.path(cache_dir,paste0("isolated-",year,"-",uf),run_id,paste(contest$election_id,contest$round,contest$office,sep="-"))
      dir.create(work,recursive=TRUE,showWarnings=FALSE)
      one<-merge(raw,contest,by=keys,all=FALSE,sort=FALSE)
      attr(one,"normalizer_version")<-2L;attr(one,"source_md5")<-source_md5
      saveRDS(one,file.path(work,paste0("places-",year,"-",uf,"-",contest$office,".rds")))
      rm(one)
      result<-tryCatch({
        x<-.historical_single_contest(year,uf,contest$office,work)
        audit<-attr(x,"vote_audit");a<-data.table::as.data.table(audit$checks)
        if(!identical(attr(x,"normalizer_version"),2L)||!identical(attr(x,"source_md5"),source_md5)||
           !identical(audit$status,"reconciled")||nrow(a)!=1L||anyNA(a$difference)||any(a$difference!=0)||
           !isTRUE(a$reconciled)||anyNA(a$computed_valid_votes)||any(a$computed_valid_votes!=a$official_valid_votes))
          stop("Uncertified isolated contest")
        if(nrow(merge(a,contest,by=keys))!=1L||any(!do.call(paste,c(x[,..keys],sep=":")) %in%
            do.call(paste,c(contest[,..keys],sep=":"))))stop("Unexpected isolated contest scope")
        if(sum(x$votes)!=a$computed_valid_votes)stop("Isolated votes differ from certified audit")
        evidence<-audit$official_destination_sources
        if(!is.null(evidence)&&nrow(data.table::as.data.table(evidence))) {
          evidence<-data.table::copy(data.table::as.data.table(evidence))
          evidence_keys<-c("year","round","office","uf")
          if(!all(evidence_keys %in% names(evidence))||anyNA(evidence[,..evidence_keys]))
            stop("Official destination evidence lacks complete scope")
          # A document may describe several races: retain only this certified
          # race, without leaking another race's evidence into its metadata.
          evidence<-evidence[evidence$year==contest$year & evidence$round==contest$round &
            evidence$office==contest$office & evidence$uf==contest$uf]
          if("election_id" %in% names(evidence))evidence<-evidence[election_id==contest$election_id]
        }
        list(x=x,checks=a,method=audit$method,evidence=evidence)
      },error=function(e)list(error=conditionMessage(e)))
      if(!is.null(result$error)) {
        blocked[[length(blocked)+1L]]<-data.table::copy(contest)[,`:=`(reconciled=FALSE,reason=result$error)]
      } else {
        results[[length(results)+1L]]<-result$x;checks[[length(checks)+1L]]<-result$checks
        if(!is.null(result$evidence)&&nrow(result$evidence))destination_sources[[length(destination_sources)+1L]]<-result$evidence
        method<-unique(c(method,result$method))
      }
      zone_path<-file.path(work,paste0("places-",year,"-",uf,"-",contest$office,"-legal-audit.csv"))
      if(file.exists(zone_path))zone_audits[[length(zone_audits)+1L]]<-data.table::fread(zone_path)
      invisible(gc())
    }
    unavailable<-if(length(blocked))data.table::rbindlist(blocked) else expected[0][,`:=`(reconciled=logical(),reason=character())]
    output<-file.path(cache_dir,paste0(stem,"-legal.rds"))
    if(length(zone_audits))data.table::fwrite(data.table::rbindlist(zone_audits,use.names=TRUE,fill=TRUE),sub("[.]rds$","-audit.csv",output))
    if(!length(results)) {
      # A prior certificate must not remain discoverable under the canonical
      # name after every current dispute was blocked. Keep a diagnostic copy,
      # then write an explicitly uncertified empty canonical cache.
      if(file.exists(output)&&!file.copy(output,file.path(cache_dir,paste0(stem,"-superseded-",run_id,".rds"))))
        stop("Cannot preserve superseded historical certificate")
      empty<-raw[0]
      attr(empty,"normalizer_version")<-2L;attr(empty,"source_md5")<-source_md5
      attr(empty,"vote_audit")<-list(status="unavailable",method="No isolated historical dispute passed current controls; all expected disputes explicitly unavailable",
        checks=data.frame(),unavailable=as.data.frame(unavailable),election_ids=unique(expected$election_id))
      saveRDS(empty,output)
      return(empty)
    }
    x<-data.table::rbindlist(results,use.names=TRUE)
    attr(x,"normalizer_version")<-2L;attr(x,"source_md5")<-source_md5
    attr(x,"vote_audit")<-list(status="reconciled",method=paste(c(method,"Disputes isolated by ordinary election ID, round and office; blocked disputes explicitly unavailable"),collapse="; "),
      checks=as.data.frame(data.table::rbindlist(checks)),unavailable=as.data.frame(unavailable),
      election_ids=unique(expected$election_id),election_scope="Ordinary election IDs selected from official detail; each dispute independently reconciled")
    if(length(destination_sources)) {
      audit<-attr(x,"vote_audit")
      audit$official_destination_sources<-as.data.frame(unique(data.table::rbindlist(destination_sources,use.names=TRUE,fill=TRUE)))
      attr(x,"vote_audit")<-audit
    }
    saveRDS(x,output)
    x
  }
  environment(wrapper)<-env;env$legal_reconcile_places<-wrapper
  env
}
