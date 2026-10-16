# Sequential, resumable state-election preparation. Run from repository root.
# Rscript --vanilla .../prepare-historical-all-ufs.R 2022 [AC,DF,...] [--dry-run]
# An explicit multi-year list runs only in order 2022 -> 2018 -> 2014 -> 2010.
# Default is 2022 ONLY. Each UF runs in an isolated child R process, DTthreads=2.
# The versioned party-technical wrapper is required: absent party registrations
# enter technical-null counts, and EVERY municipality/zone must reconcile exactly.

historical_years <- c(2022L,2018L,2014L,2010L)
historical_ufs <- c("AC","AP","RR","RO","SE","TO","AL","DF","MS","ES","MT","RN","PI","PB","AM","GO","MA","SC","PA","CE","PE","PR","RS","RJ","BA","MG","SP")
historical_version <- 1L
historical_script <- "shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R"
historical_sources <- c(historical_script,"shared-pipeline/eleicoes/historico/tse.R",
 "shared-pipeline/eleicoes/historico/legal-validity.R",
 "shared-pipeline/eleicoes/historico/legal-historical-null-totals.R",
 "shared-pipeline/eleicoes/historico/historical-senate-nonvalid.R",
 "shared-pipeline/eleicoes/historico/historical-contest-isolation.R",
 "shared-pipeline/eleicoes/agregacao/validacao/legal-party-technical-wrapper.R",
 "shared-pipeline/eleicoes/agregacao/validacao/candidate-group-fastpath.R",
 "shared-pipeline/eleicoes/historico/cleanup-historical-extractions.R")
historical_md5_cache <- new.env(parent=emptyenv())
historical_plan <- function(years=2022L,ufs=historical_ufs) {
 years<-unique(as.integer(years));ufs<-unique(toupper(ufs))
 if(!length(years)||anyNA(years)||any(!years %in% historical_years))stop("Supported years: 2022,2018,2014,2010")
 if(!length(ufs)||anyNA(ufs)||any(!ufs %in% historical_ufs))stop("Invalid UF list")
 years<-historical_years[historical_years %in% years]
 rows<-lapply(years,function(year)data.frame(year=year,uf=ufs,stringsAsFactors=FALSE))
 do.call(rbind,rows)
}
historical_identity <- function(paths) {
 paths<-unique(paths[file.exists(paths)])
 lapply(paths,function(p) {
   info<-file.info(p)
   identity<-paste(p,info$size,as.numeric(info$mtime),sep=":")
   md5<-if(exists(identity,envir=historical_md5_cache,inherits=FALSE))get(identity,envir=historical_md5_cache) else {
     value<-unname(tools::md5sum(p));assign(identity,value,envir=historical_md5_cache);value
   }
   list(path=p,size=unname(info$size),modified_utc=format(info$mtime,tz="UTC",usetz=TRUE),md5=md5)
 })
}
historical_write_json <- function(value,path) {
 dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
 jsonlite::write_json(value,paste0(path,".part"),pretty=TRUE,auto_unbox=TRUE,na="null")
 if(!file.copy(paste0(path,".part"),path,overwrite=TRUE))stop("Cannot checkpoint ",path)
 unlink(paste0(path,".part"))
}
historical_checkpoint_path <- function(cache_dir,year,uf)file.path(cache_dir,"historical-status",as.character(year),paste0(uf,".json"))
historical_checkpoint <- function(cache_dir,year,uf) {
 path<-historical_checkpoint_path(cache_dir,year,uf)
 if(!file.exists(path))return(NULL)
 tryCatch(jsonlite::read_json(path,simplifyVector=FALSE),error=function(e)NULL)
}
historical_current_checkpoint <- function(record) {
 if(is.null(record)||!identical(as.integer(record$version),historical_version)||!record$status %in% c("complete","partial"))return(FALSE)
 if(!length(record$fingerprints))return(FALSE)
 for(old in record$fingerprints) {
   if(!file.exists(old$path))return(FALSE)
   current<-historical_identity(old$path)[[1L]]
   if(!identical(current$md5,old$md5))return(FALSE)
 }
 TRUE
}
historical_contest_status <- function(expected,checks,unavailable) {
 keys<-c("year","round","election_id","office","uf")
 signature<-function(x)if(!nrow(x))character() else do.call(paste,c(x[,keys,drop=FALSE],sep=":"))
 if(!nrow(checks)||anyNA(checks$difference)||any(checks$difference!=0)||
    anyNA(checks$computed_valid_votes)||any(checks$computed_valid_votes!=checks$official_valid_votes))stop("Invalid certified audit")
 required<-signature(expected);certified<-signature(checks);blocked<-signature(unavailable)
 if(any(!required %in% c(certified,blocked)))stop("Official contest missing from both certified and unavailable audit")
 if(any(certified %in% blocked))stop("Contest is both certified and unavailable")
 if(all(required %in% certified))"complete" else "partial"
}
historical_worker <- function(year,uf,cache_dir="data-raw/eleicoes-renda/historico") {
 if(nrow(historical_plan(year,uf))!=1L)stop("Worker expects one year and UF")
 start<-Sys.time();offices<-c(3L,5L,6L,7L,8L)
 loaded_code<-historical_identity(historical_sources)
 checkpoint_path<-historical_checkpoint_path(cache_dir,year,uf)
 base<-list(version=historical_version,year=year,uf=uf,offices=offices,started_at=as.character(start))
 historical_write_json(c(base,list(status="running")),checkpoint_path)
 output<-file.path(cache_dir,paste0("places-",year,"-",uf,"-3_5_6_7_8-legal.rds"))
 sources<-c(historical_sources,file.path(cache_dir,paste0("resultados-",year,"-metadata.json")),
  file.path(cache_dir,paste0("votacao_secao_",year,"_",uf,".zip")),
  file.path(cache_dir,paste0(c("votacao_candidato_munzona","votacao_partido_munzona","detalhe_votacao_munzona"),"_",year,".zip")))
 result<-tryCatch({
   source("shared-pipeline/eleicoes/agregacao/validacao/legal-party-technical-wrapper.R")
   source("shared-pipeline/eleicoes/agregacao/validacao/candidate-group-fastpath.R")
   source("shared-pipeline/eleicoes/historico/cleanup-historical-extractions.R")
   if(identical(as.integer(year),2018L)) {
     source("shared-pipeline/eleicoes/historico/legal-historical-null-totals.R")
     source("shared-pipeline/eleicoes/historico/historical-senate-nonvalid.R")
     env<-historical_null_environment()
     env<-historical_senate_nonvalid_environment(env)
   } else env<-legal_party_technical_environment()
   env<-legal_candidate_fast_environment(base_environment=env)
   if(identical(as.integer(year),2018L)) {
     source("shared-pipeline/eleicoes/historico/historical-contest-isolation.R")
     env<-historical_isolation_environment(env)
   }
   data.table::setDTthreads(2L)
   x<-env$legal_reconcile_places(year,uf,offices,cache_dir)
   audit<-attr(x,"vote_audit")
   if(!identical(audit$status,"reconciled")||!identical(attr(x,"normalizer_version"),2L))stop("Uncertified normalized output")
   source_md5<-unname(tools::md5sum(file.path(cache_dir,paste0("votacao_secao_",year,"_",uf,".zip"))))
   if(!identical(source_md5,attr(x,"source_md5")))stop("Section-source MD5 mismatch")
   checks<-as.data.frame(audit$checks);unavailable<-as.data.frame(audit$unavailable)
   if(any(checks$year!=year)||any(checks$uf!=uf))stop("Unexpected output contest scope")
   for(i in seq_len(nrow(checks))) {
     r<-checks[i,]
     actual<-sum(x$votes[x$year==r$year&x$round==r$round&x$election_id==r$election_id&x$office==r$office&x$uf==r$uf])
     if(!is.finite(actual)||actual!=r$computed_valid_votes)stop("Certified cache differs from its audit")
   }
   detail_path<-env$legal_resource_csv(year,"detalhe_votacao_munzona",uf,cache_dir)
   cols<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","CD_CARGO","SG_UF","CD_TIPO_ELEICAO")
   detail<-data.table::fread(detail_path,select=cols,encoding="Latin-1")
   expected<-unique(detail[ANO_ELEICAO==year&SG_UF==uf&CD_TIPO_ELEICAO==2L&CD_CARGO %in% offices,
     .(year=ANO_ELEICAO,round=NR_TURNO,election_id=CD_ELEICAO,office=CD_CARGO,uf=SG_UF)])
   state<-historical_contest_status(as.data.frame(expected),checks,unavailable)
   rows<-nrow(x);rm(x,detail);invisible(gc())
   list(status=state,certified=checks,unavailable=unavailable,rows=rows,
    method=audit$method,source_md5=source_md5)
 },error=function(e)list(status="failed",error=conditionMessage(e)))
 current_code<-historical_identity(historical_sources)
 if(!identical(lapply(loaded_code,`[[`,"md5"),lapply(current_code,`[[`,"md5"))) {
   result$status<-"failed"
   result$error<-"Implementation changed while worker was active; rerun to certify its exact source provenance"
 }
 record<-c(base,result,list(finished_at=as.character(Sys.time()),elapsed_seconds=as.numeric(difftime(Sys.time(),start,units="secs")),
   fingerprints=c(loaded_code,historical_identity(setdiff(c(sources,if(file.exists(output))output),historical_sources))),
   technical_rule=if(identical(as.integer(year),2018L))
     "Historical per-dispute exact nominal/party/valid and COMBINED NULL PLUS ANNULLED ledger; separate components official only; annulled extension Senate only, pending/separate blocked; integral explicit party conversions, no allocation" else
     "Unregistered nominal and party ballots are excluded only after exact technical-null and nominal/party/valid checks in every municipality-zone; no allocation"))
 historical_write_json(record,checkpoint_path)
 if(identical(record$status,"complete")) {
   record$cleanup<-tryCatch({
     removed<-historical_cleanup_state(year,uf,cache_dir=cache_dir,apply=TRUE)
     list(status="complete",removed_files=sum(removed$status=="removed"),removed_bytes=sum(removed$bytes))
   },error=function(e)list(status="preserved",error=conditionMessage(e)))
   if(!is.null(record$cleanup$error))warning("Electoral cache certified; extraction preserved: ",record$cleanup$error)
   record$finished_at<-as.character(Sys.time())
   record$elapsed_seconds<-as.numeric(difftime(Sys.time(),start,units="secs"))
   historical_write_json(record,checkpoint_path)
 }
 message(toupper(record$status)," ",year,"/",uf," elapsed ",round(record$elapsed_seconds,1),"s")
 record
}
historical_prepare <- function(years=2022L,ufs=historical_ufs,dry_run=FALSE,rebuild=FALSE,retry_partial=FALSE,
                              cache_dir="data-raw/eleicoes-renda/historico") {
 if(!requireNamespace("jsonlite",quietly=TRUE))stop("Install jsonlite")
 plan<-historical_plan(years,ufs)
 executable<-file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
 if(.Platform$OS.type=="windows")executable<-shortPathName(executable)
 output<-list()
 for(i in seq_len(nrow(plan))) {
   year<-plan$year[i];uf<-plan$uf[i]
   record<-historical_checkpoint(cache_dir,year,uf)
   resume<-!rebuild&&historical_current_checkpoint(record)&&!(retry_partial&&identical(record$status,"partial"))
   if(dry_run) {
     output[[i]]<-data.frame(year=year,uf=uf,status=if(resume)paste0("cached-",record$status) else "planned",exit_code=NA_integer_)
     next
   }
   if(resume)message("RESUME ",year,"/",uf," ",record$status) else {
     log<-file.path(cache_dir,"historical-status",as.character(year),paste0(uf,".log"))
     dir.create(dirname(log),recursive=TRUE,showWarnings=FALSE)
     message("PREPARING ",year,"/",uf," (single child process; log ",log,")")
     arguments<-c("--vanilla",shQuote(historical_script),"--worker",as.character(year),uf,"--cache-dir",shQuote(cache_dir))
     status<-system2(executable,arguments,stdout=log,stderr=log)
     record<-historical_checkpoint(cache_dir,year,uf)
     if(is.null(record)||!record$status %in% c("complete","partial","failed")) {
       record<-list(version=historical_version,year=year,uf=uf,status="failed",error=paste("Child stopped, exit",status),finished_at=as.character(Sys.time()))
       historical_write_json(record,historical_checkpoint_path(cache_dir,year,uf))
     }
     message(toupper(record$status)," ",year,"/",uf,if(!is.null(record$error))paste0(": ",record$error) else "")
   }
   output[[i]]<-data.frame(year=year,uf=uf,status=record$status,exit_code=if(record$status=="complete")0L else 1L)
   current<-do.call(rbind,output)
   for(y in unique(current$year))write.csv(current[current$year==y,],file.path(cache_dir,paste0("prepare-",y,"-historical-status.csv")),row.names=FALSE)
   invisible(gc())
 }
 do.call(rbind,output)
}
if(sys.nframe()==0L) {
 args<-commandArgs(trailingOnly=TRUE)
 cache_dir<-"data-raw/eleicoes-renda/historico"
 cache_flag<-which(args=="--cache-dir")
 if(length(cache_flag)) {
   if(length(cache_flag)!=1L||cache_flag==length(args)||grepl("^--",args[cache_flag+1L]))stop("--cache-dir requires one directory")
   cache_dir<-args[cache_flag+1L]
   args<-args[-c(cache_flag,cache_flag+1L)]
 }
 if("--worker" %in% args) {
   values<-args[!grepl("^--",args)]
   if(length(values)!=2L)stop("Worker requires YEAR UF")
   if(!requireNamespace("jsonlite",quietly=TRUE))stop("Install jsonlite")
   result<-historical_worker(as.integer(values[1L]),toupper(values[2L]),cache_dir=cache_dir)
   quit(status=if(result$status=="complete")0L else 1L)
 }
 flags<-c("--dry-run","--rebuild","--retry-partial")
 if(any(grepl("^--",args)&!args %in% flags))stop("Unknown option")
 values<-args[!grepl("^--",args)]
 years<-if(length(values))as.integer(strsplit(values[1L],",",fixed=TRUE)[[1L]]) else 2022L
 ufs<-if(length(values)>=2L)strsplit(values[2L],",",fixed=TRUE)[[1L]] else historical_ufs
 result<-historical_prepare(years,ufs,dry_run="--dry-run" %in% args,rebuild="--rebuild" %in% args,retry_partial="--retry-partial" %in% args,cache_dir=cache_dir)
 print(result,row.names=FALSE)
 if(!"--dry-run" %in% args&&any(result$status!="complete"))quit(status=1L)
}
