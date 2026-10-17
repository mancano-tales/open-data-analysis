# Separate 2014/2010 driver. No downloads, no shared-runner mutations, no explorer writes.
# Rscript --vanilla .../prepare-legacy-all-ufs.R 2014 AC,DF [--dry-run]
legacy_driver_script <- 'shared-pipeline/eleicoes/historico/prepare-legacy-all-ufs.R'
legacy_driver_sources <- c(legacy_driver_script,
 'shared-pipeline/eleicoes/agregacao/validacao/legal-2010-2014.R',
 'shared-pipeline/eleicoes/agregacao/validacao/legacy-2010-2014-environment.R',
 'shared-pipeline/eleicoes/historico/tse.R')
legacy_driver_ufs <- c('AC','AP','RR','RO','SE','TO','AL','DF','MS','ES','MT','RN','PI','PB','AM','GO','MA','SC','PA','CE','PE','PR','RS','RJ','BA','MG','SP')
legacy_driver_keys <- c('year','round','election_id','office','uf')
legacy_driver_plan <- function(years,ufs) {
 years<-unique(as.integer(years));ufs<-unique(toupper(ufs))
 if(!length(years)||anyNA(years)||any(!years %in% c(2014L,2010L)))stop('Explicit legacy years must be 2014 and/or 2010')
 if(!length(ufs)||anyNA(ufs)||any(!ufs %in% legacy_driver_ufs))stop('Explicit valid UF list required')
 do.call(rbind,lapply(c(2014L,2010L)[c(2014L,2010L) %in% years],function(y)data.frame(year=y,uf=ufs)))
}
legacy_driver_identity <- function(paths) {
 paths<-unname(unique(paths))
 if(any(!file.exists(paths)))stop('Missing input: ',paste(paths[!file.exists(paths)],collapse=', '))
 data.frame(path=paths,bytes=unname(file.info(paths)$size),md5=unname(tools::md5sum(paths)),stringsAsFactors=FALSE)
}
legacy_driver_same_snapshot <- function(a,b) {
 all(c('path','bytes','md5') %in% names(a))&&all(c('path','bytes','md5') %in% names(b))&&
 identical(as.character(a$path),as.character(b$path))&&identical(as.numeric(a$bytes),as.numeric(b$bytes))&&
 identical(as.character(a$md5),as.character(b$md5))
}
legacy_driver_signature <- function(x) {
 if(!nrow(x))return(character())
 if(!all(legacy_driver_keys %in% names(x))||anyNA(x[,legacy_driver_keys,drop=FALSE]))stop('Incomplete legacy scope')
 z<-do.call(paste,c(x[,legacy_driver_keys,drop=FALSE],sep=':'))
 if(anyDuplicated(z))stop('Duplicate legacy contest')
 z
}
legacy_driver_partition <- function(expected,checks,unavailable) {
 e<-legacy_driver_signature(expected);a<-legacy_driver_signature(checks);u<-legacy_driver_signature(unavailable)
 if(!length(e))stop('No expected ordinary legacy contest')
 if(length(intersect(a,u)))stop('Legacy contest overlaps accepted and unavailable')
 if(!setequal(e,c(a,u)))stop('Legacy partition has missing or unexpected contest')
 if(nrow(unavailable)&&(!'reason' %in% names(unavailable)||anyNA(unavailable$reason)||any(!nzchar(trimws(unavailable$reason)))))
   stop('Unavailable legacy contest requires explicit reason')
 invisible(TRUE)
}
legacy_driver_validate <- function(places,year,uf,source_md5) {
 audit<-attr(places,'vote_audit')
 if(!identical(attr(places,'normalizer_version'),2L)||!identical(attr(places,'source_md5'),source_md5)||
    !identical(audit$status,'diagnostic_only')||!identical(audit$publishable,FALSE))stop('Expected bound nonpublishable legacy diagnostic')
 expected<-as.data.frame(audit$expected_contests);checks<-as.data.frame(audit$checks);blocked<-as.data.frame(audit$unavailable)
 legacy_driver_partition(expected,checks,blocked)
 if(any(expected$year!=year)||any(expected$uf!=uf)||any(!expected$office %in% c(3L,5L,6L,7L,8L)))stop('Unexpected requested legacy scope')
 if(nrow(checks)) {
   fields<-c('official_valid_votes','computed_valid_votes','difference','reconciled')
   if(!all(fields %in% names(checks))||anyNA(checks[,fields,drop=FALSE])||
      any(!is.finite(checks$computed_valid_votes))||any(checks$computed_valid_votes<0)||
      any(checks$difference!=0)||any(checks$computed_valid_votes!=checks$official_valid_votes)||any(!checks$reconciled))stop('Invalid legacy certificate')
 }
 if(!'votes' %in% names(places)||anyNA(places$votes)||any(!is.finite(places$votes))||any(places$votes<0)||any(places$votes %% 1!=0))stop('Invalid legacy output votes')
 if(nrow(places)) {
   if(!all(c(legacy_driver_keys,'vote_type') %in% names(places))||anyNA(as.data.frame(places)[,legacy_driver_keys,drop=FALSE])||
      any(!places$vote_type %in% c('candidate','party')))stop('Special or unscoped votes in legacy output')
   row_scope<-do.call(paste,c(as.data.frame(places)[,legacy_driver_keys,drop=FALSE],sep=':'))
   if(any(!row_scope %in% legacy_driver_signature(checks)))stop('Output contains uncertified legacy contest')
 } else row_scope<-character()
 for(i in seq_len(nrow(checks)))if(sum(places$votes[row_scope==legacy_driver_signature(checks[i,,drop=FALSE])])!=checks$computed_valid_votes[i])
   stop('Legacy output differs from audited contest denominator')
 # A zero-valid zone inside a positive contest is retained by the ledger.
 # A whole zero-valid contest has no voting percentage and is not publicable.
 zero<-if(nrow(checks))checks$computed_valid_votes==0 else logical()
 zero_checks<-checks[zero,,drop=FALSE]
 if(any(zero)) {
   z<-checks[zero,legacy_driver_keys,drop=FALSE];z$reason<-'Exact zero valid votes: no percentage denominator; ledger retained diagnostically'
   z$reconciled<-FALSE
   blocked<-as.data.frame(data.table::rbindlist(list(blocked,z),use.names=TRUE,fill=TRUE))
   places<-places[!row_scope %in% legacy_driver_signature(z),]
   checks<-checks[!zero,,drop=FALSE]
 }
 legacy_driver_partition(expected,checks,blocked)
 has_accepted<-nrow(checks)>0L
 if(has_accepted&&(!nrow(places)||sum(places$votes)!=sum(checks$computed_valid_votes)))stop('Empty or inconsistent positive legacy certificate')
 if(!has_accepted&&nrow(places))stop('Uncertified legacy cache must be empty')
 audit$checks<-checks;audit$unavailable<-blocked;audit$zero_valid_diagnostics<-zero_checks
 audit$status<-if(has_accepted)'reconciled' else 'unavailable';audit$publishable<-has_accepted
 audit$election_ids<-unique(checks$election_id)
 audit$method<-'2010/2014 reviewed exact candidate nominal and full zone ledger; proportional final zone zero/full observed pool only; generic party legend never identifies destinations; partial allocations, pending and97 blocked; whole zero-valid contests unavailable'
 if(!is.null(audit$candidate_identity)&&nrow(audit$candidate_identity)) {
   identities<-as.data.frame(audit$candidate_identity)
   sig<-do.call(paste,c(identities[,legacy_driver_keys,drop=FALSE],sep=':'))
   audit$candidate_identity<-identities[sig %in% legacy_driver_signature(checks),,drop=FALSE]
 }
 attr(places,'vote_audit')<-audit
 list(places=places,status=if(nrow(blocked))'partial' else 'complete')
}
legacy_driver_write_json <- function(x,path) {
 dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
 jsonlite::write_json(x,paste0(path,'.part'),auto_unbox=TRUE,pretty=TRUE,na='null')
 if(!file.copy(paste0(path,'.part'),path,overwrite=TRUE))stop('Cannot persist legacy checkpoint')
 unlink(paste0(path,'.part'))
}
legacy_driver_run_id <- function() paste0(Sys.getpid(),'-',format(Sys.time(),'%Y%m%d%H%M%OS6'))
legacy_driver_quarantine <- function(target,cache_dir,run_id) {
 if(!file.exists(target))return(invisible(NULL))
 dir.create(file.path(cache_dir,'superseded'),recursive=TRUE,showWarnings=FALSE)
 backup<-file.path(cache_dir,'superseded',paste0(basename(target),'-',run_id,'.snapshot'))
 if(file.exists(backup)||!file.rename(target,backup))stop('Cannot quarantine superseded legacy certificate')
 invisible(backup)
}
legacy_driver_pid_alive <- function(pid) {
 if(length(pid)!=1L||is.na(pid)||!is.finite(pid)||pid<1L)return(NA)
 if(.Platform$OS.type=='windows') {
   # tools::pskill(signal=0) is NOT a portable Windows liveness probe.
   lines<-tryCatch(system2('tasklist',c('/FI',shQuote(paste('PID eq',as.integer(pid))),'/NH'),stdout=TRUE,stderr=TRUE),error=function(e)NULL)
   if(is.null(lines)||!is.null(attr(lines,'status')))return(NA)
   return(any(grepl(paste0('^\\S+\\s+',as.integer(pid),'\\s'),lines)))
 }
 file.exists(file.path('/proc',as.character(as.integer(pid))))
}
legacy_driver_assert_idle <- function(record) {
 if(is.null(record)||!isTRUE(record$status %in% c('running','launching')))return(invisible(TRUE))
 alive<-legacy_driver_pid_alive(record$worker_pid %||% record$parent_pid)
 if(!identical(alive,FALSE))stop('Legacy UF may have an active worker; verify process and archive its running checkpoint before retrying')
 invisible(TRUE)
}
`%||%` <- function(x,y)if(is.null(x))y else x
legacy_driver_factory <- function(staging) {
 source('shared-pipeline/eleicoes/agregacao/validacao/legacy-2010-2014-environment.R')
 legacy14_environment(probe_dir=staging,publication_mode='diagnostic')
}
legacy_driver_worker <- function(year,uf,input_cache='data-raw/eleicoes-renda/historico',
                                 cache_dir=file.path(input_cache,'legacy-canonic'),factory=legacy_driver_factory,run_id=NULL) {
 if(nrow(legacy_driver_plan(year,uf))!=1L)stop('Worker expects one year/UF')
 uf<-toupper(uf);year<-as.integer(year)
 dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
 output_root<-normalizePath(cache_dir,winslash='/');input_root<-normalizePath(input_cache,winslash='/',mustWork=FALSE)
 if(.Platform$OS.type=='windows') {output_root<-tolower(output_root);input_root<-tolower(input_root)}
 if(identical(output_root,input_root))stop('Output must be isolated from input cache')
 start<-Sys.time();if(is.null(run_id))run_id<-legacy_driver_run_id()
 if(length(run_id)!=1L||!grepl('^[0-9A-Za-z.-]+$',run_id))stop('Invalid legacy run identity')
 target<-file.path(cache_dir,paste0('places-',year,'-',uf,'-3_5_6_7_8-legal.rds'))
 checkpoint<-file.path(cache_dir,'status',as.character(year),paste0(uf,'.json'))
 prior<-if(file.exists(checkpoint))tryCatch(jsonlite::read_json(checkpoint,simplifyVector=TRUE),error=function(e)NULL) else NULL
 if(!(identical(prior$run_id,run_id)&&identical(prior$status,'launching')))legacy_driver_assert_idle(prior)
 base<-list(schema_version=1L,year=year,uf=uf,run_id=run_id,worker_pid=Sys.getpid(),started_at=as.character(start),output=target)
 legacy_driver_write_json(c(base,list(status='running')),checkpoint)
 result<-tryCatch({
   # Remove a preceding certificate from discoverable output BEFORE reads that
   # may fail. Never restore it after parser, missing-input or snapshot failure.
   legacy_driver_quarantine(target,cache_dir,run_id)
   inputs<-file.path(input_cache,c(paste0('votacao_secao_',year,'_',uf,'.zip'),
     paste0(c('votacao_candidato_munzona','votacao_partido_munzona','detalhe_votacao_munzona'),'_',year,'.zip')))
   before<-legacy_driver_identity(c(legacy_driver_sources,inputs))
   staging<-file.path(cache_dir,'staging',as.character(year),uf,run_id)
   dir.create(staging,recursive=TRUE,showWarnings=FALSE)
   env<-factory(staging);data.table::setDTthreads(2L)
   places<-env$legal_reconcile_places(year,uf,c(3L,5L,6L,7L,8L),input_cache)
   raw_md5<-before$md5[before$path==inputs[1L]]
   provenance<-attr(places,'vote_audit')$provenance
   if(!identical(provenance$raw_md5,raw_md5)||length(provenance$official_members)!=3L)stop('Missing legacy selected-member provenance')
   # Hash the actual selected CSVs while they remain in staging, not a later
   # re-extraction. Their recorded hashes must match the bytes used by helper.
   member_paths<-vapply(names(provenance$official_members),function(prefix) {
     z<-provenance$official_members[[prefix]]
     file.path(staging,'extracted',paste0(prefix,'_',year),z$member)
   },character(1))
   members<-legacy_driver_identity(member_paths)
   recorded<-vapply(provenance$official_members,function(z)as.character(z$csv_md5),character(1))
   if(!identical(unname(members$md5),unname(recorded)))stop('Selected official CSV changed after being read')
   validated<-legacy_driver_validate(places,year,uf,raw_md5);places<-validated$places
   staged<-file.path(staging,'certified-output.rds');saveRDS(places,staged)
   restored<-readRDS(staged)
   if(!identical(attr(restored,'vote_audit'),attr(places,'vote_audit'))||!identical(restored$votes,places$votes))stop('Legacy staging serialization differs')
   after<-legacy_driver_identity(c(legacy_driver_sources,inputs))
   if(!legacy_driver_same_snapshot(before,after)||!legacy_driver_same_snapshot(members,legacy_driver_identity(member_paths)))stop('Legacy source snapshot changed during worker')
   # target is absent: rename a complete verified file in the same output tree.
   ready<-paste0(target,'.ready')
   if(!file.copy(staged,ready,overwrite=TRUE)||!identical(unname(tools::md5sum(staged)),unname(tools::md5sum(ready)))||
      !file.rename(ready,target))stop('Cannot install verified isolated legacy output')
   audit<-attr(places,'vote_audit')
   list(status=validated$status,audit_status=audit$status,publishable=audit$publishable,rows=nrow(places),
     certified=audit$checks,unavailable=audit$unavailable,zero_valid_diagnostics=audit$zero_valid_diagnostics,
     fingerprints=rbind(before,members,legacy_driver_identity(target)),staging=staging,method=audit$method)
 },error=function(e)list(status='failed',error=conditionMessage(e)))
 record<-c(base,result,list(finished_at=as.character(Sys.time()),elapsed_seconds=as.numeric(difftime(Sys.time(),start,units='secs'))))
 legacy_driver_write_json(record,checkpoint)
 message(toupper(record$status),' ',year,'/',uf,if(!is.null(record$error))paste0(': ',record$error) else '')
 invisible(gc());record
}
legacy_driver_current <- function(record) {
 if(is.null(record)||!isTRUE(record$status %in% c('complete','partial'))||!identical(as.integer(record$schema_version),1L))return(FALSE)
 tryCatch({
   fingerprints<-as.data.frame(record$fingerprints)
   nrow(fingerprints)>0L&&!anyNA(fingerprints)&&length(record$output)==1L&&record$output %in% fingerprints$path&&
     legacy_driver_same_snapshot(fingerprints,legacy_driver_identity(fingerprints$path))
 },error=function(e)FALSE)
}
legacy_driver_prepare <- function(years,ufs,input_cache='data-raw/eleicoes-renda/historico',
                                  cache_dir=file.path(input_cache,'legacy-canonic'),dry_run=FALSE,rebuild=FALSE) {
 plan<-legacy_driver_plan(years,ufs);output<-list()
 executable<-file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe' else 'Rscript')
 if(.Platform$OS.type=='windows')executable<-shortPathName(executable)
 for(i in seq_len(nrow(plan))) {
   year<-plan$year[i];uf<-plan$uf[i];checkpoint<-file.path(cache_dir,'status',as.character(year),paste0(uf,'.json'))
   read_record<-function()if(file.exists(checkpoint))tryCatch(jsonlite::read_json(checkpoint,simplifyVector=TRUE),error=function(e)NULL) else NULL
   record<-read_record();resume<-!rebuild&&legacy_driver_current(record)
   if(dry_run)state<-if(resume)paste0('cached-',record$status) else 'planned' else {
     if(!resume) {
       legacy_driver_assert_idle(record)
       log<-sub('[.]json$','.log',checkpoint);dir.create(dirname(log),recursive=TRUE,showWarnings=FALSE)
       run_id<-legacy_driver_run_id()
       target<-file.path(cache_dir,paste0('places-',year,'-',uf,'-3_5_6_7_8-legal.rds'))
       # Parent invalidates the preceding certificate even if R cannot start or
       # parse the child. A child-side-only quarantine leaves stale output live.
       legacy_driver_quarantine(target,cache_dir,run_id)
       base<-list(schema_version=1L,year=year,uf=uf,run_id=run_id,parent_pid=Sys.getpid(),output=target)
       legacy_driver_write_json(c(base,list(status='launching')),checkpoint)
       code<-tryCatch(suppressWarnings(system2(executable,c('--vanilla',shQuote(legacy_driver_script),'--worker',year,uf,
         '--input-cache',shQuote(input_cache),'--cache-dir',shQuote(cache_dir),'--run-id',shQuote(run_id)),stdout=log,stderr=log)),error=function(e)127L)
       record<-read_record()
       fresh<-!is.null(record)&&identical(record$run_id,run_id)&&identical(as.integer(record$year),year)&&identical(record$uf,uf)
       accepted<-identical(as.integer(code),0L)&&fresh&&identical(record$output,target)&&
         record$status %in% c('complete','partial')&&legacy_driver_current(record)
       if(!accepted) {
         detail<-if(fresh&&!is.null(record$error))paste0(': ',record$error) else ''
         legacy_driver_quarantine(target,cache_dir,paste0(run_id,'-rejected'))
         record<-c(base,list(status='failed',exit_code=as.integer(code),error=paste0('Worker did not produce a fresh current certificate; exit ',code,detail)))
         legacy_driver_write_json(record,checkpoint)
       }
     }
     state<-record$status
   }
   output[[i]]<-data.frame(year=year,uf=uf,status=state)
   message('LEGACY ',year,'/',uf,' ',state)
 }
 do.call(rbind,output)
}
if(sys.nframe()==0L) {
 args<-commandArgs(trailingOnly=TRUE);input_cache<-'data-raw/eleicoes-renda/historico';cache_dir<-NULL;run_id<-NULL
 for(flag in c('--input-cache','--cache-dir','--run-id')) {
   at<-which(args==flag)
   if(length(at)) {
     if(length(at)!=1L||at==length(args)||grepl('^--',args[at+1L]))stop(flag,' requires a directory')
     if(flag=='--input-cache')input_cache<-args[at+1L] else if(flag=='--cache-dir')cache_dir<-args[at+1L] else run_id<-args[at+1L]
     args<-args[-c(at,at+1L)]
   }
 }
 if(is.null(cache_dir))cache_dir<-file.path(input_cache,'legacy-canonic')
 flags<-c('--worker','--dry-run','--rebuild')
 if(any(grepl('^--',args)&!args %in% flags))stop('Unknown legacy option')
 values<-args[!grepl('^--',args)]
 if(length(values)!=2L)stop('Explicit YEARS and UFs required; e.g. 2014 AC,DF')
 years<-as.integer(strsplit(values[1L],',',fixed=TRUE)[[1L]]);ufs<-strsplit(values[2L],',',fixed=TRUE)[[1L]]
 if('--worker' %in% args) {
   result<-legacy_driver_worker(years,ufs,input_cache,cache_dir,run_id=run_id)
   quit(status=if(result$status=='failed')1L else 0L)
 }
 result<-legacy_driver_prepare(years,ufs,input_cache,cache_dir,dry_run='--dry-run' %in% args,rebuild='--rebuild' %in% args)
 print(result,row.names=FALSE)
 if(!'--dry-run' %in% args)quit(status=if(any(result$status=='failed'))1L else if(any(result$status=='partial'))2L else 0L)
}
