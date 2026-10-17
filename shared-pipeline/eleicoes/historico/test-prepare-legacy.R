source('shared-pipeline/eleicoes/historico/prepare-legacy-all-ufs.R')
library(data.table);setDTthreads(1L)
reject<-function(expr)stopifnot(inherits(try(expr,silent=TRUE),'try-error'))
stopifnot(identical(unique(legacy_driver_plan(c(2010L,2014L),c('AC','DF'))$year),c(2014L,2010L)))
reject(legacy_driver_plan(2024L,'AC'));reject(legacy_driver_plan(2014L,'ZZ'))
root<-tempfile('legacy-driver-fixture-');dir.create(root)
input<-file.path(root,'inputs');dir.create(input)
out<-file.path(root,'legacy-canonic')
prefixes<-c('votacao_candidato_munzona','votacao_partido_munzona','detalhe_votacao_munzona')
rawzip<-file.path(input,'votacao_secao_2014_AC.zip')
writeLines('fixture raw archive',rawzip)
for(prefix in prefixes)writeLines(paste('fixture',prefix),file.path(input,paste0(prefix,'_2014.zip')))
expected<-data.frame(year=2014L,round=1L,election_id=143L,office=c(6L,7L),uf='AC')
mode<-'partial'
factory<-function(staging) {
 env<-new.env()
 env$legal_reconcile_places<-function(year,uf,offices,cache_dir) {
   if(mode=='parser-error')stop('Synthetic parser failure before reconciliation')
   checks<-expected
   checks$official_valid_votes<-checks$computed_valid_votes<-c(50,70)
   checks$difference<-0;checks$reconciled<-TRUE
   blocked<-expected[0,];blocked$reason<-character()
   if(mode!='complete') {
     blocked<-expected[2,,drop=FALSE];blocked$reason<-'Ambiguous final legend'
     checks<-checks[1,,drop=FALSE]
   }
   if(mode=='allblocked') {
     checks<-checks[0,];blocked<-expected;blocked$reason<-'Explicit ambiguous final legend'
   }
   if(mode=='zero')checks$official_valid_votes<-checks$computed_valid_votes<-0
   places<-as.data.table(checks[,legacy_driver_keys,drop=FALSE])
   places[,`:=`(vote_type='candidate',votes=checks$computed_valid_votes)]
   if(mode=='zero')places<-places[0]
   selected<-list()
   for(prefix in prefixes) {
     member<-paste0(prefix,'_2014_AC.csv')
     folder<-file.path(staging,'extracted',paste0(prefix,'_2014'));dir.create(folder,recursive=TRUE)
     path<-file.path(folder,member);writeLines(paste('used official',prefix),path)
     selected[[prefix]]<-list(member=member,csv_md5=unname(tools::md5sum(path)))
   }
   attr(places,'normalizer_version')<-2L
   attr(places,'source_md5')<-unname(tools::md5sum(rawzip))
   attr(places,'vote_audit')<-list(status='diagnostic_only',publishable=FALSE,
     expected_contests=expected,checks=checks,unavailable=blocked,
     provenance=list(raw_md5=attr(places,'source_md5'),official_members=selected))
   if(mode=='missing')attr(places,'vote_audit')$unavailable<-blocked[0,,drop=FALSE]
   if(mode=='overlap')attr(places,'vote_audit')$unavailable<-transform(expected,reason='overlapping')
   if(mode=='unexpected')attr(places,'vote_audit')$unavailable$office<-8L
   if(mode=='mutate-zip')writeLines('changed archive after read',rawzip)
   if(mode=='mutate-csv')writeLines('changed selected member',path)
   places
 };env
}
run<-function()legacy_driver_worker(2014L,'AC',input,out,factory)
target<-file.path(out,'places-2014-AC-3_5_6_7_8-legal.rds')
r<-run();stopifnot(r$status=='partial',r$audit_status=='reconciled',r$publishable,r$rows==1L,
 file.exists(target),sum(readRDS(target)$votes)==50)
checkpoint<-file.path(out,'status/2014/AC.json')
stored<-jsonlite::read_json(checkpoint,simplifyVector=TRUE)
stopifnot(legacy_driver_current(stored))
mode<-'complete';r<-run();stopifnot(r$status=='complete',r$rows==2L)
mode<-'parser-error';r<-run();stopifnot(r$status=='failed',!file.exists(target),grepl('parser failure',r$error),
 length(list.files(file.path(out,'superseded')))==2L)
mode<-'complete';r<-run();stopifnot(r$status=='complete',file.exists(target))
mode<-'mutate-zip';r<-run();stopifnot(r$status=='failed',!file.exists(target),grepl('snapshot changed',r$error))
mode<-'allblocked';r<-run();a<-attr(readRDS(target),'vote_audit')
stopifnot(r$status=='partial',r$rows==0L,r$audit_status=='unavailable',!r$publishable,
 a$status=='unavailable',nrow(a$checks)==0L,nrow(a$unavailable)==2L)
mode<-'zero';r<-run();a<-attr(readRDS(target),'vote_audit')
stopifnot(r$status=='partial',r$rows==0L,!r$publishable,a$status=='unavailable',
 nrow(a$zero_valid_diagnostics)==1L,nrow(a$unavailable)==2L,
 a$zero_valid_diagnostics$computed_valid_votes==0,
 any(grepl('zero valid votes',a$unavailable$reason)))
for(mode in c('missing','overlap','unexpected')) {
 r<-run();stopifnot(r$status=='failed',!file.exists(target),grepl('partition|overlaps',r$error))
}
mode<-'mutate-csv';r<-run();stopifnot(r$status=='failed',!file.exists(target),grepl('CSV changed',r$error))
mode<-'complete';r<-run();stopifnot(r$status=='complete')
unlink(rawzip)
r<-run();stopifnot(r$status=='failed',!file.exists(target),grepl('Missing input',r$error))
stopifnot(all(legacy_driver_prepare(2014L,'AC',input,out,dry_run=TRUE)$status=='planned'))
reject(legacy_driver_worker(2014L,'AC',input,input,factory))
# Check actual child invocation with independently written tiny worker.
fake<-file.path(root,'fake-worker.R')
writeLines(c(
 'a<-commandArgs(trailingOnly=TRUE)',
 'pick<-function(flag)a[match(flag,a)+1L]',
 'input<-pick("--input-cache");out<-pick("--cache-dir")',
 'year<-as.integer(a[2]);uf<-a[3]',
 'p<-file.path(out,"status",as.character(year),paste0(uf,".json"))',
 'dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)',
 'target<-file.path(out,paste0("places-",year,"-",uf,"-3_5_6_7_8-legal.rds"))',
 'saveRDS(data.frame(votes=50),target)',
 'fingerprints<-data.frame(path=target,bytes=file.info(target)$size,md5=unname(tools::md5sum(target)))',
 'state<-if(file.exists(file.path(input,"partial")))"partial" else "complete"',
 'run_id<-pick("--run-id");if(file.exists(file.path(input,"stale-id")))run_id<-"old-run"',
 'jsonlite::write_json(list(schema_version=1L,year=year,uf=uf,run_id=run_id,output=target,status=state,fingerprints=fingerprints),p,auto_unbox=TRUE)',
 'if(file.exists(file.path(input,"mutate")))saveRDS(data.frame(votes=51),target)',
 'if(file.exists(file.path(input,"nonzero")))quit(status=23L)',
 'jsonlite::write_json(list(input=input,output=out),file.path(out,"child-arguments.json"),auto_unbox=TRUE)'
),fake)
original_script<-legacy_driver_script;legacy_driver_script<-fake
child_input<-file.path(root,'input cache with spaces');dir.create(child_input)
child_output<-file.path(root,'output cache with spaces')
child<-legacy_driver_prepare(2014L,'AC',child_input,child_output)
args_used<-jsonlite::read_json(file.path(child_output,'child-arguments.json'),simplifyVector=TRUE)
stopifnot(child$status=='complete',identical(args_used$input,child_input),identical(args_used$output,child_output))
child_target<-file.path(child_output,'places-2014-AC-3_5_6_7_8-legal.rds')
child_checkpoint<-file.path(child_output,'status/2014/AC.json')
read_child<-function()jsonlite::read_json(child_checkpoint,simplifyVector=TRUE)
stopifnot(legacy_driver_current(read_child()))
# Partial is a successful worker result: it exits zero, unlike the summary CLI
# which may return two to describe a completed batch with unavailable contests.
writeLines('partial',file.path(child_input,'partial'))
child<-legacy_driver_prepare(2014L,'AC',child_input,child_output,rebuild=TRUE)
stopifnot(child$status=='partial',legacy_driver_current(read_child()))
unlink(file.path(child_input,'partial'))
for(marker in c('nonzero','stale-id','mutate')) {
 writeLines(marker,file.path(child_input,marker))
 child<-legacy_driver_prepare(2014L,'AC',child_input,child_output,rebuild=TRUE)
 stopifnot(child$status=='failed',!file.exists(child_target),read_child()$status=='failed')
 unlink(file.path(child_input,marker))
 child<-legacy_driver_prepare(2014L,'AC',child_input,child_output,rebuild=TRUE)
 stopifnot(child$status=='complete',file.exists(child_target))
}
# Failure before any worker checkpoint must never reuse a preceding certificate.
for(body in c('quit(status=17L)','stop("external startup failure")','invalid R syntax %%%','quit(status=0L)')) {
 bad<-file.path(root,'external-failure.R');writeLines(body,bad)
 legacy_driver_script<-bad
 child<-legacy_driver_prepare(2014L,'AC',child_input,child_output,rebuild=TRUE)
 stopifnot(child$status=='failed',!file.exists(child_target),!legacy_driver_current(read_child()),
   if(body=='quit(status=0L)')read_child()$exit_code==0L else read_child()$exit_code!=0L)
 legacy_driver_script<-fake
 child<-legacy_driver_prepare(2014L,'AC',child_input,child_output,rebuild=TRUE)
 stopifnot(child$status=='complete',file.exists(child_target))
}
# No quarantine or checkpoint mutation is allowed while an identified worker
# is alive, or while an old running checkpoint lacks a provable process ID.
for(pid in list(Sys.getpid(),NULL)) {
 running<-read_child();running$status<-'running';running$worker_pid<-pid
 legacy_driver_write_json(running,child_checkpoint)
 hash<-unname(tools::md5sum(c(child_target,child_checkpoint)))
 reject(legacy_driver_prepare(2014L,'AC',child_input,child_output,rebuild=TRUE))
 stopifnot(identical(hash,unname(tools::md5sum(c(child_target,child_checkpoint)))))
}
legacy_driver_script<-original_script
cat('PASS: isolated legacy driver staging, exact partition/denominators, complete/partial, zero/all-unavailable diagnostics, stale certificate quarantine, parser/missing-source failure, changed ZIP rejection, current fingerprints, fresh child identity, external startup/parser failure, nonzero exit, live-worker guard; no real data.\n')
