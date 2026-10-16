source('shared-pipeline/eleicoes/historico/historical-contest-isolation.R')
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
folder<-tempfile('historical-isolation-test-');dir.create(folder)
zip<-file.path(folder,'official.zip');writeLines('fixture',zip)
raw<-data.table(year=2018L,round=c(1L,1L,2L),election_id=c(297L,297L,298L),uf='AC',office=c(6L,7L,3L),
 municipality_tse='1',zone=1L,place=1L,place_key='AC:1:1:1',candidate_id=c(1313L,13131L,13L),
 candidate_name='Candidate',vote_type='candidate',votes=c(50,70,100))
attr(raw,'normalizer_version')<-2L;attr(raw,'source_md5')<-unname(tools::md5sum(zip))
saveRDS(raw,file.path(folder,'places-2018-AC-3_5_6_7_8.rds'))
detail<-raw[,.(ANO_ELEICAO=year,NR_TURNO=round,CD_ELEICAO=election_id,CD_CARGO=office,SG_UF=uf,CD_TIPO_ELEICAO=2L)]
detail_path<-file.path(folder,'detail.csv');fwrite(detail,detail_path)
env<-new.env(parent=globalenv());env$zip<-zip;env$detail_path<-detail_path;env$poison<-FALSE;env$all_blocked<-FALSE
env$tse_download<-function(...)zip;environment(env$tse_download)<-env
env$legal_resource_csv<-function(...)detail_path;environment(env$legal_resource_csv)<-env
env$historical_null_sections<-function(...)data.table(office=c(3L,6L,7L),election_id=c(298L,297L,297L),section_nulls=0)
env$legal_reconcile_places<-function(year,uf,offices,cache_dir) {
 x<-readRDS(file.path(cache_dir,paste0('places-',year,'-',uf,'-',offices,'.rds')))
 stopifnot(nrow(x)==1L,x$office==offices)
 if(all_blocked)stop('All disputes explicitly blocked in fixture')
 if(offices==7L)stop('Partial conversion: fixture blocks state deputy only')
 a<-x[,.(year,round,election_id,office,uf,official_valid_votes=sum(votes),computed_valid_votes=sum(votes),difference=0,zones=1L,reconciled=TRUE)]
 attr(x,'vote_audit')<-list(status='reconciled',method='Fixture exact method',checks=as.data.frame(a),unavailable=data.frame())
 if(poison && offices==6L)x[,votes:=votes+1]
 x
};environment(env$legal_reconcile_places)<-env
original_resource<-env$legal_resource_csv;original_nulls<-env$historical_null_sections;original_download<-env$tse_download
env<-historical_isolation_environment(env)
x<-env$legal_reconcile_places(2018L,'AC',cache_dir=folder)
a<-attr(x,'vote_audit')
stopifnot(sum(x$votes)==150,nrow(a$checks)==2L,nrow(a$unavailable)==1L,a$unavailable$office==7L,
 grepl('Partial conversion',a$unavailable$reason),identical(env$legal_resource_csv,original_resource),
 identical(env$historical_null_sections,original_nulls),identical(env$tse_download,original_download))
# A helper returning a misleading certificate must never enter the final cache.
env$poison<-TRUE;x<-env$legal_reconcile_places(2018L,'AC',cache_dir=folder);a<-attr(x,'vote_audit')
stopifnot(sum(x$votes)==100,nrow(a$checks)==1L,nrow(a$unavailable)==2L,
 any(grepl('votes differ',a$unavailable$reason)),file.exists(zip),nrow(readRDS(file.path(folder,'places-2018-AC-3_5_6_7_8.rds')))==3L)
# A previous valid canonical cache must not remain publishable on total failure.
env$all_blocked<-TRUE
err<-tryCatch({env$legal_reconcile_places(2018L,'AC',cache_dir=folder);NULL},error=conditionMessage)
stale<-readRDS(file.path(folder,'places-2018-AC-3_5_6_7_8-legal.rds'))
stopifnot(grepl('No reconciled historical contest',err),nrow(stale)==0L,
 attr(stale,'vote_audit')$status=='unreconciled',nrow(attr(stale,'vote_audit')$unavailable)==3L,
 length(list.files(folder,pattern='superseded.*[.]rds$'))==1L)
message('PASS: independent contest scope, explicit blocked dispute, unchanged raw/source, private overrides restored, false certificate rejected')
