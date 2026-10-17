source('shared-pipeline/eleicoes/historico/legal-historical-null-totals.R')
source('shared-pipeline/eleicoes/historico/historical-senate-nonvalid.R')
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
folder<-tempfile('historical-senate-test-');dir.create(folder)
zip<-file.path(folder,'official.zip');writeLines('fixture',zip)
base<-data.table(ANO_ELEICAO=2018L,NR_TURNO=1L,CD_ELEICAO=297L,SG_UF='AP',CD_MUNICIPIO=1L,NR_ZONA=1L,CD_CARGO=5L)
raw<-data.table(year=2018L,round=1L,election_id=297L,election_date='07/10/2018',uf='AP',municipality_tse='1',zone=1L,
 place=c(1L,1L,2L),place_key=c('AP:1:1:1','AP:1:1:1','AP:1:1:2'),office=5L,candidate_id=c(123L,212L,231L),
 candidate_name=c('Valid','Excluded A','Excluded B'),vote_type='candidate',votes=c(50,2,3))
attr(raw,'source_md5')<-unname(tools::md5sum(zip));attr(raw,'normalizer_version')<-2L
save_raw<-function()saveRDS(raw,file.path(folder,'places-2018-AP-5.rds'));save_raw()
cand<-copy(base)[,`:=`(SQ_CANDIDATO='1',NR_CANDIDATO=123L,NR_PARTIDO=12L,SG_PARTIDO='PDT',
 NM_TIPO_DESTINACAO_VOTOS='Valido',QT_VOTOS_NOMINAIS=50,QT_VOTOS_NOMINAIS_VALIDOS=50)]
party<-copy(base)[,`:=`(NR_PARTIDO=12L,QT_VOTOS_LEGENDA_VALIDOS=0,QT_VOTOS_LEGENDA_ANUL_SUBJUD=0,QT_VOTOS_NOMINAIS_CONVR_LEG=0)]
detail<-copy(base)[,`:=`(QT_TOTAL_VOTOS_VALIDOS=50,QT_VOTOS_NOMINAIS_VALIDOS=50,QT_TOTAL_VOTOS_LEG_VALIDOS=0,
 QT_TOTAL_VOTOS_NULOS=8,QT_TOTAL_VOTOS_ANULADOS=2,QT_TOTAL_VOTOS_ANUL_SUBJUD=0,QT_VOTOS_ANULADOS_APU_SEP=0)]
write_sources<-function() {
 fwrite(cand,file.path(folder,'votacao_candidato_munzona.csv'));fwrite(party,file.path(folder,'votacao_partido_munzona.csv'))
 fwrite(detail,file.path(folder,'detalhe_votacao_munzona.csv'))
};write_sources()
env<-historical_senate_nonvalid_environment(historical_null_environment())
env$tse_download<-function(...)zip
env$legal_resource_csv<-function(year,dataset_prefix,uf,cache_dir)file.path(folder,paste0(dataset_prefix,'.csv'))
env$historical_null_sections<-function(...)data.table(year=2018L,round=1L,election_id=297L,uf='AP',municipality_tse='1',zone=1L,office=5L,section_nulls=5)
offices<-5L
run<-function()env$legal_reconcile_places(2018L,'AP',offices,folder)
reject<-function(pattern) {e<-tryCatch({run();NULL},error=conditionMessage);stopifnot(is.character(e),grepl(pattern,e))}
x<-run();a<-fread(file.path(folder,'places-2018-AP-5-legal-audit.csv'))
stopifnot(sum(x$votes)==50,a$nonvalid_combined_computed==10,a$nonvalid_combined_official==10,
 a$official_null_component==8,a$official_annulled_component==2,
 !any(c('null_total_computed','computed_annulled') %in% names(a)),grepl('NOT inferred',attr(x,'vote_audit')$method))
# Only the combined identity is certified. The adapter does not claim which
# excluded candidate belongs to each separate judicial category.
detail[,`:=`(QT_TOTAL_VOTOS_NULOS=7,QT_TOTAL_VOTOS_ANULADOS=3)];write_sources();stopifnot(sum(run()$votes)==50)
detail[,QT_TOTAL_VOTOS_ANULADOS:=4];write_sources();reject('No reconciled contest')
detail[,QT_TOTAL_VOTOS_ANULADOS:=3]
detail[,QT_TOTAL_VOTOS_ANUL_SUBJUD:=1];write_sources();reject('pending/annulled')
detail[,QT_TOTAL_VOTOS_ANUL_SUBJUD:=0]
detail[,CD_CARGO:=6L];write_sources()
saveRDS(raw,file.path(folder,'places-2018-AP-5_6.rds'));offices<-c(5L,6L)
reject('restricted to identified')
message('PASS: combined null-plus-annulled ledger, truthful separate official components, no candidate category inference, one-vote rejection, pending and non-Senate guards')
