source('shared-pipeline/eleicoes/historico/legal-historical-null-totals.R')
source('shared-pipeline/eleicoes/historico/historical-senate-nonvalid.R')
source('shared-pipeline/eleicoes/historico/historical-separate-votes.R')
source('shared-pipeline/eleicoes/historico/historical-party-aggregates.R')
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
folder<-tempfile('historical-separate-test-');dir.create(folder)
b<-data.table(ANO_ELEICAO=2018L,NR_TURNO=1L,CD_ELEICAO=297L,SG_UF='BA',CD_MUNICIPIO=38490L,NR_ZONA=18L,CD_CARGO=3L)
section<-b[rep(1L,5L)][,`:=`(NR_VOTAVEL=c(13L,22L,96L,97L,95L),QT_VOTOS=c(50,3,5,4,2))]
section<-rbind(section,copy(section[NR_VOTAVEL==97L])[,`:=`(ANO_ELEICAO=2014L,QT_VOTOS=777)])
member<-'votacao_secao_2018_BA.csv';fwrite(section,file.path(folder,member),sep=';')
zip<-file.path(folder,'votacao_secao_2018_BA.zip');zip::zipr(zip,member,root=folder)
observed<-historical_section_categories(zip,2018L,'BA',3L,297L)
stopifnot(nrow(observed)==1L,observed$section_nulls==5,observed$section_separate==4)
# Replacing the archive under the same name must refresh an existing CSV.
section[ANO_ELEICAO==2018L & NR_VOTAVEL==97L,QT_VOTOS:=6]
fwrite(section,file.path(folder,member),sep=';');zip::zipr(zip,member,root=folder)
changed<-historical_section_categories(zip,2018L,'BA',3L,297L)
stopifnot(nrow(changed)==1L,changed$section_separate==6)
section[ANO_ELEICAO==2018L & NR_VOTAVEL==97L,QT_VOTOS:=4]
fwrite(section,file.path(folder,member),sep=';');zip::zipr(zip,member,root=folder)
raw<-data.table(year=2018L,round=1L,election_id=297L,election_date='07/10/2018',uf='BA',municipality_tse='38490',zone=18L,
 place=c(1376L,1236L),place_key=c('BA:38490:18:1376','BA:38490:18:1236'),office=3L,
 candidate_id=c(13L,22L),candidate_name=c('Valid','Excluded'),vote_type='candidate',votes=c(50,3))
attr(raw,'normalizer_version')<-2L;attr(raw,'source_md5')<-unname(tools::md5sum(zip))
saveRDS(raw,file.path(folder,'places-2018-BA-3.rds'))
cand<-copy(b)[,`:=`(SQ_CANDIDATO='1',NR_CANDIDATO=13L,NR_PARTIDO=13L,SG_PARTIDO='PT',
 NM_TIPO_DESTINACAO_VOTOS='Valido',QT_VOTOS_NOMINAIS=50,QT_VOTOS_NOMINAIS_VALIDOS=50)]
party<-copy(b)[,`:=`(NR_PARTIDO=13L,ST_VOTO_EM_TRANSITO='N',QT_VOTOS_LEGENDA_VALIDOS=0,QT_VOTOS_NOMINAIS_CONVR_LEG=0,
 QT_TOTAL_VOTOS_LEG_VALIDOS=0,QT_VOTOS_LEGENDA_ANUL_SUBJUD=0,QT_VOTOS_NOMINAIS_ANUL_SUBJUD=0)]
detail<-copy(b)[,`:=`(QT_TOTAL_VOTOS_VALIDOS=50,QT_VOTOS_NOMINAIS_VALIDOS=50,QT_TOTAL_VOTOS_LEG_VALIDOS=0,
 QT_TOTAL_VOTOS_NULOS=8,QT_TOTAL_VOTOS_ANULADOS=0,QT_TOTAL_VOTOS_ANUL_SUBJUD=0,QT_VOTOS_ANULADOS_APU_SEP=4)]
write_sources<-function() {
 fwrite(cand,file.path(folder,'votacao_candidato_munzona.csv'));fwrite(party,file.path(folder,'votacao_partido_munzona.csv'))
 fwrite(detail,file.path(folder,'detalhe_votacao_munzona.csv'))
};write_sources()
env<-historical_separate_vote_environment(historical_senate_nonvalid_environment(historical_null_environment()))
# Verify composition with the independent aggregate projection too.
env<-historical_party_aggregate_environment(env)
env$tse_download<-function(...)zip
env$legal_resource_csv<-function(year,dataset_prefix,uf,cache_dir)file.path(folder,paste0(dataset_prefix,'.csv'))
run<-function()env$legal_reconcile_places(2018L,'BA',3L,folder)
reject<-function(pattern) {e<-tryCatch({run();NULL},error=conditionMessage);stopifnot(is.character(e),grepl(pattern,e))}
year_error<-tryCatch({historical_section_categories(zip,2014L,'BA',3L,297L);NULL},error=conditionMessage)
stopifnot(grepl('restricted to the 2018',year_error))
x<-run();a<-fread(file.path(folder,'places-2018-BA-3-legal-audit.csv'));audit<-attr(x,'vote_audit')
stopifnot(sum(x$votes)==50,a$computed_separate_votes==4,a$official_separate_votes==4,
 a$nonvalid_combined_computed==8,a$nonvalid_combined_official==8,audit$checks$separate_difference==0,
 !grepl('pending/separate categories blocked',audit$method,fixed=TRUE),grepl('NEVER added to nulls',audit$method,fixed=TRUE))
detail[,QT_VOTOS_ANULADOS_APU_SEP:=5];write_sources();reject('No reconciled contest')
detail[,QT_VOTOS_ANULADOS_APU_SEP:=4]
detail[,QT_TOTAL_VOTOS_NULOS:=9];write_sources();reject('No reconciled contest')
detail[,QT_TOTAL_VOTOS_NULOS:=8]
detail[,QT_TOTAL_VOTOS_ANUL_SUBJUD:=1];write_sources();reject('pending/annulled')
message('PASS: real ZIP96/97 reader, explicit year filter and archive-MD5 refresh, separate97/APU_SEP audit, unchanged null/valid counts, aggregate-adapter composition; one-vote and pending failures rejected')
