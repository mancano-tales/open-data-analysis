source('shared-pipeline/eleicoes/historico/legal-historical-null-totals.R')
source('shared-pipeline/eleicoes/historico/historical-party-aggregates.R')
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
b<-data.table(ANO_ELEICAO=2018L,NR_TURNO=1L,CD_ELEICAO=297L,SG_UF='AM',CD_MUNICIPIO=2003L,NR_ZONA=67L,CD_CARGO=6L)
p<-b[rep(1L,2L)][,`:=`(NR_PARTIDO=13L,ST_VOTO_EM_TRANSITO='N',SQ_COLIGACAO=c('coalition','isolated'),
 QT_VOTOS_LEGENDA_VALIDOS=10,QT_VOTOS_NOMINAIS_CONVR_LEG=7,QT_TOTAL_VOTOS_LEG_VALIDOS=17,
 QT_VOTOS_LEGENDA_ANUL_SUBJUD=0,QT_VOTOS_NOMINAIS_ANUL_SUBJUD=0,QT_VOTOS_NOMINAIS_VALIDOS=c(10,0))]
before<-copy(p);s<-historical_party_split(p)
stopifnot(nrow(s$legend)==1L,s$legend$QT_VOTOS_LEGENDA_VALIDOS==10,
 identical(s$nominal_source$QT_VOTOS_NOMINAIS_VALIDOS,c(10,0)),nrow(s$nominal_source)==2L,
 isTRUE(all.equal(as.data.frame(p),as.data.frame(before),check.attributes=FALSE)))
# Equal transit partitions are distinct contributions, not duplicates.
transit<-rbind(p,copy(p)[,ST_VOTO_EM_TRANSITO:='S']);t<-historical_party_split(transit)
stopifnot(nrow(t$legend)==2L,sum(t$legend$QT_VOTOS_LEGENDA_VALIDOS)==20,
 sum(t$legend$QT_VOTOS_NOMINAIS_CONVR_LEG)==14,nrow(t$nominal_source)==4L)
reject<-function(expr,pattern) {e<-tryCatch({force(expr);NULL},error=conditionMessage);stopifnot(is.character(e),grepl(pattern,e))}
conflict<-copy(p);conflict[2L,QT_VOTOS_LEGENDA_VALIDOS:=11]
reject(historical_party_split(conflict),'Conflicting explicit')
reject(historical_party_split(copy(p)[,ANO_ELEICAO:=2014L]),'restricted to the 2018')

folder<-tempfile('party-aggregate-test-');dir.create(folder);zip<-file.path(folder,'source.zip');writeLines('fixture',zip)
raw<-data.table(year=2018L,round=1L,election_id=297L,election_date='07/10/2018',uf='AM',municipality_tse='2003',zone=67L,
 place=c(1L,2L,1L,1L,2L),place_key=paste0('AM:2003:67:',c(1L,2L,1L,1L,2L)),office=6L,
 candidate_id=c(1313L,1314L,2211L,13L,13L),candidate_name='Fixture',vote_type=c('candidate','candidate','candidate','party','party'),votes=c(50,7,3,4,6))
attr(raw,'normalizer_version')<-2L;attr(raw,'source_md5')<-unname(tools::md5sum(zip))
saveRDS(raw,file.path(folder,'places-2018-AM-6.rds'))
cand<-b[rep(1L,2L)][,`:=`(SQ_CANDIDATO=c('1','2'),NR_CANDIDATO=c(1313L,1314L),NR_PARTIDO=13L,SG_PARTIDO='PT',
 NM_TIPO_DESTINACAO_VOTOS=c('Valido','Legenda'),QT_VOTOS_NOMINAIS=c(50,7),QT_VOTOS_NOMINAIS_VALIDOS=c(50,0))]
detail<-copy(b)[,`:=`(QT_TOTAL_VOTOS_VALIDOS=67,QT_VOTOS_NOMINAIS_VALIDOS=50,QT_TOTAL_VOTOS_LEG_VALIDOS=17,
 QT_TOTAL_VOTOS_NULOS=8,QT_TOTAL_VOTOS_ANULADOS=0,QT_TOTAL_VOTOS_ANUL_SUBJUD=0,QT_VOTOS_ANULADOS_APU_SEP=0)]
p[,QT_VOTOS_NOMINAIS_VALIDOS:=c(50,0)]
write_sources<-function() {
 fwrite(cand,file.path(folder,'votacao_candidato_munzona.csv'));fwrite(p,file.path(folder,'votacao_partido_munzona.csv'))
 fwrite(detail,file.path(folder,'detalhe_votacao_munzona.csv'))
};write_sources()
env<-historical_party_aggregate_environment(historical_null_environment())
env$tse_download<-function(...)zip
env$legal_resource_csv<-function(year,dataset_prefix,uf,cache_dir)file.path(folder,paste0(dataset_prefix,'.csv'))
env$historical_null_sections<-function(...)data.table(year=2018L,round=1L,election_id=297L,uf='AM',municipality_tse='2003',zone=67L,office=6L,section_nulls=5)
run<-function()env$legal_reconcile_places(2018L,'AM',6L,folder)
x<-run();stopifnot(sum(x$votes)==67,x[vote_type=='party',sum(votes)]==17,
 x[place==2L & vote_type=='party',sum(votes)]==13,all(attr(x,'vote_audit')$checks$difference==0))
p[2L,QT_VOTOS_LEGENDA_VALIDOS:=11];write_sources();reject(run(),'Conflicting explicit')
p[2L,QT_VOTOS_LEGENDA_VALIDOS:=10];p[2L,QT_VOTOS_NOMINAIS_ANUL_SUBJUD:=1];write_sources()
reject(run(),'original nominal pending')
message('PASS: strict party-zone-transit dependency, additive N/S partitions, original nominal rows preserved, exact conversion/direct/null ledger; conflicting and pending sources rejected')
