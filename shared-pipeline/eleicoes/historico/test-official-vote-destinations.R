source('shared-pipeline/eleicoes/historico/legal-historical-null-totals.R')
source('shared-pipeline/eleicoes/historico/official-vote-destinations.R')
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
folder<-tempfile('official-destination-test-');dir.create(folder)
pdf<-file.path(folder,'fixture.pdf');pdf_bytes<-charToRaw('%PDF-1.4\nSynthetic fixture document, not a primary report.');writeBin(pdf_bytes,pdf)
source_row<-list(id='fixture',url='https://www.tse.jus.br/fixture.pdf',source_date='2022-12-05',cache_path='fixture.pdf',
 sha256=digest::digest(file=pdf,algo='sha256'),bytes=length(pdf_bytes))
mapping<-list(schema_version=1L,sources=list(source_row),mappings=list(
 list(year=2018L,uf='RO',round=1L,office=7L,candidate_id=15800L,party_id=15L,destination='party',source_destination='Valido (legenda)',official_total_votes=7,source_id='fixture',source_page=1L,source_total_page=2L),
 list(year=2018L,uf='RO',round=1L,office=7L,candidate_id=15000L,party_id=15L,destination='null',source_destination='Nulo',official_total_votes=3,source_id='fixture',source_page=1L,source_total_page=2L)))
mapping_path<-file.path(folder,'mapping.json')
save_mapping<-function()jsonlite::write_json(mapping,mapping_path,auto_unbox=TRUE,pretty=TRUE);save_mapping()
loaded<-official_destination_load(2018L,'RO',7L,1L,folder,mapping_path)
stopifnot(nrow(loaded)==2L,nrow(official_destination_load(2018L,'AL',7L,1L,folder,mapping_path))==0L)
zip<-file.path(folder,'source.zip');writeLines('fixture raw source',zip)
base<-data.table(ANO_ELEICAO=2018L,NR_TURNO=1L,CD_ELEICAO=297L,SG_UF='RO',CD_MUNICIPIO=1L,NR_ZONA=1L,CD_CARGO=7L)
raw<-data.table(year=2018L,round=1L,election_id=297L,election_date='07/10/2018',uf='RO',municipality_tse='1',zone=1L,
 place=c(1L,1L,2L,1L,2L,1L,2L),place_key=paste0('RO:1:1:',c(1L,1L,2L,1L,2L,1L,2L)),office=7L,
 candidate_id=c(15123L,15800L,15800L,15000L,15000L,15L,15L),candidate_name='Fixture',
 vote_type=c(rep('candidate',5L),'party','party'),votes=c(50,2,5,1,2,4,6))
attr(raw,'normalizer_version')<-2L;attr(raw,'source_md5')<-unname(tools::md5sum(zip))
saveRDS(raw,file.path(folder,'places-2018-RO-7.rds'))
cand<-copy(base)[,`:=`(SQ_CANDIDATO='1',NR_CANDIDATO=15123L,NR_PARTIDO=15L,SG_PARTIDO='MDB',
 NM_TIPO_DESTINACAO_VOTOS='Valido',QT_VOTOS_NOMINAIS=50,QT_VOTOS_NOMINAIS_VALIDOS=50)]
party<-copy(base)[,`:=`(NR_PARTIDO=15L,QT_VOTOS_LEGENDA_VALIDOS=10,QT_VOTOS_LEGENDA_ANUL_SUBJUD=0,QT_VOTOS_NOMINAIS_CONVR_LEG=7)]
detail<-copy(base)[,`:=`(QT_TOTAL_VOTOS_VALIDOS=67,QT_VOTOS_NOMINAIS_VALIDOS=50,QT_TOTAL_VOTOS_LEG_VALIDOS=17,
 QT_TOTAL_VOTOS_NULOS=8,QT_TOTAL_VOTOS_ANULADOS=0,QT_TOTAL_VOTOS_ANUL_SUBJUD=0,QT_VOTOS_ANULADOS_APU_SEP=0)]
write_sources<-function() {
 fwrite(cand,file.path(folder,'votacao_candidato_munzona.csv'));fwrite(party,file.path(folder,'votacao_partido_munzona.csv'))
 fwrite(detail,file.path(folder,'detalhe_votacao_munzona.csv'))
};write_sources()
env<-historical_null_environment()
env$tse_download<-function(...)zip
env$legal_resource_csv<-function(year,dataset_prefix,uf,cache_dir)file.path(folder,paste0(dataset_prefix,'.csv'))
env$historical_null_sections<-function(...)data.table(year=2018L,round=1L,election_id=297L,uf='RO',municipality_tse='1',zone=1L,office=7L,section_nulls=5)
run<-function()env$legal_reconcile_places(2018L,'RO',7L,folder)
reject<-function(expr,pattern) {e<-tryCatch({force(expr);NULL},error=conditionMessage);stopifnot(is.character(e),grepl(pattern,e))}
# Unique whole-candidate numerical fit is insufficient without source evidence.
reject(run(),'Partial or unexplained')
env<-official_destination_environment(env,mapping_path,source_cache=folder)
x<-run();audit<-attr(x,'vote_audit')
stopifnot(sum(x$votes)==67,nrow(audit$official_destination_sources)==2L,
 x[place==1L & vote_type=='party',sum(votes)]==6,x[place==2L & vote_type=='party',sum(votes)]==11,
 !any(x$candidate_id %in% c(15000L,15800L)),grepl('no subset inference',audit$method))
# An explicit null candidate cannot be reused by a remaining unknown conversion.
party[,QT_VOTOS_NOMINAIS_CONVR_LEG:=8];write_sources();reject(run(),'Partial or unexplained')
party[,QT_VOTOS_NOMINAIS_CONVR_LEG:=7];write_sources()
mapping$mappings[[1L]]$official_total_votes<-6;save_mapping();reject(run(),'total does not match')
mapping$mappings[[1L]]$official_total_votes<-7;save_mapping()
writeBin(c(pdf_bytes,charToRaw('changed')),pdf);reject(run(),'signature/size/SHA256')
writeBin(pdf_bytes,pdf)
mapping$sources[[1L]]$cache_path<-'../outside.pdf';save_mapping();reject(run(),'Unsafe official document')
mapping$sources[[1L]]$cache_path<-'fixture.pdf';save_mapping()
raw[candidate_id==15800L & place==2L,candidate_name:='Different identity']
saveRDS(raw,file.path(folder,'places-2018-RO-7.rds'));reject(run(),'Multiple candidate identities')
message('PASS: explicit primary-document mapping, exact candidate totals and local vectors; no numerical-subset fallback; null reuse, wrong total, tampered PDF and unsafe cache path rejected')
