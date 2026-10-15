source("shared-pipeline/eleicoes/agregacao/validacao/legal-party-technical-wrapper.R")
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
env<-legal_party_technical_environment();folder<-tempfile("historical-ordinary-test-");dir.create(folder)
zip<-file.path(folder,"source.zip");writeLines("synthetic official-source checksum fixture",zip)
base<-data.table(ANO_ELEICAO=2022L,NR_TURNO=1L,CD_ELEICAO=546L,SG_UF="AC",CD_MUNICIPIO=1L,NR_ZONA=1L,CD_CARGO=6L)
raw<-data.table(year=2022L,round=1L,election_id=546L,election_date="02/10/2022",uf="AC",municipality_tse="1",zone=1L,
 place=c(1L,2L,1L,2L,1L,2L),place_key=paste0("AC:1:1:",c(1L,2L,1L,2L,1L,2L)),office=6L,
 candidate_id=c(1313L,1313L,13L,13L,29L,29L),candidate_name=rep(c("Candidate","Party","Unregistered party"),each=2L),
 vote_type=rep(c("candidate","party","party"),each=2L),votes=c(30,20,5,5,1,2))
attr(raw,"source_md5")<-unname(tools::md5sum(zip));attr(raw,"normalizer_version")<-2L
saveRDS(raw,file.path(folder,"places-2022-AC-6.rds"))
cand<-copy(base)[,`:=`(SQ_CANDIDATO="100",NR_CANDIDATO=1313L,NR_PARTIDO=13L,SG_PARTIDO="PT",
 NM_TIPO_DESTINACAO_VOTOS="V\u00e1lido",QT_VOTOS_NOMINAIS=50,QT_VOTOS_NOMINAIS_VALIDOS=50)]
party<-copy(base)[,`:=`(NR_PARTIDO=13L,QT_VOTOS_LEGENDA_VALIDOS=10,QT_VOTOS_LEGENDA_ANUL_SUBJUD=0,QT_VOTOS_LEGENDA_ANULADOS=0)]
detail<-copy(base)[,`:=`(QT_TOTAL_VOTOS_VALIDOS=60,QT_VOTOS_NOMINAIS_VALIDOS=50,QT_TOTAL_VOTOS_LEG_VALIDOS=10,QT_VOTOS_NULOS_TECNICOS=3)]
# Same annual dataset, a different extraordinary election with positive votes.
supp_cand<-copy(cand)[,`:=`(CD_ELEICAO=6278L,QT_VOTOS_NOMINAIS=100,QT_VOTOS_NOMINAIS_VALIDOS=100)]
supp_party<-copy(party)[,`:=`(CD_ELEICAO=6278L,QT_VOTOS_LEGENDA_VALIDOS=20)]
supp_detail<-copy(detail)[,`:=`(CD_ELEICAO=6278L,QT_TOTAL_VOTOS_VALIDOS=120,QT_VOTOS_NOMINAIS_VALIDOS=100,QT_TOTAL_VOTOS_LEG_VALIDOS=20)]
write_sources<-function() {
 fwrite(rbind(cand,supp_cand),file.path(folder,"votacao_candidato_munzona.csv"))
 fwrite(rbind(party,supp_party),file.path(folder,"votacao_partido_munzona.csv"))
 fwrite(rbind(detail,supp_detail),file.path(folder,"detalhe_votacao_munzona.csv"))
}
write_sources();env$tse_download<-function(...)zip
env$legal_resource_csv<-function(year,dataset_prefix,uf,cache_dir)file.path(folder,paste0(dataset_prefix,".csv"))
x<-env$legal_reconcile_places(2022L,"AC",6L,folder)
stopifnot(sum(x$votes)==60,!any(x$candidate_id==29L),all(x$election_id==546L),all(attr(x,"vote_audit")$checks$difference==0),
 identical(attr(x,"vote_audit")$election_ids,546L))
# A single extra unregistered vote must fail; no rate or municipal redistribution.
raw[candidate_id==29L&place==1L,votes:=2]
saveRDS(raw,file.path(folder,"places-2022-AC-6.rds"))
err<-tryCatch({env$legal_reconcile_places(2022L,"AC",6L,folder);NULL},error=conditionMessage)
stopifnot(is.character(err),grepl("No reconciled contest",err))
resolved<-normalizePath(folder,winslash="/",mustWork=TRUE);temp_root<-normalizePath(tempdir(),winslash="/",mustWork=TRUE)
stopifnot(startsWith(resolved,paste0(temp_root,"/")),startsWith(basename(resolved),"historical-ordinary-test-"));unlink(resolved,recursive=TRUE)
message("PASS: only ordinary election IDs reconciled; exact 3 technical party votes accepted; fourth vote rejected")
