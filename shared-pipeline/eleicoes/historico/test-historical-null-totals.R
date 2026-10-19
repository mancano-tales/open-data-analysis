source("shared-pipeline/eleicoes/historico/legal-historical-null-totals.R")
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
folder<-tempfile("historical-null-test-");dir.create(folder)
zip<-file.path(folder,"source.zip");writeLines("official fixture",zip)
base<-data.table(ANO_ELEICAO=2018L,NR_TURNO=1L,CD_ELEICAO=297L,SG_UF="AC",CD_MUNICIPIO=1L,NR_ZONA=1L,CD_CARGO=6L)
# Exercise the real selective section reader, including its CSV/ZIP schema.
reader_rows<-base[rep(1L,4L)][,`:=`(NR_VOTAVEL=c(96L,96L,95L,96L),QT_VOTOS=c(2,3,100,100))]
reader_rows[4L,CD_ELEICAO:=999L]
reader_member<-'votacao_secao_2018_AC.csv';fwrite(reader_rows,file.path(folder,reader_member),sep=';')
reader_zip<-file.path(folder,'reader.zip');zip::zipr(reader_zip,reader_member,root=folder)
read_nulls<-historical_null_sections(reader_zip,2018L,'AC',6L,297L)
stopifnot(nrow(read_nulls)==1L,read_nulls$section_nulls==5)
raw<-data.table(year=2018L,round=1L,election_id=297L,election_date="07/10/2018",uf="AC",municipality_tse="1",zone=1L,
 place=1L,place_key="AC:1:1:1",office=6L,candidate_id=c(1313L,1314L,1315L,2299L,2211L,13L),
 candidate_name=c("Valid","Known conversion","Unknown conversion","Known invalid","Unknown invalid","Party"),
 vote_type=c(rep("candidate",5L),"party"),votes=c(50,7,3,2,4,10))
attr(raw,"source_md5")<-unname(tools::md5sum(zip));attr(raw,"normalizer_version")<-2L
save_raw<-function()saveRDS(raw,file.path(folder,"places-2018-AC-6.rds"));save_raw()
cand<-base[rep(1L,3L)][,`:=`(SQ_CANDIDATO=c("1","2","3"),NR_CANDIDATO=c(1313L,1314L,2299L),NR_PARTIDO=c(13L,13L,22L),
 SG_PARTIDO=c("PT","PT","PL"),NM_TIPO_DESTINACAO_VOTOS=c("Valido","Legenda","Nulo"),
 QT_VOTOS_NOMINAIS=c(50,7,2),QT_VOTOS_NOMINAIS_VALIDOS=c(50,0,0))]
party<-base[rep(1L,2L)][,`:=`(NR_PARTIDO=c(13L,22L),QT_VOTOS_LEGENDA_VALIDOS=c(10,0),
 QT_VOTOS_LEGENDA_ANUL_SUBJUD=0,QT_VOTOS_NOMINAIS_CONVR_LEG=c(10,0))]
detail<-copy(base)[,`:=`(QT_TOTAL_VOTOS_VALIDOS=70,QT_VOTOS_NOMINAIS_VALIDOS=50,QT_TOTAL_VOTOS_LEG_VALIDOS=20,
 QT_TOTAL_VOTOS_NULOS=11,QT_TOTAL_VOTOS_ANUL_SUBJUD=0,QT_TOTAL_VOTOS_ANULADOS=0,QT_VOTOS_ANULADOS_APU_SEP=0)]
write_sources<-function() {
 fwrite(cand,file.path(folder,"votacao_candidato_munzona.csv"))
 fwrite(party,file.path(folder,"votacao_partido_munzona.csv"))
 fwrite(detail,file.path(folder,"detalhe_votacao_munzona.csv"))
}
write_sources();env<-historical_null_environment()
env$tse_download<-function(...)zip
env$legal_resource_csv<-function(year,dataset_prefix,uf,cache_dir)file.path(folder,paste0(dataset_prefix,".csv"))
env$historical_null_sections<-function(...)data.table(year=2018L,round=1L,election_id=297L,uf="AC",municipality_tse="1",zone=1L,office=6L,section_nulls=5)
run<-function()env$legal_reconcile_places(2018L,"AC",6L,folder)
expect_error<-function(pattern) {
 err<-tryCatch({run();NULL},error=conditionMessage)
 stopifnot(is.character(err),grepl(pattern,err))
}
x<-run();stopifnot(sum(x$votes)==70,sum(x[vote_type=="candidate",votes])==50,sum(x[vote_type=="party",votes])==20)
source('shared-pipeline/eleicoes/agregacao/validacao/candidate-group-fastpath.R')
env<-legal_candidate_fast_environment(base_environment=env)
fast<-run();stopifnot(isTRUE(all.equal(as.data.frame(x),as.data.frame(fast),check.attributes=FALSE)),
 identical(attr(x,'vote_audit'),attr(fast,'vote_audit')))
audit<-fread(file.path(folder,"places-2018-AC-6-legal-audit.csv"))
stopifnot(audit$null_total_computed==11,audit$null_total_official==11,all(audit$reconciled),
 !any(grepl("technical",names(audit))),grepl("TOTAL NULL",attr(x,"vote_audit")$method))
# Subtract known conversions first: only the remaining 3 unknown votes convert.
party[NR_PARTIDO==13L,QT_VOTOS_NOMINAIS_CONVR_LEG:=9];write_sources();expect_error("Partial or unexplained")
party[NR_PARTIDO==13L,QT_VOTOS_NOMINAIS_CONVR_LEG:=10]
# Positive pending counts are never absorbed into nulls, even if total matches.
detail[,QT_TOTAL_VOTOS_ANUL_SUBJUD:=1];write_sources();expect_error("pending/annulled")
detail[,QT_TOTAL_VOTOS_ANUL_SUBJUD:=0]
party[NR_PARTIDO==22L,QT_VOTOS_LEGENDA_ANUL_SUBJUD:=1];write_sources();expect_error("party pending")
party[NR_PARTIDO==22L,QT_VOTOS_LEGENDA_ANUL_SUBJUD:=0]
cand[NR_CANDIDATO==2299L,NM_TIPO_DESTINACAO_VOTOS:='Anulado sub judice'];write_sources();expect_error("candidate pending")
cand[NR_CANDIDATO==2299L,NM_TIPO_DESTINACAO_VOTOS:='Nulo']
detail[,QT_TOTAL_VOTOS_NULOS:=12];write_sources();expect_error("No reconciled contest")
detail[,QT_TOTAL_VOTOS_NULOS:=11]
detail[,QT_TOTAL_VOTOS_LEG_VALIDOS:=19];write_sources();expect_error("No reconciled contest")
detail[,QT_TOTAL_VOTOS_LEG_VALIDOS:=20]
raw[candidate_id==13L,votes:=11];save_raw();write_sources();expect_error("Raw section and official party totals differ")
raw[candidate_id==13L,votes:=10];save_raw()
# Contemporary callers are explicitly rejected before applying historical rules.
for(modern in c(2022L,2026L,2014L,2010L)) {
 err<-tryCatch({env$legal_reconcile_places(modern,"AC",6L,folder);NULL},error=conditionMessage)
 stopifnot(grepl("restricted",err))
}
message("PASS: known+unknown integral conversion, exact total nulls, pending rejection, 1-vote failures and modern-year guard")
