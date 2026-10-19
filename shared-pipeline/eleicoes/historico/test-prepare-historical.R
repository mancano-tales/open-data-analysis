source("shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R")
stopifnot(nrow(historical_plan())==27L,identical(unique(historical_plan(c(2010L,2022L,2014L,2018L))$year),historical_years))
stopifnot(inherits(try(historical_plan(2026L),silent=TRUE),"try-error"))
stopifnot(inherits(try(historical_plan(2022L,"ZZ"),silent=TRUE),"try-error"))
x<-data.frame(year=2022L,round=1L,election_id=546L,office=c(3L,5L,6L,7L),uf="AC")
c<-x;c$official_valid_votes<-c(426600,411949,434253,435770);c$computed_valid_votes<-c$official_valid_votes;c$difference<-0
stopifnot(historical_contest_status(x,c,x[FALSE,])=="complete")
blocked<-x[4,,drop=FALSE];blocked$reason<-"Explicitly blocked by exact ledger"
stopifnot(historical_contest_status(x,c[1:3,],blocked)=="partial")
allblocked<-x;allblocked$reason<-"Explicitly blocked by exact ledger"
stopifnot(historical_contest_status(x,c[FALSE,],allblocked)=="partial")
reject<-function(expr)stopifnot(inherits(try(expr,silent=TRUE),"try-error"))
reject(historical_contest_status(x,c[1:3,],allblocked)) # overlap
reject(historical_contest_status(x,c[FALSE,],allblocked[1:3,])) # missing
extra<-allblocked[1,,drop=FALSE];extra$office<-8L
reject(historical_contest_status(x,c[FALSE,],rbind(allblocked,extra)))
reject(historical_contest_status(x,rbind(c,c[1,]),allblocked[FALSE,]))
reject(historical_contest_status(x,c[FALSE,],rbind(allblocked,allblocked[1,])))
reject(historical_contest_status(x,c[FALSE,],x)) # no explicit reason
reject(historical_contest_status(x[FALSE,],c[FALSE,],allblocked[FALSE,]))
empty<-x[FALSE,];attr(empty,"normalizer_version")<-2L
stopifnot(historical_output_status(empty,list(status="unavailable",checks=data.frame())))
reject(historical_output_status(empty,list(status="reconciled",checks=c)))
nonempty<-x;attr(nonempty,"normalizer_version")<-2L
reject(historical_output_status(nonempty,list(status="unavailable",checks=data.frame())))
reject(historical_output_status(empty,list(status="unavailable",checks=c)))
env<-historical_2018_environment("fixture-central-cache")
stopifnot(is.environment(env),is.function(env$legal_reconcile_places),
 identical(env$.official_destination_source_cache,"fixture-central-cache"),
 identical(env$historical_null_sections,historical_section_categories))
stopifnot(all(c("shared-pipeline/eleicoes/historico/historical-party-aggregates.R",
 "shared-pipeline/eleicoes/historico/historical-separate-votes.R",
 "shared-pipeline/eleicoes/historico/official-vote-destinations.R",
 "shared-pipeline/eleicoes/historico/official-vote-destinations.json") %in% historical_sources))
stopifnot(!length(historical_destination_inputs(2022L,"RO","missing-cache")),
 !length(historical_destination_inputs(2018L,"AC","missing-cache")))
reject(historical_destination_inputs(2018L,"RO","missing-cache"))
# Exercise the real worker's all-unavailable acceptance path without real votes.
folder<-tempfile("all-unavailable-worker-");dir.create(folder)
writeLines("synthetic archive",file.path(folder,"votacao_secao_2018_AC.zip"))
expected18<-x;expected18$year<-2018L
detail<-data.frame(ANO_ELEICAO=expected18$year,NR_TURNO=expected18$round,
 CD_ELEICAO=expected18$election_id,CD_CARGO=expected18$office,SG_UF=expected18$uf,CD_TIPO_ELEICAO=2L)
data.table::fwrite(detail,file.path(folder,"detail.csv"))
original_factory<-historical_2018_environment
historical_2018_environment<-function(cache_dir) {
 env<-new.env(parent=globalenv())
 env$legal_resource_csv<-function(...)file.path(folder,"detail.csv")
 env$legal_reconcile_places<-function(year,uf,offices,cache_dir) {
   z<-data.frame(votes=numeric());attr(z,"normalizer_version")<-2L
   attr(z,"source_md5")<-unname(tools::md5sum(file.path(cache_dir,"votacao_secao_2018_AC.zip")))
   blocked<-expected18;blocked$reason<-"Explicit synthetic unavailable result"
   attr(z,"vote_audit")<-list(status="unavailable",checks=data.frame(),unavailable=blocked,method="Synthetic unavailable")
   saveRDS(z,file.path(cache_dir,"places-2018-AC-3_5_6_7_8-legal.rds"));z
 };env
}
record<-historical_worker(2018L,"AC",folder)
historical_2018_environment<-original_factory
stopifnot(record$status=="partial",record$audit_status=="unavailable",record$rows==0L,
 nrow(record$certified)==0L,nrow(record$unavailable)==4L)
stopifnot(inherits(try(historical_contest_status(x,c[1:3,],x[FALSE,]),silent=TRUE),"try-error"))
c$difference[1]<-1
stopifnot(inherits(try(historical_contest_status(x,c,x[FALSE,]),silent=TRUE),"try-error"))
cat("PASS historical plan, year order, exact scope partition, all-unavailable empty/unpublishable contract, overlap/missing/extra/duplicate rejection and audit-divergence tests\n")
