if (.Platform$OS.type == 'windows') invisible(Sys.setlocale('LC_CTYPE', '.UTF-8'))
source('shared-pipeline/eleicoes/agregacao/validacao/legal-2010-2014.R', encoding = 'UTF-8')
library(data.table); setDTthreads(2L)
b <- data.table(year=2014L, round=1L, election_id=143L, uf='AC', municipality_tse='001', zone=1L, office=6L)
raw <- b[rep(1L,8L)][, `:=`(candidate_id=c(1313L,1313L,1322L,9999L,13L,13L,95L,96L),
  votes=c(30,20,0,0,7,3,4,7), vote_type=c('candidate','candidate','candidate','candidate','party','party','special','special'),
  place_key=c('AC:1:1:1','AC:1:1:2','AC:1:1:1','AC:1:1:2','AC:1:1:1','AC:1:1:2','AC:1:1:1','AC:1:1:2'))]
candidates <- b[rep(1L,2L)][, `:=`(candidate_id=c(1313L,1322L),party_id=13L,official_nominal=c(50,0))]
candidates[, `:=`(candidate_uid=c('100001','100002'),official_name=c('Official valid','Official zero'))]
parties <- b[rep(1L,2L)][, `:=`(party_id=c(13L,99L),official_nominal=c(50,0),official_legend=c(10,0))]
detail <- copy(b)[, `:=`(official_nominal=50,official_legend=10,official_apurados=71,official_nulls=7,
 official_blank=4,official_annulled=0,official_pending=0,official_separate=0)]
run <- function(r=raw,c=candidates,p=parties,d=detail) legacy14_reconcile_contest(r,c,p,d)
reject <- function(expr,pattern) {
 msg <- tryCatch({force(expr);NULL},error=conditionMessage)
 stopifnot(is.character(msg),grepl(pattern,msg))
}
a <- run(); stopifnot(sum(a$legal$votes)==60,all(a$checks$reconciled))
stopifnot(all(a$legal[vote_type=='candidate',candidate_name]=='Official valid'),
 all(a$legal[vote_type=='candidate',identified_candidate_uid]=='100001'))
substitution <- rbind(candidates,copy(candidates[1])[,`:=`(candidate_uid='200001',official_nominal=1)])
reject(run(c=substitution),'Ambiguous legacy substitution')
homonym <- copy(substitution);homonym[,official_name:='Same name']
reject(run(c=homonym),'Ambiguous legacy substitution')
renamed <- rbind(candidates,copy(candidates[1])[,official_name:='Different official name'])
reject(run(c=renamed),'Ambiguous legacy substitution')
source('shared-pipeline/eleicoes/agregacao/validacao/legacy-2010-2014-environment.R',encoding='UTF-8')
reject(legacy14_environment()$legal_reconcile_places(2018L,'AC'),'Expected one legacy year')
# Cross-party adversarial: generic party legend may be RAW direct.
# Party13 direct (7,3) could lose (0,3), while party99 converts its (0,3).
# Final zone10 is unchanged, but party destinations cannot be identified.
other <- copy(raw); other[candidate_id==9999L,votes:=3]
dother <- copy(detail)[,`:=`(official_apurados=74,official_nulls=10)]
reject(run(r=other,d=dother),'ambiguous legacy legend')
# Adversarial: exact same party/zone totals permit different local vectors.
# A: valid directs (7,3), excluded nominal (2,0) null.
# B: convert (2,0), invalidate directs (0,2), final legend (9,1).
# Both have final legend10 and null9; no aggregate ledger distinguishes them.
amb <- copy(raw); amb[candidate_id==1322L,votes:=2]
damb <- copy(detail)[, `:=`(official_apurados=73,official_nulls=9)]
stopifnot(sum(c(7,3))==sum(c(9,1)), !identical(c(7,3),c(9,1)))
reject(run(r=amb,d=damb),'ambiguous legacy legend')
# At the full zone-pool endpoint every excluded nominal and direct is valid.
# No partial allocation is inferred; source place vectors are preserved.
dmax <- copy(damb)[, `:=`(official_legend=12,official_nulls=7)]
amax <- run(r=amb,d=dmax)
stopifnot(amax$checks$computed_conversion==2,all(amax$checks$reconciled),
 amax$legal[place_key=='AC:1:1:1' & vote_type=='party',sum(votes)]==9,
 amax$legal[place_key=='AC:1:1:2' & vote_type=='party',sum(votes)]==3)
# Generic party column compatible with either direct or final semantics.
pm <- copy(parties);pm[party_id==13L,official_legend:=12]
stopifnot(all(run(r=amb,p=pm,d=dmax)$checks$reconciled))
# Zero endpoint uniquely makes every remaining ballot null.
pzero <- copy(parties)[,official_legend:=0]
dzero <- copy(damb)[,`:=`(official_legend=0,official_nulls=19)]
stopifnot(sum(run(r=amb,p=pzero,d=dzero)$legal$votes)==50)
# Majoritarian exclusions do not trigger the proportional ambiguity guard.
major_raw <- copy(amb)[vote_type!='party'][,office:=3L]
major_c <- copy(candidates)[,office:=3L]; major_p <- copy(pzero)[,office:=3L]
major_d <- copy(damb)[,`:=`(office=3L,official_legend=0,official_apurados=63)]
major <- run(r=major_raw,c=major_c,p=major_p,d=major_d)
stopifnot(all(major$checks$reconciled),major$checks$computed_nulls==9)
# Present-zero and absent candidates remain distinguishable, never nominal.
absent <- copy(amb);absent[candidate_id==9999L,votes:=3]
dabs <- copy(damb)[,`:=`(official_legend=0,official_apurados=76,official_nulls=22)]
aabs <- run(r=absent,p=pzero,d=dabs)
stopifnot(aabs$candidate_checks[candidate_id==1322L,official_present],
 !aabs$candidate_checks[candidate_id==9999L,official_present])
partial <- copy(candidates);partial[candidate_id==1313L,official_nominal:=49]
reject(run(c=partial),'Positive official nominal')
wrong <- copy(detail)[,official_nulls:=8];reject(run(d=wrong),'zone reconciliation failed')
wrong <- copy(parties);wrong[party_id==13L,official_nominal:=49];reject(run(p=wrong),'party official nominal')
wrong <- copy(detail)[,official_pending:=1];reject(run(d=wrong),'annulled/pending')
wrong <- copy(detail)[,official_annulled:=1];reject(run(d=wrong),'annulled/pending')
wrong <- rbind(raw,copy(raw[1])[,`:=`(candidate_id=97L,vote_type='special',votes=1)])
reject(run(r=wrong),'code 97')
wrong <- rbind(candidates,copy(candidates[1])[,`:=`(party_id=22L,official_nominal=0)])
reject(run(c=wrong),'Ambiguous legacy candidate party')
wrong <- copy(detail)[,official_apurados:=72];reject(run(d=wrong),'zone reconciliation failed')
stopifnot(all(legacy14_reconcile_contest(copy(raw)[,year:=2010L],copy(candidates)[,year:=2010L],
 copy(parties)[,year:=2010L],copy(detail)[,year:=2010L])$checks$reconciled))
source('shared-pipeline/eleicoes/agregacao/validacao/legacy-2010-2014-environment.R',encoding='UTF-8')
entry <- legacy14_environment()
stopifnot(identical(names(formals(entry$legal_reconcile_places)),c('year','uf','offices','cache_dir')))
# Contract-only fixture: no archives are read and no files are written.
entry$legacy14_reconcile_cached <- function(...) {
 empty <- data.table()
 attr(empty,'vote_audit') <- list(status='diagnostic_only',publishable=FALSE,
   checks=data.frame(),unavailable=data.frame(office=7L,reason='ambiguous'))
 list(places=empty,path='isolated/diagnostic.rds')
}
empty <- entry$legal_reconcile_places(2014L,'AC',7L)
stopifnot(nrow(empty)==0L,identical(attr(empty,'diagnostic_path'),'isolated/diagnostic.rds'),
 nrow(attr(empty,'vote_audit')$unavailable)==1L)
entry$legacy14_reconcile_cached <- function(...) {
 bad <- data.table();attr(bad,'vote_audit') <- list(status='reconciled',publishable=TRUE)
 list(places=bad,path='bad.rds')
}
reject(entry$legal_reconcile_places(2014L,'AC',7L),'not TRUE')
# Synthetic promotion checks write ONLY to an isolated temporary directory.
tmp <- tempfile('legacy14-fixture-');dir.create(tmp)
reviewed <- legacy14_environment(publication_mode='reviewed')
reviewed$legacy14_reconcile_cached <- function(...) {
 p <- data.table(votes=1)
 attr(p,'vote_audit') <- list(status='diagnostic_only',publishable=FALSE,
   checks=data.frame(reconciled=TRUE,difference=0),unavailable=data.frame(office=7L,reason='ambiguous'),
   provenance=list(raw_md5=strrep('a',32L),official_members=rep(list(list(csv_md5=strrep('b',32L))),3L)))
 list(places=p,path='isolated/diagnostic.rds')
}
promoted <- reviewed$legal_reconcile_places(2014L,'AC',c(3L,7L),tmp)
stopifnot(attr(promoted,'vote_audit')$status=='reconciled',
 identical(attr(promoted,'vote_audit')$publishable,TRUE),
 nrow(attr(promoted,'vote_audit')$unavailable)==1L,
 file.exists(file.path(tmp,'places-2014-AC-3_7-legal.rds')))
unlink(file.path(tmp,c('places-2014-AC-3_7-legal.rds','places-2014-AC-3_7-legal-unavailable.csv')));unlink(tmp)
message('PASS: within-party and cross-party offsets blocked; exact final zone endpoints and majoritarians preserved; diagnostic/reviewed contracts and provenance checked.')

# Tiny deterministic ZIP fixtures; no zip executable/package or network needed.
# Member schemas and ballot values are described by the assertions below.
zip_bytes <- list(
 raw1='504b030414000000000000002100ea59fbbc900100009001000019000000766f746163616f5f736563616f5f323031345f41432e637376414e4f5f454c454943414f3b4e525f5455524e4f3b53475f55463b43445f4d554e49434950494f3b4e525f5a4f4e413b4e525f534543414f3b4e525f4c4f43414c5f564f544143414f3b43445f434152474f3b4e525f564f544156454c3b4e4d5f564f544156454c3b51545f564f544f533b43445f454c454943414f3b43445f5449504f5f454c454943414f3b44545f454c454943414f0a323031343b313b41433b313b313b313b31303b363b313331333b4f6c643b33303b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b323b31303b363b313331333b4f6c643b32303b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b313b31303b363b39353b426c616e6b3b343b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b323b31303b363b39363b4e756c6c3b373b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b323b31303b363b39373b50656e64696e673b303b3134333b323b30352f31302f323031340a504b0102140014000000000000002100ea59fbbc9001000090010000190000000000000000000000800100000000766f746163616f5f736563616f5f323031345f41432e637376504b0506000000000100010047000000c70100000000',
 raw2='504b030414000000000000002100a016d509900100009001000019000000766f746163616f5f736563616f5f323031345f41432e637376414e4f5f454c454943414f3b4e525f5455524e4f3b53475f55463b43445f4d554e49434950494f3b4e525f5a4f4e413b4e525f534543414f3b4e525f4c4f43414c5f564f544143414f3b43445f434152474f3b4e525f564f544156454c3b4e4d5f564f544156454c3b51545f564f544f533b43445f454c454943414f3b43445f5449504f5f454c454943414f3b44545f454c454943414f0a323031343b313b41433b313b313b313b31303b363b313331333b4f6c643b33313b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b323b31303b363b313331333b4f6c643b32303b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b313b31303b363b39353b426c616e6b3b343b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b323b31303b363b39363b4e756c6c3b373b3134333b323b30352f31302f323031340a323031343b313b41433b313b313b323b31303b363b39373b50656e64696e673b303b3134333b323b30352f31302f323031340a504b0102140014000000000000002100a016d5099001000090010000190000000000000000000000800100000000766f746163616f5f736563616f5f323031345f41432e637376504b0506000000000100010047000000c70100000000',
 candidate='504b030414000000000000002100eedd4fe6ce000000ce00000025000000766f746163616f5f63616e64696461746f5f6d756e7a6f6e615f323031345f41432e637376414e4f5f454c454943414f3b4e525f5455524e4f3b43445f454c454943414f3b53475f55463b43445f4d554e49434950494f3b4e525f5a4f4e413b43445f434152474f3b43445f5449504f5f454c454943414f3b4e525f43414e44494441544f3b4e525f5041525449444f3b53515f43414e44494441544f3b4e4d5f55524e415f43414e44494441544f3b51545f564f544f535f4e4f4d494e4149530a323031343b313b3134333b41433b313b313b363b323b313331333b31333b3130303030313b4f6666696369616c3b35300a504b0102140014000000000000002100eedd4fe6ce000000ce000000250000000000000000000000800100000000766f746163616f5f63616e64696461746f5f6d756e7a6f6e615f323031345f41432e637376504b0506000000000100010053000000110100000000',
 party='504b0304140000000000000021002b673435a0000000a000000023000000766f746163616f5f7061727469646f5f6d756e7a6f6e615f323031345f41432e637376414e4f5f454c454943414f3b4e525f5455524e4f3b43445f454c454943414f3b53475f55463b43445f4d554e49434950494f3b4e525f5a4f4e413b43445f434152474f3b43445f5449504f5f454c454943414f3b4e525f5041525449444f3b51545f564f544f535f4e4f4d494e4149533b51545f564f544f535f4c4547454e44410a323031343b313b3134333b41433b313b313b363b323b31333b35303b300a504b01021400140000000000000021002b673435a0000000a0000000230000000000000000000000800100000000766f746163616f5f7061727469646f5f6d756e7a6f6e615f323031345f41432e637376504b0506000000000100010051000000e10000000000',
 detail='504b030414000000000000002100099371ee2d0100002d01000023000000646574616c68655f766f746163616f5f6d756e7a6f6e615f323031345f41432e637376414e4f5f454c454943414f3b4e525f5455524e4f3b43445f454c454943414f3b53475f55463b43445f4d554e49434950494f3b4e525f5a4f4e413b43445f434152474f3b43445f5449504f5f454c454943414f3b51545f564f544f535f4e4f4d494e4149535f56414c49444f533b51545f544f54414c5f564f544f535f4c45475f56414c49444f533b51545f564f544f533b51545f544f54414c5f564f544f535f4e554c4f533b51545f564f544f535f4252414e434f533b51545f544f54414c5f564f544f535f414e554c41444f533b51545f544f54414c5f564f544f535f414e554c5f5355424a55443b51545f564f544f535f414e554c41444f535f4150555f5345500a323031343b313b3134333b41433b313b313b363b323b35303b303b36313b373b343b303b303b300a504b0102140014000000000000002100099371ee2d0100002d010000230000000000000000000000800100000000646574616c68655f766f746163616f5f6d756e7a6f6e615f323031345f41432e637376504b05060000000001000100510000006e0100000000')
write_zip <- function(key,path) {
 hex <- zip_bytes[[key]];start <- seq.int(1L,nchar(hex),by=2L)
 writeBin(as.raw(strtoi(substring(hex,start,start+1L),16L)),path)
}
fixture_cache <- tempfile('legacy14-acquisition-');dir.create(fixture_cache)
fixture_probe <- file.path(fixture_cache,'isolated')
raw_zip <- file.path(fixture_cache,'votacao_secao_2014_AC.zip')
write_zip('raw1',raw_zip)
for (pair in list(c('candidate','votacao_candidato_munzona'),c('party','votacao_partido_munzona'),c('detail','detalhe_votacao_munzona')))
 write_zip(pair[[1L]],file.path(fixture_cache,paste0(pair[[2L]],'_2014.zip')))
# Simulate the actual worker call sequence, including its method accessed by $.
simulated_worker <- function(env) {
 places <- env$legal_reconcile_places(2014L,'AC',6L,fixture_cache)
 path <- env$legal_resource_csv(2014L,'detalhe_votacao_munzona','AC',fixture_cache)
 official <- fread(path)
 stopifnot(nrow(official)==1L,official$QT_VOTOS==61L)
 places
}
worker_env <- legacy14_environment(probe_dir=fixture_probe)
fixture_places <- simulated_worker(worker_env)
audit <- attr(fixture_places,'vote_audit')
stopifnot(sum(fixture_places$votes)==50,audit$status=='diagnostic_only',!audit$publishable,
 audit$candidate_identity$identified_candidate_uid=='100001',audit$candidate_identity$candidate_name=='Official',
 nrow(audit$expected_contests)==1L)
# Raw sections aggregate into four local/candidate/type keys including95/96/97.
all1 <- legacy14_read_all_types(raw_zip,2014L,'AC',6L,fixture_probe)
stopifnot(nrow(all1)==4L,sum(all1$votes)==61,
 all1[candidate_id==1313L,votes]==50,all(c(95L,96L,97L) %in% all1$candidate_id))
identity1 <- attr(all1,'all_types_identity')
# A normal places cache without specials/identity is not accepted as all-types.
all_path <- file.path(fixture_probe,'all-types-2014-AC-6-v1.rds')
saveRDS(all1[vote_type=='candidate'],all_path)
attrless <- readRDS(all_path);attr(attrless,'all_types_identity') <- NULL;saveRDS(attrless,all_path)
rebuilt <- legacy14_read_all_types(raw_zip,2014L,'AC',6L,fixture_probe)
stopifnot(all(c(95L,96L,97L) %in% rebuilt$candidate_id),sum(rebuilt$votes)==61)
# Replace the ZIP under the SAME name: candidate30 becomes31 (zone total62).
# Both the old aggregate cache and extracted source must be invalidated.
write_zip('raw2',raw_zip)
all2 <- legacy14_read_all_types(raw_zip,2014L,'AC',6L,fixture_probe)
stopifnot(sum(all2$votes)==62,all2[candidate_id==1313L,votes]==51,
 !identical(identity1$zip_md5,attr(all2,'all_types_identity')$zip_md5))
# Official positive50 versus raw51 blocks the whole contest, but resource API
# remains available and reviewed output is explicitly unavailable/nonpublicable.
blocked_env <- legacy14_environment(probe_dir=fixture_probe,publication_mode='reviewed')
blocked_places <- simulated_worker(blocked_env)
blocked_audit <- attr(blocked_places,'vote_audit')
stopifnot(nrow(blocked_places)==0L,blocked_audit$status=='unavailable',!blocked_audit$publishable,
 nrow(blocked_audit$checks)==0L,nrow(blocked_audit$unavailable)==1L,
 nrow(blocked_audit$expected_contests)==1L,
 grepl('Positive official nominal',blocked_audit$unavailable$reason))
# Verify all-types grouping preserves full raw totals and ledger algebra.
comparison <- copy(raw)[,`:=`(election_date='05/10/2014',place=as.integer(sub('.*:','',place_key)),candidate_name=as.character(candidate_id))]
grouped <- legacy14_place_all_types(rbind(comparison,comparison))
stopifnot(sum(grouped$votes)==2*sum(raw$votes),all(c(95L,96L) %in% grouped$candidate_id))
double_c <- copy(candidates)[,official_nominal:=official_nominal*2]
double_p <- copy(parties)[,`:=`(official_nominal=official_nominal*2,official_legend=official_legend*2)]
double_d <- copy(detail)
for (field in setdiff(names(detail),names(b))) set(double_d,j=field,value=double_d[[field]]*2)
before_grouping <- run(r=rbind(comparison,comparison),c=double_c,p=double_p,d=double_d)
after_grouping <- run(r=grouped,c=double_c,p=double_p,d=double_d)
stopifnot(identical(before_grouping$checks,after_grouping$checks))
# Exact validated temporary root before recursive fixture cleanup on Windows.
resolved_fixture <- normalizePath(fixture_cache,winslash='/',mustWork=TRUE)
resolved_temp <- paste0(normalizePath(tempdir(),winslash='/',mustWork=TRUE),'/')
stopifnot(startsWith(resolved_fixture,resolved_temp),grepl('legacy14-acquisition-',basename(resolved_fixture),fixed=TRUE))
unlink(resolved_fixture,recursive=TRUE)
message('PASS: real worker API sequence, official identity, all-types grouping/cache, same-name ZIP invalidation, explicit all-unavailable reviewed contract.')
