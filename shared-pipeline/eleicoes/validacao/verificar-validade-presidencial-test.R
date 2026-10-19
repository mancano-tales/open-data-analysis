source("shared-pipeline/eleicoes/validacao/verificar-validade-presidencial.R")
base<-data.frame(ANO_ELEICAO=2026L,NR_TURNO=1L,CD_ELEICAO=6257L,SG_UF="AC",CD_MUNICIPIO="00001",NR_ZONA=1L,CD_CARGO=1L)
raw<-base[rep(1,6),];raw$CD_MUNICIPIO<-1L;raw$NR_VOTAVEL<-c(13,22,31,31,47,96);raw$QT_VOTOS<-c(10,20,2,3,4,9)
cand<-base[rep(1,3),];cand$NR_CANDIDATO<-c(13,22,47);cand$QT_VOTOS_NOMINAIS<-c(10,20,4);cand$QT_VOTOS_NOMINAIS_VALIDOS<-c(10,20,0)
detail<-base;detail$QT_TOTAL_VOTOS_VALIDOS<-30;detail$QT_VOTOS_NOMINAIS_VALIDOS<-30;detail$QT_TOTAL_VOTOS_LEG_VALIDOS<-0;detail$QT_VOTOS_NULOS_TECNICOS<-5
good<-verificar_validade_presidencial(raw,cand,detail)
stopifnot(identical(good$keep,c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)),all(good$checks$reconciled),
 sum(good$excluidos$QT_VOTOS)==9,good$checks$computed_technical==5)
fails<-function(r=raw,c=cand,d=detail)inherits(try(verificar_validade_presidencial(r,c,d),silent=TRUE),"try-error")
partial<-cand;partial$QT_VOTOS_NOMINAIS_VALIDOS[3]<-1;stopifnot(fails(c=partial))
tampered<-raw;tampered$QT_VOTOS[1]<-11;stopifnot(fails(r=tampered))
missing<-raw[raw$NR_VOTAVEL!=22,];stopifnot(fails(r=missing))
technical<-detail;technical$QT_VOTOS_NULOS_TECNICOS<-4;stopifnot(fails(d=technical))
total<-detail;total$QT_TOTAL_VOTOS_VALIDOS<-31;stopifnot(fails(d=total))
legend<-detail;legend$QT_TOTAL_VOTOS_LEG_VALIDOS<-1;stopifnot(fails(d=legend))
# Independent zones may not cancel each other's discrepancies.
r2<-raw;r2$NR_ZONA<-2;r2$QT_VOTOS[r2$NR_VOTAVEL==31]<-c(1,3)
c2<-cand;c2$NR_ZONA<-2;d2<-detail;d2$NR_ZONA<-2;d2$QT_VOTOS_NULOS_TECNICOS<-6
stopifnot(fails(r=rbind(raw,r2),c=rbind(cand,c2),d=rbind(detail,d2)))
# Unrelated elections must not affect the ordinary source scope.
extra<-cand;extra$CD_ELEICAO<-9999;extra$QT_VOTOS_NOMINAIS<-999
stopifnot(identical(verificar_validade_presidencial(raw,rbind(cand,extra),detail)$keep,good$keep))
# Transit records are summed, not arbitrarily deduplicated.
split<-cand[c(1,1,2,3),];split$QT_VOTOS_NOMINAIS[1:2]<-c(3,7);split$QT_VOTOS_NOMINAIS_VALIDOS[1:2]<-c(3,7)
stopifnot(identical(verificar_validade_presidencial(raw,split,detail)$keep,good$keep))
message("PASS independent validity: canonical keys, all-or-zero, technical nulls, exact zone checks, no blacklist and adversarial failures")
