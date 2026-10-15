# Compare raw section candidate/party totals with official municipal-zone totals.
# Run after prepare.R 2026 AC,DF 3,5,6,7,8 and the official detail ZIP download.
library(data.table)
root <- "data-raw/eleicoes-renda/historico"
path <- file.path(root,"detalhe_votacao_munzona_2026/detalhe_votacao_munzona_2026_BRASIL.csv")
columns <- c("ANO_ELEICAO","CD_TIPO_ELEICAO","CD_ELEICAO","NR_TURNO","SG_UF","CD_CARGO",
 "QT_TOTAL_VOTOS_VALIDOS","QT_VOTOS_CONCORRENTES","QT_VOTOS_NOMINAIS_VALIDOS",
 "QT_TOTAL_VOTOS_LEG_VALIDOS","QT_TOTAL_VOTOS_ANULADOS","QT_TOTAL_VOTOS_ANUL_SUBJUD")
official <- fread(path,select=columns,encoding="Latin-1")
check <- list()
for (state in c("AC","DF")) {
 places <- readRDS(file.path(root,paste0("places-2026-",state,"-3_5_6_7_8.rds")))
 nominal <- places[,.(nominal=sum(votes[vote_type=="candidate"]),party=sum(votes[vote_type=="party"]),raw_total=sum(votes)),
   by=.(year,round,election_id,uf,office)]
 detail <- official[SG_UF == state & CD_TIPO_ELEICAO == 2L & CD_CARGO %in% c(3L,5L,6L,7L,8L),
 .(official_valid=sum(as.numeric(QT_TOTAL_VOTOS_VALIDOS)),official_nominal_valid=sum(as.numeric(QT_VOTOS_NOMINAIS_VALIDOS)),
 official_party_valid=sum(as.numeric(QT_TOTAL_VOTOS_LEG_VALIDOS)),official_concurrent=sum(as.numeric(QT_VOTOS_CONCORRENTES)),
 official_annulled=sum(as.numeric(QT_TOTAL_VOTOS_ANULADOS)),official_subjudice=sum(as.numeric(QT_TOTAL_VOTOS_ANUL_SUBJUD))),
 by=.(year=ANO_ELEICAO,round=NR_TURNO,election_id=CD_ELEICAO,uf=SG_UF,office=CD_CARGO)]
 z <- merge(nominal,detail,by=c("year","round","election_id","uf","office"),all=TRUE)
 z[,`:=`(difference_valid=raw_total-official_valid,difference_concurrent=raw_total-official_concurrent)]
 check[[state]]<-z
}
out <- rbindlist(check)
fwrite(out,file.path(root,"audit-valid-votes-2026-AC-DF.csv"))
print(out[,.(uf,office,round,raw_total,official_valid,difference_valid,official_annulled,official_subjudice,difference_concurrent)])
