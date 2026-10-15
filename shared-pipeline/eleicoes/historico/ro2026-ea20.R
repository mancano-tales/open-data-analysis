# Narrow recovery for an official CKAN omission: RO, 2026, first-round deputies.
# Rscript --vanilla shared-pipeline/eleicoes/historico/ro2026-ea20.R
source("shared-pipeline/eleicoes/historico/tse.R")

ea20_destination <- function(x) {
  allowed <- c("V\u00e1lido","V\u00e1lido (legenda)","Anulado","Anulado sub judice")
  if(anyNA(x)||any(!x %in% allowed)) stop("Unknown official EA20 vote destination")
  x
}
ea20_assert_candidate_totals <- function(x) {
  if(any(x[!is.na(raw_votes)&!is.na(official_votes),raw_votes!=official_votes])) stop("Section and EA20 candidate totals differ")
  if(any(x[official_votes>0,is.na(raw_votes)])) stop("Positive EA20 candidate absent from sections")
  invisible(TRUE)
}
ea20_assert_zone_checks <- function(x) {
  fields<-c("difference_valid","difference_nominal","difference_legend","difference_technical")
  if(anyNA(x[,..fields])||any(as.matrix(x[,..fields])!=0)) stop("RO EA20 zone reconciliation failed")
  invisible(TRUE)
}
ea20_save <- function(x,path) {
  saveRDS(x,paste0(path,".part"))
  if(!file.copy(paste0(path,".part"),path,overwrite=TRUE))stop("Cannot save ",path)
  unlink(paste0(path,".part"))
}

ro2026_ea20 <- function(cache_dir="data-raw/eleicoes-renda/historico",merge_existing=TRUE,install_canonical=FALSE) {
  tse_require()
  data.table::setDTthreads(1L)
  outdir<-file.path(cache_dir,"ro-alternative")
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  candidates<-list();parties<-list();snapshots<-list()
  for(office in c(6L,7L)) {
    filename<-sprintf("ro-c%04d-e006259-u.json",office)
    url<-paste0("https://resultados.tse.jus.br/oficial/ele2026/6259/dados/ro/",filename)
    path<-file.path(outdir,filename)
    if(!file.exists(path))httr2::request(url)|>httr2::req_perform(path=path)
    j<-jsonlite::read_json(path,simplifyVector=FALSE)
    if(!identical(j$f,"o")||!identical(j$ele,"6259")||!identical(j$t,"1")||
       !identical(j$tpabr,"uf")||!identical(tolower(j$cdabr),"ro"))stop("Wrong EA20 scope or nonofficial source")
    snapshots[[as.character(office)]]<-list(office=office,url=url,md5=unname(tools::md5sum(path)),
      generation_date=j$dg,generation_time=j$hg,idg=j$idg,final_totalization=j$tf,progress=j$and,
      status=if(identical(j$tf,"s"))"final" else "provisional")
    if(length(j$carg)!=1L||as.integer(j$carg[[1L]]$cd)!=office)stop("Wrong EA20 cargo")
    for(cargo in j$carg)for(agr in cargo$agr)for(party in agr$par) {
      parties[[length(parties)+1L]]<-data.table::data.table(office=office,party_id=as.integer(party$n),
        destination=ea20_destination(party$dvt),official_nominal=as.numeric(party$tvtn),official_legend=as.numeric(party$tvtl))
      for(cand in party$cand)candidates[[length(candidates)+1L]]<-data.table::data.table(office=office,
        candidate_id=as.integer(cand$n),candidate_seq=cand$sqcand,party_id=as.integer(party$n),party=party$sg,
        name=cand$nmu,destination=ea20_destination(cand$dvt),official_votes=as.numeric(cand$vap))
    }
  }
  cand<-data.table::rbindlist(candidates);party<-data.table::rbindlist(parties)
  if(anyDuplicated(cand[,.(office,candidate_id)])||anyDuplicated(party[,.(office,party_id)]))stop("Ambiguous EA20 candidate or party")
  raw_path<-file.path(cache_dir,"places-2026-RO-3_5_6_7_8.rds")
  raw<-data.table::as.data.table(readRDS(raw_path))
  source_md5<-attr(raw,"source_md5")
  zip<-file.path(cache_dir,"votacao_secao_2026_RO.zip")
  if(is.null(source_md5)||!identical(source_md5,unname(tools::md5sum(zip))))stop("Raw place cache provenance mismatch")
  raw<-raw[year==2026L&round==1L&election_id==6259L&uf=="RO"&office %in% c(6L,7L)]
  nominal<-raw[vote_type=="candidate",.(raw_votes=sum(votes)),by=.(office,candidate_id)]
  compare<-merge(nominal,cand,by=c("office","candidate_id"),all=TRUE)
  compare[,difference:=raw_votes-official_votes]
  data.table::fwrite(compare,file.path(outdir,"candidate-uf-comparison.csv"))
  ea20_assert_candidate_totals(compare)
  # Prove the UF number map also has the exact official sequence in the section CSV.
  csv<-file.path(cache_dir,"votacao_secao_2026_RO/votacao_secao_2026_RO.csv")
  seqs<-data.table::fread(csv,select=c("CD_CARGO","NR_VOTAVEL","SQ_CANDIDATO"),encoding="Latin-1")
  seqs<-unique(seqs[CD_CARGO %in% c(6L,7L),.(office=CD_CARGO,candidate_id=NR_VOTAVEL,section_seq=as.character(SQ_CANDIDATO))])
  if(anyDuplicated(seqs[,.(office,candidate_id)]))stop("Multiple candidate sequences for one ballot number")
  seq_check<-merge(cand[,.(office,candidate_id,candidate_seq)],seqs,by=c("office","candidate_id"),all.x=TRUE)
  seq_check[,matches:=candidate_seq==section_seq]
  data.table::fwrite(seq_check,file.path(outdir,"sequence-comparison.csv"))
  if(anyNA(seq_check$matches)||!all(seq_check$matches))stop("EA20/section sequence mismatch")
  rm(seqs);invisible(gc())
  zonekeys<-c("year","round","election_id","uf","municipality_tse","zone","office")
  nom<-merge(raw[vote_type=="candidate"],cand[,.(office,candidate_id,party_id,party,destination)],
    by=c("office","candidate_id"),all.x=TRUE)
  technical<-nom[is.na(destination),.(technical_computed=sum(votes)),by=zonekeys]
  valid<-nom[destination=="V\u00e1lido"]
  converted<-nom[destination=="V\u00e1lido (legenda)"]
  if(nrow(converted))converted[,`:=`(candidate_id=party_id,candidate_name=party,vote_type="party")]
  legend<-merge(raw[vote_type=="party"],party[,.(office,party_id,destination)],
    by.x=c("office","candidate_id"),by.y=c("office","party_id"),all.x=TRUE)
  if(anyNA(legend$destination))stop("Party raw rows absent from official EA20")
  legend<-legend[destination=="V\u00e1lido (legenda)"]
  columns<-names(raw)
  legal<-data.table::rbindlist(list(valid[,..columns],converted[,..columns],legend[,..columns]),use.names=TRUE)
  computed<-legal[,.(computed_valid=sum(votes),computed_nominal=sum(votes[vote_type=="candidate"]),
    computed_legend=sum(votes[vote_type=="party"])),by=zonekeys]
  detail_path<-file.path(cache_dir,"detalhe_votacao_munzona_2026/detalhe_votacao_munzona_2026_RO.csv")
  detail<-data.table::fread(detail_path,encoding="Latin-1")
  detail<-detail[ANO_ELEICAO==2026L&NR_TURNO==1L&CD_ELEICAO==6259L&SG_UF=="RO"&CD_CARGO %in% c(6L,7L),
    .(official_valid=sum(as.numeric(QT_TOTAL_VOTOS_VALIDOS)),official_nominal=sum(as.numeric(QT_VOTOS_NOMINAIS_VALIDOS)),
      official_legend=sum(as.numeric(QT_TOTAL_VOTOS_LEG_VALIDOS)),technical_official=sum(as.numeric(QT_VOTOS_NULOS_TECNICOS))),
    by=.(year=ANO_ELEICAO,round=NR_TURNO,election_id=CD_ELEICAO,uf=SG_UF,
      municipality_tse=as.character(CD_MUNICIPIO),zone=NR_ZONA,office=CD_CARGO)]
  checks<-merge(detail,computed,by=zonekeys,all=TRUE)
  checks<-merge(checks,technical,by=zonekeys,all=TRUE)
  # A missing technical subtotal means no excluded rows, not an imputed vote total.
  checks[is.na(technical_computed),technical_computed:=0]
  checks[,`:=`(difference_valid=computed_valid-official_valid,difference_nominal=computed_nominal-official_nominal,
    difference_legend=computed_legend-official_legend,difference_technical=technical_computed-technical_official)]
  data.table::fwrite(checks,file.path(outdir,"zone-reconciliation.csv"))
  ea20_assert_zone_checks(checks)
  audit_checks<-checks[,.(official_valid_votes=sum(official_valid),computed_valid_votes=sum(computed_valid),
    difference=sum(difference_valid),zones=.N,reconciled=TRUE),by=.(year,round,election_id,office,uf)]
  method<-"RO2026 deputies recovered from official EA20 UF vote destination; every candidate and sequence agrees with section data; exact official nominal/party/valid/technical totals in every municipality-zone; no allocation"
  audit<-list(status="reconciled",method=method,checks=as.data.frame(audit_checks),
    unavailable=data.frame(),snapshots=snapshots,provisional=any(vapply(snapshots,function(z)z$status=="provisional",logical(1))))
  attr(legal,"vote_audit")<-audit
  attr(legal,"normalizer_version")<-2L
  attr(legal,"source_md5")<-source_md5
  deputies_path<-file.path(cache_dir,"places-2026-RO-6_7-legal.rds")
  ea20_save(legal,deputies_path)
  jsonlite::write_json(list(snapshots=snapshots,raw_votes_md5=source_md5,detail_md5=unname(tools::md5sum(detail_path))),
    file.path(outdir,"sources.json"),pretty=TRUE,auto_unbox=TRUE)
  if(merge_existing) {
    existing_path<-file.path(cache_dir,"places-2026-RO-3_5_6_7_8-legal.rds")
    old<-data.table::as.data.table(readRDS(existing_path))
    old_audit<-attr(old,"vote_audit")
    if(!identical(old_audit$status,"reconciled")||!identical(attr(old,"source_md5"),source_md5))stop("Existing certified RO cache provenance/status mismatch")
    old<-old[office %in% c(3L,5L)]
    old_checks<-data.table::as.data.table(old_audit$checks)[office %in% c(3L,5L)]
    if(anyNA(old_checks$difference)||any(old_checks$difference!=0)||!all(c(3L,5L)%in% old_checks$office))stop("Existing Gov/Sen checks incomplete")
    if(any(!old$year %in% 2026L)||any(old$round!=1L)||any(old$uf!="RO"))stop("Unexpected existing contest scope")
    combined<-data.table::rbindlist(list(old,legal),use.names=TRUE,fill=TRUE)
    audit$checks<-as.data.frame(data.table::rbindlist(list(old_checks,audit_checks),use.names=TRUE,fill=TRUE))
    unavailable<-data.table::as.data.table(old_audit$unavailable)
    if(nrow(unavailable))unavailable<-unavailable[!(year==2026L&round==1L&uf=="RO"&office %in% c(6L,7L))]
    audit$unavailable<-as.data.frame(unavailable)
    audit$method<-paste(old_audit$method,method,sep="; ")
    attr(combined,"vote_audit")<-audit;attr(combined,"normalizer_version")<-2L;attr(combined,"source_md5")<-source_md5
    combined_path<-file.path(cache_dir,"places-2026-RO-3_5_6_7-legal.rds")
    ea20_save(combined,combined_path)
    if(install_canonical)ea20_save(combined,existing_path)
  }
  message("PASS RO EA20: both deputy contests, 56 zones each; cache ",deputies_path)
  invisible(legal)
}
if(sys.nframe()==0L)ro2026_ea20()
