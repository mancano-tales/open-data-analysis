# Exact legal reconciliation. Never distribute a municipal total across sections.
source("shared-pipeline/eleicoes/historico/tse.R")
legal_resource_csv <- function(year, dataset_prefix, uf, cache_dir="data-raw/eleicoes-renda/historico") {
  metadata <- tse_metadata(year,cache_dir)
  resources <- data.table::as.data.table(metadata$result$resources)
  pattern <- paste0("/",dataset_prefix,"_",year,"[.]zip$")
  hit <- resources[grepl(pattern,url)]
  if(nrow(hit)!=1L) stop("No unique official legal source: ",pattern)
  zip <- file.path(cache_dir,basename(hit$url))
  if(!file.exists(zip)) {
    httr2::request(hit$url) |> httr2::req_timeout(1800) |> httr2::req_retry(max_tries=3) |> httr2::req_perform(path=paste0(zip,".part"))
    if(!file.rename(paste0(zip,".part"),zip)) stop("Cannot save legal ZIP")
  }
  if(dataset_prefix=="votacao_candidato_munzona" && uf!="BR") {
    catalog <- file.path(cache_dir,paste0("candidate-catalog-",year))
    provenance <- file.path(catalog,"provenance.json")
    target_catalog <- file.path(catalog,paste0(uf,".csv"))
    if(file.exists(provenance)&&file.exists(target_catalog)) {
      info <- jsonlite::read_json(provenance)
      if(identical(info$source_md5,unname(tools::md5sum(zip)))) return(target_catalog)
    }
  }
  members <- unzip(zip,list=TRUE)$Name
  member <- members[grepl(paste0("(^|/)",dataset_prefix,"_",year,"_",uf,"[.]csv$"),members,ignore.case=TRUE)]
  if(length(member)!=1L) stop("No unique legal CSV for ",year,"/",uf)
  folder <- sub("[.]zip$","",zip)
  target <- file.path(folder,member)
  if(!file.exists(target)) unzip(zip,files=member,exdir=folder)
  data.table::fwrite(data.table::data.table(url=hit$url,file=basename(zip),md5=unname(tools::md5sum(zip))),
    file.path(cache_dir,paste0("source-",dataset_prefix,"-",year,".csv")))
  target
}

legal_all_or_zero <- function(total,valid) {
  if(anyNA(total)||anyNA(valid)||any(total<0)||any(valid<0)||any(valid>total)) stop("Invalid official legal counts")
  if(any(valid!=0 & valid!=total)) stop("Partial nominal validity: cannot allocate official totals to sections")
  valid>0 & valid==total
}

legal_reconcile_places <- function(year,uf,offices=c(3L,5L,6L,7L,8L),cache_dir="data-raw/eleicoes-renda/historico") {
  tse_require()
  path <- tse_download(year,uf,cache_dir)
  raw_cache <- file.path(cache_dir,paste0("places-",year,"-",uf,"-",paste(offices,collapse="_"),".rds"))
  raw <- if(file.exists(raw_cache))readRDS(raw_cache) else NULL
  source_md5 <- unname(tools::md5sum(path))
  if(is.null(raw)||!identical(attr(raw,"normalizer_version"),2L)||!identical(attr(raw,"source_md5"),source_md5))
    raw <- tse_read_votes(path,year,offices=offices,rounds=c(1L,2L),ufs=if(uf=="BR")NULL else uf)
  raw <- raw[uf != "ZZ"]
  # Annual CKAN tables can also contain later extraordinary elections (e.g.
  # RR's 2026 supplementary governor election inside the 2022 nominal table).
  # The section reader selects ordinary elections; retain those same election
  # IDs in EVERY legal source instead of pooling an unrelated later contest.
  ordinary_elections <- unique(raw$election_id)
  if(!length(ordinary_elections)||anyNA(ordinary_elections))stop("Missing ordinary election identity in section source")
  # Validity is decided at municipality/zone/ballot-number level. Grouping
  # places first is exact for all-or-zero decisions and bounds join memory.
  # If replacement candidates share a number with mixed validity, the partial
  # count is rejected below rather than guessed at section level.
  if("section" %in% names(raw)) {
    raw <- tse_place_votes(raw)
    attr(raw,"normalizer_version") <- 2L
    attr(raw,"source_md5") <- source_md5
    saveRDS(raw,raw_cache)
    invisible(gc())
  }
  cand_path <- legal_resource_csv(year,"votacao_candidato_munzona",uf,cache_dir)
  cand_columns <- c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA",
    "CD_CARGO","SQ_CANDIDATO","NR_CANDIDATO","NR_PARTIDO","SG_PARTIDO","NM_TIPO_DESTINACAO_VOTOS",
    "QT_VOTOS_NOMINAIS","QT_VOTOS_NOMINAIS_VALIDOS")
  cand_header <- names(data.table::fread(cand_path,nrows=0L,encoding="Latin-1"))
  legacy_nominal <- !"QT_VOTOS_NOMINAIS_VALIDOS" %in% cand_header
  required_cand <- setdiff(cand_columns,c("NM_TIPO_DESTINACAO_VOTOS","QT_VOTOS_NOMINAIS_VALIDOS"))
  if(!all(required_cand %in% cand_header)) stop("Missing required official candidate columns")
  cand <- data.table::fread(cand_path,select=intersect(cand_columns,cand_header),encoding="Latin-1")
  if(!"NM_TIPO_DESTINACAO_VOTOS" %in% cand_header) cand[,NM_TIPO_DESTINACAO_VOTOS:=""]
  # Older candidate tables lack legal allocation. Retain all nominal votes
  # provisionally; exact official nominal/party/total checks below MUST pass.
  # A single annulled or unallocated vote therefore blocks this legacy path.
  if(legacy_nominal) cand[,QT_VOTOS_NOMINAIS_VALIDOS:=QT_VOTOS_NOMINAIS]
  data.table::setnames(cand,cand_columns,c("year","round","election_id","uf","municipality_tse","zone",
    "office","candidate_seq","candidate_id","party_id","party","destination","official_nominal","official_nominal_valid"))
  cand[,`:=`(municipality_tse=as.character(municipality_tse),candidate_seq=as.character(candidate_seq))]
  cand <- cand[office %in% offices & uf != "ZZ" & election_id %in% ordinary_elections]
  basekeys <- c("year","round","election_id","uf","municipality_tse","zone","office")
  keys <- c(basekeys,"candidate_id")
  # Contemporary section files carry the sequence. Older presidency files may not.
  has_seq <- any(!is.na(raw$candidate_seq) & raw$candidate_seq!="NA")
  if(has_seq) keys <- c(keys,"candidate_seq")
  cand <- cand[,.(official_nominal=sum(as.numeric(official_nominal)),official_nominal_valid=sum(as.numeric(official_nominal_valid)),
      party_id=if(data.table::uniqueN(party_id)==1L)party_id[1L] else NA_integer_,
      party=if(data.table::uniqueN(party)==1L)party[1L] else NA_character_,
      destination=paste(sort(unique(destination)),collapse="|")),by=keys]
  cand[,keep_nominal:=legal_all_or_zero(official_nominal,official_nominal_valid)]
  # The destination string is an official vote allocation, not candidacy eligibility.
  cand[,convert_party:=grepl("legenda",destination,ignore.case=TRUE)&official_nominal_valid==0]
  if(any(cand$convert_party & is.na(cand$party_id))) stop("Ambiguous converted-vote party")
  nominal <- merge(raw[vote_type=="candidate"],cand,by=keys,all.x=TRUE,sort=FALSE)
  check_nominal <- nominal[!is.na(official_nominal),.(section_nominal=sum(votes),official_nominal=official_nominal[1L]),by=keys]
  if(any(check_nominal$section_nominal!=check_nominal$official_nominal)) stop("Raw section and official candidate totals differ")
  # Compare every expected candidate key, including a candidate wholly absent in sections.
  expected <- cand[official_nominal>0, c(keys,"official_nominal"),with=FALSE]
  full <- merge(expected,check_nominal[,c(keys,"section_nominal"),with=FALSE],by=keys,all.x=TRUE)
  if(anyNA(full$section_nominal)||any(full$section_nominal!=full$official_nominal)) stop("Official positive candidate votes absent from sections")
  technical <- nominal[is.na(official_nominal),.(technical_computed=sum(votes)),by=basekeys]
  valid <- nominal[!is.na(keep_nominal)&keep_nominal==TRUE]
  converted <- nominal[!is.na(convert_party)&convert_party==TRUE]
  if(nrow(converted)) converted[,`:=`(candidate_id=party_id,candidate_name=party,vote_type="party")]
  keep_columns <- names(raw)
  party_valid <- raw[vote_type=="party"]
  if(nrow(party_valid)) {
    party_path <- legal_resource_csv(year,"votacao_partido_munzona",uf,cache_dir)
    party_cols <- c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO",
      "NR_PARTIDO","QT_VOTOS_LEGENDA_VALIDOS","QT_VOTOS_LEGENDA_ANUL_SUBJUD","QT_VOTOS_LEGENDA_ANULADOS")
    party_official <- data.table::fread(party_path,select=party_cols,encoding="Latin-1")
    data.table::setnames(party_official,party_cols,c(basekeys,"candidate_id","party_valid","party_pending","party_void"))
    party_official[,municipality_tse:=as.character(municipality_tse)]
    pkeys <- c(basekeys,"candidate_id")
    party_official <- party_official[office %in% offices & uf != "ZZ" & election_id %in% ordinary_elections,
      .(party_valid=sum(as.numeric(party_valid)),party_total=sum(as.numeric(party_valid)+party_pending+party_void)),by=pkeys]
    party_official[,keep_party:=legal_all_or_zero(party_total,party_valid)]
    party_valid <- merge(party_valid,party_official,by=pkeys,all.x=TRUE,sort=FALSE)
    pcheck <- party_valid[,.(section_total=sum(votes),official_total=party_total[1L]),by=pkeys]
    expected_party <- merge(party_official[party_total>0],pcheck,by=pkeys,all.x=TRUE)
    if(anyNA(pcheck$official_total)||anyNA(expected_party$section_total)||
       any(pcheck$section_total!=pcheck$official_total)||any(expected_party$section_total!=expected_party$party_total))
      stop("Raw section and official party totals differ")
    party_valid <- party_valid[keep_party==TRUE,..keep_columns]
  }
  legal <- data.table::rbindlist(list(valid[,..keep_columns],party_valid,converted[,..keep_columns]),use.names=TRUE)
  detail_path <- legal_resource_csv(year,"detalhe_votacao_munzona",uf,cache_dir)
  detail_cols <- c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO",
    "QT_TOTAL_VOTOS_VALIDOS","QT_VOTOS_NOMINAIS_VALIDOS","QT_TOTAL_VOTOS_LEG_VALIDOS","QT_VOTOS_NULOS_TECNICOS")
  detail <- data.table::fread(detail_path,select=detail_cols,encoding="Latin-1")
  data.table::setnames(detail,detail_cols,c(basekeys,"official_valid_votes","official_nominal_valid","official_party_valid","technical_official"))
  detail[,municipality_tse:=as.character(municipality_tse)]
  detail <- detail[office %in% offices & uf != "ZZ" & election_id %in% ordinary_elections, lapply(.SD,function(z)sum(as.numeric(z))),by=basekeys,
    .SDcols=c("official_valid_votes","official_nominal_valid","official_party_valid","technical_official")]
  computed <- legal[,.(computed_valid_votes=sum(votes),computed_nominal_valid=sum(votes[vote_type=="candidate"]),
      computed_party_valid=sum(votes[vote_type=="party"])),by=basekeys]
  checks <- merge(detail,computed,by=basekeys,all=TRUE)
  checks <- merge(checks,technical,by=basekeys,all=TRUE)
  checks[is.na(technical_computed),technical_computed:=0]
  checks[,difference:=computed_valid_votes-official_valid_votes]
  checks[,reconciled:=!is.na(difference)&difference==0 & computed_nominal_valid==official_nominal_valid &
      computed_party_valid==official_party_valid & technical_computed==technical_official]
  output <- file.path(cache_dir,paste0("places-",year,"-",uf,"-",paste(offices,collapse="_"),"-legal.rds"))
  data.table::fwrite(checks,sub("[.]rds$","-audit.csv",output))
  contest_keys <- c("year","round","election_id","office","uf")
  contests <- checks[,.(reconciled=all(!is.na(reconciled)&reconciled)),by=contest_keys]
  failed_contests <- contests[reconciled==FALSE]
  good_contests <- contests[reconciled==TRUE,..contest_keys]
  if(!nrow(good_contests)) stop("No reconciled contest; inspect ",sub("[.]rds$","-audit.csv",output))
  if(nrow(failed_contests)) warning("Some contests failed official reconciliation and are EXCLUDED from certified cache. Inspect audit CSV.")
  legal <- merge(legal,good_contests,by=contest_keys,all=FALSE,sort=FALSE)
  checks <- merge(checks,good_contests,by=contest_keys,all=FALSE,sort=FALSE)
  places <- tse_place_votes(legal)
  aggregate_checks <- checks[,.(official_valid_votes=sum(official_valid_votes),computed_valid_votes=sum(computed_valid_votes),
      difference=sum(difference),zones=.N,reconciled=all(reconciled)),by=.(year,round,election_id,office,uf)]
  audit_method <- if(legacy_nominal) "Legacy candidate totals without legal-allocation fields: all nominal votes retained only after exact official nominal, party, valid-total and technical-null checks in EVERY municipality/zone; no allocation" else "Official candidate municipality/zone all-or-zero validity; official party conversion; exact zone/cargo totals and technical null checks; no allocation"
  attr(places,"vote_audit") <- list(status="reconciled",method=audit_method,checks=as.data.frame(aggregate_checks),
    unavailable=as.data.frame(failed_contests),election_ids=ordinary_elections,
    election_scope="Ordinary election IDs selected in the section source; unrelated supplementary contests excluded")
  attr(places,"source_md5") <- source_md5
  attr(places,"normalizer_version") <- 2L
  saveRDS(places,output)
  message("Legal reconciliation PASS: ",output)
  places
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  year<-if(length(args))as.integer(args[1]) else 2026L
  ufs<-if(length(args)>=2)strsplit(args[2],",",fixed=TRUE)[[1]] else c("AC","DF")
  offices<-if(length(args)>=3)as.integer(strsplit(args[3],",",fixed=TRUE)[[1]]) else c(3L,5L,6L,7L,8L)
  data.table::setDTthreads(2L)
  for(state in ufs) {x<-legal_reconcile_places(year,state,offices);rm(x);invisible(gc())}
}
