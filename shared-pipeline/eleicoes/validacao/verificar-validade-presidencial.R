# Independent presidential-validity reference. No calls to production helpers.
# Input rows may include blanks/null codes; only nominal numbers 1..94 can survive.
verificar_validade_presidencial <- function(raw,candidatos,detalhe,ano=2026L,turno=1L,cargo=1L) {
  if(!identical(as.integer(ano),2026L)||!identical(as.integer(cargo),1L)||!turno %in% 1:2)
    stop("Independent verifier supports 2026 presidency only")
  raw<-as.data.frame(raw);candidate<-as.data.frame(candidatos);detail<-as.data.frame(detalhe)
  zone_cols<-c("ANO_ELEICAO","NR_TURNO","CD_ELEICAO","SG_UF","CD_MUNICIPIO","NR_ZONA","CD_CARGO")
  require_cols<-function(x,cols,label)if(!all(cols %in% names(x)))stop(label,": missing columns")
  require_cols(raw,c(zone_cols,"NR_VOTAVEL","QT_VOTOS"),"raw")
  require_cols(candidate,c(zone_cols,"NR_CANDIDATO","QT_VOTOS_NOMINAIS","QT_VOTOS_NOMINAIS_VALIDOS"),"candidate")
  require_cols(detail,c(zone_cols,"QT_TOTAL_VOTOS_VALIDOS","QT_VOTOS_NOMINAIS_VALIDOS","QT_TOTAL_VOTOS_LEG_VALIDOS","QT_VOTOS_NULOS_TECNICOS"),"detail")
  integer_text<-function(x,label) {
    value<-suppressWarnings(as.numeric(as.character(x)))
    if(anyNA(value)||any(!is.finite(value))||any(value<0)||any(value!=floor(value)))stop("Invalid integer key/count: ",label)
    sprintf("%.0f",value)
  }
  canonical<-function(x) {
    for(col in setdiff(zone_cols,"SG_UF"))x[[col]]<-integer_text(x[[col]],col)
    x$SG_UF<-toupper(trimws(as.character(x$SG_UF)))
    if(anyNA(x$SG_UF)||any(!grepl("^[A-Z]{2}$",x$SG_UF)))stop("Invalid UF")
    x
  }
  raw<-canonical(raw)
  if(!nrow(raw)||any(raw$ANO_ELEICAO!="2026")||any(raw$CD_CARGO!="1")||any(raw$SG_UF=="ZZ")||
     length(unique(raw$CD_ELEICAO))!=1L||length(unique(raw$NR_TURNO))!=1L||
     !all(raw$NR_TURNO==as.character(turno)))stop("Expected domestic 2026 presidential round")
  ids<-unique(raw$CD_ELEICAO);round<-unique(raw$NR_TURNO)
  source_scope<-function(x) {
    # Filter before canonicalizing: annual sources can contain unrelated elections.
    take<-!is.na(x$ANO_ELEICAO)&x$ANO_ELEICAO==2026&!is.na(x$CD_CARGO)&x$CD_CARGO==1&
      !is.na(x$NR_TURNO)&suppressWarnings(as.numeric(as.character(x$NR_TURNO)))==as.numeric(round)&!is.na(x$SG_UF)&x$SG_UF!="ZZ"&
      !is.na(x$CD_ELEICAO)&suppressWarnings(as.numeric(as.character(x$CD_ELEICAO))) %in% as.numeric(ids)
    take[is.na(take)]<-FALSE
    canonical(x[take,,drop=FALSE])
  }
  candidate<-source_scope(candidate);detail<-source_scope(detail)
  if(!nrow(candidate)||!nrow(detail))stop("Empty official presidential scope")
  count<-function(x,label)as.numeric(integer_text(x,label))
  raw$QT_VOTOS<-count(raw$QT_VOTOS,"raw votes")
  raw$NR_VOTAVEL<-integer_text(raw$NR_VOTAVEL,"raw number")
  candidate$NR_CANDIDATO<-integer_text(candidate$NR_CANDIDATO,"official number")
  if(any(!as.numeric(candidate$NR_CANDIDATO) %in% 1:94))stop("Invalid official presidential number")
  candidate$nominal<-count(candidate$QT_VOTOS_NOMINAIS,"official nominal")
  candidate$valid<-count(candidate$QT_VOTOS_NOMINAIS_VALIDOS,"official valid nominal")
  if(any(candidate$valid>candidate$nominal))stop("Official valid exceeds nominal")
  signature<-function(x,cols)do.call(paste,c(x[,cols,drop=FALSE],sep=":"))
  zone_raw<-signature(raw,zone_cols);zone_cand<-signature(candidate,zone_cols);zone_detail<-signature(detail,zone_cols)
  candidate_key<-paste(zone_cand,candidate$NR_CANDIDATO,sep=":")
  # Transit/replacement rows are aggregated before all-or-zero: mixed valid and
  # invalid votes for the same ballot number must fail rather than be allocated.
  official_nominal<-tapply(candidate$nominal,candidate_key,sum)
  official_valid<-tapply(candidate$valid,candidate_key,sum)
  if(any(official_valid!=0 & official_valid!=official_nominal))stop("Partial nominal validity cannot be spatially allocated")
  nominal_rows<-as.numeric(raw$NR_VOTAVEL) %in% 1:94
  raw_key<-paste(zone_raw,raw$NR_VOTAVEL,sep=":")
  raw_totals<-tapply(raw$QT_VOTOS[nominal_rows],raw_key[nominal_rows],sum)
  shared<-intersect(names(raw_totals),names(official_nominal))
  if(any(raw_totals[shared]!=official_nominal[shared]))stop("Raw and official candidate nominal totals differ")
  positive_keys<-names(official_nominal)[official_nominal>0]
  if(any(!positive_keys %in% names(raw_totals)))stop("Positive official candidate votes absent from raw")
  known<-raw_key %in% names(official_nominal)
  keep<-nominal_rows & known & unname(official_valid[match(raw_key,names(official_valid))])>0
  keep[is.na(keep)]<-FALSE
  technical<-nominal_rows & !known
  sum_by_zone<-function(value,key,universe) {
    if(!length(value))return(rep(0,length(universe)))
    sums<-tapply(value,key,sum);out<-as.numeric(sums[match(universe,names(sums))]);out[is.na(out)]<-0;out
  }
  universe<-sort(unique(c(zone_raw,zone_detail,zone_cand)))
  zone_frame<-rbind(raw[,zone_cols],candidate[,zone_cols],detail[,zone_cols])
  zone_frame<-zone_frame[match(universe,signature(zone_frame,zone_cols)),,drop=FALSE]
  zone_frame$raw_nominal<-sum_by_zone(raw$QT_VOTOS[nominal_rows],zone_raw[nominal_rows],universe)
  zone_frame$computed_valid<-sum_by_zone(raw$QT_VOTOS[keep],zone_raw[keep],universe)
  zone_frame$computed_technical<-sum_by_zone(raw$QT_VOTOS[technical],zone_raw[technical],universe)
  zone_frame$excluded_known_nominal<-sum_by_zone(raw$QT_VOTOS[nominal_rows & known & !keep],zone_raw[nominal_rows & known & !keep],universe)
  for(field in c("QT_TOTAL_VOTOS_VALIDOS","QT_VOTOS_NOMINAIS_VALIDOS","QT_TOTAL_VOTOS_LEG_VALIDOS","QT_VOTOS_NULOS_TECNICOS")) {
    value<-count(detail[[field]],field)
    zone_frame[[field]]<-sum_by_zone(value,zone_detail,universe)
  }
  zone_frame$official_detail_present<-universe %in% zone_detail
  zone_frame$reconciled<-zone_frame$official_detail_present &
    zone_frame$computed_valid==zone_frame$QT_TOTAL_VOTOS_VALIDOS &
    zone_frame$computed_valid==zone_frame$QT_VOTOS_NOMINAIS_VALIDOS &
    zone_frame$QT_TOTAL_VOTOS_LEG_VALIDOS==0 &
    zone_frame$computed_technical==zone_frame$QT_VOTOS_NULOS_TECNICOS
  if(any(!zone_frame$reconciled))stop("Independent presidential zone reconciliation failed")
  candidate_checks<-data.frame(key=names(official_nominal),official_nominal=as.numeric(official_nominal),
    official_valid=as.numeric(official_valid),raw_nominal=as.numeric(raw_totals[match(names(official_nominal),names(raw_totals))]))
  candidate_checks$raw_nominal[is.na(candidate_checks$raw_nominal)]<-0
  excluded<-raw[nominal_rows & !keep,,drop=FALSE]
  excluded$motivo_validade<-ifelse(known[nominal_rows & !keep],"nominal_oficial_invalido","nulo_tecnico_sem_cadastro")
  list(keep=as.logical(keep),checks=zone_frame,excluidos=excluded,candidate_checks=candidate_checks,
    method="Independent base-R official all-or-zero validity; exact nominal and zone valid/technical totals; no allocation")
}
