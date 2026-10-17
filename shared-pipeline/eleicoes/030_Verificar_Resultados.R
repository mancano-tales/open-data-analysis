# Independent numerical and geometric checks. A passed marker is written last.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/parametros.R", encoding="UTF-8")
source("shared-pipeline/eleicoes/validacao/050_Verificar_Espacial_Independente.R", encoding="UTF-8")
source("shared-pipeline/eleicoes/validacao/060_Verificar_Metricas_Post.R", encoding="UTF-8")
source("shared-pipeline/eleicoes/validacao/bootstrap-municipios.R", encoding="UTF-8")

verificar_csv_do_zip <- function(archive, member_pattern, extracted_folder) {
  index <- zip::zip_list(archive)
  member <- index[grepl(member_pattern,index$filename),,drop=FALSE]
  if(nrow(member)!=1L)stop("Expected exactly one CSV ZIP member: ",archive)
  path <- file.path(extracted_folder,member$filename)
  if(!file.exists(path))stop("Missing extracted CSV; re-extract ",archive)
  expected <- tolower(as.character(member$crc32))
  expected <- substr(paste0("00000000",expected),nchar(expected)+1L,nchar(expected)+8L)
  actual <- digest::digest(file=path,algo="crc32")
  if(file.info(path)$size!=member$uncompressed_size || !identical(actual,expected))
    stop("Extracted CSV differs from current ZIP; re-extract ",archive," (",path,")")
  data.frame(archive=archive,member=member$filename,file=path,bytes=member$uncompressed_size,
    crc32=actual,status="pass",stringsAsFactors=FALSE)
}

verificacao_coberturas_base <- function(raw, places, spatial, used, dest) {
  totals <- function(x) c(validos=sum(x$QT_VOTOS),lula=sum(x$QT_VOTOS[x$NR_VOTAVEL==13]),
                         flavio=sum(x$QT_VOTOS[x$NR_VOTAVEL==22]))
  divide <- function(a,b)if(is.finite(b)&&b>0)unname(a/b) else NA_real_
  br <- totals(raw)
  rows <- list()
  for(uf in c("BR",sort(unique(raw$SG_UF)))) {
    r <- if(uf=="BR")raw else raw[raw$SG_UF==uf,]
    p <- if(uf=="BR")places else places[places$SG_UF==uf,]
    territory <- totals(r)
    stages <- list(resultado_nacional=NULL,locais_com_votos=p$place_key,
      coordenadas_utilizaveis=intersect(p$place_key,spatial$coordinate_keys),
      locais_elegiveis_municipio=intersect(p$place_key,spatial$eligible_keys),
      locais_usados=intersect(p$place_key,used))
    for(stage in names(stages)) {
      selected <- if(stage=="resultado_nacional")p else p[p$place_key %in% stages[[stage]],]
      vote <- if(stage=="resultado_nacional")territory else colSums(selected[,c("validos","lula","flavio"),drop=FALSE])
      row <- data.frame(SG_UF=uf,etapa=stage,locais=nrow(selected))
      for(name in names(vote)) {
        row[[name]]<-unname(vote[name])
        row[[paste0(name,"_fracao_br")]]<-divide(vote[name],br[name])
        row[[paste0(name,"_fracao_territorio")]]<-divide(vote[name],territory[name])
      }
      for(name in c("lula","flavio")) {
        row[[paste0(name,"_pct_validos")]]<-100*divide(vote[name],vote["validos"])
        row[[paste0(name,"_diferenca_territorio_pp")]]<-100*divide(vote[name],vote["validos"])-100*divide(territory[name],territory["validos"])
      }
      rows[[length(rows)+1L]]<-row
    }
  }
  result <- do.call(rbind,rows)
  for(file in c("cobertura-votos.csv","cobertura-votos-uf.csv")) {
    expected <- result[if(file=="cobertura-votos.csv")result$SG_UF=="BR" else result$SG_UF!="BR",]
    verificacao_comparar_tabela(expected,read.csv(file.path(dest,file)),c("SG_UF","etapa"),file)
  }
  outside <- spatial$outside
  outside$SG_UF <- sub(":.*","",outside$place_key)
  for(name in c("validos","lula","flavio"))outside[[name]]<-places[[name]][match(outside$place_key,places$place_key)]
  verificacao_comparar_tabela(outside,read.csv(file.path(dest,"locais-fora-municipio.csv")),"place_key","locais-fora-municipio.csv")
  invisible(TRUE)
}

verificar_resultados_post <- function(dest="data-raw/eleicoes-renda",spatial_size=2000L) {
  stopifnot(requireNamespace("data.table",quietly=TRUE),requireNamespace("jsonlite",quietly=TRUE))
  data.table::setDTthreads(2L)
  marker <- file.path(dest,"validacao-metodologica.json")
  if(file.exists(marker) && !file.remove(marker))stop("Cannot invalidate previous validation marker")
  raw_path <- file.path(dest,"votos2026","votacao_secao_2026_BR.csv")
  input_paths <- c(file.path(dest,c("renda2022.rds","polling2026.rds","municipios2022.rds",
    "votos2026.zip","locais2026.zip","setores-votos.rds")),raw_path,
    "shared-pipeline/eleicoes/020_Cruzar_Setores_Votos.R","shared-pipeline/eleicoes/030_Verificar_Resultados.R",
    "shared-pipeline/eleicoes/parametros.R","posts/votacao-renda-centil/data/referencia-folha.csv",file.path("shared-pipeline/eleicoes/validacao",
      c("metodos-post.R","cobertura-votos.R","050_Verificar_Espacial_Independente.R","060_Verificar_Metricas_Post.R","bootstrap-municipios.R")))
  stopifnot(all(file.exists(input_paths)))
  before <- tools::md5sum(input_paths)
  provenance <- rbind(
    verificar_csv_do_zip(file.path(dest,"votos2026.zip"),"votacao_secao_2026_BR[.]csv$",file.path(dest,"votos2026")),
    verificar_csv_do_zip(file.path(dest,"locais2026.zip"),"eleitorado_local_votacao_2026_BRASIL[.]csv$",file.path(dest,"locais2026")))
  input_paths <- unique(c(input_paths,provenance$file))
  before <- tools::md5sum(input_paths)
  write.csv(provenance,file.path(dest,"verificacao-zip-csv.csv"),row.names=FALSE)
  guarded_names <- c("centis.csv","sensibilidade.csv","cobertura-setores.csv","cobertura-votos.csv","cobertura-votos-uf.csv","locais-fora-municipio.csv",
    "universo-centis.csv","links-metodos.csv","locais-metodos.csv","metodos-metadata.json",
    "sensibilidade-cortes-fixos.csv","centis-ponderados.csv","centis-razao.csv","conservacao-votos.csv",
    "sensibilidade-ligacao.csv","exclusoes-distancia.csv","cruzamentos-cortes-fixos.csv","diagnostico-renda.csv",
    "centis-originais-comparacao.csv","confronto-metodos.csv","bootstrap-municipios.csv","bootstrap-replicas.csv",
    "bootstrap-cruzamentos.csv","verificacao-bootstrap.csv","bootstrap-metodo.txt","comparacao-folha.csv")
  guarded_paths <- file.path(dest,guarded_names)
  stopifnot(all(file.exists(guarded_paths)))
  guarded_before <- tools::md5sum(guarded_paths)
  tracts <- as.data.frame(readRDS(file.path(dest,"setores-votos.rds")))
  tracts$code_tract <- if(is.numeric(tracts$code_tract))sprintf("%.0f",tracts$code_tract) else as.character(tracts$code_tract)
  stopifnot(!anyDuplicated(tracts$code_tract))
  message("Independent raw election aggregation, explicit year/round/office ...")
  raw <- data.table::fread(raw_path,select=c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","CD_MUNICIPIO","NR_ZONA","NR_LOCAL_VOTACAO","NR_VOTAVEL","QT_VOTOS"))
  raw <- as.data.frame(raw[ANO_ELEICAO==ELEICAO_ANO & NR_TURNO==ELEICAO_TURNO &
    CD_CARGO==ELEICAO_CARGO & SG_UF!="ZZ" & NR_VOTAVEL>0L & NR_VOTAVEL<95L])
  stopifnot(nrow(raw)>0L,all(is.finite(raw$QT_VOTOS)),all(raw$QT_VOTOS>=0))
  key <- with(raw,paste(SG_UF,CD_MUNICIPIO,NR_ZONA,NR_LOCAL_VOTACAO,sep=":"))
  positive <- is.finite(raw$NR_LOCAL_VOTACAO)&raw$NR_LOCAL_VOTACAO>0
  denominator <- tapply(raw$QT_VOTOS[positive],key[positive],sum)
  places <- data.frame(place_key=names(denominator),validos=as.numeric(denominator))
  places <- places[places$validos>0,]
  places$SG_UF <- sub(":.*","",places$place_key)
  linked <- is.finite(tracts$distancia_m)&!is.na(tracts$place_key)
  for(number in c(13L,22L)) {
    take <- positive & raw$NR_VOTAVEL==number
    numerator <- tapply(raw$QT_VOTOS[take],key[take],sum)
    num <- as.numeric(numerator[match(places$place_key,names(numerator))]);num[is.na(num)]<-0
    name <- if(number==13L)"lula" else "flavio"
    places[[name]]<-num
    expected <- 100*num[match(tracts$place_key[linked],places$place_key)]/places$validos[match(tracts$place_key[linked],places$place_key)]
    stopifnot(all(is.finite(expected)),isTRUE(all.equal(tracts[[paste0("p_",name)]][linked],expected,tolerance=1e-10)))
  }
  saveRDS(places,file.path(dest,"votos-locais-verificacao.rds"))
  x <- tracts[tracts$motivo=="incluido",]
  x <- x[order(x$V06004,x$code_tract),]
  stopifnot(nrow(x)>=100L,all(is.finite(x$p_lula)),all(is.finite(x$p_flavio)),all(x$p_lula+x$p_flavio<=100+1e-8))
  bins <- floor((seq_len(nrow(x))-1)*100/nrow(x))+1L
  baseline <- do.call(rbind,lapply(c("lula","flavio"),function(name)data.frame(centil=1:100,
    candidato=if(name=="lula")"Lula (PT)" else "Fl\u00e1vio Bolsonaro (PL)",
    votacao=as.numeric(tapply(x[[paste0("p_",name)]],bins,mean)),
    renda_sm=as.numeric(tapply(x$V06004,bins,mean))/SALARIO_MINIMO_CENSO_2022,n_setores=as.numeric(table(bins)))))
  verificacao_comparar_tabela(baseline,read.csv(file.path(dest,"centis.csv"),fileEncoding="UTF-8"),c("centil","candidato"),"centis.csv")
  coverage <- as.data.frame(table(tracts$motivo),stringsAsFactors=FALSE)
  names(coverage)<-c("motivo","setores")
  coverage<-coverage[coverage$setores>0,]
  verificacao_comparar_tabela(coverage,read.csv(file.path(dest,"cobertura-setores.csv")),"motivo","cobertura-setores.csv")
  reference_folha <- read.csv("posts/votacao-renda-centil/data/referencia-folha.csv",fileEncoding="UTF-8")
  comparison <- merge(reference_folha,baseline[,c("centil","candidato","votacao")],by=c("centil","candidato"),all.x=TRUE)
  comparison$diferenca_pp <- comparison$votacao-comparison$votacao_referencia
  comparison$erro_absoluto_pp <- abs(comparison$diferenca_pp)
  verificacao_comparar_tabela(comparison,read.csv(file.path(dest,"comparacao-folha.csv"),fileEncoding="UTF-8"),c("centil","candidato"),"comparacao-folha.csv")
  links <- read.csv(file.path(dest,"links-metodos.csv"),colClasses=c(code_tract="character"))
  stopifnot(!anyDuplicated(paste(links$code_tract,links$rank)),all(links$rank %in% 1:3),
    all(is.finite(links$distancia_m)),all(links$distancia_m>=0))
  first <- links[links$rank==1L,]
  row <- match(tracts$code_tract[linked],first$code_tract)
  stopifnot(!anyNA(row),identical(as.character(first$place_key[row]),as.character(tracts$place_key[linked])),
    all(abs(first$distancia_m[row]-tracts$distancia_m[linked])<=0.05))
  metadata <- jsonlite::read_json(file.path(dest,"metodos-metadata.json"),simplifyVector=TRUE)
  stopifnot(identical(metadata$universe,"linked"),metadata$minimum_wage_2022_brl==SALARIO_MINIMO_CENSO_2022,
    all(c(1000,2000,5000,10000,Inf) %in% as.numeric(metadata$limits_m)))
  verificar_espacial_independente(tracts,places,dest,size=spatial_size,knn_links=links)
  spatial <- readRDS(file.path(dest,"verificacao-espacial-locais.rds"))
  verificacao_coberturas_base(raw,places,spatial,unique(x$place_key),dest)
  rm(raw,key,denominator,numerator,num,expected,spatial);invisible(gc())
  reference <- verificar_arquivos_metodos(tracts,places,dest)
  old_sensitivity <- read.csv(file.path(dest,"sensibilidade.csv"))
  expected <- reference$centis_originais_comparacao
  expected <- expected[expected$distancia_maxima_m %in% unique(old_sensitivity$distancia_maxima_m),c("centil","renda_sm","n_setores","lula","flavio","distancia_maxima_m")]
  verificacao_comparar_tabela(expected,old_sensitivity,c("centil","distancia_maxima_m"),"sensibilidade.csv")
  boot <- list(centis=read.csv(file.path(dest,"bootstrap-municipios.csv")),
    replicas=read.csv(file.path(dest,"bootstrap-replicas.csv")),cruzamentos=read.csv(file.path(dest,"bootstrap-cruzamentos.csv")))
  stopifnot(identical(unique(boot$centis$replicas),500L))
  proof <- verificar_bootstrap_municipios(x,boot,salario=SALARIO_MINIMO_CENSO_2022)
  verificacao_comparar_tabela(proof,read.csv(file.path(dest,"verificacao-bootstrap.csv")),"check","verificacao-bootstrap.csv")
  stopifnot(all(proof$status=="pass"))
  stopifnot(identical(readLines(file.path(dest,"bootstrap-metodo.txt"),warn=FALSE),
    "Municipalities sampled with replacement; all their tracts repeated; income ranks recomputed in each replicate; exploratory distribution, not census sampling uncertainty."))
  packages <- c("censobr","geobr","sf","data.table","ggplot2","here","httr2","jsonlite","s2")
  write.csv(data.frame(component=c("R",packages),version=c(as.character(getRversion()),
    vapply(packages,function(p)as.character(utils::packageVersion(p)),character(1)))),file.path(dest,"versoes.csv"),row.names=FALSE)
  write.csv(data.frame(file=input_paths,md5=unname(before)),file.path(dest,"checksums.csv"),row.names=FALSE)
  stopifnot(identical(before,tools::md5sum(input_paths)))
  stopifnot(identical(guarded_before,tools::md5sum(guarded_paths)))
  outputs <- c("centis.csv","sensibilidade.csv","cobertura-votos.csv","cobertura-votos-uf.csv","locais-fora-municipio.csv",
    "universo-centis.csv","links-metodos.csv","locais-metodos.csv","metodos-metadata.json",
    "sensibilidade-cortes-fixos.csv","centis-ponderados.csv","centis-razao.csv","conservacao-votos.csv",
    "sensibilidade-ligacao.csv","exclusoes-distancia.csv","cruzamentos-cortes-fixos.csv","diagnostico-renda.csv",
    "centis-originais-comparacao.csv","confronto-metodos.csv","verificacao-metodos.csv",
    "bootstrap-municipios.csv","bootstrap-replicas.csv","bootstrap-cruzamentos.csv","verificacao-bootstrap.csv",
    "verificacao-espacial.csv","verificacao-espacial-chaves.csv","verificacao-espacial-insumos.csv","versoes.csv","checksums.csv",
    "cobertura-setores.csv","comparacao-folha.csv","bootstrap-metodo.txt","verificacao-zip-csv.csv")
  # Original per-UF shapes were independently read during the geometric audit.
  shape_hashes <- read.csv(file.path(dest,"verificacao-espacial-insumos.csv"))
  stopifnot(identical(unname(tools::md5sum(shape_hashes$file)),shape_hashes$md5))
  all_inputs <- c(before,setNames(shape_hashes$md5,shape_hashes$file));all_inputs<-all_inputs[!duplicated(names(all_inputs))]
  certificate <- list(schema_version=1L,status="passed",validated_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    checks=c("raw_year_and_vote_shares","baseline_centiles","spatial_sample_and_k3","coverage_and_outside_municipality",
      "fixed_weighted_ratio_idw_exclusions","income_count_unavailable","bootstrap_500_and_three_independent_resamples","manual_folha_reference_arithmetic","extracted_csv_zip_crc32_and_size"),
    inputmd5=as.list(all_inputs),outputmd5=as.list(setNames(unname(tools::md5sum(file.path(dest,outputs))),outputs)))
  temporary <- paste0(marker,".part")
  jsonlite::write_json(certificate,temporary,auto_unbox=TRUE,pretty=TRUE,na="null")
  if(!file.rename(temporary,marker))stop("Cannot publish validation marker")
  message("PASS: independently validated numerical outputs and geometric sample; publication marker written.")
  invisible(certificate)
}

if(sys.nframe()==0L)verificar_resultados_post()
