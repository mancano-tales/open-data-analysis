# Independent reference calculations: base R only, no calls to metodos-post.R.
verificacao_comparar_tabela <- function(expected, actual, keys, label, tolerance = 1e-8) {
  expected <- as.data.frame(expected); actual <- as.data.frame(actual)
  if (!all(names(expected) %in% names(actual))) stop(label, ": missing columns")
  signature <- function(x) do.call(paste, c(lapply(x[, keys, drop=FALSE], as.character), sep="\r"))
  a <- signature(expected); b <- signature(actual)
  if (anyDuplicated(a) || anyDuplicated(b) || !setequal(a,b)) stop(label, ": different/duplicate row keys")
  actual <- actual[match(a,b),names(expected),drop=FALSE]
  for (field in names(expected)) {
    e <- expected[[field]]; v <- actual[[field]]
    good <- if (is.numeric(e)) isTRUE(all.equal(as.numeric(e),as.numeric(v),check.attributes=FALSE,tolerance=tolerance)) else
      identical(as.character(e),as.character(v))
    if (!good) stop(label, ": independent calculation differs in ",field)
  }
  invisible(TRUE)
}

verificacao_metricas_base <- function(tracts, links, places, universe="linked",
    limits=c(1000,2000,5000,10000,Inf), minimum_wage=1212) {
  x <- as.data.frame(tracts,stringsAsFactors=FALSE)
  code <- function(z) if(is.numeric(z))sprintf("%.0f",z) else as.character(z)
  x$code_tract <- code(x$code_tract)
  if(anyDuplicated(x$code_tract))stop("Duplicate independent income universe")
  eligible <- is.finite(x$V06001)&x$V06001>0&is.finite(x$V06004)&x$V06004>0
  all_income <- x[eligible,,drop=FALSE]
  if(!"situacao" %in% names(all_income))all_income$situacao <- "nao_disponivel"
  all_income$situacao[is.na(all_income$situacao)|all_income$situacao==""] <- "nao_disponivel"
  all_income <- all_income[order(all_income$V06004,all_income$code_tract),,drop=FALSE]
  all_income$decil <- floor((seq_len(nrow(all_income))-1)*10/nrow(all_income))+1L
  l <- as.data.frame(links,stringsAsFactors=FALSE);l$code_tract<-code(l$code_tract)
  l <- l[is.finite(l$distancia_m)&!is.na(l$place_key),,drop=FALSE]
  p <- as.data.frame(places,stringsAsFactors=FALSE)
  if(anyDuplicated(p$place_key))stop("Duplicate independent place votes")
  where <- match(l$place_key,p$place_key)
  if(anyNA(where))stop("Auxiliary links reference unknown raw place")
  l <- l[p$validos[where]>0,,drop=FALSE]
  l <- l[order(l$code_tract,l$distancia_m,l$place_key),,drop=FALSE]
  l$rank <- sequence(rle(l$code_tract)$lengths)
  l <- l[l$rank<=3L,,drop=FALSE]
  u <- if(universe=="linked")all_income[all_income$code_tract %in% l$code_tract,,drop=FALSE] else all_income
  if(!universe %in% c("linked","income")||!nrow(u))stop("Invalid independent fixed universe")
  u$centil <- floor((seq_len(nrow(u))-1)*100/nrow(u))+1L
  u$centil_ponderado <- pmin(100L,floor(100*(cumsum(u$V06001)-u$V06001)/sum(u$V06001))+1L)
  u$decil <- (u$centil-1L)%/%10L+1L
  l$income_row <- match(l$code_tract,u$code_tract)
  where <- match(l$place_key,p$place_key)
  l$lula <- 100*p$lula[where]/p$validos[where]
  l$flavio <- 100*p$flavio[where]/p$validos[where]
  curve <- function(full,assigned,weighted=FALSE) {
    bins <- if(weighted)full$centil_ponderado else full$centil
    mass <- if(weighted)full$V06001 else rep(1,nrow(full))
    mean_w <- function(v,w)if(length(v))sum(v*w)/sum(w) else NA_real_
    do.call(rbind,lapply(1:100,function(bin) {
      rows <- which(bins==bin);obs <- rows[is.finite(assigned$lula[rows])&is.finite(assigned$flavio[rows])]
      data.frame(centil=bin,renda_sm=mean_w(full$V06004[rows],mass[rows])/minimum_wage,
        n_setores_total=length(rows),peso_V06001_total=sum(full$V06001[rows]),
        n_setores=length(obs),peso_V06001=sum(full$V06001[obs]),
        renda_sm_observada=mean_w(full$V06004[obs],mass[obs])/minimum_wage,
        lula=mean_w(assigned$lula[obs],mass[obs]),flavio=mean_w(assigned$flavio[obs],mass[obs]))
    }))
  }
  assigned <- function(limit,idw=FALSE) {
    take <- which(!is.na(l$income_row)&l$distancia_m<=limit&(idw|l$rank==1L))
    out <- data.frame(lula=rep(NA_real_,nrow(u)),flavio=NA_real_)
    if(!length(take))return(out)
    group <- l$income_row[take];distance <- l$distancia_m[take]
    zero <- tabulate(group[distance==0],nbins=nrow(u))>0
    weight <- numeric(length(take));weight[zero[group]] <- as.numeric(distance[zero[group]]==0)
    weight[!zero[group]] <- 1/distance[!zero[group]]
    totals <- rowsum(cbind(weight,weight*l$lula[take],weight*l$flavio[take]),group,reorder=FALSE)
    indices <- as.integer(rownames(totals))
    out$lula[indices] <- totals[,2]/totals[,1];out$flavio[indices] <- totals[,3]/totals[,1]
    out
  }
  nearest <- l[l$rank==1L,,drop=FALSE]
  nearest_row <- match(u$code_tract,nearest$code_tract)
  nearest_place <- nearest$place_key[nearest_row]
  nearest_distance <- nearest$distancia_m[nearest_row]
  full_nearest <- match(all_income$code_tract,nearest$code_tract)
  full_distance <- nearest$distancia_m[full_nearest]
  fixed<-weighted<-ratio<-conservation<-linkage<-original<-exclusions<-crossings<-list()
  crossing <- function(z,limit,rule) {
    over <- is.finite(z$lula)&is.finite(z$flavio)&z$flavio>z$lula
    sustained <- vapply(1:100,function(i)all(over[i:100]),logical(1))
    data.frame(primeiro_centil=if(any(over))which(over)[1] else NA_integer_,
      primeiro_centil_sustentado=if(any(sustained))which(sustained)[1] else NA_integer_,
      n_intervalos_superacao=sum(over&!c(FALSE,head(over,-1))),distancia_maxima_m=limit,regra=rule)
  }
  for(i in seq_along(limits)) {
    limit <- limits[i];a <- assigned(limit);a3<-assigned(limit,TRUE)
    fixed[[i]] <- curve(u,a);weighted[[i]] <- curve(u,a,TRUE)
    used <- which(is.finite(a$lula)&is.finite(a$flavio))
    sampled <- u[used,,drop=FALSE]
    sampled$centil <- floor((seq_len(nrow(sampled))-1)*100/max(1,nrow(sampled)))+1L
    original[[i]] <- curve(sampled,a[used,,drop=FALSE])
    alternate <- curve(u,a3)
    linkage[[i]] <- rbind(transform(fixed[[i]],regra="nearest"),transform(alternate,regra="idw3"))
    allocated <- matrix(NA_real_,100,3,dimnames=list(NULL,c("validos_alocados","lula_alocados","flavio_alocados")))
    mass_error <- 0;vote_original <- vote_allocated <- c(validos=0,lula=0,flavio=0)
    if(length(used)) {
      place <- nearest_place[used]
      denom <- tapply(u$V06001[used],place,sum)
      share <- u$V06001[used]/as.numeric(denom[place])
      mass <- tapply(share,place,sum);mass_error<-max(abs(mass-1))
      count <- as.matrix(p[match(place,p$place_key),c("validos","lula","flavio")])*share
      bin <- u$centil[used]
      sums <- rowsum(count,bin,reorder=FALSE)
      allocated[as.integer(rownames(sums)),] <- sums
      vote_original <- colSums(p[match(unique(place),p$place_key),c("validos","lula","flavio")])
      vote_allocated <- colSums(count)
    }
    ratio[[i]] <- fixed[[i]]
    ratio[[i]]$lula <- 100*allocated[,2]/allocated[,1];ratio[[i]]$flavio <- 100*allocated[,3]/allocated[,1]
    ratio[[i]] <- cbind(ratio[[i]],as.data.frame(allocated))
    conservation[[i]] <- data.frame(locais_usados=length(unique(nearest_place[used])),erro_maximo_massa_local=mass_error)
    for(name in c("validos","lula","flavio")) {
      conservation[[i]][[paste0(name,"_originais")]] <- vote_original[[name]]
      conservation[[i]][[paste0(name,"_alocados")]] <- vote_allocated[[name]]
    }
    reason <- ifelse(!is.finite(full_distance),"sem_local_ou_malha",ifelse(full_distance>limit,"alem_distancia_maxima","incluido"))
    group <- list(code_state=all_income$code_state,decil=all_income$decil,situacao=all_income$situacao,motivo=reason)
    exclusions[[i]] <- aggregate(data.frame(n_setores=rep(1,nrow(all_income)),peso_V06001=all_income$V06001),group,sum)
    crossings[[i]] <- rbind(crossing(fixed[[i]],limit,"nearest"),crossing(alternate,limit,"idw3"))
    for(name in c("fixed","weighted","ratio","conservation","linkage","original","exclusions")) {
      obj<-get(name);obj[[i]]$distancia_maxima_m<-limit;assign(name,obj)
    }
  }
  stack <- function(z)do.call(rbind,z)
  fixed<-stack(fixed);weighted<-stack(weighted);ratio<-stack(ratio);original<-stack(original);linkage<-stack(linkage)
  comparisons<-list()
  for(reference in c("original_reranked","nearest_fixed"))for(method in c("nearest_fixed","weighted","ratio","idw3")) {
    if(method==reference)next
    ref<-if(reference=="original_reranked")original else fixed
    other<-switch(method,nearest_fixed=fixed,weighted=weighted,ratio=ratio,idw3=linkage[linkage$regra=="idw3",])
    for(candidate in c("lula","flavio"))for(limit in limits) {
      e<-ref[ref$distancia_maxima_m==limit,candidate];v<-other[other$distancia_maxima_m==limit,candidate]
      d<-abs(e-v);d<-d[is.finite(d)]
      comparisons[[length(comparisons)+1L]]<-data.frame(distancia_maxima_m=limit,
        n_centis_comparados=length(d),diferenca_maxima_pp=if(length(d))max(d) else NA_real_,
        metodo_referencia=reference,metodo_comparado=method,candidato=candidate)
    }
  }
  income_diagnostic <- fixed[fixed$distancia_maxima_m==limits[1]&fixed$n_setores_total>0,c("centil","n_setores_total","peso_V06001_total")]
  income_diagnostic$fracao_sem_rendimento <- NA_real_
  income_diagnostic$status <- "indisponivel_sem_contagem_de_responsaveis_com_rendimento"
  list(universo_centis=u,links=l[,c("code_tract","place_key","distancia_m","rank")],
    sensibilidade_cortes_fixos=fixed,centis_ponderados=weighted,centis_razao=ratio,
    conservacao_votos=stack(conservation),sensibilidade_ligacao=linkage,
    exclusoes_distancia=stack(exclusions),cruzamentos=stack(crossings),
    diagnostico_renda=income_diagnostic,centis_originais_comparacao=original,confronto_metodos=stack(comparisons))
}

verificar_arquivos_metodos <- function(tracts, places, dest="data-raw/eleicoes-renda") {
  metadata <- jsonlite::read_json(file.path(dest,"metodos-metadata.json"),simplifyVector=TRUE)
  links <- read.csv(file.path(dest,"links-metodos.csv"),stringsAsFactors=FALSE,colClasses=c(code_tract="character"))
  local <- read.csv(file.path(dest,"locais-metodos.csv"),stringsAsFactors=FALSE)
  independent <- places[match(local$place_key,places$place_key),c("place_key","validos","lula","flavio")]
  verificacao_comparar_tabela(independent,local,"place_key","locais-metodos")
  reference <- verificacao_metricas_base(tracts,links,places,universe=metadata$universe,
    limits=as.numeric(metadata$limits_m),minimum_wage=metadata$minimum_wage_2022_brl)
  outputs <- c(sensibilidade_cortes_fixos="sensibilidade-cortes-fixos.csv",centis_ponderados="centis-ponderados.csv",
    centis_razao="centis-razao.csv",conservacao_votos="conservacao-votos.csv",sensibilidade_ligacao="sensibilidade-ligacao.csv",
    exclusoes_distancia="exclusoes-distancia.csv",cruzamentos="cruzamentos-cortes-fixos.csv",
    diagnostico_renda="diagnostico-renda.csv",centis_originais_comparacao="centis-originais-comparacao.csv",confronto_metodos="confronto-metodos.csv")
  evidence <- list()
  for(name in names(outputs)) {
    expected <- reference[[name]];actual <- read.csv(file.path(dest,outputs[[name]]),stringsAsFactors=FALSE)
    keys <- switch(name,exclusoes_distancia=c("code_state","decil","situacao","motivo","distancia_maxima_m"),
      conservacao_votos="distancia_maxima_m",cruzamentos=c("distancia_maxima_m","regra"),
      diagnostico_renda="centil",confronto_metodos=c("distancia_maxima_m","metodo_referencia","metodo_comparado","candidato"),
      sensibilidade_ligacao=c("centil","distancia_maxima_m","regra"),c("centil","distancia_maxima_m"))
    verificacao_comparar_tabela(expected,actual,keys,outputs[[name]])
    evidence[[length(evidence)+1L]]<-data.frame(arquivo=outputs[[name]],linhas=nrow(actual),status="pass",calculo="R base independente")
  }
  expected<-reference$universo_centis[,c("code_tract","code_muni","code_state","V06001","V06004","situacao","centil","centil_ponderado","decil")]
  actual<-read.csv(file.path(dest,"universo-centis.csv"),stringsAsFactors=FALSE,colClasses=c(code_tract="character"))
  verificacao_comparar_tabela(expected,actual,"code_tract","universo-centis.csv")
  write.csv(do.call(rbind,evidence),file.path(dest,"verificacao-metodos.csv"),row.names=FALSE)
  invisible(reference)
}
