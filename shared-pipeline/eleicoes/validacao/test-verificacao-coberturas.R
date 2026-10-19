# Independent base-R reference must reproduce missing-location and zero-stage scopes.
source("shared-pipeline/eleicoes/030_Verificar_Resultados.R",encoding="UTF-8")
source("shared-pipeline/eleicoes/validacao/cobertura-votos.R")
sections <- data.frame(SG_UF=c("AC","AC","SP"),CD_MUNICIPIO=c(1L,1L,2L),NR_ZONA=1L,
  NR_LOCAL_VOTACAO=c(10L,NA_integer_,20L),validos=c(50,10,40),lula=c(15,5,10),flavio=c(20,5,20))
places <- sections[c(1,3),c("SG_UF","validos","lula","flavio")]
places$place_key<-c("AC:1:1:10","SP:2:1:20")
raw <- do.call(rbind,lapply(seq_len(nrow(sections)),function(i)data.frame(
  SG_UF=sections$SG_UF[i],NR_VOTAVEL=c(13L,22L,12L),
  QT_VOTOS=c(sections$lula[i],sections$flavio[i],sections$validos[i]-sections$lula[i]-sections$flavio[i]))))
spatial <- list(coordinate_keys=places$place_key,eligible_keys=places$place_key[1],
  outside=data.frame(place_key=places$place_key[2],code_muni="2"))
folder<-tempfile("coverage-reference-");dir.create(folder)
actual<-calcular_cobertura_votos(sections,places,spatial$coordinate_keys,spatial$eligible_keys,places$place_key[1])
write.csv(actual$br,file.path(folder,"cobertura-votos.csv"),row.names=FALSE)
write.csv(actual$uf,file.path(folder,"cobertura-votos-uf.csv"),row.names=FALSE)
outside<-data.frame(SG_UF="SP",place_key=places$place_key[2],code_muni=2,validos=40,lula=10,flavio=20)
write.csv(outside,file.path(folder,"locais-fora-municipio.csv"),row.names=FALSE)
verificacao_coberturas_base(raw,places,spatial,places$place_key[1],folder)
actual$br$validos[1]<-actual$br$validos[1]+1
write.csv(actual$br,file.path(folder,"cobertura-votos.csv"),row.names=FALSE)
stopifnot(inherits(try(verificacao_coberturas_base(raw,places,spatial,places$place_key[1],folder),silent=TRUE),"try-error"))
message("PASS: independent BR/UF coverage, missing location, outside municipality, empty stages and one-vote corruption")
