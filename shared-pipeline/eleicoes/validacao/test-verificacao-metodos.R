if(.Platform$OS.type=="windows")invisible(Sys.setlocale("LC_CTYPE",".UTF-8"))
source("shared-pipeline/eleicoes/validacao/metodos-post.R",encoding="UTF-8")
source("shared-pipeline/eleicoes/validacao/060_Verificar_Metricas_Post.R",encoding="UTF-8")
tracts<-data.frame(code_tract=sprintf("%015d",1:201),code_muni=rep(c(1,2,3),c(100,100,1)),
  code_state=rep(c(11,12,99),c(100,100,1)),V06001=c(rep(c(1,3,2,4),50),1),
  V06004=c(seq_len(200)*1212,.5),situacao=rep(c("Urbana","Rural"),length.out=201))
places<-data.frame(place_key=c("A","B","C","D"),validos=c(100,200,300,400),
  lula=c(80,60,90,80),flavio=c(10,120,150,280))
links<-do.call(rbind,lapply(1:200,function(i)data.frame(code_tract=tracts$code_tract[i],
  place_key=c(if(i<=100)"A" else "D","B","C"),
  distancia_m=if(i==1)c(0,0,9000) else c(if(i%%2)500 else 6000,7000,9000))))
folder<-tempfile("independent-methods-");dir.create(folder)
post_write_methods(tracts,links,places,folder)
reference<-verificar_arquivos_metodos(tracts,places,folder)
stopifnot(nrow(read.csv(file.path(folder,"verificacao-metodos.csv")))==10L,
  all(reference$conservacao_votos$locais_usados==2L),
  all(abs(reference$conservacao_votos$validos_originais-reference$conservacao_votos$validos_alocados)<1e-9))
# A single-cell corruption must fail independently, without invoking production methods.
csv<-file.path(folder,"centis-razao.csv");altered<-read.csv(csv)
altered$lula[which(is.finite(altered$lula))[1]]<-altered$lula[which(is.finite(altered$lula))[1]]+.1
write.csv(altered,csv,row.names=FALSE)
error<-tryCatch({verificar_arquivos_metodos(tracts,places,folder);NULL},error=conditionMessage)
stopifnot(grepl("independent calculation differs in lula",error))
# Missing fixed/weighted bins retain NA; all distances excluded remain absent.
tracts$V06001[1]<-1000000
post_write_methods(tracts,transform(links,distancia_m=distancia_m+100000),places,folder,limits=1)
empty<-verificar_arquivos_metodos(tracts,places,folder)
stopifnot(all(is.na(empty$centis_razao$lula)),all(empty$sensibilidade_cortes_fixos$n_setores==0))
message("PASS: independent base-R reproduction of every method table, zero-distance IDW, missing geography, weighted gaps, conservation and deliberate output corruption")
