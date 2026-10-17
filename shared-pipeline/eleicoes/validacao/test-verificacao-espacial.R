source("shared-pipeline/eleicoes/validacao/050_Verificar_Espacial_Independente.R")
stopifnot(requireNamespace("sf", quietly = TRUE))
fixture <- tempfile("spatial-audit-")
dir.create(fixture); dir.create(file.path(fixture, "malhas"))
box <- function(x, y, half) sf::st_polygon(list(rbind(c(x-half,y-half),c(x+half,y-half),
  c(x+half,y+half),c(x-half,y+half),c(x-half,y-half))))
muni <- sf::st_sf(code_muni=c(3300001,3500001),
  geometry=sf::st_sfc(box(-54,-15,.1),box(-53,-15,.1),crs=4326))
saveRDS(muni,file.path(fixture,"municipios2022.rds"))
poll <- sf::st_sf(abbrev_state=c("RJ","RJ","SP","RJ"),
  code_muni_tse=c(1,1,2,1),nr_zona=1,nr_local_votacao=c(10,11,20,99),
  code_muni=c(3300001,3300001,3500001,3300001),dt_eleicao="04/10/2026",
  geometry=sf::st_sfc(sf::st_point(c(-54,-15)),sf::st_point(c(-54,-15)),
    sf::st_point(c(-53,-15)),sf::st_point(c(-53.0002,-15)),crs=4326))
saveRDS(poll,file.path(fixture,"polling2026.rds"))
tracts <- data.frame(code_tract=c("330000100000001","330000100000002","350000100000001","350000100000002"),
  code_muni=c(3300001,3300001,3500001,3500001),
  place_key=c("RJ:1:1:10","RJ:1:1:10","SP:2:1:20","SP:2:1:20"),
  distancia_m=NA_real_,motivo="incluido")
centres <- c(-54.001,-53.999,-53.001,-52.999)
for(uf in c("33","35")) {
  rows <- which(substr(tracts$code_tract,1,2)==uf)
  shape <- sf::st_sf(code_tract=tracts$code_tract[rows],code_muni=tracts$code_muni[rows],
    geometry=sf::st_sfc(lapply(centres[rows],function(x)box(x,-15,.0001)),crs=4326))
  saveRDS(shape,file.path(fixture,"malhas",paste0("setores2022-",uf,".rds")))
  point <- suppressWarnings(sf::st_transform(sf::st_point_on_surface(sf::st_transform(shape,5880)),4326))
  site <- poll[if(uf=="33")1 else 3,]
  tracts$distancia_m[rows] <- as.numeric(sf::st_distance(point,site))
}
places <- data.frame(place_key=c("RJ:1:1:10","RJ:1:1:11","SP:2:1:20","RJ:1:1:99"),validos=100)
before <- verificacao_amostra_uf(tracts,4L,20261016L)
stopifnot(identical(before,verificacao_amostra_uf(tracts[nrow(tracts):1,],4L,20261016L)))
good <- verificar_espacial_independente(tracts,places,dest=fixture,size=4L)
stopifnot(all(good$pass),all(good$projected_tie_count[good$uf=="33"]==2L),
  !any(grepl(":99$",good$tie_keys)))
links <- do.call(rbind,lapply(seq_len(nrow(tracts)),function(i) {
  keys<-if(i<=2)c("RJ:1:1:10","RJ:1:1:11") else "SP:2:1:20"
  data.frame(code_tract=tracts$code_tract[i],place_key=keys,
    distancia_m=tracts$distancia_m[i],rank=seq_along(keys))
}))
good <- verificar_espacial_independente(tracts,places,dest=fixture,size=4L,knn_links=links)
stopifnot(all(good$knn3_pass))
bad_links<-links;bad_links$place_key[1]<-"RJ:1:1:99"
error<-tryCatch({verificar_espacial_independente(tracts,places,dest=fixture,size=4L,knn_links=bad_links);NULL},error=conditionMessage)
stopifnot(is.character(error))
bad <- tracts;bad$distancia_m[1] <- bad$distancia_m[1]+1
error <- tryCatch({verificar_espacial_independente(bad,places,dest=fixture,size=4L);NULL},error=conditionMessage)
stopifnot(is.character(error),grepl("audit failed",error),
  any(!read.csv(file.path(fixture,"verificacao-espacial.csv"))$pass))
bad <- tracts;bad$place_key[1] <- "RJ:1:1:99"
error <- tryCatch({verificar_espacial_independente(bad,places,dest=fixture,size=4L);NULL},error=conditionMessage)
stopifnot(is.character(error))
message("PASS: deterministic UF sample; projected brute force; exact coordinate ties; municipality exclusion; altered distance/link rejected")
