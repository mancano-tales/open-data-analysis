# Tiny isolated geospatial fixtures only; no downloads or real-cache writes.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/historico/prepare-spatial-2010-pilot.R", encoding = "UTF-8")
pilot2010_require(); data.table::setDTthreads(1L)
reject <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}
square <- function(x1, y1, x2, y2) sf::st_polygon(list(rbind(c(x1,y1),c(x2,y1),c(x2,y2),c(x1,y2),c(x1,y1))))
municipalities <- sf::st_sf(code_muni = c("1200013","1200054"), code_state = 12,
  geometry = sf::st_sfc(square(-69.1,-10.1,-69,-10),square(-69,-10.1,-68.9,-10),crs=4326))
tract_codes <- c("120001300000001","120001300000002","120005400000001")
tracts <- sf::st_sf(code_tract=tract_codes, code_muni=substr(tract_codes,1,7), code_state=12,
  geometry=sf::st_sfc(square(-69.08,-10.08,-69.07,-10.07),square(-69.009,-10.009,-69.001,-10.001),square(-68.999,-10.009,-68.995,-10.005),crs=4326))
income <- data.frame(code_tract=c(tract_codes,"290010800000001","290010800000002","120001300000099"),
  code_muni=c(substr(tract_codes,1,7),"2900108","2900108","1200013"), code_state=c(12,12,12,29,29,12),
  V001=c(1,1,1,1,1,0),V007=c(100,300,200,50,150,999))
row <- function(ibge,tse,place,x,y,date="03/10/2010") data.frame(code_muni=ibge,code_muni_tse=tse,nr_zona=1,nr_local_votacao=place,
  abbrev_state="AC",dt_eleicao=date,longitude=x,latitude=y)
table <- do.call(rbind,list(row("1200013",1120,1015,-69.075,-10.075,"07/10/2012"),
  row("1200013",1120,1015,-69.075,-10.075),row("1200013",1120,1015,-69.075,-10.075),
  row("1200054",1234,2010,-68.997,-10.007),row("12000",9900,3000,-69.07,-10.07),row(NA,9901,3001,-69.07,-10.07),
  row("1200013",1120,3030,-69.06,-10.06),row("1200013",1120,3030,-69.07,-10.07),
  row("1200013",1120,4040,-68.98,-10.02),row("1200013",1120,5050,-70,-12)))
polling <- sf::st_as_sf(table,coords=c("longitude","latitude"),crs=4326)
result <- pilot2010_link_ac(income,tracts,municipalities,polling)
stopifnot(nrow(result$links)==3L,result$metadata$rank_universe_national==5L,result$metadata$rank_universe_state==3L,
  result$metadata$status=="prepared_not_integrated",result$metadata$census_year==2010L,
  result$metadata$eligible_polling_places==2L,result$metadata$identical_duplicate_rows_collapsed==1L,
  result$metadata$conflicting_place_keys==1L,nrow(result$mapping)==2L,
  sum(result$links$within_5km)==2L,!any(grepl("renda_sm|income_sm",names(result$links))))
far <- result$links[code_tract==tract_codes[2L]]
stopifnot(far$place_key=="AC:1120:1:1015",far$distance_m>5000,far$centil_state==67L,far$centil_national==81L,
  far$income_brl2010==300,far$reason=="beyond_5km",!far$within_5km)
sf::sf_use_s2(TRUE)
spherical <- as.numeric(sf::st_distance(result$tract_points[result$tract_points$code_tract==tract_codes[2L],],
  result$places[result$places$place_key==far$place_key,],by_element=TRUE))
stopifnot(abs(far$distance_m-spherical)<1e-6,
  all(c("not_2010_reference_date","not_an_exact_IBGE2010_municipality","different_2010_municipality",
        "outside_declared_2010_municipality","conflicting_place_key_coordinates","identical_duplicate_collapsed") %in% result$polling_audit$reason))
tie <- sf::st_as_sf(row("1200013",1120,9090,-69.075,-10.075),coords=c("longitude","latitude"),crs=4326)
tied <- pilot2010_link_ac(income,tracts,municipalities,rbind(polling,tie))
stopifnot(tied$links[code_tract==tract_codes[1L],nearest_ties]==2L,
  is.na(tied$links[code_tract==tract_codes[1L],place_key]),
  tied$links[code_tract==tract_codes[1L],reason]=="ambiguous_nearest_tie")
alias <- sf::st_as_sf(row("1200054",1120,9191,-68.97,-10.07),coords=c("longitude","latitude"),crs=4326)
reject(pilot2010_link_ac(income,tracts,municipalities,rbind(polling,alias)),"not bijective")

# End-to-end GeoParquet footer + bounded SQL filter, without loading national sf.
folder <- tempfile("pilot2010-geometry-fixture-");dir.create(folder)
parquet <- file.path(folder,"TEST-ONLY.parquet")
extra <- sf::st_sf(code_tract="290010800000001",code_muni="2900108",code_state=29,
  geometry=sf::st_sfc(square(-39,-13,-38.9,-12.9),crs=4326))
mixed <- rbind(tracts,extra)
raw <- as.list(sf::st_drop_geometry(mixed))
raw$geometry <- arrow::Array$create(lapply(sf::st_as_binary(sf::st_geometry(mixed)), as.raw), type=arrow::binary())
arrow_table <- do.call(arrow::Table$create, raw)
arrow_table <- arrow_table$ReplaceSchemaMetadata(list(geo=jsonlite::toJSON(list(version="1.0.0",primary_column="geometry",columns=list(geometry=list(encoding="WKB",geometry_types=list("Polygon")))),auto_unbox=TRUE)))
arrow::write_parquet(arrow_table,parquet)
selected <- pilot2010_read_ac(parquet)
stopifnot(nrow(selected)==3L,all(selected$code_state==12),
  attr(selected,"pilot_source")$selection=="code_state=12 before collection",
  attr(selected,"pilot_source")$duckdb_memory_limit=="1GB")
projjson <- list(type="GeographicCRS",name="WGS 84",
  datum=list(type="GeodeticReferenceFrame",name="World Geodetic System 1984",
    ellipsoid=list(name="WGS 84",semi_major_axis=6378137,inverse_flattening=298.257223563)),
  coordinate_system=list(subtype="ellipsoidal",axis=list(
    list(name="Latitude",abbreviation="Lat",direction="north",unit="degree"),
    list(name="Longitude",abbreviation="Lon",direction="east",unit="degree"))),
  id=list(authority="EPSG",code=4326))
geo_metadata <- list(version="1.0.0",primary_column="geometry",
  columns=list(geometry=list(encoding="WKB",geometry_types=list("Polygon"),crs=projjson)))
arrow_table <- arrow_table$ReplaceSchemaMetadata(list(geo=jsonlite::toJSON(geo_metadata,auto_unbox=TRUE)))
parquet <- file.path(folder,"TEST-ONLY-projjson.parquet")
arrow::write_parquet(arrow_table,parquet)
stopifnot(sf::st_crs(pilot2010_read_ac(parquet))$epsg==4326L)
geo_metadata$columns$geometry["crs"] <- list(NULL)
arrow_table <- arrow_table$ReplaceSchemaMetadata(list(geo=jsonlite::toJSON(geo_metadata,auto_unbox=TRUE,null="null")))
parquet <- file.path(folder,"TEST-ONLY-null-crs.parquet")
arrow::write_parquet(arrow_table,parquet)
reject(pilot2010_read_ac(parquet),"explicit-null")
message("PASS: Census2010 income ranks before coverage; original TSE keys; exact 2010 municipality and strict point-inside validation; missing/five-digit IDs rejected, no aliases; duplicate/conflict audit; within-municipality S2 nearest and 5km flag; ties unmatched; BRL2010 only; filtered GeoParquet reading under memory cap.")
