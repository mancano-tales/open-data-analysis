# Independent spatial estimator, not an exact reconstruction of Folha's kernel.
# Run from the repository root after 010_Download_Censo_TSE.R.
suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(geobr)
  library(here)
})
options(timeout = max(600, getOption("timeout")))
setDTthreads(2L)
source(here("shared-pipeline", "eleicoes", "parametros.R"))
source(here("shared-pipeline", "eleicoes", "validacao", "cobertura-votos.R"))
source(here("shared-pipeline", "eleicoes", "validacao", "metodos-post.R"))
dest <- here("data-raw", "eleicoes-renda")
unlink(file.path(dest, "validacao-metodologica.json"))
max_distance <- as.numeric(Sys.getenv("VOTOS_MAX_DIST_M", "5000"))
stopifnot(length(max_distance) == 1L, is.finite(max_distance), max_distance > 0)

read_cache <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(suppressWarnings(readRDS(path)), error = function(e) {
    message("Unreadable cache; rebuilding: ", basename(path))
    NULL
  })
}
save_cache <- function(object, path) {
  partial <- paste0(path, ".part")
  saveRDS(object, partial)
  if (!file.copy(partial, path, overwrite = TRUE)) stop("Cannot save cache: ", path)
  unlink(partial)
}

read_zip_csv <- function(zip, pattern, columns) {
  path <- file.path(dest, zip)
  if (!file.exists(path)) stop("Run 010_Download_Censo_TSE.R first: ", zip)
  entries <- unzip(path, list = TRUE)$Name
  member <- entries[grepl(pattern, entries)]
  if (length(member) != 1L) stop("Expected exactly one national CSV in ", zip)
  folder <- file.path(dest, sub("[.]zip$", "", zip))
  target <- file.path(folder, member)
  if (!file.exists(target)) unzip(path, files = member, exdir = folder)
  header <- names(fread(target, nrows = 0, encoding = "Latin-1"))
  if (!all(columns %in% header)) stop("Unexpected TSE schema in ", zip, ": ",
                                     paste(setdiff(columns, header), collapse = ", "))
  fread(target, select = columns, encoding = "Latin-1")
}

votes <- read_zip_csv("votos2026.zip", "votacao_secao_2026_BR[.]csv$",
  c("ANO_ELEICAO", "NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO",
    "CD_CARGO", "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS", "NR_LOCAL_VOTACAO"))
message("Presidential CSV loaded: ", nrow(votes), " rows.")
votes <- votes[ANO_ELEICAO == ELEICAO_ANO & NR_TURNO == ELEICAO_TURNO &
                 CD_CARGO == ELEICAO_CARGO & SG_UF != "ZZ"]
if (!nrow(votes) || anyNA(votes$QT_VOTOS) || any(votes$QT_VOTOS < 0)) stop("Invalid presidential votes.")
validar_candidatos_presidenciais(votes)
# TSE special codes 95+ are not candidates. Other valid candidates stay in denominator.
votes <- votes[NR_VOTAVEL > 0L & NR_VOTAVEL < 95L,
  .(validos = sum(as.numeric(QT_VOTOS)),
    lula = sum(as.numeric(QT_VOTOS[NR_VOTAVEL == 13L])),
    flavio = sum(as.numeric(QT_VOTOS[NR_VOTAVEL == 22L]))),
  by = .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO)]
if (sum(votes$lula) <= 0 || sum(votes$flavio) <= 0) stop("Check candidate numbers in TSE data.")
keys <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
if (anyDuplicated(votes[, ..keys])) stop("Results assign multiple places to one section.")
sections <- read_zip_csv("locais2026.zip", "eleitorado_local_votacao_2026_BRASIL[.]csv$",
  c("AA_ELEICAO", "NR_TURNO", keys, "NR_LOCAL_VOTACAO"))
sections <- unique(sections[AA_ELEICAO == 2026L & NR_TURNO == 1L,
                            c(keys, "NR_LOCAL_VOTACAO"), with = FALSE])
message("Section-to-place register loaded: ", nrow(sections), " sections.")
if (anyDuplicated(sections[, ..keys])) stop("Section maps to more than one polling place.")
setnames(sections, "NR_LOCAL_VOTACAO", "NR_LOCAL_VOTACAO_CADASTRO")
mapped <- merge(votes, sections, by = keys, all.x = TRUE, sort = FALSE)
unmatched <- mapped[is.na(NR_LOCAL_VOTACAO_CADASTRO)]
fwrite(data.table(secoes = nrow(unmatched), votos_validos = sum(unmatched$validos)),
       file.path(dest, "secoes-sem-local.csv"))
fwrite(data.table(secoes = nrow(mapped), sem_cadastro = nrow(unmatched),
  locais_divergentes = sum(mapped$NR_LOCAL_VOTACAO != mapped$NR_LOCAL_VOTACAO_CADASTRO, na.rm = TRUE)),
  file.path(dest, "auditoria-ligacao.csv"))
# Results describe the actual election location; the current register is only
# an audit source and must never silently overwrite the results' place code.
places <- mapped[!is.na(NR_LOCAL_VOTACAO) & NR_LOCAL_VOTACAO > 0,
  .(validos = sum(validos), lula = sum(lula), flavio = sum(flavio)),
  by = .(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO)]
message("Votes aggregated into ", nrow(places), " polling places.")

poll_path <- file.path(dest, "polling2026.rds")
poll <- read_cache(poll_path)
if (is.null(poll)) {
  p <- geobr::read_polling_places(year = 2026)
  if (is.null(p)) stop("geobr polling places unavailable for 2026; do not substitute another year.")
  save_cache(p, poll_path)
  poll <- p
}
message("Geobr polling places loaded: ", nrow(poll), " rows.")
poll <- st_transform(poll, 4326)
poll_xy <- st_coordinates(poll)
poll <- as.data.table(st_drop_geometry(poll))
poll[, `:=`(longitude = poll_xy[, 1], latitude = poll_xy[, 2])]
required <- c("code_muni", "code_muni_tse", "nr_zona", "nr_local_votacao", "abbrev_state", "year")
if (!all(required %in% names(poll)) || any(poll$year != 2026)) stop("Unexpected geobr polling data.")
if ("dt_eleicao" %in% names(poll)) {
  first_round <- as.character(poll$dt_eleicao) %in% c("2026-10-04", "04/10/2026")
  if (!any(first_round)) stop("First-round polling-place date not found.")
  poll <- poll[first_round]
}
poll$place_key <- paste(poll$abbrev_state, poll$code_muni_tse, poll$nr_zona, poll$nr_local_votacao, sep = ":")
# Some releases repeat places across section-level attributes. Collapse only
# identical location/municipality records; discard conflicting coordinates.
locations <- poll[, .(place_key, code_muni, longitude, latitude)]
message("Checking repeated place coordinates ...")
conflicts <- unique(locations)[, .N, by = place_key][N > 1L, place_key]
fwrite(data.table(locais_com_coordenadas_conflitantes = length(conflicts)),
       file.path(dest, "locais-conflitantes.csv"))
poll <- poll[!poll$place_key %in% conflicts & !duplicated(poll$place_key)]
message("Distinct unambiguous polling places: ", nrow(poll), ".")
places[, place_key := paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = ":")]
idx <- match(poll$place_key, places$place_key)
poll$validos <- places$validos[idx]
poll$lula <- places$lula[idx]
poll$flavio <- places$flavio[idx]
poll$p_lula <- 100 * places$lula[idx] / poll$validos
poll$p_flavio <- 100 * places$flavio[idx] / poll$validos
poll <- poll[!is.na(validos) & validos > 0]
# Excludes TSE's (-1,-1) sentinel and impossible coordinates; municipal check follows.
poll <- poll[is.finite(longitude) & is.finite(latitude) &
             longitude >= -74 & longitude <= -28 & latitude >= -34 & latitude <= 6]
poll <- st_as_sf(poll, coords = c("longitude", "latitude"), crs = 4326)
coordinate_keys <- poll$place_key
mun_path <- file.path(dest, "municipios2022.rds")
mun_raw <- read_cache(mun_path)
if (is.null(mun_raw)) {
  mun_raw <- geobr::read_municipality(year = 2022, simplified = FALSE)
  save_cache(mun_raw, mun_path)
}
mun_valid_path <- file.path(dest, "municipios2022-validos5880.rds")
mun <- read_cache(mun_valid_path)
if (is.null(mun)) {
  message("Preparing municipal boundaries in projected coordinates ...")
  mun <- st_make_valid(st_transform(mun_raw, 5880))
  save_cache(mun, mun_valid_path)
}
rm(mun_raw)
message("Checking polling-place coordinates against municipalities ...")
matches <- st_intersects(st_transform(poll, 5880), mun)
in_muni <- vapply(seq_len(nrow(poll)), function(i) {
  poll$code_muni[i] %in% mun$code_muni[matches[[i]]]
}, logical(1))
fwrite(data.table(locais_com_votos = nrow(poll),
                  locais_fora_municipio = sum(!in_muni)), file.path(dest, "cobertura-locais.csv"))
outside <- as.data.table(st_drop_geometry(poll[!in_muni, ]))
outside <- outside[, .(SG_UF = abbrev_state, place_key, code_muni, validos, lula, flavio)]
fwrite(outside, file.path(dest, "locais-fora-municipio.csv"))
poll <- poll[in_muni, ]
saveRDS(poll, file.path(dest, "locais-elegiveis.rds"))
saveRDS(places, file.path(dest, "votos-por-local.rds"))

renda <- as.data.table(readRDS(file.path(dest, "renda2022.rds")))
renda[, code_tract := sprintf("%.0f", code_tract)]
if (anyDuplicated(renda$code_tract)) stop("Duplicate census tracts.")
renda[, elegivel_renda := !is.na(V06001) & V06001 > 0 & is.finite(V06004) & V06004 > 0]
fwrite(renda[, .(setores = .N), by = elegivel_renda], file.path(dest, "cobertura-renda.csv"))
eligible <- renda[elegivel_renda == TRUE]
cache <- file.path(dest, "malhas")
dir.create(cache, showWarnings = FALSE)
rows <- list()
neighbor_rows <- list()
for (uf in sort(unique(eligible$code_state))) {
  message("Matching census tracts, UF ", uf, " ...")
  shape_path <- file.path(cache, paste0("setores2022-", uf, ".rds"))
  shape <- read_cache(shape_path)
  if (is.null(shape)) {
    shape <- geobr::read_census_tract(year = 2022, code_tract = uf, zone = "all", simplified = FALSE)
    if (is.null(shape)) stop("Census tract geometry unavailable for UF ", uf)
    save_cache(shape, shape_path)
  }
  shape$code_tract <- sprintf("%.0f", shape$code_tract)
  shape <- shape[shape$code_tract %in% eligible$code_tract, ]
  point_path <- file.path(cache, paste0("pontos2022-", uf, ".rds"))
  point_key <- paste(tools::md5sum(shape_path), tools::md5sum(file.path(dest, "renda2022.rds")), packageVersion("sf"),
                     paste(sf_extSoftVersion(), collapse = ":"), sep = ":")
  points <- read_cache(point_path)
  if (is.null(points) || !identical(attr(points, "cache_key"), point_key)) {
    points <- st_transform(st_point_on_surface(st_make_valid(st_transform(shape, 5880))), 4326)
    attr(points, "cache_key") <- point_key
    save_cache(points, point_path)
  }
  for (municipio in unique(points$code_muni)) {
    tr <- points[points$code_muni == municipio, ]
    pp <- poll[poll$code_muni == municipio, ]
    result <- data.table(code_tract = tr$code_tract, place_key = NA_character_,
                         distancia_m = NA_real_, p_lula = NA_real_, p_flavio = NA_real_)
    if (nrow(pp)) {
      pp <- pp[order(pp$place_key), ] # deterministic handling of equal-distance sites
      neighbors <- post_k_nearest(tr, pp, k = 3L)
      neighbor_rows[[length(neighbor_rows) + 1L]] <- neighbors
      first <- neighbors[rank == 1L]
      index <- match(result$code_tract, first$code_tract)
      location <- match(first$place_key[index], pp$place_key)
      result[, `:=`(place_key = first$place_key[index],
                     distancia_m = first$distancia_m[index],
                     p_lula = pp$p_lula[location], p_flavio = pp$p_flavio[location])]
    }
    rows[[length(rows) + 1L]] <- result
  }
}
links <- rbindlist(rows)
links3 <- rbindlist(neighbor_rows)
saveRDS(links3, file.path(dest, "links-tres-vizinhos.rds"))
if (anyDuplicated(links$code_tract)) stop("Duplicate tract geometry.")
tracts <- merge(renda, links, by = "code_tract", all.x = TRUE, sort = FALSE)
tracts[, motivo := fifelse(!elegivel_renda, "sem_renda_elegivel",
  fifelse(is.na(distancia_m), "sem_local_ou_malha",
  fifelse(distancia_m > max_distance, "alem_distancia_maxima", "incluido")))]
fwrite(tracts[, .(setores = .N), by = motivo], file.path(dest, "cobertura-setores.csv"))
saveRDS(tracts, file.path(dest, "setores-votos.rds"))
coverage <- calcular_cobertura_votos(votes, places, coordinate_keys, poll$place_key,
                                    unique(tracts[motivo == "incluido", place_key]))
fwrite(coverage$br, file.path(dest, "cobertura-votos.csv"))
fwrite(coverage$uf, file.path(dest, "cobertura-votos-uf.csv"))

aggregate_percentiles <- function(limit) {
  x <- copy(tracts[elegivel_renda & is.finite(distancia_m) & distancia_m <= limit])
  if (nrow(x) < 100L) stop("Fewer than 100 eligible matched tracts.")
  setorder(x, V06004, code_tract)
  # Equal tract counts, with explicit deterministic tie-breaking (not population weights).
  x[, centil := as.integer(floor((seq_len(.N) - 1) * 100 / .N) + 1)]
  x[, .(renda_sm = mean(V06004) / SALARIO_MINIMO_CENSO_2022, n_setores = .N,
         lula = mean(p_lula), flavio = mean(p_flavio)), by = centil]
}
wide <- aggregate_percentiles(max_distance)
centis <- melt(wide, id.vars = c("centil", "renda_sm", "n_setores"),
               measure.vars = c("lula", "flavio"), variable.name = "candidato", value.name = "votacao")
centis[, candidato := fifelse(candidato == "lula", "Lula (PT)", "Fl\u00e1vio Bolsonaro (PL)")]
fwrite(centis, file.path(dest, "centis.csv"))
sens <- rbindlist(lapply(sort(unique(c(1000, 2000, 5000, max_distance))), function(limit) {
  x <- aggregate_percentiles(limit)
  x[, distancia_maxima_m := limit]
  x
}))
fwrite(sens, file.path(dest, "sensibilidade.csv"))
post_write_methods(tracts, links3, places, dest,
  limits = sort(unique(c(1000, 2000, 5000, 10000, Inf, max_distance))),
  universe = "linked", minimum_wage = SALARIO_MINIMO_CENSO_2022)
source(here("shared-pipeline", "eleicoes", "validacao", "bootstrap-municipios.R"))
message("Exploratory municipality bootstrap (500 replicates, fixed seed) ...")
bootstrap <- bootstrap_municipios(tracts[motivo == "incluido"],
  replicas = 500L, seed = 20261016L, salario = SALARIO_MINIMO_CENSO_2022)
fwrite(bootstrap$centis, file.path(dest, "bootstrap-municipios.csv"))
fwrite(bootstrap$cruzamentos, file.path(dest, "bootstrap-cruzamentos.csv"))
fwrite(bootstrap$replicas, file.path(dest, "bootstrap-replicas.csv"))
fwrite(verificar_bootstrap_municipios(tracts[motivo == "incluido"], bootstrap,
  salario = SALARIO_MINIMO_CENSO_2022), file.path(dest, "verificacao-bootstrap.csv"))
writeLines(bootstrap$metodo, file.path(dest, "bootstrap-metodo.txt"))
reference <- fread(here("posts", "votacao-renda-centil", "data", "referencia-folha.csv"),
                   encoding = "UTF-8")
comparison <- merge(reference, centis[, .(centil, candidato, votacao)],
                    by = c("centil", "candidato"), all.x = TRUE)
comparison[, diferenca_pp := votacao - votacao_referencia]
comparison[, erro_absoluto_pp := abs(diferenca_pp)]
fwrite(comparison, file.path(dest, "comparacao-folha.csv"))
writeLines(c(paste("maximum_distance_m:", max_distance),
             paste("polling2026.rds md5:", tools::md5sum(poll_path)),
             capture.output(sessionInfo())), file.path(dest, "session-spatial.txt"))
message("Percentiles and coverage saved in data-raw/eleicoes-renda/.")
