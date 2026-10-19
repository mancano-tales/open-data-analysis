if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/validacao/metodos-post.R", encoding = "UTF-8")
library(data.table); setDTthreads(2L)
tracts <- data.table(code_tract = sprintf("%015d", 1:200), code_muni = rep(c("1100015", "1200013"), each = 100),
  code_state = rep(c(11L, 12L), each = 100), V06001 = rep(c(1, 3, 2, 4), 50),
  V06004 = seq_len(200) * 1212, situacao = rep(c("Urbana", "Rural"), 100))
places <- data.table(place_key = c("A", "B", "C", "D"), validos = c(100, 200, 300, 400),
  lula = c(80, 60, 90, 80), flavio = c(10, 120, 150, 280))
links <- rbindlist(lapply(seq_len(200), function(i) {
  nearest <- if (i <= 100L) "A" else "D"
  data.table(code_tract = tracts$code_tract[[i]], place_key = c(nearest, "B", "C"),
    distancia_m = c(if (i %% 2L) 500 else 6000, 7000, 9000))
}))
r <- post_methods(tracts, links, places)
stopifnot(nrow(r$universo_centis) == 200, nrow(r$sensibilidade_cortes_fixos) == 500,
  all(r$sensibilidade_cortes_fixos$n_setores_total == 2),
  sum(r$sensibilidade_cortes_fixos[distancia_maxima_m == 5000, n_setores]) == 100)
# Fixed bins remain the ORIGINAL pair, and are not re-ranked after exclusions.
stopifnot(all(r$sensibilidade_cortes_fixos[distancia_maxima_m == 5000, n_setores] == 1),
  all(r$sensibilidade_cortes_fixos[distancia_maxima_m == 5000, renda_sm] == seq(1.5, 199.5, by = 2)))
expected_weighted <- floor(100 * c(0, head(cumsum(tracts$V06001), -1)) / sum(tracts$V06001)) + 1L
stopifnot(identical(r$universo_centis$centil_ponderado, as.integer(expected_weighted)))
# Ratio allocation conserves used-place votes once, regardless of tract count.
for (limit in c(1000, 2000, 5000, 10000, Inf)) {
  z <- r$conservacao_votos[distancia_maxima_m == limit]
  stopifnot(abs(z$validos_originais - z$validos_alocados) < 1e-9,
    abs(z$lula_originais - z$lula_alocados) < 1e-9,
    abs(z$flavio_originais - z$flavio_alocados) < 1e-9,
    z$locais_usados == 2L, z$validos_originais == 500)
}
# Independent brute force in R base: allocate each used local across sectors.
u <- as.data.frame(r$universo_centis); nearest <- as.data.frame(r$links[rank == 1 & distancia_m <= 5000])
obs <- merge(u, nearest, by = "code_tract")
p <- as.data.frame(places); q <- match(obs$place_key, p$place_key)
a <- obs$V06001 / ave(obs$V06001, obs$place_key, FUN = sum)
num <- tapply(p$lula[q] * a, obs$centil, sum); den <- tapply(p$validos[q] * a, obs$centil, sum)
stopifnot(max(abs(r$centis_razao[distancia_maxima_m == 5000, lula] - 100 * num / den)) < 1e-10)
# IDW exact-zero policy and deterministic ties by place key, not input order.
zeros <- data.table(code_tract = "z", place_key = c("C", "B", "A"), distancia_m = c(1, 0, 0))
z <- post_idw(post_links(zeros, places), Inf, "idw3")
stopifnot(z[place_key == "A", peso] == .5, z[place_key == "B", peso] == .5, z[place_key == "C", peso] == 0)
z <- post_idw(post_links(data.table(code_tract = "x", place_key = c("B", "A", "C"), distancia_m = c(2, 1, 4)), places), Inf, "idw3")
stopifnot(max(abs(z$peso - c(1, .5, .25) / 1.75)) < 1e-12)
# Real missing geography is excluded separately, and linked/default universe
# remains distinguishable from explicitly requested income universe.
no_geo <- copy(tracts[1])[, `:=`(code_tract = "missing", V06004 = .5, code_state = 99L)]
expanded <- rbind(tracts, no_geo)
rr <- post_methods(expanded, links, places)
stopifnot(nrow(rr$universo_centis) == 200, nrow(post_methods(expanded, links, places, universe = "income")$universo_centis) == 201,
  all(rr$exclusoes_distancia[code_state == 99, motivo] == "sem_local_ou_malha"),
  all(is.na(rr$diagnostico_renda$fracao_sem_rendimento)),
  all(grepl("indisponivel", rr$diagnostico_renda$status)))
# Large weighted tracts leave empty centiles; absent estimates are NA, not zero.
large <- copy(tracts); large[1, V06001 := 1000000]
large_result <- post_methods(large, links, places, limits = 5000)
stopifnot(any(large_result$centis_ponderados$n_setores_total == 0),
  all(is.na(large_result$centis_ponderados[n_setores == 0, lula])))
none <- post_methods(tracts, copy(links)[, distancia_m := distancia_m + 100000], places, limits = 1)
stopifnot(all(none$sensibilidade_cortes_fixos$n_setores == 0), all(is.na(none$centis_razao$lula)))
# The orchestrator produces review inputs/outputs without changing centis.csv.
tmp <- tempfile("post-methods-");dir.create(tmp)
post_write_methods(tracts, links, places, tmp)
stopifnot(file.exists(file.path(tmp, "links-metodos.csv")), file.exists(file.path(tmp, "metodos-metadata.json")),
  !file.exists(file.path(tmp, "centis.csv")))
for (f in list.files(tmp, full.names = TRUE)) unlink(f)
unlink(tmp)
if (requireNamespace("sf", quietly = TRUE)) {
  points <- sf::st_as_sf(data.frame(code_tract = c("x", "y"), code_muni = c("m1", "m2"), x = c(0, 0), y = c(0, 0)), coords = c("x", "y"), crs = 4326)
  pp <- sf::st_as_sf(data.frame(place_key = c("C", "B", "A", "OTHER"), code_muni = c("m1", "m1", "m1", "m2"), x = c(.01, 0, 0, .1), y = 0), coords = c("x", "y"), crs = 4326)
  spatial <- post_k_nearest(points, pp, chunk_size = 1)
  brute <- post_k_nearest(points, pp, chunk_size = 1, algorithm = "bruteforce")
  stopifnot(identical(spatial[code_tract == "x", place_key], c("A", "B", "C")),
    identical(spatial[code_tract == "y", place_key], "OTHER"), all(spatial[place_key %in% c("A", "B"), distancia_m] == 0))
  stopifnot(identical(spatial$place_key, brute$place_key), max(abs(spatial$distancia_m - brute$distancia_m)) < 1e-6)
  # More than four tied sites force indexed boundary expansion. Changing input
  # row order cannot change the three lexicographically smallest winning keys.
  many <- sf::st_as_sf(data.frame(place_key = c("G", "D", "F", "B", "A", "E", "C", "H"),
    code_muni = "m1", x = c(rep(0, 7), .01), y = 0), coords = c("x", "y"), crs = 4326)
  many_index <- post_k_nearest(points[1, ], many)
  many_brute <- post_k_nearest(points[1, ], many, algorithm = "bruteforce")
  stopifnot(identical(many_index$place_key, c("A", "B", "C")), identical(many_index$place_key, many_brute$place_key))
  set.seed(20261016)
  random_points <- sf::st_as_sf(data.frame(code_tract = paste0("t", 1:25), code_muni = "m1",
    x = runif(25, -51, -50), y = runif(25, -11, -10)), coords = c("x", "y"), crs = 4326)
  random_places <- sf::st_as_sf(data.frame(place_key = paste0("p", 1:35), code_muni = "m1",
    x = runif(35, -51, -50), y = runif(35, -11, -10)), coords = c("x", "y"), crs = 4326)
  indexed <- post_k_nearest(random_points, random_places)
  exhaustive <- post_k_nearest(random_points, random_places, algorithm = "bruteforce", chunk_size = 4)
  stopifnot(identical(indexed$place_key, exhaustive$place_key), max(abs(indexed$distancia_m - exhaustive$distancia_m)) < 1e-6)
}
message("PASS: fixed and weighted universes, conservative local allocation, IDW zeros/ties, missing/empty groups, output contract and bounded municipal k3.")
