# Gate and publishing-contract fixture; it does not certify national data.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
source("shared-pipeline/eleicoes/040_Preparar_Post.R", encoding = "UTF-8")
stopifnot(requireNamespace("jsonlite", quietly = TRUE))
reject <- function(expr, pattern) {
  msg <- tryCatch({force(expr); NULL}, error = conditionMessage)
  stopifnot(is.character(msg), grepl(pattern, msg))
}
root <- tempfile("post-publication-");dir.create(root)
raw <- file.path(root, "data-raw/eleicoes-renda");dir.create(raw, recursive = TRUE)
public <- file.path(root, "posts/votacao-renda-centil/data");dir.create(public, recursive = TRUE)
put <- function(name, x) utils::write.csv(x, file.path(raw, name), row.names = FALSE)
for (name in post_snapshot_files()) {
  if (grepl("[.]csv$", name)) put(name, data.frame(fixture = 1))
  else writeLines(if (grepl("[.]json$", name)) "{}" else "fixture", file.path(raw, name))
}
coverage <- data.frame(etapa = c("resultado_nacional", "locais_usados"), validos = c(1000, 600), locais = c(10, 6),
  validos_fracao_br = c(1, .6), lula_pct_validos = c(45, 40), flavio_pct_validos = c(50, 55))
put("cobertura-votos.csv", coverage)
baseline <- rbind(data.frame(centil = 1:100, candidato = "Lula (PT)", votacao = 40, renda_sm = 1:100, n_setores = 2),
  data.frame(centil = 1:100, candidato = "Flavio Bolsonaro (PL)", votacao = 55, renda_sm = 1:100, n_setores = 2))
put("centis.csv", baseline)
put("centis-originais-comparacao.csv", data.frame(centil = 1:100, distancia_maxima_m = 5000,
  lula = 40, flavio = 55, renda_sm = 1:100, n_setores = 2))
ex <- expand.grid(decil = 1:10, situacao = c("Urbana", "Rural"),
  motivo = c("incluido", "alem_distancia_maxima", "sem_local_ou_malha"), stringsAsFactors = FALSE)
ex$n_setores <- ifelse(ex$motivo == "incluido", 80, ifelse(ex$motivo == "alem_distancia_maxima", 15, 5))
ex$distancia_maxima_m <- 5000;put("exclusoes-distancia.csv", ex)
put("verificacao-espacial.csv", data.frame(uf = c("AC", "SP"), pass = TRUE, knn3_pass = TRUE,
  saved_geodesic_error_m = c(.01, .02), projected_diagnostic_pass = c(TRUE, FALSE), seed = 20261016))
put("locais-fora-municipio.csv", data.frame(place_key = c("AC:1:1:1", "SP:2:2:2"),
  validos = c(10, 20), lula = c(4, 6), flavio = c(5, 10)))
put("cruzamentos-cortes-fixos.csv", data.frame(distancia_maxima_m = c(1000, 2000, 5000, 10000, Inf),
  regra = "nearest", primeiro_centil = c(34, 35, 35, 36, 37), primeiro_centil_sustentado = c(36, 36, 37, 38, 39)))
put("confronto-metodos.csv", data.frame(distancia_maxima_m = 5000,
  metodo_referencia = c(rep("original_reranked", 6), rep("nearest_fixed", 2)),
  metodo_comparado = c(rep(c("nearest_fixed", "weighted", "ratio"), each = 2), "idw3", "idw3"),
  candidato = rep(c("lula", "flavio"), 4), diferenca_maxima_pp = 1:8, n_centis_comparados = 100))
put("bootstrap-cruzamentos.csv", data.frame(replica = 1:500,
  primeiro_centil_flavio_supera_lula = rep(c(35L, 36L), each = 250), seed = 20261016))
put("bootstrap-replicas.csv", data.frame(replica = rep(1:500, each = 100), centil = rep(1:100, 500),
  lula = 40, flavio = 55, renda_sm = rep(1:100, 500)))
put("comparacao-folha.csv", data.frame(candidato = c("Lula (PT)", "Flavio Bolsonaro (PL)"),
  erro_absoluto_pp = c(1, 2), incerteza_visual_pp = 1.5))
reference_key <- "posts/votacao-renda-centil/data/referencia-folha.csv"
utils::write.csv(data.frame(centil = 46, candidato = "Lula (PT)", votacao_referencia = 41.12), file.path(root, reference_key), row.names = FALSE)
writeLines("input", file.path(root, "input.txt"))
# Extra certified private outputs must still never enter the public snapshot.
put("links-metodos.csv", data.frame(code_tract = "private", place_key = "private"))
checks <- c("raw_year_and_vote_shares", "official_legal_validity_every_zone", "baseline_centiles", "spatial_sample_and_k3",
  "coverage_and_outside_municipality", "fixed_weighted_ratio_idw_exclusions", "income_count_unavailable",
  "bootstrap_500_and_three_independent_resamples")
output_names <- c(post_snapshot_files(), "verificacao-espacial.csv", "locais-fora-municipio.csv", "links-metodos.csv")
cert <- list(schema_version = 1L, status = "passed", validated_at_utc = "2026-10-16T12:00:00Z", checks = checks,
  inputmd5 = as.list(setNames(unname(tools::md5sum(file.path(root, c("input.txt", reference_key)))), c("input.txt", reference_key))),
  outputmd5 = as.list(setNames(unname(tools::md5sum(file.path(raw, output_names))), output_names)))
marker <- file.path(raw, "validacao-metodologica.json")
save_marker <- function(x = cert) jsonlite::write_json(x, marker, auto_unbox = TRUE, pretty = TRUE)
reject(preparar_post(root = root, destination = "published"), "Run 030")
bad <- cert;bad$status <- "failed";save_marker(bad)
reject(preparar_post(root = root, destination = "published"), "not passed")
bad <- cert;bad$checks <- setdiff(bad$checks,"official_legal_validity_every_zone");save_marker(bad)
reject(preparar_post(root = root, destination = "published"), "Missing independent checks")
save_marker();writeLines("changed input", file.path(root, "input.txt"))
reject(preparar_post(root = root, destination = "published"), "Stale input")
writeLines("input", file.path(root, "input.txt"))
original <- readLines(file.path(raw, "centis.csv"));writeLines("bad", file.path(raw, "centis.csv"))
reject(preparar_post(root = root, destination = "published"), "Stale output")
writeLines(original, file.path(raw, "centis.csv"))
bad <- cert;bad$outputmd5$`centis.csv` <- NULL;save_marker(bad)
reject(preparar_post(root = root, destination = "published"), "absent from independent certificate")
bad <- cert;bad$inputmd5 <- setNames(list(strrep("a", 32)), "../outside");save_marker(bad)
reject(preparar_post(root = root, destination = "published"), "safe repository-relative")
save_marker()
result <- preparar_post(root = root, destination = "published")
destination <- file.path(root, "published")
stopifnot(file.exists(file.path(destination, "validacao-metodologica.json")),
  file.exists(file.path(destination, "bootstrap-replicas.csv")),
  !file.exists(file.path(destination, "links-metodos.csv")),
  !file.exists(file.path(destination, "locais-fora-municipio.csv")),
  !file.exists(file.path(destination, "verificacao-espacial.csv")))
pt <- readLines(file.path(destination, "diagnosticos-pt.md"), encoding = "UTF-8")
en <- readLines(file.path(destination, "diagnosticos-en.md"), encoding = "UTF-8")
stopifnot(any(grepl("60,00%", pt, fixed = TRUE)), any(grepl("60.00%", en, fixed = TRUE)),
  !any(grepl("^#", pt)), !any(grepl("^#", en)),
  sum(result$summary$outside$validos) == 30, sum(result$summary$spatial$setores_testados) == 2)
reject(preparar_post(root = root, destination = "other", primary_distance_m = 1000), "Primary distance")
# No generic raw-directory copy: a forbidden destination file also blocks reuse.
writeLines("private", file.path(destination, "links-metodos.csv"))
reject(preparar_post(root = root, destination = "published"), "Individual linkage")
resolved <- normalizePath(root, winslash = "/", mustWork = TRUE)
temp_root <- paste0(normalizePath(tempdir(), winslash = "/", mustWork = TRUE), "/")
stopifnot(startsWith(resolved, temp_root), grepl("post-publication-", basename(resolved), fixed = TRUE))
unlink(resolved, recursive = TRUE)
message("PASS: certificate/status/current hashes, explicit whitelist, private-data exclusion, bilingual numbers and reference/primary-distance gates.")
