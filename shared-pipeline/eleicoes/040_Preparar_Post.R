# Publish aggregate outputs only after the independent 030 certificate passes.
# Source to test functions; the CLI runs only when this file is executed directly.
post_snapshot_files <- function() c(
  "centis.csv", "sensibilidade.csv", "cobertura-setores.csv", "cobertura-votos.csv", "cobertura-votos-uf.csv",
  "metodos-metadata.json", "sensibilidade-cortes-fixos.csv", "centis-ponderados.csv", "centis-razao.csv",
  "conservacao-votos.csv", "sensibilidade-ligacao.csv", "exclusoes-distancia.csv", "cruzamentos-cortes-fixos.csv",
  "diagnostico-renda.csv", "centis-originais-comparacao.csv", "confronto-metodos.csv", "verificacao-metodos.csv",
  "bootstrap-municipios.csv", "bootstrap-replicas.csv", "bootstrap-cruzamentos.csv", "bootstrap-metodo.txt",
  "verificacao-bootstrap.csv", "verificacao-espacial-chaves.csv", "verificacao-espacial-insumos.csv",
  "comparacao-folha.csv", "versoes.csv", "checksums.csv", "verificacao-zip-csv.csv")

post_snapshot_path <- function(root, relative, must_exist = TRUE) {
  relative <- gsub("\\", "/", relative, fixed = TRUE)
  if (length(relative) != 1L || is.na(relative) || !nzchar(relative) ||
      grepl("^(/|[A-Za-z]:)", relative) || any(strsplit(relative, "/", fixed = TRUE)[[1L]] %in% c("..", ".")))
    stop("Snapshot requires a safe repository-relative path")
  path <- file.path(root, relative)
  if (must_exist && (!file.exists(path) || isTRUE(file.info(path)$isdir))) stop("Missing snapshot file: ", relative)
  path
}

post_snapshot_hashes <- function(map, resolver, label) {
  if (!is.list(map) || !length(map) || is.null(names(map)) || anyDuplicated(names(map))) stop("Invalid ", label, " hashes")
  valid <- vapply(map, function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[a-fA-F0-9]{32}$", x), logical(1))
  if (!all(valid)) stop("Malformed ", label, " MD5")
  paths <- vapply(names(map), resolver, character(1))
  current <- unname(tools::md5sum(paths)); expected <- tolower(unlist(map, use.names = FALSE))
  if (anyNA(current) || any(tolower(current) != expected)) stop("Stale ", label, " certificate: ", paste(names(map)[is.na(current) | tolower(current) != expected], collapse = ", "))
  invisible(paths)
}

post_snapshot_gate <- function(root = ".", source = "data-raw/eleicoes-renda") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  directory <- post_snapshot_path(root, source, FALSE)
  marker <- file.path(directory, "validacao-metodologica.json")
  if (!file.exists(marker)) stop("Run 030 successfully before preparing the snapshot")
  cert <- jsonlite::read_json(marker, simplifyVector = FALSE)
  if (!identical(cert$status, "passed") || !identical(as.integer(cert$schema_version), 1L)) stop("Independent validation is not passed")
  required_checks <- c("raw_year_and_vote_shares", "baseline_centiles", "spatial_sample_and_k3",
    "coverage_and_outside_municipality", "fixed_weighted_ratio_idw_exclusions", "income_count_unavailable",
    "bootstrap_500_and_three_independent_resamples")
  if (!all(required_checks %in% unlist(cert$checks))) stop("Missing independent checks in publication certificate")
  copied <- post_snapshot_files()
  private <- c("verificacao-espacial.csv", "locais-fora-municipio.csv")
  if (!all(c(copied, private) %in% names(cert$outputmd5))) stop("Publication output absent from independent certificate")
  post_snapshot_hashes(cert$inputmd5, function(x) post_snapshot_path(root, x), "input")
  post_snapshot_hashes(cert$outputmd5, function(x) post_snapshot_path(directory, x), "output")
  reference <- "posts/votacao-renda-centil/data/referencia-folha.csv"
  if (!reference %in% names(cert$inputmd5)) stop("Visual reference missing from independently hashed inputs")
  list(root = root, source = directory, certificate = cert, marker = marker, reference = reference,
       marker_md5 = unname(tools::md5sum(marker)), files = copied, private = private)
}

post_snapshot_read <- function(directory, file, required = character()) {
  x <- utils::read.csv(file.path(directory, file), stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(required %in% names(x))) stop("Unexpected snapshot schema: ", file)
  x
}

post_snapshot_summary <- function(stage, primary_distance_m = 5000) {
  read <- function(file, required = character()) post_snapshot_read(stage, file, required)
  coverage <- read("cobertura-votos.csv", c("etapa", "validos", "locais", "validos_fracao_br", "lula_pct_validos", "flavio_pct_validos"))
  national <- coverage[coverage$etapa == "resultado_nacional", , drop = FALSE]
  used <- coverage[coverage$etapa == "locais_usados", , drop = FALSE]
  if (nrow(national) != 1L || nrow(used) != 1L || national$validos <= 0) stop("Invalid aggregate national coverage")
  baseline <- read("centis.csv", c("centil", "candidato", "votacao", "renda_sm", "n_setores"))
  reranked <- read("centis-originais-comparacao.csv", c("centil", "distancia_maxima_m", "lula", "flavio", "renda_sm", "n_setores"))
  primary <- reranked[reranked$distancia_maxima_m == primary_distance_m, , drop = FALSE]
  if (nrow(primary) != 100L) stop("Primary distance missing from validated scenarios")
  for (name in c("lula", "flavio")) {
    z <- baseline[if (name == "lula") grepl("^Lula", baseline$candidato) else grepl("Bolsonaro", baseline$candidato), ]
    ix <- match(primary$centil, z$centil)
    if (nrow(z) != 100L || anyNA(ix) || any(abs(z$votacao[ix] - primary[[name]]) > 1e-8) ||
        any(abs(z$renda_sm[ix] - primary$renda_sm) > 1e-8) || any(z$n_setores[ix] != primary$n_setores))
      stop("Primary distance does not reproduce the published baseline")
  }
  excluded <- read("exclusoes-distancia.csv", c("decil", "situacao", "motivo", "n_setores", "distancia_maxima_m"))
  excluded <- excluded[excluded$distancia_maxima_m == primary_distance_m, , drop = FALSE]
  summarize_exclusion <- function(field) {
    parts <- split(excluded, excluded[[field]])
    ans <- do.call(rbind, lapply(names(parts), function(key) {
      z <- parts[[key]]; total <- sum(z$n_setores)
      data.frame(grupo = key, elegiveis = total,
        alem_distancia = sum(z$n_setores[z$motivo == "alem_distancia_maxima"]),
        sem_local = sum(z$n_setores[z$motivo == "sem_local_ou_malha"]), stringsAsFactors = FALSE)
    }))
    if (field == "decil") ans <- ans[order(as.numeric(ans$grupo)), , drop = FALSE]
    ans
  }
  spatial <- read("__private-verificacao-espacial.csv", c("uf", "pass", "knn3_pass", "saved_geodesic_error_m", "projected_diagnostic_pass", "seed"))
  spatial_summary <- do.call(rbind, lapply(split(spatial, spatial$uf), function(z) data.frame(
    uf = z$uf[[1L]], setores_testados = nrow(z), aprovados = sum(z$pass), k3_aprovados = sum(z$knn3_pass),
    erro_geodesico_max_m = max(z$saved_geodesic_error_m), divergencias_projecao_diagnostica = sum(!z$projected_diagnostic_pass),
    seed = z$seed[[1L]])))
  outside <- read("__private-locais-fora-municipio.csv", c("place_key", "validos", "lula", "flavio"))
  outside$uf <- sub(":.*", "", outside$place_key)
  outside_summary <- if (nrow(outside)) do.call(rbind, lapply(split(outside, outside$uf), function(z)
    data.frame(uf = z$uf[[1L]], locais = nrow(z), validos = sum(z$validos), lula = sum(z$lula), flavio = sum(z$flavio)))) else
      data.frame(uf = character(), locais = integer(), validos = numeric(), lula = numeric(), flavio = numeric())
  crossings <- read("cruzamentos-cortes-fixos.csv", c("distancia_maxima_m", "regra", "primeiro_centil", "primeiro_centil_sustentado"))
  comparisons <- read("confronto-metodos.csv", c("distancia_maxima_m", "metodo_referencia", "metodo_comparado", "candidato", "diferenca_maxima_pp", "n_centis_comparados"))
  comparisons <- comparisons[comparisons$distancia_maxima_m == primary_distance_m &
    (comparisons$metodo_referencia == "original_reranked" |
     (comparisons$metodo_referencia == "nearest_fixed" & comparisons$metodo_comparado == "idw3")), , drop = FALSE]
  bootstrap <- read("bootstrap-cruzamentos.csv", c("replica", "primeiro_centil_flavio_supera_lula", "seed"))
  replicas <- read("bootstrap-replicas.csv", c("replica", "centil", "lula", "flavio", "renda_sm"))
  if (nrow(bootstrap) != 500L || nrow(replicas) != 50000L || length(unique(bootstrap$replica)) != 500L)
    stop("Snapshot requires the independently validated 500-replicate bootstrap")
  observed_crossings <- bootstrap$primeiro_centil_flavio_supera_lula
  finite <- is.finite(observed_crossings)
  bootstrap_q <- if (any(finite)) stats::quantile(observed_crossings[finite], c(.025, .5, .975), names = FALSE) else rep(NA_real_, 3L)
  reference <- read("comparacao-folha.csv", c("candidato", "erro_absoluto_pp", "incerteza_visual_pp"))
  reference_summary <- do.call(rbind, lapply(split(reference, reference$candidato), function(z) {
    valid <- is.finite(z$erro_absoluto_pp)
    data.frame(candidato = z$candidato[[1L]], pontos = sum(valid),
      mae_pp = if (any(valid)) mean(z$erro_absoluto_pp[valid]) else NA_real_)
  }))
  list(national = national, used = used, deciles = summarize_exclusion("decil"), situations = summarize_exclusion("situacao"),
    spatial = spatial_summary, outside = outside_summary, crossings = crossings, comparisons = comparisons,
    bootstrap_quantiles = bootstrap_q, bootstrap_observed = sum(finite), reference = reference_summary,
    primary_distance_m = primary_distance_m)
}

post_snapshot_fragment <- function(summary, language = c("pt", "en")) {
  language <- match.arg(language); pt <- language == "pt"
  text <- function(a, b) if (pt) a else b
  number <- function(x, digits = 0L) ifelse(is.finite(x), formatC(x, digits = digits, format = "f",
    big.mark = if (pt) "." else ",", decimal.mark = if (pt) "," else "."), text("não observado", "not observed"))
  table <- function(headers, rows) c(paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |"),
    apply(rows, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |")), "")
  n <- summary$national; u <- summary$used
  result <- c(sprintf(text("Os locais usados representam %s de %s votos do denominador nacional (%s%%), em %s locais. Nesse conjunto, Lula tem %s%% e Flávio Bolsonaro %s%%, diferenças de %s e %s p.p. em relação aos percentuais nacionais de %s%% e %s%%.",
    "Used polling places represent %s of %s votes in the national denominator (%s%%), across %s places. Within that subset, Lula has %s%% and Flávio Bolsonaro %s%%, differences of %s and %s percentage points from national shares of %s%% and %s%%."),
    number(u$validos), number(n$validos), number(100 * u$validos_fracao_br, 2), number(u$locais),
    number(u$lula_pct_validos, 2), number(u$flavio_pct_validos, 2),
    number(u$lula_pct_validos - n$lula_pct_validos, 2), number(u$flavio_pct_validos - n$flavio_pct_validos, 2),
    number(n$lula_pct_validos, 2), number(n$flavio_pct_validos, 2)), "",
    text("A figura principal mantém a média simples por setor e os centis recalculados na amostra incluída. As tabelas abaixo preservam os cortes definidos antes do filtro de distância; são exercícios distintos.",
      "The main figure retains the simple mean across tracts and percentile ranks recomputed in the included sample. The tables below preserve ranks defined before distance exclusions; these are distinct exercises."), "",
    sprintf(text("**Exclusões a %s km.** Decis nacionais de todos os setores com renda elegível, incluindo os sem ligação geográfica:",
      "**Exclusions at %s km.** National income deciles of all income-eligible tracts, including those without a geographic link:"), number(summary$primary_distance_m / 1000, 1)), "")
  for (kind in c("deciles", "situations")) {
    z <- summary[[kind]]
    group <- z$grupo
    if (!pt && kind == "situations") group <- ifelse(group == "Urbana", "Urban", ifelse(group == "Rural", "Rural", "Unavailable"))
    result <- c(result, table(c(if (kind == "deciles") text("Decil", "Decile") else text("Situação", "Situation"),
      text("Elegíveis", "Eligible"), text("Além do limite", "Beyond limit"), text("Fração", "Share"), text("Sem local/malha", "No place/boundary")),
      cbind(group, number(z$elegiveis), number(z$alem_distancia), paste0(number(100 * z$alem_distancia / z$elegiveis, 2), "%"), number(z$sem_local))))
  }
  z <- summary$crossings[summary$crossings$regra == "nearest", , drop = FALSE]
  z <- z[order(z$distancia_maxima_m), ]
  limits <- ifelse(is.infinite(z$distancia_maxima_m), text("Sem limite", "Unlimited"), paste0(number(z$distancia_maxima_m / 1000, 0), " km"))
  result <- c(result, text("**Cruzamentos com cortes fixos.** A primeira superação e a superação sustentada até o fim da distribuição podem diferir:",
    "**Crossings under fixed ranks.** The first crossing may differ from a crossing sustained through the end of the distribution:"), "",
    table(c(text("Limite", "Limit"), text("Primeiro centil", "First percentile"), text("Primeiro sustentado", "First sustained percentile")),
      cbind(limits, number(z$primeiro_centil), number(z$primeiro_centil_sustentado))))
  z <- summary$comparisons
  labels <- c(original_reranked = text("Original reranqueada", "Original reranked"), nearest_fixed = text("Cortes fixos", "Fixed ranks"),
    weighted = text("Ponderada por V06001", "V06001 weighted"), ratio = text("Razão das somas", "Ratio of sums"), idw3 = "IDW k3")
  result <- c(result, text("**Diferenças máximas entre métodos**, no limite principal, comparando o mesmo índice de centil de cada definição:",
    "**Maximum method differences**, at the main distance limit, comparing the same percentile index under each definition:"), "",
    table(c(text("Referência", "Reference"), text("Comparação", "Comparison"), text("Candidato", "Candidate"), text("Máximo (p.p.)", "Maximum (pp)"), text("Centis comparados", "Compared percentiles")),
      cbind(unname(labels[z$metodo_referencia]), unname(labels[z$metodo_comparado]), ifelse(z$candidato == "lula", "Lula", "Flávio Bolsonaro"),
        number(z$diferenca_maxima_pp, 2), number(z$n_centis_comparados))),
    text("V06001 pondera pessoas responsáveis pelos domicílios, não eleitores. A razão das somas conserva os votos dos locais usados e os aloca entre setores proporcionalmente a V06001; essa aproximação não revela onde cada eleitor reside. O IDW k3 também mede proximidade, não a inscrição eleitoral real.",
      "V06001 weights household heads, not voters. The ratio-of-sums method preserves the votes of used polling places and allocates them across linked tracts in proportion to V06001; this approximation does not identify voters’ residences. IDW k3 also measures proximity rather than actual electoral registration."), "")
  b <- summary$bootstrap_quantiles
  result <- c(result, sprintf(text("No bootstrap exploratório de 500 reamostragens de municípios, %s réplicas tiveram cruzamento observado. Os quantis 2,5%%, 50%% e 97,5%% do primeiro centil de superação foram %s, %s e %s. As bandas por centil são pontuais, não simultâneas, e não medem erro amostral do Censo nem incerteza da ligação espacial.",
    "In the exploratory bootstrap of 500 municipality resamples, %s replicates had an observed crossing. The 2.5%%, 50%% and 97.5%% quantiles of the first crossing percentile were %s, %s and %s. Percentile bands are pointwise, not simultaneous; they measure neither census sampling error nor uncertainty in the spatial linkage."),
    number(summary$bootstrap_observed), number(b[1], 1), number(b[2], 1), number(b[3], 1)), "",
    sprintf(text("A auditoria geométrica independente aprovou %s de %s setores amostrados e %s verificações k3. A discordância da projeção EPSG:5880 permaneceu como diagnóstico em %s casos, sem substituir a referência geodésica.",
      "The independent geometric audit passed %s of %s sampled tracts and %s k3 checks. EPSG:5880 projection disagreement remained a diagnostic in %s cases and did not replace the geodesic reference."),
      number(sum(summary$spatial$aprovados)), number(sum(summary$spatial$setores_testados)), number(sum(summary$spatial$k3_aprovados)),
      number(sum(summary$spatial$divergencias_projecao_diagnostica))), "",
    sprintf(text("Foram excluídos %s locais com coordenadas fora do respectivo município na malha de 2022, contendo %s votos do denominador nacional (%s%%). Renda e malha de 2022 não captam todas as mudanças até a eleição de 2026.",
      "%s polling places with coordinates outside their municipality in the 2022 boundary layer were excluded, containing %s votes in the national denominator (%s%%). Income and boundaries from 2022 do not capture every change through the 2026 election."),
      number(sum(summary$outside$locais)), number(sum(summary$outside$validos)), number(100 * sum(summary$outside$validos) / n$validos, 2)), "",
    text("V06004 considera responsáveis com rendimento. O arquivo ResponsavelRenda não fornece a contagem desse grupo; portanto não calculamos a fração de responsáveis sem rendimento. V06002 conta moradores e não permite recuperar essa fração.",
      "V06004 covers household heads with income. ResponsavelRenda does not provide the count of this group, so the share of heads without income is unavailable. V06002 counts residents and cannot recover that share."), "",
    text("**Referência visual aproximada da Folha.** O erro absoluto médio dos pontos legíveis é:",
      "**Approximate visual Folha reference.** Mean absolute error over readable points is:"), "",
    table(c(text("Candidato", "Candidate"), text("Pontos legíveis", "Readable points"), text("MAE (p.p.)", "MAE (pp)")),
      cbind(summary$reference$candidato, number(summary$reference$pontos), number(summary$reference$mae_pp, 2))),
    text("Os pontos vêm de leitura visual manual e do tooltip da imagem, com arredondamento e regiões encobertas. Esse MAE não valida a curva completa nem demonstra equivalência ao método da Folha.",
      "The reference points come from manual visual readings and the image tooltip, including rounding and obscured regions. This MAE neither validates the complete curve nor establishes equivalence to Folha’s method."), "")
  while (length(result) && identical(tail(result, 1L), "")) result <- head(result, -1L)
  result
}

preparar_post <- function(root = ".", source = "data-raw/eleicoes-renda",
                          destination = "posts/votacao-renda-centil/data", primary_distance_m = 5000,
                          maximum_public_bytes = 20 * 1024^2) {
  gate <- post_snapshot_gate(root, source)
  target <- post_snapshot_path(gate$root, destination, FALSE)
  forbidden <- c("links-metodos.csv", "locais-metodos.csv", "universo-centis.csv", "verificacao-espacial.csv", "locais-fora-municipio.csv", "setores-votos.rds")
  if (any(file.exists(file.path(target, forbidden)))) stop("Individual linkage/location data found in public snapshot directory")
  stage <- tempfile("post-snapshot-");dir.create(stage)
  on.exit({ for (f in list.files(stage, full.names = TRUE)) unlink(f); unlink(stage) }, add = TRUE)
  cert <- gate$certificate
  for (name in c(gate$files, gate$private)) {
    file <- file.path(gate$source, name)
    if (file.info(file)$size > maximum_public_bytes && name %in% gate$files) stop("Aggregate publication file exceeds size bound: ", name)
    staged_name <- if (name %in% gate$private) paste0("__private-", name) else name
    staged <- file.path(stage, staged_name)
    if (!file.copy(file, staged, overwrite = TRUE) || unname(tools::md5sum(staged)) != cert$outputmd5[[name]]) stop("Snapshot copy changed: ", name)
  }
  # Reference is an independently hashed input, not a result produced by 020.
  reference_source <- post_snapshot_path(gate$root, gate$reference)
  if (!file.copy(reference_source, file.path(stage, "referencia-folha.csv"), overwrite = TRUE) ||
      unname(tools::md5sum(file.path(stage, "referencia-folha.csv"))) != cert$inputmd5[[gate$reference]]) stop("Visual reference changed during copy")
  summary <- post_snapshot_summary(stage, primary_distance_m)
  utils::write.csv(summary$spatial, file.path(stage, "verificacao-espacial-resumo.csv"), row.names = FALSE)
  utils::write.csv(summary$outside, file.path(stage, "locais-fora-municipio-resumo.csv"), row.names = FALSE)
  writeLines(enc2utf8(post_snapshot_fragment(summary, "pt")), file.path(stage, "diagnosticos-pt.md"), useBytes = TRUE)
  writeLines(enc2utf8(post_snapshot_fragment(summary, "en")), file.path(stage, "diagnosticos-en.md"), useBytes = TRUE)
  current_marker_md5 <- unname(tools::md5sum(gate$marker))
  if (is.na(current_marker_md5) || current_marker_md5 != gate$marker_md5) stop("Validation certificate changed during preparation")
  if (!file.copy(gate$marker, file.path(stage, "validacao-metodologica.json"), overwrite = TRUE)) stop("Cannot stage validation certificate")
  published <- c(gate$files, "referencia-folha.csv", "verificacao-espacial-resumo.csv", "locais-fora-municipio-resumo.csv", "diagnosticos-pt.md", "diagnosticos-en.md")
  manifest_files <- c(published, "validacao-metodologica.json")
  public_manifest <- list(schema_version = 1L, validated_at_utc = cert$validated_at_utc,
    source_certificate_md5 = gate$marker_md5, primary_distance_m = primary_distance_m,
    derived_by_publisher = c("verificacao-espacial-resumo.csv", "locais-fora-municipio-resumo.csv", "diagnosticos-pt.md", "diagnosticos-en.md"),
    filemd5 = as.list(setNames(unname(tools::md5sum(file.path(stage, manifest_files))), manifest_files)))
  jsonlite::write_json(public_manifest, file.path(stage, "publicacao-snapshot.json"), auto_unbox = TRUE, pretty = TRUE)
  published <- c(published, "publicacao-snapshot.json")
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  old_marker <- file.path(target, "validacao-metodologica.json")
  if (file.exists(old_marker) && !file.remove(old_marker)) stop("Cannot invalidate previous public certificate")
  for (name in published) if (!file.copy(file.path(stage, name), file.path(target, name), overwrite = TRUE)) stop("Cannot publish aggregate: ", name)
  if (!identical(unname(tools::md5sum(file.path(stage, published))), unname(tools::md5sum(file.path(target, published)))))
    stop("Published aggregate hashes differ from the staged snapshot")
  if (!file.copy(file.path(stage, "validacao-metodologica.json"), old_marker, overwrite = TRUE)) stop("Cannot publish validation certificate last")
  message("Prepared independently validated aggregate snapshot and bilingual fragments: ", destination)
  invisible(list(files = c(published, "validacao-metodologica.json"), summary = summary))
}

if (sys.nframe() == 0L) {
  if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
  preparar_post()
}
