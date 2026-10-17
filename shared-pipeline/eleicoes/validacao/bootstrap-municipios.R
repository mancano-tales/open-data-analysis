# Exploratory cluster resampling: this is not a sampling confidence interval
# for a census, and does not measure uncertainty in the spatial linkage.
bootstrap_municipios <- function(tracts, replicas = 500L, seed = 20261016L,
                                 salario = 1212) {
  required <- c("code_tract", "code_muni", "V06004", "p_lula", "p_flavio")
  if (!all(required %in% names(tracts))) stop("Missing bootstrap inputs.")
  x <- as.data.frame(tracts)[, required]
  if (nrow(x) < 100L || anyNA(x) || anyDuplicated(x$code_tract) ||
      any(!is.finite(as.matrix(x[, c("V06004", "p_lula", "p_flavio")]))))
    stop("Invalid bootstrap tracts.")
  if (replicas < 2L || replicas != as.integer(replicas)) stop("Invalid replicate count.")
  x <- x[order(x$V06004, x$code_tract), ]
  municipalities <- unique(x$code_muni)
  group <- match(x$code_muni, municipalities)
  if (length(municipalities) < 2L) stop("At least two municipalities required.")
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (is.null(old_seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  estimates <- array(NA_real_, dim = c(100L, 3L, replicas),
    dimnames = list(NULL, c("lula", "flavio", "renda_sm"), NULL))
  crossings <- rep(NA_integer_, replicas)
  values <- cbind(lula = x$p_lula, flavio = x$p_flavio, renda_sm = x$V06004 / salario)
  for (b in seq_len(replicas)) {
    multiplicity <- tabulate(sample.int(length(municipalities),
      length(municipalities), replace = TRUE), nbins = length(municipalities))
    indices <- rep.int(seq_len(nrow(x)), multiplicity[group])
    if (length(indices) < 100L) stop("Resample has fewer than 100 tracts.")
    bins <- floor((seq_along(indices) - 1) * 100 / length(indices)) + 1L
    counts <- tabulate(bins, nbins = 100L)
    estimates[, , b] <- rowsum(values[indices, , drop = FALSE], bins,
                                reorder = FALSE) / counts
    above <- which(estimates[, "flavio", b] > estimates[, "lula", b])
    if (length(above)) crossings[b] <- above[1L]
  }
  summaries <- do.call(rbind, lapply(dimnames(estimates)[[2L]], function(variable) {
    q <- t(apply(estimates[, variable, ], 1L, quantile,
                 probs = c(.025, .5, .975), names = FALSE))
    data.frame(centil = 1:100, variavel = variable, p025 = q[, 1L],
               mediana = q[, 2L], p975 = q[, 3L], replicas = replicas, seed = seed)
  }))
  list(centis = summaries, cruzamentos = data.frame(replica = seq_len(replicas),
    primeiro_centil_flavio_supera_lula = crossings, seed = seed),
    replicas = data.frame(replica = rep(seq_len(replicas), each = 100L),
      centil = rep(1:100, replicas),
      lula = as.vector(estimates[, "lula", ]),
      flavio = as.vector(estimates[, "flavio", ]),
      renda_sm = as.vector(estimates[, "renda_sm", ])),
    metodo = "Municipalities sampled with replacement; all their tracts repeated; income ranks recomputed in each replicate; exploratory distribution, not census sampling uncertainty.")
}

# Separate base-R reconstruction: expand sampled municipalities, sort the
# replicated records, and aggregate each group instead of weighting rows.
verificar_bootstrap_municipios <- function(tracts, result, salario = 1212) {
  x <- as.data.frame(tracts)
  x <- x[order(x$V06004, x$code_tract), ]
  municipalities <- unique(x$code_muni)
  replicas <- unique(result$centis$replicas)
  seed <- unique(result$centis$seed)
  stopifnot(length(replicas) == 1L, length(seed) == 1L,
    nrow(result$replicas) == 100L * replicas,
    nrow(result$cruzamentos) == replicas)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (is.null(old_seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  index_by_muni <- split(seq_len(nrow(x)), as.character(x$code_muni))
  for (b in seq_len(min(3L, replicas))) {
    sampled <- municipalities[sample.int(length(municipalities),
      length(municipalities), replace = TRUE)]
    indices <- unlist(index_by_muni[as.character(sampled)], use.names = FALSE)
    z <- x[indices, ]
    z <- z[order(z$V06004, z$code_tract), ]
    # Compute group ends independently of production's per-row floor formula.
    ends <- ceiling((1:100) * nrow(z) / 100)
    bins <- rep(1:100, diff(c(0L, ends)))
    observed <- result$replicas[result$replicas$replica == b, ]
    for (variable in c("lula", "flavio", "renda_sm")) {
      value <- switch(variable, lula = z$p_lula, flavio = z$p_flavio,
                       renda_sm = z$V06004 / salario)
      expected <- as.numeric(tapply(value, bins, mean))
      stopifnot(isTRUE(all.equal(observed[[variable]], expected, tolerance = 1e-10)))
    }
  }
  for (variable in c("lula", "flavio", "renda_sm")) {
    z <- result$replicas
    quantiles <- t(vapply(split(z[[variable]], z$centil), quantile,
      numeric(3), probs = c(.025, .5, .975), names = FALSE))
    actual <- result$centis[result$centis$variavel == variable, ]
    stopifnot(isTRUE(all.equal(unname(as.matrix(actual[, c("p025", "mediana", "p975")])),
                              unname(quantiles), tolerance = 1e-10)))
  }
  crossings <- vapply(split(result$replicas, result$replicas$replica), function(z) {
    above <- z$centil[z$flavio > z$lula]
    if (length(above)) min(above) else NA_integer_
  }, integer(1))
  stopifnot(identical(unname(crossings), result$cruzamentos$primeiro_centil_flavio_supera_lula))
  data.frame(check = c("first_three_cluster_resamples", "all_quantiles", "all_crossings"),
    status = "pass", replicas = replicas, seed = seed)
}
