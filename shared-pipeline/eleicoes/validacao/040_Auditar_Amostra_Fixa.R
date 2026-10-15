# Run from repository root after 020. Does not alter existing estimates.
# Base R only. Outputs distinguish coverage changes from income-rank changes.
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[[1L]] else "data-raw/eleicoes-renda/setores-votos.rds"
output <- if (length(args) > 1L) args[[2L]] else "data-raw/eleicoes-renda/validacao"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
x <- as.data.frame(readRDS(input))
required <- c("code_tract", "V06001", "V06004", "elegivel_renda", "distancia_m",
              "place_key", "p_lula", "p_flavio")
stopifnot(all(required %in% names(x)), !anyDuplicated(x$code_tract))
x <- x[, required, drop = FALSE]
valid_income <- !is.na(x$V06001) & x$V06001 > 0 & is.finite(x$V06004) & x$V06004 > 0
stopifnot(identical(as.logical(x$elegivel_renda), valid_income))
x <- x[valid_income, , drop = FALSE]
x <- x[order(x$V06004, as.character(x$code_tract)), , drop = FALSE]
stopifnot(nrow(x) >= 100L)
x$centil_fixo <- floor((seq_len(nrow(x)) - 1L) * 100 / nrow(x)) + 1L
x$uf <- substr(as.character(x$code_tract), 1L, 2L)
stopifnot(diff(range(tabulate(x$centil_fixo, 100L))) <= 1L)
available <- is.finite(x$distancia_m) & !is.na(x$place_key)
for (candidate in c("p_lula", "p_flavio")) {
  stopifnot(all(is.finite(x[[candidate]][available])),
            all(x[[candidate]][available] >= 0 & x[[candidate]][available] <= 100))
}
stopifnot(all(x$p_lula[available] + x$p_flavio[available] <= 100 + 1e-8))
save_csv <- function(value, name) write.csv(value, file.path(output, name), row.names = FALSE, na = "")
curves <- list(); diagnostics <- list(); coverage_uf <- list()
safe_mean <- function(z) if (length(z)) mean(z) else NA_real_
for (limit in c(1000, 2000, 5000)) {
  keep <- available & x$distancia_m <= limit
  selected <- x[keep, , drop = FALSE]
  stopifnot(nrow(selected) >= 100L)
  selected$centil_amostra <- floor((seq_len(nrow(selected)) - 1L) * 100 / nrow(selected)) + 1L
  place_counts <- table(selected$place_key)
  diagnostics[[as.character(limit)]] <- data.frame(
    distancia_maxima_m = limit, setores_renda_elegivel = nrow(x), setores_incluidos = nrow(selected),
    locais_distintos = length(place_counts), max_setores_mesmo_local = max(place_counts),
    mediana_setores_local = median(as.numeric(place_counts)),
    setores_com_centil_alterado = sum(selected$centil_fixo != selected$centil_amostra),
    maior_alteracao_centil = max(abs(selected$centil_fixo - selected$centil_amostra)),
    # Concentration diagnostic only: not a statistical effective sample size.
    locais_equivalentes_peso_setorial = sum(place_counts)^2 / sum(as.numeric(place_counts)^2))
  coverage_uf[[as.character(limit)]] <- do.call(rbind, lapply(sort(unique(x$uf)), function(state) {
    full <- x$uf == state
    data.frame(distancia_maxima_m = limit, uf = state, setores_elegiveis = sum(full),
               setores_incluidos = sum(keep & full), cobertura = mean(keep[full]))
  }))
  curves[[as.character(limit)]] <- do.call(rbind, lapply(1:100, function(bin) {
    baseline <- x$centil_fixo == bin
    fixed <- selected[selected$centil_fixo == bin, , drop = FALSE]
    reranked <- selected[selected$centil_amostra == bin, , drop = FALSE]
    data.frame(distancia_maxima_m = limit, centil = bin,
      setores_universo = sum(baseline), setores_observados = nrow(fixed),
      cobertura = nrow(fixed) / sum(baseline),
      renda_sm_universo = mean(x$V06004[baseline]) / 1212,
      renda_sm_observada = safe_mean(fixed$V06004) / 1212,
      lula_centil_fixo = safe_mean(fixed$p_lula), flavio_centil_fixo = safe_mean(fixed$p_flavio),
      lula_centil_amostra = mean(reranked$p_lula), flavio_centil_amostra = mean(reranked$p_flavio))
  }))
}
curves <- do.call(rbind, curves)
save_csv(curves, "sensibilidade-ranking-fixo.csv")
save_csv(do.call(rbind, diagnostics), "diagnostico-amostra.csv")
save_csv(do.call(rbind, coverage_uf), "cobertura-uf.csv")
stopifnot(nrow(curves) == 300L,
          all(curves$cobertura >= 0 & curves$cobertura <= 1),
          all(tapply(curves$setores_universo, curves$distancia_maxima_m, sum) == nrow(x)))
# Compare the reconstructed sample-rank curve with the existing independent output.
existing <- file.path(dirname(input), "sensibilidade.csv")
if (file.exists(existing)) {
  previous <- read.csv(existing)
  combined <- merge(curves, previous, by = c("distancia_maxima_m", "centil"))
  stopifnot(nrow(combined) == 300L,
            max(abs(combined$lula_centil_amostra - combined$lula)) < 1e-8,
            max(abs(combined$flavio_centil_amostra - combined$flavio)) < 1e-8)
}
print(do.call(rbind, diagnostics), row.names = FALSE)
message("PASS: fixed 2022 income universe, coverage, rank changes and independent sample-rank reconstruction.")
