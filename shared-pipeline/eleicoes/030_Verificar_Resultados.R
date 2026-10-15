# Validate the numerical product against the saved tract-level estimates.
suppressPackageStartupMessages({ library(data.table); library(here) })
setDTthreads(2L)
dest <- here("data-raw", "eleicoes-renda")
tracts <- as.data.table(readRDS(file.path(dest, "setores-votos.rds")))
centis <- fread(file.path(dest, "centis.csv"), encoding = "UTF-8")
x <- tracts[motivo == "incluido"]
stopifnot(nrow(x) >= 100, !anyDuplicated(x$code_tract),
          all(is.finite(x$p_lula)), all(is.finite(x$p_flavio)),
          all(x$p_lula >= 0 & x$p_lula <= 100),
          all(x$p_flavio >= 0 & x$p_flavio <= 100),
          all(x$p_lula + x$p_flavio <= 100 + 1e-8),
          nrow(centis) == 200L, !anyDuplicated(centis[, .(centil, candidato)]))
# Recompute place shares directly from the raw result rows with base R tapply,
# independently of the section-register join and aggregation in script 020.
raw <- fread(file.path(dest, "votos2026", "votacao_secao_2026_BR.csv"),
  select = c("NR_TURNO", "CD_CARGO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA",
             "NR_LOCAL_VOTACAO", "NR_VOTAVEL", "QT_VOTOS"))
raw <- raw[NR_TURNO == 1L & CD_CARGO == 1L & SG_UF != "ZZ" &
             NR_VOTAVEL > 0L & NR_VOTAVEL < 95L]
place <- with(raw, paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = ":"))
denominator <- tapply(as.numeric(raw$QT_VOTOS), place, sum)
for (number in c(13L, 22L)) {
  keep <- raw$NR_VOTAVEL == number
  numerator <- tapply(as.numeric(raw$QT_VOTOS[keep]), place[keep], sum)
  num <- as.numeric(numerator[match(x$place_key, names(numerator))])
  num[is.na(num)] <- 0
  expected <- 100 * num / as.numeric(denominator[match(x$place_key, names(denominator))])
  actual <- if (number == 13L) x$p_lula else x$p_flavio
  stopifnot(all(is.finite(expected)), isTRUE(all.equal(actual, expected, tolerance = 1e-10)))
}
rm(raw, place, denominator, numerator, num, expected, actual)
# Independently reproduce equal-count bins with base R and their means with tapply.
ord <- order(x$V06004, x$code_tract)
x <- x[ord]
bins <- floor((seq_len(nrow(x)) - 1) * 100 / nrow(x)) + 1L
counts <- table(bins)
stopifnot(max(counts) - min(counts) <= 1L)
for (candidate in c("Lula (PT)", "Fl\u00e1vio Bolsonaro (PL)")) {
  series <- if (candidate == "Lula (PT)") x$p_lula else x$p_flavio
  actual <- centis[candidato == candidate][order(centil)]
  expected <- as.numeric(tapply(series, bins, mean))
  stopifnot(identical(actual$centil, 1:100),
            isTRUE(all.equal(actual$votacao, expected, tolerance = 1e-10)),
            sum(actual$n_setores) == nrow(x),
            isTRUE(all.equal(actual$renda_sm,
                             as.numeric(tapply(x$V06004 / 1212, bins, mean)), tolerance = 1e-10)))
}
coverage <- tracts[, .(setores = .N), by = motivo]
stopifnot(sum(coverage$setores) == nrow(tracts))
print(coverage)
print(centis[centil %in% c(1L, 46L, 100L)])
packages <- c("censobr", "geobr", "sf", "data.table", "ggplot2", "here", "httr2")
fwrite(data.table(component = c("R", packages),
                  version = c(as.character(getRversion()),
                    vapply(packages, function(p) as.character(packageVersion(p)), character(1)))),
       file.path(dest, "versoes.csv"))
inputs <- c("renda2022.rds", "polling2026.rds", "municipios2022.rds", "votos2026.zip", "locais2026.zip")
fwrite(data.table(file = inputs, md5 = unname(tools::md5sum(file.path(dest, inputs)))),
       file.path(dest, "checksums.csv"))
message("PASS: 100 centiles, equal tract counts, means, income units and vote denominators.")
