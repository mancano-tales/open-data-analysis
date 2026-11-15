# Base-R regression tests: never combine populations or partial extractions.
source("shared-pipeline/sedap/portas-anuais/dados-populacao.R")
local({
  raiz <- tempfile("coortes-fixture-")
  dir.create(raiz)
  on.exit(unlink(raiz, recursive = TRUE))
  publicado <- file.path(raiz, "posts", "portas-renda-coortes-anuais", "data")
  cache <- file.path(raiz, "data-raw", "sedap", "portas-anuais", "inscritos")
  dir.create(publicado, recursive = TRUE)
  dir.create(cache, recursive = TRUE)
  nomes <- c("matriz_2012_porta.csv", "matriz_2012_modalidade.csv", "row_loss.csv")
  falha <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
  file.create(file.path(publicado, nomes))
  write.csv(data.frame(populacao = "presentes"), file.path(publicado, "populacao.csv"), row.names = FALSE)
  falha(resolver_dados_coortes(raiz, "inscritos", 2012))
  stopifnot(resolver_dados_coortes(raiz, "presentes", 2012)$diretorio == publicado)
  file.create(file.path(cache, nomes[1]))
  falha(resolver_dados_coortes(raiz, "inscritos", 2012))
  file.create(file.path(cache, nomes[-1]))
  stopifnot(resolver_dados_coortes(raiz, "inscritos", 2012)$diretorio == cache)
})
# Check the redistributable snapshot without needing credentials or R packages.
snapshot <- resolver_dados_coortes(".", "inscritos")
for (ano in 2012:2021) {
  porta <- read.csv(file.path(snapshot$diretorio, sprintf("matriz_%d_porta.csv", ano)))
  modal <- read.csv(file.path(snapshot$diretorio, sprintf("matriz_%d_modalidade.csv", ano)))
  stopifnot(all(is.finite(porta$N)), all(porta$N >= 0),
            all(is.finite(modal$N)), all(modal$N >= 0),
            sum(porta$N) > 0, sum(modal$N) > 0)
  # Independent noisy queries can differ slightly; flag a material divergence.
  stopifnot(abs(sum(porta$N) / sum(modal$N) - 1) < 0.01)
}
p <- read.csv("posts/portas-renda-coortes-anuais/data/probabilidades_por_decil.csv")
totais <- subset(p, tipo == "via" & cod == "T")
somas <- aggregate(p ~ coorte + decil, subset(p, tipo == "via" & cod != "T"), sum)
comparacao <- merge(totais, somas, by = c("coorte", "decil"))
stopifnot(nrow(comparacao) == 100, all(abs(comparacao$p.x - comparacao$p.y) <= 0.03))
cat("PASS: population isolation, complete snapshots and published cohort invariants.\n")
