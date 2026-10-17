source("shared-pipeline/eleicoes/validacao/bootstrap-municipios.R")
x <- data.frame(code_tract = sprintf("%06d", 1:400),
  code_muni = rep(1:4, each = 100), V06004 = 1212,
  p_lula = 40, p_flavio = 50)
set.seed(42)
before <- .Random.seed
a <- bootstrap_municipios(x, replicas = 10L)
stopifnot(all(verificar_bootstrap_municipios(x, a)$status == "pass"))
stopifnot(identical(before, .Random.seed),
  all(a$centis$p025[a$centis$variavel == "lula"] == 40),
  all(a$centis$p975[a$centis$variavel == "flavio"] == 50),
  all(a$cruzamentos$primeiro_centil_flavio_supera_lula == 1L),
  identical(a, bootstrap_municipios(x, replicas = 10L)))
x$p_lula <- ifelse(x$code_muni < 3L, 60, 20)
x$p_flavio <- 100 - x$p_lula
b <- bootstrap_municipios(x, replicas = 40L)
stopifnot(any(b$centis$p975 > b$centis$p025))
stopifnot(all(verificar_bootstrap_municipios(x, b)$status == "pass"))
b$replicas$lula[1L] <- b$replicas$lula[1L] + 1
stopifnot(inherits(try(verificar_bootstrap_municipios(x, b), silent = TRUE), "try-error"))
cat("PASS: municipality bootstrap preserves cluster multiplicity, seed and constant curves.\n")
