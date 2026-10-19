# Run from the repository root.
source("shared-pipeline/eleicoes/agregacao/censo-referencia.R")
stopifnot(eleicao_censo(2022)$income_variable == "V06004",
          eleicao_censo(2010)$income_variable == "V007",
          eleicao_censo(2010)$minimum_wage_brl == 510,
          eleicao_censo_allowed(2010, 2014), !eleicao_censo_allowed(2010, 2022),
          eleicao_censo_allowed(2022, 2010),
          eleicao_censo_series_id("2014-1-3-SP-state", 2022) == "2014-1-3-SP-state",
          eleicao_censo_series_id("2014-1-3-SP-state", 2010) == "2014-1-3-SP-state-c2010")
stopifnot(inherits(try(eleicao_censo(2000), silent = TRUE), "try-error"))
x <- data.frame(code_tract = c(1, 2, 3, 4), V001 = c(10, 0, 5, 7), V007 = c(1000, 900, NA, 0))
u <- eleicao_censo_universe(x, 2010)
stopifnot(nrow(u) == 1L, u$code_tract == "1", u$census_year == 2010L,
          identical(names(u), c("code_tract", "heads", "income", "census_year")))
stopifnot(inherits(try(eleicao_censo_universe(x[, c("code_tract", "V001")], 2010), silent = TRUE), "try-error"))
cat("test-censo-referencia OK\n")
