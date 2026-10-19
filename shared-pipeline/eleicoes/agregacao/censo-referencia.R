# Census income references for the explorer. Source from the repository root.
# One entry per census: everything that differs between 2022 and 2010 lives here,
# so the rest of the pipeline receives a normalized universe and never branches
# on the year. No downloads or side effects on source().
eleicao_censos <- list(
  `2022` = list(
    census_year = 2022L,
    censobr_dataset = "ResponsavelRenda",
    income_variable = "V06004",       # mean nominal income of heads WITH income
    head_count_variable = "V06001",   # household heads
    minimum_wage_brl = 1212,          # July 2022
    tract_geometry_year = 2022L,
    municipality_geometry_year = 2022L,
    elections = c(2010L, 2014L, 2018L, 2022L, 2026L),
    income_definition = list(
      pt = "Rendimento nominal médio mensal dos responsáveis com rendimento por domicílios particulares permanentes ocupados (V06004), Censo 2022; salários mínimos de 2022 (R$ 1.212).",
      en = "Mean nominal monthly income of household heads with income in permanent private households (V06004), Census 2022; 2022 minimum wages (BRL 1,212).")
  ),
  `2010` = list(
    census_year = 2010L,
    censobr_dataset = "Basico",
    # Pilot metadata (data-raw/eleicoes-renda/censo2010/metadata.json), dictionary
    # pp. 43-44: V007 = mean nominal income of heads WITH income; V005 includes
    # heads without income and is NOT equivalent to 2022's V06004.
    income_variable = "V007",
    head_count_variable = "V001",
    minimum_wage_brl = 510,           # July 2010
    tract_geometry_year = 2010L,
    municipality_geometry_year = 2010L,
    elections = c(2010L, 2014L),
    income_definition = list(
      pt = "Rendimento nominal médio mensal das pessoas responsáveis com rendimento por domicílios particulares permanentes (V007), Censo 2010; salários mínimos de 2010 (R$ 510).",
      en = "Mean nominal monthly income of household heads with income in permanent private households (V007), Census 2010; 2010 minimum wages (BRL 510).")
  )
)

eleicao_censo <- function(census_year) {
  key <- as.character(as.integer(census_year))
  config <- eleicao_censos[[key]]
  if (is.null(config)) stop("Unsupported census reference: ", census_year)
  config
}

# Fails when a series would combine an election with a census not enabled for it.
eleicao_censo_allowed <- function(census_year, election_year) {
  as.integer(election_year) %in% eleicao_censo(census_year)$elections
}

# Normalizes any census table to code_tract / heads / income, keeping the same
# eligibility rule as 2022: positive head count and positive finite income.
eleicao_censo_universe <- function(table, census_year) {
  config <- eleicao_censo(census_year)
  x <- data.table::as.data.table(table)
  needed <- c("code_tract", config$head_count_variable, config$income_variable)
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("Census ", config$census_year, " table lacks: ", paste(missing, collapse = ", "))
  x <- x[, ..needed]
  data.table::setnames(x, needed, c("code_tract", "heads", "income"))
  x[, code_tract := if (is.numeric(code_tract)) sprintf("%.0f", code_tract) else as.character(code_tract)]
  x[, heads := suppressWarnings(as.numeric(heads))]
  x[, income := suppressWarnings(as.numeric(income))]
  if (anyNA(x$code_tract) || anyDuplicated(x$code_tract)) stop("Missing/duplicate census tract")
  x[, census_year := config$census_year]
  x[is.finite(heads) & heads > 0 & is.finite(income) & income > 0]
}

# Series identifiers: Census 2022 keeps the existing IDs; other censuses add a
# suffix so no published URL changes.
eleicao_censo_series_id <- function(base_id, census_year) {
  if (as.integer(census_year) == 2022L) base_id else paste0(base_id, "-c", as.integer(census_year))
}
