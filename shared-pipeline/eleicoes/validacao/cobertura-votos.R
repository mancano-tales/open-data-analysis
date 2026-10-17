# Place totals are counted once at each stage, never once per linked tract.
calcular_cobertura_votos <- function(sections, places, coordinate_keys,
                                    eligible_keys, used_keys) {
  sections <- data.table::as.data.table(sections)
  places <- data.table::as.data.table(places)
  if (anyDuplicated(places$place_key)) stop("Duplicate place totals.")
  if (!all(used_keys %in% eligible_keys) ||
      !all(eligible_keys %in% coordinate_keys) ||
      !all(coordinate_keys %in% places$place_key)) stop("Non-nested coverage stages.")
  stages <- list(locais_com_votos = places,
    coordenadas_utilizaveis = places[place_key %in% coordinate_keys],
    locais_elegiveis_municipio = places[place_key %in% eligible_keys],
    locais_usados = places[place_key %in% used_keys])
  totals <- function(x) c(validos = sum(x$validos), lula = sum(x$lula),
                          flavio = sum(x$flavio))
  national <- totals(sections)
  if (national[["validos"]] <= 0) stop("No national valid votes.")
  one_scope <- function(uf) {
    s <- if (uf == "BR") sections else sections[SG_UF == uf]
    reference <- totals(s)
    rows <- list()
    for (stage in c("resultado_nacional", names(stages))) {
      x <- if (stage == "resultado_nacional") s else stages[[stage]]
      if (uf != "BR" && stage != "resultado_nacional") x <- x[SG_UF == uf]
      value <- totals(x)
      n_places <- if (stage == "resultado_nacional") {
        keys <- unique(x[!is.na(NR_LOCAL_VOTACAO) & NR_LOCAL_VOTACAO > 0,
          paste(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO, sep = ":")])
        length(keys)
      } else nrow(x)
      row <- data.table::data.table(SG_UF = uf, etapa = stage, locais = n_places)
      for (vote in names(value)) {
        row[[vote]] <- unname(value[[vote]])
        row[[paste0(vote, "_fracao_br")]] <- if (national[[vote]] > 0)
          value[[vote]] / national[[vote]] else NA_real_
        row[[paste0(vote, "_fracao_territorio")]] <- if (reference[[vote]] > 0)
          value[[vote]] / reference[[vote]] else NA_real_
      }
      for (candidate in c("lula", "flavio")) {
        share <- if (value[["validos"]] > 0) 100 * value[[candidate]] / value[["validos"]] else NA_real_
        row[[paste0(candidate, "_pct_validos")]] <- share
        row[[paste0(candidate, "_diferenca_territorio_pp")]] <- share -
          100 * reference[[candidate]] / reference[["validos"]]
      }
      rows[[length(rows) + 1L]] <- row
    }
    data.table::rbindlist(rows)
  }
  list(br = one_scope("BR"),
       uf = data.table::rbindlist(lapply(sort(unique(sections$SG_UF)), one_scope)))
}
