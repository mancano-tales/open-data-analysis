# Independent diagnostic entry point. Does not patch any active legal helper.
legacy14_environment <- function(probe_dir = NULL, publication_mode = c('diagnostic','reviewed')) {
  publication_mode <- match.arg(publication_mode)
  env <- new.env(parent = globalenv())
  sys.source('shared-pipeline/eleicoes/agregacao/validacao/legal-2010-2014.R', envir = env)
  env$legal_resource_csv <- function(year, prefix, uf, cache_dir = 'data-raw/eleicoes-renda/historico') {
    if (length(year)!=1L || !year %in% c(2010L,2014L) || length(uf)!=1L || !grepl('^[A-Za-z]{2}$',uf))
      stop('Expected one legacy year and UF')
    if (!prefix %in% c('votacao_candidato_munzona','votacao_partido_munzona','detalhe_votacao_munzona'))
      stop('Unsupported legacy official resource')
    output_dir <- if (is.null(probe_dir)) file.path(cache_dir,'legacy14-isolated') else probe_dir
    env$legacy14_cached_csv(prefix,year,toupper(uf),cache_dir,output_dir)
  }
  env$legal_reconcile_places <- function(year, uf, offices = c(3L,5L,6L,7L,8L),
                                         cache_dir = 'data-raw/eleicoes-renda/historico') {
    output_dir <- if (is.null(probe_dir)) file.path(cache_dir,'legacy14-isolated') else probe_dir
    result <- env$legacy14_reconcile_cached(year,uf,offices,cache_dir,output_dir)
    places <- result$places
    attr(places,'diagnostic_path') <- result$path
    stopifnot(identical(attr(places,'vote_audit')$status,'diagnostic_only'),
              identical(attr(places,'vote_audit')$publishable,FALSE))
    # Prepared for explicit integration AFTER independent review; not invoked
    # by this file or any runner. Default remains strictly diagnostic.
    if (publication_mode == 'reviewed') {
      audit <- attr(places,'vote_audit')
      stopifnot(length(audit$provenance$official_members)==3L,
                grepl('^[a-f0-9]{32}$',audit$provenance$raw_md5),
                all(vapply(audit$provenance$official_members,
                  function(z) grepl('^[a-f0-9]{32}$',z$csv_md5), logical(1))))
      has_accepted <- nrow(audit$checks)>0L
      if (has_accepted) stopifnot(all(audit$checks$reconciled),all(audit$checks$difference==0))
      audit$status <- if (has_accepted) 'reconciled' else 'unavailable'
      audit$publishable <- has_accepted
      audit$method <- '2010/2014 exact candidate nominal and full zone ledger; majoritarian without legend; proportional final zone zero/full observed pool only; generic party legend never anchors validity or conversion; no allocation'
      attr(places,'vote_audit') <- audit
      target <- file.path(cache_dir,paste0('places-',year,'-',toupper(uf),'-',
        paste(sort(unique(as.integer(offices))),collapse='_'),'-legal.rds'))
      attr(places,'canonical_path') <- target
      saveRDS(places,target)
      if (!is.null(result$zone_checks) && nrow(result$zone_checks))
        data.table::fwrite(result$zone_checks,sub('[.]rds$','-audit.csv',target))
      if (nrow(audit$unavailable))
        data.table::fwrite(audit$unavailable,sub('[.]rds$','-unavailable.csv',target))
    }
    places
  }
  env
}
