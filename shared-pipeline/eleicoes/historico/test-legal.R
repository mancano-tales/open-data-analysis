# No network: legality decisions must never spread totals across sections.
source('shared-pipeline/eleicoes/historico/legal-validity.R')
stopifnot(identical(legal_all_or_zero(c(10,20,0),c(10,0,0)),c(TRUE,FALSE,FALSE)))
reject <- function(total,valid) {
  stopifnot(tryCatch({legal_all_or_zero(total,valid);FALSE},error=function(e)TRUE))
}
reject(10,5);reject(10,11);reject(10,-1);reject(NA_real_,0)
message('PASS: all-or-zero validity, partial allocation rejected, invalid official counts rejected.')
