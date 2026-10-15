source("shared-pipeline/eleicoes/historico/ro2026-ea20.R")
suppressPackageStartupMessages(library(data.table))
stopifnot(identical(ea20_destination(c("V\u00e1lido","V\u00e1lido (legenda)","Anulado","Anulado sub judice")),
 c("V\u00e1lido","V\u00e1lido (legenda)","Anulado","Anulado sub judice")))
stopifnot(inherits(try(ea20_destination("APTO"),silent=TRUE),"try-error"))
stopifnot(inherits(try(ea20_destination(""),silent=TRUE),"try-error"))
x<-data.table(raw_votes=c(10,2),official_votes=c(10,NA_real_))
stopifnot(isTRUE(ea20_assert_candidate_totals(x)))
x[1,official_votes:=9]
stopifnot(inherits(try(ea20_assert_candidate_totals(x),silent=TRUE),"try-error"))
x<-data.table(raw_votes=NA_real_,official_votes=1)
stopifnot(inherits(try(ea20_assert_candidate_totals(x),silent=TRUE),"try-error"))
z<-data.table(difference_valid=0,difference_nominal=0,difference_legend=0,difference_technical=0)
stopifnot(isTRUE(ea20_assert_zone_checks(z)))
z[,difference_technical:=1]
stopifnot(inherits(try(ea20_assert_zone_checks(z),silent=TRUE),"try-error"))
z[,difference_technical:=NA_real_]
stopifnot(inherits(try(ea20_assert_zone_checks(z),silent=TRUE),"try-error"))
cat("PASS EA20 destination, positive-candidate completeness, exact-vote and technical-null rejection tests\n")
