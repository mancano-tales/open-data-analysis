source("shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R")
stopifnot(nrow(historical_plan())==27L,identical(unique(historical_plan(c(2010L,2022L,2014L,2018L))$year),historical_years))
stopifnot(inherits(try(historical_plan(2026L),silent=TRUE),"try-error"))
stopifnot(inherits(try(historical_plan(2022L,"ZZ"),silent=TRUE),"try-error"))
x<-data.frame(year=2022L,round=1L,election_id=546L,office=c(3L,5L,6L,7L),uf="AC")
c<-x;c$official_valid_votes<-c(426600,411949,434253,435770);c$computed_valid_votes<-c$official_valid_votes;c$difference<-0
stopifnot(historical_contest_status(x,c,x[FALSE,])=="complete")
stopifnot(historical_contest_status(x,c[1:3,],x[4,])=="partial")
stopifnot(inherits(try(historical_contest_status(x,c[1:3,],x[FALSE,]),silent=TRUE),"try-error"))
c$difference[1]<-1
stopifnot(inherits(try(historical_contest_status(x,c,x[FALSE,]),silent=TRUE),"try-error"))
cat("PASS historical plan, year order, scope, complete/partial/undeclared-gap and audit-divergence tests\n")
