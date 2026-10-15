source("shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R")
stopifnot(requireNamespace("jsonlite",quietly=TRUE))
folder<-tempfile("historical-custom-cache-test-");dir.create(folder)
custom<-file.path(folder,"custom cache with spaces")
worker<-file.path(folder,"fake-worker.R")
writeLines(c(
 'args<-commandArgs(trailingOnly=TRUE)',
 'i<-match("--cache-dir",args);stopifnot(!is.na(i),i<length(args))',
 'cache<-args[i+1L];year<-as.integer(args[2L]);uf<-args[3L]',
 'path<-file.path(cache,"historical-status",as.character(year),paste0(uf,".json"))',
 'dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)',
 'jsonlite::write_json(list(version=1L,year=year,uf=uf,status="complete"),path,auto_unbox=TRUE)',
 'writeLines(normalizePath(cache,winslash="/"),file.path(cache,"cache-used.txt"))'
),worker)
historical_script<-worker
result<-historical_prepare(2022L,"AC",cache_dir=custom)
stopifnot(result$status=="complete",file.exists(file.path(custom,"historical-status/2022/AC.json")),
 file.exists(file.path(custom,"prepare-2022-historical-status.csv")),
 identical(readLines(file.path(custom,"cache-used.txt")),normalizePath(custom,winslash="/")))
message("PASS: actual child R process received custom cache_dir, including spaces; checkpoint/log/summary stayed there")
