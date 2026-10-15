# Delete only reproducible archive-member CSVs after a COMPLETE historical checkpoint.
# Default is dry run. Use YEAR [UF,UF,...] --apply to remove validated files.
if(!exists("historical_checkpoint",mode="function"))
 source("shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R")

historical_verify_zip_member <- function(zip,member,expected_crc,expected_size,cache_dir) {
 scratch_root<-file.path(cache_dir,"cleanup-verification")
 dir.create(scratch_root,recursive=TRUE,showWarnings=FALSE)
 scratch<-tempfile("verify-member-",tmpdir=scratch_root)
 dir.create(scratch)
 root_abs<-normalizePath(cache_dir,winslash="/",mustWork=TRUE)
 scratch_abs<-normalizePath(scratch,winslash="/",mustWork=TRUE)
 if(!startsWith(tolower(scratch_abs),paste0(tolower(root_abs),"/cleanup-verification/")))stop("Unsafe verification directory")
 target<-file.path(scratch,member)
 on.exit({
   if(file.exists(target)) {
     absolute<-normalizePath(target,winslash="/",mustWork=TRUE)
     if(startsWith(tolower(absolute),paste0(tolower(scratch_abs),"/")))file.remove(target)
   }
   # Only empty directories belonging to this uniquely named verification run.
   dirs<-list.dirs(scratch,recursive=TRUE,full.names=TRUE)
   for(d in dirs[order(nchar(dirs),decreasing=TRUE)]) {
     absolute<-normalizePath(d,winslash="/",mustWork=TRUE)
     if((identical(absolute,scratch_abs)||startsWith(tolower(absolute),paste0(tolower(scratch_abs),"/")))&&
        !length(list.files(d,all.files=TRUE,no..=TRUE)))unlink(d,recursive=FALSE)
   }
 },add=TRUE)
 tryCatch(zip::unzip(zip,files=member,exdir=scratch,overwrite=FALSE),
   error=function(e)stop("ZIP decompression verification failed: ",conditionMessage(e)),
   warning=function(w)stop("ZIP decompression verification failed: ",conditionMessage(w)))
 if(!file.exists(target)||file.info(target)$size!=expected_size||
    tolower(digest::digest(file=target,algo="crc32",serialize=FALSE))!=expected_crc)
   stop("ZIP decompression verification failed: extracted member length/CRC differs")
 invisible(TRUE)
}

historical_cleanup_state <- function(year,uf,cache_dir="data-raw/eleicoes-renda/historico",apply=FALSE) {
 for(pkg in c("jsonlite","zip","digest"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg," before cleanup")
 historical_plan(year,uf)
 record<-historical_checkpoint(cache_dir,year,uf)
 if(is.null(record)||!identical(record$status,"complete"))return(data.frame(year=year,uf=uf,status="not_complete",target="",bytes=0))
 root<-normalizePath(cache_dir,winslash="/",mustWork=TRUE)
 within<-function(p)startsWith(tolower(p),paste0(tolower(root),"/"))
 prefixes<-c("votacao_secao","votacao_candidato_munzona","votacao_partido_munzona","detalhe_votacao_munzona")
 results<-list()
 for(prefix in prefixes) {
   zip_name<-if(prefix=="votacao_secao")paste0(prefix,"_",year,"_",uf,".zip") else paste0(prefix,"_",year,".zip")
   zip<-file.path(cache_dir,zip_name)
   if(!file.exists(zip))stop("Missing ZIP; refusing extraction cleanup: ",zip)
   resolved_zip<-normalizePath(zip,winslash="/",mustWork=TRUE)
   if(!within(resolved_zip)||tolower(dirname(resolved_zip))!=tolower(root)||basename(resolved_zip)!=zip_name)stop("ZIP escaped intended historical directory")
   known<-Filter(function(x)identical(x$path,zip),record$fingerprints)
   if(length(known)!=1L||!identical(known[[1L]]$md5,historical_identity(zip)[[1L]]$md5))stop("ZIP differs from complete checkpoint: ",zip)
   extracted<-sub("[.]zip$","",zip)
   if(!dir.exists(extracted))next
   resolved_dir<-normalizePath(extracted,winslash="/",mustWork=TRUE)
   if(!within(resolved_dir)||tolower(dirname(resolved_dir))!=tolower(root)||basename(resolved_dir)!=sub("[.]zip$","",zip_name))stop("Extraction directory does not exactly correspond to ZIP")
   entries<-unzip(zip,list=TRUE)
   expected<-paste0(prefix,"_",year,"_",uf,".csv")
   normalized_members<-gsub("\\\\","/",entries$Name)
   match<-which(basename(normalized_members)==expected)
   if(length(match)!=1L)stop("Expected exactly one UF CSV member in verified archive: ",zip)
   member<-normalized_members[match]
   if(grepl("(^/|^[A-Za-z]:|(^|/)[.][.](/|$))",member))stop("Unsafe ZIP member path")
   target<-file.path(extracted,member)
   if(!file.exists(target))next
   resolved_target<-normalizePath(target,winslash="/",mustWork=TRUE)
   if(!within(resolved_target)||!startsWith(tolower(resolved_target),paste0(tolower(resolved_dir),"/"))||basename(resolved_target)!=expected)stop("CSV target escaped its verified extraction directory")
   bytes<-unname(file.info(target)$size)
   if(is.na(bytes)||bytes!=entries$Length[match])stop("Extracted file length differs from archive member; preserving it")
   central<-zip::zip_list(zip)
   crc_row<-which(gsub("\\\\","/",central$filename)==member)
   if(length(crc_row)!=1L)stop("Ambiguous CRC archive member")
   expected_crc<-tolower(gsub(" ","0",sprintf("%8s",as.character(central$crc32[crc_row]))))
   actual_crc<-tolower(digest::digest(file=target,algo="crc32",serialize=FALSE))
   if(!identical(expected_crc,actual_crc))stop("Extracted file CRC differs from archive member; preserving it")
   # A readable central directory and matching old CSV do not prove that the
   # compressed payload is intact. Extract and CRC-check a fresh temporary copy.
   historical_verify_zip_member(zip,member,expected_crc,bytes,cache_dir)
   # Every absolute path and archive/checkpoint identity was checked above.
   # Remove ONE explicitly listed regular file. Never recurse or remove folders.
   if(dir.exists(target))stop("Expected regular CSV file, got directory")
   if(apply&&!file.remove(target))stop("Cannot remove validated extracted CSV: ",resolved_target)
   results[[length(results)+1L]]<-data.frame(year=year,uf=uf,status=if(apply)"removed" else "would_remove",
     target=resolved_target,bytes=bytes,stringsAsFactors=FALSE)
 }
 if(!length(results))return(data.frame(year=year,uf=uf,status="already_clean",target="",bytes=0))
 do.call(rbind,results)
}
if(sys.nframe()==0L) {
 args<-commandArgs(trailingOnly=TRUE);apply<-"--apply" %in% args
 if(any(grepl("^--",args)&!args %in% c("--apply","--dry-run")))stop("Unknown option")
 values<-args[!grepl("^--",args)]
 if(!length(values))stop("Usage: cleanup-historical-extractions.R YEAR [UF,UF,...] [--apply]")
 year<-as.integer(values[1L]);ufs<-if(length(values)>=2L)strsplit(values[2L],",",fixed=TRUE)[[1L]] else historical_ufs
 rows<-lapply(toupper(ufs),function(uf)historical_cleanup_state(year,uf,apply=apply))
 report<-do.call(rbind,rows);print(report,row.names=FALSE)
 message(if(apply)"Removed " else "Would remove ",format(sum(report$bytes)/1024^3,digits=4)," GiB of verified extracted CSVs; ZIPs/RDS/logs/provenance retained")
 if(apply)write.csv(report,file.path("data-raw/eleicoes-renda/historico",paste0("cleanup-",year,"-",format(Sys.time(),"%Y%m%d-%H%M%S"),".csv")),row.names=FALSE)
}
