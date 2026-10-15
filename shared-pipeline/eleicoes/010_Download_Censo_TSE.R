# Run from the repository root. Public sources; no token required.
# Downloads are cached; metadata and checksums record the exact input vintage.
suppressPackageStartupMessages({
  library(censobr)
  library(data.table)
  library(httr2)
  library(here)
})
options(timeout = max(600, getOption("timeout")))
dest <- here("data-raw", "eleicoes-renda")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

fetch_resource <- function(dataset, pattern, filename) {
  metadata <- request(paste0("https://dadosabertos.tse.jus.br/api/3/action/package_show?id=", dataset)) |>
    req_perform() |> resp_body_json(simplifyVector = TRUE)
  if (!isTRUE(metadata$success)) stop("TSE metadata request failed: ", dataset)
  resources <- as.data.table(metadata$result$resources)
  hits <- resources[grepl(pattern, url)]
  if (nrow(hits) != 1L) stop("Expected one TSE resource for ", pattern, "; found ", nrow(hits))
  writeLines(jsonlite::toJSON(metadata, auto_unbox = TRUE, pretty = TRUE),
             file.path(dest, paste0(dataset, "-metadata.json")))
  target <- file.path(dest, filename)
  if (!file.exists(target)) {
    partial <- paste0(target, ".part")
    request(hits$url) |> req_perform(path = partial)
    if (!file.rename(partial, target)) stop("Cannot finish download: ", target)
  }
  if (!nrow(unzip(target, list = TRUE))) stop("Empty or corrupt ZIP: ", target)
  data.table(source = hits$url, file = filename,
             md5 = unname(tools::md5sum(target)), checked_at = as.character(Sys.time()))
}

renda_path <- file.path(dest, "renda2022.rds")
if (!file.exists(renda_path)) {
  renda <- censobr::read_tracts(year = 2022, dataset = "ResponsavelRenda", as_data_frame = TRUE)
  if (!all(c("code_tract", "code_muni", "V06001", "V06004") %in% names(renda))) {
    stop("Unexpected censobr schema. Inspect data_dictionary(year=2022, dataset='tracts').")
  }
  saveRDS(renda, renda_path)
}
manifest <- rbindlist(list(
  fetch_resource("resultados-2026", "/votacao_secao_2026_BR[.]zip$", "votos2026.zip"),
  fetch_resource("eleitorado-2026", "/eleitorado_local_votacao_2026[.]zip$", "locais2026.zip")
))
fwrite(manifest, file.path(dest, "manifest.csv"))
writeLines(c(paste("censobr:", packageVersion("censobr")),
             paste("renda2022.rds md5:", tools::md5sum(renda_path)),
             capture.output(sessionInfo())), file.path(dest, "session-download.txt"))
message("Inputs ready in data-raw/eleicoes-renda/.")
