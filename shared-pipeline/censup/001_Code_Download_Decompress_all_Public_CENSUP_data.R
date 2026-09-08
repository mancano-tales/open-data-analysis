# ============================================================
# CENSUP: Download + Extrai + Organiza (2009-2024)
# Usa httr2 (download robusto) + archive (extração robusta)
# + Sys.glob() BFS (cópia com encoding corrompido no Windows)
# ============================================================

library(httr2)
library(archive)
library(here)

base_dir    <- here::here("data-raw", "INEP")
main_folder <- "WWWWCenso_Educ_Superior_Auto_R"
main_path   <- file.path(base_dir, main_folder)
base_url    <- "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_"

anos <- 2009:2023

dir.create(main_path, showWarnings = FALSE, recursive = TRUE)

# ── FUNÇÃO 1: Download robusto ────────────────────────────────

download_robusto <- function(url, destfile) {
 if (file.exists(destfile)) {
  tamanho_local <- file.size(destfile)
  
  tamanho_remoto <- tryCatch({
   resp <- request(url) |> req_method("HEAD") |> req_perform()
   as.numeric(resp_header(resp, "Content-Length"))
  }, error = function(e) NA)
  
  if (!is.na(tamanho_remoto) && abs(tamanho_local - tamanho_remoto) < 1000) {
   cat("  ✅ ZIP já completo (", round(tamanho_local / 1e6, 1), "MB) — pulando download\n")
   return(invisible(TRUE))
  } else {
   cat("  ⚠️  ZIP incompleto (", round(tamanho_local / 1e6, 1), "MB local vs",
       round(tamanho_remoto / 1e6, 1), "MB remoto) — re-baixando...\n")
   file.remove(destfile)
  }
 }
 
 cat("  📥 Baixando de:", url, "\n")
 
 tryCatch({
  request(url) |>
   req_timeout(1800) |>
   req_retry(max_tries = 3, backoff = ~10) |>
   req_perform() |>
   resp_body_raw() |>
   writeBin(destfile)
  
  cat("  ✅ Download OK (", round(file.size(destfile) / 1e6, 1), "MB)\n")
  return(invisible(TRUE))
  
 }, error = function(e) {
  cat("  ❌ Falha no download:", e$message, "\n")
  if (file.exists(destfile)) file.remove(destfile)
  return(invisible(FALSE))
 })
}

# ── FUNÇÃO 2: Extração com archive ───────────────────────────

extrai_zip <- function(zip_path, temp_path) {
 cat("  📦 Extraindo com archive...\n")
 tryCatch({
  archive_extract(zip_path, dir = temp_path)
  cat("  ✅ Extração OK\n")
  return(invisible(TRUE))
 }, error = function(e) {
  cat("  ❌ Falha na extração:", e$message, "\n")
  return(invisible(FALSE))
 })
}

# ── FUNÇÃO 3: Cópia robusta via Sys.glob() BFS ───────────────
# Sys.glob() expande o glob direto na API do Windows,
# ignorando o encoding corrompido da string (ex: "EducaÆo")

copiar_pasta_seguro <- function(src, dst) {
 dir.create(dst, showWarnings = FALSE, recursive = TRUE)
 
 old_locale <- Sys.getlocale("LC_ALL")
 Sys.setlocale("LC_ALL", "C")
 
 # BFS: percorre toda a árvore de diretórios com Sys.glob()
 arquivos <- c()
 fila     <- src
 
 while (length(fila) > 0) {
  atual <- fila[1]
  fila  <- fila[-1]
  
  conteudo <- Sys.glob(file.path(atual, "*"))
  if (length(conteudo) == 0) next
  
  dirs  <- conteudo[dir.exists(conteudo)]
  files <- conteudo[!dir.exists(conteudo)]
  
  arquivos <- c(arquivos, files)
  fila     <- c(fila, dirs)
 }
 
 if (length(arquivos) == 0) {
  Sys.setlocale("LC_ALL", old_locale)
  cat("    ⚠️  Nenhum arquivo em:", src, "\n")
  return(FALSE)
 }
 
 # Copia arquivo a arquivo preservando subestrutura
 sucesso <- sapply(arquivos, function(arq) {
  rel  <- substring(arq, nchar(src) + 2)
  dest <- file.path(dst, rel)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  file.copy(arq, dest, overwrite = TRUE)
 })
 
 Sys.setlocale("LC_ALL", old_locale)
 
 n_ok  <- sum(sucesso, na.rm = TRUE)
 n_tot <- length(sucesso)
 cat("    →", n_ok, "/", n_tot, "arquivos copiados\n")
 
 return(n_ok > 0)
}

# ── LOOP PRINCIPAL ────────────────────────────────────────────

for (ano in anos) {
 cat("\n══════════════════════════════════════════════\n")
 cat("ANO:", ano, "\n")
 
 zip_nome <- paste0("microdados_censo_da_educacao_superior_", ano, ".zip")
 zip_path <- file.path(main_path, zip_nome)
 zip_url  <- paste0(base_url, ano, ".zip")
 ano_path <- file.path(main_path, ano)
 
 # ── 1. Pula ano se já foi organizado ─────────────
 if (dir.exists(ano_path) &&
     length(Sys.glob(file.path(ano_path, "*", "*"))) > 0) {
  cat("  ⏭️  Ano já processado — pulando\n")
  next
 }
 
 # ── 2. Download ───────────────────────────────────
 ok_download <- download_robusto(zip_url, zip_path)
 if (!isTRUE(ok_download)) {
  cat("  ⏭️  Pulando ano", ano, "(download falhou)\n")
  next
 }
 
 # ── 3. Extração ───────────────────────────────────
 temp_path <- file.path(main_path, paste0("_temp_", ano))
 unlink(temp_path, recursive = TRUE)
 dir.create(temp_path, showWarnings = FALSE)
 
 ok_extracao <- extrai_zip(zip_path, temp_path)
 if (!isTRUE(ok_extracao)) {
  unlink(temp_path, recursive = TRUE)
  next
 }
 
 # ── 4. Localiza pasta pai com locale neutro ───────
 old_locale <- Sys.getlocale("LC_ALL")
 Sys.setlocale("LC_ALL", "C")
 temp_subdirs <- Sys.glob(file.path(temp_path, "*"))
 temp_subdirs <- temp_subdirs[dir.exists(temp_subdirs)]
 Sys.setlocale("LC_ALL", old_locale)
 
 pasta_pai <- if (length(temp_subdirs) > 0) temp_subdirs[1] else temp_path
 cat("  📁 Pasta pai:", basename(pasta_pai), "\n")
 
 # ── 5. Cópia seletiva: dados / anexos / leia-me ───
 dir.create(ano_path, showWarnings = FALSE)
 
 pastas_alvo <- c("dados", "anexos", "leia-me")
 copiou_algo <- FALSE
 
 for (pp in pastas_alvo) {
  src <- file.path(pasta_pai, pp)
  
  old_locale <- Sys.getlocale("LC_ALL")
  Sys.setlocale("LC_ALL", "C")
  existe <- dir.exists(src)
  Sys.setlocale("LC_ALL", old_locale)
  
  if (existe) {
   cat("  📋 Copiando:", pp, "\n")
   ok <- copiar_pasta_seguro(src, file.path(ano_path, pp))
   if (ok) {
    cat("  ✅ Copiado:", pp, "\n")
    copiou_algo <- TRUE
   } else {
    cat("  ❌ Falha ao copiar:", pp, "\n")
   }
  }
 }
 
 # Fallback: anos antigos com arquivos soltos na raiz do ZIP
 if (!copiou_algo) {
  cat("  ℹ️  Sem subpastas padrão — copiando tudo para dados/\n")
  ok <- copiar_pasta_seguro(pasta_pai, file.path(ano_path, "dados"))
  if (ok) copiou_algo <- TRUE
 }
 
 # ── 6. Limpeza ────────────────────────────────────
 unlink(temp_path, recursive = TRUE)
 cat("  🧹 Temp limpa\n")
 
 if (copiou_algo) {
  cat("  ✅ ANO", ano, "CONCLUÍDO\n")
 } else {
  cat("  ❌ ANO", ano, "FALHOU — pasta vazia, verifique manualmente\n")
 }
}

cat("\n══════════════════════════════════════════════\n")
cat("✅ TODOS OS ANOS PROCESSADOS!\n")
cat("Estrutura final em:", main_path, "\n")
