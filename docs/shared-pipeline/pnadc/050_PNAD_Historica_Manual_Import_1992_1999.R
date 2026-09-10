# ==============================================================================
# SCRIPT: 050_PNAD_Historica_Manual_Import_1992_1999.R
#
# OBJETIVO / PURPOSE:
#   Baixar e importar microdados da PNAD da década de 1990 (1992, 1993, 1995-1999).
#   Abordagem: Lê dinamicamente os dicionários de importação SAS do IBGE para
#   extrair posições (fwf) sem depender do pacote microdadosBrasil.
#
# DECISÕES METODOLÓGICAS (ver docs/harmonization_decisions.md):
#   D09: Abordagem de importação manual lendo dicionários SAS para evitar bugs.
#   Renda Per Capita calculada via V4722 (Rend Fam) / V4724 (Composição Fam).
# ==============================================================================

library(here)
library(dplyr)
library(readr)
library(stringr)
library(httr)

options(scipen = 999)

BASE_DIR   <- here::here("data-raw", "harmonizing-br-data")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
CACHE_DIR  <- here::here("data-raw", "pnad_anual_raw")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

anos_anos_90 <- c(1992, 1993, 1995, 1996, 1997, 1998, 1999)

# ── PARÂMETROS ────────────────────────────────────────────────────────────────
# Decisão D03 — Cobertura geográfica (rural Norte)
#   TRUE  → exclui UFs 11-16 rurais de todos os anos (série consistente — padrão)
#           Para 1992-1999, o rural Norte não era amostrado pelo IBGE, portanto
#           o filtro é tautológico — não afeta os dados desta década.
#           DEVE ser igual ao EXCLUIR_RURAL_NORTE em 035_Splice_Microdados.R.
#   FALSE → sem filtro geográfico
EXCLUIR_RURAL_NORTE <- TRUE
UF_NORTE_ORIGINAL <- c(11L, 12L, 13L, 14L, 15L, 16L)  # RO, AC, AM, RR, PA, AP

# Variáveis alvo essenciais da década de 90 (V0101 removido devido a erro estrutural no dicionário SAS)
# V0606 + V0607: "já frequentou escola" + "nível mais alto frequentado" — necessários para ens_sup correto (D10)
# V4728: situação censitária (urbano/rural) — necessária para filtro geográfico (D03)
alvo_vars <- c("UF", "V0302", "V8005", "V0404", "V4703", "V4722", "V4724", "V4729",
               "V0602", "V0603", "V0604", "V0605", "V6003",
               "V0606", "V0607",
               "V4728")

# Função para extrair posições a partir do dicionário SAS do IBGE
parse_sas_dict <- function(sas_file, vars_of_interest) {
  linhas <- readLines(sas_file, encoding = "latin1", warn = FALSE)
  
  posicoes <- data.frame(
    var = character(),
    start = integer(),
    width = integer(),
    stringsAsFactors = FALSE
  )
  
  for (v in vars_of_interest) {
    # Busca a variável apenas onde ela é efetivamente declarada (ex: @00005 UF)
    idx <- grep(paste0("@ *[0-9]+ *", v, " *"), linhas, useBytes = TRUE)
    
    if (length(idx) > 0) {
      linha_var <- linhas[idx[1]]
      
      start_str <- stringr::str_extract(linha_var, "(?<=@)[0-9]+")
      
      # Divide pela variável, sem regex para evitar falhas
      parte_final <- strsplit(linha_var, v, fixed = TRUE)[[1]]
      
      if (length(parte_final) >= 2) {
        width_str <- stringr::str_extract(parte_final[2], "[0-9]+")
        
        start_pos <- as.integer(start_str)
        width <- as.integer(width_str)
        
        if (!is.na(start_pos) && !is.na(width)) {
          posicoes <- bind_rows(posicoes, data.frame(var = v, start = start_pos, width = width))
        }
      }
    }
  }
  
  posicoes <- posicoes %>%
    mutate(end = start + width - 1) %>%
    arrange(start)
  
  return(posicoes)
}

resultados <- list()

for (ano_i in anos_anos_90) {
  cat(sprintf("\n══════ Processando PNAD Histórica %d ══════\n", ano_i))
  
  ano_dir <- file.path(CACHE_DIR, as.character(ano_i))
  dir.create(ano_dir, showWarnings = FALSE)
  
  tryCatch({
    # 1. Download e extração do Dicionário (Layout.zip)
    url_layout <- sprintf("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/%d/Layout.zip", ano_i)
    dest_layout <- file.path(ano_dir, "Layout.zip")
    
    if (!file.exists(dest_layout)) {
      cat("  → Baixando Layout (Dicionário SAS)...\n")
      download.file(url_layout, dest_layout, mode = "wb", quiet = TRUE)
    }
    
    # Encontra o arquivo SAS_PES ou equivalente (pessoas)
    arquivos_layout <- unzip(dest_layout, list = TRUE)
    sas_file_info <- arquivos_layout %>% filter(
      grepl("(SAS|Input).*PE.*\\.(TXT|SAS|txt|sas)$", Name, ignore.case = TRUE) | 
      grepl("pe.*\\.sas$", Name, ignore.case = TRUE)
    )
    
    if (nrow(sas_file_info) == 0) {
      stop("Arquivo SAS de Pessoas não encontrado no Layout.")
    }
    
    unzip(dest_layout, files = sas_file_info$Name[1], exdir = ano_dir)
    sas_path <- file.path(ano_dir, sas_file_info$Name[1])
    
    # 2. Faz o parsing do dicionário SAS
    cat("  → Lendo metadados (FWF)...\n")
    dict_fwf <- parse_sas_dict(sas_path, alvo_vars)
    
    if(nrow(dict_fwf) < length(alvo_vars)) {
      cat("  ⚠ Atenção: Algumas variáveis não foram encontradas no dicionário SAS deste ano.\n")
      print(setdiff(alvo_vars, dict_fwf$var))
    }
    
    # Cria o objeto de fwf posições compatível com readr
    col_positions <- fwf_positions(
      start = dict_fwf$start,
      end   = dict_fwf$end,
      col_names = dict_fwf$var
    )
    
    # 3. Download e Extração dos Dados (Dados.zip)
    url_dados <- sprintf("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/%d/Dados.zip", ano_i)
    dest_dados <- file.path(ano_dir, "Dados.zip")
    
    if (!file.exists(dest_dados)) {
      cat("  → Baixando Dados (Pessoas)...\n")
      download.file(url_dados, dest_dados, mode = "wb", quiet = TRUE)
    }
    
    arquivos_dados <- unzip(dest_dados, list = TRUE)
    txt_file_info <- arquivos_dados %>% filter(
      grepl("^P", basename(Name), ignore.case = TRUE) & 
      !grepl("\\.zip$", basename(Name), ignore.case = TRUE)
    )
    
    if (nrow(txt_file_info) == 0) {
      stop("Arquivo TXT de Pessoas não encontrado.")
    }
    
    cat("  → Extraindo microdados (Aguarde, arquivo pesado)...\n")
    unzip(dest_dados, files = txt_file_info$Name[1], exdir = ano_dir)
    txt_path <- file.path(ano_dir, txt_file_info$Name[1])
    
    # 4. Leitura Otimizada com read_fwf
    cat("  → Lendo arquivo TXT...\n")
    df <- read_fwf(txt_path, col_positions = col_positions, col_types = cols(.default = col_character()), progress = FALSE)
    
    # ── Normalização dos nomes de variáveis (variam entre anos) ──────────────────
    # "frequenta escola": em alguns anos é V0602, em outros V0604
    if (!"V0602" %in% names(df) && "V0604" %in% names(df)) df$V0602 <- df$V0604
    # "curso que frequenta": V6003 ou V0605 como fallback de V0603
    if (!"V0603" %in% names(df)) {
      if ("V6003" %in% names(df)) df$V0603 <- df$V6003
      else if ("V0605" %in% names(df)) df$V0603 <- df$V0605
    }
    # Garantir colunas para o caso de não existirem no dicionário do ano
    for (col in c("V0602","V0603","V0606","V0607","V4728")) {
      if (!col %in% names(df)) df[[col]] <- NA_character_
    }

    # 5. Transformação e Cálculo da Per Capita
    df <- df %>%
      mutate(
        ano   = as.integer(ano_i),
        uf    = as.integer(UF),
        peso  = as.numeric(V4729),
        idade = as.integer(V8005),
        # ── Sentinela "sem declaração" (fix 2026-07-04) ──────────────────────────
        # V4722 usa 999999999999 (doze noves) para renda familiar nao declarada.
        # Sem este filtro, ~2,5% das pessoas/ano (8-10 mil linhas) entravam com
        # renda de ate 10^12, contaminando medias de renda_real e a atribuicao
        # de decis dos anos 1990 (sentinelas ranqueados como "D10"). NA aqui
        # propaga para renda_dom_pcta e cai no filtro renda > 0 do 035 (D02,
        # que sempre declarou excluir renda nao declarada). Ver
        # docs/harmonization_decisions.md (entrada 2026-07-04).
        renda_fam = {
          v <- suppressWarnings(as.numeric(V4722))
          ifelse(!is.na(v) & v >= 999999999999, NA_real_, v)
        },
        comp_fam  = as.integer(V4724),
        anos_estudo_bruto = as.integer(V4703),
        # ── ens_sup (D10): ingressou no ensino superior em qualquer momento ──────
        # Usa as.integer() para normalizar "5" e "05" → ambos viram 5 (robusto a
        # zero-padding que varia entre anos da PNAD).
        # Códigos (LAYOUT/CATEGORI.TXT — dicionário PNAD):
        #   V0602: 2=SIM (frequenta), 4=NÃO
        #   V0603: 5=Superior, 9=Mestrado/Doutorado  (int, leading zero nos anos 2000s)
        #   V0606: 2=SIM (já frequentou antes), 4=NÃO
        #   V0607: 6=Superior, 7=Mestrado/Doutorado  (int, leading zero nos anos 2000s)
        ens_sup = case_when(
          # Caso 1: estudante atual no superior
          suppressWarnings(as.integer(V0602)) == 2L &
            suppressWarnings(as.integer(V0603)) %in% c(5L, 9L)                           ~ 1L,
          # Caso 2: ex-aluno do superior (graduado ou desistente)
          suppressWarnings(as.integer(V0602)) == 4L &
            suppressWarnings(as.integer(V0606)) == 2L &
            suppressWarnings(as.integer(V0607)) %in% c(6L, 7L)                           ~ 1L,
          TRUE                                                                            ~ 0L
        ),
        situacao_censitaria = as.integer(V4728)
      ) %>%
      mutate(
        # Renda familiar per capita calculada na mão
        renda_dom_pcta = renda_fam / comp_fam,
        # Harmonização de escolaridade (1 = menos de 1 ano, ..., 16 = 15 anos)
        anos_estudo_num = case_when(
          anos_estudo_bruto == 1 ~ 0,
          anos_estudo_bruto >= 2 & anos_estudo_bruto <= 16 ~ anos_estudo_bruto - 1,
          TRUE ~ NA_real_
        )
      ) %>%
      # ── Filtro geográfico rural Norte (D03) ─────────────────────────────────────
      filter(
        if (EXCLUIR_RURAL_NORTE) {
          !(uf %in% UF_NORTE_ORIGINAL & !is.na(situacao_censitaria) & situacao_censitaria >= 4)
        } else {
          TRUE  # sem filtro geográfico
        }
      )
    
    cat(sprintf("  ✓ %d observações lidas com sucesso.\n", nrow(df)))
    
    resultados[[as.character(ano_i)]] <- df
    
  }, error = function(e) {
    cat(sprintf("  ✗ ERRO no ano %d: %s\n", ano_i, e$message))
  })
}

cat("\n\n══════ Salvando Microdados Harmonizados (1992-1999) ══════\n")
df_historico <- bind_rows(resultados)

# Salva em Parquet para altíssima performance e compressão
if (!require(arrow)) install.packages("arrow", repos = "https://cloud.r-project.org")
library(arrow)

output_file <- file.path(OUTPUT_DIR, "PNAD_Anual_1992_1999.parquet")
write_parquet(df_historico, output_file)
cat(sprintf("✅ Concluído! %d linhas salvas em %s\n", nrow(df_historico), output_file))
