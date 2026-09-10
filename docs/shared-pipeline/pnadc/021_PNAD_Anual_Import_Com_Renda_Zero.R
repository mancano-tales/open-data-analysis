# ==============================================================================
# SCRIPT: 021_PNAD_Anual_Import_Com_Renda_Zero.R
#
# OBJETIVO / PURPOSE:
#   Cópia de trabalho do 020, com UMA única diferença funcional: PRESERVA os
#   domicílios de renda ZERO, que o 020 descarta na linha do filtro (D02).
#   Serve à série paralela de índices de desigualdade calculados pela
#   metodologia OFICIAL do IBGE, que inclui os zeros.
#
# POR QUE UM SCRIPT SEPARADO, e não um parâmetro no 020: o 020 alimenta o
#   parquet principal, e o 035_Splice_Microdados.R calcula os DECIS sobre a
#   população com renda > 0. Reprocessar o principal incluindo zeros moveria
#   as fronteiras de decil e quebraria a comparabilidade de todas as figuras
#   por decil já promovidas (041H, 245D, 042C, 042D, 231...). Este script
#   escreve num arquivo próprio e não toca em nada do pipeline principal.
#
# CONTEXTO EMPÍRICO (2026-08-08): excluir os zeros rebaixa o Gini em ~0,0050 e
#   a Palma em ~0,127, sistematicamente. Incluindo-os, o Gini reproduz a série
#   oficial do IBGE com desvio médio de −0,0001. Os intermediários da PNAD dos
#   anos 1990 (PNAD_Anual_1992_1999.parquet) JÁ preservam os zeros — só
#   2001-2015 precisava ser refeito, e é o que este script faz.
#
# ATENÇÃO: o filtro de renda NÃO DECLARADA (renda_dom_pcta < 999999) continua
#   valendo e não deve ser relaxado — renda não declarada não é renda zero.
#
# SAÍDA: output/PNAD_Anual_2001_2015_com_zero.parquet
# PLANO: 9-vers/plan/2026-08-08_Plano_Gini_Oficial_e_Gap_Pandemia.md (WP1)
# VER TAMBÉM: 020 (original, universo D02), 238 (monta a série oficial)
#
# DECISÕES METODOLÓGICAS:
#   D09: Abordagem de importação manual lendo dicionários SAS para evitar bugs.
#   D02: DELIBERADAMENTE NÃO aplicada aqui (é o propósito do script).
#   Variáveis: V0101, UF, V0302, V8005, V0404, V4729, V4732, V4803.
# ==============================================================================

library(here)
library(dplyr)
library(readr)
library(stringr)
library(rvest)

options(scipen = 999)

BASE_DIR   <- here::here("data-raw", "harmonizing-br-data")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
CACHE_DIR  <- here::here("data-raw", "pnad_anual_raw")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# Todos os anos disponíveis — PNAD não foi coletada em 2000 (Censo) e 2010 (Censo)
anos <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011, 2012, 2013, 2014, 2015)

# ── PARÂMETROS ────────────────────────────────────────────────────────────────
# Decisão D03 — Cobertura geográfica (rural Norte)
#   TRUE  → exclui UFs 11-16 rurais de TODOS os anos PNAD (série consistente — padrão)
#           Rural Norte não foi amostrado pelo IBGE pré-2004, portanto antes de 2004
#           o filtro é tautológico. O efeito real ocorre apenas em 2004-2015.
#           DEVE ser igual ao EXCLUIR_RURAL_NORTE em 035_Splice_Microdados.R.
#   FALSE → sem filtro geográfico (inclui rural Norte a partir de 2004)
#           Equivale à escolha do Salata et al. (2025) para a PNAD.
EXCLUIR_RURAL_NORTE <- TRUE
UF_NORTE_ORIGINAL <- c(11L, 12L, 13L, 14L, 15L, 16L)  # RO, AC, AM, RR, PA, AP

# Variáveis alvo: V0606 + V0607 adicionados para ens_sup correto (D10)
# V4728: situação censitária para filtro rural Norte (D03)
# V4803: anos de estudo a partir de 2007 (substituiu V4703)
# S0602, S6003, S0606, S6007: variantes S-prefix da PNAD 2007+ (renomeação do IBGE)
#   — o dicionário 2007+ usa S0602 (antes V0602), S6003 (antes V6003/V0603),
#     S0606 (antes V0606), S6007 (antes V0607).
alvo_vars <- c("V0101", "UF", "V0302", "V8005", "V0404",
               "V4703", "V4803", "V4722", "V4724", "V4729",
               "V0602", "V0603", "V6003",
               "V0606", "V0607",
               "V6007",            # 2007+: renome de V0607 (confirmado em INPUT PES2007.txt)
               "V4728",
               "S0602", "S6003", "S0606", "S6007")

parse_sas_dict <- function(sas_file, vars_of_interest) {
  linhas <- readLines(sas_file, encoding = "latin1", warn = FALSE)
  linhas <- iconv(linhas, from = "latin1", to = "ASCII", sub = "")
  linhas <- stringr::str_squish(linhas)
  
  posicoes <- data.frame(
    var = character(),
    start = integer(),
    width = integer(),
    stringsAsFactors = FALSE
  )
  
  for (v in vars_of_interest) {
    # Agora que usamos str_squish, o formato é garantidamente "@numero Vnome "
    idx <- grep(paste0("^@\\s*[0-9]+\\s+", v, "(\\s+|$)"), linhas, ignore.case = TRUE)
    
    if (length(idx) > 0) {
      linha_var <- linhas[idx[1]]
      
      start_str <- stringr::str_extract(linha_var, "(?<=@)[0-9]+")
      
      # O width vem logo após a variável, por exemplo "@18 V0101 4. "
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

# Mapeia dinamicamente os nomes dos zips no HTTP do IBGE
get_http_files <- function(url) {
  Sys.sleep(1) # Prevenir bloqueio
  tryCatch({
    page <- read_html(url)
    links <- page %>% html_nodes("a") %>% html_attr("href")
    return(links)
  }, error = function(e) return(character(0)))
}

for (ano_i in anos) {
  cat(sprintf("\n══════ Processando PNAD Anual %d ══════\n", ano_i))
  
  ano_dir <- file.path(CACHE_DIR, as.character(ano_i))
  dir.create(ano_dir, showWarnings = FALSE)
  
  tryCatch({
    
    # Define as URLs dinamicamente baseado no ano (via HTTPS)
    if (ano_i <= 2012) {
      zips_root <- list.files(CACHE_DIR, pattern = sprintf("PNAD_reponderado_%d.*\\.zip$", ano_i), full.names = TRUE, ignore.case = TRUE)
      if (length(zips_root) > 0) {
        dest_zip <- zips_root[1]
      } else {
        base_url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2012/"
        arquivos_http <- get_http_files(base_url)
        nome_zip <- arquivos_http[grepl(sprintf("PNAD_reponderado_%d", ano_i), arquivos_http, ignore.case = TRUE)][1]
        dest_zip <- file.path(ano_dir, nome_zip)
        if (!file.exists(dest_zip)) {
          cat("  → Baixando pacote unificado...\n")
          download.file(paste0(base_url, nome_zip), dest_zip, mode = "wb", quiet = TRUE)
        }
      }
      
      zip_layout <- dest_zip
      zip_dados <- dest_zip
      
    } else {
      # Anos 2013-2015
      zips_layout <- list.files(ano_dir, pattern = "Dicionarios.*\\.zip$|Layout.*\\.zip$|Leitura.*\\.zip$", full.names = TRUE, ignore.case = TRUE)
      zips_dados <- list.files(ano_dir, pattern = "Dados.*\\.zip$", full.names = TRUE, ignore.case = TRUE)
      
      if (length(zips_layout) > 0 && length(zips_dados) > 0) {
        zip_layout <- zips_layout[1]
        zip_dados <- zips_dados[1]
      } else {
        base_url <- sprintf("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/%d/", ano_i)
        arquivos_http <- get_http_files(base_url)
        
        nome_layout <- arquivos_http[grepl("Dicionario|Layout|Leitura", arquivos_http, ignore.case = TRUE) & grepl("\\.zip$", arquivos_http, ignore.case = TRUE)][1]
        nome_dados <- arquivos_http[grepl("Dados", arquivos_http, ignore.case = TRUE) & grepl("\\.zip$", arquivos_http, ignore.case = TRUE)][1]
        
        zip_layout <- file.path(ano_dir, nome_layout)
        zip_dados <- file.path(ano_dir, nome_dados)
        
        if (!file.exists(zip_layout)) {
          cat("  → Baixando Dicionários...\n")
          download.file(paste0(base_url, nome_layout), zip_layout, mode = "wb", quiet = TRUE)
        }
        if (!file.exists(zip_dados)) {
          cat("  → Baixando Dados...\n")
          download.file(paste0(base_url, nome_dados), zip_dados, mode = "wb", quiet = TRUE)
        }
      }
    }
    
    # 1. Extração do Dicionário SAS
    ps_cmd_layout <- sprintf('powershell -command "Expand-Archive -Path \'%s\' -DestinationPath \'%s/temp_layout\' -Force; Get-ChildItem -Path \'%s/temp_layout\' -Recurse -Filter \'*PE*.sas\' | Move-Item -Destination \'%s\' -Force; Get-ChildItem -Path \'%s/temp_layout\' -Recurse -Filter \'*PE*.txt\' | Move-Item -Destination \'%s\' -Force"', zip_layout, ano_dir, ano_dir, ano_dir, ano_dir, ano_dir)
    system(ps_cmd_layout, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    sas_path_list <- list.files(ano_dir, full.names = TRUE, pattern = "^(?i)(input|sas).*PE.*\\.(sas|txt)$")
    
    if (length(sas_path_list) == 0) {
      stop("Arquivo SAS de Pessoas não encontrado após extração.")
    }
    sas_path <- sas_path_list[1]
    
    cat("  → Lendo metadados (FWF)...\n")
    dict_fwf <- parse_sas_dict(sas_path, alvo_vars)
    
    col_positions <- fwf_positions(
      start = dict_fwf$start,
      end   = dict_fwf$end,
      col_names = dict_fwf$var
    )
    
    # 2. Extração dos Dados TXT
    cat("  → Extraindo microdados (Aguarde, arquivo pesado)...\n")
    ps_cmd_dados <- sprintf('powershell -command "Expand-Archive -Path \'%s\' -DestinationPath \'%s/temp_dados\' -Force; Get-ChildItem -Path \'%s/temp_dados\' -Recurse -Filter \'*PE*.dat\' | Move-Item -Destination \'%s\' -Force; Get-ChildItem -Path \'%s/temp_dados\' -Recurse -Filter \'*PE*.txt\' | Move-Item -Destination \'%s\' -Force"', zip_dados, ano_dir, ano_dir, ano_dir, ano_dir, ano_dir)
    system(ps_cmd_dados, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    txt_path_list <- list.files(ano_dir, full.names = TRUE, pattern = "^(?i)PE.*\\.(dat|txt)$")
    
    if (length(txt_path_list) == 0) {
      stop("Arquivo TXT/DAT de Pessoas não encontrado após extração.")
    }
    txt_path <- txt_path_list[1]
    
    # 3. Leitura com read_fwf
    cat("  → Lendo arquivo TXT...\n")
    df <- read_fwf(txt_path, col_positions = col_positions, col_types = cols(.default = col_character()), progress = FALSE)
    
    # 4. Harmonização e limpeza de nomes de variáveis
    # Anos de estudo: V4803 substituiu V4703 a partir de 2007
    if ("V4803" %in% names(df)) df$V4703 <- df$V4803
    # Curso que frequenta: V6003 ou V0603 (nomes variam por ano)
    if (!"V6003" %in% names(df) && "V0603" %in% names(df)) df$V6003 <- df$V0603
    if (!"V0603" %in% names(df) && "V6003" %in% names(df)) df$V0603 <- df$V6003
    # --- Normalizar variáveis S-prefix (PNAD 2007+, redesign do IBGE) ---
    # V0602 ← S0602: "frequenta escola?"
    if (!"V0602" %in% names(df) || all(is.na(df$V0602))) {
      if ("S0602" %in% names(df)) df$V0602 <- df$S0602
    }
    # V6003/V0603 ← S6003: "curso que frequenta"
    if ((!"V6003" %in% names(df) || all(is.na(df$V6003)))) {
      if ("S6003" %in% names(df)) { df$V6003 <- df$S6003; df$V0603 <- df$S6003 }
    }
    # V0606 ← S0606: "anteriormente frequentou escola?"
    if (!"V0606" %in% names(df) || all(is.na(df$V0606))) {
      if ("S0606" %in% names(df)) df$V0606 <- df$S0606
    }
    # Garantir existência de todas as colunas necessárias
    for (col in c("V0602","V0603","V6003","V0606","V0607","V6007","V4728")) {
      if (!col %in% names(df)) df[[col]] <- NA_character_
    }

    df <- df %>%
      mutate(
        ano   = as.integer(ano_i),
        uf    = as.integer(UF),
        peso  = as.numeric(V4729),
        idade = as.integer(V8005),
        renda_fam = as.numeric(V4722),
        comp_fam  = as.integer(V4724),
        anos_estudo_bruto = as.integer(V4703),
        # ── ens_sup (D10): ingressou no ensino superior em qualquer momento ──────
        # Fonte: Salata et al. (2025) var_PNAD_comp90.do via DATAZOOM:
        #   ens_sup = 1 if (curso_freq==5 | curso_freq==9) | (curso_nao_freq==6 | curso_nao_freq==7)
        # DATAZOOM harmoniza os códigos brutos do IBGE para uma escala unificada.
        # Mapeamento de códigos brutos → DATAZOOM (confirmado nos arquivos SPSS e empiricamente):
        #
        #   2001-2006:  V0602=2 (SIM freq.atual), V0603="05" (sup.atual), V0603="09" (mestrado)
        #               V0606=2 (SIM freq.ant.),  V0607="06" (sup.ant.),  V0607="07" (mestrado)
        #
        #   2007-2015:  V0602=2 (SIM freq.atual), V6003="05" (sup.atual), V6003="11" (mestrado/dout.)
        #               V0606=2 (SIM freq.ant.),  V6007="08" (sup.ant.),  V6007="09" (mestrado)
        #
        # Nota: para 2007+, V6003 substitui V0603 e V6007 substitui V0607 (redesign IBGE).
        #
        # CORREÇÃO 2026-06-30: o código 2007+ abaixo usava `V6003 %in% c(11L,12L,13L,14L)`,
        # documentação desatualizada/incorreta (sem validação contra o dicionário oficial
        # do IBGE) que sobrevivia apenas neste script -- a fórmula realmente validada e
        # usada em toda a dissertação é a de 035_Splice_Microdados.R ("Codificações
        # confirmadas nos dicionários oficiais do IBGE... e validadas empiricamente"):
        # `cur_ %in% c(5L, 11L)` para ano>=2007 (5=superior graduação atual,
        # 11=mestrado/doutorado atual). O código antigo (11:14) capturava uma fatia
        # pequena e não representativa do superior (população estimada ~15x menor que o
        # Censo da Educação Superior/INEP); verificado empiricamente em 2026-06-30 ao
        # construir 4-DA-Code/2026-05_PNADcIBGE/042_Curva_Concentracao_Rede_Antes_Depois_Cotas.R.
        # Esta coluna `ens_sup` (a deste script, 020) NÃO é a que alimenta a dissertação --
        # 035_Splice_Microdados.R recalcula ens_sup/ens_sup_a do zero a partir das colunas
        # brutas (V0602/V6003/V0603/V0606/V6007/V0607) preservadas neste parquet, já com a
        # fórmula correta, então os resultados já publicados não foram afetados por este
        # bug. A correção aqui evita que o comentário/código errado induza outro
        # desenvolvedor (ou assistente IA) a erro no futuro.
        ens_sup = case_when(
          # Caso 1: estudante ATUALMENTE no superior
          suppressWarnings(as.integer(V0602)) == 2L &
            (
              # 2001-2006: V0603/V6003 = 5 (superior), 9 (mestrado/doutorado)
              suppressWarnings(as.integer(V6003)) %in% c(5L, 9L) |
              suppressWarnings(as.integer(V0603)) %in% c(5L, 9L) |
              # 2007+: V6003 = 5 (superior graduação), 11 (mestrado/doutorado)
              suppressWarnings(as.integer(V6003)) %in% c(5L, 11L)
            )                                                                             ~ 1L,
          # Caso 2: JÁ FREQUENTOU o superior (graduado ou desistente)
          suppressWarnings(as.integer(V0602)) == 4L &
            suppressWarnings(as.integer(V0606)) == 2L &
            (
              # 2001-2006: V0607 = 6 (superior), 7 (mestrado/doutorado)
              suppressWarnings(as.integer(V0607)) %in% c(6L, 7L) |
              # 2007+: V6007 = 8 (superior), 9 (mestrado/doutorado)
              suppressWarnings(as.integer(V6007)) %in% c(8L, 9L)
            )                                                                             ~ 1L,
          TRUE                                                                            ~ 0L
        ),
        situacao_censitaria = as.integer(V4728)
      ) %>%
      mutate(
        renda_dom_pcta = renda_fam / comp_fam,
        anos_estudo_num = case_when(
          anos_estudo_bruto == 1 ~ 0,
          anos_estudo_bruto >= 2 & anos_estudo_bruto <= 16 ~ anos_estudo_bruto - 1,
          TRUE ~ NA_real_
        )
      ) %>%
      # ÚNICA diferença funcional vs 020: ">= 0" no lugar de "> 0", para reter
      # os domicílios de renda zero (metodologia oficial do IBGE). O teto de
      # 999999 (renda NÃO DECLARADA) permanece — não declarada não é zero.
      filter(!is.na(renda_dom_pcta), renda_dom_pcta >= 0, !is.na(peso), peso > 0,
             renda_dom_pcta < 999999) %>%
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

cat("\n\n══════ Salvando Microdados Harmonizados (2001-2015) ══════\n")
df_historico <- bind_rows(resultados)

# Salva em Parquet para altíssima performance e compressão
if (!require(arrow)) install.packages("arrow", repos = "https://cloud.r-project.org")
library(arrow)

output_file <- file.path(OUTPUT_DIR, "PNAD_Anual_2001_2015_com_zero.parquet")
write_parquet(df_historico, output_file)
cat(sprintf("   Zeros retidos: %s linhas (%.2f%% do total)\n",
            format(sum(df_historico$renda_dom_pcta == 0), big.mark = "."),
            100 * mean(df_historico$renda_dom_pcta == 0)))
cat(sprintf("✅ Concluído! %d linhas salvas em %s\n", nrow(df_historico), output_file))
