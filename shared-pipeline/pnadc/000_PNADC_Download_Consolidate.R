# ==============================================================================
# SCRIPT: 000_PNADC_Download_Consolidate.R
#
# OBJETIVO: Baixar e consolidar microdados da PNAD Contínua (2012-2024) via
#           pacote PNADcIBGE, produzindo um banco harmonizado com as mesmas
#           variáveis-chave do dataset Salata et al. (2025) para continuidade
#           das análises de acesso ao ensino superior por decil de renda.
#
# CONTEXTO: O dataset Salata et al. (2025) cobre 1992-2022 com variáveis
#           harmonizadas entre PNAD antiga e PNAD Contínua. Este script acessa
#           a PNADC diretamente pelo pacote oficial do IBGE para os anos
#           2012-2024, permitindo extensão ou validação das análises.
#
# MAPEAMENTO DE VARIÁVEIS (Salata → PNADC nativa):
#   peso          ← V1032    (peso amostral da 1ª visita anual)
#   renda_dom_pcta← VD5008   (rendimento domiciliar per capita, R$ correntes)
#   ens_sup_a     ← V3002==1 & V3003/V3003A %in% sup_freq_codes  (frequenta superior)
#   ens_sup       ← ens_sup_a | V3009/V3009A %in% sup_nfreq_codes (ingressou superior)
#   idade         ← V2009
#   sexo          ← V2007
#   cor           ← V2010
#   rede_ens      ← V3004   (rede pública=1 / privada=2, para quem frequenta)
#
# CORREÇÕES 2026-06-09 (confirmadas via pacote de replicação Salata et al. 2025):
#   BUG ORIGINAL: ens_sup_a usava V3001==1 & V3002 %in% 9:12, que é SEMPRE falso
#     porque V3002 só tem valores 1 (Sim) e 2 (Não) — NÃO é o código do curso!
#     V3002 = "Frequenta escola?" (binário); V3003/V3003A = "Qual curso frequenta?"
#
#   CORREÇÃO: usar V3002==1 (frequenta) + V3003/V3003A (código do curso superior)
#     Também: ens_sup não deve usar VD3004>=6 (captura só formados), mas sim
#     (V3003 cur) | (V3009 ant) para capturar também estudantes atuais e desistentes.
#
#   MUDANÇA DE NOME DE VARIÁVEL ENTRE PERÍODOS (fonte: dicionários IBGE + Salata):
#     2012-2014: V3003 (7-9=superior atual),  V3009 (10-12=superior anterior)
#     2015+:     V3003A (8-11=superior atual), V3009A (12-15=superior anterior)
#     Scripts agora detectam automaticamente qual versão está disponível.
#
# NOTA METODOLÓGICA:
#   - Usamos interview=1 (primeira visita anual) em vez de quarter=4 porque
#     VD5008 (renda domiciliar per capita) só existe no questionário anual.
#     O trimestral foca no mercado de trabalho e tem apenas renda individual.
#     interview=1 é um corte transversal anual representativo, adequado para
#     análise de acesso ao ensino superior.
#   - Deflação NÃO é necessária para o cálculo de decis de renda: decis são
#     calculados DENTRO de cada ano (posição relativa na distribuição anual),
#     portanto valores correntes e deflacionados produzem a mesma classificação.
#     VD5008 é usada diretamente como renda_dom_pcta sem deflação adicional.
#   - Diferença de cobertura: PNADC cobre todo o território nacional desde
#     2012; a PNAD antiga tinha cobertura variável até 2004. O rec_geo do
#     Salata não se aplica aqui (PNADC é sempre nacional/completa).
#
# TEMPO ESTIMADO: 30-90 minutos (download ~150-200 MB por ano).
#   Os arquivos baixados são salvos em SAVE_DIR e reutilizados em runs futuras
#   (reload = FALSE evita re-download se o arquivo já existir).
#
# SAÍDA: pnadc_consolidado_2012_2024_interview1.rds  (uso em R)
#        pnadc_consolidado_2012_2024_interview1.csv  (portabilidade)
# ==============================================================================


# ── 1. CONFIGURAÇÕES ──────────────────────────────────────────────────────────

# 2025 incluído em 2026-08-09: o IBGE publicou PNADC_2025_visita1 em
# 2026-05-08. Verificado antes de estender (ver plano de 2026-08-08): o
# dicionário de 2025 traz todas as variáveis usadas aqui (V1032, V2009,
# VD5008, VD3004, V3002, V3003A, V3009A), e NENHUM ano de 2012-2024 tem
# revisão pendente no FTP — os arquivos locais já são a safra corrente, então
# estender não arrasta reprocessamento retroativo da série.
ANOS_PNADC    <- 2012:2025   # Anos a baixar (ajustar se necessário)
ENTREVISTA    <- 1           # interview=1: 1ª visita anual (tem VD5008)

# Diretório onde os microdados brutos serão salvos (pesados: ~150 MB cada)
SAVE_DIR <- here::here("data-raw", "pnadc_raw")

# Diretório de saída do banco consolidado
OUTPUT_DIR <- here::here("data-raw")


# ── 2. PACOTES ────────────────────────────────────────────────────────────────

library(PNADcIBGE)   # install.packages("PNADcIBGE") se necessário
library(survey)
library(dplyr)
library(tidyr)

# Criar diretórios se não existirem
dir.create(SAVE_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ── 3. VARIÁVEIS A EXTRAIR ───────────────────────────────────────────────────
#
# O parâmetro vars= em get_pnadc() seleciona variáveis além das obrigatórias
# de desenho amostral (Estrato, UPA, V1027 — sempre incluídas pelo pacote).

VARS_PNADC <- c(
  # Demográficas
  "V2007",   # Sexo (1=Homem, 2=Mulher)
  "V2009",   # Idade (anos completos)
  "V2010",   # Cor ou raça (1=Branca, 2=Preta, 3=Amarela, 4=Parda, 5=Indígena)

  # Geografica
  "UF",      # Unidade da federação (código numérico)
  "V1022",   # Situação do domicílio (1=Urbana, 2=Rural)
  "V1023",   # Tipo de área (1=Capital, 2=Resto da RM, 3=Resto da UF)

  # Educação — frequência escolar atual
  # MAPEAMENTO CORRETO (confirmado via Salata et al. 2025):
  #   V3002  = "Frequenta escola ou creche?" → 1=Sim, 2=Não  (binária — NÃO é o curso!)
  #   V3002A = Rede de ensino (pública/privada)
  #   V3003  = "Qual é o curso que frequenta?" 2012-2014: 7-9=superior  (sem "A")
  #   V3003A = "Qual é o curso que frequenta?" 2015+:     8-11=superior (com "A")
  #   V3009  = "Curso mais elevado que frequentou?" 2012-2014: 10-12=superior (sem "A")
  #   V3009A = "Curso mais elevado que frequentou?" 2015+:     12-15=superior (com "A")
  # Salata 2012-2014: ens_sup = 1 if (V3003>=7 & V3003<=9) | (V3009>=10 & V3009<=12)
  # Salata 2015+:     ens_sup = 1 if (V3003A>=8 & V3003A<=11) | (V3009A>=12 & V3009A<=15)
  "V3002",   # Frequenta escola ou creche? (1=Sim, 2=Não) — presente em todos os anos
  "V3003",   # Curso que frequenta — 2012-2014 (ausente em 2015+, PNADcIBGE ignora)
  "V3003A",  # Curso que frequenta — 2015+ (ausente em 2012-2014, PNADcIBGE ignora)
  "V3009",   # Curso mais elevado anterior — 2012-2014
  "V3009A",  # Curso mais elevado anterior — 2015+
  "V3004",   # Rede escolar (1=Pública, 2=Privada) — apenas se V3002==1

  # Educação — nível de instrução alcançado (variável derivada IBGE)
  "VD3004",  # Nível mais elevado freq. ou concluído (1=s/instr..7=Sup.completo)
             #   5=Médio completo e sup.incompleto, 6=Superior completo, 7=Pós-graduação
  "VD3005",  # Anos de estudo brutos (0-16; entrada no superior ≈ VD3005>=13 após correc. Salata)

  # Renda domiciliar per capita
  "VD5008"   # Rendimento domiciliar per capita (R$ correntes — disponível
             # apenas no questionário anual; por isso usamos interview=1)
             # Deflação NÃO necessária: decis calculados dentro de cada ano
)


# ── 4. FUNÇÃO: PROCESSAR UM ANO ──────────────────────────────────────────────

processar_ano_pnadc <- function(ano, entrevista = 1) {

  cat(sprintf("══ Baixando PNADC %d (interview=%d) ", ano, entrevista),
      format(Sys.time(), "[%H:%M:%S]"), "\n")

  tryCatch({

    # 4.1 Download (ou leitura do cache) — sem deflator (não necessário para decis)
    dados_raw <- get_pnadc(
      year      = ano,
      interview = entrevista,  # 1ª visita anual: tem VD5008 (renda per capita)
      vars      = VARS_PNADC,
      labels    = FALSE,       # Manter códigos numéricos para harmonização
      deflator  = FALSE,       # Deflação desnecessária para cálculo de decis
      design    = FALSE,       # Retorna data.frame, não svydesign
      reload    = FALSE,       # Não re-baixar se arquivo já existe em savedir
      savedir   = SAVE_DIR
    )

    cat(sprintf("   ✓ Baixado: %d observações, %d colunas\n", nrow(dados_raw), ncol(dados_raw)))

    # ── Normalizar variáveis de curso que variam de nome entre períodos ──────────
    # Fonte: Salata et al. (2025) var_PNADc_2012_2014.do, var_PNADc_2015.do e var_PNADc_2016_2022.do
    # 2012-2014: V3003 (7-9=superior atual), V3009 (10-12=superior anterior)
    # 2015:      TRANSIÇÃO — ambos V3003A e V3003 presentes (questionários antigo e novo simultâneos)
    #            V3003A (8-11=superior novo), V3003 (7-9=superior antigo)
    # 2016+:     V3003A (8-11=superior atual), V3009A (12-15=superior anterior)
    if ("V3003A" %in% names(dados_raw) && "V3003" %in% names(dados_raw)) {
      # 2015 TRANSIÇÃO: dois tipos de questionário simultâneos no mesmo arquivo
      # Salata var_PNADc_2015.do:
      #   ens_sup = 1 if (V3003A>=8 & V3003A<=11) | (V3003>=7 & V3003<=9)
      #           | (V3009A>=12 & V3009A<=15) | (V3009>=10 & V3009<=12)
      dados_raw$V3003_curso     <- as.numeric(dados_raw$V3003A)  # questionário novo (8-11=sup)
      dados_raw$V3003_curso_old <- as.numeric(dados_raw$V3003)   # questionário antigo (7-9=sup)
      dados_raw$V3009_curso     <- as.numeric(if ("V3009A" %in% names(dados_raw)) dados_raw$V3009A else NA)
      dados_raw$V3009_curso_old <- as.numeric(if ("V3009"  %in% names(dados_raw)) dados_raw$V3009  else NA)
      sup_freq_codes  <- 8:11    # para V3003A (novo)
      sup_nfreq_codes <- 12:15   # para V3009A (novo)
      pct_new <- round(mean(!is.na(dados_raw$V3003_curso) | !is.na(dados_raw$V3009_curso)) * 100)
      cat(sprintf("   ✓ Curso: V3003A + V3003 (2015 transição — ambos os questionários)\n"))
    } else if ("V3003A" %in% names(dados_raw)) {
      # 2016+: só questionário novo
      dados_raw$V3003_curso  <- as.numeric(dados_raw$V3003A)
      dados_raw$V3009_curso  <- as.numeric(if ("V3009A" %in% names(dados_raw)) dados_raw$V3009A else NA)
      sup_freq_codes  <- 8:11    # V3003A: 8=grad, 9=esp, 10=mestrado, 11=doutorado
      sup_nfreq_codes <- 12:15   # V3009A: 12=grad, 13=esp, 14=mestrado, 15=doutorado
      cat(sprintf("   ✓ Curso: V3003A/V3009A (2015+ coding)\n"))
    } else if ("V3003" %in% names(dados_raw)) {
      # 2012-2014: só questionário antigo
      dados_raw$V3003_curso  <- as.numeric(dados_raw$V3003)
      dados_raw$V3009_curso  <- as.numeric(if ("V3009" %in% names(dados_raw)) dados_raw$V3009 else NA)
      sup_freq_codes  <- 7:9     # V3003: 7=grad, 8=espec, 9=mestrado/dout
      sup_nfreq_codes <- 10:12   # V3009: 10=grad, 11=espec, 12=mestrado/dout
      cat(sprintf("   ✓ Curso: V3003/V3009 (2012-2014 coding)\n"))
    } else {
      dados_raw$V3003_curso  <- NA_real_
      dados_raw$V3009_curso  <- NA_real_
      sup_freq_codes  <- integer(0)
      sup_nfreq_codes <- integer(0)
      cat("   ⚠ Variáveis de curso não encontradas — ens_sup será NA\n")
    }

    # Flag para 2015 transição (avaliado antes do mutate — names(.) não funciona dentro do mutate)
    has_old_codes <- "V3003_curso_old" %in% names(dados_raw)

    # Detectar variável de peso (nomes variam entre releases do IBGE)
    # V1032 = fator de expansão do MORADOR (pessoa) — dados anuais por visita
    # V1027 = fator de expansão — dados trimestrais
    # V1028 = usado em alguns releases intermediários
    peso_candidatos <- c("V1032", "V1027", "V1028", "posest_sxi", "posest", "V1030")
    peso_col <- intersect(peso_candidatos, names(dados_raw))[1]

    if (is.na(peso_col)) {
      # Diagnóstico: imprimir todas as colunas para identificar o nome correto
      cat("\n   ⚠ PESO NÃO ENCONTRADO. Colunas disponíveis no dado:\n")
      cat(paste(names(dados_raw), collapse = "\n   "), "\n\n")
      stop("Inspecione os nomes acima e adicione a variável correta em peso_candidatos")
    }

    cat(sprintf("   ✓ Peso: %s\n", peso_col))

    # Compatibilidade entre anos: variáveis que sumiram em releases mais recentes
    # V3004 (rede pública/privada): presente até 2017, ausente a partir de 2018
    if (!"V3004" %in% names(dados_raw)) {
      dados_raw[["V3004"]] <- NA_integer_
      cat("   ⚠ V3004 ausente neste ano → rede_ens = NA (removida pelo IBGE em 2018+)\n")
    }

    # 4.2 Harmonização das variáveis
    dados_harmonizados <- dados_raw %>%
      rename(
        # (Estrato, UPA já estão presentes — o pacote os inclui sempre)
        idade   = V2009,
        sexo    = V2007,
        cor_raw = V2010,
        uf      = UF
      ) %>%
      mutate(
        ano        = as.integer(ano),
        entrevista = as.integer(entrevista),

        # ── PESO AMOSTRAL ────────────────────────────────────────────────
        # V1028 nos dados anuais (visita), V1027 nos trimestrais
        peso = as.numeric(.data[[peso_col]]),

        # ── RENDA DOMICILIAR PER CAPITA ──────────────────────────────────
        # VD5008: R$ correntes do período de referência — suficiente para
        # calcular decis relativos dentro de cada ano
        renda_dom_pcta = as.numeric(VD5008),

        # ── ENSINO SUPERIOR — FREQUÊNCIA ATUAL (= ens_sup_a no Salata) ──
        # Fonte: Salata et al. (2025):
        #   2012-2014: ens_sup_a = 1 if V3003 >= 7 & V3003 <= 9
        #   2015:      ens_sup_a = 1 if (V3003A>=8 & V3003A<=11) | (V3003>=7 & V3003<=9)
        #   2016+:     ens_sup_a = 1 if V3003A >= 8 & V3003A <= 11
        # V3003_curso + V3003_curso_old (2015 apenas) cobrem todos os casos
        ens_sup_a = as.integer(
          # Questionário novo (2016+) ou único (2012-2014)
          (!is.na(V3002) & as.numeric(V3002) == 1 &
           !is.na(V3003_curso) & V3003_curso %in% sup_freq_codes) |
          # 2015: respondentes do questionário antigo (V3003_curso_old só existe em 2015)
          if (has_old_codes) {
            !is.na(V3002) & as.numeric(V3002) == 1 &
            !is.na(V3003_curso_old) & V3003_curso_old %in% 7:9
          } else { FALSE }
        ),

        # ── ENSINO SUPERIOR — INGRESSOU EM QUALQUER MOMENTO (= ens_sup Salata) ──
        # Fonte: Salata et al. (2025) — 2015 var_PNADc_2015.do:
        #   ens_sup = 1 if (V3003A>=8|V3003A<=11) | (V3003>=7|V3003<=9)
        #           | (V3009A>=12|V3009A<=15) | (V3009>=10|V3009<=12)
        ens_sup = as.integer(
          # Atualmente no superior (quest. novo OU antigo)
          (!is.na(V3002) & as.numeric(V3002) == 1 &
           !is.na(V3003_curso) & V3003_curso %in% sup_freq_codes) |
          if (has_old_codes) {
            !is.na(V3002) & as.numeric(V3002) == 1 &
            !is.na(V3003_curso_old) & V3003_curso_old %in% 7:9
          } else { FALSE } |
          # Já frequentou superior (quest. novo OU antigo)
          (!is.na(V3009_curso) & V3009_curso %in% sup_nfreq_codes) |
          if (has_old_codes) {
            !is.na(V3009_curso_old) & V3009_curso_old %in% 10:12
          } else { FALSE }
        ),

        # ── ENSINO SUPERIOR COMPLETO ──────────────────────────────────────
        # VD3004 >= 6: superior completo ou pós-graduação
        ens_sup_completo = as.integer(!is.na(VD3004) & as.numeric(VD3004) >= 6),

        # ── ENSINO MÉDIO (= ens_medio no Salata) ────────────────────────
        ens_medio = as.integer(!is.na(VD3004) & as.numeric(VD3004) >= 4),

        # ── REDE ESCOLAR (pública/privada) ──────────────────────────────
        # V3004: 1=Pública, 2=Privada (NA para quem não frequenta)
        rede_ens = as.integer(V3004),  # 1=pública, 2=privada, NA=não freq.

        # ── COR/RAÇA (recodificada compatível com Salata) ────────────────
        # Salata usa: 0=Branca, 1=Preta/Parda (negra)
        # PNADC: 1=Branca, 2=Preta, 3=Amarela, 4=Parda, 5=Indígena
        cor = case_when(
          cor_raw == 1 ~ 0L,                # Branca
          cor_raw %in% c(2, 4) ~ 1L,        # Preta ou Parda = Negra
          cor_raw %in% c(3, 5) ~ NA_integer_, # Amarela/Indígena → NA
          TRUE ~ NA_integer_
        ),

        # ── SEXO ─────────────────────────────────────────────────────────
        sexo = as.integer(sexo),  # 1=Homem, 2=Mulher

        # ── LOCAL/SITUAÇÃO ───────────────────────────────────────────────
        local = as.integer(V1022),  # 1=Urbana, 2=Rural

        # ── rec_idade: 18-24 anos (equivalente ao Salata rec_idade==1) ───
        rec_idade = as.integer(idade >= 18 & idade <= 24)
      ) %>%
      select(
        # Identificadores e desenho amostral
        ano, entrevista, Estrato, UPA, peso,
        # Demográficas
        idade, rec_idade, sexo, cor, cor_raw, uf, local,
        # Educação — variáveis computadas
        ens_sup_a, ens_sup, ens_sup_completo, ens_medio, rede_ens,
        # Educação — variáveis normalizadas (combinam V3003/V3003A e V3009/V3009A)
        V3003_curso, V3009_curso,
        # Colunas opcionais: só presentes em 2015 (transição de questionário)
        any_of(c("V3003_curso_old", "V3009_curso_old")),
        # Educação — variáveis derivadas IBGE
        VD3004, VD3005,
        # Renda (R$ correntes — suficiente para decis relativos por ano)
        renda_dom_pcta
      )

    cat(sprintf("   ✓ Processado: %d obs, %d variáveis\n",
                nrow(dados_harmonizados), ncol(dados_harmonizados)))

    return(dados_harmonizados)

  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("unavailable|length zero", msg, ignore.case = TRUE)) {
      cat(sprintf("   ⚠ %d (interview=%d): dados indisponíveis (provável COVID/gap IBGE)\n",
                  ano, entrevista))
    } else {
      cat(sprintf("   ✗ ERRO em %d (interview=%d): %s\n", ano, entrevista, msg))
    }
    return(NULL)
  })
}


# ── 5. LOOP SOBRE TODOS OS ANOS ──────────────────────────────────────────────

cat("\n╔════════════════════════════════════════════════╗\n")
cat("║  CONSOLIDAÇÃO PNAD CONTÍNUA — PNADcIBGE        ║\n")
cat("╚════════════════════════════════════════════════╝\n\n")
cat("Anos:", paste(ANOS_PNADC, collapse=", "), "\n")
cat("Modalidade: interview=1 (1ª visita anual — tem VD5008)\n\n")

lista_anos <- list()

for (ano in ANOS_PNADC) {
  dados <- processar_ano_pnadc(ano, ENTREVISTA)
  if (!is.null(dados)) {
    lista_anos[[as.character(ano)]] <- dados
  }
  Sys.sleep(1)  # Pausa entre downloads para não sobrecarregar servidor IBGE
}


# ── 6. CONSOLIDAÇÃO ──────────────────────────────────────────────────────────

anos_baixados <- names(lista_anos)

if (length(lista_anos) == 0) {
  stop("Nenhum ano foi baixado com sucesso. Verifique a conexão e os logs acima.")
}

cat(sprintf("\n✓ Anos baixados com sucesso: %s\n", paste(anos_baixados, collapse=", ")))

pnadc_consolidado <- bind_rows(lista_anos)

cat(sprintf("✓ Total de observações no banco consolidado: %s\n",
            format(nrow(pnadc_consolidado), big.mark=".")))


# ── 7. SALVAR ────────────────────────────────────────────────────────────────

output_rds <- file.path(OUTPUT_DIR,
  sprintf("pnadc_consolidado_%s_%s_interview%d.rds",
          min(anos_baixados), max(anos_baixados), ENTREVISTA))

output_csv <- gsub("\\.rds$", ".csv", output_rds)

saveRDS(pnadc_consolidado, output_rds)
write.csv(pnadc_consolidado, output_csv, row.names = FALSE)

cat(sprintf("\n✅ Banco salvo em:\n   %s\n   %s\n", output_rds, output_csv))


# ── 8. DIAGNÓSTICO RÁPIDO ────────────────────────────────────────────────────

cat("\n── Diagnóstico ─────────────────────────────────────────────────────────\n")
cat("Observações por ano:\n")
print(table(pnadc_consolidado$ano))

cat("\nTaxa de acesso ao ES (18-24 anos, média nacional por ano):\n")
pnadc_consolidado %>%
  filter(rec_idade == 1, !is.na(renda_dom_pcta)) %>%
  mutate(sup_total = as.integer(ens_sup == 1 | ens_sup_a == 1)) %>%
  group_by(ano) %>%
  summarise(
    taxa_acesso = weighted.mean(sup_total, w = peso, na.rm = TRUE) * 100,
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(taxa_acesso = sprintf("%.1f%%", taxa_acesso)) %>%
  print(n = Inf)

cat("\n── Estrutura do banco ──────────────────────────────────────────────────\n")
glimpse(pnadc_consolidado)
