# ==============================================================================
# SCRIPT: 035_Splice_Microdados.R
#
# OBJETIVO / PURPOSE:
#   Criar bancos unificados de microdados unindo PNAD Anual (1992-2015) e
#   PNAD Contínua (2012-2024). Gera dois outputs:
#     - Microdados_Todas_Idades_1992_2024.parquet  (toda a população)
#     - Microdados_Jovens_18_24_1992_2024.parquet  (apenas 18-24 anos)
#
# PASSOS:
#   1. Carrega os parquet da PNAD Anual
#   2. Carrega o RDS cacheado da PNADC
#   3. Constrói as variáveis de educação harmonizadas (decisão D19):
#        ing_medio      = ingressou no ensino médio alguma vez
#        medio_completo = concluiu o ensino médio
#        ens_sup        = ingressou no ensino superior alguma vez (matriculado,
#                         graduado ou evadido) — definição validada vs Salata
#        ens_sup_a      = matriculado no ensino superior atualmente
#        sup_completo   = concluiu o ensino superior
#      PNAD Anual: códigos de curso por regime (1992-2006 vs 2007+) + anos de estudo
#      PNADC: ens_sup/ens_sup_a do cache (script 000) + VD3004
#      (substitui a antiga ens_medio, que era inconsistente entre fontes — D19)
#   4. Aplica conversões cambiais e IPCA (base Jan/2024)
#   5. Calcula decis, vintis E quintis de renda em nível NACIONAL (toda pop. com renda > 0)
#   6. Salva Microdados_Todas_Idades_1992_2024.parquet
#   7. Filtra 18-24 e salva Microdados_Jovens_18_24_1992_2024.parquet
#
# 2026-09-03 (WP1/WP2 do plano 9-vers/plan/2026-09-03_Plano_RRA_Renda_Salata.md):
#   acrescentadas colunas sexo/cor/regiao/rural (controles demográficos) e
#   quintil de renda — ADITIVO, nenhuma coluna/filtro pré-existente mudou.
#   Verificado por diff byte-a-byte das colunas antigas antes/depois do commit.
# ==============================================================================

library(dplyr)
library(tidyr)
library(Hmisc)
library(deflateBR)
library(arrow)
library(here)

options(scipen = 999)

# ── PARÂMETROS ────────────────────────────────────────────────────────────────
# Decisão D03 — Cobertura geográfica (rural Norte)
#   TRUE  → exclui UFs 11-16 rurais de TODAS as fontes (série consistente — padrão)
#           Aplica-se à PNADc via colunas `uf` e `local` do cache.
#           As PNAD (scripts 020/050) devem ser re-rodadas com EXCLUIR_RURAL_NORTE=TRUE.
#           Efeito: −17 a −19 k obs/ano na PNADc 2016-2019; taxa de acesso +0.4pp.
#   FALSE → sem filtro geográfico (inclui rural Norte em tudo)
#           Cria inconsistência temporal pois a PNAD pré-2004 não amostrava rural Norte.
# Para replicar Salata et al. (2025): usar TRUE aqui E nas PNAD (020/050).
# ⚠ A série VALIDADA do projeto (vs Salata, 2026-06-10; ex.: 2024 Nac=28.5%) foi
#   gerada com FALSE aqui (PNADC inclui rural Norte; ver memória/README — a
#   exclusão D03 aplica-se às PNAD nos imports 020/050). Com TRUE, as taxas
#   PNADC sobem ~+0.4pp (2016+). Mantido FALSE para preservar a série de referência.
EXCLUIR_RURAL_NORTE <- FALSE
UF_NORTE <- c(11L, 12L, 13L, 14L, 15L, 16L)  # RO, AC, AM, RR, PA, AP

# ── Controles demográficos (2026-09-03, WP1 do plano do RRA de renda) ───────
# Adição ADITIVA para 9-vers/plan/2026-09-03_Plano_RRA_Renda_Salata.md: sexo,
# cor, região e rural/urbano, usados como controles nos modelos logit do
# risco relativo ajustado (RRA). Decisão de desenho revisada em 2026-09-03:
# a tentativa original de juntar esses controles via um parquet auxiliar
# separado (script 036) foi abandonada porque a chave por valor (ano, fonte,
# idade, peso, renda_dom_pcta) colide em ~20% das linhas 18-24 (pesos
# amostrais e renda per capita se repetem entre pessoas diferentes — não é
# uma chave única). Trazer os controles aqui, junto com as colunas que já
# alimentam ens_sup/decil, é o único jeito robusto de garantir alinhamento
# linha-a-linha. NENHUMA coluna/filtro/lógica pré-existente foi alterada —
# só colunas novas foram acrescentadas ao select() final de cada fonte.
# Norte é a referência (Tabela 1, Salata et al. 2025: "região geográfica
# [norte (referência), nordeste, sudeste, sul e centro-oeste]"). Esta região
# é DIFERENTE de UF_NORTE acima (11:16 = filtro de cobertura amostral D03) —
# aqui são as 7 UFs completas da região Norte (11:17, incluindo Tocantins).
regiao_de_uf <- function(uf) {
  dplyr::case_when(
    uf %in% 11:17 ~ "Norte",
    uf %in% 21:29 ~ "Nordeste",
    uf %in% c(31L, 32L, 33L, 35L) ~ "Sudeste",
    uf %in% c(41L, 42L, 43L) ~ "Sul",
    uf %in% c(50L, 51L, 52L, 53L) ~ "Centro-Oeste",
    TRUE ~ NA_character_
  )
}

BASE_DIR   <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
# 2026-08-09: cache estendido para 2025 (o IBGE publicou PNADC_2025_visita1 em
# 2026-05-08). O nome do cache deriva de min/max ano no script 000, por isso
# mudou junto. O arquivo antigo (…_2012_2024_…) permanece em disco para que os
# snapshots .R das figuras já promovidas continuem reproduzíveis.
CACHED_PNADC <- here::here("data-raw", "pnadc_consolidado_2012_2025_interview1.rds")

cat("══════ Iniciando Splice de Microdados (1992-2024) ══════\n")

# 1. Carrega PNAD Anual
cat("  → Carregando PNAD Anual (1992-2015)...\n")
df_90 <- arrow::read_parquet(file.path(OUTPUT_DIR, "PNAD_Anual_1992_1999.parquet"))
df_00 <- arrow::read_parquet(file.path(OUTPUT_DIR, "PNAD_Anual_2001_2015.parquet"))

# Verifica que a coluna ens_sup existe (gerada pelos scripts 050/020 corrigidos)
# ens_sup = 1 se ingressou no ES em qualquer momento (matriculado, graduado ou desistente)
for (df_check in list(df_90, df_00)) {
  if (!"ens_sup" %in% names(df_check)) {
    stop("Coluna 'ens_sup' ausente nos parquets PNAD. Execute os scripts 050 e 020 atualizados.")
  }
}

# ── Variáveis de educação PNAD (D19) ─────────────────────────────────────────
# Construídas aqui a partir das colunas brutas preservadas nos parquets 020/050.
# Codificações confirmadas nos dicionários oficiais do IBGE (CATEGORI.TXT 1992;
# "Dicionário de variáveis de pessoas - 2007.xls") e validadas empiricamente:
#
#   Curso ATUAL  (V0603 1992-2006; V6003 2007+ — no parquet 2001-15 são cópias):
#     1992-2006: 1=reg.fund, 2=reg.médio, 3=supl.fund, 4=supl.médio, 5=SUPERIOR,
#                6=alfab.adultos(92-99: alfab; 01-06 idem), 8=pré-vestibular, 9=MESTRADO/DOUT
#     2007+    : 1=reg.fund, 2=reg.médio, 3=EJA fund, 4=EJA médio, 5=SUPERIOR,
#                6=alfab.adultos, 7=creche, 8=CA, 9=MATERNAL/JARDIM(!), 10=pré-vest,
#                11=MESTRADO/DOUT  (não existem códigos 12-14)
#     ⚠ Correção D19: o 020 aplicava {5,9} a todos os anos; em 2007+ o código 9 é
#       "maternal" — crianças eram marcadas ens_sup=1 na base de todas as idades.
#       Aqui: superior atual = {5,9} até 2006 e {5,11} em 2007+.
#   Curso ANTERIOR (V0607 1992-2006; V6007 2007+):
#     1992-2006: 1=elementar, 2=médio 1ºciclo(ginásio), 3=médio 2ºciclo(colegial),
#                4=1ºgrau, 5=2ºgrau, 6=SUPERIOR, 7=MESTRADO/DOUT
#     2007+    : idem 1-5, 6=EJA fund, 7=EJA médio, 8=SUPERIOR, 9=MESTRADO/DOUT
#   Mapeamento sistema antigo: médio 2º ciclo (colegial) conta como ensino médio;
#   médio 1º ciclo (ginásio) NÃO (equivale ao fundamental II).
df_pnad <- bind_rows(df_90, df_00) %>%   # bind_rows preenche V6003/V6007 com NA em 1992-99
  filter(!is.na(peso), peso > 0, !is.na(renda_dom_pcta), renda_dom_pcta > 0) %>%
  mutate(
    fonte = "PNAD Anual",
    freq_     = suppressWarnings(as.integer(V0602)) == 2L,
    ja_freq_  = suppressWarnings(as.integer(V0602)) == 4L &
                suppressWarnings(as.integer(V0606)) == 2L,
    cur_      = coalesce(suppressWarnings(as.integer(V6003)),
                         suppressWarnings(as.integer(V0603))),
    ant_      = if_else(ano >= 2007,
                        suppressWarnings(as.integer(V6007)),
                        suppressWarnings(as.integer(V0607))),
    # ── ens_sup_a: matriculado no ensino superior AGORA (inclui pós, como ens_sup)
    ens_sup_a = as.integer(
      !is.na(freq_) & freq_ & !is.na(cur_) &
      ((ano <  2007 & cur_ %in% c(5L, 9L)) |
       (ano >= 2007 & cur_ %in% c(5L, 11L)))
    ),
    # ── ens_sup: ingressou no superior em qualquer momento (D10 + correção D19)
    # Recomputado com os códigos corrigidos por regime. Para 18-24 é idêntico ao
    # ens_sup dos parquets 020/050 (validado vs Salata); difere apenas nas
    # crianças 2007+ em maternal (falso positivo do código antigo).
    ens_sup = as.integer(
      ens_sup_a == 1L |
      (!is.na(ja_freq_) & ja_freq_ & !is.na(ant_) &
       ((ano <  2007 & ant_ %in% c(6L, 7L)) |
        (ano >= 2007 & ant_ %in% c(8L, 9L))))
    ),
    # ── ing_medio: ingressou no ensino médio alguma vez ──────────────────────
    # = cursa médio agora (regular ou EJA/supletivo) ∪ curso anterior ≥ colegial
    #   ∪ pré-vestibular atual ∪ anos_estudo>=9 (≥1 série do médio concluída)
    #   ∪ ens_sup (quem entrou no superior passou pelo médio)
    ing_medio = as.integer(
      (!is.na(freq_) & freq_ & !is.na(cur_) &
        (cur_ %in% c(2L, 4L) |
         (ano <  2007 & cur_ == 8L) |        # pré-vestibular (92-2006)
         (ano >= 2007 & cur_ == 10L))) |     # pré-vestibular (2007+)
      (!is.na(ja_freq_) & ja_freq_ & !is.na(ant_) &
        ((ano <  2007 & ant_ %in% c(3L, 5L, 6L, 7L)) |
         (ano >= 2007 & ant_ %in% c(3L, 5L, 7L, 8L, 9L)))) |
      (!is.na(anos_estudo_num) & anos_estudo_num >= 9) |
      ens_sup == 1L
    ),
    # ── medio_completo: concluiu o ensino médio ──────────────────────────────
    # Proxy padrão: 11+ anos de estudo (V4703, derivada IBGE do bloco de educação).
    # ∪ ens_sup para coerência hierárquica (entrou no superior ⇒ concluiu médio).
    # ⚠ Caveat 1992-1995: ~21-22% de NA em anos_estudo_num → leve subestimação.
    medio_completo = as.integer(
      (!is.na(anos_estudo_num) & anos_estudo_num >= 11) | ens_sup == 1L
    ),
    # ── sup_completo: concluiu o ensino superior ─────────────────────────────
    # Proxy padrão da literatura: 15+ anos de estudo. (Rota direta V0609/V0611
    # não está nos parquets intermediários; consistência > precisão pontual.)
    sup_completo = as.integer(!is.na(anos_estudo_num) & anos_estudo_num >= 15),
    # Conversão cambial (D01): Cruzeiros/Cruzeiros Reais → Reais antes do deflator IPCA
    renda_corrigida = case_when(
      ano == 1992 ~ renda_dom_pcta / 2750000,  # Cr$ → R$
      ano == 1993 ~ renda_dom_pcta / 2750,      # CR$ → R$
      TRUE ~ renda_dom_pcta
    ),
    ref_date = as.Date(paste0(ano, "-09-01")),
    # ── Controles demográficos (WP1, 2026-09-03) — ver nota acima ───────────
    # V0302 (dicionário oficial PNAD): 2=Masculino, 4=Feminino. Recodificado
    # para a mesma escala 1=Homem/2=Mulher usada pela PNADC (cache 000).
    v0302_ = suppressWarnings(as.integer(V0302)),
    sexo = case_when(v0302_ == 2L ~ 1L, v0302_ == 4L ~ 2L, TRUE ~ NA_integer_),
    # V0404 (dicionário oficial PNAD): 2=Branca, 4=Preta, 6=Amarela, 8=Parda,
    # 0=Indígena, 9=Ignorada. Recodificado IDÊNTICO ao que
    # 000_PNADC_Download_Consolidate.R já faz para a PNADC — 0=Branca,
    # 1=Preta/Parda (negra), NA=Amarela/Indígena — para manter o painel
    # pessoa-ano homogêneo entre as duas fontes.
    v0404_ = suppressWarnings(as.integer(V0404)),
    cor = case_when(
      v0404_ == 2L ~ 0L,
      v0404_ %in% c(4L, 8L) ~ 1L,
      v0404_ %in% c(6L, 0L, 9L) ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    # situacao_censitaria (V4728): 1-3 = urbano, 4-8 = rural (mesmo corte já
    # usado no filtro rural Norte da decisão D03 nos scripts 020/050).
    rural = case_when(
      situacao_censitaria %in% 1:3 ~ 0L,
      situacao_censitaria %in% 4:8 ~ 1L,
      TRUE ~ NA_integer_
    ),
    regiao = regiao_de_uf(uf)
  ) %>%
  select(ano, idade, peso, renda_dom_pcta, renda_corrigida, ref_date,
         ing_medio, medio_completo, ens_sup, ens_sup_a, sup_completo, fonte,
         sexo, cor, regiao, rural)

rm(df_90, df_00); gc()

# 2. Carrega PNADC
cat("  → Carregando PNADC (2012-2024)...\n")
df_pnadc <- readRDS(CACHED_PNADC)

  # Filtro geográfico rural Norte (D03) — equivalente ao rec_geo de Salata
  # Cache usa: uf (int, 11-53) e local (1=Urbano, 2=Rural)
  if (EXCLUIR_RURAL_NORTE) {
    n_antes <- nrow(df_pnadc)
    df_pnadc <- df_pnadc %>%
      filter(!(as.integer(uf) %in% UF_NORTE & as.integer(local) == 2L))
    cat(sprintf("  → Rural Norte PNADc excluído: %d obs removidas de %d (%.2f%%)\n",
                n_antes - nrow(df_pnadc), n_antes,
                100 * (n_antes - nrow(df_pnadc)) / n_antes))
  } else {
    cat("  → Rural Norte PNADc INCLUÍDO (EXCLUIR_RURAL_NORTE = FALSE)\n")
  }

  # Verificar disponibilidade de variáveis ANTES do mutate (mais confiável que names(.))
  # Prioridade: ens_sup pré-computado no cache (script 000 corrigido 2026-06-09)
  #             = (V3003A/V3003 in sup_freq_codes) | (V3009A/V3009 in sup_nfreq_codes)
  #             Equivalente exato ao Salata (V3003A>=8 | V3009A>=12)
  # Sentinel: V3003_curso só existe no cache gerado APÓS a correção 2026-06-09
  # (script 000 adiciona V3003/V3003A normalizadas antes de salvar)
  # Cache antigo (antes da correção): V3003_curso=FALSE, ens_sup_a=0, ens_sup=VD3004>=6 apenas
  has_v3003_curso <- "V3003_curso" %in% names(df_pnadc)
  has_ens_sup     <- "ens_sup" %in% names(df_pnadc)
  has_ens_sup_ok  <- has_ens_sup && has_v3003_curso && mean(df_pnadc$ens_sup, na.rm = TRUE) > 0.01
  # ens_sup_a pré-computada no script 000 (V3002==1 & V3003_curso em códigos superior,
  # ciente da troca de questionário 2012-14/2015/2016+). Cache antigo bugado tinha ens_sup_a≡0.
  has_ens_sup_a   <- "ens_sup_a" %in% names(df_pnadc) &&
                     mean(df_pnadc$ens_sup_a, na.rm = TRUE) > 0.005
  has_vd3005      <- "VD3005" %in% names(df_pnadc)
  has_vd3004      <- "VD3004" %in% names(df_pnadc)
  # VD3004 (codificação oficial IBGE, 7 categorias):
  #   1=Sem instrução, 2=Fund.incompleto, 3=Fund.completo, 4=MÉDIO INCOMPLETO,
  #   5=Médio completo, 6=Superior incompleto, 7=Superior completo
  # (D19: a antiga ens_medio usava VD3004>=4 achando que 4=médio completo — era
  #  médio INCOMPLETO; gap de ~7pp vs PNAD no overlap. Agora: ing_medio = >=4,
  #  medio_completo = >=5, sup_completo = ==7.)
  cat(sprintf(
    "  → Cache PNADC: V3003_curso(sentinel)=%s  ens_sup(ok)=%s  ens_sup_a(ok)=%s  VD3005=%s  VD3004=%s\n",
    has_v3003_curso, has_ens_sup_ok, has_ens_sup_a, has_vd3005, has_vd3004))
  if (!has_ens_sup_a) stop("ens_sup_a ausente/inválida no cache PNADC — re-rode o script 000.")
  if (!has_vd3004)    stop("VD3004 ausente no cache PNADC — necessária para ing_medio/medio_completo/sup_completo.")
  if (has_ens_sup_ok) {
    cat(sprintf("  → Usando ens_sup do cache (equiv. Salata). Media geral: %.1f%%\n",
      mean(df_pnadc$ens_sup, na.rm = TRUE) * 100))
  } else {
    cat("  ⚠ Cache desatualizado (sem V3003_curso) — fallback para VD3005/VD3004 (proxy imperfeito)\n")
    cat("    → Para corrigir: re-rodar script 000_PNADC_Download_Consolidate.R\n")
  }

df_pnadc <- df_pnadc %>%
  filter(!is.na(peso), peso > 0, !is.na(renda_dom_pcta), renda_dom_pcta > 0) %>%
  mutate(
    fonte = "PNAD Contínua",
    # ens_sup: "ingressou no ensino superior em qualquer momento"
    # Hierarquia de fontes (ordem de preferência):
    #
    # 1ª opção (PREFERENCIAL): ens_sup pré-computado no cache pelo script 000
    #   Fórmula: (V3002==1 & V3003_curso %in% sup_freq_codes) | (V3009_curso %in% sup_nfreq_codes)
    #   Equivalente exato ao Salata: (V3003A>=8 | V3009A>=12)
    #   Cache re-gerado em 2026-06-09 — esta é a opção correta para todos os anos PNADC.
    #
    # Fallback 1 (proxy imperfeito): VD3005 >= 13
    #   Problema: VD3005=12 pode ser "médio completo (9 anos)" OU "1º ano superior"
    #   — ambiguidade não resolvível sem V3003A. Subestima ~5pp em 2016+.
    #
    # Fallback 2 (último recurso): VD3004 >= 6 (só formados — subestima ~6pp)
    ens_sup = if (has_ens_sup_ok) {
      as.integer(ens_sup)    # pré-computado no cache (equivalente ao Salata)
    } else if (has_vd3005) {
      stop("ens_sup ausente/inválido no cache — fallback para VD3005>=13 não permitido. Re-rode o script 000.")
    } else if (has_vd3004) {
      stop("VD3005 ausente — fallback para VD3004>=6 não permitido. Re-rode o script 000.")
    } else {
      stop("Nenhuma variável de ensino superior encontrada no cache PNADC.")
    },
    # ens_sup_a: matriculado no superior AGORA — pass-through do cache (script 000),
    # que trata a troca de questionário (V3003 2012-14 / V3003A 2015+ / transição 2015).
    ens_sup_a = as.integer(ens_sup_a),
    # Variáveis de médio/conclusão via VD3004 (codificação correta — ver D19 acima)
    vd3004_   = suppressWarnings(as.integer(VD3004)),
    ing_medio      = as.integer(vd3004_ >= 4L),
    medio_completo = as.integer(vd3004_ >= 5L),
    sup_completo   = as.integer(vd3004_ == 7L),
    renda_corrigida = renda_dom_pcta,
    ref_date = as.Date(sprintf("%d-06-15", ano)),
    # ── Controles demográficos (WP1, 2026-09-03) — ver nota acima ───────────
    # sexo e cor já vêm pré-computados e recodificados do cache (script 000);
    # aqui só passam adiante (pass-through), como ens_sup_a já faz.
    # local (V1022): 1=Urbana, 2=Rural — inverter para o binário 0/1 do rural.
    rural = case_when(local == 1L ~ 0L, local == 2L ~ 1L, TRUE ~ NA_integer_),
    regiao = regiao_de_uf(uf)
  ) %>%
  select(ano, idade, peso, renda_dom_pcta, renda_corrigida, ref_date,
         ing_medio, medio_completo, ens_sup, ens_sup_a, sup_completo, fonte,
         sexo, cor, regiao, rural)

# 3. Consolidação e Deflação
cat("  → Unindo bases e deflacionando para Jan/2024...\n")
df_all <- bind_rows(df_pnad, df_pnadc)
rm(df_pnad, df_pnadc); gc()

df_all <- df_all %>%
  mutate(
    renda_real = deflateBR::ipca(renda_corrigida, ref_date, "01/2024")
  ) %>%
  select(-ref_date, -renda_corrigida)

# 4. Cálculo dos Decis, Vintis e Quintis
# Quintil acrescentado em 2026-09-03 (WP2 do plano do RRA de renda) — mesma
# lógica de corte por ano × fonte que decil/vintil já usam, não um derivado
# aritmético do decil (evita assumir que os pontos de corte batem exatamente).
cat("  → Calculando decis, vintis e quintis de renda para toda a população...\n")
df_all <- df_all %>%
  group_by(ano, fonte) %>%
  mutate(
    limites_decil = list({
      brks <- wtd.quantile(renda_real, weights = peso, probs = seq(0, 1, by = 0.10), na.rm = TRUE)
      brks[1] <- -Inf; brks[11] <- Inf; brks
    }),
    limites_vintil = list({
      brks <- wtd.quantile(renda_real, weights = peso, probs = seq(0, 1, by = 0.05), na.rm = TRUE)
      brks[1] <- -Inf; brks[21] <- Inf; brks
    }),
    limites_quintil = list({
      brks <- wtd.quantile(renda_real, weights = peso, probs = seq(0, 1, by = 0.20), na.rm = TRUE)
      brks[1] <- -Inf; brks[6] <- Inf; brks
    })
  ) %>%
  mutate(
    decil   = cut(renda_real, breaks = limites_decil[[1]],   labels = 1:10, include.lowest = TRUE),
    vintil  = cut(renda_real, breaks = limites_vintil[[1]],  labels = 1:20, include.lowest = TRUE),
    quintil = cut(renda_real, breaks = limites_quintil[[1]], labels = 1:5,  include.lowest = TRUE)
  ) %>%
  ungroup() %>%
  select(-limites_decil, -limites_vintil, -limites_quintil) %>%
  mutate(
    decil   = as.integer(as.character(decil)),
    vintil  = as.integer(as.character(vintil)),
    quintil = as.integer(as.character(quintil))
  )

# 5. Salvar base completa — todas as idades
# Colunas: ano, idade, peso, renda_dom_pcta, renda_real,
#          ing_medio, medio_completo, ens_sup, ens_sup_a, sup_completo,
#          decil, vintil, quintil, fonte, sexo, cor, regiao, rural
df_todas <- df_all %>%
  filter(!is.na(decil))  # exclui apenas quem ficou sem decil (sem renda no ano)

# Checagem de hierarquia lógica (D19) — aborta se houver violações
viol <- df_todas %>%
  summarise(
    a = sum(ens_sup_a > ens_sup, na.rm = TRUE),
    b = sum(sup_completo > medio_completo, na.rm = TRUE),
    c = sum(medio_completo > ing_medio, na.rm = TRUE),
    d = sum(ens_sup > ing_medio, na.rm = TRUE)
  )
if (sum(unlist(viol)) > 0) {
  print(viol)
  stop("Violação da hierarquia educacional (ens_sup_a ⊆ ens_sup; sup_completo ⊆ medio_completo ⊆ ing_medio; ens_sup ⊆ ing_medio).")
}
cat("  → Hierarquia educacional OK (0 violações)\n")

output_todas <- file.path(OUTPUT_DIR, "Microdados_Todas_Idades_1992_2025.parquet")
cat(sprintf("  → Salvando base todas as idades (%d obs) em %s...\n",
            nrow(df_todas), output_todas))
write_parquet(df_todas, output_todas)

# 6. Salvar base 18-24 (backwards-compatible — mantém mesmo nome)
cat("  → Filtrando jovens (18-24 anos)...\n")
df_jovens <- df_todas %>%
  filter(idade >= 18, idade <= 24, !is.na(ens_sup))

output_jovens <- file.path(OUTPUT_DIR, "Microdados_Jovens_18_24_1992_2025.parquet")
cat(sprintf("  → Salvando base 18-24 (%d obs) em %s...\n",
            nrow(df_jovens), output_jovens))
write_parquet(df_jovens, output_jovens)

cat("\n✅ Concluído com sucesso!\n")
cat(sprintf("   Base todas as idades : %s\n", output_todas))
cat(sprintf("   Base jovens 18-24    : %s\n", output_jovens))
