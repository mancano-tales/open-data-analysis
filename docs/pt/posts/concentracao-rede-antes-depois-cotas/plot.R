# ==============================================================================
# SCRIPT: 042_Curva_Concentracao_Rede_Antes_Depois_Cotas.R
#
# OBJETIVO: Estender 041_Curva_Concentracao_Rede_Publica_Privada.R com uma
#           dimensão temporal: curvas de concentração de renda do acesso ao
#           ensino superior por rede (geral/pública/privada), em 3 períodos
#           em relação à Lei de Cotas (12.711/2012, sancionada em agosto de
#           2012, com rampa de implementação linear de 4 anos até atingir o
#           mínimo de 50% das vagas em 2016):
#             - "Before quotas"   (2001-2011, PNAD Anual)
#             - "Transition"      (2012-2016, rampa de implementação)
#             - "Mature quotas"   (2017-2024, lei plenamente em vigor)
#
# ACHADO CRÍTICO #2 (verificado nesta sessão, 2026-06-30, após pergunta do
# autor sobre comparar antes/depois das cotas):
#   Para PNAD Anual, a variável de rede de ensino é V6002 ("REDE DE ENSINO"),
#   ESTÁVEL em nome em toda a série 2001-2015 (ao contrário de quase toda
#   outra variável de educação nesse período, que muda de nome por ano --
#   ver 020_PNAD_Anual_Manual_Import_2001_2015.R). Verificado via leitura
#   direta dos dicionários SAS brutos do IBGE (cache local em
#   5-data/pnad_anual_raw/<ano>/). Codificação: 1. (1 caractere). Valores
#   2 = Rede PÚBLICA, 4 = Rede PRIVADA (verificado empiricamente: a
#   distribuição ponderada de V6002 entre TODOS os que frequentam escola,
#   qualquer nível, em 2007, é 78.7% código 2 / 21.3% código 4 -- consistente
#   com o ensino básico brasileiro ser majoritariamente público; CONFIRMA
#   2=pública, não o contrário).
#
#   ARMADILHA ENCONTRADA E CORRIGIDA: o comentário em
#   020_PNAD_Anual_Manual_Import_2001_2015.R (linhas ~245-248) afirma que,
#   para 2007+, "V6003 = 11 (superior), 12 (especialização), 13 (mestrado),
#   14 (doutorado)" -- mas essa é uma documentação DESATUALIZADA do próprio
#   projeto. A fórmula realmente usada e validada (em
#   035_Splice_Microdados.R, linhas ~100-110, confirmada contra os
#   dicionários oficiais do IBGE "Dicionário de variáveis de pessoas -
#   2007.xls") é `cur_ %in% c(5L, 11L)` para ano>=2007 -- código 5 =
#   SUPERIOR (graduação), código 11 = MESTRADO/DOUTORADO. Usar 11:14 (como
#   o comentário do 020 sugeria) captura uma fatia pequena e não
#   representativa: cruzando errado, a população estimada de matriculados
#   no superior em 2007 ficava em ~330 mil pessoas (15x menor que os ~4,9
#   milhões do Censo da Educação Superior/INEP daquele ano) e o split
#   pública/privada saía artificialmente perto de 50/50. Usando a fórmula
#   CORRETA (5, 11), a população estimada bate com a ordem de grandeza do
#   Censo (5,8 milhões em 2007) e o split pública/privada (24%/76% em 2007)
#   bate quase exatamente com o número já citado no projeto a partir de
#   Salata et al. (2025): "72,7% das matrículas em 2005 estavam na rede
#   privada" (docs/references/2026-06-10_Salata_et_al_2025_Origem_Social.md,
#   linha 287). Para anos < 2007 (regime antigo, variável V0603 em vez de
#   V6003), os códigos usados são 5 e 9 (idem 035_Splice_Microdados.R).
#
#   Cobertura testada e confirmada (100% de V6002 não-NA entre quem atende
#   à fórmula ens_sup_a acima) em: 2001, 2007, 2011 (antes), 2012, 2013,
#   2014, 2015 (transição). Variável V6002 segue presente até 2015 (último
#   ano da PNAD Anual antes da migração para a PNADC).
#
# FONTES POR PERÍODO:
#   Before (2001-2011): PNAD Anual, V6002, via leitura direta dos
#     dicionários SAS brutos do IBGE (mesma técnica "zero-dependencies" do
#     020_PNAD_Anual_Manual_Import_2001_2015.R). Pula 2010 (ano censitário,
#     sem PNAD). Inclui parte do regime pré-2007 (V0603) e pós-2007 (V6003).
#   Transition (2012-2016): 2012-2015 via PNAD Anual (V6002, último ano
#     disponível é 2015); 2016 via PNADC nativa (V3002A -- a PNAD Anual não
#     cobre 2016, e a PNADC só passa a ter V3002A com cobertura plena do
#     superior a partir de 2015, confirmado em 041).
#   Mature quotas (2017-2024): PNADC nativa (V3002A), pulando 2020-2021
#     (gap COVID -- decisão D11 do projeto, sem 1ª entrevista padrão
#     disponível via PNADcIBGE nesses anos).
#
# DECISÃO DE ESCOPO (após discussão com o autor, 2026-06-30):
#   Baseline "antes" = TODA a janela disponível 2001-2011 (não um subconjunto
#   de 3 anos), a pedido do autor -- mais estável estatisticamente, ao custo
#   de misturar uma década inteira de composição social pré-lei.
#   Período de transição é mostrado como uma curva própria (não omitido),
#   também a pedido do autor -- ciente de que mistura anos com proporções de
#   cotas diferentes (rampa de 25% a 50% das vagas) e por isso deve ser lido
#   como "instantâneo da transição", não como um patamar estável.
#   Extração standalone (não toca nos pipelines/caches compartilhados
#   000/001_PNADC_*.R nem nos parquets gerados por 020/050/035 da pasta
#   2026-06_Harmonizing-BR-Data) -- mesma lógica de 041.
#
# METODOLOGIA: idêntica a 041_Curva_Concentracao_Rede_Publica_Privada.R
#   (curva de concentração em grade percentual de 101 pontos; curva por
#   período = média simples das curvas anuais daquele período, ponto a
#   ponto na grade; Índice de Erreygers ano a ano via mesma fórmula de
#   020_PNADC_Indices_Concentracao.R). Ver aquele script para a explicação
#   completa da mecânica da curva.
# ==============================================================================

suppressPackageStartupMessages({
  library(PNADcIBGE); library(dplyr); library(readr); library(stringr)
  library(ggplot2); library(here)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

PNAD_RAW_DIR  <- "C:/Users/Mancano/Documents/MancanoSync/5-data/pnad_anual_raw"
PNADC_RAW_DIR <- "C:/Users/Mancano/Documents/MancanoSync/5-data/pnadc_raw"
N_PONTOS      <- 101L

ANOS_ANTES           <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011)
ANOS_TRANSICAO_PNAD  <- c(2012, 2013, 2014, 2015)
ANOS_TRANSICAO_PNADC <- c(2016)
ANOS_MADURO          <- c(2017, 2018, 2019, 2022, 2023, 2024)

PERIODOS <- c("Before quotas (2001-2011)", "Transition (2012-2016)", "Mature quotas (2017-2024)")

# ==============================================================================
# 1A. EXTRAÇÃO -- PNAD ANUAL (2001-2015)
# Parser "zero-dependencies" idêntico em técnica a
# 020_PNAD_Anual_Manual_Import_2001_2015.R (lê os dicionários SAS brutos do
# IBGE diretamente). Fórmula de ens_sup_a copiada da versão CORRIGIDA e
# validada em 035_Splice_Microdados.R (ver achado crítico #2 no cabeçalho).
# ==============================================================================
parse_sas_dict <- function(sas_file, vars_of_interest) {
  linhas <- readLines(sas_file, encoding = "latin1", warn = FALSE)
  linhas <- iconv(linhas, from = "latin1", to = "ASCII", sub = "")
  linhas <- stringr::str_squish(linhas)
  posicoes <- data.frame(var = character(), start = integer(), width = integer(), stringsAsFactors = FALSE)
  for (v in vars_of_interest) {
    idx <- grep(paste0("^@\\s*[0-9]+\\s+", v, "(\\s+|$)"), linhas, ignore.case = TRUE)
    if (length(idx) > 0) {
      linha_var <- linhas[idx[1]]
      start_str <- stringr::str_extract(linha_var, "(?<=@)[0-9]+")
      parte_final <- strsplit(linha_var, v, fixed = TRUE)[[1]]
      if (length(parte_final) >= 2) {
        width_str <- stringr::str_extract(parte_final[2], "[0-9]+")
        start_pos <- as.integer(start_str); width <- as.integer(width_str)
        if (!is.na(start_pos) && !is.na(width)) {
          posicoes <- bind_rows(posicoes, data.frame(var = v, start = start_pos, width = width))
        }
      }
    }
  }
  posicoes %>% mutate(end = start + width - 1) %>% arrange(start)
}

achar_arquivo <- function(dir, padrao) {
  fs <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = padrao, ignore.case = TRUE)
  fs <- fs[!grepl("temp_|/temp", fs)]
  if (length(fs) == 0) return(NA_character_)
  fs[1]
}

extrair_pnad_anual <- function(ano) {
  cat(sprintf("Lendo PNAD Anual %d (cache local em 5-data/pnad_anual_raw/)...\n", ano))
  dir_ano  <- file.path(PNAD_RAW_DIR, as.character(ano))
  sas_path <- achar_arquivo(dir_ano, "input.*PES.*\\.(txt|sas)$")
  txt_path <- achar_arquivo(dir_ano, "^PES[0-9]+\\.(txt|dat)$")
  if (is.na(sas_path) || is.na(txt_path)) stop(sprintf("Arquivos PNAD Anual %d nao encontrados", ano))

  vars <- c("V0602", "V6002", "V6003", "V0603", "V8005", "V4722", "V4724", "V4729")
  dict <- parse_sas_dict(sas_path, vars)
  col_pos <- fwf_positions(start = dict$start, end = dict$end, col_names = dict$var)
  df <- read_fwf(txt_path, col_positions = col_pos, col_types = cols(.default = col_character()), progress = FALSE)

  if (!"V6003" %in% names(df)) df$V6003 <- NA_character_
  if (!"V0603" %in% names(df)) df$V0603 <- NA_character_
  # cur_: curso atual, harmonizado entre regimes -- mesma lógica de coalesce
  # de 035_Splice_Microdados.R (V6003 2007+, V0603 pré-2007)
  cur_ <- coalesce(suppressWarnings(as.integer(df$V6003)), suppressWarnings(as.integer(df$V0603)))
  cur_codes <- if (ano >= 2007) c(5L, 11L) else c(5L, 9L)

  df %>%
    mutate(
      ano_      = as.integer(ano),
      idade_    = suppressWarnings(as.integer(V8005)),
      peso_     = suppressWarnings(as.numeric(V4729)),
      renda_    = suppressWarnings(as.numeric(V4722)) / suppressWarnings(as.numeric(V4724)),
      ens_sup_a_ = as.integer(
        !is.na(suppressWarnings(as.integer(V0602))) & suppressWarnings(as.integer(V0602)) == 2L &
        !is.na(cur_) & cur_ %in% cur_codes
      ),
      # V6002: 2 = Rede PÚBLICA, 4 = Rede PRIVADA (verificado nesta sessão --
      # ver achado crítico #2 no cabeçalho)
      rede_pub_  = as.integer(ens_sup_a_ == 1L & V6002 == "2"),
      rede_priv_ = as.integer(ens_sup_a_ == 1L & V6002 == "4")
    ) %>%
    transmute(ano = ano_, idade = idade_, peso = peso_, renda_dom_pcta = renda_,
              ens_sup_a = ens_sup_a_, rede_pub = rede_pub_, rede_priv = rede_priv_) %>%
    filter(idade >= 18, idade <= 24,
           !is.na(renda_dom_pcta), renda_dom_pcta > 0,
           !is.na(peso), peso > 0)
}

# ==============================================================================
# 1B. EXTRAÇÃO -- PNADC NATIVA (2016-2024)
# Idêntica a extrair_ano() em 041_Curva_Concentracao_Rede_Publica_Privada.R
# ==============================================================================
extrair_pnadc <- function(ano) {
  cat(sprintf("Lendo PNADC %d (cache local em 5-data/pnadc_raw/)...\n", ano))
  d <- get_pnadc(
    year = ano, interview = 1,
    vars = c("V2009", "V3002", "V3003A", "V3002A", "VD5008"),
    labels = FALSE, deflator = FALSE, design = FALSE,
    reload = FALSE, savedir = PNADC_RAW_DIR
  )
  peso_col <- intersect(c("V1032", "V1027", "V1028"), names(d))[1]
  stopifnot(!is.na(peso_col))

  d %>%
    transmute(
      ano            = ano,
      idade          = as.numeric(V2009),
      peso           = as.numeric(.data[[peso_col]]),
      renda_dom_pcta = as.numeric(VD5008),
      ens_sup_a = as.integer(
        !is.na(V3002) & as.numeric(V3002) == 1 &
        !is.na(V3003A) & as.numeric(V3003A) %in% 8:11
      ),
      # V3002A: 1 = Rede PRIVADA, 2 = Rede PÚBLICA (INVERTIDO vs V6002/V3004
      # -- ver 041_Curva_Concentracao_Rede_Publica_Privada.R)
      rede_pub  = as.integer(ens_sup_a == 1L & as.numeric(V3002A) == 2),
      rede_priv = as.integer(ens_sup_a == 1L & as.numeric(V3002A) == 1)
    ) %>%
    filter(idade >= 18, idade <= 24,
           !is.na(renda_dom_pcta), renda_dom_pcta > 0,
           !is.na(peso), peso > 0)
}

# ==============================================================================
# 2. EXTRAIR TODOS OS ANOS E ATRIBUIR PERÍODO
# ==============================================================================
dados_antes      <- bind_rows(lapply(ANOS_ANTES, extrair_pnad_anual)) %>% mutate(periodo = PERIODOS[1])
dados_transicao  <- bind_rows(
  bind_rows(lapply(ANOS_TRANSICAO_PNAD, extrair_pnad_anual)),
  bind_rows(lapply(ANOS_TRANSICAO_PNADC, extrair_pnadc))
) %>% mutate(periodo = PERIODOS[2])
dados_maduro     <- bind_rows(lapply(ANOS_MADURO, extrair_pnadc)) %>% mutate(periodo = PERIODOS[3])

dados <- bind_rows(dados_antes, dados_transicao, dados_maduro) %>%
  mutate(periodo = factor(periodo, levels = PERIODOS))

cat("\n══ Resumo por período ══════════════════════════════════════════════\n")
print(dados %>% group_by(periodo, ano) %>%
        summarise(n_matric = sum(ens_sup_a), n_pub = sum(rede_pub), n_priv = sum(rede_priv),
                  .groups = "drop"),
      n = 30)

# ==============================================================================
# 3. CURVA DE CONCENTRAÇÃO (grade percentual uniforme, 101 pontos)
# Lógica idêntica a calc_curva() em 041 / calc_curva_concentracao() em 040
# ==============================================================================
calc_curva <- function(df, y_col, n_pontos = N_PONTOS) {
  df <- df %>% arrange(renda_dom_pcta)
  total_w   <- sum(df$peso)
  total_y_w <- sum(df$peso * df[[y_col]])
  if (total_y_w <= 0) return(NULL)
  cum_pop     <- cumsum(df$peso) / total_w
  cum_outcome <- cumsum(df$peso * df[[y_col]]) / total_y_w
  grid_p <- seq(0, 1, length.out = n_pontos)
  grid_L <- approx(x = c(0, cum_pop), y = c(0, cum_outcome), xout = grid_p, rule = 2)$y
  data.frame(p = grid_p, L = grid_L)
}

SERIES <- c(ens_sup_a = "Overall", rede_pub = "Public network", rede_priv = "Private network")

combos <- distinct(dados, ano, periodo)
curvas_por_ano <- lapply(seq_len(nrow(combos)), function(i) {
  ano_i <- combos$ano[i]; periodo_i <- combos$periodo[i]
  df_a <- filter(dados, ano == ano_i, periodo == periodo_i)
  lapply(names(SERIES), function(col) {
    curva <- calc_curva(df_a, col)
    if (is.null(curva)) return(NULL)
    curva$serie   <- SERIES[[col]]
    curva$ano     <- ano_i
    curva$periodo <- periodo_i
    curva
  }) %>% bind_rows()
}) %>% bind_rows()

# Curva combinada por período: média simples das curvas anuais ponto a ponto
# na grade (cada ano pesa igual -- mesma justificativa de 041)
curva_pooled <- curvas_por_ano %>%
  group_by(periodo, serie, p) %>%
  summarise(L = mean(L), .groups = "drop") %>%
  mutate(
    periodo = factor(periodo, levels = PERIODOS),
    serie   = factor(serie, levels = c("Overall", "Public network", "Private network"))
  )

# ==============================================================================
# 4. ÍNDICE DE ERREYGERS (ano a ano) -- mesma fórmula de
#    020_PNADC_Indices_Concentracao.R / 041
# ==============================================================================
weighted_frac_rank <- function(x, w) {
  Nw <- sum(w); ord <- order(x); x_s <- x[ord]; w_s <- w[ord]
  cumw <- cumsum(w_s); r_s <- (cumw - w_s / 2) / Nw
  runs <- rle(x_s); pos <- 1L
  for (k in seq_along(runs$lengths)) {
    len <- runs$lengths[k]
    if (len > 1L) { idx <- pos:(pos + len - 1L); r_s[idx] <- weighted.mean(r_s[idx], w_s[idx]) }
    pos <- pos + len
  }
  r <- numeric(length(x)); r[ord] <- r_s; r
}
calc_erreygers <- function(y, w, x_income) {
  ok <- !is.na(y) & !is.na(w) & !is.na(x_income) & w > 0
  y <- y[ok]; w <- w[ok]; x <- x_income[ok]
  mu <- weighted.mean(y, w)
  if (mu <= 0 || mu >= 1) return(NA_real_)
  r <- weighted_frac_rank(x, w)
  cov_yr <- weighted.mean((y - mu) * (r - 0.5), w)
  CI <- 2 * cov_yr / mu
  4 * mu * CI
}

tab_E <- lapply(seq_len(nrow(combos)), function(i) {
  ano_i <- combos$ano[i]; periodo_i <- combos$periodo[i]
  df_a <- filter(dados, ano == ano_i, periodo == periodo_i)
  data.frame(
    ano = ano_i, periodo = periodo_i,
    E_overall = calc_erreygers(df_a$ens_sup_a, df_a$peso, df_a$renda_dom_pcta),
    E_publica = calc_erreygers(df_a$rede_pub,  df_a$peso, df_a$renda_dom_pcta),
    E_privada = calc_erreygers(df_a$rede_priv, df_a$peso, df_a$renda_dom_pcta)
  )
}) %>% bind_rows() %>% arrange(ano)
cat("\nÍndice de Erreygers por ano (população de referência: 18-24 anos):\n")
print(tab_E %>% mutate(across(starts_with("E_"), \(x) round(x, 4))), row.names = FALSE)

tab_E_periodo <- tab_E %>% group_by(periodo) %>%
  summarise(across(starts_with("E_"), mean), .groups = "drop") %>%
  mutate(periodo = factor(periodo, levels = PERIODOS)) %>% arrange(periodo)
cat("\nÍndice de Erreygers médio por período:\n")
print(tab_E_periodo %>% mutate(across(starts_with("E_"), \(x) round(x, 4))), row.names = FALSE)

# ==============================================================================
# 5. GRÁFICO -- pequenos múltiplos por período (3 painéis), 3 séries cada
# ==============================================================================
p_curva <- ggplot(curva_pooled, aes(x = p, y = L, colour = serie)) +
  geom_abline(slope = 1, intercept = 0, colour = "#333333",
              linewidth = 0.5, linetype = "dashed") +
  geom_line(linewidth = 1.0) +
  facet_wrap(~periodo, ncol = 3) +
  scale_colour_thesis(name = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                      breaks = seq(0, 1, 0.5), expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                      breaks = seq(0, 1, 0.5), expand = expansion(mult = 0.01)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Cumulative population share (poorest → richest)",
    y = "Cumulative share of tertiary enrollment"
  ) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "plain"),
        # Espaço extra entre painéis -- evita colisão dos rótulos "100%"/"0%"
        # nas bordas adjacentes dos 3 painéis (visto na 1ª inspeção visual,
        # 2026-06-30)
        panel.spacing.x = unit(1.6, "lines"))

# ── LEGENDA / DRAFT PARA O QUARTO ────────────────────────────────────────────
# fig-cap: "Income concentration curves for tertiary education enrollment by
#   network (public vs. private), before and after the 2012 Lei de Cotas,
#   Brazil 2001-2024 (PNAD Anual + PNAD Contínua, pooled by period).
#   Source: IBGE -- PNAD Anual (2001-2015) and PNAD Contínua via PNADcIBGE
#   (2016-2024)."
# fig-label: fig-concentracao-rede-antes-depois-cotas
# Nota metodológica (bloco "> **Note:**" no .qmd):
#   "Each panel pools the year-specific concentration curves within that
#   period (simple average across years, uniform 101-point percentile
#   grid), so each year contributes equally regardless of sample size.
#   'Before quotas' = 2001-2011 (PNAD Anual, excludes 2010, a census year
#   with no PNAD). 'Transition' = 2012-2016, the Lei de Cotas (12.711/2012)
#   phase-in period (quota share rising linearly from a minimum toward 50%
#   of seats by 2016); 2012-2015 from PNAD Anual, 2016 from PNAD Contínua
#   (the last year covered by PNAD Anual in this project's pipeline).
#   'Mature quotas' = 2017-2024 (PNAD Contínua), excluding 2020-2021 (no
#   standard first-interview data available for those years -- COVID-19).
#   Network variable: V6002 (PNAD Anual, 2001-2015) and V3002A (PNAD
#   Contínua, 2016+); see script header for the discovery that the
#   existing project's rede_ens (V3004-derived) never covers tertiary
#   students, and that V6002's correct 'currently enrolled' course-code
#   filter required correcting a stale comment in
#   020_PNAD_Anual_Manual_Import_2001_2015.R against the validated logic in
#   035_Splice_Microdados.R."
# Variáveis-chave: V6002 (PNAD Anual), V3002A (PNADC), ens_sup_a, renda_dom_pcta
# Referência cruzada no texto: @fig-concentracao-rede-antes-depois-cotas
# Dimensões: LARGURA_PAISAGEM × ~10cm (3 painéis quadrados lado a lado)
# Paleta: PALETA_QUALITATIVA (scale_colour_thesis) -- 3 categorias sem ordem
# ─────────────────────────────────────────────────────────────────────────────

salvar_grafico(p_curva, prefixo = "042_Curva_Concentracao_Rede_Antes_Depois_Cotas",
               largura = LARGURA_PAISAGEM, altura = 10, unidades = "cm",
               formato = "pdf")

# ──────────────────────────────────────────────────────────────────────────
# PROMOÇÃO MANUAL PARA REVISÃO — pacote PDF + .R + .qmd em 6-images-tables/final/
# Figura exploratória, validada visualmente (Ghostscript/TinyTeX) em
# 2026-06-30. NÃO inserida em nenhum capítulo ainda -- aguardando revisão do
# autor. Achado a discutir com o autor: o índice de Erreygers da rede
# pública é quase idêntico nos 3 períodos (0.064 antes, 0.063 transição,
# 0.067 maduro) -- esta medida (concentração de "estar na rede pública"
# relativa à população geral 18-24) não mostra o efeito redistributivo das
# cotas que se poderia esperar a priori; isso não significa que as cotas
# não tiveram efeito, apenas que este recorte específico (rede vs.
# população geral) pode não ser a lente mais sensível para capturá-lo --
# um efeito de composição racial/de renda DENTRO da rede pública, por
# exemplo, não aparece nesta curva.
# ──────────────────────────────────────────────────────────────────────────
finalizar_figura(
  plot        = p_curva,
  fig_label   = "concentracao-rede-antes-depois-cotas",
  fig_cap     = "Income concentration curves for tertiary education enrollment by network (public vs. private), before, during, and after the 2012 Lei de Cotas, Brazil 2001-2024 (PNAD Anual + PNAD Contínua, pooled by period).",
  fonte       = "IBGE — PNAD Anual (2001-2015) and PNAD Contínua via PNADcIBGE (2016-2024)",
  nota        = paste0(
    "Each panel pools the year-specific concentration curves within that period (simple average across years, uniform 101-point percentile grid), so each year contributes equally regardless of sample size. 'Before quotas' = 2001-2011 (PNAD Anual, excludes 2010, a census year with no PNAD). 'Transition' = 2012-2016, the Lei de Cotas (12.711/2012) phase-in period (quota share rising linearly toward a minimum of 50% of seats by 2016); 2012-2015 from PNAD Anual, 2016 from PNAD Contínua (the last year covered by PNAD Anual in this project's pipeline). 'Mature quotas' = 2017-2024 (PNAD Contínua), excluding 2020-2021 (no standard first-interview data available for those years, COVID-19). Network variable: V6002 (PNAD Anual, 2001-2015, coded 2 = public / 4 = private) and V3002A (PNAD Contínua, 2016+, coded 1 = private / 2 = public -- note the reversed coding between sources). Erreygers concentration index by period (population of reference: all 18-24-year-olds): overall = 0.291 (before) / 0.272 (transition) / 0.297 (mature); public network = 0.064 / 0.063 / 0.067; private network = 0.227 / 0.209 / 0.231 (see script console output for year-by-year values). The public-network index is essentially flat across all three periods -- this specific measure (concentration of public-network access relative to the general 18-24 population) does not show a clear redistributive shift after the quota law, which does not necessarily mean the law had no effect, only that this particular cut (network relative to the general population) may not be the most sensitive lens for it; a composition shift by race or income *within* the public network, for instance, would not be visible in this curve. See script header for the full discovery and verification log, including a correction to a stale course-code comment in 020_PNAD_Anual_Manual_Import_2001_2015.R."
  ),
  script_path = here::here("4-DA-Code", "2026-05_PNADcIBGE",
                            "042_Curva_Concentracao_Rede_Antes_Depois_Cotas.R"),
  largura = LARGURA_PAISAGEM, altura = 10, unidades = "cm"
)

cat("\n══════ Script 042 concluído ══════\n")
