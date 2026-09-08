# ==============================================================================
# SCRIPT: 043_Wagstaff_TimeSeries_Rede_Publica_Privada.R
#
# OBJETIVO: Série temporal (2001-2024) do índice de Wagstaff de concentração
#           de renda no acesso ao ensino superior -- geral, rede pública e
#           rede privada -- a pedido do autor após ver os índices de
#           Erreygers agregados por período em 042. Mesma base de dados de
#           042_Curva_Concentracao_Rede_Antes_Depois_Cotas.R (PNAD Anual
#           2001-2015 via V6002 + PNAD Contínua 2016-2024 via V3002A), mas
#           ano a ano em vez de agrupado em 3 períodos, e Wagstaff em vez de
#           Erreygers.
#
# ÍNDICE DE WAGSTAFF: W = CI / (1 - μ) -- normaliza o Concentration Index
#   (Kakwani) pela prevalência, permitindo comparar anos/grupos com taxas de
#   acesso (μ) muito diferentes (ex: rede pública tem prevalência muito
#   menor que rede privada na população 18-24). Mesma fórmula de
#   020_PNADC_Indices_Concentracao.R (calc_ci_indices()). Reaproveitada
#   aqui ao lado de Erreygers (E = 4μ×CI) para comparação -- ver nota no
#   .qmd sobre a diferença de leitura entre as duas métricas.
#
# COBERTURA DOS QUESTIONÁRIOS (pergunta do autor, verificada nesta sessão):
#   PNAD/PNADC NÃO foram a campo em 2010 (ano censitário -- Censo
#   Demográfico substitui a PNAD nesse ano, prática histórica do IBGE).
#   PNADC 2020-2021: a pesquisa trimestral rodou normalmente durante a
#   pandemia, mas o arquivo padrão de "1ª entrevista" (visita anual com
#   VD5008/renda domiciliar per capita, que é o que toda a pipeline deste
#   projeto usa) NÃO está disponível via PNADcIBGE::get_pnadc() para esses
#   dois anos -- testado diretamente nesta sessão:
#     get_pnadc(year=2020, interview=1, ...) -> "Data unavailable for
#     selected interview and year." (idem 2021)
#   Isso é consistente com a decisão D11 já documentada no projeto
#   (harmonization_decisions.md): o IBGE adotou entrevista por telefone
#   (CATI) na pandemia e publicou arquivos especiais/retroponderados
#   distintos do desenho padrão, que o projeto optou por não usar por
#   problemas de comparabilidade de modo de coleta -- não porque a
#   pergunta de rede de ensino tenha sido removida do questionário.
#   Portanto: TODOS os anos em que a pesquisa-padrão deste projeto está
#   disponível (2001-2009, 2011-2019, 2022-2024 -- 21 anos) têm a pergunta
#   de rede de ensino com cobertura plena para quem está matriculado no
#   superior (testado e confirmado em 041/042); os únicos anos realmente
#   ausentes da série são 2010 (sem PNAD) e 2020-2021 (sem arquivo-padrão
#   de 1ª entrevista neste pipeline).
#
# EXTRAÇÃO: idêntica a 042_Curva_Concentracao_Rede_Antes_Depois_Cotas.R
#   (funções extrair_pnad_anual()/extrair_pnadc() copiadas de lá, incluindo
#   a correção de códigos V6003 documentada naquele script e agora também
#   corrigida em 020_PNAD_Anual_Manual_Import_2001_2015.R).
# ==============================================================================

suppressPackageStartupMessages({
  library(PNADcIBGE)
  library(dplyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(here)
  library(patchwork)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

PNAD_RAW_DIR <- file.path(dirname(here::here()), "5-data", "pnad_anual_raw")
PNADC_RAW_DIR <- file.path(dirname(here::here()), "5-data", "pnadc_raw")

ANOS_PNAD_ANUAL <- c(
  2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011,
  2012, 2013, 2014, 2015
)
ANOS_PNADC <- c(2016, 2017, 2018, 2019, 2022, 2023, 2024, 2025)

# ==============================================================================
# 1. EXTRAÇÃO -- idêntica a 042 (ver aquele script para o achado crítico
#    sobre V6002/V3002A e a correção de códigos)
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
        start_pos <- as.integer(start_str)
        width <- as.integer(width_str)
        if (!is.na(start_pos) && !is.na(width)) {
          posicoes <- bind_rows(posicoes, data.frame(var = v, start = start_pos, width = width))
        }
      }
    }
  }
  posicoes %>%
    mutate(end = start + width - 1) %>%
    arrange(start)
}

achar_arquivo <- function(dir, padrao) {
  fs <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = padrao, ignore.case = TRUE)
  fs <- fs[!grepl("temp_|/temp", fs)]
  if (length(fs) == 0) {
    return(NA_character_)
  }
  fs[1]
}

extrair_pnad_anual <- function(ano) {
  cat(sprintf("Lendo PNAD Anual %d (cache local em 5-data/pnad_anual_raw/)...\n", ano))
  dir_ano <- file.path(PNAD_RAW_DIR, as.character(ano))
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
  # de 035_Splice_Microdados.R (V6003 2007+, V0603 pré-2007). Códigos
  # corrigidos em 020_PNAD_Anual_Manual_Import_2001_2015.R em 2026-06-30
  # (ver aquele script e 042 para o achado completo).
  cur_ <- coalesce(suppressWarnings(as.integer(df$V6003)), suppressWarnings(as.integer(df$V0603)))
  cur_codes <- if (ano >= 2007) c(5L, 11L) else c(5L, 9L)

  df %>%
    mutate(
      ano_ = as.integer(ano),
      idade_ = suppressWarnings(as.integer(V8005)),
      peso_ = suppressWarnings(as.numeric(V4729)),
      renda_ = suppressWarnings(as.numeric(V4722)) / suppressWarnings(as.numeric(V4724)),
      ens_sup_a_ = as.integer(
        !is.na(suppressWarnings(as.integer(V0602))) & suppressWarnings(as.integer(V0602)) == 2L &
          !is.na(cur_) & cur_ %in% cur_codes
      ),
      # V6002: 2 = Rede PÚBLICA, 4 = Rede PRIVADA
      rede_pub_ = as.integer(ens_sup_a_ == 1L & V6002 == "2"),
      rede_priv_ = as.integer(ens_sup_a_ == 1L & V6002 == "4")
    ) %>%
    transmute(
      ano = ano_, idade = idade_, peso = peso_, renda_dom_pcta = renda_,
      ens_sup_a = ens_sup_a_, rede_pub = rede_pub_, rede_priv = rede_priv_
    ) %>%
    filter(
      idade >= 18, idade <= 24,
      !is.na(renda_dom_pcta), renda_dom_pcta > 0,
      !is.na(peso), peso > 0
    )
}

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
      ano = ano,
      idade = as.numeric(V2009),
      peso = as.numeric(.data[[peso_col]]),
      renda_dom_pcta = as.numeric(VD5008),
      ens_sup_a = as.integer(
        !is.na(V3002) & as.numeric(V3002) == 1 &
          !is.na(V3003A) & as.numeric(V3003A) %in% 8:11
      ),
      # V3002A: 1 = Rede PRIVADA, 2 = Rede PÚBLICA (INVERTIDO vs V6002)
      rede_pub = as.integer(ens_sup_a == 1L & as.numeric(V3002A) == 2),
      rede_priv = as.integer(ens_sup_a == 1L & as.numeric(V3002A) == 1)
    ) %>%
    filter(
      idade >= 18, idade <= 24,
      !is.na(renda_dom_pcta), renda_dom_pcta > 0,
      !is.na(peso), peso > 0
    )
}

CACHE_RDS <- here::here("4-DA-Code", "output", "dados_rede_publica_privada_2001_2025.rds")
if (file.exists(CACHE_RDS)) {
  cat(sprintf("[cache] Reaproveitando extração já salva em %s\n", CACHE_RDS))
  dados <- readRDS(CACHE_RDS)
} else {
  dados <- bind_rows(
    bind_rows(lapply(ANOS_PNAD_ANUAL, extrair_pnad_anual)),
    bind_rows(lapply(ANOS_PNADC, extrair_pnadc))
  ) %>% arrange(ano)
  # Salva para reuso futuro (evita repetir ~10 min de extração bruta a cada
  # nova pergunta sobre estes mesmos dados)
  saveRDS(dados, CACHE_RDS)
}
cat(sprintf("\n[dados] %s linhas (18-24 anos, todas as fontes)\n", format(nrow(dados), big.mark = ".")))

# ==============================================================================
# 2. ÍNDICES CI / WAGSTAFF / ERREYGERS -- mesma fórmula de
#    020_PNADC_Indices_Concentracao.R (calc_ci_indices())
# ==============================================================================
weighted_frac_rank <- function(x, w) {
  Nw <- sum(w)
  ord <- order(x)
  x_s <- x[ord]
  w_s <- w[ord]
  cumw <- cumsum(w_s)
  r_s <- (cumw - w_s / 2) / Nw
  runs <- rle(x_s)
  pos <- 1L
  for (k in seq_along(runs$lengths)) {
    len <- runs$lengths[k]
    if (len > 1L) {
      idx <- pos:(pos + len - 1L)
      r_s[idx] <- weighted.mean(r_s[idx], w_s[idx])
    }
    pos <- pos + len
  }
  r <- numeric(length(x))
  r[ord] <- r_s
  r
}
calc_ci_indices <- function(y, w, x_income) {
  ok <- !is.na(y) & !is.na(w) & !is.na(x_income) & w > 0
  y <- y[ok]
  w <- w[ok]
  x <- x_income[ok]
  if (length(y) < 20) {
    return(list(CI = NA_real_, W = NA_real_, E = NA_real_, mu = NA_real_))
  }
  mu <- weighted.mean(y, w)
  if (mu <= 0 | mu >= 1) {
    return(list(CI = NA_real_, W = NA_real_, E = NA_real_, mu = mu))
  }
  r <- weighted_frac_rank(x, w)
  cov_yr <- weighted.mean((y - mu) * (r - 0.5), w)
  CI <- 2 * cov_yr / mu
  W <- CI / (1 - mu)
  E <- 4 * mu * CI
  list(CI = CI, W = W, E = E, mu = mu)
}

SERIES_VARS <- c(ens_sup_a = "Overall", rede_pub = "Public network", rede_priv = "Private network")

anos_disponiveis <- sort(unique(dados$ano))
tab_idx <- lapply(anos_disponiveis, function(a) {
  df_a <- filter(dados, ano == a)
  linhas <- lapply(names(SERIES_VARS), function(col) {
    est <- calc_ci_indices(df_a[[col]], df_a$peso, df_a$renda_dom_pcta)
    data.frame(ano = a, serie = SERIES_VARS[[col]], mu = est$mu, CI = est$CI, W = est$W, E = est$E)
  })
  bind_rows(linhas)
}) %>% bind_rows()

cat("\n══ Índices CI / Wagstaff / Erreygers por ano e série ══════════════════\n")
print(tab_idx %>% mutate(across(c(mu, CI, W, E), \(x) round(x, 4))), row.names = FALSE)

OUTPUT_DIR_RES <- here::here("4-DA-Code", "output")
write.csv(tab_idx %>% mutate(across(c(mu, CI, W, E), \(x) round(x, 6))),
  file.path(OUTPUT_DIR_RES, "wagstaff_timeseries_rede_publica_privada_2001_2025.csv"),
  row.names = FALSE
)

# ==============================================================================
# 2.5 INTERVALOS DE CONFIANÇA — bootstrap individual (B=200 replicações)
#
# Método: para cada ano × série, reamostrar linhas com reposição (bootstrap
# simples, não cluster). Difere de 020_PNADC_Indices_Concentracao.R (stratified
# cluster bootstrap com UPA dentro de Estrato) porque a extração atual de 043
# não preserva variáveis de desenho amostral (Estrato, UPA) — elas são dropadas
# nos transmute() de extrair_pnadc() e não são incluídas no parse do PNAD Anual.
# Para estimativas nacionais com n >> 1 UPA por estrato, o bootstrap simples
# produz IC ligeiramente mais estreitos que o design-based (deff ≈ 1.2–1.5 para
# Wagstaff nacional), mas é adequado para comunicar incerteza na série temporal.
# ==============================================================================

BOOT_CACHE <- here::here(
  "4-DA-Code", "output",
  "wagstaff_bootstrap_rede_publica_privada_2001_2025.rds"
)
B_BOOTSTRAP <- 200
set.seed(2026)

if (file.exists(BOOT_CACHE)) {
  cat(sprintf("[cache] Reaproveitando bootstrap de %s\n", basename(BOOT_CACHE)))
  tab_boot <- readRDS(BOOT_CACHE)
} else {
  cat(sprintf(
    "Bootstrap B=%d (individual, %d anos x %d series)...\n",
    B_BOOTSTRAP, length(anos_disponiveis), length(SERIES_VARS)
  ))

  tab_boot <- bind_rows(lapply(anos_disponiveis, function(a) {
    df_a <- filter(dados, ano == a)
    cat(sprintf("  %d (n=%s) ", a, format(nrow(df_a), big.mark = ".")))

    res_a <- bind_rows(lapply(names(SERIES_VARS), function(col) {
      mat <- matrix(NA_real_, B_BOOTSTRAP, 2L,
        dimnames = list(NULL, c("W", "CI"))
      )
      for (b in seq_len(B_BOOTSTRAP)) {
        idx_b <- sample.int(nrow(df_a), nrow(df_a), replace = TRUE)
        est_b <- calc_ci_indices(
          df_a[[col]][idx_b],
          df_a$peso[idx_b],
          df_a$renda_dom_pcta[idx_b]
        )
        mat[b, ] <- c(est_b$W, est_b$CI)
      }
      data.frame(
        ano   = a,
        serie = SERIES_VARS[[col]],
        W_lo  = quantile(mat[, "W"], 0.025, na.rm = TRUE),
        W_hi  = quantile(mat[, "W"], 0.975, na.rm = TRUE)
      )
    }))
    cat("✓\n")
    res_a
  }))

  saveRDS(tab_boot, BOOT_CACHE)
  cat(sprintf("✅ Bootstrap salvo: %s\n", BOOT_CACHE))
}

# ==============================================================================
# 3. GRÁFICO -- série temporal do índice de Wagstaff, 2001-2025 + Banner de Governos
# Insere linhas com ano=NA-gap (2010, 2020, 2021) para que geom_line quebre
# visualmente nesses anos, em vez de interpolar uma linha reta sobre eles
# (mesmo princípio de 020_PNADC_Indices_Concentracao.R, "gap COVID").
# ==============================================================================
anos_todos <- seq(min(anos_disponiveis), max(anos_disponiveis))
anos_gap <- setdiff(anos_todos, anos_disponiveis)
cat(sprintf("\nAnos sem dados (gap visual no gráfico): %s\n", paste(anos_gap, collapse = ", ")))

grade_completa <- expand.grid(ano = anos_todos, serie = unname(SERIES_VARS), stringsAsFactors = FALSE)
tab_plot <- grade_completa %>%
  left_join(tab_idx %>% select(ano, serie, W), by = c("ano", "serie")) %>%
  left_join(tab_boot %>% select(ano, serie, W_lo, W_hi), by = c("ano", "serie")) %>%
  mutate(serie = factor(serie, levels = c("Overall", "Public network", "Private network")))

ANO_MAX_SERIE <- max(tab_plot$ano)
ANO_MIN_SERIE <- min(tab_plot$ano)

scale_x_compartilhada <- scale_x_anos_tese(anos = tab_plot$ano)

p_banner <- banner_governos(
  fim_dados = ANO_MAX_SERIE,
  ini_dados = ANO_MIN_SERIE,
  scale_x   = scale_x_compartilhada
) +
  theme(plot.margin = margin(2, 5.5, 1, 5.5))

p_main <- ggplot(tab_plot, aes(x = ano, y = W, colour = serie)) +
  linhas_transicao_gov() +
  geom_ribbon(aes(ymin = W_lo, ymax = W_hi, fill = serie),
    alpha = 0.15, colour = NA
  ) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.3, na.rm = TRUE) +
  scale_colour_thesis(name = NULL) +
  scale_fill_thesis(name = NULL) +
  scale_x_compartilhada +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Year", y = "Wagstaff concentration index (W = CI / (1 − mu))") +
  theme(
    legend.position = "bottom",
    plot.margin = margin(1, 5.5, 5.5, 5.5)
  )

p_combined <- p_banner / p_main + plot_layout(heights = c(1, 11))

# ── LEGENDA / DRAFT PARA O QUARTO ────────────────────────────────────────────
# fig-cap: "Income inequality in tertiary enrollment by network type (Wagstaff index), Brazil, 2001–2025."
# fig-label: fig-wagstaff-rede-publica-privada-timeseries
# Nota metodológica (bloco "\begin{fignote}"):
#   "Each line is the Wagstaff concentration index (W) of currently-enrolled
#   tertiary students aged 18--24 by income rank, by network. Shaded bands: 95%
#   bootstrap CIs (individual resampling, B=200). Lines break at 2010 and
#   2020--2021 (no standard-design survey those years). Source: IBGE — PNAD
#   Anual (2001--2015) and PNAD Contínua via \texttt{PNADcIBGE} (2016--2025)."
# Variáveis-chave: V6002 (PNAD Anual), V3002A (PNADC), ens_sup_a, renda_dom_pcta
# Referência cruzada no texto: @fig-wagstaff-rede-publica-privada-timeseries
# Dimensões: LARGURA_TEXTO × ALTURA_ALTA (6.27 × 5.00 in, 300 dpi)
# Paleta: PALETA_QUALITATIVA (scale_colour_thesis) -- 3 categorias sem ordem
# ─────────────────────────────────────────────────────────────────────────────

salvar_grafico(p_combined,
  prefixo = "043_Wagstaff_TimeSeries_Rede_Publica_Privada",
  largura = LARGURA_TEXTO, altura = ALTURA_ALTA, unidades = "in",
  formato = "pdf"
)

finalizar_figura(
  plot = p_combined,
  fig_label = "wagstaff-rede-publica-privada-timeseries",
  fig_cap = paste0("Income inequality in tertiary enrollment by network type (Wagstaff index), Brazil, 2001–", ANO_MAX_SERIE, "."),
  fonte = paste0("IBGE — PNAD Anual (2001--2015) and PNAD Contínua via \\texttt{PNADcIBGE} (2016--", ANO_MAX_SERIE, ")"),
  nota = paste0(
    "Each line is the Wagstaff concentration index (W) of currently-enrolled tertiary students aged 18--24 by income rank, by network. Shaded bands: 95\\% bootstrap CIs (individual resampling, $B=200$). Lines break at 2010 and 2020--2021 (no standard-design survey those years)."
  ),
  apendice = "sec-fignote-wagstaff-rede-publica-privada-timeseries",
  script_path = here::here(
    "4-DA-Code", "2026-05_PNADcIBGE",
    "043_Wagstaff_TimeSeries_Rede_Publica_Privada.R"
  ),
  largura = LARGURA_TEXTO, altura = ALTURA_ALTA, unidades = "in"
)

cat("\n══════ Script 043 concluído ══════\n")
