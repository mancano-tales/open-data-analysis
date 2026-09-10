# ==============================================================================
# SCRIPT: 010_IPCA_Curso_Superior_Series.R
#
# O QUE O IPCA PODE DIZER sobre precos de mensalidade do ensino superior:
# emenda a serie do subitem "Curso superior" do IPCA (1991-presente) a partir
# de 5 vintages do SIDRA e produz tres leituras analiticas:
#   (1) Indice REAL de mensalidade  = subitem / IPCA geral   (base ago/1999)
#   (2) Razao mensalidade / salario minimo nominal            (base ago/1999)
#   (3) Peso do subitem na cesta do IPCA (onde disponivel: 1419, 2938, 7060)
#
# CAVEATS CENTRAIS (repetir na nota de qualquer figura promovida):
#   - E um indice de REAJUSTE (variacao de preco das mesmas instituicoes da
#     cesta), NAO nivel de preco, NAO ticket medio, NAO mediana.
#   - Nao captura barateamento por COMPOSICAO (migracao para EAD/low-cost):
#     o ticket medio de mercado pode cair com o IPCA real de mensalidade
#     subindo — historias complementares, nao contraditorias.
#   - Cobertura SNIPC: regioes metropolitanas, familias de 1-40 SM; emendas
#     entre vintages seguem reponderacoes da POF — a serie longa e uma
#     construcao nossa, declarada como tal.
#   - Pre-1994 (hiperinflacao): apenas sensibilidade, nunca na serie-mestre.
#
# TABELAS SIDRA (variacao mensal por subitem, salvo indicado) — janelas
# VERIFICADAS via info_sidra()$period em 2026-07-04:
#   58    jan/1991 - jul/1999
#   655   ago/1999 - jun/2006
#   2938  jul/2006 - dez/2011   (tambem peso mensal)
#   1419  jan/2012 - dez/2019   (tambem peso mensal)
#   7060  jan/2020 - presente   (tambem peso mensal e acumulados)
#   1737  dez/1979 - presente   IPCA geral, numero-indice (deflator)
# SM nominal: ipeadatar MTE12_SALMIN12 (mensal)
#
# FASE 2 (roteiro, fora deste script — cada item com aprovacao propria):
#   POF 2002-03/2008-09/2017-18 (gasto real com ed. superior por decil);
#   Semesp Mapa do Ensino Superior (ticket medio publico, captura composicao);
#   FIES/ProUni dados administrativos FNDE/MEC (proxy de preco IES x curso).
#
# ESTILO: utils/plot_theme.R | Cache: output/cache_sidra_<tabela>.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(ggplot2); library(scales); library(here)
  library(sidrar); library(ipeadatar)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())
options(scipen = 999)

BASE_DIR  <- here::here("data-raw", "ipca-mensalidades")
CACHE_DIR <- file.path(BASE_DIR, "output")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

# Base do indice: inicio da serie continua pos-hiperinflacao (tabela 655)
MES_BASE <- as.Date("1999-08-01")

# Janelas por tabela (contiguas por construcao; validado adiante).
# VERIFICADO 2026-07-04: a tabela 58 (1991-1999) NAO tem subitem "curso
# superior" — o nivel mais fino de educacao e "Cursos formais" (7301006),
# que mistura todos os niveis de ensino. Por isso 58 entra APENAS na figura
# de sensibilidade, como serie distinta e rotulada; o indice-mestre de
# mensalidade do ensino superior comeca em ago/1999 (tabela 655).
# Nomenclatura por vintage (mesmo conceito, nomes diferentes):
#   655 = "Curso terceiro grau" (8101005, pre-LDB); 1419/2938 = "Ensino
#   superior" (8101005); 7060 = "Curso superior" (1211102). O padrao regex
#   cobre as tres; o console imprime qual casou em cada tabela.
VINTAGES <- tibble::tribble(
  ~tabela, ~ini,        ~fim,                          ~padrao,          ~papel,
  58L,     "199101",    "199907",                      "cursos formais", "sensibilidade",
  655L,    "199908",    "200606",                      "curso superior|curso terceiro grau|ensino superior", "master",
  2938L,   "200607",    "201112",                      "curso superior|curso terceiro grau|ensino superior", "master",
  1419L,   "201201",    "201912",                      "curso superior|curso terceiro grau|ensino superior", "master",
  7060L,   "202001",    format(Sys.Date(), "%Y%m"),    "curso superior|curso terceiro grau|ensino superior", "master"
)

# ==============================================================================
# 1. DESCOBERTA + FETCH COM CACHE
# ==============================================================================

# Localiza a categoria "curso superior" na(s) classificacao(oes) da tabela,
# via metadados (info_sidra) — codigos numericos mudam entre vintages.
descobrir_categoria <- function(tabela, padrao = "curso superior") {
  info <- sidrar::info_sidra(tabela)
  cc   <- info$classific_category
  for (nm in names(cc)) {
    df <- as.data.frame(cc[[nm]])
    names(df) <- tolower(names(df))
    col_desc <- intersect(c("desc", "descricao", "nome"), names(df))[1]
    col_cod  <- intersect(c("cod", "codigo", "id"), names(df))[1]
    if (is.na(col_desc) || is.na(col_cod)) next
    hit <- df[grepl(padrao, df[[col_desc]], ignore.case = TRUE), , drop = FALSE]
    if (nrow(hit) > 0) {
      return(list(
        classific = sub(" .*$", "", nm),          # ex: "c315"
        cod       = hit[[col_cod]][1],
        desc      = hit[[col_desc]][1],
        n_hits    = nrow(hit)
      ))
    }
  }
  NULL
}

# Fetch com cache em RDS; refaz se o cache nao cobrir a janela pedida.
fetch_sidra_cached <- function(tabela, ini, fim, classific = NULL, cod = NULL) {
  cache_file <- file.path(CACHE_DIR, sprintf("cache_sidra_%d.rds", tabela))
  periodo    <- paste0(ini, "-", fim)
  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (identical(cached$periodo, periodo)) {
      cat(sprintf("  [%d] cache ok (%s)\n", tabela, periodo))
      return(cached$dados)
    }
    cat(sprintf("  [%d] cache com periodo diferente (%s vs %s) — refetch\n",
                tabela, cached$periodo, periodo))
  }
  cat(sprintf("  [%d] buscando %s no SIDRA...\n", tabela, periodo))
  args <- list(x = tabela, period = periodo, geo = "Brazil")
  if (!is.null(classific)) {
    args$classific <- classific
    args$category  <- list(cod)
  }
  dados <- do.call(sidrar::get_sidra, args)
  saveRDS(list(periodo = periodo, dados = dados), cache_file)
  dados
}

# Normaliza o retorno do sidrar para (data, variavel, valor)
normalizar <- function(df) {
  col_mes <- grep("^Mês \\(Código\\)$", names(df), value = TRUE)[1]
  if (is.na(col_mes)) col_mes <- grep("digo", grep("^M", names(df), value = TRUE),
                                       value = TRUE)[1]
  df |>
    transmute(
      data     = ym(.data[[col_mes]]),
      variavel = .data[["Variável"]],
      valor    = suppressWarnings(as.numeric(.data[["Valor"]]))
    )
}

cat("══════ 010_IPCA_Curso_Superior_Series ══════\n\n")
cat("1. Descobrindo e buscando o subitem 'curso superior' por vintage...\n")

series_vintages <- lapply(seq_len(nrow(VINTAGES)), function(i) {
  tab <- VINTAGES$tabela[i]
  cat_info <- descobrir_categoria(tab, padrao = VINTAGES$padrao[i])
  if (is.null(cat_info))
    stop(sprintf("Tabela %d: nenhuma categoria casa com '%s'. ",
                 tab, VINTAGES$padrao[i]),
         "Inspecionar info_sidra(", tab, ") manualmente.")
  if (cat_info$n_hits > 1)
    cat(sprintf("  [%d] AVISO: %d categorias casam; usando a primeira.\n",
                tab, cat_info$n_hits))
  cat(sprintf("  [%d] subitem: '%s' (cod %s, %s)\n",
              tab, cat_info$desc, cat_info$cod, cat_info$classific))
  bruto <- fetch_sidra_cached(tab, VINTAGES$ini[i], VINTAGES$fim[i],
                              classific = cat_info$classific,
                              cod       = cat_info$cod)
  normalizar(bruto) |>
    mutate(tabela = tab, subitem_desc = cat_info$desc,
           papel = VINTAGES$papel[i])
})

sub_all <- bind_rows(series_vintages)

# --- Variacao mensal do subitem CURSO SUPERIOR, serie-mestre (ago/1999+) ------
sub_var <- sub_all |>
  filter(papel == "master") |>
  filter(str_detect(variavel, regex("variação mensal", ignore_case = TRUE))) |>
  filter(!is.na(valor)) |>
  arrange(data)

# --- Cursos formais (1991-1999), APENAS sensibilidade -------------------------
pre_var <- sub_all |>
  filter(papel == "sensibilidade") |>
  filter(str_detect(variavel, regex("variação mensal", ignore_case = TRUE))) |>
  filter(!is.na(valor)) |>
  arrange(data)

# Validacao da emenda: meses consecutivos, sem duplicatas, sem buracos
stopifnot(!any(duplicated(sub_var$data)))
salto <- which(diff(sub_var$data) > 31)
if (length(salto) > 0)
  stop("Buraco na emenda do subitem em: ",
       paste(format(sub_var$data[salto], "%Y-%m"), collapse = ", "))
cat(sprintf("\n  Serie do subitem emendada: %s a %s (%d meses, sem buracos)\n",
            format(min(sub_var$data), "%Y-%m"), format(max(sub_var$data), "%Y-%m"),
            nrow(sub_var)))

# --- Peso mensal do subitem curso superior (onde existir) ---------------------
sub_peso <- sub_all |>
  filter(papel == "master") |>
  filter(str_detect(variavel, regex("peso mensal", ignore_case = TRUE))) |>
  filter(!is.na(valor)) |>
  arrange(data)
cat(sprintf("  Peso mensal disponivel: %d meses (%s a %s)\n",
            nrow(sub_peso),
            if (nrow(sub_peso) > 0) format(min(sub_peso$data), "%Y-%m") else "-",
            if (nrow(sub_peso) > 0) format(max(sub_peso$data), "%Y-%m") else "-"))

# ==============================================================================
# 2. IPCA GERAL (1737, numero-indice) E SALARIO MINIMO (ipeadatar)
# ==============================================================================
cat("\n2. IPCA geral (1737) e salario minimo (MTE12_SALMIN12)...\n")

ipca_geral <- fetch_sidra_cached(1737L, "199101", format(Sys.Date(), "%Y%m")) |>
  normalizar() |>
  filter(str_detect(variavel, regex("número-índice", ignore_case = TRUE))) |>
  filter(!is.na(valor)) |>
  select(data, ipca_idx = valor) |>
  arrange(data)

CACHE_SM <- file.path(CACHE_DIR, "cache_sm_mensal.rds")
if (file.exists(CACHE_SM)) {
  sm <- readRDS(CACHE_SM)
  cat("  SM: cache ok\n")
} else {
  cat("  SM: buscando via ipeadatar...\n")
  sm <- ipeadatar::ipeadata("MTE12_SALMIN12") |>
    transmute(data = as.Date(date), sm_nominal = value) |>
    filter(data >= as.Date("1991-01-01"))
  saveRDS(sm, CACHE_SM)
}

# ==============================================================================
# 3. INDICES (base 100 em ago/1999)
# ==============================================================================
cat("\n3. Construindo indices (base 100 = ago/1999)...\n")

serie <- sub_var |>
  select(data, var_mensal = valor) |>
  mutate(sub_idx_bruto = cumprod(1 + var_mensal / 100)) |>
  left_join(ipca_geral, by = "data") |>
  left_join(sm, by = "data")

if (any(is.na(serie$ipca_idx)))
  stop("IPCA geral (1737) sem cobertura para: ",
       paste(format(serie$data[is.na(serie$ipca_idx)], "%Y-%m"), collapse = ", "))

base <- serie |> filter(data == MES_BASE)
stopifnot(nrow(base) == 1)

serie <- serie |>
  mutate(
    sub_idx  = 100 * sub_idx_bruto / base$sub_idx_bruto,
    ipca_b   = 100 * ipca_idx / base$ipca_idx,
    sm_b     = 100 * sm_nominal / base$sm_nominal,
    # (1) Indice real de mensalidade: deflacionado pelo IPCA cheio
    idx_real = 100 * sub_idx / ipca_b,
    # (2) Razao mensalidade / salario minimo (acessibilidade p/ base da distr.)
    idx_vs_sm = 100 * sub_idx / sm_b
  )

# --- Sanidade ------------------------------------------------------------------
cat("\nSANIDADE:\n")
# (a) 1737 vs deflateBR no periodo 2020-2024 (devem coincidir)
f_1737 <- serie$ipca_idx[serie$data == as.Date("2024-01-01")] /
          serie$ipca_idx[serie$data == as.Date("2020-01-01")]
f_defl <- deflateBR::deflate(1, as.Date("2020-01-01"), "01/2024", "ipca")
cat(sprintf("  IPCA 2020-01 -> 2024-01: 1737 = %.4f | deflateBR = %.4f (dif %.2f%%)\n",
            f_1737, f_defl, 100 * abs(f_1737 / f_defl - 1)))
# (b) reajuste acumulado 12m do subitem em anos selecionados
acum12 <- sub_var |>
  mutate(ano = year(data)) |>
  group_by(ano) |>
  summarise(reajuste_ano = round((prod(1 + valor / 100) - 1) * 100, 2),
            n = n(), .groups = "drop") |>
  filter(n == 12)
cat("  Reajuste anual do subitem (amostra):\n")
print(acum12 |> filter(ano %in% c(2000, 2005, 2010, 2015, 2019, 2021, 2024)),
      n = Inf)
# (c) leituras-chave dos indices
cat("  Indice real (ago/1999=100) em marcos:\n")
print(serie |>
        filter(data %in% as.Date(c("1999-08-01", "2005-01-01", "2010-01-01",
                                    "2015-01-01", "2020-01-01", "2025-01-01"))) |>
        transmute(data = format(data, "%Y-%m"), idx_real = round(idx_real, 1),
                  idx_vs_sm = round(idx_vs_sm, 1)) |>
        as.data.frame(), row.names = FALSE)

# Exporta a serie mensal para consumo por outros scripts (ex.: fardo
# mensalidade/renda por decil em 2026-06_Harmonizing-BR-Data/R/02_validation)
saveRDS(serie, file.path(CACHE_DIR, "serie_mensalidade_mensal.rds"))
cat("  Serie mensal exportada para output/serie_mensalidade_mensal.rds\n")

# ==============================================================================
# 4. FIGURAS (rascunhos em graphs/ — promocao via finalizar_figura depois)
# ==============================================================================
cat("\n4. Gerando rascunhos...\n")

# ── FIG 1 (principal): indice real + razao vs SM, ago/1999-presente ──────────
df_fig1 <- serie |>
  select(data, `Real tuition index (deflated by headline IPCA)` = idx_real,
         `Tuition relative to the minimum wage` = idx_vs_sm) |>
  pivot_longer(-data, names_to = "serie_lbl", values_to = "valor")

cores_fig1 <- c(
  "Real tuition index (deflated by headline IPCA)" = "#0072B2",
  "Tuition relative to the minimum wage"           = "#D55E00"
)

lbl_fim <- df_fig1 |> group_by(serie_lbl) |> filter(data == max(data)) |> ungroup()

p1 <- ggplot(df_fig1, aes(x = data, y = valor, colour = serie_lbl)) +
  geom_hline(yintercept = 100, colour = "grey60", linewidth = 0.35,
             linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = cores_fig1, name = NULL) +
  # Grade de anos padronizada (WRITING-STYLE.md Sec 13.8) em vez da grade
  # manual de 5 em 5 anos usada ate 2026-07-06
  scale_x_anos_tese_date(datas = df_fig1$data,
                         expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(breaks = seq(0, 140, 20)) +
  labs(x = NULL, y = "Index (Aug 1999 = 100)") +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.02, 0.05),
    legend.justification   = c(0, 0),
    legend.background      = element_rect(fill = "white", colour = "grey85",
                                          linewidth = 0.3),
    legend.margin          = margin(2, 6, 3, 4),
    legend.text            = element_text(size = 7.5, family = THESIS_FONT),
    panel.grid.major.x     = element_blank()
  )

salvar_grafico(p1, prefixo = "010_IPCA_Mensalidade_Fig1_real_e_vs_SM",
               largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, formato = "pdf")

# ── FIG 2 (sensibilidade): 1991-1999 via "Cursos formais" + serie-mestre ─────
# A serie pre-ago/1999 e OUTRO subitem (Cursos formais: todos os niveis de
# ensino) — plotada como segmento distinto, reancorado para 100 na juncao
# (jul/1999), NUNCA emendada no indice-mestre. Hiperinflacao pre-1994:
# medicao mensal com caveats fortes.
LBL_PRE    <- "Formal courses, all levels (1991–1999 subitem)"
LBL_MASTER <- "Higher education tuition (Curso superior)"

pre_real <- pre_var |>
  select(data, var_mensal = valor) |>
  mutate(idx_bruto = cumprod(1 + var_mensal / 100)) |>
  left_join(ipca_geral, by = "data") |>
  mutate(
    real_bruto = idx_bruto / ipca_idx,
    idx_real   = 100 * real_bruto / real_bruto[dplyr::n()],
    serie_lbl  = LBL_PRE
  )

df_fig2 <- bind_rows(
  pre_real |> select(data, idx_real, serie_lbl),
  serie |> transmute(data, idx_real, serie_lbl = LBL_MASTER)
) |>
  mutate(serie_lbl = factor(serie_lbl, levels = c(LBL_MASTER, LBL_PRE)))

p2 <- ggplot(df_fig2,
             aes(x = data, y = idx_real, colour = serie_lbl,
                 group = serie_lbl)) +
  geom_hline(yintercept = 100, colour = "grey60", linewidth = 0.35,
             linetype = "dashed") +
  geom_vline(xintercept = MES_BASE, colour = "grey70", linewidth = 0.35,
             linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = setNames(c("#0072B2", "grey60"),
                                        c(LBL_MASTER, LBL_PRE)),
                      name = NULL) +
  scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2026-01-01"),
                            "5 years"),
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = NULL, y = "Real tuition index (Aug 1999 = 100)") +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.02, 0.96),
    legend.justification   = c(0, 1),
    legend.background      = element_rect(fill = "white", colour = "grey85",
                                          linewidth = 0.3),
    legend.margin          = margin(2, 6, 3, 4),
    legend.text            = element_text(size = 7.5, family = THESIS_FONT),
    panel.grid.major.x     = element_blank()
  )

salvar_grafico(p2, prefixo = "010_IPCA_Mensalidade_Fig2_sensibilidade_1991",
               largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, formato = "pdf")

# ── FIG 3 (secundaria): peso do subitem na cesta do IPCA ─────────────────────
if (nrow(sub_peso) > 0) {
  p3 <- ggplot(sub_peso, aes(x = data, y = valor)) +
    geom_line(linewidth = 0.7, colour = "#009E73") +
    scale_x_date(date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = "Weight in the IPCA basket (%)") +
    theme(panel.grid.major.x = element_blank())
  salvar_grafico(p3, prefixo = "010_IPCA_Mensalidade_Fig3_peso_cesta",
                 largura = LARGURA_TEXTO, altura = ALTURA_PADRAO, formato = "pdf")
}

# ==============================================================================
# 5. PROMOCAO — Fig 1 escolhida pelo autor (2026-07-04) para a dissertacao.
# Nota em LaTeX-pronto (formato fignote de finalizar_figura: \% , \texttt{},
# "--" para en-dash); fonte entra no final do fignote, nao no caption.
# ==============================================================================
# TRUE apenas na execucao de promocao (bundle 2026-07-06 gerado com fignote
# enxuta -- politica do plano fignotes/apendice; ver Extended Figure Notes
# no apendice para o detalhe metodologico completo removido daqui)
PROMOVER <- TRUE
if (PROMOVER) {
  nota_fig1 <- paste0(
    "Blue: tuition readjustment relative to headline consumer prices ",
    "(IPCA, Aug. 1999 = 100). Orange: the same tuition index relative to ",
    "the minimum wage. Spliced from four SIDRA subitem vintages; does not ",
    "capture compositional cheapening via distance learning."
  )

  finalizar_figura(
    plot        = p1,
    fig_label   = "ipca-mensalidade-curso-superior",
    fig_cap     = paste0(
      "Higher-education tuition relative to overall consumer prices and to ",
      "the minimum wage, Brazil, 1999–2026."
    ),
    nota        = nota_fig1,
    apendice    = "sec-fignote-ipca-mensalidade-curso-superior",
    fonte       = paste0(
      "IBGE --- IPCA subitem-level series via SIDRA (tables 655, 2938, ",
      "1419, 7060) and headline IPCA (table 1737); IPEADATA (minimum wage ",
      "series \\texttt{MTE12\\_SALMIN12})"
    ),
    script_path = file.path(BASE_DIR, "R", "010_IPCA_Curso_Superior_Series.R"),
    largura     = LARGURA_TEXTO,
    altura      = ALTURA_PADRAO
  )
}

cat("\n═════ 010_IPCA_Curso_Superior_Series concluido ═════\n")
