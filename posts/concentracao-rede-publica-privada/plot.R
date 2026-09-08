# ==============================================================================
# SCRIPT: 041_Curva_Concentracao_Rede_Publica_Privada.R
#
# OBJETIVO: Curvas de concentração de renda do acesso ao ensino superior
#           (18-24 anos), comparando: (1) acesso geral, (2) acesso à rede
#           PÚBLICA, (3) acesso à rede PRIVADA. Mostra qual dos dois setores
#           é mais concentrado entre os mais ricos -- pergunta de pesquisa
#           nova, nenhum script do projeto ainda cruzava rede x decil/renda.
#
# ACHADO CRÍTICO (verificado nesta sessão, 2026-06-30):
#   A variável `rede_ens` já existente no pipeline do projeto (cache
#   5-data/pnadc_consolidado_2012_2024_interview1.rds, gerada por
#   000_PNADC_Download_Consolidate.R a partir de V3004) NUNCA cobre
#   estudantes do ensino superior, em nenhum ano -- confirmado empiricamente:
#   das 428.231 observações com ens_sup_a==1 entre 2012-2017 (única janela em
#   que V3004 existe no cache), 100% têm rede_ens = NA. V3004, na estrutura
#   real do questionário PNADC, só é preenchida para quem frequenta níveis
#   básicos (creche a fundamental) -- nunca para quem frequenta superior.
#
#   A variável CORRETA para rede pública/privada no ensino superior é
#   V3002A, que substituiu V3004 no questionário revisado do IBGE. Verificado
#   via PNADcIBGE::get_pnadc() ano a ano: V3002A cobre 100% dos matriculados
#   atuais no superior (V3002==1 & V3003A %in% 8:11) em TODOS os anos de 2015
#   a 2024 (117/117 em 2015, 625/625 em 2016, ..., 591/591 em 2024). Não
#   existe nos anos 2012-2014 (nem sob esse nome nem um equivalente cobrindo
#   o superior -- testado diretamente: V3004 em 2012-2014 também tem 0/0
#   sobreposição com matriculados no superior).
#
#   Logo, ao contrário do que constava no CLAUDE.md (que descrevia rede_ens
#   como disponível 2012-2017 e "removida pelo IBGE em 2018"), a variável
#   CORRETA está disponível de forma CONTÍNUA por 10 anos (2015-2024) --
#   mais longa do que se pensava, não mais curta. Consistente com Salata
#   et al. (2025, docs/references/2026-06-10_Salata_et_al_2025_Origem_Social.md,
#   nota de rodapé 13), que reportam informação de rede de ensino frequentada
#   disponível na PNAD/PNADC de 2001 a 2022 para o indicador DEP4 do artigo
#   (estar matriculado em instituição pública, condicional a ter ingressado
#   no superior).
#
#   GOTCHA DE CODIFICAÇÃO: V3002A usa códigos INVERTIDOS em relação à
#   convenção documentada para V3004 em 000_PNADC_Download_Consolidate.R
#   (lá: 1=Pública, 2=Privada). Para V3002A, verificado diretamente via
#   get_pnadc(labels=TRUE) cruzado com get_pnadc(labels=FALSE) no mesmo
#   pull: 1 = Rede PRIVADA, 2 = Rede PÚBLICA. Não reaproveitar a constante
#   de V3004 sem inverter o sinal.
#
# DECISÃO DE ESCOPO (após discussão com o autor, 2026-06-30):
#   Extração standalone neste script via PNADcIBGE::get_pnadc() -- os anos
#   já estão em cache local de download em 5-data/pnadc_raw/ (baixados pelo
#   000_PNADC_Download_Consolidate.R), então a releitura é rápida e não
#   requer nova conexão com o IBGE. Este script NÃO modifica o pipeline/
#   cache compartilhado (000/001_PNADC_*.R) para não arriscar quebrar outras
#   análises que já dependem do cache atual. Uma eventual correção do
#   pipeline compartilhado (adicionar V3002A ao lado de V3004, documentar a
#   inversão de código, e então versões futuras de rede_ens cobrindo
#   também o superior) fica para decisão futura do autor -- fora do escopo
#   desta figura exploratória.
#
# ANOS USADOS: 2022, 2023, 2024 (agrupados / "pooled"). Justificativa: os
#   anos de transição do questionário (2015-2017) têm amostras de
#   matriculados no superior muito pequenas (117 em 2015, 625 em 2016) para
#   uma curva de concentração estável quando subdividida em público/
#   privado; os 3 anos mais recentes têm amostras maiores e estáveis
#   (~590-760 matriculados/ano, ~190-250 rede pública, ~400-520 rede
#   privada -- ver contagens no console ao rodar) e descrevem o cenário de
#   financiamento mais atual (pós-pandemia, FIES/ProUni em configuração
#   recente). Uma versão com toda a série 2015-2024 ano a ano é uma extensão
#   natural se o autor quiser ver a evolução temporal -- não construída
#   aqui por estar fora do escopo pedido (comparação setorial, não série
#   histórica).
#
# METODOLOGIA: curva de concentração reamostrada em grade percentual
#   uniforme (101 pontos), réplica da lógica de
#   040_Curvas_Concentracao_PNADC_Salata.R (calc_curva_concentracao()).
#   Curva "pooled" = média simples das 3 curvas anuais ponto a ponto na
#   grade (cada ano pesa igual, independente do tamanho amostral; o
#   ranking de renda usado em cada curva anual já é intra-ano, consistente
#   com a convenção D18 do projeto de decis/vintis sempre calculados
#   dentro do ano).
#   Índice de Erreygers (E = 4*mu*CI) calculado ano a ano com a mesma
#   fórmula de 020_PNADC_Indices_Concentracao.R (weighted_frac_rank() +
#   lógica de calc_ci_indices()), reportado no console e no bloco de nota.
#   População de referência (ranking de renda e denominador do indicador):
#   todos os 18-24 anos com renda domiciliar per capita > 0, consistente
#   com a convenção D02/D08 do projeto (harmonization_decisions.md).
# ==============================================================================

suppressPackageStartupMessages({
  library(PNADcIBGE); library(dplyr); library(ggplot2); library(here)
})
source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

SAVE_DIR <- "C:/Users/Mancano/Documents/MancanoSync/5-data/pnadc_raw"
ANOS     <- c(2022L, 2023L, 2024L)
N_PONTOS <- 101L

# ==============================================================================
# 1. EXTRAÇÃO (standalone -- NÃO usa o cache 5-data/pnadc_consolidado_*.rds;
#    ver decisão de escopo no cabeçalho)
# ==============================================================================
extrair_ano <- function(ano) {
  cat(sprintf("Lendo PNADC %d (cache local em 5-data/pnadc_raw/)...\n", ano))
  d <- get_pnadc(
    year = ano, interview = 1,
    vars = c("V2009", "V3002", "V3003A", "V3002A", "VD5008"),
    labels = FALSE, deflator = FALSE, design = FALSE,
    reload = FALSE, savedir = SAVE_DIR
  )
  peso_col <- intersect(c("V1032", "V1027", "V1028"), names(d))[1]
  stopifnot(!is.na(peso_col))

  d %>%
    transmute(
      ano            = ano,
      idade          = as.numeric(V2009),
      peso           = as.numeric(.data[[peso_col]]),
      renda_dom_pcta = as.numeric(VD5008),
      # ens_sup_a: mesma fórmula de 000_PNADC_Download_Consolidate.R
      # (V3002==1 frequenta & V3003A 8:11 = curso superior atual)
      ens_sup_a = as.integer(
        !is.na(V3002) & as.numeric(V3002) == 1 &
        !is.na(V3003A) & as.numeric(V3003A) %in% 8:11
      ),
      # V3002A: 1 = Rede PRIVADA, 2 = Rede PÚBLICA (verificado nesta sessão
      # via get_pnadc(labels=TRUE) x labels=FALSE -- INVERTIDO em relação
      # à convenção de V3004 documentada em 000_PNADC_Download_Consolidate.R)
      rede_pub  = as.integer(ens_sup_a == 1L & as.numeric(V3002A) == 2),
      rede_priv = as.integer(ens_sup_a == 1L & as.numeric(V3002A) == 1)
    ) %>%
    filter(idade >= 18, idade <= 24,
           !is.na(renda_dom_pcta), renda_dom_pcta > 0,
           !is.na(peso), peso > 0)
}

dados <- bind_rows(lapply(ANOS, extrair_ano))
cat(sprintf("\nTotal: %s observações 18-24 anos | %s matriculados no superior\n",
            format(nrow(dados), big.mark = "."),
            format(sum(dados$ens_sup_a), big.mark = ".")))
cat(sprintf("  rede pública: %s | rede privada: %s\n",
            format(sum(dados$rede_pub), big.mark = "."),
            format(sum(dados$rede_priv), big.mark = ".")))

# ==============================================================================
# 2. CURVA DE CONCENTRAÇÃO (grade percentual uniforme, 101 pontos)
# Lógica idêntica a calc_curva_concentracao() em
# 040_Curvas_Concentracao_PNADC_Salata.R
# ==============================================================================
calc_curva <- function(df, y_col, n_pontos = N_PONTOS) {
  df <- df %>% arrange(renda_dom_pcta)
  total_w   <- sum(df$peso)
  total_y_w <- sum(df$peso * df[[y_col]])
  if (total_y_w <= 0) return(NULL)

  cum_pop     <- cumsum(df$peso) / total_w
  cum_outcome <- cumsum(df$peso * df[[y_col]]) / total_y_w

  grid_p <- seq(0, 1, length.out = n_pontos)
  grid_L <- approx(x = c(0, cum_pop), y = c(0, cum_outcome),
                    xout = grid_p, rule = 2)$y
  data.frame(p = grid_p, L = grid_L)
}

SERIES <- c(ens_sup_a = "Overall", rede_pub = "Public network", rede_priv = "Private network")

curvas_por_ano <- lapply(ANOS, function(a) {
  df_a <- filter(dados, ano == a)
  lapply(names(SERIES), function(col) {
    curva <- calc_curva(df_a, col)
    if (is.null(curva)) return(NULL)
    curva$serie <- SERIES[[col]]
    curva$ano   <- a
    curva
  }) %>% bind_rows()
}) %>% bind_rows()

# Curva combinada: média simples das curvas anuais ponto a ponto na grade
# (cada ano pesa igual; ver justificativa no cabeçalho)
curva_pooled <- curvas_por_ano %>%
  group_by(serie, p) %>%
  summarise(L = mean(L), .groups = "drop") %>%
  mutate(serie = factor(serie, levels = c("Overall", "Public network", "Private network")))

# ==============================================================================
# 3. ÍNDICE DE ERREYGERS (ano a ano) -- mesma fórmula de
#    020_PNADC_Indices_Concentracao.R (weighted_frac_rank + CI/Wagstaff/Erreygers)
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

tab_E <- lapply(ANOS, function(a) {
  df_a <- filter(dados, ano == a)
  data.frame(
    ano       = a,
    E_overall = calc_erreygers(df_a$ens_sup_a, df_a$peso, df_a$renda_dom_pcta),
    E_publica = calc_erreygers(df_a$rede_pub,  df_a$peso, df_a$renda_dom_pcta),
    E_privada = calc_erreygers(df_a$rede_priv, df_a$peso, df_a$renda_dom_pcta)
  )
}) %>% bind_rows()
cat("\nÍndice de Erreygers por ano (população de referência: 18-24 anos):\n")
print(tab_E %>% mutate(across(starts_with("E_"), \(x) round(x, 4))), row.names = FALSE)
E_medio <- colMeans(tab_E[, c("E_overall", "E_publica", "E_privada")])
cat(sprintf("\nMédia 2022-2024: geral=%.3f | pública=%.3f | privada=%.3f\n",
            E_medio["E_overall"], E_medio["E_publica"], E_medio["E_privada"]))

# ==============================================================================
# 4. GRÁFICO
# Comparação qualitativa de 3 categorias (geral/pública/privada) -- não é
# delta de ganho/perda, então scale_colour_thesis() (Okabe-Ito qualitativa)
# é a escala correta, não scale_colour_delta().
# ==============================================================================
p_curva <- ggplot(curva_pooled, aes(x = p, y = L, colour = serie)) +
  geom_abline(slope = 1, intercept = 0, colour = "#333333",
              linewidth = 0.5, linetype = "dashed") +
  geom_line(linewidth = 1.0) +
  scale_colour_thesis(name = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                      breaks = seq(0, 1, 0.25), expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                      breaks = seq(0, 1, 0.25), expand = expansion(mult = 0.01)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Cumulative population share (poorest → richest)",
    y = "Cumulative share of tertiary enrollment"
  ) +
  theme(legend.position = "bottom")

# ── LEGENDA / DRAFT PARA O QUARTO ────────────────────────────────────────────
# fig-cap: "Income concentration curves for tertiary education enrollment by
#   network (public vs. private), ages 18-24, Brazil 2022-2024 (pooled).
#   Source: IBGE -- PNAD Contínua via PNADcIBGE."
# fig-label: fig-concentracao-rede-publica-privada
# Nota metodológica (bloco "> **Note:**" no .qmd):
#   "Curves show, for each network, the cumulative share of currently
#   enrolled tertiary students accounted for by the poorest p% of the
#   18-24-year-old population (ranked by per capita household income,
#   within each year). The closer a curve sits to the 45-degree line, the
#   more evenly distributed access to that network is across the income
#   distribution; curves bowing below the diagonal indicate concentration
#   among higher-income deciles. Pooled curve = simple average of the
#   year-specific curves for 2022, 2023 and 2024 (each resampled on a
#   uniform 101-point percentile grid), so each year contributes equally
#   regardless of sample size. Erreygers concentration index, 2022-2024
#   average: see script console output for exact values (overall, public,
#   private network). Network variable: V3002A (IBGE PNADC), not the
#   V3004-derived rede_ens used elsewhere in this project -- V3004 never
#   covers tertiary students in any year; V3002A is its 2015+ replacement
#   and is the variable used here, with reversed coding (1 = private,
#   2 = public) relative to the V3004 convention. See script header for
#   the full discovery and verification log."
# Variáveis-chave: V3002A (rede, INVERTIDA vs V3004), V3003A (curso atual),
#   ens_sup_a, renda_dom_pcta
# Referência cruzada no texto: @fig-concentracao-rede-publica-privada
# Dimensões: LARGURA_2_3 × ALTURA_ALTA (4.20 × 5.00 in, 300 dpi)
# Paleta: PALETA_QUALITATIVA (scale_colour_thesis) -- 3 categorias sem ordem
# ─────────────────────────────────────────────────────────────────────────────

salvar_grafico(p_curva, prefixo = "041_Curva_Concentracao_Rede_Publica_Privada",
               largura = LARGURA_TEXTO, altura = ALTURA_ALTA, unidades = "in",
               formato = "pdf")

# ──────────────────────────────────────────────────────────────────────────
# PROMOÇÃO MANUAL PARA REVISÃO — pacote PDF + .R + .qmd em 6-images-tables/final/
# Figura exploratória, validada visualmente (conversão PDF→PNG via
# Ghostscript do TinyTeX, pdftools/magick desativados nesta máquina) em
# 2026-06-30. NÃO inserida em nenhum capítulo ainda -- aguardando revisão
# do autor (decisão de design: ano único vs. série completa 2015-2024;
# eventual correção do pipeline compartilhado para V3002A).
# ──────────────────────────────────────────────────────────────────────────
finalizar_figura(
  plot        = p_curva,
  fig_label   = "concentracao-rede-publica-privada",
  fig_cap     = "Income concentration in tertiary enrollment by network type, ages 18-24, Brazil, 2022-2024.",
  fonte       = "IBGE — PNAD Contínua via \\texttt{PNADcIBGE} (2022-2024)",
  nota        = paste0(
    "Curves show the cumulative share of currently-enrolled tertiary students accounted for by the poorest $p\\%$ of the 18--24 population, by network. A curve closer to the 45-degree line indicates access more evenly distributed across income; bowing below it indicates concentration among the rich. Pooled across 2022--2024."
  ),
  apendice    = "sec-fignote-concentracao-rede-publica-privada",
  script_path = here::here("4-DA-Code", "2026-05_PNADcIBGE",
                            "041_Curva_Concentracao_Rede_Publica_Privada.R"),
  largura = LARGURA_TEXTO, altura = ALTURA_ALTA, unidades = "in"
)

cat("\n══════ Script 041 concluído ══════\n")
