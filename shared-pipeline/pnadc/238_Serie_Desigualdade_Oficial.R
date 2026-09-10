# ==============================================================================
# SCRIPT: 238_Serie_Desigualdade_Oficial.R
#
# SERIE ANUAL DE DESIGUALDADE DE RENDA PELA METODOLOGIA OFICIAL, 1992-2024:
# Gini, razao de Palma e razao P90/P10 calculados sobre o universo que INCLUI
# domicilios de renda zero — como faz o IBGE, e ao contrario da decisao D02,
# que os exclui do parquet principal desta tese.
#
# POR QUE ESTA SERIE EXISTE (autor, 2026-08-08): o Gini e a estatistica que
# qualquer leitor compara de cabeca com o numero publicado. Excluir os zeros
# rebaixa o Gini em ~0,0050 e a Palma em ~0,127 sistematicamente, o que e
# suficiente para gerar pergunta de banca. Com os zeros, o Gini reproduz a
# serie oficial com desvio medio de -0,0001.
#
# 🚨 ESTA SERIE NAO SUBSTITUI O PARQUET PRINCIPAL, e nao deve ser usada para
# nada por decil. O 035_Splice_Microdados.R calcula os DECIS sobre a populacao
# com renda > 0; reprocessar o principal com zeros moveria as fronteiras de
# decil e quebraria a comparabilidade de todas as figuras por decil ja
# promovidas (041H, 245D, 042C, 042D, 231...). Por isso a serie e paralela e
# minima: so indices agregados. Dentro da figura 236, os paineis A e B passam
# a vir daqui (universo oficial) e o painel C continua vindo do parquet
# principal (universo D02) — inconsistencia DELIBERADA, cada painel no
# universo correto para o que mede, e declarada na nota da figura.
#
# FONTES (tres, ja disponiveis — nenhuma exige download):
#   1992-1999  output/PNAD_Anual_1992_1999.parquet          (ja preserva zeros)
#   2001-2015  output/PNAD_Anual_2001_2015_com_zero.parquet (gerado pelo 021)
#   2016-2024  5-data/pnadc_consolidado_2012_2025_interview1.rds (preserva zeros)
#
# EMENDA: a mesma da tese — PNAD Anual ate 2015, PNADC de 2016 em diante.
# O cache da PNADC cobre 2012-2024, entao 2012-2015 existem nas duas fontes;
# aqui, como no resto da tese, valem os da PNAD.
#
# NAO DEFLACIONA, de proposito: Gini, Palma e P90/P10 sao escala-invariantes
# dentro do ano, entao rodam sobre a renda NOMINAL. Isso mantem o deflator
# (D01) fora deste script e elimina uma fonte de erro.
#
# ATENCAO: renda nao declarada (999999+) ja foi excluida na importacao e NAO
# e o mesmo que renda zero.
#
# PLANO: 9-vers/plan/2026-08-08_Plano_Gini_Oficial_e_Gap_Pandemia.md (WP2)
# VER TAMBEM: 021 (importacao com zeros), 236 (figura que consome esta serie)
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(here); library(Hmisc)
})
options(scipen = 999)

BASE_DIR <- here::here("data-raw", "harmonizing-br-data")
OUT      <- file.path(BASE_DIR, "output")
PQ_90    <- file.path(OUT, "PNAD_Anual_1992_1999.parquet")
PQ_00    <- file.path(OUT, "PNAD_Anual_2001_2015_com_zero.parquet")
CACHE    <- file.path(here::here("data-raw"),
                       "pnadc_consolidado_2012_2025_interview1.rds")
ANO_SPLICE <- 2015L

# ------------------------------------------------------------------------------
# 1. Helpers — identicos aos do 236, para que as duas series sejam comparaveis
# ------------------------------------------------------------------------------
gini_w <- function(y, w) {
  ord <- order(y); y_s <- y[ord]; w_s <- w[ord]
  cw <- cumsum(w_s); n <- sum(w_s)
  sum(w_s * y_s * (2 * cw - w_s - n)) / (n * sum(w_s * y_s))
}

palma_w <- function(y, w) {
  ord <- order(y); y_s <- y[ord]; w_s <- w[ord]
  cw  <- cumsum(w_s) / sum(w_s)
  tot <- sum(w_s * y_s)
  bottom40 <- sum(w_s[cw <= 0.40] * y_s[cw <= 0.40]) / tot
  top10    <- sum(w_s[cw >  0.90] * y_s[cw >  0.90]) / tot
  top10 / bottom40
}

# P90/P10 e indefinido quando o P10 e zero — o que passa a acontecer assim que
# os zeros entram (se >10% da populacao tem renda zero, o decimo percentil e
# zero e a razao explode). Devolver NA em vez de Inf, e deixar o painel
# quebrar a linha, e mais honesto do que fabricar um numero.
razao_p90p10 <- function(y, w) {
  q <- Hmisc::wtd.quantile(y, weights = w, probs = c(0.10, 0.90),
                           normwt = FALSE, na.rm = TRUE)
  if (is.na(q[1]) || q[1] <= 0) return(NA_real_)
  as.numeric(q[2] / q[1])
}

indices <- function(df, rotulo) {
  df |>
    group_by(ano) |>
    summarise(
      gini   = gini_w(renda_dom_pcta, peso),
      palma  = palma_w(renda_dom_pcta, peso),
      p90p10 = razao_p90p10(renda_dom_pcta, peso),
      media_real = weighted.mean(renda_real, peso),
      pct_zero = 100 * sum(peso[renda_dom_pcta == 0]) / sum(peso),
      n        = n(),
      .groups = "drop"
    ) |>
    mutate(universo = rotulo)
}

# Deflacao identica a do 035: conversao cambial de 1992-93 (D01), data de
# referencia set/1 na PNAD e jun/15 na PNADC, IPCA para jan/2024.
deflacionar <- function(df, mes_ref) {
  df |>
    mutate(
      renda_corrigida = case_when(
        ano == 1992 ~ renda_dom_pcta / 2750000,  # Cr$  -> R$
        ano == 1993 ~ renda_dom_pcta / 2750,     # CR$  -> R$
        TRUE        ~ renda_dom_pcta
      ),
      ref_date   = as.Date(sprintf("%d-%s", ano, mes_ref)),
      renda_real = deflateBR::ipca(renda_corrigida, ref_date, "01/2024")
    ) |>
    select(-renda_corrigida, -ref_date)
}

# ------------------------------------------------------------------------------
# 2. Carregar as tres fontes
# ------------------------------------------------------------------------------
cat("Carregando PNAD 1992-1999...\n")
p90 <- read_parquet(PQ_90, col_select = c("ano", "peso", "renda_dom_pcta")) |>
  filter(!is.na(renda_dom_pcta), !is.na(peso), peso > 0)

cat("Carregando PNAD 2001-2015 (com zeros)...\n")
p00 <- read_parquet(PQ_00, col_select = c("ano", "peso", "renda_dom_pcta")) |>
  filter(!is.na(renda_dom_pcta), !is.na(peso), peso > 0)

cat("Carregando cache PNADC...\n")
pc <- readRDS(CACHE)
names(pc) <- tolower(names(pc))
pc <- pc |>
  select(ano, peso, renda_dom_pcta) |>
  filter(!is.na(renda_dom_pcta), !is.na(peso), peso > 0, ano > ANO_SPLICE)

cat("Deflacionando para jan/2024 (IPCA)...\n")
todas <- bind_rows(
  deflacionar(bind_rows(p90, p00), "09-01"),  # PNAD  — referencia setembro
  deflacionar(pc,                  "06-15")   # PNADC — referencia junho
) |> arrange(ano)

cat(sprintf("Anos na serie propria: %s\n",
            paste(sort(unique(todas$ano)), collapse = ", ")))

# ------------------------------------------------------------------------------
# 3. Series: universo oficial (com zeros) e universo D02 (sem), para medir o gap
# ------------------------------------------------------------------------------
cat("Calculando indices (com e sem zeros)...\n")
serie_oficial <- indices(todas, "oficial (com renda zero)")
serie_d02     <- indices(todas |> filter(renda_dom_pcta > 0), "D02 (sem renda zero)")

serie <- bind_rows(serie_oficial, serie_d02)

efeito <- serie_oficial |>
  select(ano, gini_com = gini, palma_com = palma, pct_zero) |>
  left_join(serie_d02 |> select(ano, gini_sem = gini, palma_sem = palma),
            by = "ano") |>
  mutate(d_gini = gini_com - gini_sem, d_palma = palma_com - palma_sem)

cat("\n══ EFEITO DE INCLUIR OS ZEROS, por ano ══\n")
print(as.data.frame(efeito |>
        mutate(across(c(gini_com, gini_sem, d_gini), ~ round(.x, 4)),
               across(c(palma_com, palma_sem, d_palma, pct_zero), ~ round(.x, 3))) |>
        select(ano, pct_zero, gini_sem, gini_com, d_gini, palma_sem, palma_com, d_palma)),
      row.names = FALSE)

cat(sprintf("\nEfeito medio: Gini %+.4f | Palma %+.3f\n",
            mean(efeito$d_gini), mean(efeito$d_palma)))

# ------------------------------------------------------------------------------
# 4. Conferencia contra o IBGE publicado (PNADC, rendimento de todas as fontes)
# ------------------------------------------------------------------------------
oficial_ibge <- tibble::tribble(
  ~ano, ~ibge,
  2016L, 0.537, 2017L, 0.540, 2018L, 0.545, 2019L, 0.544,
  2022L, 0.518, 2023L, 0.516
)

conf <- oficial_ibge |>
  left_join(serie_oficial |> select(ano, gini), by = "ano") |>
  mutate(desvio = gini - ibge)

cat("\n══ CONFERENCIA vs IBGE publicado (anos de PNADC na serie emendada) ══\n")
print(as.data.frame(conf |> mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)
cat(sprintf("Desvio medio vs IBGE: %+.4f\n", mean(conf$desvio, na.rm = TRUE)))

# ------------------------------------------------------------------------------
# 5. Sanidade do P90/P10 sob o universo oficial
# ------------------------------------------------------------------------------
n_na <- sum(is.na(serie_oficial$p90p10))
cat(sprintf("\nP90/P10 indefinido (P10 = 0) em %d de %d anos.\n",
            n_na, nrow(serie_oficial)))
if (n_na > 0) {
  cat("Anos afetados:",
      paste(serie_oficial$ano[is.na(serie_oficial$p90p10)], collapse = ", "), "\n")
  cat("=> Sob o universo oficial, use Palma (robusta a zeros), nao P90/P10.\n")
}

# ------------------------------------------------------------------------------
# 6. ENXERTO DE 2020-2021 (WP3) — PNADC anual RETROPONDERADA, via Salata et al.
#
# O IBGE nao publicou PNADC_2020/2021_visita1 no formato padrao (D11, razao 1);
# o que existe sao as bases anuais retroponderadas. Elas estao disponiveis
# localmente na replicacao de Salata et al. (2025), que as usa via DataZoom.
#
# 🚨 CAVEAT DE MODO, que a nota da figura DEVE declarar: em 2020 a coleta
# passou de presencial para telefonica (CATI), o que subenumera a renda
# domiciliar e exclui domicilios sem telefone — vies contra pobres e rurais.
# Esta e a razao 2 do D11, e ela e ESPECIFICAMENTE sobre renda. O autor
# decidiu incluir 2020-2021 assim mesmo (2026-08-08); a contrapartida
# acordada e que os dois pontos sejam VISUALMENTE DISTINGUIDOS nas figuras,
# nunca plotados como se fossem observacoes de mesma qualidade.
#
# COMPARABILIDADE VERIFICADA antes de enxertar: calculando Gini e Palma na
# base do Salata para todos os anos em comum, o desvio contra a nossa serie
# nos anos VIZINHOS do gap (2018, 2019, 2022) e de -0,0018 no Gini e -0,033 na
# Palma — menor que o efeito da propria emenda PNAD->PNADC (+0,010). E os
# valores de 2020 (0,5236) e 2021 (0,5443) reproduzem exatamente o Gini
# oficial do IBGE para esses anos. O enxerto, portanto, nao introduz degrau.
# ------------------------------------------------------------------------------
cat("\nEnxertando 2020-2021 (PNADC visita 5, microdado do IBGE)...\n")
PQ_V5 <- file.path(OUT, "PNADC_Visita5_2019_2022.parquet")

if (file.exists(PQ_V5)) {
  gap <- read_parquet(PQ_V5) |>
    filter(ano %in% c(2020L, 2021L), !is.na(renda_dom_pcta)) |>
    deflacionar("06-15")

  serie_gap <- indices(gap, "oficial (com renda zero)") |>
    mutate(retroponderado = TRUE)

  # FONTE (trocada em 2026-08-09): antes estes dois anos vinham da base de
  # Salata et al. (2025) e so podiam alimentar indices, porque a renda dele ja
  # vinha deflacionada para uma base nao declarada. Agora vem do microdado do
  # IBGE (visita 5, script 022), com valor NOMINAL e o nosso deflator — o que
  # permite tambem a RENDA MEDIA. Confirmacao de que as duas fontes eram a
  # mesma coisa: as contagens batem exatamente (2020 = 355.436, 2021 = 335.100,
  # 2022 = 380.928), ou seja, Salata usava visita 5.
  #
  # EFEITO DE VISITA, medido em 2019 e 2022, anos em que as duas visitas
  # coexistem: a visita 5 devolve Gini -0,0042, Palma -0,078 e renda media
  # -4,95% em relacao a visita 1. NAO aplicamos correcao — o sinal e
  # consistente mas a magnitude varia demais entre os dois anos (-3,75% e
  # -6,15%) para justificar um fator unico, e inventar um seria pior do que
  # declarar o vies. Vale notar que o Gini oficial do IBGE para 2020 (0,524) e
  # 2021 (0,544) coincide com o que a visita 5 devolve aqui: a serie oficial
  # herda a MESMA descontinuidade de visita, entao nao estamos introduzindo um
  # problema que a referencia nao tenha.

  serie_final <- bind_rows(
    serie_oficial |> mutate(retroponderado = FALSE),
    serie_gap
  ) |> arrange(ano)

  cat("\n══ 2020-2021 ENXERTADOS (visita 5, indices E renda media) ══\n")
  print(as.data.frame(serie_gap |>
          select(ano, gini, palma, p90p10, media_real, pct_zero) |>
          mutate(across(c(gini), ~ round(.x, 4)),
                 across(c(palma, p90p10, pct_zero), ~ round(.x, 3)),
                 media_real = round(media_real, 0))), row.names = FALSE)
  cat("Conferencia Gini: IBGE publica 0,524 (2020) e 0,544 (2021).\n")
  cat("Conferencia renda: Souza & Hecksher (2026) reportam ~1.730 (2020) e 1.610 (2021).\n")
} else {
  cat("AVISO: parquet da visita 5 nao encontrado — rode antes o script 022.\n")
  serie_final <- serie_oficial |> mutate(retroponderado = FALSE)
}

saveRDS(list(oficial = serie_final, d02 = serie_d02),
        file.path(OUT, "238_serie_desigualdade_oficial.rds"))

cat("\n══ SERIE OFICIAL FINAL (1992-2024) ══\n")
print(as.data.frame(serie_final |>
        select(ano, gini, palma, p90p10, media_real, retroponderado) |>
        mutate(across(c(gini), ~ round(.x, 4)),
               across(c(palma, p90p10), ~ round(.x, 3)),
               media_real = round(media_real, 0))), row.names = FALSE)

cat("\n═════ 238_Serie_Desigualdade_Oficial concluido ═════\n")
