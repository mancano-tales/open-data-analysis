# ==============================================================================
# ODA: portado de 4-DA-Code/2026-08_Replicacao_Senkevics2024/ em 2026-09-20; saidas em
#      data-raw/sedap/senkevics/coorte_<ano>/. Exige SEDAP_TOKEN (Tier B).
# 025_Pontos_Corte.R
# ==============================================================================
# Calcula os pontos de corte dos decis de renda e de desempenho a partir das
# distribuicoes extraidas em 020, reproduzindo a aritmetica dos autores:
#   renda = real_SM * (pm_sm / qtde_hab)          (1_DataPrep_and_Model.R:40)
#   income_D = cut(renda, quantile(renda, seq(0,1,.1)), ...)        (linha 51)
#   performance = cut(desempenho, quantile(...), ...)               (linha 66)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(deflateBR)
})

source(here::here("shared-pipeline", "sedap", "senkevics", "015_Especificacao.R"))

# ------------------------------------------------------------------------------
# 1. Base da faixa de renda, deflacionada (INPC -> 03/2023)
# ------------------------------------------------------------------------------
# Deflacionamos a base declarada NO QUESTIONARIO de cada edicao, nao o salario
# minimo legal — ver a nota em 015_Especificacao.R sobre 2022, em que o ENEM
# manteve as faixas de 2021. Referencia de maio de cada ano, como nos autores.
anos <- ANOS_ENEM
base_nominal <- unname(BASE_FAIXA_RENDA[as.character(anos)])
stopifnot(!any(is.na(base_nominal)))

base_real <- inpc(
  nominal_values = base_nominal,
  nominal_dates  = as.Date(sprintf("%d-05-01", anos)),
  real_date      = INPC_DATA_ALVO
)
tab_sm <- tibble(ano_enem = anos, base_nominal = base_nominal, sm_real = base_real)

cat("=== Base da faixa de renda, nominal e deflacionada (INPC -> 03/2023) ===
")
print(tab_sm)
write_csv(tab_sm, file.path(DIR_DADOS, "025_base_faixa_real.csv"))

# ------------------------------------------------------------------------------
# 2. Decis de renda familiar per capita
# ------------------------------------------------------------------------------
# faixa_renda vem como letra (A-Q) e qtde_hab como inteiro. Forcamos o tipo de
# leitura: sem isso, o readr converteria "1D"-like em numero (a armadilha que
# quebrou a tentativa anterior).

renda <- read_csv(
  file.path(DIR_DADOS, "020_dist_renda_moradores.csv"),
  col_types = cols(
    faixa_renda = col_character(), qtde_hab = col_integer(),
    N = col_double(), ano_enem = col_integer()
  )
) %>%
  filter(
    !is.na(faixa_renda), faixa_renda %in% names(PM_SM),
    !is.na(qtde_hab), qtde_hab >= 1
  ) %>%
  left_join(tab_sm, by = "ano_enem") %>%
  mutate(
    pm_sm = PM_SM[faixa_renda],
    rfpc  = sm_real * (pm_sm / qtde_hab)
  )

stopifnot(!any(is.na(renda$rfpc)))

# Quantil ponderado sobre a distribuicao discreta. Equivale a quantile() sobre o
# microdado individual: a renda so assume 17 x 20 valores por edicao, entao a
# distribuicao aqui e exata, nao uma aproximacao por binagem.
quantil_ponderado <- function(valor, peso, probs) {
  o <- order(valor)
  v <- valor[o]
  w <- peso[o]
  cum <- cumsum(w) / sum(w)
  vapply(probs, function(p) v[which(cum >= p)[1]], numeric(1))
}

cortes_renda <- quantil_ponderado(renda$rfpc, renda$N, seq(0, 1, 0.1))
cortes_renda[1] <- -Inf
cortes_renda[11] <- Inf

cat("\n=== Pontos de corte dos decis de renda (R$ de 03/2023, per capita) ===\n")
print(round(cortes_renda, 2))

# Diagnostico: a renda e MUITO discreta (pm_sm/qtde_hab), entao decis exatos de
# 10% sao impossiveis. Medimos o desvio em vez de escondê-lo.
renda_check <- renda %>%
  mutate(dec = cut(rfpc, cortes_renda, labels = paste0(1:10, "D"), include.lowest = TRUE)) %>%
  count(dec, wt = N) %>%
  mutate(share = n / sum(n))
cat("\nDistribuicao realizada dos decis de renda:\n")
print(as.data.frame(renda_check %>% mutate(share = sprintf("%.1f%%", 100 * share))))

# ------------------------------------------------------------------------------
# 3. Decis de desempenho
# ------------------------------------------------------------------------------
nota <- read_csv(
  file.path(DIR_DADOS, "020_dist_nota.csv"),
  col_types = cols(bin_nota = col_double(), N = col_double(), ano_enem = col_integer())
) %>%
  filter(!is.na(bin_nota), bin_nota > 0) %>%
  # bin_nota = FLOOR(media); o valor tipico dentro do bin e o ponto medio.
  mutate(nota = bin_nota + 0.5)

cortes_nota <- quantil_ponderado(nota$nota, nota$N, seq(0, 1, 0.1))
cortes_nota[1] <- -Inf
cortes_nota[11] <- Inf

cat("\n=== Pontos de corte dos decis de desempenho (media das 4 objetivas) ===\n")
print(round(cortes_nota, 1))

nota_check <- nota %>%
  mutate(dec = cut(nota, cortes_nota, labels = paste0(1:10, "D"), include.lowest = TRUE)) %>%
  count(dec, wt = N) %>%
  mutate(share = n / sum(n))
cat("\nDistribuicao realizada dos decis de desempenho:\n")
print(as.data.frame(nota_check %>% mutate(share = sprintf("%.1f%%", 100 * share))))

cat("\nN total da amostra analitica:", format(sum(nota$N), big.mark = " "), "\n")
if (COORTE_ANO == 2012) cat("Referencia do artigo (coorte 2012): 1 133 027
")

# ------------------------------------------------------------------------------
# 4. Gravar
# ------------------------------------------------------------------------------
write_csv(
  tibble(decil = 1:11, corte_renda = cortes_renda, corte_nota = cortes_nota),
  file.path(DIR_DADOS, "025_pontos_corte.csv")
)

cat("\n[OK] 025 concluido.\n")
