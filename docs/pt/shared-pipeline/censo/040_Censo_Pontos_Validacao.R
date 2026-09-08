# ==============================================================================
# SCRIPT: 040_Censo_Pontos_Validacao.R
#
# Diamantes de validação censitária para a série PNAD/PNADC 1992–2025.
# Censos incluídos: 1991, 2000, 2010 (via censobr, promovidos na tese).
#
# Censo 2022: RASCUNHO. Fonte: censobr::read_population()/read_households()
# (branch dev do pacote, apos censobr::import_microdata22_controlado() ter
# sido rodado uma vez -- ver vinheta "Working with 2022 microdata" do pacote
# e docs/harmonization_decisions.md D12, adendo 2026-09-03). Substitui o
# pipeline proprio usado em 2026-09-02 (01_pipeline/042_Ingest_Censo_2022.R +
# utils/censobr_compat.R, movidos para R/deprecated/ em 2026-09-03): esse
# pipeline reconstruia renda_dom_pcta manualmente duplicando D0360, que o
# IBGE ja entrega pronto -- ver docs/audits/2026-09-03_Auditoria_Ingestao_
# Censo_2022.md. A migracao foi validada cruzando os resultados dos dois
# pipelines (D1/D5/D9/D10/Nacional batem exatos ao decimo).
#
# Pinagem de reprodutibilidade: testado com censobr commit
# dd48725f6d8763874acd15d2eff222e2024470ee (2026-09-03), instalado via
# remotes::install_github("ipeaGIT/censobr"). A funcao ainda nao foi lancada
# no CRAN nem mergeada na main -- reverificar os numeros ao atualizar o
# pacote (o cache do censobr e versionado por "data release": uma nova
# versao pode nao encontrar os arquivos importados sob a anterior, exigindo
# rodar import_microdata22_controlado() de novo a partir do zip original do
# IBGE, guardado em 5-data/IBGE/Censo_2022_amostra/raw_zip/).
#
# Os pontos de 2022 vão só para Censo_Pontos_Validacao_1991_2022.parquet --
# NÃO promovido, NÃO referenciado em nenhuma figura final da tese. Antes de
# promover: reconferir a alegação de IC <= 0,6pp (herdada de 1991/2000/2010)
# especificamente para 2022 -- na versão anterior do pipeline o D10 de 2022
# já excedia esse limite (IC de 0,69pp; ver auditoria).
#
# Outputs:
#   output/Censo_Pontos_Validacao_1991_2022.parquet (rascunho, com 2022)
#   output/Censo_Pontos_Validacao_1991_2000_2010.parquet (legado -- consumido
#     por 041H_Tese_Decil_Lines_Censo.R / Figura 2.1; NUNCA inclui 2022)
#   graphs/HHMM_040A_Censo_Validacao_Nossa_Harmonizacao.png
#   graphs/HHMM_040B_Censo_Validacao_Salata_2025.png
# ==============================================================================

suppressPackageStartupMessages({
  library(censobr)
  library(arrow)
  library(dplyr)
  library(Hmisc)
  library(deflateBR)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(here)
})

options(scipen = 999)

BASE_DIR <- here::here("4-DA-Code", "2026-06_Harmonizing-BR-Data")
OUTPUT_DIR <- here::here("6-images-tables", "graphs")
PARQUET_PNAD <- file.path(BASE_DIR, "output", "Microdados_Jovens_18_24_1992_2025.parquet")
SALATA_PATH <- here::here("data-raw", "Salata-etal-2025", "Agregada.parquet")
DATE_PREFIX <- format(Sys.time(), "%Y-%m-%d_%H%M")

EXCLUIR_RURAL_NORTE <- TRUE
NORTE_RURAL_UF <- c(11L, 12L, 13L, 14L, 15L, 16L)
BASE_IPCA <- "01/2024"

cat("══════ 040_Censo_Pontos_Validacao.R (1991, 2000, 2010, 2022) ══════\n\n")

# ── Função: taxas de acesso por decil + nacional ──────────────────────────────
taxa_acesso <- function(df, ano_val) {
  by_decil <- df |>
    group_by(decil) |>
    summarise(
      prop = Hmisc::wtd.mean(ens_sup, weights = peso, na.rm = TRUE) * 100,
      n_eff = sum(peso)^2 / sum(peso^2),
      .groups = "drop"
    ) |>
    mutate(
      se = sqrt((prop / 100) * (1 - prop / 100) / pmax(n_eff, 1)) * 100,
      lower = pmax(prop - 1.96 * se, 0),
      upper = pmin(prop + 1.96 * se, 100),
      ano = ano_val, fonte = "Censo Demográfico (IBGE)"
    )
  nacional <- df |>
    summarise(
      prop  = Hmisc::wtd.mean(ens_sup, weights = peso, na.rm = TRUE) * 100,
      n_eff = sum(peso)^2 / sum(peso^2)
    ) |>
    mutate(
      se = sqrt((prop / 100) * (1 - prop / 100) / pmax(n_eff, 1)) * 100,
      lower = pmax(prop - 1.96 * se, 0),
      upper = pmin(prop + 1.96 * se, 100),
      decil = 0L, ano = ano_val, fonte = "Censo Demográfico (IBGE)"
    )
  bind_rows(by_decil, nacional)
}

# ==============================================================================
# 1. CENSO 1991
# ==============================================================================
cat("══ Censo 1991 ══\n")
cat("  → Carregando 1991 (Arrow cache)...\n")
df_1991_raw <- censobr::read_population(year = 1991)

geo_filt_1991 <- if (EXCLUIR_RURAL_NORTE) {
  quote(!(V1061 >= 4L & code_state %in% c(11L, 12L, 13L, 14L, 15L, 16L)))
} else {
  quote(TRUE)
}

cat("  → Calculando decil map via V3049 (rank-based CDF, corrige bins vazios)...\n")
pop_1991_dec <- df_1991_raw |>
  filter(!is.na(V3049), V3049 > 0L, !is.na(V7301), V7301 > 0) |>
  select(V3049, V7301, V1061, code_state) |>
  collect() |>
  filter(eval(geo_filt_1991)) |>
  mutate(peso = V7301 / 1e8, renda_ord = as.integer(V3049))

decil_map_1991 <- pop_1991_dec |>
  group_by(renda_ord) |>
  summarise(w = sum(peso), .groups = "drop") |>
  arrange(renda_ord) |>
  mutate(
    cum_w_lower = lag(cumsum(w), default = 0),
    cdf_mid     = (cum_w_lower + w / 2) / sum(w),
    decil_val   = as.integer(pmax(pmin(ceiling(cdf_mid * 10), 10L), 1L))
  ) |>
  select(renda_ord, decil_val)
rm(pop_1991_dec)
invisible(gc())

cat("  → Carregando jovens 18–24 (Arrow filter)...\n")
jovens_1991 <- df_1991_raw |>
  filter(V3072 >= 18L, V3072 <= 24L) |>
  collect() |>
  filter(eval(geo_filt_1991)) |>
  mutate(
    peso = V7301 / 1e8,
    idade = as.integer(V3072),
    renda_ord = as.integer(V3049)
  ) |>
  filter(!is.na(peso), peso > 0, !is.na(renda_ord), renda_ord > 0L) |>
  left_join(decil_map_1991, by = "renda_ord") |>
  mutate(
    decil = decil_val,
    ens_sup = as.integer(
      (V0325 == 3L) |
        (V0326 == 6L) |
        (V0328 %in% c(7L, 8L))
    )
  ) |>
  select(-decil_val) |>
  filter(!is.na(ens_sup), !is.na(decil))

cat(sprintf("  → %d jovens 18–24 (pós-filtros)\n", nrow(jovens_1991)))
cat(sprintf(
  "  → Taxa nacional ens_sup: %.1f%%\n",
  Hmisc::wtd.mean(jovens_1991$ens_sup, weights = jovens_1991$peso) * 100
))

taxa_1991 <- taxa_acesso(jovens_1991, 1991)
cat(sprintf(
  "  → D1=%.1f%% | D5=%.1f%% | D10=%.1f%% | Nac=%.1f%%\n",
  taxa_1991$prop[taxa_1991$decil == 1], taxa_1991$prop[taxa_1991$decil == 5],
  taxa_1991$prop[taxa_1991$decil == 10], taxa_1991$prop[taxa_1991$decil == 0]
))
rm(jovens_1991, df_1991_raw, decil_map_1991)
invisible(gc())

# ==============================================================================
# 2. CENSO 2000
# ==============================================================================
cat("\n══ Censo 2000 ══\n")

cat("  → Passo 1/3: renda per capita domiciliar (Arrow lazy)...\n")
hh_pcta_2000 <- censobr::read_population(
  year = 2000, columns = c("AREAP", "V0300", "V4614")
) |>
  mutate(V4614_num = as.numeric(V4614)) |>
  group_by(AREAP, V0300) |>
  summarise(renda_hh = sum(V4614_num, na.rm = TRUE), n_moradores = n(), .groups = "drop") |>
  collect() |>
  mutate(renda_dom_pcta = renda_hh / n_moradores)
cat(sprintf("  → %d domicílios\n", nrow(hh_pcta_2000)))

cat("  → Passo 2/3: decil breaks (todos indivíduos)...\n")
pop_2000_full <- censobr::read_population(
  year = 2000, columns = c("AREAP", "V0300", "P001", "V1006", "code_state")
) |>
  collect() |>
  mutate(peso = as.numeric(P001)) |>
  filter(!is.na(peso), peso > 0) |>
  left_join(hh_pcta_2000 |> select(AREAP, V0300, renda_dom_pcta), by = c("AREAP", "V0300")) |>
  filter(!is.na(renda_dom_pcta), renda_dom_pcta > 0)

if (EXCLUIR_RURAL_NORTE) {
  pop_2000_full <- pop_2000_full |>
    filter(!(V1006 == "2" & code_state %in% NORTE_RURAL_UF))
}
brks_2000 <- Hmisc::wtd.quantile(pop_2000_full$renda_dom_pcta,
  weights = pop_2000_full$peso,
  probs = seq(0, 1, 0.1), na.rm = TRUE
)
brks_2000[1] <- -Inf
brks_2000[11] <- Inf
rm(pop_2000_full)
invisible(gc())

cat("  → Passo 3/3: jovens 18–24...\n")
jovens_2000 <- censobr::read_population(
  year = 2000, columns = c("AREAP", "V0300", "P001", "V4752", "V0429", "V0430", "V0432", "V1006", "code_state")
) |>
  filter(as.integer(V4752) >= 18L, as.integer(V4752) <= 24L) |>
  collect() |>
  mutate(peso = as.numeric(P001), idade = as.integer(V4752)) |>
  filter(!is.na(peso), peso > 0) |>
  left_join(hh_pcta_2000, by = c("AREAP", "V0300")) |>
  filter(!is.na(renda_dom_pcta), renda_dom_pcta > 0)
rm(hh_pcta_2000)
invisible(gc())

if (EXCLUIR_RURAL_NORTE) {
  jovens_2000 <- jovens_2000 |>
    filter(!(V1006 == "2" & code_state %in% NORTE_RURAL_UF))
}

jovens_2000 <- jovens_2000 |>
  mutate(
    renda_real = deflateBR::ipca(renda_dom_pcta, as.Date("2000-08-01"), BASE_IPCA),
    decil = as.integer(cut(renda_dom_pcta, breaks = brks_2000, labels = 1:10, include.lowest = TRUE)),
    ens_sup = as.integer(
      (V0429 %in% c("1", "2") & V0430 %in% c("12", "13")) |
        (V0429 == "3" & V0432 %in% c("7", "8"))
    )
  ) |>
  filter(!is.na(ens_sup), !is.na(decil))

cat(sprintf(
  "  → %d jovens 18–24 | Nac=%.1f%%\n", nrow(jovens_2000),
  Hmisc::wtd.mean(jovens_2000$ens_sup, weights = jovens_2000$peso) * 100
))
taxa_2000 <- taxa_acesso(jovens_2000, 2000)
cat(sprintf(
  "  → D1=%.1f%% | D5=%.1f%% | D10=%.1f%% | Nac=%.1f%%\n",
  taxa_2000$prop[taxa_2000$decil == 1], taxa_2000$prop[taxa_2000$decil == 5],
  taxa_2000$prop[taxa_2000$decil == 10], taxa_2000$prop[taxa_2000$decil == 0]
))
rm(jovens_2000, brks_2000)
invisible(gc())

# ==============================================================================
# 3. CENSO 2010
# ==============================================================================
cat("\n══ Censo 2010 ══\n")

cat("  → Decil breaks (população completa)...\n")
pop_2010_full <- censobr::read_population(
  year = 2010, columns = c("V0010", "V6531", "V1006", "code_state")
) |>
  collect() |>
  mutate(peso = as.numeric(V0010), renda = as.numeric(V6531)) |>
  filter(!is.na(renda), renda > 0, !is.na(peso), peso > 0)

if (EXCLUIR_RURAL_NORTE) {
  pop_2010_full <- pop_2010_full |>
    filter(!(V1006 == "2" & code_state %in% NORTE_RURAL_UF))
}
brks_2010 <- Hmisc::wtd.quantile(pop_2010_full$renda,
  weights = pop_2010_full$peso,
  probs = seq(0, 1, 0.1), na.rm = TRUE
)
brks_2010[1] <- -Inf
brks_2010[11] <- Inf
rm(pop_2010_full)
invisible(gc())

cat("  → Jovens 18–24 (Arrow filter)...\n")
jovens_2010 <- censobr::read_population(
  year = 2010,
  columns = c("V0010", "V6531", "V6036", "V0628", "V0629", "V0633", "V1006", "code_state")
) |>
  filter(as.integer(V6036) >= 18L, as.integer(V6036) <= 24L) |>
  collect() |>
  mutate(peso = as.numeric(V0010), renda_nominal = as.numeric(V6531), idade = as.integer(V6036)) |>
  filter(!is.na(renda_nominal), renda_nominal > 0, !is.na(peso), peso > 0)

if (EXCLUIR_RURAL_NORTE) {
  jovens_2010 <- jovens_2010 |>
    filter(!(V1006 == "2" & code_state %in% NORTE_RURAL_UF))
}

jovens_2010 <- jovens_2010 |>
  mutate(
    renda_real = deflateBR::ipca(renda_nominal, as.Date("2010-07-01"), BASE_IPCA),
    decil = as.integer(cut(renda_nominal, breaks = brks_2010, labels = 1:10, include.lowest = TRUE)),
    ens_sup = as.integer(
      (V0628 %in% c("1", "2") & V0629 %in% c("09", "10", "11", "12")) |
        (V0628 == "3" & V0633 %in% c("11", "12", "13", "14"))
    )
  ) |>
  filter(!is.na(ens_sup), !is.na(decil))

cat(sprintf(
  "  → %d jovens 18–24 | Nac=%.1f%%\n", nrow(jovens_2010),
  Hmisc::wtd.mean(jovens_2010$ens_sup, weights = jovens_2010$peso) * 100
))
taxa_2010 <- taxa_acesso(jovens_2010, 2010)
cat(sprintf(
  "  → D1=%.1f%% | D5=%.1f%% | D10=%.1f%% | Nac=%.1f%%\n",
  taxa_2010$prop[taxa_2010$decil == 1], taxa_2010$prop[taxa_2010$decil == 5],
  taxa_2010$prop[taxa_2010$decil == 10], taxa_2010$prop[taxa_2010$decil == 0]
))
rm(jovens_2010, brks_2010)
invisible(gc())

# ==============================================================================
# 4. CENSO 2022 [RASCUNHO -- ver nota no cabecalho do script]
# ==============================================================================
#
# Fonte: censobr::read_population()/read_households(year=2022) (branch dev,
# apos import_microdata22_controlado() -- ver cabecalho do script). Ao
# contrario dos anos 1991/2000/2010, os codigos categoricos do schema
# oficial do Censo 2022 sao inteiro puro, SEM zero a esquerda (P0660==8L,
# nao "08") -- todas as comparacoes abaixo usam inteiro.
#
# ── Variáveis confirmadas (Dicionario_de_Variaveis_Microdados_CD2022.pdf, IBGE;
#    o PDF esta marcado como protegido mas o texto extrai normalmente via
#    pdftotext) ────────────────────────────────────────────────────────────
#   P0100/D0100  controle (ID do domicílio, liga Pessoas a Domicílios)
#   P0111  peso amostral (pessoas) | P0181  idade calculada em anos
#   D0140  situação do domicílio: 1=Urbana; 2=Rural
#   D0360  rendimento domiciliar per capita, OFICIAL do IBGE (já pré-calculado
#     excluindo pensionista/empregado doméstico/parente de empregado
#     doméstico) -- usado diretamente como renda_dom_pcta, sem reconstrução.
#   P0170  condição no domicílio: 17=pensionista, 18=empregado(a) doméstico(a),
#     19=parente do(a) empregado(a) doméstico(a), 20=individual em domicílio
#     coletivo (mesma exclusão que D0360 já aplica; replicada aqui a nível de
#     pessoa para não herdar a renda per capita do empregador/instituição).
#   P0130  espécie da unidade visitada: 6=domicílio coletivo com morador.
#   ens_sup: (P0650==1 frequenta agora & P0660 em {8,9,10,11} superior/pós)
#     | (P0650 em {2,3} não frequenta agora & P0700 em {12,13,14,15} curso
#     mais elevado já frequentado = superior/pós) -- P0700 é branco quando
#     P0650==3 (nunca frequentou), então o segundo ramo nunca dispara para
#     quem nunca estudou; verificado linha a linha contra o dicionário.
#
# Limitação em aberto: a amostra do Censo 2022 teve desenho e cobertura
# distintos dos censos 1991/2000/2010; a alegação "IC <= 0,6pp" do Data
# Codebook (herdada dos censos anteriores) NÃO foi reverificada para 2022 --
# checar os ICs impressos abaixo antes de estender essa alegação ao ano de
# 2022 em qualquer texto (na versão anterior do pipeline o D10 de 2022 já
# excedia esse limite, IC de 0,69pp; ver docs/audits/).
# ==============================================================================
cat("\n══ Censo 2022 (rascunho) ══\n")

CHAVE_DOM_2022 <- c("code_state", "code_muni", "code_weighting")

cat("  → Passo 1/3: renda per capita domiciliar (D0360 oficial)...\n")
hh_pcta_2022 <- censobr::read_households(
  year = 2022, columns = c(CHAVE_DOM_2022, "D0100", "D0140", "D0360")
) |>
  rename(renda_dom_pcta = D0360) |>
  collect()

cat("  → Passo 2/3: decil breaks (população completa, membros da família principal)...\n")
pop_2022_full <- censobr::read_population(
  year = 2022,
  columns = c(CHAVE_DOM_2022, "P0100", "P0111", "P0170", "P0130")
) |>
  collect() |>
  mutate(
    is_membro_principal = !(P0170 %in% c(17L, 18L, 19L, 20L)),
    is_dom_coletivo = (P0130 == 6L | P0170 == 20L)
  ) |>
  filter(is_membro_principal, !is_dom_coletivo, !is.na(P0111), P0111 > 0) |>
  left_join(hh_pcta_2022, by = c(CHAVE_DOM_2022, "P0100" = "D0100")) |>
  filter(!is.na(renda_dom_pcta), renda_dom_pcta > 0)

if (EXCLUIR_RURAL_NORTE) {
  pop_2022_full <- pop_2022_full |>
    filter(!(D0140 == 2L & code_state %in% NORTE_RURAL_UF))
}

brks_2022 <- Hmisc::wtd.quantile(
  pop_2022_full$renda_dom_pcta,
  weights = pop_2022_full$P0111,
  probs   = seq(0, 1, 0.1),
  na.rm   = TRUE
)
brks_2022[1] <- -Inf
brks_2022[11] <- Inf
rm(pop_2022_full)
invisible(gc())

cat("  → Passo 3/3: jovens 18–24...\n")
jovens_2022 <- censobr::read_population(
  year = 2022,
  columns = c(CHAVE_DOM_2022, "P0100", "P0111", "P0181", "P0170", "P0130", "P0650", "P0660", "P0700")
) |>
  filter(P0181 >= 18L, P0181 <= 24L) |>
  collect() |>
  mutate(
    is_membro_principal = !(P0170 %in% c(17L, 18L, 19L, 20L)),
    is_dom_coletivo = (P0130 == 6L | P0170 == 20L)
  ) |>
  filter(is_membro_principal, !is_dom_coletivo, !is.na(P0111), P0111 > 0) |>
  left_join(hh_pcta_2022, by = c(CHAVE_DOM_2022, "P0100" = "D0100")) |>
  filter(!is.na(renda_dom_pcta), renda_dom_pcta > 0)
rm(hh_pcta_2022)
invisible(gc())

if (EXCLUIR_RURAL_NORTE) {
  jovens_2022 <- jovens_2022 |>
    filter(!(D0140 == 2L & code_state %in% NORTE_RURAL_UF))
}

jovens_2022 <- jovens_2022 |>
  mutate(
    peso  = as.numeric(P0111),
    idade = as.integer(P0181),
    decil = as.integer(cut(renda_dom_pcta, breaks = brks_2022, labels = 1:10, include.lowest = TRUE)),
    ens_sup = as.integer(
      (P0650 == 1L & P0660 %in% c(8L, 9L, 10L, 11L)) |
        (P0650 %in% c(2L, 3L) & P0700 %in% c(12L, 13L, 14L, 15L))
    )
  ) |>
  filter(!is.na(ens_sup), !is.na(decil))

cat(sprintf(
  "  → %d jovens 18–24 | Nac=%.1f%%\n", nrow(jovens_2022),
  Hmisc::wtd.mean(jovens_2022$ens_sup, weights = jovens_2022$peso) * 100
))
taxa_2022 <- taxa_acesso(jovens_2022, 2022)
cat(sprintf(
  "  → D1=%.1f%% | D5=%.1f%% | D10=%.1f%% | Nac=%.1f%%\n",
  taxa_2022$prop[taxa_2022$decil == 1], taxa_2022$prop[taxa_2022$decil == 5],
  taxa_2022$prop[taxa_2022$decil == 10], taxa_2022$prop[taxa_2022$decil == 0]
))
rm(jovens_2022, brks_2022)
invisible(gc())

# ==============================================================================
# 5. COMBINAR E SALVAR
# ==============================================================================
cat("\n══ Salvando output ══\n")
censo_pontos <- bind_rows(taxa_1991, taxa_2000, taxa_2010, taxa_2022)
censo_pontos_leg <- bind_rows(taxa_1991, taxa_2000, taxa_2010)

censo_out_2022 <- file.path(BASE_DIR, "output", "Censo_Pontos_Validacao_1991_2022.parquet")
censo_out_leg <- file.path(BASE_DIR, "output", "Censo_Pontos_Validacao_1991_2000_2010.parquet")

arrow::write_parquet(censo_pontos, censo_out_2022)
# O arquivo legado NAO recebe o Censo 2022: ele e a fonte que a Figura 2.1
# promovida na tese consome (via 041H_Tese_Decil_Lines_Censo.R). O Censo 2022
# e rascunho -- ainda sob revisao (ver docs/audits/) -- e nao deve alterar
# silenciosamente um artefato do qual uma figura ja publicada depende.
arrow::write_parquet(censo_pontos_leg, censo_out_leg)
cat(sprintf("✅ Parquet 1991-2022 (rascunho): %s\n", censo_out_2022))
cat(sprintf("✅ Parquet 1991-2000-2010 (legado, inalterado): %s\n", censo_out_leg))

cat("\nResumo da Validação Censitária por Decil (1991, 2000, 2010, 2022):\n")
censo_pontos |>
  filter(decil %in% c(0L, 1L, 5L, 10L)) |>
  mutate(label = case_when(decil == 0 ~ "Nacional", TRUE ~ paste0("D", decil))) |>
  select(ano, label, prop, lower, upper) |>
  arrange(ano, label) |>
  mutate(across(prop:upper, ~ sprintf("%.1f%%", .x))) |>
  print(n = Inf)

# ==============================================================================
# 6. PREPARAR SÉRIES PNAD/PNADC E SALATA
# ==============================================================================
cat("\n══ Preparando séries PNAD/PNADC e Salata ══\n")

dados_harm <- arrow::read_parquet(PARQUET_PNAD)
base_harm <- dados_harm |>
  filter(
    !(fonte == "PNAD Anual" & ano >= 2012),
    !is.na(ens_sup), !is.na(decil), !is.na(peso)
  ) |>
  mutate(decil_num = as.integer(decil))

calc_serie <- function(base) {
  dec <- base |>
    group_by(ano, decil_num) |>
    summarise(
      prop = Hmisc::wtd.mean(ens_sup, weights = peso, na.rm = TRUE) * 100,
      n_eff = sum(peso)^2 / sum(peso^2), .groups = "drop"
    ) |>
    mutate(
      se = sqrt((prop / 100) * (1 - prop / 100) / n_eff) * 100,
      lower = pmax(prop - 1.96 * se, 0), upper = pmin(prop + 1.96 * se, 100)
    )
  nac <- base |>
    group_by(ano) |>
    summarise(
      prop = Hmisc::wtd.mean(ens_sup, weights = peso, na.rm = TRUE) * 100,
      n_eff = sum(peso)^2 / sum(peso^2), .groups = "drop"
    ) |>
    mutate(
      se = sqrt((prop / 100) * (1 - prop / 100) / n_eff) * 100,
      lower = pmax(prop - 1.96 * se, 0), upper = pmin(prop + 1.96 * se, 100)
    )
  list(dec = dec, nac = nac)
}

serie_harm <- calc_serie(base_harm)

# --- Salata et al. (2025) ---
salata_raw <- arrow::read_parquet(SALATA_PATH,
  col_select = c("ano", "idade", "ens_sup", "renda_dom_pcta", "peso")
)

base_salata <- salata_raw |>
  filter(
    idade >= 18, idade <= 24,
    !is.na(renda_dom_pcta), renda_dom_pcta > 0,
    !is.na(ens_sup), !is.na(peso)
  ) |>
  mutate(ens_sup = coalesce(as.integer(unclass(ens_sup)), 0L)) |>
  group_by(ano) |>
  mutate(
    decil_num = {
      brks <- Hmisc::wtd.quantile(renda_dom_pcta, weights = peso, probs = seq(0, 1, 0.1), na.rm = TRUE)
      brks[1] <- -Inf
      brks[11] <- Inf
      as.integer(cut(renda_dom_pcta, breaks = brks, labels = 1:10, include.lowest = TRUE))
    }
  ) |>
  ungroup() |>
  filter(!is.na(decil_num))

serie_salata <- calc_serie(base_salata)

# ==============================================================================
# 7. GRÁFICOS OVERLAY COM DIAMANTES
# ==============================================================================
COR_D1 <- "#C0392B"
COR_D5 <- "#D4AC0D"
COR_D10 <- "#1A5276"
COR_NAC <- "#1E8449"
COR_GRAY <- "#BBBBBB"

GOV <- data.frame(
  nome = c("FHC I", "FHC II", "Lula I", "Lula II", "Dilma I", "Dilma II", "Temer", "Bolsonaro", "Lula III"),
  ini = c(1995, 1999, 2003, 2007, 2011, 2015, 2016, 2019, 2023),
  fim = c(1998, 2002, 2006, 2010, 2014, 2015, 2018, 2022, 2026),
  grupo = c("PSDB", "PSDB", "PT", "PT", "PT", "PT", "MDB", "PL", "PT"),
  stringsAsFactors = FALSE
) |> filter(ini <= 2025)
COR_GRUPO <- c("PSDB" = "#AED6F1", "PT" = "#F9A8A8", "MDB" = "#D5D8DC", "PL" = "#FAD7A0")

censo_nac_plt <- censo_pontos |> filter(decil == 0L)
censo_dec_plt <- censo_pontos |>
  filter(decil > 0L) |>
  mutate(categoria = case_when(
    decil == 1 ~ "D1", decil == 5 ~ "D5", decil == 10 ~ "D10", TRUE ~ "gray"
  ))
censo_d1 <- censo_dec_plt |> filter(categoria == "D1")
censo_d5 <- censo_dec_plt |> filter(categoria == "D5")
censo_d10 <- censo_dec_plt |> filter(categoria == "D10")
censo_gray <- censo_dec_plt |> filter(categoria == "gray")

make_overlay_plot <- function(serie, titulo_sub, x_end = NULL) {
  df_dec <- serie$dec
  df_nac <- serie$nac
  last_yr <- if (!is.null(x_end)) x_end else max(df_dec$ano)

  df_gray <- df_dec |> filter(!decil_num %in% c(1, 5, 10))
  df_high <- bind_rows(
    df_dec |> filter(decil_num == 1) |> mutate(serie_ = "D1: bottom 10%"),
    df_dec |> filter(decil_num == 5) |> mutate(serie_ = "D5: 40–50%"),
    df_dec |> filter(decil_num == 10) |> mutate(serie_ = "D10: top 10%")
  ) |> mutate(serie_ = factor(serie_,
    levels = c("D1: bottom 10%", "D10: top 10%", "D5: 40–50%")
  ))

  COR_HIGH <- c("D1: bottom 10%" = COR_D1, "D10: top 10%" = COR_D10, "D5: 40–50%" = COR_D5)
  LWT_HIGH <- c("D1: bottom 10%" = 1.2, "D10: top 10%" = 1.2, "D5: 40–50%" = 0.85)

  d1_end <- df_dec |>
    filter(decil_num == 1, ano == last_yr) |>
    pull(prop)
  d10_end <- df_dec |>
    filter(decil_num == 10, ano == last_yr) |>
    pull(prop)
  nac_end <- df_nac |>
    filter(ano == last_yr) |>
    pull(prop)
  if (!length(d1_end)) {
    d1_end <- df_dec |>
      filter(decil_num == 1) |>
      slice_max(ano) |>
      pull(prop)
  }
  if (!length(d10_end)) {
    d10_end <- df_dec |>
      filter(decil_num == 10) |>
      slice_max(ano) |>
      pull(prop)
  }
  if (!length(nac_end)) {
    nac_end <- df_nac |>
      slice_max(ano) |>
      pull(prop)
  }

  censo_nac_1991_y <- censo_nac_plt |>
    filter(ano == 1991) |>
    pull(prop)

  ggplot() +
    geom_rect(
      data = GOV,
      aes(xmin = ini, xmax = pmin(fim + 1, 2026), ymin = -Inf, ymax = Inf, fill = grupo),
      alpha = 0.18, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = "Government", values = COR_GRUPO,
      guide = guide_legend(order = 4, override.aes = list(alpha = 0.5, colour = NA))
    ) +
    geom_line(
      data = df_gray,
      aes(x = ano, y = prop, group = decil_num),
      colour = COR_GRAY, linewidth = 0.5, alpha = 0.85
    ) +
    geom_line(
      data = df_nac, aes(x = ano, y = prop),
      colour = COR_NAC, linewidth = 1.0, linetype = "longdash"
    ) +
    geom_line(
      data = df_high,
      aes(x = ano, y = prop, colour = serie_, linewidth = serie_, group = serie_)
    ) +
    geom_point(
      data = df_high |> filter(serie_ != "D5: 40–50%"),
      aes(x = ano, y = prop, colour = serie_), size = 1.6, show.legend = FALSE
    ) +
    # ── DIAMANTES DO CENSO (1991, 2000, 2010, 2022) ─────────────────────────
    geom_point(
      data = censo_gray,
      aes(x = ano, y = prop), shape = 23, fill = COR_GRAY, colour = "white", size = 2.4, stroke = 0.65
    ) +
    geom_point(
      data = censo_d5,
      aes(x = ano, y = prop), shape = 23, fill = COR_D5, colour = "white", size = 3.2, stroke = 0.85
    ) +
    geom_point(
      data = censo_d1,
      aes(x = ano, y = prop), shape = 23, fill = COR_D1, colour = "white", size = 3.8, stroke = 0.95
    ) +
    geom_point(
      data = censo_d10,
      aes(x = ano, y = prop), shape = 23, fill = COR_D10, colour = "white", size = 3.8, stroke = 0.95
    ) +
    geom_point(
      data = censo_nac_plt,
      aes(x = ano, y = prop), shape = 23, fill = COR_NAC, colour = "white", size = 5.0, stroke = 1.1
    ) +
    scale_colour_manual(
      name = "Highlighted\ndeciles", values = COR_HIGH,
      guide = guide_legend(order = 1, override.aes = list(linewidth = c(1.2, 1.2, 0.85)))
    ) +
    scale_linewidth_manual(values = LWT_HIGH, guide = "none") +
    annotate("text",
      x = last_yr + 0.5, y = d10_end + 0.8,
      label = "D10: top 10%", hjust = 0, size = 2.7, colour = COR_D10, fontface = "bold"
    ) +
    annotate("text",
      x = last_yr + 0.5, y = d1_end,
      label = "D1: bottom 10%", hjust = 0, size = 2.7, colour = COR_D1, fontface = "bold"
    ) +
    annotate("text",
      x = last_yr + 0.5, y = nac_end + 1.5,
      label = sprintf("Nat.\n%.0f%%", nac_end),
      hjust = 0, size = 2.3, colour = COR_NAC, lineheight = 0.9
    ) +
    annotate("text",
      x = 1991.4, y = censo_nac_1991_y + 5,
      label = "◆ Census\n(1991, 2000,\n2010, 2022)", hjust = 0, size = 2.2, colour = "#444", lineheight = 0.85
    ) +
    annotate("segment",
      x = 1991.3, xend = 1991.05, y = censo_nac_1991_y + 4, yend = censo_nac_1991_y + 0.8,
      colour = "#888", linewidth = 0.4, arrow = arrow(length = unit(0.10, "cm"), type = "open")
    ) +
    scale_x_continuous(breaks = seq(1991, 2025, 4), expand = expansion(mult = c(0.02, 0.13))) +
    scale_y_continuous(
      limits = c(0, 100), breaks = seq(0, 80, 20),
      labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.01, 0.03))
    ) +
    labs(subtitle = titulo_sub, x = NULL, y = "Access rate (%)") +
    theme_minimal(base_size = 10) +
    theme(
      plot.subtitle = element_text(size = 9, colour = "#333", face = "bold"),
      legend.position = "right", legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 8), legend.spacing.y = unit(0.2, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5),
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      plot.margin = margin(4, 10, 4, 8)
    )
}

geo_note <- if (EXCLUIR_RURAL_NORTE) " · Rural Norte excl. (D03)" else ""

p_harm <- make_overlay_plot(
  serie      = serie_harm,
  titulo_sub = paste0("Nossa harmonização — PNAD Anual 1992–2015 + PNADC 2016–2025", geo_note),
  x_end      = 2025
)

p_salata <- make_overlay_plot(
  serie      = serie_salata,
  titulo_sub = paste0("Salata et al. (2025) — PNAD/PNADC harmonizados 1992–2022", geo_note),
  x_end      = 2022
)

caption_txt <- paste0(
  "Sources: PNAD/PNADC harmonized (IBGE) + Censos Demográficos 1991, 2000, 2010, 2022 (censobr/ipeaGIT standard).\n",
  "ens_sup = entered tertiary education (ever enrolled). Income: household per capita, IPCA-deflated (Jan/2024).\n",
  "Decisions D01–D26."
)

p_combined <- (p_harm / p_salata) +
  plot_annotation(
    title = "Tertiary education access by income decile — Series comparison with Census validation (◆)",
    caption = caption_txt,
    theme = theme(
      plot.title   = element_text(face = "bold", size = 12),
      plot.caption = element_text(size = 7, colour = "#666", lineheight = 1.2)
    )
  )

png_combined <- file.path(OUTPUT_DIR, paste0(DATE_PREFIX, "_040_Censo_Validacao_Combined.png"))
ggsave(png_combined, plot = p_combined, width = 22, height = 26, units = "cm", dpi = 300)
cat(sprintf("✅ Combined: %s\n", png_combined))

p_harm_full <- p_harm +
  labs(
    title   = "Tertiary education access rate by income decile — Own harmonization",
    caption = caption_txt
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.caption = element_text(size = 7, colour = "#666", lineheight = 1.2)
  )

png_harm <- file.path(OUTPUT_DIR, paste0(DATE_PREFIX, "_040A_Validacao_Nossa_Harmonizacao.png"))
ggsave(png_harm, plot = p_harm_full, width = 22, height = 13, units = "cm", dpi = 300)
cat(sprintf("✅ Nossa harm.: %s\n", png_harm))

cat("\n══════ Script 040 concluído com sucesso ══════\n")
