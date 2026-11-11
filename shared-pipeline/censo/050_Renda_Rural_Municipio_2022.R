# ==============================================================================
# SCRIPT: 050_Renda_Rural_Municipio_2022.R
# ODA: portado de 4-DA-Code/2026-11_Renda-Rural-Municipios/R/
#      010_Calcula_Renda_Rural_Municipio.R (repositorio da dissertacao) em
#      2026-11-11.
#
# Rendimento domiciliar per capita medio da populacao rural de cada municipio
# brasileiro, Censo Demografico 2022 (questionario da amostra).
#
# Fonte: microdados da amostra do Censo 2022 em ACESSO CONTROLADO (zip
# solicitado ao IBGE em https://microdados.ibge.gov.br/), importados uma vez
# para o cache do {censobr} com censobr::import_microdata22_controlado()
# (ramo dev; testado com o commit dd48725f6d8763874acd15d2eff222e2024470ee,
# o mesmo de 040_Censo_Pontos_Validacao.R). Sem esse zip, este script nao
# roda: a tabela municipal agregada que ele grava e redistribuida em
# posts/renda-rural-municipio-2022/data/ e basta para redesenhar o mapa.
#
# Definicoes (Dicionario_de_Variaveis_Microdados_CD2022.pdf, IBGE):
#   D0140  situacao do domicilio: 2 = Rural (povoado, nucleo rural, lugarejo,
#          area rural exclusive aglomerados)
#   D0360  rendimento domiciliar per capita, em reais, calculado pelo IBGE
#          excluindo os rendimentos de pensionistas, empregados domesticos e
#          parentes de empregados domesticos; so para domicilios particulares
#   P0111  peso amostral da pessoa (o mesmo do domicilio)
#   P0170  condicao no domicilio: 17 pensionista, 18 empregado(a) domestico(a),
#          19 parente do(a) empregado(a) domestico(a), 20 individual em
#          domicilio coletivo
#   P0130  especie da unidade visitada: 6 = domicilio coletivo com morador
#
# Unidade de analise: pessoa moradora de domicilio particular rural, exceto as
# condicoes 17-20. Cada pessoa recebe o D0360 do seu domicilio; a media e
# ponderada por P0111. Domicilios com rendimento zero entram na media (mesma
# base das estatisticas oficiais de rendimento domiciliar per capita).
#
# Valores em reais de janeiro de 2024 (IPCA via deflateBR, a partir da data
# de referencia do Censo, 31/07/2022). A versao nominal tambem e gravada.
#
# Precisao: erro-padrao por linearizacao da razao, com o domicilio como
# unidade primaria dentro do municipio (aproximacao com reposicao, sem
# estratos nem calibracao por area de ponderacao, de modo que tende a
# superestimar um pouco o erro). Serve para sinalizar municipios com amostra
# rural pequena (coeficiente de variacao alto), nao como IC oficial.
#
# Saida: posts/renda-rural-municipio-2022/data/renda_rural_municipio_2022.csv
#   (uma linha por municipio com domicilio rural na amostra; so agregados,
#   nenhum registro individual)
# ==============================================================================

suppressPackageStartupMessages({
  library(censobr)
  library(arrow)
  library(dplyr)
  library(deflateBR)
  library(here)
})

DATA_REF_CENSO2022 <- as.Date("2022-07-31")
BASE_IPCA <- "01/2024"

DIR_SAIDA <- here::here("posts", "renda-rural-municipio-2022", "data")
if (!dir.exists(DIR_SAIDA)) dir.create(DIR_SAIDA, recursive = TRUE)

CHAVE_DOM <- c("code_state", "code_muni", "code_weighting")
fmt_n <- function(x) format(x, big.mark = ".", decimal.mark = ",")

# 1. Domicilios rurais -----------------------------------------------------------
dom_rural <- censobr::read_households(
  year = 2022, columns = c(CHAVE_DOM, "D0100", "D0140", "D0360")
) |>
  filter(D0140 == 2L) |>
  collect()
cat(sprintf("Domicilios rurais na amostra: %s\n", fmt_n(nrow(dom_rural))))

# 2. Pessoas desses domicilios ---------------------------------------------------
pes <- censobr::read_population(
  year = 2022, columns = c(CHAVE_DOM, "P0100", "P0111", "P0130", "P0170")
) |>
  collect() |>
  inner_join(dom_rural, by = c(CHAVE_DOM, "P0100" = "D0100")) |>
  filter(
    !(P0170 %in% c(17L, 18L, 19L, 20L)),
    !(P0130 %in% 6L),
    !is.na(P0111), P0111 > 0,
    !is.na(D0360)
  )
cat(sprintf("Pessoas elegiveis: %s\n", fmt_n(nrow(pes))))

# 3. Deflator --------------------------------------------------------------------
FATOR_IPCA <- deflateBR::ipca(1, DATA_REF_CENSO2022, BASE_IPCA)
cat(sprintf("Fator IPCA 07/2022 -> %s: %.6f\n", BASE_IPCA, FATOR_IPCA))

# 4. Agregacao por municipio (domicilio = unidade primaria) ---------------------
por_dom <- pes |>
  group_by(code_state, code_muni, code_weighting, P0100) |>
  summarise(w = first(P0111), m = n(), y = first(D0360), .groups = "drop")

renda_mun <- por_dom |>
  group_by(code_muni) |>
  summarise(
    n_dom_amostra = n(),
    n_pes_amostra = sum(m),
    pop_rural_est = round(sum(w * m)),
    renda_media_nominal = sum(w * m * y) / sum(w * m),
    renda_mediana_nominal = {
      o <- order(y)
      cw <- cumsum((w * m)[o]) / sum(w * m)
      y[o][which(cw >= 0.5)[1]]
    },
    prop_renda_zero = sum(w * m * (y == 0)) / sum(w * m),
    ep_media_nominal = {
      ybar <- sum(w * m * y) / sum(w * m)
      z <- w * m * (y - ybar)
      k <- length(z)
      if (k > 1) sqrt(k / (k - 1) * sum(z^2)) / sum(w * m) else NA_real_
    },
    .groups = "drop"
  ) |>
  mutate(
    cv = ep_media_nominal / renda_media_nominal,
    renda_media_jan2024 = renda_media_nominal * FATOR_IPCA,
    renda_mediana_jan2024 = renda_mediana_nominal * FATOR_IPCA,
    ep_media_jan2024 = ep_media_nominal * FATOR_IPCA
  )

nac <- por_dom |>
  summarise(pop = sum(w * m), media = sum(w * m * y) / sum(w * m),
            zero = sum(w * m * (y == 0)) / sum(w * m))
cat(sprintf(
  "Brasil rural: populacao estimada %s; media R$ %.2f (07/2022) = R$ %.2f (01/2024); renda zero %.1f%%\n",
  fmt_n(round(nac$pop)), nac$media, nac$media * FATOR_IPCA, 100 * nac$zero
))
cat(sprintf("Municipios com domicilio rural na amostra: %d; com CV > 30%%: %d\n",
            nrow(renda_mun), sum(renda_mun$cv > 0.30, na.rm = TRUE)))

# 5. Nomes dos municipios (malha municipal de 2022 do geobr) ---------------------
nomes <- geobr::read_municipality(year = 2022, showProgress = FALSE) |>
  sf::st_drop_geometry() |>
  transmute(code_muni = as.integer(code_muni), municipio = name_muni,
            uf = abbrev_state, regiao = name_region)

saida <- renda_mun |>
  mutate(code_muni = as.integer(code_muni)) |>
  left_join(nomes, by = "code_muni") |>
  select(code_muni, municipio, uf, regiao, n_dom_amostra, n_pes_amostra,
         pop_rural_est, renda_media_jan2024, renda_mediana_jan2024,
         ep_media_jan2024, cv, prop_renda_zero,
         renda_media_nominal, renda_mediana_nominal) |>
  mutate(across(c(renda_media_jan2024, renda_mediana_jan2024, ep_media_jan2024,
                  renda_media_nominal, renda_mediana_nominal), ~ round(.x, 2)),
         across(c(cv, prop_renda_zero), ~ round(.x, 4))) |>
  arrange(code_muni)

arquivo <- file.path(DIR_SAIDA, "renda_rural_municipio_2022.csv")
write.csv(saida, arquivo, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Salvo: %s\n", arquivo))
