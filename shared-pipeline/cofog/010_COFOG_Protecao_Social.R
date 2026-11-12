# ==============================================================================
# SCRIPT: shared-pipeline/cofog/010_COFOG_Protecao_Social.R
#
# Despesa do governo geral em protecao social (funcao 710 da COFOG), em % do
# PIB, 2010-2023, para o Brasil, os paises nordicos, a Europa continental e a
# America Latina.
#
# FONTES
#   FMI, Government Finance Statistics, dataflow IMF.STA:GFS_COFOG (API SDMX
#   2.1): setor S13 (governo geral), GF10_T, transformacao POGDP_PT (% do PIB).
#   OCDE, contas nacionais: Table 11 (DSD_NASEC10@DF_TABLE11, GF10, despesa
#   total OTE, S13, moeda nacional) dividida pelo PIB nominal da Table 1
#   (DSD_NAMAIN10@DF_TABLE1_EXPENDITURE, B1GQ a precos correntes). Usada so
#   para paises sem o dado no FMI (Chile e Colombia). Para a Franca, a razao da
#   OCDE coincide com o valor do FMI em todos os anos de 2010 a 2023.
#
# COBERTURA (conferida em 2026-11-12): na America Latina, o FMI tem o governo
# geral so para Costa Rica, El Salvador, Guatemala, Panama e Republica
# Dominicana no periodo; a OCDE acrescenta Chile (2018 em diante) e Colombia.
# Argentina, Bolivia, Equador, Mexico, Paraguai, Peru e Uruguai nao tem o dado
# de 2010 a 2023 em nenhuma das duas fontes.
#
# SAIDA: posts/protecao-social-brasil-europa-america-latina/data/protecao_social.csv
#        (iso3, ano, pct_pib, fonte), redistribuido com o post.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(readr)
  library(here)
})

ANO_INI <- 2010
ANO_FIM <- 2023
EUROPA <- c("DNK", "FIN", "ISL", "NOR", "SWE",                # nordicos
            "AUT", "BEL", "FRA", "DEU", "LUX", "NLD", "CHE")  # Europa continental
AMERICA_LATINA <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU",
                    "GTM", "HND", "HTI", "MEX", "NIC", "PAN", "PER", "PRY", "SLV",
                    "URY", "VEN")
SAIDA <- here::here("posts", "protecao-social-brasil-europa-america-latina", "data",
                    "protecao_social.csv")

ACEITA <- "application/vnd.sdmx.data+csv;version=1.0.0"
sdmx_csv <- function(url) {
  request(url) |>
    req_url_query(startPeriod = ANO_INI, endPeriod = ANO_FIM) |>
    req_headers(Accept = ACEITA) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform() |>
    resp_body_string() |>
    I() |>
    read_csv(show_col_types = FALSE)
}

# FMI: um pedido por pais (o servidor recusa chaves muito longas)
fmi <- lapply(c(EUROPA, AMERICA_LATINA), function(p) {
  x <- sdmx_csv(paste0("https://api.imf.org/external/sdmx/2.1/data/IMF.STA,GFS_COFOG/",
                       p, ".S13.G2MF.GF10_T.POGDP_PT.A"))
  if (!"OBS_VALUE" %in% names(x)) return(NULL)
  x |> transmute(iso3 = COUNTRY, ano = as.integer(TIME_PERIOD), pct_pib = as.numeric(OBS_VALUE))
}) |>
  bind_rows() |>
  filter(!is.na(pct_pib)) |>
  mutate(fonte = "FMI")

# OCDE: so os paises latino-americanos que faltam no FMI
faltam <- setdiff(AMERICA_LATINA, unique(fmi$iso3))
oecd <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,"
pp <- paste(faltam, collapse = "+")
gasto <- sdmx_csv(paste0(oecd, "DSD_NASEC10@DF_TABLE11,1.1/A.", pp,
                         ".S13._Z.D.OTE._Z.GF10.XDC.S.V.N.T1100"))
pib <- sdmx_csv(paste0(oecd, "DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,/A.", pp, ".S1..B1GQ......."))
ocde <- inner_join(
  gasto |> transmute(iso3 = REF_AREA, ano = as.integer(TIME_PERIOD), gasto = OBS_VALUE * 10^UNIT_MULT),
  pib |> filter(UNIT_MEASURE == "XDC", PRICE_BASE == "V") |>
    transmute(iso3 = REF_AREA, ano = as.integer(TIME_PERIOD), pib = OBS_VALUE * 10^UNIT_MULT),
  by = c("iso3", "ano")
) |>
  transmute(iso3, ano, pct_pib = 100 * gasto / pib, fonte = "OCDE")

dados <- bind_rows(fmi, ocde) |> arrange(iso3, ano)
dir.create(dirname(SAIDA), recursive = TRUE, showWarnings = FALSE)
write_csv(dados, SAIDA)
print(count(dados, iso3, fonte), n = 50)
