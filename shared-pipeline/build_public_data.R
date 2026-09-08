# ==============================================================================
# build_public_data.R
#
# PURPOSE: Reconstruct every PUBLIC dataset (Tier A / Tier A cross-repo) used
# by this repository, from scratch, in the correct order. This does NOT cover
# Tier B figures (shared-pipeline/sedap/), which require an approved SEDAP+
# credential (SEDAP_TOKEN) — see shared-pipeline/sedap/010_SEDAP_Cliente.R and
# the "Reproducibility" section of each Tier B post.
#
# WHAT THIS DOES: sources the numbered scripts of each data source in
# dependency order. Each script downloads and/or caches its own intermediate
# files under data-raw/ (created automatically). Running this end to end
# downloads several hundred MB to a few GB of public microdata and can take a
# long time — run one source() block at a time if you only need one figure's
# data (see the "Pipeline" section of the specific post you're reproducing).
#
# PREREQUISITES: R packages used across these scripts — arrow, dplyr, tidyr,
# Hmisc, deflateBR, here, PNADcIBGE, censobr, sidrar, ipeadatar, httr2,
# archive, readr, stringr, survey.
# ==============================================================================

here_root <- here::here()
cat("Building public datasets into:", file.path(dirname(here_root), "..", "data-raw"), "\n\n")

# ── 1. PNAD Anual + PNAD Contínua (harmonized panel) ─────────────────────────
# 000 downloads/consolidates PNAD Contínua; 020/050 manually import the PNAD
# Anual vintages that 035 needs (1992-1999 and 2001-2015); 021 is an
# alternative PNAD Anual import used by a subset of figures independently of
# 035. 035 splices everything into the harmonized panel that most Tier A
# figures read from; 238 derives the official inequality series from it.
cat("[1/5] PNAD Contínua: download + consolidate (000)\n")
source(here::here("shared-pipeline", "pnadc", "000_PNADC_Download_Consolidate.R"))

cat("\n[2/5] PNAD Anual manual imports (020, 050, 021)\n")
source(here::here("shared-pipeline", "pnadc", "020_PNAD_Anual_Manual_Import_2001_2015.R"))
source(here::here("shared-pipeline", "pnadc", "050_PNAD_Historica_Manual_Import_1992_1999.R"))
source(here::here("shared-pipeline", "pnadc", "021_PNAD_Anual_Import_Com_Renda_Zero.R"))

cat("\n[3/5] Splice PNAD Anual + PNAD Contínua into the harmonized panel (035)\n")
source(here::here("shared-pipeline", "pnadc", "035_Splice_Microdados.R"))

cat("\n[3b/5] Official income-inequality series, Gini/Palma (238)\n")
source(here::here("shared-pipeline", "pnadc", "238_Serie_Desigualdade_Oficial.R"))

# ── 2. Demographic Census validation points (via {censobr}) ──────────────────
cat("\n[4/5] Demographic Census validation points (censo/040)\n")
source(here::here("shared-pipeline", "censo", "040_Censo_Pontos_Validacao.R"))

# ── 3. Higher Education Census (CENSUP), public microdata ────────────────────
cat("\n[5/5] CENSUP: download + decompress public microdata (censup/001)\n")
source(here::here("shared-pipeline", "censup", "001_Code_Download_Decompress_all_Public_CENSUP_data.R"))

# ── 4. IPCA tuition subitem (via {sidrar}/{ipeadatar}) ────────────────────────
cat("\n[bonus] IPCA higher-education tuition series (ipca/010) — self-contained\n")
source(here::here("shared-pipeline", "ipca", "010_IPCA_Curso_Superior_Series.R"))

# ── 5. educabr2 (cross-repo, no local build needed) ───────────────────────────
# expansao-composicao-matriculas and the six educabr2-* posts read from the
# public R package `educabr2`, which embeds its own data:
#   remotes::install_github("mancano-tales/educabr2")
# No script here builds it — install the package instead.

cat("\nDone. Public datasets for every Tier A / Tier A (cross-repo) figure are",
    "now in data-raw/. Tier B figures (shared-pipeline/sedap/) still require",
    "an approved SEDAP+ credential — see their own posts.\n")
