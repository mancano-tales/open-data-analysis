# Aggregate replication snapshot

This folder contains only aggregate outputs of the independent spatial
estimator in `shared-pipeline/eleicoes/`. It does not contain voter records,
polling-place addresses or a transcription of the reference image.

The snapshot uses Census 2022 household-head income (`V06004`, through
`censobr`) and the first round of the 2026 presidential election (TSE).
Polling-place coordinates and boundaries come from `geobr`. See the post
for the estimator, exclusions and differences from the Folha analysis.

- `centis.csv`: two candidate series, 100 tract percentiles each. `votacao`
  is the unweighted mean of tract vote percentages; `renda_sm` is mean
  household-head income in 2022 minimum wages (BRL 1,212); `n_setores`
  counts tracts, separately repeated for each candidate.
- `sensibilidade.csv`: estimates under maximum distances of 1, 2 and 5 km.
  Each distance uses its own eligible sample and percentile ranking.
- `cobertura-setores.csv`, `cobertura-locais.csv`, `cobertura-renda.csv`:
  exclusion counts at the tract, polling-place and income stages.
- `auditoria-ligacao.csv`: differences between section location codes in
  election results and in the current voter register. Results take precedence.
- `manifest.csv`: public TSE download URLs and MD5 checksums of raw ZIPs.
- `checksums.csv`, `versoes.csv`: fingerprints of cached inputs and the R and
  package versions used for numerical validation.

The default maximum distance is 5,000 metres. A tract receives the vote
shares of the nearest eligible polling place to its interior point, within
the same municipality. Votes at that place are first summed across sections.
The denominator includes all valid presidential candidates, not just the
two displayed candidates. Percentiles contain equal numbers of eligible
tracts as closely as possible, not equal populations.

Run `posts/votacao-renda-centil/plot.R` from the repository root to draw this
snapshot without the upstream downloads. To rebuild from public sources,
run scripts `010`, `020` and `030` in `shared-pipeline/eleicoes/`, then the
plot script. When both exist, `plot.R` prefers the newly generated
`data-raw/eleicoes-renda/centis.csv` over this snapshot.
