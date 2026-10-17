# Aggregate replication snapshot

This folder contains only aggregate outputs of the independent spatial
estimator in `shared-pipeline/eleicoes/`. It does not contain voter records,
polling-place addresses. Approximate visual reference readings are kept
separately from the calculated election curves and labelled as such.

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
- `cobertura-votos.csv`, `cobertura-votos-uf.csv`: unique polling-place vote
  totals at each selection stage, coverage of domestic valid votes and
  differences in candidate shares relative to the territorial reference.
- `sensibilidade-cortes-fixos.csv`: 1/2/5/10 km and unlimited estimates with
  the same income-eligible, geographically linked universe ranked once.
  Missing tracts stay missing instead of changing percentile boundaries.
- `centis-ponderados.csv`: percentile boundaries and means weighted by
  `V06001` (household heads, including heads without income; not voters).
  Whole tracts are retained, so groups are not exactly 1% of heads.
- `centis-razao.csv`, `conservacao-votos.csv`: candidate/valid vote ratios
  after allocating each used place once across linked tracts, proportional
  to `V06001`. Vote totals are conserved; tract allocation is assumed.
- `sensibilidade-ligacao.csv`: nearest and inverse-distance three-neighbour
  estimates on the same fixed percentile groups.
- `exclusoes-distancia.csv`, `cruzamentos-cortes-fixos.csv`,
  `confronto-metodos.csv`: exclusions by state, income decile and census
  urban/rural classification; first/sustained crossings; method differences.
- `bootstrap-municipios.csv`, `bootstrap-replicas.csv`,
  `bootstrap-cruzamentos.csv`, `verificacao-bootstrap.csv`: 500 fixed-seed
  municipality resamples, their pointwise quantiles and independent checks.
  These are exploratory distributions, not census sampling confidence bands.
- `diagnostico-renda.csv`: the share of heads without income is explicitly
  unavailable; `ResponsavelRenda` does not supply the required count.
- `referencia-folha.csv`, `comparacao-folha.csv`: rounded manual readings
  from the supplied image, the exact visible tooltip and arithmetic errors.
  Obscured values remain missing. Reading tolerances are not statistical
  confidence intervals. The reference is never inserted into the curves.
- `verificacao-espacial-resumo.csv`, `locais-fora-municipio-resumo.csv`:
  state summaries of the independent 2,000-tract geometric audit and
  excluded polling places' vote totals. EPSG:5880 discrepancies are retained
  as diagnostics; acceptance uses independent base-R spherical distances.
- `verificacao-zip-csv.csv`: CSV sizes and CRC32 checked against both TSE ZIPs.
- `validacao-metodologica.json`: successful independent validation and hashes
  of inputs, numerical source and outputs; written only after all checks pass.
- `publicacao-snapshot.json`: byte hashes of published and derived files.
  `.gitattributes` preserves their bytes across platform checkouts.
- `diagnosticos-pt.md`, `diagnosticos-en.md`: generated bilingual tables.

The default maximum distance is 5,000 metres. A tract receives the vote
shares of the nearest eligible polling place to its interior point, within
the same municipality. Votes at that place are first summed across sections.
The denominator includes all valid presidential candidates, not just the
two displayed candidates. Percentiles contain equal numbers of eligible
tracts as closely as possible, not equal populations.

Run `posts/votacao-renda-centil/plot.R` from the repository root to draw this
snapshot without the upstream downloads. To rebuild from public sources,
run scripts `010`, `020`, `030` and `040` in `shared-pipeline/eleicoes/`, then the
plot script. When both exist, `plot.R` prefers the newly generated
`data-raw/eleicoes-renda/centis.csv` over this snapshot.

Income is measured in 2022 for heads **with income**, four years before the
election. Spatial proximity does not identify where tract residents voted.
Higher candidate shares in higher-income areas do not establish individual
voters' incomes or an individual income–vote relationship.
