# Static aggregate explorer data contract, version 1

`app.js` reads `data/manifest.json`, then fetches only the selected series.
Source aggregates are plain `.json`; the published manifest keeps the same
schema but points to `.json.gz` files. The browser decodes gzip before parsing
the same data structure. EN and PT share the canonical published data under
`posts/explorador-eleitoral/data/`; no second PT dataset copy is required.
See [PUBLISHING.md](PUBLISHING.md) for build order, backup checks and URL migration.
No raw electoral or census microdata belong in this directory. Serve through
Quarto preview or an HTTP server; browser `file://` blocks JSON fetches.

Manifest fields:

```json
{"schema_version":1,"offices":{"1":{"pt":"Presidente","en":"President"}},"series":[{"id":"2026-1-1-BR","year":2026,"round":1,"office":"1","office_label":{"pt":"Presidente","en":"President"},"uf":"BR","income_reference":"national","url":"2026-1-1-BR.json","status":"available"}]}
```

URLs are relative to the directory containing the manifest. Only entries
with `status: "available"` appear in selectors. One series per combination
of year, round, office, territory and `income_reference`. The latter is
`"national"` or `"state"`; missing values in older manifests mean national.
A state-reference series uses its national counterpart's ID plus `-state`.
The BR territory supports national reference only. Never publish duplicate
entries for the same five dimensions: the UI rejects ambiguous selections.
Add municipal scope as a new field and
control before publishing municipal series. A national aggregation of
subnational offices needs an explicit definition of candidate comparability.

Office codes follow the TSE (`"1"`, `"3"`, `"5"`, `"6"`, `"7"`, `"8"`), as
strings. The interface tolerates legacy office slugs and reads friendly
labels from `offices` or each entry's `office_label`.

Each series has the following fields (metadata text may be a string or
`{"pt":"…","en":"…"}`):

```json
{"metadata":{"percentile_scope":"BR","method":{"pt":"…","en":"…"},"income_definition":{"pt":"…","en":"…"},"warning":{"pt":"…","en":"…"},"coverage":{"included_tracts":410010,"total_tracts":458772},"sources":[{"label":"TSE","url":"https://dadosabertos.tse.jus.br/"}]},"candidates":[{"id":"lula","name":"Lula","party":"PT","color":"#e63d65"}],"points":[{"centil":1,"renda_sm":0.58,"n_setores":4101,"n_setores_total":4495,"candidate_id":"lula","votacao":77.14}]}
```

All numbers must be JSON numbers (never localized strings). `votacao` is
on a 0–100 scale. Every candidate must have exactly 100 points and unique
centils 1–100. Candidate IDs must match; names are plain text. The expanded
series use one of two fixed Census 2022 income references. National reference
(`income_reference: "national"`, `metadata.percentile_scope: "BR"`) ranks
ALL Brazilian census tracts with eligible income before any electoral/spatial
exclusions, using mean income of household heads with income (V06004). Its
national groups remain fixed between elections and within UF filters.
State reference (`income_reference: "state"`, `metadata.percentile_scope:
"AC"` for Acre, `"SP"` for São Paulo, or the corresponding actual UF code)
instead ranks all income-eligible tracts within the selected UF before
the same exclusions, with approximately 1% of that state's tracts in each
group. Its state groups likewise remain fixed between elections. `renda_sm`
describes the complete income-eligible group in the selected territory,
before vote/spatial exclusions. `n_setores_total`
is the number of eligible income tracts in the selected territory/group;
`n_setores` counts those covered by the electoral estimator. Missing vote
shares use JSON null, never zero. `renda_sm` uses the 2022 minimum wage,
BRL 1,212. `n_setores` is repeated for each candidate and must sum to
`included_tracts` separately for every candidate. Document any changed
universe, percentile ranking, weighting or income variable in metadata;
never silently mix these estimators in one chart.

The scope value is the exact UF abbreviation, never the literal `"UF"`.
Before displaying curves, the UI requires `percentile_scope` to equal `"BR"`
for a national reference or the selected territory code for a state reference.
Missing or conflicting scope metadata are rejected visibly; the UI must not
silently relabel an incompatible series. If `metadata.income_reference` is
present, it must also match the manifest entry.

The fifth selector, after year/round/office/territory, chooses income reference.
Presidential contests default to national reference, including their UF
filters; other offices default to state reference when available. Changing
office resets this default. Changing UF retains the user's preferred reference;
when unavailable (including BR with a state preference), the UI explicitly
announces the available fallback and retains the preference for a later UF
change. Candidate selections are preserved across reference changes when
their IDs still exist. Manifest requests use `cache: "no-cache"` so a newly
deployed list of available references is revalidated by the browser.

`metadata.vote_audit` records the mandatory independent reconciliation with
official TSE electoral totals, not certification of spatial attribution:

```json
{"status":"reconciled","method":"Municipality/zone/candidate valid votes compared with official TSE aggregates","official_valid_votes":12345,"computed_valid_votes":12345,"difference":0,"checked_units":100,"ufs":["DF"],"scope":"official-valid-votes","source":{"label":"TSE","url":"https://dadosabertos.tse.jus.br/"}}
```

The UI displays reconciled status only when `status` is `reconciled`,
`difference` is zero and the two totals agree. `not_run`, missing audit,
`unreconciled`, `failed` and inconsistent totals never show a passed status.
`scope` and `source` are optional for backward compatibility. Nominal and
party-list votes count as legally applicable; blank/invalid votes do not.
The builder, rather than the UI, enforces which disputes may be published.

`vote_audit.provisional: true` requires a visible provisional-results warning
even when vote totals reconcile exactly. `vote_audit.snapshots` may be an
array or an object keyed by office. Preserve each relevant snapshot's URL,
MD5, `generation_date`, `generation_time`, `idg`, `final_totalization`,
`progress` and `status`. Combined caches must scope this evidence to the
selected contest: provisional deputy snapshots must not label unrelated
governor or senate contests provisional. Numerical agreement with a later
CSV never establishes final legal totalization of an earlier EA20 snapshot.

Optional `candidates[].number` adds the ballot number to labels and search.
Optional `metadata.default_candidates` is an array of candidate IDs to show
initially. Otherwise the interface selects the six candidates with greatest
mean tract vote share across the percentile points (this ordering is NOT a
ranking of national vote totals). Candidate search ignores accents and
matches name, number and party. Only the first 100 matching checkboxes are
rendered; refine the query to find others. Selection is independent of the
visible search results, and no raw data or alternative denominators are
created by these display controls.

`metadata.warning` must distinguish neighbourhood estimates from actual
votes of tract residents and identify incomplete candidate coverage. Do not
infer zero votes from absent candidates or absent years. Candidate selection
affects only visible curves: the denominator remains all valid votes for
the selected office, as defined by the upstream estimator.

The manifest contains only series with fixed national or state income ranks and
reconciled official valid-vote inputs. It must not include the older
matched-sample centiles from the initial reconstruction.

`metadata.spatial.within_year_register_reused: true` identifies use of the
first-round polling register to match second-round votes of that same year.
The source date and round are recorded in `polling_reference_date` and
`polling_reference_round`. The UI displays this approximation explicitly.

## Census reference (`census_year`)

Each manifest entry may carry `census_year` (`2022` or `2010`). A missing
value means `2022`, so every existing series keeps its meaning and URL. The
selector "Censo de referência / Reference census" sits between territory and
income reference and only lists censuses available for that contest; Census
2022 is the default. One series per combination of year, round, office,
territory, `census_year` and `income_reference`.

A Census 2010 series:

- uses the ID of its 2022 counterpart plus `-c2010` (for example
  `2014-1-3-SP-state-c2010`);
- sets `metadata.income_year` to `2010`; the UI rejects a series whose
  `income_year` differs from the selected census;
- ranks Census 2010 tracts with V007 (mean nominal income of heads WITH
  income, Basico dataset) and heads V001, with the same eligibility rule and
  the same fixed-universe percentiles as 2022;
- reports `renda_sm` in 2010 minimum wages (BRL 510);
- uses 2010 tract and municipality geometry; polling places still come from
  the election year;
- is published only for the elections enabled in
  `shared-pipeline/eleicoes/agregacao/censo-referencia.R` (2010 and 2014).

Census 2010 and Census 2022 tracts do not match one to one. Curves from
different censuses must never be subtracted or drawn as one series.

## Comparator (`compare.js`)

The comparator below the explorer reads the same manifest and series; it adds
no data files. It subtracts party curves of two series (B − A) only when both
share office, territory, `income_reference` and `census_year`. A party curve
is the sum, per centile, of the shares of every candidate and legend row whose
ballot number starts with that two-digit party number (or whose
`party_number` field says so). A centile missing in either series stays
`null`. The optional manifest field `parties` maps
`{"<year>": {"<number>": "<acronym>"}}` for labels only; matching always uses
the number.
