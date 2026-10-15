# Static aggregate explorer data contract, version 1

`app.js` reads `data/manifest.json`, then fetches only the selected series.
No raw electoral or census microdata belong in this directory. Serve through
Quarto preview or an HTTP server; browser `file://` blocks JSON fetches.

Manifest fields:

```json
{"schema_version":1,"offices":{"1":{"pt":"Presidente","en":"President"}},"series":[{"id":"2026-1-1-BR","year":2026,"round":1,"office":"1","office_label":{"pt":"Presidente","en":"President"},"uf":"BR","url":"2026-1-1-BR.json","status":"available"}]}
```

URLs are relative to the directory containing the manifest. Only entries
with `status: "available"` appear in selectors. One series per combination
of year, round, office and territory. Add municipal scope as a new field and
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
series use `metadata.percentile_scope: "BR"`: rank ALL census tracts with
eligible income, before any electoral/spatial exclusions, by mean income
of household heads with income, using Census 2022 V06004. These national
groups remain fixed between elections and within UF filters. `renda_sm`
describes the complete income-eligible group in the selected territory,
before vote/spatial exclusions. `n_setores_total`
is the number of eligible income tracts in the selected territory/group;
`n_setores` counts those covered by the electoral estimator. Missing vote
shares use JSON null, never zero. `renda_sm` uses the 2022 minimum wage,
BRL 1,212. `n_setores` is repeated for each candidate and must sum to
`included_tracts` separately for every candidate. Document any changed
universe, percentile ranking, weighting or income variable in metadata;
never silently mix these estimators in one chart.

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

The manifest contains only series with fixed national income ranks and
reconciled official valid-vote inputs. It must not include the older
matched-sample centiles from the initial reconstruction.

`metadata.spatial.within_year_register_reused: true` identifies use of the
first-round polling register to match second-round votes of that same year.
The source date and round are recorded in `polling_reference_date` and
`polling_reference_round`. The UI displays this approximation explicitly.
