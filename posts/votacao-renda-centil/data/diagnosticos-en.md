Used polling places represent 108,710,471 of 118,969,906 votes in the national denominator (91.38%), across 81,076 places. Within that subset, Lula has 44.84% and Flávio Bolsonaro 47.29%, differences of -0.32 and 0.25 percentage points from national shares of 45.16% and 47.04%.

The main figure retains the simple mean across tracts and percentile ranks recomputed in the included sample. The tables below preserve ranks defined before distance exclusions; these are distinct exercises.

**Exclusions at 5.0 km.** National income deciles of all income-eligible tracts, including those without a geographic link:

| Decile | Eligible | Beyond limit | Share | No place/boundary |
| --- | --- | --- | --- | --- |
| 1 | 44,954 | 10,827 | 24.08% | 5 |
| 2 | 44,953 | 6,546 | 14.56% | 6 |
| 3 | 44,953 | 3,934 | 8.75% | 3 |
| 4 | 44,953 | 3,370 | 7.50% | 7 |
| 5 | 44,953 | 3,253 | 7.24% | 6 |
| 6 | 44,953 | 3,289 | 7.32% | 6 |
| 7 | 44,953 | 3,055 | 6.80% | 2 |
| 8 | 44,953 | 2,707 | 6.02% | 1 |
| 9 | 44,953 | 1,852 | 4.12% | 2 |
| 10 | 44,953 | 650 | 1.45% | 0 |

| Situation | Eligible | Beyond limit | Share | No place/boundary |
| --- | --- | --- | --- | --- |
| Rural | 102,167 | 37,299 | 36.51% | 19 |
| Urban | 347,364 | 2,184 | 0.63% | 19 |

**Crossings under fixed ranks.** The first crossing may differ from a crossing sustained through the end of the distribution:

| Limit | First percentile | First sustained percentile |
| --- | --- | --- |
| 1 km | 40 | 40 |
| 2 km | 38 | 38 |
| 5 km | 37 | 37 |
| 10 km | 37 | 37 |
| Unlimited | 37 | 37 |

**Maximum method differences**, at the main distance limit, comparing the same percentile index under each definition:

| Reference | Comparison | Candidate | Maximum (pp) | Compared percentiles |
| --- | --- | --- | --- | --- |
| Original reranked | Fixed ranks | Lula | 2.90 | 100 |
| Original reranked | Fixed ranks | Flávio Bolsonaro | 2.59 | 100 |
| Original reranked | V06001 weighted | Lula | 3.48 | 100 |
| Original reranked | V06001 weighted | Flávio Bolsonaro | 3.04 | 100 |
| Original reranked | Ratio of sums | Lula | 3.90 | 100 |
| Original reranked | Ratio of sums | Flávio Bolsonaro | 3.37 | 100 |
| Original reranked | IDW k3 | Lula | 2.52 | 100 |
| Original reranked | IDW k3 | Flávio Bolsonaro | 2.25 | 100 |
| Fixed ranks | IDW k3 | Lula | 0.51 | 100 |
| Fixed ranks | IDW k3 | Flávio Bolsonaro | 0.45 | 100 |

V06001 weights household heads, not voters. The ratio-of-sums method preserves the votes of used polling places and allocates them across linked tracts in proportion to V06001; this approximation does not identify voters’ residences. IDW k3 also measures proximity rather than actual electoral registration.

In the exploratory bootstrap of 500 municipality resamples, 500 replicates had an observed crossing. The 2.5%, 50% and 97.5% quantiles of the first crossing percentile were 33.0, 36.0 and 39.0. Percentile bands are pointwise, not simultaneous; they measure neither census sampling error nor uncertainty in the spatial linkage.

The independent geometric audit passed 2,000 of 2,000 sampled tracts and 2,000 k3 checks. EPSG:5880 projection disagreement remained a diagnostic in 502 cases and did not replace the geodesic reference.

818 polling places with coordinates outside their municipality in the 2022 boundary layer were excluded, containing 269,462 votes in the national denominator (0.23%). Income and boundaries from 2022 do not capture every change through the 2026 election.

V06004 covers household heads with income. ResponsavelRenda does not provide the count of this group, so the share of heads without income is unavailable. V06002 counts residents and cannot recover that share.

**Approximate visual Folha reference.** Mean absolute error over readable points is:

| Candidate | Readable points | MAE (pp) |
| --- | --- | --- |
| Flávio Bolsonaro (PL) | 6 | 0.82 |
| Lula (PT) | 7 | 1.19 |

The reference points come from manual visual readings and the image tooltip, including rounding and obscured regions. This MAE neither validates the complete curve nor establishes equivalence to Folha’s method.
