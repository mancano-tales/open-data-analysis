// Pure-function tests for concentration.js, with hand-solved examples.
const fs = require('node:fs'), path = require('node:path'), assert = require('node:assert/strict');
const c = require('./concentration.js');
const close = (a, b, msg) => assert.ok(Math.abs(a - b) < 1e-12, `${msg}: ${a} != ${b}`);
const pts = (ys, ns) => ys.map((y, i) => ({centil: i + 1, votacao: y, n_setores: ns ? ns[i] : 10}));

// Two equal groups: R = 0.25, 0.75. y = (0, 10): mu = 5, sum f*y*R = 3.75, CI = 2*3.75/5 - 1 = 0.5.
close(c.concentrationIndex(pts([0, 10])), 0.5, 'rich-concentrated');
close(c.concentrationIndex(pts([10, 0])), -0.5, 'poor-concentrated');
close(c.concentrationIndex(pts([7, 7, 7, 7])), 0, 'equal shares');
// Unequal group sizes: f = (0.25, 0.75), R = (0.125, 0.625), y = (4, 8): mu = 7,
// sum = 0.25*4*0.125 + 0.75*8*0.625 = 3.875, CI = 2*3.875/7 - 1 = 0.107142857...
close(c.concentrationIndex(pts([4, 8], [1, 3])), 2 * 3.875 / 7 - 1, 'weighted groups');
// Same as the covariance form 2*cov(y, R)/mu with tract weights.
{
  const ys = Array.from({length: 100}, (_, i) => 5 + 0.3 * i + (i % 7)), ns = Array.from({length: 100}, (_, i) => 1 + (i % 5));
  const W = ns.reduce((a, b) => a + b, 0); let cum = 0;
  const f = ns.map(n => n / W), R = f.map(v => (cum += v) - v / 2);
  const mu = ys.reduce((s, y, i) => s + f[i] * y, 0), rbar = R.reduce((s, r, i) => s + f[i] * r, 0);
  const cov = ys.reduce((s, y, i) => s + f[i] * (y - mu) * (R[i] - rbar), 0);
  close(c.concentrationIndex(pts(ys, ns)), 2 * cov / mu, 'covariance form');
}
// Null or empty groups are skipped, never treated as zero.
close(c.concentrationIndex([...pts([0, 10]), {centil: 3, votacao: null, n_setores: 0}]), 0.5, 'null group skipped');
assert.equal(c.concentrationIndex(pts([0, 0])), null, 'no votes: undefined index');
assert.equal(c.concentrationIndex([]), null);

// Party labels from the manifest when the series has none.
const manifest = {parties: {'2022': {'13': 'PT', '22': 'PL'}}};
assert.equal(c.partyOf({id: '1311'}, manifest, 2022), 'PT');
assert.equal(c.partyOf({id: '1311', party: 'XX'}, manifest, 2022), 'XX');
assert.equal(c.partyOf({id: '9911'}, manifest, 2022), '99');

const rows = [{id: '1311', name: 'Ana Silva', party: 'PT', ci: 0.2, mean_share: 1},
  {id: '2211', name: 'José Souza', party: 'PL', ci: -0.1, mean_share: 3}, {id: '1322', name: 'Bia', party: 'PT', ci: null, mean_share: 0}];
assert.deepEqual(c.filterRows(rows, {party: 'PT'}).map(r => r.id), ['1311', '1322']);
assert.deepEqual(c.filterRows(rows, {query: 'jose'}).map(r => r.id), ['2211'], 'accent-insensitive search');
assert.deepEqual(c.filterRows(rows, {query: '2211'}).map(r => r.id), ['2211'], 'search by number');
assert.deepEqual(c.sortRows(rows, 'ci', 'desc').map(r => r.id), ['1311', '2211', '1322'], 'nulls last');
assert.deepEqual(c.sortRows(rows, 'ci', 'asc').map(r => r.id), ['2211', '1311', '1322']);
assert.match(c.toCsv(rows).split('\n')[0], /indice_concentracao/);

// Real data sanity: 2022 presidential runoff, national reference.
const series = JSON.parse(fs.readFileSync(path.join(__dirname, 'data/series/2022-2-1-BR.json'), 'utf8'));
const real = Object.fromEntries(c.candidateRows(series, manifest, 2022).map(r => [r.id, r]));
const withLegend = {...series, candidates: [...series.candidates, {id: '13', name: 'Legenda', vote_type: 'party'}]};
assert.equal(c.candidateRows(withLegend, manifest, 2022).length, series.candidates.length,
  'party-list votes do not appear as candidates in the concentration table');
assert.ok(real['13'].ci < 0 && real['22'].ci > 0, `Lula concentrated in poorer tracts, Bolsonaro in richer: ${real['13'].ci}, ${real['22'].ci}`);
assert.equal(real['13'].party, 'PT');
console.log('test-concentration OK', real['13'].ci.toFixed(3), real['22'].ci.toFixed(3));
assert.deepEqual(c.filterRows(rows, {minShare: 1}).map(r => r.id), ['1311', '2211'], 'minimum mean share');
console.log('test-concentration minShare OK');

// Wagstaff (2005): y = (0, 10)% in two equal groups: CI = 0.5, mu = 5% = 0.05, CI_W = 0.5 / 0.95.
close(c.wagstaffIndex(pts([0, 10])), 0.5 / 0.95, 'Wagstaff normalization');
close(c.wagstaffIndex(pts([7, 7])), 0, 'Wagstaff of equal shares');
// A 50% candidate fully in the richer half: CI = 0.5 (bound 1 - mu), CI_W = 1.
close(c.wagstaffIndex(pts([0, 100])), 1, 'Wagstaff reaches the bound');
assert.equal(c.wagstaffIndex(pts([100, 100])), null, 'mu = 100%: undefined');
const realRows = c.candidateRows(series, manifest, 2022);
for (const r of realRows) if (r.ci != null) assert.ok(Math.abs(r.ci) >= Math.abs(r.ci_standard) - 1e-12);
assert.match(c.toCsv(realRows).split('\n')[0], /indice_concentracao_wagstaff,indice_concentracao_padrao/);
console.log('test-concentration Wagstaff OK');
