// Pure-function tests for compare.js. UI behaviour is covered separately.
const fs = require('node:fs'), path = require('node:path'), assert = require('node:assert/strict');
const c = require('./compare.js');
const data = file => JSON.parse(fs.readFileSync(path.join(__dirname, 'data', 'series', file), 'utf8'));

assert.equal(c.partyNumber({id: '13'}), '13');
assert.equal(c.partyNumber({id: '131'}), '13');
assert.equal(c.partyNumber({id: '13111'}), '13');
assert.equal(c.partyNumber({id: 'lula'}), null);
assert.equal(c.partyNumber({id: 'lula', party_number: 13}), '13');

const point = (id, centil, votacao, n = 10) => ({candidate_id: id, centil, votacao, n_setores: n, n_setores_total: 12, renda_sm: 1});
const synthetic = shares => ({candidates: [{id: '1311'}, {id: '1322'}, {id: '13', vote_type: 'party'}, {id: '4511'}],
  points: ['1311', '1322', '13', '4511'].flatMap((id, k) => Array.from({length: 100}, (_, i) => point(id, i + 1, i === 49 ? null : shares[k])))});
const curve = c.partyCurve(synthetic([10, 5, 1, 30]), '13');
assert.equal(curve.length, 100);
assert.equal(curve[0].votacao, 16, 'candidates plus legend votes of the same party add up');
assert.equal(curve[49].votacao, null, 'uncovered centile stays null, never zero');
assert.equal(c.partyCurve(synthetic([1, 1, 1, 1]), '99'), null);

const diff = c.differenceCurve(c.partyCurve(synthetic([10, 5, 1, 30]), '13'), c.partyCurve(synthetic([20, 5, 1, 30]), '13'));
assert.equal(diff[0].diferenca_pp, 10, 'difference is B minus A');
assert.equal(diff[49].diferenca_pp, null);
assert.match(c.toCsv(diff).split('\n')[50], /^50,,,,/);

const base = {id: '2022-1-1-BR', office: '1', uf: 'BR', year: 2022, round: 1};
assert.deepEqual(c.compatible(base, {...base, id: '2026-1-1-BR', year: 2026}), {ok: true});
assert.equal(c.compatible(base, base).reason, 'same');
assert.equal(c.compatible(base, {...base, id: 'x', office: '3'}).reason, 'office');
assert.equal(c.compatible(base, {...base, id: 'x', uf: 'SP'}).reason, 'uf');
assert.equal(c.compatible(base, {...base, id: 'x', income_reference: 'state'}).reason, 'income_reference');
assert.equal(c.compatible(base, {...base, id: 'x', census_year: 2010}).reason, 'census_year');
assert.ok(c.compatible(base, {...base, id: 'x', census_year: 2022}).ok, 'missing census_year means 2022');

const metadata = {income_year: 2022, income_reference: 'national', percentile_scope: 'BR', office: '1', uf: 'BR', year: 2022, round: 1};
assert.ok(c.matchesMetadata({metadata}, base));
assert.ok(c.matchesMetadata({metadata: {...metadata, income_year: undefined, income_reference: undefined}}, base));
for (const key of ['income_year', 'income_reference', 'percentile_scope', 'office', 'uf', 'year', 'round']) {
  assert.equal(c.matchesMetadata({metadata: {...metadata, [key]: 'wrong'}}, base), false, `${key} mismatch`);
}
assert.equal(c.matchesMetadata({metadata: {...metadata, year: 2026}}, base), false, 'swapped election payload');
assert.ok(c.matchesMetadata({metadata: {...metadata, percentile_scope: 'AC', uf: 'AC', income_reference: 'state'}},
  {...base, uf: 'AC', income_reference: 'state'}));
assert.equal(c.matchesMetadata({metadata: {...metadata, percentile_scope: 'BR', uf: 'AC', income_reference: 'state'}},
  {...base, uf: 'AC', income_reference: 'state'}), false);

// Real published data: PT 2022 vs 2026, national presidential series.
const a = c.partyCurve(data('2022-1-1-BR.json'), '13'), b = c.partyCurve(data('2026-1-1-BR.json'), '13');
const real = c.differenceCurve(a, b);
assert.equal(real.length, 100);
for (const r of real) if (r.diferenca_pp != null) assert.ok(Math.abs(r.diferenca_pp - (r.b - r.a)) < 1e-9);
console.log('test-compare OK');
