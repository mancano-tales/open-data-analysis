#!/usr/bin/env node
'use strict';
// Read-only historical gate. Expected coverage is presidential, not all state
// offices; every selected-year published series is nevertheless validated.
const fs = require('node:fs');
const path = require('node:path');
const zlib = require('node:zlib');
const os = require('node:os');
const assert = require('node:assert/strict');
const { UFS } = require('./acceptance-2026.cjs');
const ref = item => item.income_reference === undefined ? 'national' : item.income_reference;
const key = item => `${Number(item.year)}-${Number(item.round)}-${String(item.office)}-${String(item.uf).toUpperCase()}${ref(item) === 'state' ? '-state' : ''}`;
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const integer = (x, name) => { if (!Number.isInteger(x) || x < 0) throw new Error(`${name} must be a nonnegative integer`); };
const close = (a, b) => Math.abs(a - b) <= 1e-7 * Math.max(1, Math.abs(a), Math.abs(b));

function readLocalPayload(dataDir, url) {
  if (typeof url !== 'string' || !url || /^[a-z][a-z\d+.-]*:/i.test(url) || url.startsWith('/') || url.includes('\\') || /[?#]/.test(url)) {
    throw new Error('expected a relative local JSON or JSON.gz URL');
  }
  const base = fs.realpathSync(dataDir), target = path.resolve(base, decodeURIComponent(url));
  const relative = path.relative(base, target);
  if (!relative || relative.startsWith('..') || path.isAbsolute(relative) || !/\.json(?:\.gz)?$/.test(target)) throw new Error('series URL leaves data directory or has unsupported format');
  const realTarget = fs.realpathSync(target), realRelative = path.relative(base, realTarget);
  if (realRelative.startsWith('..') || path.isAbsolute(realRelative)) throw new Error('series symlink leaves data directory');
  let bytes = fs.readFileSync(realTarget);
  if (target.endsWith('.gz')) bytes = zlib.gunzipSync(bytes, { maxOutputLength: 256 * 1024 * 1024 });
  return JSON.parse(bytes.toString('utf8').replace(/^\uFEFF/, ''));
}

function expectedPresidential(years, rounds, references) {
  const rows = [];
  for (const year of years) for (const round of rounds) for (const income_reference of references) {
    for (const uf of income_reference === 'state' ? UFS : ['BR', ...UFS]) rows.push({ year, round, office: '1', uf, income_reference });
  }
  return rows;
}

function validateSeries(item, series) {
  const id = key(item), reference = ref(item), uf = String(item.uf).toUpperCase(), office = String(item.office);
  if (!['national', 'state'].includes(reference) || (reference === 'state' && uf === 'BR')) throw new Error('invalid income reference/territory');
  if (!Number.isInteger(item.year) || ![1, 2].includes(item.round) || !['BR', ...UFS].includes(uf)) throw new Error('invalid year, round or territory');
  if (!['1', '3', '5', '6', '7', '8'].includes(office) || (office !== '1' && uf === 'BR') || (office === '7' && uf === 'DF') || (office === '8' && uf !== 'DF')) throw new Error('invalid office/territory');
  const meta = series?.metadata;
  if (!meta || key(meta) !== id || ref(meta) !== reference) throw new Error('metadata contest/reference differs from manifest');
  if (meta.percentile_scope !== (reference === 'state' ? uf : 'BR')) throw new Error('wrong percentile_scope for income reference');
  if (meta.income_year !== 2022) throw new Error('income_year must remain 2022');
  const audit = meta.vote_audit;
  if (audit?.status !== 'reconciled') throw new Error('missing reconciled official audit');
  for (const field of ['official_valid_votes', 'computed_valid_votes', 'difference']) integer(audit[field], `audit ${field}`);
  if (audit.difference !== 0 || audit.computed_valid_votes !== audit.official_valid_votes) throw new Error('official audit differs');
  integer(audit.checked_units, 'audit checked_units');
  if (!audit.checked_units) throw new Error('empty official audit');
  const auditUfs = Array.isArray(audit.ufs) ? audit.ufs : [audit.ufs];
  if (uf === 'BR' ? !UFS.every(state => auditUfs.includes(state)) : !auditUfs.includes(uf)) throw new Error('official audit misses contest territory');
  const coverage = meta.coverage;
  if (!coverage) throw new Error('missing coverage');
  integer(coverage.total_tracts, 'total_tracts'); integer(coverage.included_tracts, 'included_tracts');
  if (!coverage.included_tracts || coverage.included_tracts > coverage.total_tracts) throw new Error('invalid aggregate coverage');
  if (!Array.isArray(series.candidates) || !series.candidates.length || !Array.isArray(series.points)) throw new Error('missing candidate/point arrays');
  const candidates = series.candidates.map(c => String(c.id));
  if (candidates.some(id => id === 'undefined' || id === 'null') || new Set(candidates).size !== candidates.length) throw new Error('missing/duplicate candidate ids');
  if (series.points.length !== candidates.length * 100) throw new Error('each candidate must have 100 points');
  const candidateSet = new Set(candidates), observed = new Set(), bins = new Map(), totals = new Map();
  for (const point of series.points) {
    const candidate = String(point.candidate_id), bin = point.centil, identity = `${candidate}:${bin}`;
    if (!candidateSet.has(candidate) || !Number.isInteger(bin) || bin < 1 || bin > 100 || observed.has(identity)) throw new Error('unknown/duplicate candidate percentile');
    observed.add(identity);
    integer(point.n_setores, 'point n_setores'); integer(point.n_setores_total, 'point n_setores_total');
    if (point.n_setores > point.n_setores_total) throw new Error('point coverage exceeds universe');
    if (point.n_setores_total ? typeof point.renda_sm !== 'number' || !Number.isFinite(point.renda_sm) || point.renda_sm <= 0 : point.renda_sm !== null) throw new Error('invalid full-universe income mean');
    if (point.n_setores ? typeof point.votacao !== 'number' || !Number.isFinite(point.votacao) || point.votacao < 0 || point.votacao > 100 : point.votacao !== null) throw new Error('vote percentage must be null exactly when uncovered');
    const values = [point.n_setores, point.n_setores_total, point.renda_sm];
    if (bins.has(bin) && !same(bins.get(bin), values)) throw new Error('income/coverage changes between candidates in the same percentile');
    bins.set(bin, values);
    if (point.n_setores) totals.set(bin, (totals.get(bin) || 0) + point.votacao);
  }
  let included = 0, total = 0, incomeTotal = 0;
  for (const [n, nTotal, income] of bins.values()) { included += n; total += nTotal; incomeTotal += nTotal ? nTotal * income : 0; }
  if (bins.size !== 100 || included !== coverage.included_tracts || total !== coverage.total_tracts) throw new Error('point coverage totals differ from metadata');
  for (const sum of totals.values()) {
    if (sum > 100 + 1e-5 || (['1', '3', '5'].includes(office) && Math.abs(sum - 100) > 1e-5)) throw new Error('candidate shares do not reconcile with complete denominator');
  }
  return { id, reference, year: item.year, round: item.round, office, uf,
    candidate_ids: [...candidates].sort(), included_tracts: included, total_tracts: total,
    mean_income_sm: incomeTotal / total, observed_places: coverage.observed_places,
    official_valid_votes: audit.official_valid_votes, computed_valid_votes: audit.computed_valid_votes,
    difference: audit.difference, checked_units: audit.checked_units,
    audit_ufs: [...auditUfs].sort(), provisional: Boolean(audit.provisional) };
}

function evaluateHistorical(manifest, loadSeries, { years = [2022, 2018, 2014, 2010], rounds = [1, 2], references = ['national', 'state'] } = {}) {
  const errors = [], summaries = new Map(), ids = new Set(), available = new Set();
  const expected = expectedPresidential(years, rounds, references);
  if (manifest?.schema_version !== 1 || !Array.isArray(manifest?.series)) errors.push('invalid manifest schema/series');
  for (const item of manifest?.series || []) {
    if (!years.includes(Number(item.year))) continue;
    const id = key(item);
    if (item.id !== id || ids.has(id)) { errors.push(`${id}: noncanonical/duplicate id`); continue; }
    ids.add(id);
    if (item.status !== 'available') continue;
    try {
      const summary = validateSeries(item, loadSeries(item.url));
      summaries.set(id, summary); available.add(id);
    } catch (error) { errors.push(`${id}: ${error.message}`); }
  }
  let pairs = 0;
  const pairFields = ['candidate_ids', 'included_tracts', 'total_tracts', 'observed_places',
    'official_valid_votes', 'computed_valid_votes', 'difference', 'checked_units', 'audit_ufs', 'provisional'];
  for (const summary of summaries.values()) {
    if (summary.reference !== 'state') continue;
    const national = summaries.get(summary.id.replace(/-state$/, ''));
    if (!national) continue;
    pairs++;
    for (const field of pairFields) if (!same(national[field], summary[field])) errors.push(`${summary.id}: paired national/state ${field} differs`);
    if (!close(national.mean_income_sm, summary.mean_income_sm)) errors.push(`${summary.id}: paired full-universe mean income differs`);
  }
  const missing = expected.map(key).filter(id => !available.has(id));
  const byYear = years.map(year => ({ year, expected: expected.filter(x => x.year === year).length,
    available: expected.filter(x => x.year === year && available.has(key(x))).length,
    by_reference: references.map(income_reference => ({ income_reference,
      expected: expected.filter(x => x.year === year && ref(x) === income_reference).length,
      available: expected.filter(x => x.year === year && ref(x) === income_reference && available.has(key(x))).length })) }));
  return { phase: 'historical-presidency', state: errors.length ? 'INVALID' : missing.length ? 'IN_PROGRESS' : 'COMPLETE',
    complete: !errors.length && !missing.length, years, rounds, references,
    expected: expected.length, available: expected.length - missing.length,
    checked_files: summaries.size, paired_references: pairs,
    provisional_series: [...summaries.values()].filter(x => x.provisional).length,
    other_published_offices_checked: [...summaries.values()].filter(x => x.office !== '1').length,
    by_year: byYear, missing, errors };
}

function selfTest() {
  const item = (uf, reference) => ({ year: 2022, round: 1, office: '1', uf, income_reference: reference,
    id: `2022-1-1-${uf}${reference === 'state' ? '-state' : ''}`, url: `${uf}-${reference}.json`, status: 'available' });
  const make = item => ({ metadata: { ...item, income_year: 2022, percentile_scope: ref(item) === 'state' ? item.uf : 'BR',
    coverage: { included_tracts: 100, total_tracts: 100, observed_places: 1 },
    vote_audit: { status: 'reconciled', official_valid_votes: 100, computed_valid_votes: 100, difference: 0, checked_units: 1, ufs: item.uf } },
    candidates: [{ id: '13' }, { id: '22' }], points: ['13', '22'].flatMap(candidate_id => Array.from({ length: 100 }, (_, index) => ({
      centil: index + 1, renda_sm: 1, n_setores: 1, n_setores_total: 1, candidate_id, votacao: candidate_id === '13' ? 40 : 60 }))) });
  const items = [item('SP', 'national'), item('SP', 'state')], data = new Map(items.map(x => [x.url, make(x)]));
  const run = () => evaluateHistorical({ schema_version: 1, series: items }, url => data.get(url), { years: [2022], rounds: [1] });
  assert.equal(run().state, 'IN_PROGRESS'); assert.equal(run().paired_references, 1); assert.equal(run().available, 2);
  const original = structuredClone(data.get(items[1].url));
  const mutate = change => { const copy = structuredClone(original); change(copy); data.set(items[1].url, copy); assert.equal(run().state, 'INVALID'); data.set(items[1].url, original); };
  mutate(s => { s.points[100].n_setores = 0; s.points[100].votacao = null; });
  mutate(s => { s.metadata.round = 2; });
  mutate(s => { s.metadata.percentile_scope = 'BR'; });
  mutate(s => { s.metadata.vote_audit.difference = 1; });
  mutate(s => { s.metadata.vote_audit.official_valid_votes = 200; s.metadata.vote_audit.computed_valid_votes = 200; });
  mutate(s => { s.points[0].votacao = null; });
  mutate(s => { s.points[0].renda_sm = 2; });
  const complete = expectedPresidential([2022], [1], ['national', 'state']).map(x => ({ ...x, id: key(x), url: `${key(x)}.json`, status: 'available' }));
  const payloads = new Map(complete.map(x => { const s = make(x); if (x.uf === 'BR') s.metadata.vote_audit.ufs = UFS; return [x.url, s]; }));
  assert.equal(evaluateHistorical({ schema_version: 1, series: complete }, url => payloads.get(url), { years: [2022], rounds: [1] }).complete, true);
  assert.throws(() => readLocalPayload(__dirname, '../outside.json'));
  assert.throws(() => readLocalPayload(__dirname, '%2e%2e/outside.json.gz'));
  const folder = fs.mkdtempSync(path.join(os.tmpdir(), 'historical-gate-'));
  try {
    const fixture = make(items[0]);
    fs.writeFileSync(path.join(folder, 'fixture.json.gz'), zlib.gzipSync(Buffer.from(JSON.stringify(fixture))));
    assert.deepEqual(readLocalPayload(folder, 'fixture.json.gz'), fixture);
    fs.writeFileSync(path.join(folder, 'broken.json.gz'), 'not gzip');
    assert.throws(() => readLocalPayload(folder, 'broken.json.gz'));
  } finally {
    const resolved = fs.realpathSync(folder), temp = fs.realpathSync(os.tmpdir());
    assert.equal(path.dirname(resolved), temp);
    assert(path.basename(resolved).startsWith('historical-gate-'));
    fs.rmSync(resolved, { recursive: true, force: true });
  }
  console.log('PASS historical self-test: year/round/reference metadata, paired coverage/audit, candidate-independent centiles, null handling, gzip, completion and local path guards');
}

function main(args) {
  const positional = args.filter(arg => !arg.startsWith('--'));
  const allowed = arg => ['--self-test', '--progress', '--json', '--national-only'].includes(arg) || /^--(?:years|rounds)=/.test(arg);
  if (positional.length > 1 || args.some(arg => arg.startsWith('--') && !allowed(arg))) throw new Error('Usage: node acceptance-historical.cjs [data-directory] [--years=2022,2018,2014,2010] [--rounds=1,2] [--national-only] [--progress] [--json] [--self-test]');
  if (args.includes('--self-test')) return selfTest();
  const values = (name, defaults) => { const entry = args.find(x => x.startsWith(`--${name}=`)); return entry ? entry.split('=')[1].split(',').map(Number) : defaults; };
  const years = values('years', [2022, 2018, 2014, 2010]), rounds = values('rounds', [1, 2]);
  if (!years.length || years.some(x => !Number.isInteger(x) || x < 1989) || new Set(years).size !== years.length || !rounds.length || rounds.some(x => ![1, 2].includes(x)) || new Set(rounds).size !== rounds.length) throw new Error('invalid requested years/rounds');
  const dataDir = path.resolve(positional[0] || path.join(__dirname, '../../../posts/explorador-eleitoral/data'));
  const manifest = JSON.parse(fs.readFileSync(path.join(dataDir, 'manifest.json'), 'utf8').replace(/^\uFEFF/, ''));
  const result = evaluateHistorical(manifest, url => readLocalPayload(dataDir, url), { years, rounds, references: args.includes('--national-only') ? ['national'] : ['national', 'state'] });
  if (args.includes('--json')) console.log(JSON.stringify(result, null, 2));
  else {
    console.log(`${result.state}: historical presidency ${result.available}/${result.expected}; ${result.checked_files} files checked, ${result.paired_references} paired national/state; complete=${result.complete}`);
    for (const row of result.by_year) console.log(`  ${row.year}: ${row.available}/${row.expected}`);
    for (const error of result.errors) console.error(`ERROR: ${error}`);
    if (result.missing.length) console.log(`Missing: ${result.missing.join(', ')}`);
  }
  process.exitCode = result.errors.length || (!args.includes('--progress') && result.missing.length) ? 1 : 0;
}

module.exports = { readLocalPayload, validateSeries, expectedPresidential, evaluateHistorical };
if (require.main === module) { try { main(process.argv.slice(2)); } catch (error) { console.error(`INVALID: ${error.message}`); process.exitCode = 1; } }
