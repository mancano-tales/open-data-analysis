#!/usr/bin/env node
'use strict';
// Content gate against small state checkpoints. It does NOT rehash archives,
// recalculate RDS/ZIP provenance, or certify that checkpoints are still current.
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const { UFS } = require('./acceptance-2026.cjs');
const { evaluateHistorical, expectedPresidential, readLocalPayload } = require('./acceptance-historical.cjs');
const reference = item => item.income_reference === undefined ? 'national' : item.income_reference;
const contestKey = row => `${Number(row.year)}-${Number(row.round)}-${String(row.office)}-${String(row.uf).toUpperCase()}`;
const seriesKey = (row, ref = reference(row)) => `${contestKey(row)}${ref === 'state' ? '-state' : ''}`;
const list = (value, name) => { if (value === undefined || value === null) return []; if (!Array.isArray(value)) throw new Error(`${name} must be an array`); return value; };

function evaluateStateHistorical(manifest, loadSeries, loadCheckpoint, { years = [2022, 2018, 2014, 2010], rounds = [1, 2] } = {}) {
  // Retain only the tiny metadata needed for checkpoint comparisons, never
  // candidate-point payloads. The existing historical gate validates those.
  const metadata = new Map();
  const presidency = evaluateHistorical(manifest, url => {
    const payload = loadSeries(url);
    if (payload?.metadata) metadata.set(url, payload.metadata);
    return payload;
  }, { years, rounds, references: ['national', 'state'] });
  const errors = [...presidency.errors], progress = [], checkpoints = [], certified = new Map(), blocked = new Set();
  for (const year of years) for (const uf of UFS) {
    let record;
    try { record = loadCheckpoint(year, uf); }
    catch (error) { errors.push(`${year}/${uf}: checkpoint unreadable: ${error.message}`); continue; }
    if (!record) { checkpoints.push({ year, uf, status: 'missing' }); progress.push(`${year}/${uf}: missing checkpoint`); continue; }
    const status = record.status;
    checkpoints.push({ year, uf, status });
    if (record.version !== 1 || Number(record.year) !== year || record.uf !== uf || !['complete', 'partial', 'failed', 'running'].includes(status)) {
      errors.push(`${year}/${uf}: invalid checkpoint identity/version/status`); continue;
    }
    if (status !== 'complete') progress.push(`${year}/${uf}: checkpoint ${status}`);
    try {
      const checks = list(record.certified, 'certified'), unavailable = list(record.unavailable, 'unavailable');
      if (status === 'complete' && (!checks.length || unavailable.length)) throw new Error('complete checkpoint must contain certified contests and no unavailable contests');
      const validateIdentity = row => {
        const office = String(row.office), round = Number(row.round);
        if (Number(row.year) !== year || row.uf !== uf || ![1, 2].includes(round) || !['3', '5', '6', '7', '8'].includes(office) ||
            (round === 2 && office !== '3') || (office === '7' && uf === 'DF') || (office === '8' && uf !== 'DF')) throw new Error('invalid checkpoint contest scope');
      };
      for (const row of unavailable) {
        validateIdentity(row);
        if (rounds.includes(Number(row.round))) blocked.add(contestKey(row));
      }
      for (const row of checks) {
        validateIdentity(row);
        if (!rounds.includes(Number(row.round))) continue;
        for (const field of ['official_valid_votes', 'computed_valid_votes', 'difference']) if (!Number.isInteger(row[field]) || row[field] < 0) throw new Error(`invalid certified ${field}`);
        if (row.difference !== 0 || row.official_valid_votes !== row.computed_valid_votes || row.computed_valid_votes === 0 || row.reconciled === false) throw new Error('checkpoint certified votes do not reconcile to positive totals');
        const id = contestKey(row);
        if (certified.has(id)) throw new Error('duplicate certified contest; cannot pool election identities');
        certified.set(id, { ...row, source_md5: record.source_md5 });
      }
    } catch (error) { errors.push(`${year}/${uf}: ${error.message}`); }
  }
  for (const id of blocked) if (certified.has(id)) errors.push(`${id}: contest both certified and unavailable`);
  const published = new Map();
  for (const item of Array.isArray(manifest?.series) ? manifest.series : []) {
    if (!years.includes(Number(item.year)) || String(item.office) === '1' || item.status !== 'available') continue;
    const id = contestKey(item), fullId = seriesKey(item);
    if (blocked.has(id)) errors.push(`${fullId}: unavailable checkpoint contest must not be published`);
    const check = certified.get(id), checkpoint = checkpoints.find(row => row.year === Number(item.year) && row.uf === item.uf);
    if (!check) {
      if (!checkpoint || ['missing', 'running', 'failed'].includes(checkpoint.status)) progress.push(`${fullId}: published contest has no completed checkpoint certification`);
      else errors.push(`${fullId}: published contest absent from checkpoint certified contests`);
      continue;
    }
    if (presidency.errors.some(error => error.startsWith(`${fullId}:`))) continue;
    const meta = metadata.get(item.url);
    if (!meta) continue;
    const audit = meta.vote_audit;
    if (!audit || audit.status !== 'reconciled' || audit.difference !== 0 ||
        audit.official_valid_votes !== check.official_valid_votes || audit.computed_valid_votes !== check.computed_valid_votes) {
      errors.push(`${fullId}: published audit totals differ from certified checkpoint`); continue;
    }
    if (meta.vote_source_md5 !== undefined && check.source_md5 !== undefined && meta.vote_source_md5 !== check.source_md5) {
      errors.push(`${fullId}: stored vote source digest differs from checkpoint (digest not recomputed)`); continue;
    }
    published.set(fullId, item);
  }
  const expected = [...certified.values()].flatMap(row => ['national', 'state'].map(ref => seriesKey(row, ref)));
  const missing = expected.filter(id => !published.has(id));
  const byYear = years.map(year => ({ year,
    checkpoint_statuses: Object.fromEntries(['complete', 'partial', 'failed', 'running', 'missing'].map(status => [status, checkpoints.filter(row => row.year === year && row.status === status).length])),
    certified_contests: [...certified.values()].filter(row => Number(row.year) === year).length,
    expected_series: expected.filter(id => id.startsWith(`${year}-`)).length,
    available_series: [...published.keys()].filter(id => id.startsWith(`${year}-`)).length,
    unavailable_contests: [...blocked].filter(id => id.startsWith(`${year}-`)).length }));
  const complete = presidency.complete && !errors.length && !progress.length && !missing.length;
  return { phase: 'historical-presidency-and-certified-state-contests', state: errors.length ? 'INVALID' : complete ? 'COMPLETE' : 'IN_PROGRESS', complete,
    validation_scope: 'Published aggregate content compared with stored checkpoint certification; archive/RDS/source provenance is NOT independently recertified',
    archive_fingerprints_rehashed: false, years, rounds,
    presidency: { complete: presidency.complete, available: presidency.available, expected: presidency.expected, missing: presidency.missing },
    state_contests: { certified: certified.size, expected_series: expected.length, available_series: published.size, unavailable_contests: blocked.size, missing_series: missing, by_year: byYear },
    progress, errors };
}

function selfTest() {
  const checkpoints = new Map(), payloads = new Map(), records = [];
  const publish = row => {
    const income_reference = reference(row), item = { ...row, income_reference, id: seriesKey(row), url: `${seriesKey(row)}.json`, status: 'available' };
    records.push(item);
    payloads.set(item.url, { metadata: { year: row.year, round: row.round, office: row.office, uf: row.uf, income_reference, income_year: 2022,
      percentile_scope: income_reference === 'state' ? row.uf : 'BR', coverage: { included_tracts: 100, total_tracts: 100, observed_places: 1 },
      vote_audit: { status: 'reconciled', official_valid_votes: row.official_valid_votes || 100, computed_valid_votes: row.computed_valid_votes || 100, difference: 0,
        checked_units: 1, ufs: row.uf === 'BR' ? UFS : row.uf } }, candidates: [{ id: '13' }, { id: '22' }],
      points: ['13', '22'].flatMap(candidate_id => Array.from({ length: 100 }, (_, i) => ({ centil: i + 1, renda_sm: 1,
        n_setores: 1, n_setores_total: 1, candidate_id, votacao: candidate_id === '13' ? 40 : 60 }))) });
  };
  for (const row of expectedPresidential([2022], [1, 2], ['national', 'state'])) publish(row);
  for (const uf of UFS) {
    const row = { year: 2022, round: 1, election_id: 546, office: 3, uf, official_valid_votes: 100, computed_valid_votes: 100, difference: 0, reconciled: true };
    const certified = [row];
    if (uf === 'SP') certified.push({ ...row, round: 2, election_id: 547, official_valid_votes: 120, computed_valid_votes: 120 });
    if (uf === 'SP') certified.push({ ...row, office: 6, official_valid_votes: 90, computed_valid_votes: 90 });
    checkpoints.set(uf, { version: 1, year: 2022, uf, status: 'complete', certified, unavailable: [] });
    for (const contest of certified) for (const income_reference of ['national', 'state']) publish({ ...contest, income_reference });
  }
  const run = () => evaluateStateHistorical({ schema_version: 1, series: records }, url => payloads.get(url), (year, uf) => checkpoints.get(uf), { years: [2022] });
  assert.equal(run().complete, true); assert.equal(run().state_contests.expected_series, 58); // Only SP has a runoff; it also certifies one deputy contest.
  const saved = checkpoints.get('AC'); checkpoints.delete('AC');
  assert.equal(run().state, 'IN_PROGRESS'); assert.equal(run().complete, false); checkpoints.set('AC', saved);
  const removed = records.splice(records.findIndex(x => x.id === '2022-1-3-SP-state'), 1)[0];
  assert.equal(run().state, 'IN_PROGRESS'); assert(run().state_contests.missing_series.includes(removed.id)); records.push(removed);
  const deputyItems = records.filter(x => x.id.startsWith('2022-1-6-SP'));
  const withoutDeputies = records.filter(x => !x.id.startsWith('2022-1-6-SP'));
  const missingOffice = evaluateStateHistorical({ schema_version: 1, series: withoutDeputies }, url => payloads.get(url), (year, uf) => checkpoints.get(uf), { years: [2022] });
  assert.equal(missingOffice.state, 'IN_PROGRESS');
  assert(deputyItems.every(item => missingOffice.state_contests.missing_series.includes(item.id)));
  const partial = structuredClone(saved); partial.status = 'partial';
  const blocked = partial.certified.pop(); partial.unavailable.push(blocked); checkpoints.set('AC', partial);
  assert.equal(run().state, 'INVALID'); assert(run().errors.some(x => x.includes('unavailable')));
  const filtered = records.filter(x => !x.id.startsWith('2022-1-3-AC'));
  const partialRun = evaluateStateHistorical({ schema_version: 1, series: filtered }, url => payloads.get(url), (year, uf) => checkpoints.get(uf), { years: [2022] });
  assert.equal(partialRun.state, 'IN_PROGRESS'); assert.equal(partialRun.complete, false); checkpoints.set('AC', saved);
  const payload = payloads.get(removed.url), original = payload.metadata.vote_audit;
  payload.metadata.vote_audit = { ...original, official_valid_votes: 999, computed_valid_votes: 999 };
  assert.equal(run().state, 'INVALID'); payload.metadata.vote_audit = original;
  const fakeRunoff = { ...removed, round: 2, uf: 'AC' }; fakeRunoff.id = seriesKey(fakeRunoff); fakeRunoff.url = 'fake-runoff.json';
  payloads.set(fakeRunoff.url, { ...payload, metadata: { ...payload.metadata, round: 2, uf: 'AC', percentile_scope: 'AC', vote_audit: { ...original, ufs: 'AC' } } });
  records.push(fakeRunoff); assert.equal(run().state, 'INVALID'); records.pop();
  const failed = { version: 1, year: 2022, uf: 'AC', status: 'failed', error: 'fixture failure' }; checkpoints.set('AC', failed);
  assert.equal(run().state, 'IN_PROGRESS'); checkpoints.set('AC', saved);
  assert.equal(run().complete, true);
  console.log('PASS state-history self-test: checkpoint-derived runoff, missing/partial/failed checkpoints, missing office reference, blocked publication and checkpoint vote totals');
}

function main(args) {
  const positional = args.filter(arg => !arg.startsWith('--'));
  const known = arg => ['--progress', '--json', '--self-test'].includes(arg) || /^--(?:years|rounds|state-audits)=/.test(arg);
  if (positional.length > 1 || args.some(arg => arg.startsWith('--') && !known(arg))) throw new Error('Usage: node acceptance-state-historical.cjs [data-directory] --state-audits=checkpoint-root [--years=2022] [--rounds=1,2] [--progress] [--json] [--self-test]');
  if (args.includes('--self-test')) return selfTest();
  const option = (name, fallback) => { const arg = args.find(x => x.startsWith(`--${name}=`)); return arg ? arg.slice(name.length + 3) : fallback; };
  const years = option('years', '2022,2018,2014,2010').split(',').map(Number), rounds = option('rounds', '1,2').split(',').map(Number);
  if (!years.length || years.some(x => !Number.isInteger(x) || x < 1989) || new Set(years).size !== years.length || !rounds.length || rounds.some(x => ![1, 2].includes(x)) || new Set(rounds).size !== rounds.length) throw new Error('invalid selected years/rounds');
  const dataDir = path.resolve(positional[0] || path.join(__dirname, '../../../posts/explorador-eleitoral/data'));
  const checkpointRoot = path.resolve(option('state-audits', 'data-raw/eleicoes-renda/historico/historical-status'));
  const manifest = JSON.parse(fs.readFileSync(path.join(dataDir, 'manifest.json'), 'utf8').replace(/^\uFEFF/, ''));
  const result = evaluateStateHistorical(manifest, url => readLocalPayload(dataDir, url), (year, uf) => {
    const file = path.join(checkpointRoot, String(year), `${uf}.json`);
    return fs.existsSync(file) ? JSON.parse(fs.readFileSync(file, 'utf8').replace(/^\uFEFF/, '')) : null;
  }, { years, rounds });
  if (args.includes('--json')) console.log(JSON.stringify(result, null, 2));
  else {
    console.log(`${result.state}: presidency ${result.presidency.available}/${result.presidency.expected}; checkpoint-certified state series ${result.state_contests.available_series}/${result.state_contests.expected_series}; complete=${result.complete}`);
    for (const row of result.state_contests.by_year) console.log(`  ${row.year}: ${JSON.stringify(row.checkpoint_statuses)}; certified contests=${row.certified_contests}, state series=${row.available_series}/${row.expected_series}`);
    for (const error of result.errors) console.error(`ERROR: ${error}`);
    console.log(result.validation_scope);
  }
  process.exitCode = result.errors.length || (!args.includes('--progress') && !result.complete) ? 1 : 0;
}
module.exports = { evaluateStateHistorical };
if (require.main === module) { try { main(process.argv.slice(2)); } catch (error) { console.error(`INVALID: ${error.message}`); process.exitCode = 1; } }
