#!/usr/bin/env node
'use strict';
// Read-only gate: 136 national-income contests, or 271 with --both-references.
// --progress reports missing contests with exit 0, but malformed published data
// always fails. Other years/rounds may coexist without satisfying 2026 coverage.
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const UFS = 'AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO'.split(' ');
const LABELS = { 1: 'Presidente', 3: 'Governador', 5: 'Senador', 6: 'Deputado federal', 7: 'Deputado estadual', 8: 'Deputado distrital' };
const incomeReference = item => item.income_reference === undefined ? 'national' : item.income_reference;
const key = item => `${Number(item.year)}-${Number(item.round)}-${String(item.office)}-${String(item.uf).toUpperCase()}${incomeReference(item) === 'state' ? '-state' : ''}`;
function expectedContests(reference = 'national') {
  if (!['national', 'state'].includes(reference)) throw new Error('Unknown income reference');
  const rows = [];
  for (const uf of ['BR', ...UFS]) rows.push({ year: 2026, round: 1, office: '1', uf });
  for (const office of ['3', '5', '6']) for (const uf of UFS) rows.push({ year: 2026, round: 1, office, uf });
  for (const uf of UFS.filter(uf => uf !== 'DF')) rows.push({ year: 2026, round: 1, office: '7', uf });
  rows.push({ year: 2026, round: 1, office: '8', uf: 'DF' });
  assert.equal(rows.length, 136);
  return reference === 'state' ? rows.filter(item => item.uf !== 'BR').map(item => ({ ...item, income_reference: 'state' })) : rows;
}
function evaluateManifest(manifest, loadSeries, { bothReferences = false } = {}) {
  const errors = [], available = new Set(), ids = new Set();
  // Retain only small comparison summaries. Never retain points or full payloads.
  const summaries = new Map();
  const references = bothReferences ? ['national', 'state'] : ['national'];
  const expected = references.flatMap(reference => expectedContests(reference));
  const records = Array.isArray(manifest?.series) ? manifest.series : [];
  if (manifest?.schema_version !== 1) errors.push('Manifest schema_version must be 1');
  if (!Array.isArray(manifest?.series)) errors.push('Manifest series must be an array');
  let checkedFiles = 0;
  for (const item of records) {
    const id = key(item);
    if (typeof item.id !== 'string' || item.id !== id) errors.push(`Noncanonical or missing series id: ${item.id}`);
    if (ids.has(item.id)) errors.push(`Duplicate series id: ${item.id}`);
    ids.add(item.id);
    const office = String(item.office), uf = String(item.uf).toUpperCase();
    const reference = incomeReference(item);
    if (!['national', 'state'].includes(reference)) errors.push(`${id}: unknown income_reference`);
    if (reference === 'state' && uf === 'BR') errors.push(`${id}: state income reference cannot use BR territory`);
    if (!Number.isInteger(Number(item.year)) || ![1, 2].includes(Number(item.round)) || !['BR', ...UFS].includes(uf)) {
      errors.push(`${id}: invalid year, round or UF`);
    }
    if (['3', '5', '6', '7', '8'].includes(office) && uf === 'BR') errors.push(`${id}: state office cannot use BR territory`);
    if (office === '7' && uf === 'DF') errors.push(`${id}: DF cannot have deputado estadual`);
    if (office === '8' && uf !== 'DF') errors.push(`${id}: deputado distrital must be DF`);
    if (item.status !== 'available') continue;
    if (typeof item.url !== 'string' || !item.url || /^[a-z][a-z\d+.-]*:/i.test(item.url) ||
        item.url.startsWith('/') || item.url.includes('\\') || /[?#]/.test(item.url)) {
      errors.push(`${id}: expected a local relative JSON URL`);
      continue;
    }
    try {
      const series = loadSeries(item.url);
      checkedFiles++;
      const meta = series?.metadata;
      if (!meta || key(meta) !== id) throw new Error('series metadata differs from manifest contest');
      if (incomeReference(meta) !== reference) throw new Error('manifest and series income references differ');
      const expectedScope = reference === 'state' ? uf : 'BR';
      const actualScope = meta.percentile_scope === undefined ? meta.scope : meta.percentile_scope;
      if (actualScope !== expectedScope || (meta.scope !== undefined && meta.scope !== expectedScope)) {
        throw new Error(`income percentile scope must be ${expectedScope}`);
      }
      const audit = meta.vote_audit;
      if (audit?.status !== 'reconciled') throw new Error('missing reconciled official vote audit');
      for (const field of ['official_valid_votes', 'computed_valid_votes', 'difference']) {
        if (typeof audit[field] !== 'number' || !Number.isFinite(audit[field])) throw new Error(`audit ${field} must be finite numeric`);
      }
      if (audit.official_valid_votes < 0 || audit.computed_valid_votes < 0 || audit.difference !== 0 ||
          audit.official_valid_votes !== audit.computed_valid_votes) {
        throw new Error('official vote audit has a nonzero difference');
      }
      if (!Number.isInteger(audit.checked_units) || audit.checked_units < 1) throw new Error('audit checked_units must be positive');
      const auditedUfs = Array.isArray(audit.ufs) ? audit.ufs : [audit.ufs];
      if (uf === 'BR' ? !UFS.every(state => auditedUfs.includes(state)) : !auditedUfs.includes(uf)) {
        throw new Error('audit does not cover the contest territory');
      }
      const coverage = meta.coverage || {};
      for (const field of ['total_tracts', 'included_tracts']) {
        if (coverage[field] !== undefined && coverage[field] !== null &&
            (!Number.isInteger(coverage[field]) || coverage[field] < 0)) throw new Error(`coverage ${field} must be a nonnegative integer`);
      }
      const candidateIds = Array.isArray(series.candidates) ? series.candidates.map(candidate => {
        if (candidate?.id === undefined || candidate.id === null) throw new Error('candidate missing id');
        return String(candidate.id);
      }) : undefined;
      const summary = {
        total_tracts: coverage.total_tracts, included_tracts: coverage.included_tracts,
        candidate_ids: candidateIds === undefined ? undefined : [...new Set(candidateIds)].sort(),
        official_valid_votes: audit.official_valid_votes, computed_valid_votes: audit.computed_valid_votes,
        difference: audit.difference
      };
      const baseId = key({ ...item, income_reference: 'national' });
      if (!summaries.has(baseId)) summaries.set(baseId, {});
      summaries.get(baseId)[reference] = summary;
      available.add(id);
    } catch (error) {
      errors.push(`${id} (${item.url}): ${error.message}`);
    }
  }
  const paired = { pairs: 0, fully_compared_pairs: 0, mismatched_pairs: 0,
    field_checks: { total_tracts: 0, included_tracts: 0, candidate_ids: 0, official_valid_votes: 0, computed_valid_votes: 0, difference: 0 } };
  for (const [baseId, pair] of summaries) {
    if (!pair.national || !pair.state) continue;
    paired.pairs++;
    let checked = 0, mismatched = false;
    for (const field of Object.keys(paired.field_checks)) {
      const a = pair.national[field], b = pair.state[field];
      // Legacy fixtures may omit coverage/candidates. Real exports supply all six.
      if (a === undefined || a === null || b === undefined || b === null) continue;
      checked++; paired.field_checks[field]++;
      if (field === 'candidate_ids' ? JSON.stringify(a) !== JSON.stringify(b) : a !== b) {
        errors.push(`${baseId}: national/state ${field} differs`);
        mismatched = true;
      }
    }
    if (checked === Object.keys(paired.field_checks).length) paired.fully_compared_pairs++;
    if (mismatched) paired.mismatched_pairs++;
  }
  const missing = expected.filter(item => !available.has(key(item)));
  const byOffice = references.flatMap(reference => Object.keys(LABELS).map(office => {
    const targets = expected.filter(item => item.office === office && incomeReference(item) === reference);
    const absent = targets.filter(item => !available.has(key(item))).map(item => item.uf);
    return { office, label: LABELS[office], income_reference: reference, available: targets.length - absent.length, expected: targets.length, missing_ufs: absent };
  }));
  const byReference = ['national', 'state'].map(reference => {
    const targets = expectedContests(reference);
    const absent = targets.filter(item => !available.has(key(item)));
    return { income_reference: reference, required: references.includes(reference), available: targets.length - absent.length,
      expected: targets.length, missing: absent.map(key) };
  });
  return {
    state: errors.length ? 'INVALID' : missing.length ? 'IN_PROGRESS' : 'COMPLETE',
    year: 2026, round: 1, requested_references: references, expected: expected.length, available: expected.length - missing.length,
    manifest_series: records.length, checked_files: checkedFiles,
    additional_available_series: [...available].filter(id => !expected.some(item => key(item) === id)).length,
    paired_references: paired, by_reference: byReference, by_office: byOffice, missing: missing.map(key), errors
  };
}
function exitCodeFor(result, progress = false) {
  return result.errors.length || (!progress && result.missing.length) ? 1 : 0;
}
function readLocalSeries(dataDir, url) {
  const decoded = decodeURIComponent(url);
  const base = fs.realpathSync(dataDir);
  const target = path.resolve(base, decoded);
  const relative = path.relative(base, target);
  if (!relative || relative.startsWith('..') || path.isAbsolute(relative) || path.extname(target) !== '.json') {
    throw new Error('series URL leaves the data directory or is not JSON');
  }
  const realTarget = fs.realpathSync(target);
  const realRelative = path.relative(base, realTarget);
  if (realRelative.startsWith('..') || path.isAbsolute(realRelative)) throw new Error('series symlink leaves the data directory');
  return JSON.parse(fs.readFileSync(realTarget, 'utf8').replace(/^\uFEFF/, ''));
}
function selfTest() {
  const fixture = expectedContests().map(item => ({ ...item, id: key(item), url: `series/${key(item)}.json`, status: 'available' }));
  const makeSeries = item => ({ metadata: { ...item, percentile_scope: incomeReference(item) === 'state' ? item.uf : 'BR', vote_audit: { status: 'reconciled', official_valid_votes: 1,
    computed_valid_votes: 1, difference: 0, checked_units: 1, ufs: item.uf === 'BR' ? UFS : item.uf } } });
  const payloads = new Map(fixture.map(item => [item.url, makeSeries(item)]));
  const load = url => { if (!payloads.has(url)) throw new Error('missing file'); return payloads.get(url); };
  const run = (series, options) => evaluateManifest({ schema_version: 1, series }, load, options);
  assert.equal(run(fixture).state, 'COMPLETE');
  assert.equal(expectedContests('state').length, 135);
  const stateFixture = expectedContests('state').map(item => ({ ...item, id: key(item), url: `series/${key(item)}.json`, status: 'available' }));
  for (const item of stateFixture) payloads.set(item.url, makeSeries(item));
  const both = [...fixture, ...stateFixture];
  const completeBoth = run(both, { bothReferences: true });
  assert.equal(completeBoth.state, 'COMPLETE'); assert.equal(completeBoth.available, 271);
  assert.equal(completeBoth.by_reference[0].available, 136); assert.equal(completeBoth.by_reference[1].available, 135);
  assert.equal(completeBoth.paired_references.pairs, 135);
  assert.equal(completeBoth.paired_references.fully_compared_pairs, 0); // Minimal legacy fixtures omit optional summaries.
  const nationalPairItem = fixture.find(item => item.uf === 'AC' && item.office === '1');
  const statePairItem = stateFixture.find(item => item.uf === 'AC' && item.office === '1');
  for (const [index, item] of [nationalPairItem, statePairItem].entries()) {
    const full = structuredClone(payloads.get(item.url));
    full.metadata.coverage = { total_tracts: 100, included_tracts: 80 };
    full.candidates = index ? [{ id: '22' }, { id: '13' }] : [{ id: '13' }, { id: '22' }];
    full.points = [{ centil: 1, votacao: index ? 20 : 10 }]; // Different curves/scopes are expected after reranking.
    payloads.set(item.url, full);
  }
  const pairedComplete = run(both, { bothReferences: true });
  assert.equal(pairedComplete.state, 'COMPLETE'); assert.equal(pairedComplete.paired_references.fully_compared_pairs, 1);
  assert.equal(pairedComplete.paired_references.field_checks.candidate_ids, 1);
  const pairedOriginal = structuredClone(payloads.get(statePairItem.url));
  for (const field of ['total_tracts', 'included_tracts']) {
    const changedPair = structuredClone(pairedOriginal); changedPair.metadata.coverage[field]++;
    payloads.set(statePairItem.url, changedPair);
    const badPair = run(both, { bothReferences: true });
    assert.equal(badPair.state, 'INVALID'); assert.equal(badPair.paired_references.mismatched_pairs, 1);
    assert(badPair.errors.some(error => error.includes(`${field} differs`)));
  }
  const differentCandidates = structuredClone(pairedOriginal); differentCandidates.candidates[0].id = '99';
  payloads.set(statePairItem.url, differentCandidates);
  assert(run(both).errors.some(error => error.includes('candidate_ids differs')));
  const differentVotes = structuredClone(pairedOriginal);
  differentVotes.metadata.vote_audit.official_valid_votes = 2;
  differentVotes.metadata.vote_audit.computed_valid_votes = 2; // Each audit still reconciles individually.
  payloads.set(statePairItem.url, differentVotes);
  assert(run(both).errors.some(error => error.includes('official_valid_votes differs')));
  assert.equal(exitCodeFor(run(both), true), 1); // Progress must not conceal a paired-data mismatch.
  payloads.set(statePairItem.url, pairedOriginal);
  assert.equal(run(both, { bothReferences: true }).state, 'COMPLETE');
  assert.equal(run(both).available, 136); // Extra state series never increase national coverage.
  const nationalOnlyBoth = run(fixture, { bothReferences: true });
  assert.equal(nationalOnlyBoth.state, 'IN_PROGRESS'); assert.equal(nationalOnlyBoth.missing.length, 135);
  assert.equal(exitCodeFor(nationalOnlyBoth), 1); assert.equal(exitCodeFor(nationalOnlyBoth, true), 0);
  assert.equal(run(stateFixture).available, 0); assert.equal(run(stateFixture).missing.length, 136);
  assert.equal(run([...fixture.slice(1), ...stateFixture], { bothReferences: true }).available, 270);
  const explicitNational = fixture.map(item => ({ ...item, income_reference: 'national' }));
  assert.equal(run(explicitNational).state, 'COMPLETE');
  const invalidStateBr = { ...fixture[0], id: `${fixture[0].id}-state`, income_reference: 'state' };
  assert.equal(run([...fixture, invalidStateBr]).state, 'INVALID');
  assert.equal(run([{ ...stateFixture[0], id: stateFixture[0].id.replace(/-state$/, '') }]).state, 'INVALID');
  assert.equal(run([{ ...fixture[0], income_reference: 'regional' }]).state, 'INVALID');
  const statePayload = payloads.get(stateFixture[0].url);
  payloads.set(stateFixture[0].url, { metadata: { ...statePayload.metadata, income_reference: 'national' } });
  assert.equal(run(both, { bothReferences: true }).state, 'INVALID');
  payloads.set(stateFixture[0].url, { metadata: { ...statePayload.metadata, percentile_scope: 'BR' } });
  assert.equal(run(both, { bothReferences: true }).state, 'INVALID');
  payloads.set(stateFixture[0].url, statePayload);
  const nationalPayload = payloads.get(fixture[1].url);
  payloads.set(fixture[1].url, { metadata: { ...nationalPayload.metadata, percentile_scope: fixture[1].uf } });
  assert.equal(run(fixture).state, 'INVALID');
  payloads.set(fixture[1].url, nationalPayload);
  const partial = run(fixture.slice(1));
  assert.equal(partial.state, 'IN_PROGRESS'); assert.equal(partial.available, 135);
  assert.equal(exitCodeFor(partial), 1); assert.equal(exitCodeFor(partial, true), 0);
  const historic = { year: 2018, round: 2, office: '1', uf: 'BR', status: 'available' };
  historic.id = key(historic); historic.url = `series/${historic.id}.json`; payloads.set(historic.url, makeSeries(historic));
  assert.equal(run([...fixture, historic]).state, 'COMPLETE');
  assert.equal(run([...fixture.slice(1), historic]).state, 'IN_PROGRESS');
  const unavailable = { ...fixture[0], status: 'unavailable' };
  assert.equal(run([unavailable, ...fixture.slice(1)]).available, 135);
  assert.equal(run([...fixture, fixture[0]]).state, 'INVALID');
  assert.equal(run([{ ...fixture[0], url: 'series/missing.json' }, ...fixture.slice(1)]).state, 'INVALID');
  assert.equal(run([{ ...fixture[0], id: '2026-1-7-DF', office: '7', uf: 'DF' }, ...fixture.slice(1)]).state, 'INVALID');
  const changed = structuredClone(payloads.get(fixture[0].url)); changed.metadata.vote_audit.difference = 1;
  payloads.set(fixture[0].url, changed);
  assert.equal(run(fixture).state, 'INVALID'); assert.equal(exitCodeFor(run(fixture), true), 1);
  assert.throws(() => readLocalSeries(__dirname, '../outside.json'));
  assert.throws(() => readLocalSeries(__dirname, '%2e%2e/outside.json'));
  console.log('PASS acceptance self-tests: 136 national/135 state/271 combined, paired coverage/candidate/audit equality, ranking/scopes allowed to differ, legacy omissions, progress, history and integrity checks');
}
function main(args) {
  const known = new Set(['--progress', '--json', '--self-test', '--both-references']);
  const unknown = args.filter(arg => arg.startsWith('--') && !known.has(arg));
  const positional = args.filter(arg => !arg.startsWith('--'));
  if (unknown.length || positional.length > 1) throw new Error('Usage: node acceptance-2026.cjs [data-directory] [--both-references] [--progress] [--json] [--self-test]');
  if (args.includes('--self-test')) { selfTest(); return; }
  const dataDir = path.resolve(positional[0] || path.join(__dirname, '../../../posts/explorador-eleitoral/data'));
  const manifest = JSON.parse(fs.readFileSync(path.join(dataDir, 'manifest.json'), 'utf8').replace(/^\uFEFF/, ''));
  const result = evaluateManifest(manifest, url => readLocalSeries(dataDir, url), { bothReferences: args.includes('--both-references') });
  if (args.includes('--json')) console.log(JSON.stringify(result, null, 2));
  else {
    console.log(`${result.state}: 2026 round 1 coverage ${result.available}/${result.expected} (${result.requested_references.join('+')}); ${result.checked_files} published JSON files checked`);
    console.log(`Paired national/state: ${result.paired_references.pairs}; fully compared ${result.paired_references.fully_compared_pairs}; mismatched ${result.paired_references.mismatched_pairs}`);
    for (const row of result.by_office) console.log(`  [${row.income_reference}] ${row.label}: ${row.available}/${row.expected}${row.missing_ufs.length ? `; missing ${row.missing_ufs.join(', ')}` : ''}`);
    if (result.missing.length) console.log(`Missing contest IDs: ${result.missing.join(', ')}`);
    for (const error of result.errors) console.error(`ERROR: ${error}`);
    if (args.includes('--progress') && result.state === 'IN_PROGRESS') console.log('Progress mode: missing contests remain; completion has NOT been accepted.');
  }
  process.exitCode = exitCodeFor(result, args.includes('--progress'));
}
module.exports = { UFS, expectedContests, evaluateManifest, exitCodeFor, readLocalSeries };
if (require.main === module) { try { main(process.argv.slice(2)); } catch (error) { console.error(`INVALID: ${error.message}`); process.exitCode = 1; } }

