#!/usr/bin/env node
'use strict';
// Read-only acceptance gate for all 136 first-round 2026 territorial contests.
// --progress reports missing contests with exit 0, but malformed published data
// always fails. Other years/rounds may coexist without satisfying 2026 coverage.
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const UFS = 'AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO'.split(' ');
const LABELS = { 1: 'Presidente', 3: 'Governador', 5: 'Senador', 6: 'Deputado federal', 7: 'Deputado estadual', 8: 'Deputado distrital' };
const key = ({ year, round, office, uf }) => `${Number(year)}-${Number(round)}-${String(office)}-${String(uf).toUpperCase()}`;
function expectedContests() {
  const rows = [];
  for (const uf of ['BR', ...UFS]) rows.push({ year: 2026, round: 1, office: '1', uf });
  for (const office of ['3', '5', '6']) for (const uf of UFS) rows.push({ year: 2026, round: 1, office, uf });
  for (const uf of UFS.filter(uf => uf !== 'DF')) rows.push({ year: 2026, round: 1, office: '7', uf });
  rows.push({ year: 2026, round: 1, office: '8', uf: 'DF' });
  assert.equal(rows.length, 136);
  return rows;
}
function evaluateManifest(manifest, loadSeries) {
  const errors = [], available = new Set(), ids = new Set();
  const expected = expectedContests();
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
      available.add(id);
    } catch (error) {
      errors.push(`${id} (${item.url}): ${error.message}`);
    }
  }
  const missing = expected.filter(item => !available.has(key(item)));
  const byOffice = Object.keys(LABELS).map(office => {
    const targets = expected.filter(item => item.office === office);
    const absent = targets.filter(item => !available.has(key(item))).map(item => item.uf);
    return { office, label: LABELS[office], available: targets.length - absent.length, expected: targets.length, missing_ufs: absent };
  });
  return {
    state: errors.length ? 'INVALID' : missing.length ? 'IN_PROGRESS' : 'COMPLETE',
    year: 2026, round: 1, expected: 136, available: 136 - missing.length,
    manifest_series: records.length, checked_files: checkedFiles,
    additional_available_series: [...available].filter(id => !expected.some(item => key(item) === id)).length,
    by_office: byOffice, missing: missing.map(key), errors
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
  const makeSeries = item => ({ metadata: { ...item, vote_audit: { status: 'reconciled', official_valid_votes: 1,
    computed_valid_votes: 1, difference: 0, checked_units: 1, ufs: item.uf === 'BR' ? UFS : item.uf } } });
  const payloads = new Map(fixture.map(item => [item.url, makeSeries(item)]));
  const load = url => { if (!payloads.has(url)) throw new Error('missing file'); return payloads.get(url); };
  const run = series => evaluateManifest({ schema_version: 1, series }, load);
  assert.equal(run(fixture).state, 'COMPLETE');
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
  console.log('PASS acceptance self-tests: complete/partial modes, historical coexistence, unavailable, duplicate, bad URL, illegal office and audit divergence');
}
function main(args) {
  const known = new Set(['--progress', '--json', '--self-test']);
  const unknown = args.filter(arg => arg.startsWith('--') && !known.has(arg));
  const positional = args.filter(arg => !arg.startsWith('--'));
  if (unknown.length || positional.length > 1) throw new Error('Usage: node acceptance-2026.cjs [data-directory] [--progress] [--json] [--self-test]');
  if (args.includes('--self-test')) { selfTest(); return; }
  const dataDir = path.resolve(positional[0] || path.join(__dirname, '../../../posts/explorador-eleitoral/data'));
  const manifest = JSON.parse(fs.readFileSync(path.join(dataDir, 'manifest.json'), 'utf8').replace(/^\uFEFF/, ''));
  const result = evaluateManifest(manifest, url => readLocalSeries(dataDir, url));
  if (args.includes('--json')) console.log(JSON.stringify(result, null, 2));
  else {
    console.log(`${result.state}: 2026 round 1 coverage ${result.available}/${result.expected}; ${result.checked_files} published JSON files checked`);
    for (const row of result.by_office) console.log(`  ${row.label}: ${row.available}/${row.expected}${row.missing_ufs.length ? `; missing ${row.missing_ufs.join(', ')}` : ''}`);
    if (result.missing.length) console.log(`Missing contest IDs: ${result.missing.join(', ')}`);
    for (const error of result.errors) console.error(`ERROR: ${error}`);
    if (args.includes('--progress') && result.state === 'IN_PROGRESS') console.log('Progress mode: missing contests remain; completion has NOT been accepted.');
  }
  process.exitCode = exitCodeFor(result, args.includes('--progress'));
}
module.exports = { UFS, expectedContests, evaluateManifest, exitCodeFor, readLocalSeries };
if (require.main === module) { try { main(process.argv.slice(2)); } catch (error) { console.error(`INVALID: ${error.message}`); process.exitCode = 1; } }

