#!/usr/bin/env node
'use strict';
// Aggregate-content gate. The R export guard, not this gate, rehashes ZIP/RDS inputs.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const assert = require('node:assert/strict');
const { UFS } = require('./acceptance-2026.cjs');
const { evaluateStateHistorical } = require('./acceptance-state-historical.cjs');
const { expectedPresidential, readLocalPayload } = require('./acceptance-historical.cjs');
const PROTOCOL = 'legacy-canonic-v1';
const OFFICIAL = ['votacao_candidato_munzona', 'votacao_partido_munzona', 'detalhe_votacao_munzona'];
const md5 = bytes => crypto.createHash('md5').update(bytes).digest('hex');
const fullKey = row => [row.year, row.round, row.election_id, row.office, row.uf].join(':');
const shortKey = row => [row.year, row.round, row.office, row.uf].join(':');

function evaluateLegacyHistorical(manifest, loadSeries, loadCheckpoint, { years = [2014], rounds = [1, 2] } = {}) {
  assert(years.length && new Set(years).size === years.length && years.every(year => [2010, 2014].includes(year)), 'Legacy years must be unique explicit 2010/2014');
  assert(rounds.length && new Set(rounds).size === rounds.length && rounds.every(round => [1, 2].includes(round)), 'Invalid selected rounds');
  const errors = [], certificates = new Map();
  for (const year of years) for (const uf of UFS) {
    const id = `${year}:${uf}`;
    try {
      const entry = loadCheckpoint(year, uf);
      if (!entry) continue;
      const r = entry.record;
      assert(r && r.schema_version === 1 && Number(r.year) === year && r.uf === uf, 'invalid LEGACY checkpoint identity/schema');
      if (!['complete', 'partial'].includes(r.status)) continue;
      assert(typeof r.run_id === 'string' && r.run_id.length > 0, 'missing execution identity');
      assert(/^[a-f0-9]{32}$/.test(entry.md5), 'missing checkpoint byte digest');
      assert(Array.isArray(r.certified) && Array.isArray(r.unavailable) && Array.isArray(r.fingerprints), 'malformed LEGACY tables');
      assert(r.certified.length + r.unavailable.length > 0, 'empty checkpoint partition');
      const all = [...r.certified, ...r.unavailable];
      const keys = all.map(fullKey), contests = all.map(shortKey);
      assert(new Set(keys).size === keys.length && new Set(contests).size === contests.length, 'duplicate, overlapping or multiple-election contest');
      for (const row of all) {
        assert(Number(row.year) === year && row.uf === uf && [1, 2].includes(Number(row.round)) && [3, 5, 6, 7, 8].includes(Number(row.office)), 'unexpected contest scope');
        assert(Number.isInteger(row.election_id) && row.election_id > 0, 'missing election identity');
      }
      for (const round of [1, 2]) assert(new Set(all.filter(r => Number(r.round) === round).map(r => r.election_id)).size <= 1, 'multiple ordinary elections in one round');
      for (const row of r.unavailable) assert(typeof row.reason === 'string' && row.reason.trim(), 'unavailable contest lacks reason');
      for (const row of r.certified) assert(row.reconciled === true, 'certified contest must explicitly reconcile');
      assert(r.publishable === (r.certified.length > 0), 'publishability differs from certified scope');
      assert(r.audit_status === (r.certified.length ? 'reconciled' : 'unavailable'), 'invalid audit status');
      assert(typeof r.output === 'string' && path.basename(r.output) === `places-${year}-${uf}-3_5_6_7_8-legal.rds`, 'unexpected output binding');
      if (entry.expectedOutput) assert(path.resolve(r.output) === path.resolve(entry.expectedOutput), 'output outside canonical cache');
      const fingerprints = r.fingerprints;
      assert(new Set(fingerprints.map(row => row.path)).size === fingerprints.length, 'duplicate fingerprints');
      for (const row of fingerprints) assert(typeof row.path === 'string' && Number.isFinite(row.bytes) && row.bytes >= 0 && /^[a-f0-9]{32}$/.test(row.md5), 'invalid stored input fingerprint');
      assert(fingerprints.some(row => row.path === r.output), 'output fingerprint absent');
      const source = fingerprints.filter(row => path.basename(row.path) === `votacao_secao_${year}_${uf}.zip`);
      assert(source.length === 1, 'raw section source fingerprint absent/ambiguous');
      certificates.set(id, { ...entry, normalized: { ...r, version: 1, source_md5: source[0].md5 } });
    } catch (error) { errors.push(`${id}: ${error.message}`); }
  }
  const payloadMetadata = new Map();
  const result = evaluateStateHistorical(manifest, url => {
    const payload = loadSeries(url);
    payloadMetadata.set(url, payload?.metadata);
    return payload;
  }, (year, uf) => certificates.get(`${year}:${uf}`)?.normalized || null, { years, rounds });
  errors.push(...result.errors);
  for (const item of manifest.series || []) {
    if (item.status !== 'available' || !years.includes(Number(item.year)) || String(item.office) === '1') continue;
    const entry = certificates.get(`${item.year}:${item.uf}`), metadata = payloadMetadata.get(item.url);
    if (!entry) { errors.push(`${item.id}: state series lacks completed LEGACY certification`); continue; }
    if (metadata?.vote_cache_protocol !== PROTOCOL || metadata?.vote_checkpoint_md5 !== entry.md5)
      errors.push(`${item.id}: protocol/checkpoint digest does not match current stored certificate`);
    if (!/^[a-f0-9]{32}$/.test(metadata?.vote_source_md5 || '') || metadata.vote_source_md5 !== entry.normalized.source_md5)
      errors.push(`${item.id}: raw vote source digest absent or different from checkpoint`);
    const r = entry.record, declared = metadata?.vote_certificate;
    const target = r.certified.filter(row => shortKey(row) === shortKey(item));
    if (!declared || target.length !== 1 || declared.protocol !== PROTOCOL || declared.checkpoint_md5 !== entry.md5 ||
        declared.checkpoint !== `status/${item.year}/${item.uf}.json` || declared.status !== r.status ||
        declared.election_id !== target[0].election_id || declared.certified_contests !== r.certified.length ||
        declared.unavailable_contests !== r.unavailable.length) {
      errors.push(`${item.id}: declared vote certificate identity/scope differs from checkpoint`);
      continue;
    }
    const members = declared.official_members;
    if (!members || Object.keys(members).length !== OFFICIAL.length || !OFFICIAL.every(prefix => members[prefix])) {
      errors.push(`${item.id}: official member provenance incomplete`); continue;
    }
    for (const prefix of OFFICIAL) {
      const member = members[prefix], basename = `${prefix}_${item.year}_${item.uf}.csv`.toLowerCase();
      const matching = r.fingerprints.filter(row => path.basename(row.path).toLowerCase() === basename);
      if (typeof member.archive !== 'string' || path.basename(member.archive).toLowerCase() !== `${prefix}_${item.year}.zip` ||
          typeof member.member !== 'string' || path.basename(member.member).toLowerCase() !== basename || matching.length !== 1 ||
          member.csv_md5 !== matching[0].md5 || (member.csv_bytes !== undefined && member.csv_bytes !== matching[0].bytes))
        errors.push(`${item.id}: ${prefix} declared member differs from stored fingerprint`);
    }
  }
  return { ...result, phase: 'legacy-certified-state-content', errors,
    state: errors.length ? 'INVALID' : result.state, complete: result.complete && !errors.length,
    certified_scope_complete: !errors.length && certificates.size === years.length * UFS.length && result.presidency.complete && result.state_contests.missing_series.length === 0,
    processed_ufs: certificates.size, checkpoint_protocol: PROTOCOL,
    validation_scope: 'Aggregate payloads compared with stored LEGACY checkpoint totals and checkpoint byte digest; ZIP/RDS/CSV fingerprints are NOT rehashed by this gate',
    archive_fingerprints_rehashed: false };
}

function selfTest() {
  const records = [], payloads = new Map(), checkpoints = new Map();
  const publish = (row, legacy = false) => {
    const ref = row.income_reference || 'national', id = `${row.year}-${row.round}-${row.office}-${row.uf}${ref === 'state' ? '-state' : ''}`;
    records.push({ ...row, id, url: `${id}.json`, income_reference: ref, status: 'available' });
    payloads.set(`${id}.json`, { metadata: { year: row.year, round: row.round, office: row.office, uf: row.uf, income_reference: ref,
      income_year: 2022, percentile_scope: ref === 'state' ? row.uf : 'BR',
      coverage: { total_tracts: 100, included_tracts: 100, observed_places: 1 },
      vote_audit: { status: 'reconciled', difference: 0, official_valid_votes: 100, computed_valid_votes: 100, checked_units: 1, ufs: row.uf === 'BR' ? UFS : row.uf },
      ...(legacy ? { vote_cache_protocol: PROTOCOL, vote_checkpoint_md5: 'a'.repeat(32), vote_source_md5: 'b'.repeat(32),
        vote_certificate: { protocol: PROTOCOL, checkpoint_md5: 'a'.repeat(32), checkpoint: `status/${row.year}/${row.uf}.json`, status: 'partial',
          election_id: row.election_id, certified_contests: 1, unavailable_contests: 1,
          official_members: Object.fromEntries(OFFICIAL.map(prefix => [prefix, { archive: `${prefix}_${row.year}.zip`, member: `${prefix}_${row.year}_${row.uf}.csv`, csv_md5: 'e'.repeat(32), csv_bytes: 3 }])) } } : {}) },
      candidates: [{ id: '13' }, { id: '22' }], points: ['13', '22'].flatMap(candidate_id => Array.from({ length: 100 }, (_, i) => ({
        centil: i + 1, renda_sm: 1, n_setores: 1, n_setores_total: 1, candidate_id, votacao: candidate_id === '13' ? 40 : 60 }))) });
  };
  for (const row of expectedPresidential([2014], [1, 2], ['national', 'state'])) publish(row);
  for (const uf of UFS) {
    const contest = { year: 2014, round: 1, election_id: 143, uf, office: 3, official_valid_votes: 100, computed_valid_votes: 100, difference: 0, reconciled: true };
    const output = `fixture/legacy-canonic/places-2014-${uf}-3_5_6_7_8-legal.rds`;
    checkpoints.set(uf, { md5: 'a'.repeat(32), record: { schema_version: 1, year: 2014, uf, run_id: 'fixture-1', status: 'partial',
      audit_status: 'reconciled', publishable: true, output, certified: [contest], unavailable: [{ ...contest, office: 7, reason: 'ambiguous legend' }],
      fingerprints: [{ path: output, bytes: 1, md5: 'c'.repeat(32) }, { path: `raw/votacao_secao_2014_${uf}.zip`, bytes: 2, md5: 'b'.repeat(32) },
        ...OFFICIAL.map(prefix => ({ path: `staging/${prefix}_2014_${uf}.csv`, bytes: 3, md5: 'e'.repeat(32) }))] } });
    if (uf === 'DF') checkpoints.get(uf).record.unavailable[0].office = 8;
    for (const income_reference of ['national', 'state']) publish({ ...contest, income_reference }, true);
  }
  const run = () => evaluateLegacyHistorical({ schema_version: 1, series: records }, url => payloads.get(url), (_year, uf) => checkpoints.get(uf));
  assert.equal(run().errors.length, 0); assert.equal(run().state_contests.available_series, 54); assert.equal(run().certified_scope_complete, true);
  const saved = structuredClone(checkpoints.get('AC'));
  checkpoints.get('AC').record.schema_version = 2; assert(run().errors.length); checkpoints.set('AC', structuredClone(saved));
  checkpoints.get('AC').md5 = 'd'.repeat(32); assert(run().errors.some(x => x.includes('digest'))); checkpoints.set('AC', structuredClone(saved));
  checkpoints.get('AC').record.status = 'running'; assert(run().errors.some(x => x.includes('lacks completed'))); checkpoints.set('AC', structuredClone(saved));
  checkpoints.get('AC').record.certified[0].computed_valid_votes = 0; assert(run().errors.length); checkpoints.set('AC', structuredClone(saved));
  checkpoints.get('AC').record.unavailable.push({ ...saved.record.certified[0], reason: 'blocked' }); assert(run().errors.length); checkpoints.set('AC', structuredClone(saved));
  const meta = payloads.get('2014-1-3-AC.json').metadata;
  delete meta.vote_source_md5; assert(run().errors.some(x => x.includes('source digest'))); meta.vote_source_md5 = 'b'.repeat(32);
  delete checkpoints.get('AC').record.certified[0].reconciled; assert(run().errors.length); checkpoints.set('AC', structuredClone(saved));
  checkpoints.get('AC').record.certified[0].reconciled = 'false'; assert(run().errors.length); checkpoints.set('AC', structuredClone(saved));
  meta.vote_certificate.election_id = 99999; assert(run().errors.some(x => x.includes('certificate identity'))); meta.vote_certificate.election_id = 143;
  meta.vote_certificate.official_members[OFFICIAL[0]].csv_md5 = 'f'.repeat(32); assert(run().errors.some(x => x.includes('declared member')));
  meta.vote_certificate.official_members[OFFICIAL[0]].csv_md5 = 'e'.repeat(32);
  meta.vote_cache_protocol = 'historical'; assert(run().errors.length);
  console.log('PASS LEGACY content gate: partial scope, current checkpoint digest, protocol, schema, running, zero denominator and overlapping certification');
}

function main(args) {
  if (args.includes('--self-test')) return selfTest();
  const positional = args.filter(x => !x.startsWith('--'));
  assert(positional.length <= 1 && args.every(x => !x.startsWith('--') || ['--progress', '--json', '--certified'].includes(x) || /^--(?:years|legacy-audits)=/.test(x)), 'Unknown argument');
  const option = (name, fallback) => args.find(x => x.startsWith(`--${name}=`))?.slice(name.length + 3) || fallback;
  const years = option('years', '2014').split(',').map(Number);
  const directory = path.resolve(positional[0] || 'posts/explorador-eleitoral/data');
  const root = path.resolve(option('legacy-audits', 'data-raw/eleicoes-renda/historico/legacy-canonic/status'));
  const manifest = JSON.parse(fs.readFileSync(path.join(directory, 'manifest.json'), 'utf8').replace(/^\uFEFF/, ''));
  const result = evaluateLegacyHistorical(manifest, url => readLocalPayload(directory, url), (year, uf) => {
    const file = path.join(root, String(year), `${uf}.json`);
    if (!fs.existsSync(file)) return null;
    const bytes = fs.readFileSync(file);
    return { record: JSON.parse(bytes.toString('utf8').replace(/^\uFEFF/, '')), md5: md5(bytes),
      expectedOutput: path.join(path.dirname(root), `places-${year}-${uf}-3_5_6_7_8-legal.rds`) };
  }, { years });
  if (args.includes('--json')) console.log(JSON.stringify(result, null, 2));
  else {
    console.log(`${result.state}: LEGACY UFs ${result.processed_ufs}/${years.length * UFS.length}; state series ${result.state_contests.available_series}/${result.state_contests.expected_series}; certified_scope_complete=${result.certified_scope_complete}; unavailable contests=${result.state_contests.unavailable_contests}`);
    for (const error of result.errors) console.error(`ERROR: ${error}`);
    console.log(result.validation_scope);
  }
  process.exitCode = result.errors.length || (args.includes('--certified') ? !result.certified_scope_complete : (!args.includes('--progress') && !result.complete)) ? 1 : 0;
}
module.exports = { evaluateLegacyHistorical };
if (require.main === module) { try { main(process.argv.slice(2)); } catch (error) { console.error(`INVALID: ${error.message}`); process.exitCode = 1; } }
