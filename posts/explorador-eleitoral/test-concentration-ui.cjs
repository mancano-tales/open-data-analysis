'use strict';
// UI test for the independent concentration table: its own selectors, its own
// manifest/series loading, metadata check, filters and sorting.
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const assert = require('node:assert/strict');

class Element {
  constructor(tag) { this.tag = tag; this.children = []; this.events = {}; this._value = ''; this.textContent = ''; }
  append(...nodes) { for (const node of nodes) { node.parent = this; this.children.push(node); } }
  replaceChildren(...nodes) { this.children = []; this.append(...nodes); }
  setAttribute(key, value) { this[key] = value; }
  addEventListener(type, handler) { this.events[type] = handler; }
  get value() { return this._value || (this.tag === 'select' ? this.children[0]?.value : '') || ''; }
  set value(value) { this._value = String(value); }
}

const source = fs.readFileSync(path.join(__dirname, 'concentration.js'), 'utf8');
const flush = () => new Promise(resolve => setImmediate(resolve));
const candidates = [
  {id: '13001', name: 'Alpha', party_number: 13},
  {id: '22001', name: 'Beta', party_number: 22},
  {id: '99001', name: 'Gamma', party_number: 99}
];
const series = (meta, president = false) => ({
  metadata: meta,
  candidates: president ? [{id: '13', name: 'Pres'}] : candidates,
  points: (president ? [{id: '13'}] : candidates).flatMap((candidate, index) => [1, 2].map(centil => ({
    candidate_id: candidate.id, centil, n_setores: 10,
    votacao: index === 0 ? (centil === 1 ? 0 : 2) : index === 1 ? (centil === 1 ? 2 : 0) : 0.05
  })))
});
const entries = [
  {id: '2022-1-1-BR', year: 2022, round: 1, office: '1', uf: 'BR', url: 'series/2022-1-1-BR.json', status: 'available'},
  {id: '2022-1-6-SP-state', year: 2022, round: 1, office: '6', uf: 'SP', income_reference: 'state', url: 'series/2022-1-6-SP-state.json', status: 'available'},
  {id: '2022-1-6-SP', year: 2022, round: 1, office: '6', uf: 'SP', url: 'series/2022-1-6-SP.json', status: 'available'},
  {id: '2022-1-6-RJ-state', year: 2022, round: 1, office: '6', uf: 'RJ', income_reference: 'state', url: 'series/2022-1-6-RJ-state.json', status: 'available'}
];
const files = {
  'data/manifest.json': {series: entries, parties: {'2022': {'13': 'PT', '22': 'PL', '99': 'XYZ'}}},
  'data/series/2022-1-1-BR.json': series({income_year: 2022, percentile_scope: 'BR', office: '1', uf: 'BR', year: 2022, round: 1}, true),
  'data/series/2022-1-6-SP-state.json': series({income_year: 2022, percentile_scope: 'SP', income_reference: 'state', office: '6', uf: 'SP', year: 2022, round: 1}),
  'data/series/2022-1-6-SP.json': series({income_year: 2022, percentile_scope: 'BR', office: '6', uf: 'SP', year: 2022, round: 1}),
  // Wrong payload behind a valid manifest entry: must be rejected.
  'data/series/2022-1-6-RJ-state.json': series({income_year: 2022, percentile_scope: 'SP', income_reference: 'state', office: '6', uf: 'SP', year: 2022, round: 1})
};

(async () => {
  for (const lang of ['pt', 'en']) {
    const controls = new Element('div'), filters = new Element('div'), table = new Element('table');
    const status = new Element('p'), downloads = new Element('div');
    const root = {dataset: {lang, base: 'data/'}, querySelector: selector => ({
      '.concentration-controls': controls, '.concentration-filters': filters, '.concentration-table': table,
      '.concentration-status': status, '.concentration-downloads': downloads
    })[selector]};
    const fetched = [];
    const document = {baseURI: 'http://localhost/posts/explorador-eleitoral/', createElement: tag => new Element(tag), querySelectorAll: () => [root]};
    const fetch = async url => {
      const key = new URL(url).pathname.replace('/posts/explorador-eleitoral/', ''); fetched.push(key);
      assert.ok(files[key], key); return {ok: true, json: async () => structuredClone(files[key])};
    };
    vm.runInNewContext(source, {document, fetch, URL: class extends URL { static createObjectURL() { return 'blob:fixture'; } },
      Blob, Uint8Array, TextDecoder, Response, DecompressionStream, console});
    for (let i = 0; i < 5; i++) await flush();
    const select = i => controls.children[i].children[0];
    const [year, round, office, uf, census, reference] = [0, 1, 2, 3, 4, 5].map(select);
    const change = async (element, value) => { element.value = value; element.events.change(); for (let i = 0; i < 5; i++) await flush(); };
    assert.equal(controls.children.length, 6, 'own selectors: year, round, office, territory, census, reference');
    assert.deepEqual([office.value, uf.value, reference.value], ['6', 'SP', 'state'], 'opens on federal deputy, SP, state reference');
    assert.ok(fetched.includes('data/series/2022-1-6-SP-state.json'), 'loads its own series');
    const body = () => table.children[1].children;
    const ids = () => body().map(row => row.children[0].textContent.split(' · ')[0]);
    assert.deepEqual(ids(), ['13001', '22001'], 'default 0.1% threshold');
    assert.match(table.children[0].children[0].children[2].children[0].textContent, /Wagstaff/);
    table.children[0].children[0].children[2].children[0].events.click();
    assert.deepEqual(ids(), ['22001', '13001'], 'index sort reverses');
    const [search, party, minShare] = [0, 1, 2].map(i => filters.children[i].children[0]);
    party.value = 'PT'; party.events.change();
    assert.deepEqual(ids(), ['13001'], 'party filter uses manifest acronym');
    party.value = ''; party.events.change();
    search.value = 'pl'; search.events.input();
    assert.deepEqual(ids(), ['22001'], 'search matches party acronym');
    search.value = ''; search.events.input();
    minShare.value = '0'; minShare.events.change();
    assert.equal(body().length, 3, 'lowering threshold reveals small candidate');
    await change(reference, 'national');
    assert.ok(fetched.includes('data/series/2022-1-6-SP.json'), 'reference change loads another series');
    await change(uf, 'RJ');
    assert.match(status.textContent, lang === 'pt' ? /recusada/ : /rejected/, 'mismatched payload is rejected');
    assert.equal(table.children.length, 0);
    await change(office, '1');
    assert.equal(uf.value, 'BR'); assert.deepEqual(ids(), ['13'], 'office change reloads the table');
  }
  console.log('test-concentration-ui OK');
})().catch(error => { console.error(error); process.exitCode = 1; });
