'use strict';
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
const candidates = [
  {id: '13001', name: 'Alpha', party_number: 13},
  {id: '22001', name: 'Beta', party_number: 22},
  {id: '99001', name: 'Gamma', party_number: 99}
];
const series = {candidates, points: candidates.flatMap((candidate, index) => [1, 2].map(centil => ({
  candidate_id: candidate.id, centil, n_setores: 10,
  votacao: index === 0 ? (centil === 1 ? 0 : 2) : index === 1 ? (centil === 1 ? 2 : 0) : 0.05
})))};

for (const lang of ['pt', 'en']) {
  const controls = new Element('div'), table = new Element('table');
  const status = new Element('p'), downloads = new Element('div');
  const root = {dataset: {lang}, querySelector: selector => ({
    '.concentration-controls': controls, '.concentration-table': table,
    '.concentration-status': status, '.concentration-downloads': downloads
  })[selector]};
  const listeners = {}, dispatched = [];
  const document = {
    createElement: tag => new Element(tag), querySelectorAll: () => [root],
    addEventListener: (type, handler) => { listeners[type] = handler; },
    dispatchEvent: event => dispatched.push(event)
  };
  class CustomEvent { constructor(type, options) { this.type = type; this.detail = options.detail; } }
  vm.runInNewContext(source, {document, CustomEvent, URL: {createObjectURL: () => 'blob:fixture'}, Blob});
  assert.equal(table.children.length, 0, 'table waits for explorer:series');
  assert.equal(controls.children[2].children[0].value, '0.1', 'default threshold is 0.1%');
  listeners['explorer:series']({detail: {series, entry: {id: 'fixture', year: 2022},
    manifest: {parties: {'2022': {'13': 'PT', '22': 'PL', '99': 'XYZ'}}}}});
  const body = () => table.children[1].children;
  const ids = () => body().map(row => row.children[0].children[0].textContent.split(' · ')[0]);
  assert.deepEqual(ids(), ['13001', '22001'], 'table appears with the default threshold');
  assert.equal(table.children[0].children[0].children[2]['aria-sort'], 'descending');
  table.children[0].children[0].children[2].children[0].events.click();
  assert.deepEqual(ids(), ['22001', '13001'], 'index sort reverses');
  const party = controls.children[1].children[0];
  party.value = 'PT'; party.events.change();
  assert.deepEqual(ids(), ['13001'], 'party filter uses manifest acronym');
  party.value = ''; party.events.change();
  const search = controls.children[0].children[0];
  search.value = 'pl'; search.events.input();
  assert.deepEqual(ids(), ['22001'], 'search matches party acronym');
  search.value = ''; search.events.input();
  controls.children[2].children[0].value = '0'; controls.children[2].children[0].events.change();
  assert.equal(body().length, 3, 'lowering threshold reveals small candidate');
  body()[0].children[0].children[0].events.click();
  assert.equal(dispatched.at(-1).type, 'explorer:select');
  assert.equal(dispatched.at(-1).detail.id, ids()[0]);
}
console.log('test-concentration-ui OK');
