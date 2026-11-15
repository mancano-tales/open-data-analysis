// Check committed publication paths; unrelated ignored local output is excluded.
'use strict';
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const {execFileSync} = require('node:child_process');
const root = path.resolve(__dirname, '..');
const files = execFileSync('git', ['ls-files', '-z', '--', 'docs'],
  {cwd: root, encoding: 'utf8', maxBuffer: 16 * 1024 * 1024}).split('\0').filter(Boolean);
const failures = [];
for (const file of files.filter(f => f.endsWith('.html'))) {
  const html = fs.readFileSync(path.join(root, file), 'utf8');
  for (const match of html.matchAll(/(?:href|src)="([^"]+)"/g)) {
    const href = match[1].split(/[?#]/)[0];
    if (!href || /^(?:[a-z]+:|\/)/i.test(href)) continue;
    const target = path.resolve(root, path.dirname(file), decodeURIComponent(href));
    if (!fs.existsSync(target)) failures.push(file + ': ' + href);
  }
}
for (const directory of ['docs', 'docs/pt']) {
  const home = directory === 'docs' ? 'index.html' : 'index-pt.html';
  const listing = fs.readFileSync(path.join(root, directory, home), 'utf8');
  const rows = [...listing.matchAll(/class="title listing-title"/g)].length;
  const expected = fs.readdirSync(path.join(root, directory === 'docs' ? 'posts' : 'posts-pt'))
    .filter(post => {
      const source = path.join(root, directory === 'docs' ? 'posts' : 'posts-pt', post, 'index.qmd');
      return fs.existsSync(source) && !/^draft:\s*true\s*$/m.test(fs.readFileSync(source, 'utf8'));
    }).length;
  assert.equal(rows, expected, directory + ': catalogue missing rendered posts');
  const search = JSON.parse(fs.readFileSync(path.join(root, directory, 'search.json'), 'utf8'));
  for (const post of ['gini-renda-mediana', 'gasto-federal-educacao-superior',
    'gasto-estudante-educacao-basica']) {
    assert.ok(search.some(item => item.href.includes('/' + post + '/')),
      directory + ': missing search entry for ' + post);
    for (const file of fs.readdirSync(path.join(root, 'posts', post, 'data'))) {
      assert.ok(fs.existsSync(path.join(root, 'docs/posts', post, 'data', file)),
        'Missing public aggregate ' + post + '/' + file);
    }
  }
}
const pt = fs.readFileSync(path.join(root, 'docs/pt/index-pt.html'), 'utf8');
assert.match(pt, /href="https:\/\/mancano-tales\.github\.io\/open-data-analysis\/"[^>]*>\s*<span class="menu-text">English/);
assert.deepEqual(failures, [], 'Broken published relative links');
console.log('PASS: published links, language navigation, search entries and downloadable aggregates.');
