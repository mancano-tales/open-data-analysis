// Quarto post-render: restore source downloads, then publish shared gzip data.
'use strict';
const fs = require('node:fs/promises');
const path = require('node:path');
const {execFileSync} = require('node:child_process');
const {publish} = require('../posts/explorador-eleitoral/publish-static.cjs');
const root = path.resolve(__dirname, '..');
async function copy(relative, destination) {
  await fs.mkdir(path.dirname(path.join(root, destination)), {recursive: true});
  await fs.copyFile(path.join(root, relative), path.join(root, destination));
}
(async () => {
  const sources = execFileSync('git', ['ls-files', '-z', '--cached', '--others', '--exclude-standard', '--', 'shared-pipeline'],
    {cwd: root, encoding: 'utf8'}).split('\0').filter(f => /\.(R|csv)$/.test(f));
  for (const prefix of ['docs/', 'docs/pt/']) {
    for (const source of sources) {
      // Refresh existing downloads; add only dependencies introduced by this
      // repair, rather than publishing every historical test and helper.
      const required = ['shared-pipeline/sedap/portas-anuais/dados-populacao.R',
        'shared-pipeline/utils/plot_theme.R'];
      const exists = await fs.access(path.join(root, prefix + source)).then(() => true, () => false);
      if (exists || required.includes(source)) await copy(source, prefix + source);
    }
    for (const asset of ['app.js', 'app.css', 'compare.js', 'concentration.js',
      'DATA-CONTRACT.md', 'PUBLISHING.md']) {
      const source = 'posts/explorador-eleitoral/' + asset;
      await copy(source, prefix + source);
    }
  }
  for (const post of ['gini-renda-mediana', 'gasto-federal-educacao-superior',
    'gasto-estudante-educacao-basica']) {
    const directory = 'posts/' + post + '/data/';
    for (const file of await fs.readdir(path.join(root, directory))) {
      if (file.endsWith('.csv')) {
        await copy(directory + file, 'docs/' + directory + file);
        await copy(directory + file, 'docs/pt/' + directory + file);
      }
    }
  }
  await copy('posts/explorador-eleitoral/data/availability.csv',
    'docs/posts/explorador-eleitoral/data/availability.csv');
  // Quarto's navbar template appends a space after anchors. Keep our new
  // language links clean without reformatting the rest of generated HTML.
  const htmlFiles = execFileSync('git', ['ls-files', '-z', '--', 'docs'],
    {cwd: root, encoding: 'utf8', maxBuffer: 16 * 1024 * 1024})
    .split('\0').filter(file => file.endsWith('.html'));
  for (const file of htmlFiles) {
    const target = path.join(root, file);
    const html = await fs.readFile(target, 'utf8');
    const clean = html.replace(/(href="https:\/\/mancano-tales\.github\.io\/open-data-analysis\/(?:pt\/index-pt\.html)?">)[ \t]+$/gm, '$1');
    if (clean !== html) await fs.writeFile(target, clean);
  }
  // Pruning validates each file against its durable Git backup before removal.
  console.log(await publish({repoRoot: root, apply: true, pruneJson: true, prunePt: true}));
})().catch(error => { console.error(error); process.exitCode = 1; });
