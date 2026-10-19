// Retrieve only the exact primary documents declared in the reviewed table.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

async function main() {
  const args = process.argv.slice(2);
  const checkOnly = args.includes('--check-only');
  const cacheFlag = args.indexOf('--cache-dir');
  if (cacheFlag >= 0 && (!args[cacheFlag + 1] || args[cacheFlag + 1].startsWith('--'))) throw Error('--cache-dir requires a directory');
  const cache = path.resolve(cacheFlag >= 0 ? args[cacheFlag + 1] : 'data-raw/eleicoes-renda/historico');
  const table = JSON.parse(fs.readFileSync('shared-pipeline/eleicoes/historico/official-vote-destinations.json', 'utf8'));
  if (table.schema_version !== 1) throw Error('Unknown official destination schema');
  for (const source of table.sources) {
    if (path.isAbsolute(source.cache_path) || source.cache_path.split(/[\\/]/).includes('..')) throw Error('Unsafe document cache path');
    const target = path.resolve(cache, source.cache_path);
    if (!target.startsWith(cache + path.sep)) throw Error('Document outside requested cache');
    const verify = bytes => bytes.subarray(0, 5).toString('ascii') === '%PDF-' && bytes.length === source.bytes &&
      crypto.createHash('sha256').update(bytes).digest('hex') === source.sha256;
    if (fs.existsSync(target) && verify(fs.readFileSync(target))) {
      process.stdout.write(`VERIFIED ${source.id}: ${source.bytes} bytes, SHA256 ${source.sha256}\n`);
      continue;
    }
    if (checkOnly) throw Error(`Missing or invalid official document: ${source.id}`);
    const response = await fetch(source.url, {signal: AbortSignal.timeout(120000)});
    if (!response.ok) throw Error(`Official document ${source.id}: HTTP ${response.status}`);
    const bytes = Buffer.from(await response.arrayBuffer());
    if (!verify(bytes)) throw Error(`Official document signature/size/SHA256 changed: ${source.id}`);
    fs.mkdirSync(path.dirname(target), {recursive: true});
    fs.writeFileSync(target + '.part', bytes);
    fs.renameSync(target + '.part', target);
    process.stdout.write(`SAVED ${source.id}: ${source.bytes} bytes, SHA256 ${source.sha256}\n`);
  }
}
main().catch(error => { process.stderr.write(error.message + '\n'); process.exitCode = 1; });
