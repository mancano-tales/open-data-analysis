// Validate deployed paths without a browser; full mode checks real gzip bytes.
'use strict';
const fs=require('node:fs/promises'),nativeFs=require('node:fs'),path=require('node:path');
const assert=require('node:assert/strict'),zlib=require('node:zlib'),crypto=require('node:crypto');
const {Readable}=require('node:stream');
const {gitTextDigest}=require('../../tools/git-text-digest.cjs');
const root=path.resolve(__dirname,'../..'),published=path.join(root,'docs/posts/explorador-eleitoral/data');
async function listFiles(directory){const files=[];try{for(const item of await fs.readdir(directory,{withFileTypes:true})){const file=path.join(directory,item.name);if(item.isDirectory())files.push(...await listFiles(file));else if(item.isFile())files.push(file);}}catch(error){if(error.code!=='ENOENT')throw error;}return files;}
(async()=>{
  // Regression for Windows publications checked out on Linux: boundaries can
  // split both CRLF and UTF-8 characters. Numerical/content edits must differ.
  const fixture=Buffer.from('São\r\n{"n":1,"escaped":"\\r\\n"}\rX\r');
  const normalized=fixture.toString('utf8').replace(/\r\n/g,'\n');
  const expected=crypto.createHash('sha256').update(normalized).digest('hex');
  assert.equal(await gitTextDigest(Readable.from([...fixture].map(b=>Buffer.from([b])))),expected);
  assert.notEqual(await gitTextDigest(Readable.from([Buffer.from(normalized.replace('"n":1','"n":2'))])),expected);
  const qmd=await fs.readFile(path.join(root,'posts-pt/explorador-eleitoral/index.qmd'),'utf8');
  const html=await fs.readFile(path.join(root,'docs/pt/posts-pt/explorador-eleitoral/index.html'),'utf8');
  for(const asset of ['app.js','app.css','compare.js','concentration.js']){
    const source=await fs.readFile(path.join(__dirname,asset));
    for(const directory of ['docs/posts/explorador-eleitoral','docs/pt/posts/explorador-eleitoral'])
      assert.deepEqual(await fs.readFile(path.join(root,directory,asset)),source,`Stale published ${asset} in ${directory}`);
  }
  const relative=html.match(/data-base="([^"]+)"/)[1];
  assert.equal(relative,'../../../posts/explorador-eleitoral/data/');
  assert.ok(!qmd.includes('../../posts/explorador-eleitoral/data/**'));
  for(const prefix of ['/','/open-data-analysis/']){
    const base=`https://example.invalid${prefix}pt/posts-pt/explorador-eleitoral/`;
    const canonical=new URL(relative,base);
    assert.equal(canonical.pathname,`${prefix}posts/explorador-eleitoral/data/`);
    for(const file of ['manifest.json','availability.csv'])assert.ok(html.includes(`href="${relative}${file}"`));
    const series=new URL('series/2026-1-1-AC-state.json.gz',canonical);
    assert.equal(series.pathname,`${prefix}posts/explorador-eleitoral/data/series/2026-1-1-AC-state.json.gz`);
  }
  if(process.argv.includes('--links-only')){console.log('PASS: PT and source links share canonical EN data, with and without a project URL prefix.');return;}
  const source=JSON.parse(await fs.readFile(path.join(__dirname,'data/manifest.json'),'utf8'));
  const manifest=JSON.parse(await fs.readFile(path.join(published,'manifest.json'),'utf8'));
  assert.equal(manifest.series.length,source.series.length);
  const originals=new Map(source.series.map(entry=>[entry.id,entry]));
  let compressedBytes=0;
  for(const entry of manifest.series){
    const original=originals.get(entry.id);assert.ok(original);
    assert.deepEqual(entry,{...original,url:original.url+'.gz'});
    const file=path.join(published,entry.url);compressedBytes+=(await fs.stat(file)).size;
    assert.equal(await gitTextDigest(nativeFs.createReadStream(file).pipe(zlib.createGunzip())),await gitTextDigest(nativeFs.createReadStream(path.join(__dirname,'data',original.url))),`Published data differs after Git CRLF/LF normalization: ${entry.id}`);
    await assert.rejects(fs.access(path.join(published,original.url)),'Raw published dataset must be pruned after backup');
  }
  assert.equal((await listFiles(path.join(root,'docs/pt/posts/explorador-eleitoral/data'))).filter(file=>/\.(json|csv)$/.test(file)).length,0,'Redundant PT datasets remain');
  const files=await listFiles(path.join(root,'docs'));let bytes=0;for(const file of files)bytes+=(await fs.stat(file)).size;
  assert.ok(bytes<1_000_000_000,`Published directory too large: ${bytes} bytes`);
  console.log(`PASS: ${manifest.series.length} shared gzip series match their sources after Git CRLF/LF normalization; ${compressedBytes} compressed bytes; whole site ${bytes} bytes; no redundant PT data.`);
})().catch(error=>{console.error(error);process.exitCode=1;});
