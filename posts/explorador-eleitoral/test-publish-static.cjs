// Isolated TEST-ONLY files; never reads or changes source electoral datasets.
'use strict';
const fs=require('node:fs/promises'),path=require('node:path'),os=require('node:os'),assert=require('node:assert/strict'),zlib=require('node:zlib');
const {publish,lfBlobHash}=require('./publish-static.cjs');
(async()=>{
  const root=await fs.mkdtemp(path.join(os.tmpdir(),'publish-static-test-'));
  const source=path.join(root,'posts/explorador-eleitoral/data'),published=path.join(root,'docs/posts/explorador-eleitoral/data');
  await fs.mkdir(path.join(source,'series'),{recursive:true});
  await fs.mkdir(path.join(root,'posts/explorador-eleitoral'),{recursive:true});
  await fs.writeFile(path.join(root,'posts/explorador-eleitoral/compare.js'),'// TEST FIXTURE ONLY\n');
  await fs.writeFile(path.join(root,'posts/explorador-eleitoral/concentration.js'),'// TEST CONCENTRATION FIXTURE ONLY\n');
  await fs.writeFile(path.join(root,'posts/explorador-eleitoral/app.js'),'// TEST APP FIXTURE ONLY\n');
  await fs.writeFile(path.join(root,'posts/explorador-eleitoral/app.css'),'/* TEST CSS FIXTURE ONLY */\n');
  const input=JSON.stringify({TEST_FIXTURE_ONLY:true,values:Array.from({length:1000},(_,i)=>i%10)})+'\n';
  const manifest=JSON.stringify({schema_version:1,series:[{id:'fixture',status:'available',url:'series/fixture.json'}]});
  await fs.writeFile(path.join(source,'series/fixture.json'),input);await fs.writeFile(path.join(source,'manifest.json'),manifest);await fs.writeFile(path.join(source,'availability.csv'),'TEST_FIXTURE_ONLY\n');
  const plan=await publish({repoRoot:root});assert.equal(plan.mode,'dry-run');await assert.rejects(fs.access(published));
  const result=await publish({repoRoot:root,apply:true});assert.equal(result.series,1);assert.ok(result.compressedBytes<result.sourceBytes);assert.equal(result.compressedSeries,1);assert.equal(result.reusedSeries,0);assert.equal(result.cacheStatus,'missing');
  const gzipFile=path.join(published,'series/fixture.json.gz'),first=await fs.readFile(gzipFile);
  assert.equal(zlib.gunzipSync(first).toString('utf8'),input);assert.equal(first.readUInt32LE(4),0);
  const output=JSON.parse(await fs.readFile(path.join(published,'manifest.json'),'utf8'));assert.equal(output.series[0].url,'series/fixture.json.gz');
  for(const directory of ['docs/posts/explorador-eleitoral','docs/pt/posts/explorador-eleitoral']){
    assert.equal(await fs.readFile(path.join(root,directory,'compare.js'),'utf8'),'// TEST FIXTURE ONLY\n');
    assert.equal(await fs.readFile(path.join(root,directory,'concentration.js'),'utf8'),'// TEST CONCENTRATION FIXTURE ONLY\n');
    assert.equal(await fs.readFile(path.join(root,directory,'app.js'),'utf8'),'// TEST APP FIXTURE ONLY\n');
    assert.equal(await fs.readFile(path.join(root,directory,'app.css'),'utf8'),'/* TEST CSS FIXTURE ONLY */\n');
  }
  const cached=await publish({repoRoot:root,apply:true});assert.equal(cached.reusedSeries,1);assert.equal(cached.compressedSeries,0);assert.deepEqual(await fs.readFile(gzipFile),first);
  assert.equal(await fs.readFile(path.join(source,'manifest.json'),'utf8'),manifest);assert.equal(await fs.readFile(path.join(source,'series/fixture.json'),'utf8'),input);
  const cachePath=path.join(root,'data-raw/eleicoes-renda/publication-cache.json');
  const recorded=JSON.parse(await fs.readFile(cachePath,'utf8'));assert.match(recorded.files['series/fixture.json'].source_sha256,/^[a-f0-9]{64}$/);assert.match(recorded.files['series/fixture.json'].gzip_sha256,/^[a-f0-9]{64}$/);
  // Same length AND restored mtime: content digests must detect source changes.
  const sourceFile=path.join(source,'series/fixture.json'),sourceStat=await fs.stat(sourceFile);
  const modified=input.replace('"values":[0','"values":[9');assert.notEqual(modified,input);assert.equal(Buffer.byteLength(modified),Buffer.byteLength(input));
  await fs.writeFile(sourceFile,modified);await fs.utimes(sourceFile,sourceStat.atime,sourceStat.mtime);
  const sourceChanged=await publish({repoRoot:root,apply:true});assert.equal(sourceChanged.compressedSeries,1);assert.equal(sourceChanged.reusedSeries,0);
  const changedGzip=await fs.readFile(gzipFile);assert.equal(zlib.gunzipSync(changedGzip).toString('utf8'),modified);
  // A same-size gzip corruption must not survive a cache hit or be blessed.
  const gzipStat=await fs.stat(gzipFile),corrupt=Buffer.from(changedGzip);corrupt[corrupt.length-1]^=255;
  await fs.writeFile(gzipFile,corrupt);await fs.utimes(gzipFile,gzipStat.atime,gzipStat.mtime);
  assert.equal((await fs.stat(gzipFile)).size,gzipStat.size);
  const repaired=await publish({repoRoot:root,apply:true});assert.equal(repaired.compressedSeries,1);assert.equal(repaired.reusedSeries,0);assert.deepEqual(await fs.readFile(gzipFile),changedGzip);
  await fs.writeFile(cachePath,'{broken cache');
  const invalid=await publish({repoRoot:root,apply:true});assert.equal(invalid.cacheStatus,'invalid');assert.equal(invalid.compressedSeries,1);assert.deepEqual(await fs.readFile(gzipFile),changedGzip);
  const badDigest=JSON.parse(await fs.readFile(cachePath,'utf8'));badDigest.files['series/fixture.json'].gzip_sha256='not-a-digest';await fs.writeFile(cachePath,JSON.stringify(badDigest));
  const invalidEntry=await publish({repoRoot:root,apply:true});assert.equal(invalidEntry.compressedSeries,1);assert.equal(invalidEntry.reusedSeries,0);
  await fs.unlink(cachePath);
  const missing=await publish({repoRoot:root,apply:true});assert.equal(missing.cacheStatus,'missing');assert.equal(missing.compressedSeries,1);
  await fs.unlink(gzipFile);
  const missingGzip=await publish({repoRoot:root,apply:true});assert.equal(missingGzip.compressedSeries,1);assert.deepEqual(await fs.readFile(gzipFile),changedGzip);
  const finalReuse=await publish({repoRoot:root,apply:true});assert.equal(finalReuse.reusedSeries,1);assert.equal(finalReuse.compressedSeries,0);
  await fs.writeFile(path.join(published,'series/fixture.json'),input);
  const crypto=require('node:crypto');
  await fs.writeFile(path.join(source,'line-endings.txt'),'a\r\nb\r\n');
  assert.equal(await lfBlobHash(path.join(source,'line-endings.txt')),crypto.createHash('sha1').update('blob 4\0a\nb\n').digest('hex'));
  await assert.rejects(publish({repoRoot:root,apply:true,pruneJson:true}),/HEAD|backup|preserved/);
  assert.equal(await fs.readFile(path.join(published,'series/fixture.json'),'utf8'),input);
  await assert.rejects(publish({repoRoot:root,pruneJson:true}),/require --apply/);
  console.log('PASS: dry-run, deterministic gzip, SHA256 cache reuse, same-size source change and gzip corruption repair, missing/corrupt cache fallback, unchanged source manifest, LF Git backup normalization and pruning blocked without backup.');
})().catch(error=>{console.error(error);process.exitCode=1;});
