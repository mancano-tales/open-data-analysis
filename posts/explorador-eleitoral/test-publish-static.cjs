// Isolated TEST-ONLY files; never reads or changes source electoral datasets.
'use strict';
const fs=require('node:fs/promises'),path=require('node:path'),assert=require('node:assert/strict'),zlib=require('node:zlib');
const {publish}=require('./publish-static.cjs');
(async()=>{
  const parent=path.resolve(__dirname,'../../data-raw/eleicoes-renda');await fs.mkdir(parent,{recursive:true});
  const root=await fs.mkdtemp(path.join(parent,'publish-static-test-'));
  const source=path.join(root,'posts/explorador-eleitoral/data'),published=path.join(root,'docs/posts/explorador-eleitoral/data');
  await fs.mkdir(path.join(source,'series'),{recursive:true});
  const input=JSON.stringify({TEST_FIXTURE_ONLY:true,values:Array.from({length:1000},(_,i)=>i%10)})+'\n';
  const manifest=JSON.stringify({schema_version:1,series:[{id:'fixture',status:'available',url:'series/fixture.json'}]});
  await fs.writeFile(path.join(source,'series/fixture.json'),input);await fs.writeFile(path.join(source,'manifest.json'),manifest);await fs.writeFile(path.join(source,'availability.csv'),'TEST_FIXTURE_ONLY\n');
  const plan=await publish({repoRoot:root});assert.equal(plan.mode,'dry-run');await assert.rejects(fs.access(published));
  const result=await publish({repoRoot:root,apply:true});assert.equal(result.series,1);assert.ok(result.compressedBytes<result.sourceBytes);
  const gzipFile=path.join(published,'series/fixture.json.gz'),first=await fs.readFile(gzipFile);
  assert.equal(zlib.gunzipSync(first).toString('utf8'),input);assert.equal(first.readUInt32LE(4),0);
  const output=JSON.parse(await fs.readFile(path.join(published,'manifest.json'),'utf8'));assert.equal(output.series[0].url,'series/fixture.json.gz');
  await publish({repoRoot:root,apply:true});assert.deepEqual(await fs.readFile(gzipFile),first);
  assert.equal(await fs.readFile(path.join(source,'manifest.json'),'utf8'),manifest);assert.equal(await fs.readFile(path.join(source,'series/fixture.json'),'utf8'),input);
  await fs.writeFile(path.join(published,'series/fixture.json'),input);
  await assert.rejects(publish({repoRoot:root,apply:true,pruneJson:true}),/HEAD|backup|preserved/);
  assert.equal(await fs.readFile(path.join(published,'series/fixture.json'),'utf8'),input);
  await assert.rejects(publish({repoRoot:root,pruneJson:true}),/require --apply/);
  console.log('PASS: dry-run, streaming gzip roundtrip/determinism, compact manifest URLs, unchanged sources, and deletion blocked without Git backup.');
})().catch(error=>{console.error(error);process.exitCode=1;});
