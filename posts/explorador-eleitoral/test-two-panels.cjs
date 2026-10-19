'use strict';
const fs=require('node:fs'),path=require('node:path'),assert=require('node:assert/strict');
const repo=path.resolve(__dirname,'../..');
const pages=[
  ['docs/posts/explorador-eleitoral/two-panels.html','posts/explorador-eleitoral/two-panels.html','pt/posts-pt/explorador-eleitoral/dois-paineis.html'],
  ['docs/pt/posts-pt/explorador-eleitoral/dois-paineis.html','pt/posts-pt/explorador-eleitoral/dois-paineis.html','posts/explorador-eleitoral/two-panels.html']
];
for(const [file,route,other] of pages){
  const html=fs.readFileSync(path.join(repo,file),'utf8');
  const frames=[...html.matchAll(/<iframe\b([^>]+)>/g)].map(m=>m[1]);
  assert.equal(frames.length,2);
  assert.notEqual(frames[0].match(/title="([^"]+)"/)[1],frames[1].match(/title="([^"]+)"/)[1]);
  assert.ok(html.indexOf('callout-warning')<html.indexOf('<iframe'));
  for(const prefix of ['/','/open-data-analysis/']){
    const current=new URL(route,'https://example.invalid'+prefix);
    for(const frame of frames){
      const target=new URL(frame.match(/src="([^"]+)"/)[1],current);
      assert.equal(target.origin,current.origin);
      assert.equal(target.pathname,current.pathname.replace(/[^/]+$/,''));
      assert.equal(target.searchParams.get('panel'),'1');
    }
    const language=html.match(/<a class="language-switch" href="([^"]+)"/)[1];
    assert.equal(new URL(language,current).pathname,prefix+other);
  }
}
const css=fs.readFileSync(path.join(__dirname,'two-panels.css'),'utf8');
assert.match(css,/repeat\(2,minmax\(0,1fr\)\)/);
assert.match(css,/@media\(max-width:900px\)/);
for(const file of ['docs/posts/explorador-eleitoral/index.html','docs/pt/posts-pt/explorador-eleitoral/index.html']){
  const html=fs.readFileSync(path.join(repo,file),'utf8');
  assert.ok(html.includes('panel-embed-v1.css'));
  assert.ok(html.includes('app.js?v=concentration-v1'));
  assert.ok(html.includes('compare.js?v=concentration-v1'));
  assert.ok(html.includes('concentration.js?v=concentration-v1'));
}
console.log('PASS: two same-origin independent frames; PT/EN links with project prefixes; notice before frames; responsive layout; embed assets.');
