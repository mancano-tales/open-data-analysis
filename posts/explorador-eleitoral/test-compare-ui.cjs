'use strict';
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict');
const source=fs.readFileSync(path.join(__dirname,'compare.js'),'utf8');
const flush=()=>new Promise(resolve=>setImmediate(resolve));
class Element {
  constructor(tag){this.tag=tag;this.children=[];this.events={};this.classList={add(){},remove(){}};this.textContent='';this._value='';}
  append(...nodes){for(const node of nodes){node.parent=this;this.children.push(node);}}
  replaceChildren(...nodes){this.children=[];this.append(...nodes);}
  setAttribute(key,value){this[key]=value;}
  addEventListener(key,handler){this.events[key]=handler;}
  closest(){return this.parent;}
  toggleAttribute(key,present){this[key]=present;}
  get value(){return this._value||this.children[0]?.value||'';}
  set value(value){this._value=String(value);}
}
const entry=(year,office,uf,reference,census)=>({id:`${year}-1-${office}-${uf}${reference==='state'?'-state':''}${census===2010?'-c2010':''}`,
  year,round:1,office,uf,income_reference:reference,census_year:census,url:'',status:'available'});
const entries=[entry(2010,'1','BR','national',2022),entry(2014,'1','BR','national',2022),
  entry(2010,'1','BR','national',2010),entry(2014,'1','BR','national',2010),
  entry(2010,'3','AC','state',2022),entry(2014,'3','AC','state',2022),
  entry(2010,'3','AC','state',2010),entry(2014,'3','AC','state',2010),
  entry(2010,'3','DF','national',2022),entry(2014,'3','DF','national',2022)];
for(const e of entries)e.url=`${e.id}.json`;
const payload=e=>({candidates:[{id:'13',party_number:13},{id:'22',party_number:22}],
  points:[13,22].flatMap(id=>Array.from({length:100},(_,i)=>({candidate_id:String(id),centil:i+1,
    votacao:i===49?null:(id===13?(e.year===2010?30:40):(e.year===2010?20:10)),
    n_setores:8,n_setores_total:10,renda_sm:1})))});
const files=new Map(entries.map(e=>[`data/${e.url}`,payload(e)]));
files.set('data/manifest.json',{schema_version:1,series:entries,parties:{'2010':{'13':'PT'},'2014':{'13':'PT'}}});
async function run(lang){
  const root=new Element('section');root.dataset={lang,base:'data/'};
  const elements=Object.fromEntries(['.compare-controls','.compare-chart','.compare-status','.compare-downloads'].map(k=>[k,new Element(k)]));
  root.querySelector=key=>elements[key];
  const draws=[],purges=[],mainChart=new Element('main-chart'),errors=[];
  const url=class extends URL {static createObjectURL(blob){run.lastCsv=blob;return 'blob:fixture';}};
  const context={URL:url,Map,Set,Promise,Blob,TextDecoder,Uint8Array,Response,DecompressionStream,
    document:{baseURI:'http://localhost/posts/explorador-eleitoral/',createElement:tag=>new Element(tag),querySelectorAll:()=>[root]},
    window:{innerWidth:375},Plotly:{react:(chart,traces,layout)=>draws.push({chart,traces,layout}),purge:chart=>purges.push(chart)},
    fetch:async target=>{const key=new URL(target).pathname.replace('/posts/explorador-eleitoral/','');
      assert.ok(files.has(key),key);return {ok:true,json:async()=>files.get(key)};},console:{error:e=>errors.push(e)}};
  vm.runInNewContext(source,context);await flush();await flush();
  const controls=Object.fromEntries(['office','uf','income_reference','census_year','a','b','party'].map((k,i)=>[k,elements['.compare-controls'].children[i].children[0]]));
  const change=async(key,value)=>{controls[key].value=value;controls[key].events.change();await flush();await flush();};
  assert.equal(errors.length,0);
  assert.match(controls.party.children.find(o=>o.value==='13').textContent,/13.*PT/);
  assert.ok(draws.length);assert.ok(draws.every(d=>d.chart===elements['.compare-chart']&&d.chart!==mainChart));
  assert.equal(draws.at(-1).traces[0].y[0],10,'B minus A');
  assert.equal(draws.at(-1).traces[0].y[49],null,'uncovered centile remains empty');
  assert.equal(draws.at(-1).layout.hovermode,'x unified');
  assert.equal(elements['.compare-downloads'].children.length,1);
  assert.match(await run.lastCsv.text(),/centil,votacao_a,votacao_b,diferenca_pp/);
  await change('b',controls.a.value);
  assert.match(elements['.compare-status'].textContent,lang==='pt'?/não podem ser comparadas/:/cannot be compared/);
  assert.equal(elements['.compare-downloads'].children.length,0);
  await change('b',entries[1].id);
  await change('census_year','2010');assert.equal(controls.census_year.value,'2010');
  assert.ok(draws.at(-1).chart===elements['.compare-chart']);
  await change('office','3');await change('uf','AC');await change('income_reference','state');
  assert.equal(controls.office.value,'3');assert.equal(controls.uf.value,'AC');assert.equal(controls.income_reference.value,'state');
  await change('census_year','2022');await change('uf','DF');assert.equal(controls.uf.value,'DF');
  assert.equal(controls.income_reference.value,'national');
  // A party missing from one election must never be offered or plotted.
  const missing=files.get(`data/${entries[1].url}`);missing.candidates=missing.candidates.filter(c=>c.id!=='22');
  missing.points=missing.points.filter(p=>p.candidate_id!=='22');
  await change('office','1');await change('uf','BR');
  assert.ok(!controls.party.children.some(o=>o.value==='22'));
  assert.ok(draws.every(d=>d.chart!==mainChart));
  assert.equal(errors.length,0);
  return {draws:draws.length,purges:purges.length};
}
(async()=>{for(const lang of ['pt','en'])await run(lang);console.log('test-compare-ui OK');})().catch(error=>{console.error(error);process.exitCode=1;});
