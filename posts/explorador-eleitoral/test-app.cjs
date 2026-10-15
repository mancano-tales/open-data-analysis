// Unit-level DOM substitute; visual/browser verification is a separate check.
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict');
const source=__dirname,base='http://localhost/posts/explorador-eleitoral/';
const manifest=JSON.parse(fs.readFileSync(path.join(source,'data/manifest.json'),'utf8'));
assert.ok(manifest.series.length>0);
for(const item of process.argv.includes('--ui-only')?[]:manifest.series){
  const data=JSON.parse(fs.readFileSync(path.join(source,'data',item.url),'utf8'));
  assert.equal(data.metadata.vote_audit.status,'reconciled');
  assert.equal(data.metadata.vote_audit.difference,0);
  assert.equal(data.metadata.percentile_scope,'BR');
  assert.equal(data.points.length,100*data.candidates.length);
  const centils=new Map(data.candidates.map(c=>[c.id,new Set()]));
  for(const p of data.points){assert.ok(centils.has(p.candidate_id));centils.get(p.candidate_id).add(p.centil);}
  for(const c of data.candidates)assert.equal(centils.get(c.id).size,100);
  assert.match(data.metadata.method,/Média/,'UTF-8 method must survive export');
  for(const p of data.points){assert.ok(p.n_setores<=p.n_setores_total);assert.ok(p.votacao===null||p.votacao>=0&&p.votacao<=100);}
}
class Element{
  constructor(tag){this.tag=tag;this.children=[];this.events={};this.style={setProperty(){}};this.classList={add(){},remove(){}};this.textContent='';this._value='';}
  append(...items){for(const item of items){item.parent=this;this.children.push(item);}}
  replaceChildren(...items){this.children=[];this.append(...items);}
  before(...items){const siblings=this.parent.children;for(const item of items){const i=siblings.indexOf(item);if(i>=0)siblings.splice(i,1);}siblings.splice(siblings.indexOf(this),0,...items);items.forEach(i=>i.parent=this.parent);}
  setAttribute(k,v){this[k]=v;}addEventListener(k,fn){this.events[k]=fn;}
  get value(){return this._value||(this.tag==='select'?this.children[0]?.value:'')||'';}set value(v){this._value=v;}
}
function harness(lang='pt',auditOverride=null){
const root=new Element('section');root.dataset={lang,base:'data/'};
const selectors={};for(const s of ['.chart-status','.chart','.explorer-controls','.coverage','.candidate-list','legend','fieldset','.download-links'])selectors[s]=new Element(s);
selectors.fieldset.append(selectors.legend,selectors['.candidate-list']);
root.append(selectors['.explorer-controls'],selectors.fieldset,selectors['.chart-status'],selectors['.chart'],selectors['.coverage'],selectors['.download-links']);root.querySelector=s=>selectors[s];
const errors=[],draws=[];
const context={URL,Set,Map,console:{error:e=>errors.push(e)},window:{innerWidth:1200},document:{baseURI:base,createElement:t=>new Element(t),createTextNode:t=>({textContent:t}),querySelectorAll:()=>[root]},Plotly:{react:(chart,traces,layout)=>draws.push({traces,layout})},fetch:async url=>({ok:true,json:async()=>{const data=JSON.parse(fs.readFileSync(path.join(source,new URL(url).pathname.replace('/posts/explorador-eleitoral/','')),'utf8'));if(data.metadata&&auditOverride)data.metadata.vote_audit={...data.metadata.vote_audit,...auditOverride};return data;}})};
return {root,selectors,errors,draws,context};
}
const flush=()=>new Promise(resolve=>setImmediate(resolve));
const textContent=element=>[element.textContent||'',...(element.children||[]).map(textContent)].join(' ');
(async()=>{
  const {root,selectors,errors,draws,context}=harness();
  vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),context);await flush();await flush();
  assert.equal(errors.length,0);const n=draws.at(-1).traces.length;assert.ok(n>0&&n<=6);
  assert.equal(selectors['.explorer-controls'].children.length,4);
  const selects=selectors['.explorer-controls'].children.map(x=>x.children[0]);assert.equal(selects[2].value,'1');assert.equal(selects[3].value,'BR');
  const search=selectors.fieldset.children.find(el=>el.className==='candidate-search').children[0];
  search.value=draws.at(-1).traces[0].name.normalize('NFD').replace(/[\u0300-\u036f]/g,'').toLowerCase();search.events.input();
  assert.equal(selectors['.candidate-list'].children.length,1);
  const checkbox=selectors['.candidate-list'].children[0].children[0];assert.ok(checkbox.checked);checkbox.checked=false;checkbox.events.change();assert.equal(draws.at(-1).traces.length,n-1);
  search.value='unmatched-name';search.events.input();assert.equal(draws.at(-1).traces.length,n-1);
  const tools=selectors.fieldset.children.find(el=>el.className==='candidate-tools');tools.children[1].events.click();assert.equal(draws.at(-1).traces.length,0);assert.match(selectors['.chart-status'].textContent,/Selecione/);
  tools.children[0].events.click();assert.equal(draws.at(-1).traces.length,n);
  selects[0].events.change();await flush();await flush();assert.equal(draws.at(-1).traces.length,n);
  assert.equal(selectors['.download-links'].children.length,2);assert.equal(errors.length,0);
  // Explicit in-memory fixture: numeric reconciliation remains successful,
  // but a provisional EA20 snapshot must never appear legally final.
  const provisional={status:'reconciled',official_valid_votes:100,computed_valid_votes:100,difference:0,provisional:true,snapshots:{'6':{office:6,generation_date:'13/10/2026',generation_time:'16:26:47',final_totalization:'n',progress:'p',url:'https://resultados.tse.jus.br/test-only.json'}}};
  for(const lang of ['pt','en']){
    const h=harness(lang,provisional);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    assert.equal(h.errors.length,0);
    const panel=h.root.children.find(el=>el.className==='audit');
    assert.match(textContent(panel),lang==='pt'?/Dados provisórios/:/Provisional data/);
    assert.match(textContent(panel),lang==='pt'?/não significa totalização jurídica final/:/do not establish final legal totalization/);
    assert.match(textContent(panel),/13\/10\/2026 16:26:47/);
    assert.match(textContent(panel),lang==='pt'?/Votos reconciliados/:/Votes reconciled/);
    assert.equal(panel.children.find(el=>el.className==='provisional-warning').tag,'p');
    assert.equal(panel.children.flatMap(el=>el.children||[]).find(el=>el.tag==='a')?.href,'https://resultados.tse.jus.br/test-only.json');
  }
  const final=harness('pt',{provisional:false});vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),final.context);await flush();await flush();
  assert.doesNotMatch(textContent(final.root.children.find(el=>el.className==='audit')),/Dados provisórios/);
  console.log(`PASS: UI selectors/search/selection/reload and PT/EN provisional-snapshot warnings; ${process.argv.includes('--ui-only')?'dataset sweep skipped':manifest.series.length+' real datasets validated'}.`);
})().catch(e=>{console.error(e);process.exitCode=1;});
