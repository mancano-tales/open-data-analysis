// Unit-level DOM substitute; visual/browser verification is a separate check.
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict');
const source=__dirname,base='http://localhost/posts/explorador-eleitoral/';
const manifest=JSON.parse(fs.readFileSync(path.join(source,'data/manifest.json'),'utf8'));
assert.ok(manifest.series.length>0);
for(const item of process.argv.includes('--ui-only')?[]:manifest.series){
  const data=JSON.parse(fs.readFileSync(path.join(source,'data',item.url),'utf8'));
  assert.equal(data.metadata.vote_audit.status,'reconciled');
  assert.equal(data.metadata.vote_audit.difference,0);
  assert.equal(data.metadata.percentile_scope,(item.income_reference||'national')==='state'?item.uf:'BR');
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
  after(...items){const siblings=this.parent.children;siblings.splice(siblings.indexOf(this)+1,0,...items);items.forEach(i=>i.parent=this.parent);}
  setAttribute(k,v){this[k]=v;}addEventListener(k,fn){this.events[k]=fn;}
  get value(){return this._value||(this.tag==='select'?this.children[0]?.value:'')||'';}set value(v){this._value=v;}
}
function harness(lang='pt',auditOverride=null,fixtures=null){
const root=new Element('section');root.dataset={lang,base:'data/'};
const selectors={};for(const s of ['.chart-status','.chart','.explorer-controls','.coverage','.candidate-list','legend','fieldset','.download-links'])selectors[s]=new Element(s);
selectors.fieldset.append(selectors.legend,selectors['.candidate-list']);
root.append(selectors['.explorer-controls'],selectors.fieldset,selectors['.chart-status'],selectors['.chart'],selectors['.coverage'],selectors['.download-links']);root.querySelector=s=>selectors[s];
const errors=[],draws=[],fetchCalls=[],purges=[];
const context={URL,Set,Map,console:{error:e=>errors.push(e)},window:{innerWidth:1200},document:{baseURI:base,createElement:t=>new Element(t),createTextNode:t=>({textContent:t}),querySelectorAll:()=>[root]},Plotly:{react:(chart,traces,layout)=>draws.push({traces,layout}),purge:chart=>purges.push(chart)},fetch:async (url,options)=>{fetchCalls.push({url:String(url),options});return {ok:true,json:async()=>{const relative=new URL(url).pathname.replace('/posts/explorador-eleitoral/','');const data=fixtures?JSON.parse(JSON.stringify(fixtures[relative])):JSON.parse(fs.readFileSync(path.join(source,relative),'utf8'));if(data.metadata&&auditOverride)data.metadata.vote_audit={...data.metadata.vote_audit,...auditOverride};return data;}};}};
return {root,selectors,errors,draws,context,fetchCalls,purges};
}
const flush=()=>new Promise(resolve=>setImmediate(resolve));
const textContent=element=>[element.textContent||'',...(element.children||[]).map(textContent)].join(' ');
(async()=>{
  const {root,selectors,errors,draws,context}=harness();
  vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),context);await flush();await flush();
  assert.equal(errors.length,0);const n=draws.at(-1).traces.length;assert.ok(n>0&&n<=6);
  assert.equal(selectors['.explorer-controls'].children.length,5);
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
  // Explicit TEST-ONLY in-memory fixtures. Distinct y/renda values ensure
  // selecting a basis fetches that basis rather than the first contest entry.
  const fixtureEntries=[
    {uf:'BR',office:'1'}, {uf:'AC',office:'1'}, {uf:'AC',office:'1',income_reference:'state'},
    {uf:'DF',office:'1'}, {uf:'DF',office:'1',income_reference:'state'},
    {uf:'AC',office:'6'}, {uf:'AC',office:'6',income_reference:'state'}, {uf:'DF',office:'6'}
  ].map(entry=>{const id=`fixture-2026-1-${entry.office}-${entry.uf}${entry.income_reference==='state'?'-state':''}`;return {...entry,id,year:2026,round:1,office_label:entry.office==='1'?'Presidente':'Deputado federal',status:'available',url:`${id}.json`};});
  const fixtures={'data/manifest.json':{schema_version:1,series:fixtureEntries}};
  for(const entry of fixtureEntries){const state=entry.income_reference==='state';fixtures[`data/${entry.url}`]={metadata:{percentile_scope:state?entry.uf:'BR',method:'TEST FIXTURE ONLY',income_definition:'TEST FIXTURE ONLY',coverage:{included_tracts:100,total_tracts:100},sources:[]},candidates:[{id:'13',name:'Fixture Alpha'},{id:'22',name:'Fixture Beta'}],points:['13','22'].flatMap(candidate_id=>Array.from({length:100},(_,index)=>({candidate_id,centil:index+1,renda_sm:state?1:100,n_setores:1,n_setores_total:1,votacao:candidate_id==='13'?(state?60:70):(state?40:30)})))};}
  for(const lang of ['pt','en']){
    const h=harness(lang,null,fixtures);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    const controls=h.selectors['.explorer-controls'].children.map(label=>label.children[0]);
    const change=async(index,value)=>{controls[index].value=value;controls[index].events.change();await flush();await flush();};
    assert.equal(controls[4].value,'national');assert.equal(h.draws.at(-1).traces[0].y[0],70);
    assert.equal(h.fetchCalls.find(call=>call.url.endsWith('manifest.json')).options.cache,'no-cache');
    await change(3,'AC');assert.equal(controls[4].value,'national');
    const checkbox=h.selectors['.candidate-list'].children[0].children[0];checkbox.checked=false;checkbox.events.change();
    const candidateKept=h.draws.at(-1).traces[0].name;
    await change(4,'state');assert.equal(h.draws.at(-1).traces.length,1);assert.equal(h.draws.at(-1).traces[0].name,candidateKept);assert.equal(h.draws.at(-1).traces[0].y[0],40);
    assert.match(h.draws.at(-1).layout.xaxis.title.text,/AC/);assert.match(textContent(h.selectors['.coverage']),lang==='pt'?/Referência estadual/:/State reference/);
    await change(3,'DF');assert.equal(controls[4].value,'state');
    await change(3,'BR');assert.equal(controls[4].value,'national');assert.equal(controls[4].children.length,1);
    assert.match(textContent(h.root.children.find(el=>el.className==='reference-notice')),lang==='pt'?/não está disponível/:/unavailable/);
    await change(3,'AC');assert.equal(controls[4].value,'state');
    await change(2,'6');assert.equal(controls[4].value,'state');
    await change(4,'national');await change(3,'DF');assert.equal(controls[4].value,'national');
    await change(3,'AC');await change(4,'state');await change(3,'DF');assert.equal(controls[4].value,'national');
    await change(3,'AC');assert.equal(controls[4].value,'state');
    await change(2,'1');assert.equal(controls[4].value,'national');
    assert.equal(h.errors.length,0);
  }
  // Reject the former literal-UF bug and incompatible state/national data.
  for(const scope of ['UF','DF','BR',undefined]){
    const mismatch=JSON.parse(JSON.stringify(fixtures));
    const stateEntry=fixtureEntries.find(entry=>entry.uf==='AC'&&entry.income_reference==='state');
    mismatch[`data/${stateEntry.url}`].metadata.percentile_scope=scope;
    const h=harness('pt',null,mismatch);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    const controls=h.selectors['.explorer-controls'].children.map(label=>label.children[0]);
    controls[3].value='AC';controls[3].events.change();await flush();await flush();
    const before=h.draws.length;controls[4].value='state';controls[4].events.change();await flush();await flush();
    assert.equal(h.errors.length,1);assert.equal(h.draws.length,before);assert.equal(h.purges.length,1);
    assert.match(h.selectors['.chart-status'].textContent,/Série recusada.*universo AC/);
    assert.equal(h.selectors['.coverage'].children.length,0);
  }
  const realAC=manifest.series.find(entry=>entry.uf==='AC'&&entry.office==='1'&&entry.income_reference==='state');
  assert.ok(realAC,'Real AC-state series must be available for this regression test');
  const acData=JSON.parse(fs.readFileSync(path.join(source,'data',realAC.url),'utf8'));
  assert.equal(acData.metadata.percentile_scope,'AC');
  const realFixtures={'data/manifest.json':{schema_version:1,series:[realAC]},[`data/${realAC.url}`]:acData};
  for(const lang of ['pt','en']){
    const h=harness(lang,null,realFixtures);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    assert.equal(h.errors.length,0);assert.match(h.draws.at(-1).layout.xaxis.title.text,/AC/);
    assert.match(textContent(h.selectors['.coverage']),lang==='pt'?/Referência estadual.*setores de AC/:/State reference.*AC tracts/);
    assert.doesNotMatch(textContent(h.selectors['.coverage']),/Snapshot legado|Legacy snapshot/);
  }
  console.log(`PASS: UI selection/search/reload; PT/EN provisional warnings; national/state defaults, preferences, explicit fallback, candidate preservation, legacy manifest, no-cache refresh, real AC-state scope and rejection of incompatible scope; ${process.argv.includes('--ui-only')?'dataset sweep skipped':manifest.series.length+' real datasets validated'}.`);
})().catch(e=>{console.error(e);process.exitCode=1;});
