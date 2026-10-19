// Unit-level DOM substitute; visual/browser verification is a separate check.
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict'),zlib=require('node:zlib');
const source=__dirname,base='http://localhost/posts/explorador-eleitoral/';
const manifest=JSON.parse(fs.readFileSync(path.join(source,'data/manifest.json'),'utf8'));
assert.ok(manifest.series.length>0);
if(manifest.parties)for(const [year,map] of Object.entries(manifest.parties)){
  assert.match(year,/^20\d{2}$/);
  for(const [number,acronym] of Object.entries(map)){
    assert.match(number,/^\d{2}$/);assert.ok(typeof acronym==='string'&&acronym.length>0);
  }
}
for(const item of process.argv.includes('--ui-only')?[]:manifest.series){
  const data=JSON.parse(fs.readFileSync(path.join(source,'data',item.url),'utf8'));
  assert.equal(data.metadata.vote_audit.status,'reconciled');
  assert.equal(data.metadata.vote_audit.difference,0);
  assert.equal(data.metadata.percentile_scope,(item.income_reference||'national')==='state'?item.uf:'BR');
  if(item.census_year===2010){
    assert.match(item.id,/-c2010$/);
    assert.equal(data.metadata.income_year,2010);
    assert.equal(data.metadata.minimum_wage_brl,510);
    assert.match(JSON.stringify(data.metadata.income_definition),/V007/);
    for(const candidate of data.candidates)
      assert.equal(data.points.filter(p=>p.candidate_id===candidate.id).reduce((n,p)=>n+p.n_setores,0),data.metadata.coverage.included_tracts);
  }
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
const errors=[],draws=[],fetchCalls=[],purges=[],documentEvents={};
const document={baseURI:base,createElement:t=>new Element(t),createTextNode:t=>({textContent:t}),querySelectorAll:()=>[root],addEventListener:(type,handler)=>{documentEvents[type]=handler;},dispatchEvent:event=>documentEvents[event.type]?.(event)};
const context={URL,Set,Map,Uint8Array,TextDecoder,Response,Blob,DecompressionStream,console:{error:e=>errors.push(e)},window:{innerWidth:1200},document,Plotly:{react:(chart,traces,layout)=>draws.push({traces,layout}),purge:chart=>purges.push(chart)},fetch:async (url,options)=>{fetchCalls.push({url:String(url),options});const relative=new URL(url).pathname.replace('/posts/explorador-eleitoral/','');const bytes=fixtures?(Buffer.isBuffer(fixtures[relative])?fixtures[relative]:Buffer.from(JSON.stringify(fixtures[relative]))):fs.readFileSync(path.join(source,relative));return {ok:true,arrayBuffer:async()=>bytes.buffer.slice(bytes.byteOffset,bytes.byteOffset+bytes.byteLength),json:async()=>{const data=JSON.parse(bytes.toString('utf8'));if(data.metadata&&auditOverride)data.metadata.vote_audit={...data.metadata.vote_audit,...auditOverride};return data;}};}};
return {root,selectors,errors,draws,context,fetchCalls,purges};
}
const flush=()=>new Promise(resolve=>setImmediate(resolve));
const textContent=element=>[element.textContent||'',...(element.children||[]).map(textContent)].join(' ');
(async()=>{
  const {root,selectors,errors,draws,context}=harness();
  vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),context);await flush();await flush();
  assert.equal(errors.length,0);const n=draws.at(-1).traces.length;assert.ok(n>0&&n<=6);
  assert.equal(selectors['.explorer-controls'].children.length,6);
  const selects=selectors['.explorer-controls'].children.map(x=>x.children[0]);assert.equal(selects[2].value,'1');assert.equal(selects[3].value,'BR');
  const search=selectors.fieldset.children.find(el=>el.className==='candidate-search').children[0];
  search.value=draws.at(-1).traces[0].name.normalize('NFD').replace(/[\u0300-\u036f]/g,'').toLowerCase();search.events.input();
  assert.equal(selectors['.candidate-list'].children.length,1);
  const checkbox=selectors['.candidate-list'].children[0].children[0],selectedId=checkbox.value;assert.ok(checkbox.checked);checkbox.checked=false;checkbox.events.change();assert.equal(draws.at(-1).traces.length,n-1);
  search.value='unmatched-name';search.events.input();assert.equal(draws.at(-1).traces.length,n-1);
  const tools=selectors.fieldset.children.find(el=>el.className==='candidate-tools');tools.children[1].events.click();assert.equal(draws.at(-1).traces.length,0);assert.match(selectors['.chart-status'].textContent,/Selecione/);
  root.querySelector('.chart').scrollIntoView=()=>{};
  context.document.dispatchEvent({type:'explorer:select',detail:{id:selectedId}});
  assert.equal(draws.at(-1).traces.length,1,'loaded candidate is added from concentration row');
  const validDrawCount=draws.length;
  context.document.dispatchEvent({type:'explorer:select',detail:{id:'not-in-loaded-series'}});
  assert.equal(draws.length,validDrawCount,'unknown id is ignored');
  tools.children[0].events.click();assert.equal(draws.at(-1).traces.length,n);
  // Bulk actions include every record, independently of the active search.
  tools.children[2].events.click();
  const allCount=draws.at(-1).traces.length;assert.ok(allCount>n);
  assert.match(selectors.fieldset.children.find(el=>el.className==='candidate-tools').children[2].textContent,/Incluir todos/);
  assert.equal(selectors['.candidate-list'].children.length,1,'Search stays active');
  tools.children[1].events.click();assert.equal(draws.at(-1).traces.length,0);
  tools.children[0].events.click();
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
    {uf:'AC',office:'6'}, {uf:'AC',office:'6',income_reference:'state'}, {uf:'DF',office:'6'},
    {uf:'AC',office:'3'}, {uf:'AC',office:'3',income_reference:'state'},
    {uf:'BA',office:'3'}, {uf:'BA',office:'3',income_reference:'state'}
  ].map(entry=>{const id=`fixture-2026-1-${entry.office}-${entry.uf}${entry.income_reference==='state'?'-state':''}`;return {...entry,id,year:2026,round:1,office_label:entry.office==='1'?'Presidente':entry.office==='3'?'Governador':'Deputado federal',status:'available',url:`${id}.json`};});
  const fixtures={'data/manifest.json':{schema_version:1,series:fixtureEntries}};
  for(const entry of fixtureEntries){const state=entry.income_reference==='state';fixtures[`data/${entry.url}`]={metadata:{percentile_scope:state?entry.uf:'BR',method:'TEST FIXTURE ONLY',income_definition:'TEST FIXTURE ONLY',coverage:{included_tracts:100,total_tracts:100},sources:[]},candidates:[{id:'13',name:'Fixture Alpha'},{id:'22',name:'Fixture Beta'}],points:['13','22'].flatMap(candidate_id=>Array.from({length:100},(_,index)=>({candidate_id,centil:index+1,renda_sm:state?1:100,n_setores:1,n_setores_total:1,votacao:candidate_id==='13'?(state?60:70):(state?40:30)})))};}
  for(const lang of ['pt','en']){
    const h=harness(lang,null,fixtures);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    const controls=h.selectors['.explorer-controls'].children.map(label=>label.children[0]);
    const change=async(index,value)=>{controls[index].value=value;controls[index].events.change();await flush();await flush();};
    assert.equal(controls[5].value,'national');assert.equal(h.draws.at(-1).traces[0].y[0],70);
    assert.equal(h.fetchCalls.find(call=>call.url.endsWith('manifest.json')).options.cache,'no-cache');
    await change(3,'AC');assert.equal(controls[5].value,'national');
    const checkbox=h.selectors['.candidate-list'].children[0].children[0];checkbox.checked=false;checkbox.events.change();
    const candidateKept=h.draws.at(-1).traces[0].name;
    await change(5,'state');assert.equal(h.draws.at(-1).traces.length,1);assert.equal(h.draws.at(-1).traces[0].name,candidateKept);assert.equal(h.draws.at(-1).traces[0].y[0],40);
    assert.match(h.draws.at(-1).layout.xaxis.title.text,/AC/);assert.match(textContent(h.selectors['.coverage']),lang==='pt'?/Referência estadual/:/State reference/);
    await change(3,'DF');assert.equal(controls[5].value,'state');
    await change(3,'BR');assert.equal(controls[5].value,'national');assert.equal(controls[5].children.length,1);
    assert.match(textContent(h.root.children.find(el=>el.className==='reference-notice')),lang==='pt'?/não está disponível/:/unavailable/);
    await change(3,'AC');assert.equal(controls[5].value,'state');
    await change(2,'6');assert.equal(controls[5].value,'state');
    await change(5,'national');await change(3,'DF');assert.equal(controls[5].value,'national');
    await change(3,'AC');await change(5,'state');await change(3,'DF');assert.equal(controls[5].value,'national');
    await change(3,'AC');assert.equal(controls[5].value,'state');
    await change(2,'1');assert.equal(controls[5].value,'national');
    await change(2,'3');await change(3,'BA');assert.equal(controls[3].value,'BA');
    await change(2,'6');assert.equal(controls[3].value,'AC');
    const territoryNotice=h.root.children.find(el=>el.className==='territory-notice');
    assert.equal(territoryNotice.hidden,false);assert.match(textContent(territoryNotice),/BA/);assert.match(textContent(territoryNotice),/AC/);
    assert.match(textContent(territoryNotice),lang==='pt'?/território exibido mudou/:/displayed territory changed/);
    assert.match(h.draws.at(-1).layout.xaxis.title.text,/AC/);
    await change(3,'DF');assert.equal(territoryNotice.hidden,true);assert.equal(territoryNotice.textContent,'');
    assert.equal(h.errors.length,0);
  }
  // More than the visible-list cap: bulk selection must include hidden records.
  for(const lang of ['pt','en']){
    const many=JSON.parse(JSON.stringify(fixtures));const entry=fixtureEntries[0];
    const payload=many[`data/${entry.url}`];
    payload.candidates=Array.from({length:105},(_,i)=>({id:`record-${i}`,name:`Test record ${i}`}));
    payload.points=payload.candidates.map(c=>({candidate_id:c.id,centil:1,renda_sm:1,n_setores:1,n_setores_total:1,votacao:1}));
    const h=harness(lang,null,many);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    const tools=h.selectors.fieldset.children.find(el=>el.className==='candidate-tools');
    const search=h.selectors.fieldset.children.find(el=>el.className==='candidate-search').children[0];
    search.value='Test record 104';search.events.input();
    tools.children[2].events.click();assert.equal(h.draws.at(-1).traces.length,105);
    assert.equal(search.value,'Test record 104');assert.equal(h.selectors['.candidate-list'].children.length,1);
    tools.children[1].events.click();assert.equal(h.draws.at(-1).traces.length,0);
    search.value='';search.events.input();assert.equal(h.selectors['.candidate-list'].children.length,100);
    assert.equal(h.selectors['.candidate-list'].children.filter(x=>x.children[0]?.checked).length,0);
    assert.equal(h.errors.length,0);
  }
  // Compact presentation is opt-in and restricted to an iframe context.
  for(const requested of [false,true])for(const framed of [false,true]){
    const h=harness('en',null,fixtures);const self={};const classes=[];
    h.context.window.self=self;h.context.window.top=framed?{}:self;
    h.context.document.baseURI=base+(requested?'?panel=1':'');
    h.context.document.body={classList:{add:value=>classes.push(value)}};
    vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    assert.equal(classes.includes('embedded-explorer'),requested&&framed);
    const note=h.root.children.find(el=>el.className==='embedded-note');
    assert.equal(Boolean(note),requested&&framed);
    if(note){assert.equal(h.draws.at(-1).traces.length,0);assert.equal(h.selectors.fieldset.children.find(el=>el.className==='candidate-tools').children[0].hidden,true);assert.equal(note.children.find(el=>el.tag==='a').href,base);assert.equal(note.children.find(el=>el.tag==='a').target,'_blank');}
    assert.equal(h.errors.length,0);
  }
  // Browser gzip decoding, including a host that already decoded transport gzip.
  const compressedEntry={...fixtureEntries[0],url:fixtureEntries[0].url+'.gz'};
  const originalBytes=Buffer.from(JSON.stringify(fixtures[`data/${fixtureEntries[0].url}`]));
  for(const bytes of [zlib.gzipSync(originalBytes,{level:9}),originalBytes]){
    const compactFixtures={'data/manifest.json':{series:[compressedEntry]},[`data/${compressedEntry.url}`]:bytes};
    const h=harness('pt',null,compactFixtures);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);
    for(let step=0;step<30&&!h.draws.length&&!h.errors.length;step++)await new Promise(resolve=>setTimeout(resolve,5));
    assert.equal(h.errors.length,0);assert.equal(h.draws.at(-1).traces[0].y[0],70);
    assert.match(h.selectors['.download-links'].children[0].textContent,/JSON.gz/);
    assert.ok(String(h.selectors['.download-links'].children[0].href).endsWith('.json.gz'));
  }
  // Reject the former literal-UF bug and incompatible state/national data.
  for(const scope of ['UF','DF','BR',undefined]){
    const mismatch=JSON.parse(JSON.stringify(fixtures));
    const stateEntry=fixtureEntries.find(entry=>entry.uf==='AC'&&entry.income_reference==='state');
    mismatch[`data/${stateEntry.url}`].metadata.percentile_scope=scope;
    const h=harness('pt',null,mismatch);vm.runInNewContext(fs.readFileSync(path.join(source,'app.js'),'utf8'),h.context);await flush();await flush();
    const controls=h.selectors['.explorer-controls'].children.map(label=>label.children[0]);
    controls[3].value='AC';controls[3].events.change();await flush();await flush();
    const before=h.draws.length;controls[5].value='state';controls[5].events.change();await flush();await flush();
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
  console.log(`PASS: UI selection/search/reload; PT/EN provisional warnings; income-reference defaults/fallback/preservation; gzip/plain browser decoding; no-cache manifest; real AC-state and incompatible-scope rejection; ${process.argv.includes('--ui-only')?'dataset sweep skipped':manifest.series.length+' real datasets validated'}.`);
})().catch(e=>{console.error(e);process.exitCode=1;});
