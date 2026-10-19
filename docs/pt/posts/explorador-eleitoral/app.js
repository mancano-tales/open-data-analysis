/* Aggregate-only static explorer. See DATA-CONTRACT.md for the input format. */
(() => {
  'use strict';
  const embeddedPanel=new URL(document.baseURI).searchParams.get('panel')==='1'&&window.self!==window.top;
  if(embeddedPanel)document.body.classList.add('embedded-explorer');
  const copy = {
    pt: {year:'Eleição',round:'Turno',office:'Cargo',uf:'Território',candidates:'Candidatos: selecione as curvas',loading:'Carregando os dados agregados…',error:'Não foi possível carregar o explorador. Use os arquivos de dados abaixo ou recarregue a página.',x:'Centis dos setores por renda (mais pobres → mais ricos)',y:'Votação estimada (%)',income:'Renda média',tracts:'Setores no centil',centil:'Centil',coverage:'Cobertura',of:'de',included:'setores incluídos',download:'Baixar esta série (JSON)',manifest:'Ver séries disponíveis (JSON)',none:'Selecione ao menos um candidato para mostrar uma curva.',available:'Séries disponíveis',sources:'Fontes',roundLabel:n=>`${n}º turno`,national:'Brasil',votes:'dos votos válidos',method:'Estimador',incomeDef:'Renda',noPlotly:'A biblioteca do gráfico não carregou. Verifique a conexão e recarregue a página.'},
    en: {year:'Election',round:'Round',office:'Office',uf:'Territory',candidates:'Candidates: select curves',loading:'Loading aggregate data…',error:'The explorer could not load. Use the data files below or reload the page.',x:'Tract income percentiles (poorest → richest)',y:'Estimated vote share (%)',income:'Mean income',tracts:'Tracts in percentile',centil:'Percentile',coverage:'Coverage',of:'of',included:'tracts included',download:'Download this series (JSON)',manifest:'Available series (JSON)',none:'Select at least one candidate to display a curve.',available:'Available series',sources:'Sources',roundLabel:n=>`Round ${n}`,national:'Brazil',votes:'of valid votes',method:'Estimator',incomeDef:'Income',noPlotly:'The chart library did not load. Check your connection and reload the page.'}
  };
  async function start(root) {
    const lang = root.dataset.lang === 'pt' ? 'pt' : 'en';
    const t = copy[lang], locale = lang === 'pt' ? 'pt-BR' : 'en-US';
    const fmt = (v,d=0) => Number(v).toLocaleString(locale,{minimumFractionDigits:d,maximumFractionDigits:d});
    const base = new URL(root.dataset.base,document.baseURI);
    const status = root.querySelector('.chart-status'), chart=root.querySelector('.chart');
    const controls=root.querySelector('.explorer-controls'), coverage=root.querySelector('.coverage');
    const candidateBox=root.querySelector('.candidate-list');
    const availability=document.createElement('p');availability.className='available-series';controls.before(availability);
    const auditPanel=document.createElement('div');auditPanel.className='audit';auditPanel.setAttribute('role','status');coverage.before(auditPanel);
    root.querySelector('legend').textContent=t.candidates;
    const keys=['year','round','office','uf','census_year','income_reference'], selects={};
    t.income_reference=lang==='pt'?'Referência de renda':'Income reference';t.census_year=lang==='pt'?'Censo de referência':'Reference census';
    const text=(tag,value,parent)=>{const el=document.createElement(tag);el.textContent=value;parent.append(el);return el;};
    if(embeddedPanel){
      const note=document.createElement('p');note.className='embedded-note';
      note.append(document.createTextNode(lang==='pt'?'A renda é do setor, não do eleitor. ':'Income describes the tract, not the voter. '));
      const full=text('a',lang==='pt'?'Página completa e metodologia':'Full page and methodology',note);
      const url=new URL(document.baseURI);url.searchParams.delete('panel');full.href=url.href;full.target='_blank';full.rel='noopener';
      controls.before(note);
    }

    keys.forEach(key=>{const label=text('label',t[key],controls);const select=document.createElement('select');select.setAttribute('aria-label',t[key]);label.append(select);selects[key]=select;});
    status.textContent=t.loading;
    let manifest, selected, data, request=0, activeIds=new Set(), rankedCandidates=[], preferredReference=null, previousOffice=null, loadedContestKey=null;
    const referenceNotice=document.createElement('p');referenceNotice.className='reference-notice';referenceNotice.setAttribute('role','status');controls.after(referenceNotice);
    const territoryNotice=document.createElement('p');territoryNotice.className='territory-notice';territoryNotice.setAttribute('role','status');territoryNotice.hidden=true;referenceNotice.after(territoryNotice);
    const searchLabel=text('label',lang==='pt'?'Buscar candidato por nome, número ou partido':'Search candidates by name, number or party',root.querySelector('fieldset'));
    const search=document.createElement('input');search.type='search';search.placeholder=lang==='pt'?'Digite um nome ou número…':'Enter a name or number…';search.setAttribute('aria-label',searchLabel.textContent);searchLabel.append(search);searchLabel.className='candidate-search';
    const candidateTools=document.createElement('div');candidateTools.className='candidate-tools';
    const topButton=text('button',lang==='pt'?'Mostrar 6 principais':'Show leading 6',candidateTools);topButton.type='button';topButton.hidden=embeddedPanel;
    const clearButton=text('button',lang==='pt'?'Limpar seleção':'Clear selection',candidateTools);clearButton.type='button';
    const allButton=text('button',lang==='pt'?'Incluir todos':'Include all',candidateTools);allButton.type='button';
    allButton.title=lang==='pt'?'Inclui todos os candidatos desta disputa, mesmo os ocultos pela busca.':'Includes every candidate in this contest, including those hidden by search.';
    topButton.disabled=true;clearButton.disabled=true;allButton.disabled=true;search.disabled=true;
    const selectionInfo=text('span','',candidateTools);selectionInfo.setAttribute('aria-live','polite');
    candidateBox.before(searchLabel,candidateTools);
    function candidateLabel(candidate) {const number=candidate.number??(/^\d+$/.test(candidate.id)?candidate.id:null);return `${number==null?'':`${number} · `}${candidate.name}${candidate.party?` (${candidate.party})`:''}`;}
    function renderCandidates() {
      candidateBox.replaceChildren();
      const normalized=value=>String(value).normalize('NFD').replace(/[\u0300-\u036f]/g,'').toLowerCase();
      const query=normalized(search.value.trim());
      const matches=rankedCandidates.filter(candidate=>normalized(candidateLabel(candidate)).includes(query));
      const shown=matches.slice(0,100);
      shown.forEach(candidate=>{const label=document.createElement('label');label.style.setProperty('--candidate-color',candidate.color||'#354b64');const input=document.createElement('input');input.type='checkbox';input.value=candidate.id;input.checked=activeIds.has(candidate.id);input.addEventListener('change',()=>{if(input.checked)activeIds.add(candidate.id);else activeIds.delete(candidate.id);updateSelectionInfo(matches.length);draw();});label.append(input,document.createTextNode(candidateLabel(candidate)));candidateBox.append(label);});
      if(!shown.length)text('p',lang==='pt'?'Nenhum candidato encontrado.':'No candidates found.',candidateBox);
      updateSelectionInfo(matches.length);
    }
    function updateSelectionInfo(matches) {selectionInfo.textContent=lang==='pt'?`${activeIds.size} curvas selecionadas · ${matches} candidatos encontrados${matches>100?' (lista limitada a 100; refine a busca)':''}`:`${activeIds.size} curves selected · ${matches} candidates found${matches>100?' (showing 100; refine search)':''}`;}
    search.addEventListener('input',renderCandidates);
    // The concentration table asks to show a candidate; only ids of the loaded series are accepted.
    document.addEventListener?.('explorer:select',event=>{const id=event.detail?.id;if(!data||!data.candidates.some(c=>c.id===id))return;activeIds.add(id);renderCandidates();draw();chart.scrollIntoView?.({behavior:'smooth',block:'center'});});
    topButton.addEventListener('click',()=>{activeIds=new Set(rankedCandidates.slice(0,6).map(c=>c.id));renderCandidates();draw();});
    clearButton.addEventListener('click',()=>{activeIds.clear();renderCandidates();draw();});
    allButton.addEventListener('click',()=>{activeIds=new Set(data.candidates.map(c=>c.id));renderCandidates();draw();});
    const seriesUrl=entry=>new URL(entry.url,base);
    function link(parent,label,url) { const a=text('a',label,parent);a.href=url;return a; }
    function localized(value) { return typeof value==='object' ? (value[lang]||value.pt||value.en||'') : value; }
    async function read(url,options) {
      const response=await fetch(url,options);if(!response.ok)throw new Error(`HTTP ${response.status}`);
      if(!new URL(url).pathname.endsWith('.json.gz'))return response.json();
      const bytes=new Uint8Array(await response.arrayBuffer());
      // Pages serves .gz as a file. Also accept hosts that already decoded an
      // HTTP Content-Encoding gzip response before exposing it to fetch().
      if(bytes[0]!==31||bytes[1]!==139)return JSON.parse(new TextDecoder().decode(bytes));
      if(typeof DecompressionStream==='undefined'){
        const error=new Error('Gzip decompression unavailable');error.userMessage=lang==='pt'?'Este navegador não oferece descompactação gzip. Atualize o navegador ou baixe o arquivo JSON.gz para consultar os dados.':'This browser cannot decompress gzip. Update it or download the JSON.gz file to inspect the data.';throw error;
      }
      return new Response(new Blob([bytes]).stream().pipeThrough(new DecompressionStream('gzip'))).json();
    }
    const referenceOf=entry=>entry.income_reference||'national';
    // Series without census_year predate the 2010 reference and use Census 2022.
    const censusOf=entry=>String(entry.census_year||2022);
    const referenceLabel=value=>value==='state'?(lang==='pt'?'UF — renda relativa ao estado':'State — income relative to state'):(lang==='pt'?'Brasil — renda relativa ao país':'Brazil — income relative to country');
    const contestKey=entry=>['year','round','office','uf'].map(key=>entry[key]).join('|')+'|'+censusOf(entry);
    function validateIncomeScope(entry,metadata) {
      const expected=referenceOf(entry)==='state'?entry.uf:'BR';
      if(String(metadata?.income_year||2022)!==censusOf(entry)){const error=new Error(`Census mismatch: expected ${censusOf(entry)}, received ${metadata?.income_year}`);error.userMessage=lang==='pt'?`Série recusada: o censo dos metadados não corresponde ao Censo ${censusOf(entry)} selecionado. Nenhuma curva desta série foi exibida.`:`Series rejected: its metadata census does not match the selected Census ${censusOf(entry)}. No curves from this series were displayed.`;throw error;}
      if(metadata?.percentile_scope!==expected||(referenceOf(entry)==='state'&&entry.uf==='BR')||(metadata.income_reference&&metadata.income_reference!==referenceOf(entry))){
        const error=new Error(`Income reference mismatch: expected ${expected}, received ${metadata?.percentile_scope}`);
        error.userMessage=lang==='pt'?`Série recusada: a referência de renda selecionada exige o universo ${expected}, mas os metadados não correspondem. Nenhuma curva desta série foi exibida.`:`Series rejected: the selected income reference requires scope ${expected}, but its metadata do not match. No curves from this series were displayed.`;
        throw error;
      }
    }
    function resolve(changedIndex=-1) {
      const wantedTerritory=selects.uf.value;
      let subset=manifest.series.filter(s=>s.status==='available'&&!(s.uf==='BR'&&referenceOf(s)==='state'));
      keys.slice(0,5).forEach(key=>{
        const wanted=selects[key].value;
        const values=[...new Set(subset.map(s=>key==='census_year'?censusOf(s):String(s[key])))];
        if(key==='census_year')values.sort((a,b)=>Number(b)-Number(a));
        if(key==='year')values.sort((a,b)=>Number(b)-Number(a));
        if(key==='office'||key==='round')values.sort((a,b)=>Number(a)-Number(b));
        if(key==='uf')values.sort((a,b)=>a==='BR'?-1:b==='BR'?1:a.localeCompare(b));
        selects[key].replaceChildren();
        values.forEach(value=>{const officeLabel=manifest.offices?.[value]||subset.find(s=>String(s.office)===value)?.office_label||value;const option=text('option',key==='round'?t.roundLabel(value):key==='uf'&&value==='BR'?t.national:key==='office'?localized(officeLabel):key==='census_year'?`${lang==='pt'?'Censo':'Census'} ${value}`:value,selects[key]);option.value=value;});
        selects[key].value=values.includes(wanted)?wanted:(values[0]||'');
        subset=subset.filter(s=>(key==='census_year'?censusOf(s):String(s[key]))===selects[key].value);
      });
      const office=selects.office.value;
      if(preferredReference===null||office!==previousOffice||changedIndex===2)preferredReference=office==='1'||office==='presidente'?'national':'state';
      if(changedIndex===5)preferredReference=selects.income_reference.value;
      previousOffice=office;
      const references=[...new Set(subset.map(referenceOf))].sort((a,b)=>a==='national'?-1:b==='national'?1:a.localeCompare(b));
      selects.income_reference.replaceChildren();
      references.forEach(value=>{const option=text('option',referenceLabel(value),selects.income_reference);option.value=value;});
      const actualReference=references.includes(preferredReference)?preferredReference:references[0];
      selects.income_reference.value=actualReference;
      referenceNotice.textContent=actualReference===preferredReference?'':(lang==='pt'?`A referência ${preferredReference==='state'?'estadual':'nacional'} não está disponível nesta combinação${selects.uf.value==='BR'?' (Brasil permite somente a referência nacional)':''}. Exibindo ${referenceLabel(actualReference)}. A preferência será mantida ao trocar de UF.`:`The ${preferredReference==='state'?'state':'national'} reference is unavailable for this selection${selects.uf.value==='BR'?' (Brazil allows the national reference only)':''}. Showing ${referenceLabel(actualReference)}. Your preference is retained when changing state.`);
      const matches=subset.filter(s=>referenceOf(s)===actualReference);
      const territoryChanged=Boolean(wantedTerritory&&wantedTerritory!==selects.uf.value&&changedIndex!==3);
      territoryNotice.hidden=!territoryChanged;
      territoryNotice.textContent=territoryChanged?(lang==='pt'?`Não há série disponível para ${wantedTerritory} nesta combinação de eleição, turno e cargo. O território exibido mudou para ${selects.uf.value}.`:`No series is available for ${wantedTerritory} under this election, round and office. The displayed territory changed to ${selects.uf.value}.`):'';
      if(matches.length!==1){status.textContent=t.error;status.classList.add('error');console.error(new Error('Missing or duplicate series for selected income reference'));return;}
      selected=matches[0];
      return load();
    }
    function draw() {
      const checked=activeIds;
      const traces=data.candidates.filter(c=>checked.has(c.id)).map(candidate=>{
        const rows=data.points.filter(p=>p.candidate_id===candidate.id).sort((a,b)=>a.centil-b.centil);
        return {type:'scatter',mode:'lines',name:`${candidate.name}${candidate.party?` (${candidate.party})`:''}`,x:rows.map(p=>p.centil),y:rows.map(p=>p.votacao),customdata:rows.map(p=>[fmt(p.renda_sm,2),fmt(p.n_setores),p.n_setores_total==null?'':` / ${fmt(p.n_setores_total)}`]),line:{width:3,color:candidate.color},connectgaps:false,hovertemplate:`<b>%{fullData.name}</b><br>${t.centil} %{x}<br><b>%{y:.2f}%</b> ${t.votes}<br>${t.income}: %{customdata[0]} SM (${data.metadata.income_year||2022})<br>${t.tracts}: %{customdata[1]}%{customdata[2]}<extra></extra>`};
      });
      status.textContent=traces.length?'':t.none;
      const incomeScope=data.metadata.percentile_scope;
      const xTitle=incomeScope===selected.uf&&selected.uf!=='BR'?(lang==='pt'?`Centis de renda dentro de ${selected.uf} (mais pobres → mais ricos)`:`Income percentiles within ${selected.uf} (poorest → richest)`):(lang==='pt'?'Centis de renda do Brasil (mais pobres → mais ricos)':'Brazil income percentiles (poorest → richest)');
      Plotly.react(chart,traces,{autosize:true,height:window.innerWidth<600?410:510,margin:{l:58,r:20,t:30,b:85},paper_bgcolor:'#fff',plot_bgcolor:'#fff',font:{family:'system-ui, sans-serif',color:'#425361',size:12},xaxis:{title:{text:xTitle,standoff:22},range:[1,100],tickvals:[1,20,40,60,80,100],gridcolor:'#edf0f2',zeroline:false},yaxis:{title:{text:t.y,standoff:12},range:[0,100],ticksuffix:'%',gridcolor:'#edf0f2',zeroline:false},legend:{orientation:'h',y:1.13,x:0},hovermode:'closest',dragmode:false},{responsive:true,displaylogo:false,scrollZoom:false,modeBarButtonsToRemove:['select2d','lasso2d','pan2d','zoomIn2d','zoomOut2d','autoScale2d'],toImageButtonOptions:{filename:`votacao-renda-${selected.id}`,format:'png',scale:2}});
    }
    async function load() {
        const current=++request;status.textContent=t.loading;status.classList.remove('error');
      try {
        const preserveSelection=loadedContestKey===contestKey(selected),previousSelection=new Set(activeIds);
        const loaded=await read(seriesUrl(selected));if(current!==request)return;validateIncomeScope(selected,loaded.metadata);data=loaded;
        // Deputy series carry no party field: label (and search) by the manifest acronym of the ballot-number prefix.
        data.candidates.forEach(c=>{if(!c.party&&/^\d{2,5}$/.test(String(c.id))){const acronym=manifest.parties?.[String(selected.year)]?.[String(c.id).slice(0,2)];if(acronym)c.party=acronym;}});
        const scores=new Map(data.candidates.map(c=>[c.id,0]));
        data.points.forEach(point=>scores.set(point.candidate_id,(scores.get(point.candidate_id)||0)+(point.votacao||0)));
        rankedCandidates=embeddedPanel?[...data.candidates]:[...data.candidates].sort((a,b)=>scores.get(b.id)-scores.get(a.id));
        const retained=[...previousSelection].filter(id=>scores.has(id));
        activeIds=new Set(preserveSelection&&(retained.length||previousSelection.size===0)?retained:(embeddedPanel?[]:(data.metadata.default_candidates||rankedCandidates.slice(0,6).map(c=>c.id))).filter(id=>scores.has(id)));
        loadedContestKey=contestKey(selected);
        if(!preserveSelection)search.value='';topButton.disabled=false;clearButton.disabled=false;allButton.disabled=false;search.disabled=false;renderCandidates();
        coverage.replaceChildren();
        const meta=data.metadata,cov=meta.coverage;
        auditPanel.replaceChildren();auditPanel.className='audit';
        const audit=meta.vote_audit;
        const reconciled=audit?.status==='reconciled'&&Number(audit.difference)===0&&Number(audit.official_valid_votes)===Number(audit.computed_valid_votes);
        const inconsistent=audit&&(audit.status==='unreconciled'||audit.status==='failed'||audit.status==='reconciled'&&!reconciled);
        if(reconciled){
          auditPanel.classList.add('passed');
          text('strong',lang==='pt'?'Votos reconciliados com os totais oficiais':'Votes reconciled with official totals',auditPanel);
          text('p',lang==='pt'?`${fmt(audit.computed_valid_votes)} votos válidos computados; ${fmt(audit.official_valid_votes)} nos totais oficiais; diferença: ${fmt(audit.difference)}.`:`${fmt(audit.computed_valid_votes)} valid votes computed; ${fmt(audit.official_valid_votes)} in official totals; difference: ${fmt(audit.difference)}.`,auditPanel);
          if(audit.checked_units!=null)text('p',lang==='pt'?`${fmt(audit.checked_units)} unidades eleitorais conferidas.`:`${fmt(audit.checked_units)} electoral units checked.`,auditPanel);
        }else{
          if(inconsistent)auditPanel.classList.add('failed');
          text('strong',inconsistent?(lang==='pt'?'Divergência na reconciliação dos votos':'Vote reconciliation discrepancy'):(lang==='pt'?'Sem reconciliação oficial publicada para esta série':'No published official-total reconciliation for this series'),auditPanel);
        }
        text('p',lang==='pt'?'A conferência compara votos válidos aos agregados do TSE por disputa. Ela não certifica a associação espacial dos locais de votação aos setores censitários.':'This check compares valid votes with TSE aggregates for the contest. It does not certify the spatial assignment of polling places to census tracts.',auditPanel);
        if(audit?.source?.url)link(auditPanel,audit.source.label||'TSE',audit.source.url);
        if(audit?.provisional===true){
          const warning=text('p',lang==='pt'?'Dados provisórios: a igualdade dos totais não significa totalização jurídica final. Os resultados podem mudar conforme o TSE atualiza a apuração e a situação dos votos.':'Provisional data: matching totals do not establish final legal totalization. Results may change as the TSE updates the count and vote status.',auditPanel);
          warning.className='provisional-warning';
          const snapshots=Array.isArray(audit.snapshots)?audit.snapshots:Object.values(audit.snapshots||{});
          snapshots.forEach(snapshot=>{
            const generated=[snapshot.generation_date,snapshot.generation_time].filter(Boolean).join(' ');
            const info=text('p',`${lang==='pt'?'Snapshot oficial':'Official snapshot'}${generated?`: ${generated}`:''}${snapshot.progress?` · ${lang==='pt'?'andamento':'progress'}: ${snapshot.progress}`:''}${snapshot.final_totalization?` · tf: ${snapshot.final_totalization}`:''}.`,auditPanel);
            if(snapshot.url){info.append(document.createTextNode(' '));link(info,lang==='pt'?'Fonte do snapshot':'Snapshot source',snapshot.url);}
          });
        }
        text('p',`${t.coverage}: ${fmt(cov.included_tracts)} ${t.of} ${fmt(cov.total_tracts)} ${t.included} (${fmt(100*cov.included_tracts/cov.total_tracts,2)}%).`,coverage);
        const scope=meta.percentile_scope===selected.uf&&selected.uf!=='BR'?(lang==='pt'?`Referência estadual: os setores de ${selected.uf} são ordenados pela renda do Censo ${data.metadata.income_year||2022} e divididos em 100 grupos antes das exclusões eleitorais. O primeiro centil reúne o 1% de setores mais pobres desta UF, não do Brasil. A renda média representa o grupo completo; a contagem de setores mostra a cobertura eleitoral.`:`State reference: ${selected.uf} tracts are ranked by Census ${data.metadata.income_year||2022} income and divided into 100 groups before electoral exclusions. The first percentile contains this state's poorest 1% of tracts, rather than Brazil's. Mean income describes the complete group; tract counts describe electoral coverage.`):(lang==='pt'?`Referência nacional: centis do Censo ${data.metadata.income_year||2022} definidos no Brasil inteiro antes de excluir setores sem estimativa de votação. Filtros por UF preservam esses grupos nacionais; algumas faixas podem ter poucos setores na UF. A renda média representa o grupo completo no território selecionado; a contagem de setores mostra a cobertura eleitoral.`:`National reference: Census ${data.metadata.income_year||2022} percentiles defined across Brazil before excluding tracts without vote estimates. State filters preserve these national groups; some groups may have few tracts in a state. Mean income describes the complete group within the selected territory; tract counts describe electoral coverage.`);
        text('p',scope,coverage);
        text('p',`${t.method}: ${localized(meta.method)}`,coverage);
        text('p',`${t.incomeDef}: ${localized(meta.income_definition)}`,coverage);
        if(meta.spatial?.within_year_register_reused)text('p',lang==='pt'?`Coordenadas do cadastro do primeiro turno do mesmo ano (${meta.spatial.polling_reference_date}). Mudanças de endereço entre turnos podem não ser detectadas; locais sem chave correspondente ficam sem estimativa.`:`Coordinates use the same year's first-round register (${meta.spatial.polling_reference_date}). Address changes between rounds may be undetected; unmatched place keys receive no estimate.`,coverage);
        if(meta.senate_note)text('p',localized(meta.senate_note),coverage);
        if(meta.warning)text('p',localized(meta.warning),coverage);
        const sources=text('p',`${t.sources}: `,coverage);
        (meta.sources||[]).forEach((source,index)=>{if(index)sources.append(document.createTextNode(' · '));link(sources,source.label||source.name||source.url,source.url);});
        const downloads=root.querySelector('.download-links');downloads.replaceChildren();link(downloads,new URL(seriesUrl(selected)).pathname.endsWith('.json.gz')?(lang==='pt'?'Baixar esta série (JSON.gz compactado)':'Download this series (compressed JSON.gz)'):t.download,seriesUrl(selected));link(downloads,t.manifest,new URL('manifest.json',base));
        draw();
        if(typeof CustomEvent==='function')document.dispatchEvent?.(new CustomEvent('explorer:series',{detail:{series:data,entry:selected,manifest}}));
      }catch(error){if(current!==request)return;status.textContent=error.userMessage||t.error;status.classList.add('error');if(error.userMessage){if(typeof Plotly.purge==='function')Plotly.purge(chart);coverage.replaceChildren();auditPanel.replaceChildren();candidateBox.replaceChildren();topButton.disabled=true;clearButton.disabled=true;allButton.disabled=true;search.disabled=true;}console.error(error);}
    }
    try {
      manifest=await read(new URL('manifest.json',base),{cache:'no-cache'});
      if(!manifest.series?.some(s=>s.status==='available'))throw new Error('No available series');
      const available=manifest.series.filter(s=>s.status==='available');
      const electionCount=new Set(available.map(s=>s.year)).size;
      availability.textContent=lang==='pt'?`${available.length} séries disponíveis · ${electionCount} ${electionCount===1?'eleição':'eleições'} · ${new Set(available.map(s=>s.office)).size} cargos. Os seletores mostram somente dados disponíveis.`:`${available.length} available series · ${electionCount} ${electionCount===1?'election':'elections'} · ${new Set(available.map(s=>s.office)).size} offices. Selectors show available data only.`;
      if(typeof Plotly==='undefined'){status.textContent=t.noPlotly;return;}
      keys.forEach((key,index)=>selects[key].addEventListener('change',()=>resolve(index)));
      await resolve();
    }catch(error){status.textContent=t.error;status.classList.add('error');console.error(error);}
  }
  document.querySelectorAll('.election-explorer').forEach(start);
})();
