/* Aggregate-only static explorer. See DATA-CONTRACT.md for the input format. */
(() => {
  'use strict';
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
    const keys=['year','round','office','uf'], selects={};
    const text=(tag,value,parent)=>{const el=document.createElement(tag);el.textContent=value;parent.append(el);return el;};
    keys.forEach(key=>{const label=text('label',t[key],controls);const select=document.createElement('select');select.setAttribute('aria-label',t[key]);label.append(select);selects[key]=select;});
    status.textContent=t.loading;
    let manifest, selected, data, request=0, activeIds=new Set(), rankedCandidates=[];
    const searchLabel=text('label',lang==='pt'?'Buscar candidato por nome, número ou partido':'Search candidates by name, number or party',root.querySelector('fieldset'));
    const search=document.createElement('input');search.type='search';search.placeholder=lang==='pt'?'Digite um nome ou número…':'Enter a name or number…';search.setAttribute('aria-label',searchLabel.textContent);searchLabel.append(search);searchLabel.className='candidate-search';
    const candidateTools=document.createElement('div');candidateTools.className='candidate-tools';
    const topButton=text('button',lang==='pt'?'Mostrar 6 principais':'Show leading 6',candidateTools);topButton.type='button';
    const clearButton=text('button',lang==='pt'?'Limpar seleção':'Clear selection',candidateTools);clearButton.type='button';
    topButton.disabled=true;clearButton.disabled=true;search.disabled=true;
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
    topButton.addEventListener('click',()=>{activeIds=new Set(rankedCandidates.slice(0,6).map(c=>c.id));renderCandidates();draw();});
    clearButton.addEventListener('click',()=>{activeIds.clear();renderCandidates();draw();});
    const seriesUrl=entry=>new URL(entry.url,base);
    function link(parent,label,url) { const a=text('a',label,parent);a.href=url;return a; }
    function localized(value) { return typeof value==='object' ? (value[lang]||value.pt||value.en||'') : value; }
    async function read(url) {const response=await fetch(url);if(!response.ok)throw new Error(`HTTP ${response.status}`);return response.json();}
    function resolve(changedIndex=-1) {
      let subset=manifest.series.filter(s=>s.status==='available');
      keys.forEach((key,index)=>{
        const wanted=selects[key].value;
        const values=[...new Set(subset.map(s=>String(s[key])))];
        if(key==='year')values.sort((a,b)=>Number(b)-Number(a));
        if(key==='office'||key==='round')values.sort((a,b)=>Number(a)-Number(b));
        if(key==='uf')values.sort((a,b)=>a==='BR'?-1:b==='BR'?1:a.localeCompare(b));
        selects[key].replaceChildren();
        values.forEach(value=>{const officeLabel=manifest.offices?.[value]||subset.find(s=>String(s.office)===value)?.office_label||value;const option=text('option',key==='round'?t.roundLabel(value):key==='uf'&&value==='BR'?t.national:key==='office'?localized(officeLabel):value,selects[key]);option.value=value;});
        if(values.includes(wanted))selects[key].value=wanted;
        subset=subset.filter(s=>String(s[key])===selects[key].value);
      });
      selected=subset[0];
      return load();
    }
    function draw() {
      const checked=activeIds;
      const traces=data.candidates.filter(c=>checked.has(c.id)).map(candidate=>{
        const rows=data.points.filter(p=>p.candidate_id===candidate.id).sort((a,b)=>a.centil-b.centil);
        return {type:'scatter',mode:'lines',name:`${candidate.name}${candidate.party?` (${candidate.party})`:''}`,x:rows.map(p=>p.centil),y:rows.map(p=>p.votacao),customdata:rows.map(p=>[fmt(p.renda_sm,2),fmt(p.n_setores),p.n_setores_total==null?'':` / ${fmt(p.n_setores_total)}`]),line:{width:3,color:candidate.color},connectgaps:false,hovertemplate:`<b>%{fullData.name}</b><br>${t.centil} %{x}<br><b>%{y:.2f}%</b> ${t.votes}<br>${t.income}: %{customdata[0]} SM (2022)<br>${t.tracts}: %{customdata[1]}%{customdata[2]}<extra></extra>`};
      });
      status.textContent=traces.length?'':t.none;
      Plotly.react(chart,traces,{autosize:true,height:window.innerWidth<600?410:510,margin:{l:58,r:20,t:30,b:85},paper_bgcolor:'#fff',plot_bgcolor:'#fff',font:{family:'system-ui, sans-serif',color:'#425361',size:12},xaxis:{title:{text:t.x,standoff:22},range:[1,100],tickvals:[1,20,40,60,80,100],gridcolor:'#edf0f2',zeroline:false},yaxis:{title:{text:t.y,standoff:12},range:[0,100],ticksuffix:'%',gridcolor:'#edf0f2',zeroline:false},legend:{orientation:'h',y:1.13,x:0},hovermode:'closest',dragmode:false},{responsive:true,displaylogo:false,scrollZoom:false,modeBarButtonsToRemove:['select2d','lasso2d','pan2d','zoomIn2d','zoomOut2d','autoScale2d'],toImageButtonOptions:{filename:`votacao-renda-${selected.id}`,format:'png',scale:2}});
    }
    async function load() {
        const current=++request;status.textContent=t.loading;status.classList.remove('error');
      try {
        const loaded=await read(seriesUrl(selected));if(current!==request)return;data=loaded;
        const scores=new Map(data.candidates.map(c=>[c.id,0]));
        data.points.forEach(point=>scores.set(point.candidate_id,(scores.get(point.candidate_id)||0)+(point.votacao||0)));
        rankedCandidates=[...data.candidates].sort((a,b)=>scores.get(b.id)-scores.get(a.id));
        activeIds=new Set((data.metadata.default_candidates||rankedCandidates.slice(0,6).map(c=>c.id)).filter(id=>scores.has(id)));
        search.value='';topButton.disabled=false;clearButton.disabled=false;search.disabled=false;renderCandidates();
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
        text('p',`${t.coverage}: ${fmt(cov.included_tracts)} ${t.of} ${fmt(cov.total_tracts)} ${t.included} (${fmt(100*cov.included_tracts/cov.total_tracts,2)}%).`,coverage);
        const scope=meta.percentile_scope==='BR' ? (lang==='pt'?'Centis nacionais fixos do Censo 2022, definidos antes de excluir setores sem estimativa de votação. Filtros por UF preservam esses grupos nacionais. A renda média representa o grupo completo; a contagem de setores mostra a cobertura eleitoral.':'Fixed national Census 2022 percentiles, defined before excluding tracts without vote estimates. State filters preserve these national groups. Mean income describes the complete group; tract counts describe electoral coverage.') : (lang==='pt'?'Snapshot legado: centis recalculados na amostra de setores com renda e votação estimada em 2026. Este eixo difere dos centis nacionais fixos usados na expansão histórica.':'Legacy snapshot: percentiles calculated within the 2026 sample of tracts with income and estimated votes. This axis differs from the fixed national percentiles used for historical expansion.');
        text('p',scope,coverage);
        text('p',`${t.method}: ${localized(meta.method)}`,coverage);
        text('p',`${t.incomeDef}: ${localized(meta.income_definition)}`,coverage);
        if(meta.spatial?.within_year_register_reused)text('p',lang==='pt'?`Coordenadas do cadastro do primeiro turno do mesmo ano (${meta.spatial.polling_reference_date}). Mudanças de endereço entre turnos podem não ser detectadas; locais sem chave correspondente ficam sem estimativa.`:`Coordinates use the same year's first-round register (${meta.spatial.polling_reference_date}). Address changes between rounds may be undetected; unmatched place keys receive no estimate.`,coverage);
        if(meta.senate_note)text('p',localized(meta.senate_note),coverage);
        if(meta.warning)text('p',localized(meta.warning),coverage);
        const sources=text('p',`${t.sources}: `,coverage);
        (meta.sources||[]).forEach((source,index)=>{if(index)sources.append(document.createTextNode(' · '));link(sources,source.label||source.name||source.url,source.url);});
        const downloads=root.querySelector('.download-links');downloads.replaceChildren();link(downloads,t.download,seriesUrl(selected));link(downloads,t.manifest,new URL('manifest.json',base));
        draw();
      }catch(error){if(current!==request)return;status.textContent=t.error;status.classList.add('error');console.error(error);}
    }
    try {
      manifest=await read(new URL('manifest.json',base));
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
