/* Static explorer for campaign suppliers (TSE 2026). Reads data/ built by
   shared-pipeline/fornecedores/010_build_fornecedores_2026.R. */
(() => {
  'use strict';
  const T = {
    pt: {uf:'Território',cargo:'Cargo',all:'Todos',candidate:'Candidato',party:'Partido',search:'Buscar por nome, número ou partido',
      loading:'Carregando…',error:'Não foi possível carregar os dados. Recarregue a página ou baixe os arquivos abaixo.',
      total:'Despesa contratada',meta:'Meta (direto)',inter:'Intermediários de pagamento',boost:'Impulsionamento',records:'lançamentos',
      cands:'candidatos',supplier:'Fornecedor',share:'do total',noCand:'Nenhum candidato encontrado.',pf:'pessoa física',
      metaParty:'Pagamentos diretos à Meta por partido (R$ milhões)',metaCargo:'Pagamentos diretos à Meta por cargo (R$ milhões)',
      metaTop:'Candidatos que mais contrataram a Meta diretamente',rank:'#',value:'Meta (R$)',pct:'% da despesa',
      partyChart:'Principais fornecedores',x:'R$ milhões',national:'Brasil (presidente)',showing:n=>`${n} candidatos na lista`,
      metaShare:'Parcela da despesa'},
    en: {uf:'Territory',cargo:'Office',all:'All',candidate:'Candidate',party:'Party',search:'Search by name, number or party',
      loading:'Loading…',error:'The data could not load. Reload the page or download the files below.',
      total:'Contracted spending',meta:'Meta (direct)',inter:'Payment intermediaries',boost:'Content boosting',records:'entries',
      cands:'candidates',supplier:'Supplier',share:'of total',noCand:'No candidates found.',pf:'individual',
      metaParty:'Direct payments to Meta by party (R$ millions)',metaCargo:'Direct payments to Meta by office (R$ millions)',
      metaTop:'Candidates with the largest direct Meta contracts',rank:'#',value:'Meta (R$)',pct:'% of spending',
      partyChart:'Leading suppliers',x:'R$ millions',national:'Brazil (president)',showing:n=>`${n} candidates listed`,
      metaShare:'Share of spending'}
  };
  const CARGO_EN = {'Presidente':'President','Governador':'Governor','Senador':'Senator','Deputado Federal':'Federal deputy',
    'Deputado Estadual':'State deputy','Deputado Distrital':'District deputy','Todos':'All'};
  const META = '#1f5fa8', OTHER = '#8a96a3', PF = '#c2a35a', INTER = '#6f8fb8';
  const norm = s => String(s).normalize('NFD').replace(/[̀-ͯ]/g,'').toLowerCase();

  function el(tag, attrs = {}, ...kids) {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) k === 'text' ? e.textContent = v : e.setAttribute(k, v);
    e.append(...kids); return e;
  }
  function select(label, options, parent) {
    const s = el('select', {'aria-label': label});
    options.forEach(([v, l]) => s.append(el('option', {value: v, text: l})));
    parent.append(el('label', {}, label, s)); return s;
  }
  function csv(text) {
    const rows = []; let row = [], cell = '', q = false;
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (q) { if (c === '"' && text[i + 1] === '"') { cell += '"'; i++; } else if (c === '"') q = false; else cell += c; }
      else if (c === '"') q = true;
      else if (c === ',') { row.push(cell); cell = ''; }
      else if (c === '\n') { row.push(cell); rows.push(row); row = []; cell = ''; }
      else if (c !== '\r') cell += c;
    }
    if (cell || row.length) { row.push(cell); rows.push(row); }
    const [head, ...body] = rows;
    return body.map(r => Object.fromEntries(head.map((h, i) => [h, r[i]])));
  }

  async function start(root) {
    const lang = root.dataset.lang === 'en' ? 'en' : 'pt', t = T[lang], loc = lang === 'pt' ? 'pt-BR' : 'en-US';
    const base = new URL(root.dataset.base, document.baseURI);
    const brl = v => 'R$ ' + Number(v).toLocaleString(loc, {maximumFractionDigits: 0});
    const mi = v => 'R$ ' + (v / 1e6).toLocaleString(loc, {minimumFractionDigits: 1, maximumFractionDigits: 1}) + (lang === 'pt' ? ' mi' : ' M');
    const pct = v => (100 * v).toLocaleString(loc, {maximumFractionDigits: 1}) + '%';
    const cargoName = c => lang === 'en' ? (CARGO_EN[c] || c) : c;
    const get = async (path, type = 'json') => { const r = await fetch(new URL(path, base)); if (!r.ok) throw new Error(r.status); return r[type](); };
    const status = el('p', {class: 'status', role: 'status', 'aria-live': 'polite', text: t.loading});
    root.append(status);
    const config = {responsive: true, displaylogo: false, modeBarButtonsToRemove: ['lasso2d', 'select2d']};

    function bars(div, rows, title) {
      // rows: [{label, v, color, hover}] in descending order
      const r = rows.slice().reverse();
      const maxV = Math.max(...rows.map(d => d.v)) / 1e6, longest = Math.max(...rows.map(d => d.label.length));
      Plotly.react(div, [{type: 'bar', orientation: 'h', x: r.map(d => d.v / 1e6), y: r.map(d => d.label),
        marker: {color: r.map(d => d.color)}, hovertext: r.map(d => d.hover), hoverinfo: 'text',
        text: r.map(d => d.text || ''), textposition: 'outside', cliponaxis: false}], {
        title: {text: title, font: {size: 15}, x: 0, xanchor: 'left'},
        height: Math.max(260, 70 + 28 * rows.length), margin: {l: Math.min(320, 16 + 7.4 * longest), r: 20, t: 46, b: 40},
        xaxis: {title: t.x, range: [0, maxV * 1.18], gridcolor: '#e6e9ec'}, yaxis: {automargin: false, tickfont: {size: 11}},
        font: {family: 'inherit', color: '#253441'}, paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)'
      }, config);
    }
    const short = s => s.length > 40 ? s.slice(0, 38) + '…' : s;
    const isMeta = f => f.doc && (f.doc.startsWith('13347016') || /^META PLATFORMS/.test(f.nome));
    const isInter = f => f.doc && (f.doc.startsWith('25021356') || /^EBANX/.test(f.nome));
    const supplierRows = (list, total) => list.map(f => ({
      label: short(f.pf ? (lang === 'pt' ? f.nome : 'Individuals (sum, names withheld)') : f.nome), v: f.v,
      color: isMeta(f) ? META : isInter(f) ? INTER : f.pf ? PF : OTHER, text: pct(f.v / total),
      hover: `${f.pf ? (lang === 'pt' ? f.nome : 'Individuals (sum, names withheld)') : f.nome}${f.doc ? '<br>CNPJ ' + f.doc : ''}<br>${brl(f.v)} · ${pct(f.v / total)} ${t.share}<br>${f.n.toLocaleString(loc)} ${t.records}${f.candidatos ? ' · ' + f.candidatos.toLocaleString(loc) + ' ' + t.cands : ''}`
    }));
    function summary(x) {
      return el('dl', {class: 'kpis'},
        ...[[t.total, mi(x.total)], [t.meta, `${mi(x.meta)} (${pct(x.meta / x.total)})`],
            [t.inter, mi(x.intermediario)], [t.boost, mi(x.impulsionamento)]]
          .map(([k, v]) => el('div', {}, el('dt', {text: k}), el('dd', {text: v}))));
    }

    try {
      const view = root.dataset.view;
      if (view === 'meta') {
        const [res, rows] = await Promise.all([get('resumo.json'), get('meta-candidatos.csv', 'text').then(csv)]);
        status.remove();
        const g = res.geral;
        root.append(summary(g));
        const d1 = el('div', {class: 'chart'}), d2 = el('div', {class: 'chart'});
        root.append(d2, d1);
        const parties = res.por_partido.filter(p => p.meta > 0).slice(0, 20);
        bars(d1, parties.map(p => ({label: p.partido, v: p.meta, color: META, text: pct(p.share),
          hover: `${p.partido}<br>${t.meta}: ${brl(p.meta)}<br>${pct(p.share)} ${t.share} (${mi(p.total)})<br>${p.candidatos_meta} / ${p.candidatos} ${t.cands}`})), t.metaParty);
        bars(d2, res.por_cargo.map(c => ({label: cargoName(c.cargo), v: c.meta, color: META, text: pct(c.meta / c.total),
          hover: `${cargoName(c.cargo)}<br>${t.meta}: ${brl(c.meta)}<br>${pct(c.meta / c.total)} ${t.share}<br>${c.candidatos_meta} / ${c.candidatos} ${t.cands}`})), t.metaCargo);
        const filter = el('input', {type: 'search', placeholder: t.search, 'aria-label': t.search});
        const table = el('table', {class: 'meta-table'});
        const draw = () => {
          const q = norm(filter.value.trim());
          const hits = rows.filter(r => !q || norm(`${r.nome} ${r.numero} ${r.partido} ${r.uf}`).includes(q)).slice(0, 50);
          table.replaceChildren(el('thead', {}, el('tr', {}, ...[t.rank, t.candidate, t.party, t.uf, t.cargo, t.value, t.pct].map(h => el('th', {text: h})))),
            el('tbody', {}, ...hits.map(r => el('tr', {}, el('td', {text: rows.indexOf(r) + 1}), el('td', {text: r.nome}), el('td', {text: r.partido}),
              el('td', {text: r.uf}), el('td', {text: cargoName(r.cargo)}), el('td', {class: 'num', text: brl(r.meta)}), el('td', {class: 'num', text: pct(+r.share)})))));
        };
        filter.addEventListener('input', draw); draw();
        root.append(el('h3', {text: t.metaTop}), el('label', {class: 'search'}, filter), el('div', {class: 'table-wrap'}, table));
      }

      if (view === 'candidato') {
        const res = await get('resumo.json');
        status.remove();
        const controls = el('div', {class: 'controls'}); root.append(controls);
        const uf = select(t.uf, res.ufs.map(u => [u, u === 'BR' ? t.national : u]), controls);
        const cargo = select(t.cargo, [], controls);
        const search = el('input', {type: 'search', placeholder: t.search, 'aria-label': t.search});
        controls.append(el('label', {class: 'wide'}, t.search, search));
        const cand = select(t.candidate, [], controls); cand.parentElement.classList.add('wide');
        const info = el('div', {class: 'info'}), chart = el('div', {class: 'chart'});
        root.append(info, chart);
        const cache = {}; let data = [];
        uf.value = 'SP';
        const fill = () => {
          const q = norm(search.value.trim());
          const list = data.filter(c => (cargo.value === 'Todos' || c.cargo === cargo.value) &&
            (!q || norm(`${c.nome} ${c.numero} ${c.partido}`).includes(q)));
          const prev = cand.value;
          cand.replaceChildren(...list.map(c => el('option', {value: c.id, text: `${c.numero} · ${c.nome} (${c.partido}) — ${mi(c.total)}`})));
          if (list.some(c => c.id === prev)) cand.value = prev;
          info.dataset.count = t.showing(list.length);
          if (!list.length) { info.replaceChildren(el('p', {text: t.noCand})); Plotly.purge(chart); return; }
          draw();
        };
        const draw = () => {
          const c = data.find(x => x.id === cand.value); if (!c) return;
          info.replaceChildren(el('p', {class: 'who', text: `${c.nome} · ${c.partido} · ${cargoName(c.cargo)} · ${uf.value === 'BR' ? t.national : uf.value} · ${c.n.toLocaleString(loc)} ${t.records} · ${info.dataset.count}`}), summary(c));
          bars(chart, supplierRows(c.fornecedores, c.total), `${t.partyChart}: ${c.nome}`);
        };
        const load = async () => {
          const wanted = uf.value;
          status.textContent = t.loading; root.prepend(status);
          const rows = cache[wanted] ||= (await get(`candidatos/${wanted}.json`)).candidatos;
          if (wanted !== uf.value) return; // a newer territory was selected meanwhile
          data = rows;
          status.remove();
          const cargos = [...new Set(data.map(c => c.cargo))];
          const prev = cargo.value;
          cargo.replaceChildren(...[['Todos', t.all], ...cargos.map(c => [c, cargoName(c)])].map(([v, l]) => el('option', {value: v, text: l})));
          cargo.value = cargos.includes(prev) ? prev : (cargos.includes('Governador') ? 'Governador' : cargos[0]);
          fill();
        };
        uf.addEventListener('change', load); cargo.addEventListener('change', fill);
        search.addEventListener('input', fill); cand.addEventListener('change', draw);
        await load();
      }

      if (view === 'partido') {
        const all = await get('partidos.json');
        status.remove();
        const controls = el('div', {class: 'controls'}); root.append(controls);
        const totals = all.filter(p => p.cargo === 'Todos').sort((a, b) => b.total - a.total);
        const party = select(t.party, totals.map(p => [p.partido, `${p.partido} — ${mi(p.total)}`]), controls);
        const cargo = select(t.cargo, [], controls);
        const info = el('div', {class: 'info'}), chart = el('div', {class: 'chart'});
        root.append(info, chart);
        const cargoOrder = ['Todos', 'Presidente', 'Governador', 'Senador', 'Deputado Federal', 'Deputado Estadual', 'Deputado Distrital'];
        const fill = () => {
          const cs = all.filter(p => p.partido === party.value).map(p => p.cargo).sort((a, b) => cargoOrder.indexOf(a) - cargoOrder.indexOf(b));
          const prev = cargo.value;
          cargo.replaceChildren(...cs.map(c => el('option', {value: c, text: cargoName(c)})));
          cargo.value = cs.includes(prev) ? prev : 'Todos';
          draw();
        };
        const draw = () => {
          const p = all.find(x => x.partido === party.value && x.cargo === cargo.value);
          info.replaceChildren(el('p', {class: 'who', text: `${p.partido} · ${cargoName(p.cargo)} · ${p.candidatos.toLocaleString(loc)} ${t.cands} · ${p.n.toLocaleString(loc)} ${t.records}`}), summary(p));
          bars(chart, supplierRows(p.fornecedores, p.total), `${t.partyChart}: ${p.partido} (${cargoName(p.cargo)})`);
        };
        party.addEventListener('change', fill); cargo.addEventListener('change', draw);
        fill();
      }
    } catch (e) {
      console.error(e);
      status.textContent = typeof Plotly === 'undefined' ? t.error + ' (Plotly)' : t.error;
      status.classList.add('error');
    }
  }
  document.querySelectorAll('.fornecedores-app').forEach(start);
})();
