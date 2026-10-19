/* Static explorer for campaign suppliers (TSE 2026). Reads data/ built by
   shared-pipeline/fornecedores/010_build_fornecedores_2026.R. */
(() => {
  'use strict';
  const T = {
    pt: {uf:'Território',cargo:'Cargo',all:'Todos',candidate:'Candidato',party:'Partido',search:'Buscar por nome, número ou partido',
      loading:'Carregando…',error:'Não foi possível carregar os dados. Recarregue a página ou baixe os arquivos abaixo.',
      total:'Despesa contratada',meta:'Facebook (direto)',inter:'Intermediários de pagamento',boost:'Impulsionamento',records:'lançamentos',
      cands:'candidatos',supplier:'Fornecedor',share:'do total',noCand:'Nenhum candidato encontrado.',pf:'pessoa física',
      metaParty:'Contratado diretamente com o Facebook, por partido (20 maiores)',metaCargo:'Contratado diretamente com o Facebook, por cargo',metaSub:'Valor contratado e parcela da despesa total do grupo',supSub:'Valor contratado e parcela da despesa total',pfLegend:'Pessoas físicas (soma)',pfLabel:'Pessoas físicas (soma, sem nomes)',others:'Demais fornecedores',caption:'Fonte: TSE, prestação de contas de candidatos 2026 (despesas contratadas, dados de 18/10/2026).',
      metaTop:'Candidatos que mais contrataram o Facebook diretamente',rank:'#',value:'Facebook (R$)',pct:'% da despesa',totalCol:'Despesa total (R$)',sortHint:'Clique no cabeçalho para ordenar',
      partyChart:'Maiores fornecedores',x:'R$ milhões',national:'Brasil (presidente)',showing:n=>`${n} candidatos na lista`,
      metaShare:'Parcela da despesa'},
    en: {uf:'Territory',cargo:'Office',all:'All',candidate:'Candidate',party:'Party',search:'Search by name, number or party',
      loading:'Loading…',error:'The data could not load. Reload the page or download the files below.',
      total:'Contracted spending',meta:'Facebook (direct)',inter:'Payment intermediaries',boost:'Content boosting',records:'entries',
      cands:'candidates',supplier:'Supplier',share:'of total',noCand:'No candidates found.',pf:'individual',
      metaParty:'Contracted directly with Facebook, by party (top 20)',metaCargo:'Contracted directly with Facebook, by office',metaSub:'Contracted value and share of the group’s total spending',supSub:'Contracted value and share of total spending',pfLegend:'Individuals (sum)',pfLabel:'Individuals (sum, names withheld)',others:'Other suppliers',caption:'Source: TSE, 2026 candidate accounts (contracted expenses, data as of 18 Oct 2026).',
      metaTop:'Candidates with the largest direct Facebook contracts',rank:'#',value:'Facebook (R$)',pct:'% of spending',totalCol:'Total spending (R$)',sortHint:'Click a header to sort',
      partyChart:'Largest suppliers',x:'R$ millions',national:'Brazil (president)',showing:n=>`${n} candidates listed`,
      metaShare:'Share of spending'}
  };
  const CARGO_EN = {'Presidente':'President','Governador':'Governor','Senador':'Senator','Deputado Federal':'Federal deputy',
    'Deputado Estadual':'State deputy','Deputado Distrital':'District deputy','Todos':'All'};
  // Okabe-Ito, as in shared-pipeline/utils/plot_theme.R; gray carries the context.
  const META = '#0072B2', INTER = '#56B4E9', PF = '#E69F00', OTHER = '#C4CAD0', INK = '#333333', MUTED = '#666666', GRID = '#EEEEEE';
  const KEEP_UPPER = new Set(['LTDA','LTDA.','S.A.','S.A','S/A','SA','ME','EPP','EIRELI','SPE','LLC','PT','PL','PSD','MDB','PP','PSB','PDT','PSDB','PSOL','PV','DC','PRTB','PSTU','PCO','UP','PCB','PMB','PRD','II','III','IV','TV','S/S']);
  const LOWER = new Set(['de','da','do','das','dos','e','em','para','por','a','o']);
  const titleCase = s => String(s).toLowerCase().split(/(\s+)/).map((w, i) => {
    if (KEEP_UPPER.has(w.toUpperCase())) return w.toUpperCase();
    if (i > 0 && LOWER.has(w)) return w;
    return w.charAt(0).toUpperCase() + w.slice(1);
  }).join('');
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
    // Millions from R$ 1 mi up; thousands below, so small campaigns don't read as 0,0.
    const mi = v => v >= 1e6 || v === 0
      ? 'R$ ' + (v / 1e6).toLocaleString(loc, {minimumFractionDigits: 1, maximumFractionDigits: 1}) + (lang === 'pt' ? ' mi' : ' M')
      : 'R$ ' + (v / 1e3).toLocaleString(loc, {maximumFractionDigits: v < 1e4 ? 1 : 0}) + (lang === 'pt' ? ' mil' : 'k');
    const pct = v => (100 * v).toLocaleString(loc, {maximumFractionDigits: 1}) + '%';
    const cargoName = c => lang === 'en' ? (CARGO_EN[c] || c) : c;
    const get = async (path, type = 'json') => { const r = await fetch(new URL(path, base)); if (!r.ok) throw new Error(r.status); return r[type](); };
    const status = el('p', {class: 'status', role: 'status', 'aria-live': 'polite', text: t.loading});
    root.append(status);
    const config = {responsive: true, displaylogo: false, modeBarButtonsToRemove: ['lasso2d', 'select2d']};

    // Thesis-style horizontal bars: title and subtitle above in HTML, one hue for
    // the highlighted series and gray for context, values labelled at bar ends.
    function bars(div, rows, title, opts = {}) {
      const head = el('div', {class: 'fig-head'}, el('p', {class: 'fig-title', text: title}));
      if (opts.subtitle) head.append(el('p', {class: 'fig-sub', text: opts.subtitle}));
      if (opts.legend) head.append(el('p', {class: 'fig-legend'}, ...opts.legend.map(([c, l]) =>
        el('span', {}, el('i', {style: `background:${c}`}), l))));
      const plot = el('div', {class: 'fig-plot'});
      div.replaceChildren(head, plot, el('p', {class: 'fig-caption', text: t.caption}));
      const r = rows.slice().reverse();
      const big = Math.max(...rows.map(d => d.v)) >= 1e6, unit = big ? 1e6 : 1e3;
      const maxV = Math.max(...rows.map(d => d.v)) / unit;
      const longest = Math.max(...rows.map(d => d.label.length));
      const narrow = div.clientWidth < 560;
      Plotly.react(plot, [{type: 'bar', orientation: 'h', x: r.map(d => d.v / unit), y: r.map((d, i) => i),
        marker: {color: r.map(d => d.color), line: {width: 0}}, width: 0.62,
        hovertext: r.map(d => d.hover), hoverinfo: 'text', hoverlabel: {bgcolor: '#fff', bordercolor: '#ccc', font: {color: INK}},
        textposition: 'none'}], {
        annotations: r.map((d, i) => ({...d, i})).filter(d => d.text).map(d => ({x: d.v / unit, y: d.i, text: d.text, xanchor: 'left', xshift: 14,
          showarrow: false, font: {color: MUTED, size: 11}})),
        height: Math.max(220, 34 + 26 * rows.length), bargap: 0.3,
        margin: {l: Math.min(narrow ? 180 : 310, 24 + 6.6 * longest), r: 34, t: 6, b: 34},
        xaxis: {range: [0, maxV * (narrow ? 1.5 : 1.3)], gridcolor: GRID, zeroline: true, zerolinecolor: '#CCCCCC',
                tickfont: {color: MUTED, size: 11}, tickprefix: 'R$ ', ticksuffix: big ? (lang === 'pt' ? ' mi' : ' M') : (lang === 'pt' ? ' mil' : 'k'), fixedrange: true},
        yaxis: {tickvals: r.map((d, i) => i), ticktext: r.map(d => d.label), automargin: false, tickfont: {color: INK, size: 12}, ticks: 'outside', ticklen: 8, tickcolor: 'rgba(0,0,0,0)', fixedrange: true},
        separators: lang === 'pt' ? ',.' : '.,',
        font: {family: 'inherit', color: INK}, paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)', showlegend: false
      }, {responsive: true, displaylogo: false, modeBarButtons: [['toImage']], displayModeBar: 'hover',
          toImageButtonOptions: {format: 'png', scale: 2, filename: 'fornecedores-2026'}});
    }
    const short = s => { s = titleCase(s); return s.length > 38 ? s.slice(0, 36) + '…' : s; };
    const isMeta = f => f.doc && (f.doc.startsWith('13347016') || /^META PLATFORMS/.test(f.nome));
    const isInter = f => f.doc && (f.doc.startsWith('25021356') || /^EBANX/.test(f.nome));
    const supplierRows = (list, total) => list.map(f => ({
      label: f.pf ? t.pfLabel : short(f.nome), v: f.v,
      color: isMeta(f) ? META : isInter(f) ? INTER : f.pf ? PF : OTHER, text: `${mi(f.v)} · ${pct(f.v / total)}`,
      hover: `<b>${f.pf ? (lang === 'pt' ? f.nome : 'Individuals (sum, names withheld)') : titleCase(f.nome)}</b>${f.doc ? '<br>CNPJ ' + f.doc : ''}<br>${brl(f.v)} · ${pct(f.v / total)} ${t.share}<br>${f.n.toLocaleString(loc)} ${t.records}${f.candidatos ? ' · ' + f.candidatos.toLocaleString(loc) + ' ' + t.cands : ''}`
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
        bars(d1, parties.map(p => ({label: p.partido, v: p.meta, color: META, text: `${mi(p.meta)} · ${pct(p.share)}`,
          hover: `${p.partido}<br>${t.meta}: ${brl(p.meta)}<br>${pct(p.share)} ${t.share} (${mi(p.total)})<br>${p.candidatos_meta} / ${p.candidatos} ${t.cands}`})), t.metaParty, {subtitle: t.metaSub});
        bars(d2, res.por_cargo.map(c => ({label: cargoName(c.cargo), v: c.meta, color: META, text: `${mi(c.meta)} · ${pct(c.meta / c.total)}`,
          hover: `${cargoName(c.cargo)}<br>${t.meta}: ${brl(c.meta)}<br>${pct(c.meta / c.total)} ${t.share}<br>${c.candidatos_meta} / ${c.candidatos} ${t.cands}`})), t.metaCargo, {subtitle: t.metaSub});
        const filter = el('input', {type: 'search', placeholder: t.search, 'aria-label': t.search});
        const table = el('table', {class: 'meta-table'});
        rows.forEach((r, i) => { r.rank = i + 1; r.meta = +r.meta; r.total = +r.total; r.share = +r.share; });
        const cols = [
          {key: 'rank', label: t.rank, num: true, show: r => r.rank},
          {key: 'nome', label: t.candidate, show: r => r.nome},
          {key: 'partido', label: t.party, show: r => r.partido},
          {key: 'uf', label: t.uf, show: r => r.uf},
          {key: 'cargo', label: t.cargo, show: r => cargoName(r.cargo)},
          {key: 'total', label: t.totalCol, num: true, show: r => brl(r.total)},
          {key: 'meta', label: t.value, num: true, show: r => brl(r.meta)},
          {key: 'share', label: t.pct, num: true, show: r => pct(r.share)}];
        let sortKey = 'meta', sortDir = -1;
        const draw = () => {
          const q = norm(filter.value.trim());
          const col = cols.find(c => c.key === sortKey);
          const cmp = col.num ? (a, b) => a[sortKey] - b[sortKey] : (a, b) => String(col.show(a)).localeCompare(String(col.show(b)), loc);
          const hits = rows.filter(r => !q || norm(`${r.nome} ${r.numero} ${r.partido} ${r.uf}`).includes(q))
            .sort((a, b) => sortDir * cmp(a, b) || a.rank - b.rank).slice(0, 50);
          const head = cols.map(c => {
            const active = c.key === sortKey;
            const button = el('button', {type: 'button', class: 'sort' + (active ? ' active' : '')}, c.label, el('span', {'aria-hidden': 'true', text: active ? (sortDir > 0 ? ' ▲' : ' ▼') : ' ↕'}));
            button.addEventListener('click', () => {
              if (sortKey === c.key) sortDir = -sortDir; else { sortKey = c.key; sortDir = c.num && c.key !== 'rank' ? -1 : 1; }
              draw();
            });
            return el('th', {class: c.num ? 'num' : '', 'aria-sort': active ? (sortDir > 0 ? 'ascending' : 'descending') : 'none'}, button);
          });
          table.replaceChildren(el('thead', {}, el('tr', {}, ...head)),
            el('tbody', {}, ...hits.map(r => el('tr', {}, ...cols.map(c => el('td', {class: c.num ? 'num' : '', text: c.show(r)}))))));
        };
        filter.addEventListener('input', draw); draw();
        root.append(el('h3', {text: t.metaTop}), el('p', {class: 'who', text: t.sortHint}), el('label', {class: 'search'}, filter), el('div', {class: 'table-wrap'}, table));
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
          bars(chart, supplierRows(c.fornecedores, c.total), `${t.partyChart}: ${titleCase(c.nome)}`, {subtitle: t.supSub, legend: [[META, t.meta], [INTER, t.inter], [PF, t.pfLegend], [OTHER, t.others]]});
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
          bars(chart, supplierRows(p.fornecedores.slice(0, 15), p.total), `${t.partyChart}: ${p.partido} (${cargoName(p.cargo)})`, {subtitle: t.supSub, legend: [[META, t.meta], [INTER, t.inter], [PF, t.pfLegend], [OTHER, t.others]]});
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
