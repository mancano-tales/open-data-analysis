/* Income concentration index per candidate, below the explorer. Reads the series
   app.js already loaded (event "explorer:series") and never redraws the main chart;
   a row click asks app.js to add that candidate ("explorer:select").
   Pure helpers are exported for Node tests. See DATA-CONTRACT.md, "Concentration". */
(function (global) {
  'use strict';

  // Standard concentration index for grouped data (Kakwani et al., 1997): CI = (2 / mu) * sum(f_t * y_t * R_t) - 1, where groups t follow
  // income order, f_t is the share of observed tracts in group t, y_t the mean
  // vote share of those tracts and R_t the fractional rank at the group midpoint.
  // -1: all of the candidate's share in the poorest tracts; 0: equal across
  // income; +1: all in the richest. Groups without observed tracts are skipped.
  function concentrationIndex(points) {
    const rows = points.filter(p => p.votacao != null && Number.isFinite(p.votacao) && p.n_setores > 0)
      .sort((a, b) => a.centil - b.centil);
    const total = rows.reduce((s, p) => s + p.n_setores, 0);
    if (!total) return null;
    let cumulative = 0, mu = 0, weighted = 0;
    for (const p of rows) {
      const f = p.n_setores / total, rank = cumulative + f / 2;
      mu += f * p.votacao; weighted += f * p.votacao * rank; cumulative += f;
    }
    return mu > 0 ? 2 * weighted / mu - 1 : null;
  }

  // Wagstaff (2005) normalization for a variable bounded by 0-100%: CI / (1 - mu),
  // mu as a proportion. Lets high-share candidates reach +/-1 like small ones.
  function wagstaffIndex(points) {
    const ci = concentrationIndex(points);
    if (ci == null) return null;
    const rows = points.filter(p => p.votacao != null && Number.isFinite(p.votacao) && p.n_setores > 0);
    const total = rows.reduce((s, p) => s + p.n_setores, 0);
    const mu = rows.reduce((s, p) => s + p.n_setores * p.votacao, 0) / total / 100;
    return mu < 1 ? ci / (1 - mu) : null;
  }

  function partyNumber(candidate) {
    if (candidate.party_number != null) return String(candidate.party_number);
    const id = String(candidate.id);
    return /^\d{2,5}$/.test(id) ? id.slice(0, 2) : null;
  }

  function partyOf(candidate, manifest, year) {
    if (candidate.party) return candidate.party;
    const number = partyNumber(candidate);
    return number ? manifest?.parties?.[String(year)]?.[number] || number : '';
  }

  // One row per candidate with its CI and mean vote share across observed tracts.
  function candidateRows(series, manifest, year) {
    // Legend rows belong to party curves, not the candidate table or its CSV.
    const candidates = series.candidates.filter(c => c.vote_type !== 'party');
    const byCandidate = new Map(candidates.map(c => [c.id, []]));
    for (const p of series.points) byCandidate.get(p.candidate_id)?.push(p);
    return candidates.map(c => {
      const points = byCandidate.get(c.id) || [];
      const observed = points.filter(p => p.votacao != null && p.n_setores > 0);
      const tracts = observed.reduce((s, p) => s + p.n_setores, 0);
      const mean = tracts ? observed.reduce((s, p) => s + p.votacao * p.n_setores, 0) / tracts : null;
      return {id: c.id, name: c.name, party: partyOf(c, manifest, year), ci: wagstaffIndex(points), ci_standard: concentrationIndex(points), mean_share: mean};
    });
  }

  const normalized = value => String(value).normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
  // minShare: minimum mean vote share (%). Tiny candidates give extreme, unstable indices.
  function filterRows(rows, {query = '', party = '', minShare = 0} = {}) {
    const q = normalized(query.trim());
    return rows.filter(r => (!party || r.party === party) && (!minShare || (r.mean_share ?? 0) >= minShare) &&
      (!q || normalized(`${r.id} ${r.name} ${r.party}`).includes(q)));
  }

  function sortRows(rows, key = 'ci', direction = 'desc') {
    const sign = direction === 'asc' ? 1 : -1;
    return [...rows].sort((a, b) => {
      const x = a[key], y = b[key];
      if (x == null && y == null) return 0;
      if (x == null) return 1;
      if (y == null) return -1;
      return typeof x === 'string' ? sign * x.localeCompare(y) : sign * (x - y);
    });
  }

  function toCsv(rows) {
    const cell = v => v == null ? '' : /[",\n]/.test(String(v)) ? `"${String(v).replace(/"/g, '""')}"` : String(v);
    return ['candidato_id,nome,partido,indice_concentracao_wagstaff,indice_concentracao_padrao,votacao_media', ...rows.map(r => [r.id, r.name, r.party, r.ci, r.ci_standard, r.mean_share].map(cell).join(','))].join('\n');
  }

  const api = {concentrationIndex, wagstaffIndex, partyNumber, partyOf, candidateRows, filterRows, sortRows, toCsv};
  if (typeof module === 'object' && module.exports) module.exports = api;
  if (typeof document === 'undefined') return;
  global.ElectionConcentration = api;

  const copy = {
    pt: {title: 'Concentração da votação por renda', search: 'Buscar candidato por nome, número ou partido', party: 'Partido', allParties: 'Todos os partidos',
      name: 'Candidato', partyCol: 'Partido', ci: 'Índice de concentração', mean: 'Votação média (%)', add: 'Mostrar no gráfico',
      count: (n, total) => `${n} de ${total} candidatos`, minShare: 'Votação média mínima', all: 'Todos', download: 'Baixar a tabela (CSV)', empty: 'Nenhum candidato encontrado.', loading: 'Aguardando a série selecionada…'},
    en: {title: 'Income concentration of the vote', search: 'Search candidates by name, number or party', party: 'Party', allParties: 'All parties',
      name: 'Candidate', partyCol: 'Party', ci: 'Concentration index', mean: 'Mean vote share (%)', add: 'Show on chart',
      count: (n, total) => `${n} of ${total} candidates`, minShare: 'Minimum mean vote share', all: 'All', download: 'Download the table (CSV)', empty: 'No candidates found.', loading: 'Waiting for the selected series…'}
  };

  function start(root) {
    const lang = root.dataset.lang === 'pt' ? 'pt' : 'en', t = copy[lang], locale = lang === 'pt' ? 'pt-BR' : 'en-US';
    const fmt = (v, d) => v == null ? '—' : v.toLocaleString(locale, {minimumFractionDigits: d, maximumFractionDigits: d});
    const controls = root.querySelector('.concentration-controls'), table = root.querySelector('.concentration-table');
    const status = root.querySelector('.concentration-status'), downloads = root.querySelector('.concentration-downloads');
    const searchLabel = document.createElement('label'); searchLabel.textContent = t.search;
    const search = document.createElement('input'); search.type = 'search'; search.setAttribute('aria-label', t.search); searchLabel.append(search);
    const partyLabel = document.createElement('label'); partyLabel.textContent = t.party;
    const partySelect = document.createElement('select'); partySelect.setAttribute('aria-label', t.party); partyLabel.append(partySelect);
    const minLabel = document.createElement('label'); minLabel.textContent = t.minShare;
    const minSelect = document.createElement('select'); minSelect.setAttribute('aria-label', t.minShare); minLabel.append(minSelect);
    for (const [value, label] of [['0', t.all], ['0.1', '0,1%'], ['0.5', '0,5%'], ['1', '1%']]) {
      const option = document.createElement('option'); option.value = value; option.textContent = lang === 'pt' ? label : label.replace(',', '.'); minSelect.append(option);
    }
    minSelect.value = '0.1';
    controls.append(searchLabel, partyLabel, minLabel);
    status.textContent = t.loading;
    let rows = [], sortKey = 'ci', direction = 'desc', entry = null;
    function render() {
      const shown = sortRows(filterRows(rows, {query: search.value, party: partySelect.value, minShare: Number(minSelect.value)}), sortKey, direction);
      status.textContent = t.count(shown.length, rows.length);
      const head = document.createElement('tr');
      for (const [key, label] of [['name', t.name], ['party', t.partyCol], ['ci', t.ci], ['mean_share', t.mean]]) {
        const th = document.createElement('th'); th.scope = 'col';
        const button = document.createElement('button'); button.type = 'button'; button.textContent = label;
        th.setAttribute('aria-sort', sortKey === key ? (direction === 'asc' ? 'ascending' : 'descending') : 'none');
        button.addEventListener('click', () => { direction = sortKey === key && direction === 'desc' ? 'asc' : 'desc'; sortKey = key; render(); });
        th.append(button); head.append(th);
      }
      const body = document.createElement('tbody');
      for (const r of shown) {
        const tr = document.createElement('tr');
        const nameCell = document.createElement('td');
        const add = document.createElement('button'); add.type = 'button'; add.className = 'concentration-add';
        add.textContent = `${/^\d+$/.test(r.id) ? `${r.id} · ` : ''}${r.name}`; add.title = t.add;
        add.addEventListener('click', () => document.dispatchEvent(new CustomEvent('explorer:select', {detail: {id: r.id}})));
        nameCell.append(add); tr.append(nameCell);
        for (const value of [r.party, fmt(r.ci, 3), fmt(r.mean_share, 2)]) { const td = document.createElement('td'); td.textContent = value; tr.append(td); }
        body.append(tr);
      }
      const thead = document.createElement('thead'); thead.append(head);
      table.replaceChildren(thead, body);
      if (!shown.length) status.textContent = t.empty;
      downloads.replaceChildren();
      const link = document.createElement('a'); link.textContent = t.download;
      link.href = URL.createObjectURL(new Blob([toCsv(shown)], {type: 'text/csv'}));
      link.download = `concentracao-${entry?.id || 'serie'}.csv`; downloads.append(link);
    }
    search.addEventListener('input', render);
    partySelect.addEventListener('change', render);
    minSelect.addEventListener('change', render);
    document.addEventListener('explorer:series', event => {
      const {series, entry: selected, manifest} = event.detail;
      entry = selected;
      rows = candidateRows(series, manifest, selected.year);
      const wanted = partySelect.value;
      const parties = [...new Set(rows.map(r => r.party).filter(Boolean))].sort((a, b) => a.localeCompare(b));
      partySelect.replaceChildren();
      for (const [value, label] of [['', t.allParties], ...parties.map(p => [p, p])]) {
        const option = document.createElement('option'); option.value = value; option.textContent = label; partySelect.append(option);
      }
      partySelect.value = parties.includes(wanted) ? wanted : '';
      render();
    });
  }
  document.querySelectorAll('.election-concentration').forEach(start);
})(typeof globalThis !== 'undefined' ? globalThis : this);
