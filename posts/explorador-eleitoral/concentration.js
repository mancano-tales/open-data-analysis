/* Income concentration index per candidate, in its own section below the explorer,
   with its own selectors: it loads series from the manifest and never touches the
   main chart.
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
    pt: {search: 'Buscar candidato por nome, número ou partido', party: 'Partido', allParties: 'Todos os partidos',
      year: 'Eleição', round: 'Turno', office: 'Cargo', uf: 'Território', census_year: 'Censo', income_reference: 'Referência de renda',
      national: 'Brasil', state: 'Dentro da UF', roundLabel: n => `${n}º turno`, censusLabel: v => `Censo ${v}`,
      name: 'Candidato', partyCol: 'Partido', ci: 'Índice de concentração (Wagstaff)', mean: 'Votação média (%)',
      count: (n, total) => `${n} de ${total} candidatos`, minShare: 'Votação média mínima', all: 'Todos',
      download: 'Baixar a tabela (CSV)', empty: 'Nenhum candidato encontrado.', loading: 'Carregando a série…',
      error: 'Não foi possível carregar a série.', rejected: 'Série recusada: os metadados não correspondem à seleção.'},
    en: {search: 'Search candidates by name, number or party', party: 'Party', allParties: 'All parties',
      year: 'Election', round: 'Round', office: 'Office', uf: 'Territory', census_year: 'Census', income_reference: 'Income reference',
      national: 'Brazil', state: 'Within state', roundLabel: n => `Round ${n}`, censusLabel: v => `Census ${v}`,
      name: 'Candidate', partyCol: 'Party', ci: 'Concentration index (Wagstaff)', mean: 'Mean vote share (%)',
      count: (n, total) => `${n} of ${total} candidates`, minShare: 'Minimum mean vote share', all: 'All',
      download: 'Download the table (CSV)', empty: 'No candidates found.', loading: 'Loading the series…',
      error: 'The series could not be loaded.', rejected: 'Series rejected: its metadata do not match the selection.'}
  };

  const censusOf = entry => String(entry.census_year || 2022);
  const referenceOf = entry => entry.income_reference || 'national';
  // The table chooses its own series; it never depends on the explorer above.
  function seriesMatches(series, entry) {
    const m = series?.metadata;
    if (!m) return false;
    const scope = referenceOf(entry) === 'state' ? entry.uf : 'BR';
    return String(m.income_year ?? 2022) === censusOf(entry) && m.percentile_scope === scope &&
      (m.income_reference == null || m.income_reference === referenceOf(entry)) &&
      ['office', 'uf', 'year', 'round'].every(k => m[k] == null || String(m[k]) === String(entry[k]));
  }

  function start(root) {
    const lang = root.dataset.lang === 'pt' ? 'pt' : 'en', t = copy[lang], locale = lang === 'pt' ? 'pt-BR' : 'en-US';
    const fmt = (v, d) => v == null ? '—' : v.toLocaleString(locale, {minimumFractionDigits: d, maximumFractionDigits: d});
    const base = new URL(root.dataset.base, document.baseURI);
    const controls = root.querySelector('.concentration-controls'), filters = root.querySelector('.concentration-filters') || controls;
    const table = root.querySelector('.concentration-table');
    const status = root.querySelector('.concentration-status'), downloads = root.querySelector('.concentration-downloads');
    const label = (text, element) => { const l = document.createElement('label'); l.textContent = text; element.setAttribute('aria-label', text); l.append(element); return l; };
    const keys = ['year', 'round', 'office', 'uf', 'census_year', 'income_reference'], selects = {};
    for (const key of keys) { selects[key] = document.createElement('select'); controls.append(label(t[key], selects[key])); }
    const search = document.createElement('input'); search.type = 'search';
    const partySelect = document.createElement('select'), minSelect = document.createElement('select');
    for (const [value, text] of [['0', t.all], ['0.1', '0,1%'], ['0.5', '0,5%'], ['1', '1%']]) {
      const option = document.createElement('option'); option.value = value; option.textContent = lang === 'pt' ? text : text.replace(',', '.'); minSelect.append(option);
    }
    minSelect.value = '0.1';
    filters.append(label(t.search, search), label(t.party, partySelect), label(t.minShare, minSelect));
    status.textContent = t.loading;
    let manifest, rows = [], sortKey = 'ci', direction = 'desc', entry = null, request = 0;
    const cache = new Map();
    async function read(url, options) {
      const response = await fetch(url, options); if (!response.ok) throw new Error(`HTTP ${response.status}`);
      if (!new URL(url).pathname.endsWith('.json.gz')) return response.json();
      const bytes = new Uint8Array(await response.arrayBuffer());
      if (bytes[0] !== 31 || bytes[1] !== 139) return JSON.parse(new TextDecoder().decode(bytes));
      return new Response(new Blob([bytes]).stream().pipeThrough(new DecompressionStream('gzip'))).json();
    }
    const load = e => { const url = new URL(e.url, base).href; if (!cache.has(url)) cache.set(url, read(url)); return cache.get(url); };
    const officeLabel = v => { const l = manifest.offices?.[v] || manifest.series.find(s => String(s.office) === v)?.office_label || v; return typeof l === 'object' ? (l[lang] || l.pt) : l; };
    const valueOf = (s, key) => key === 'census_year' ? censusOf(s) : key === 'income_reference' ? referenceOf(s) : String(s[key]);
    const labelOf = (key, v) => key === 'round' ? t.roundLabel(v) : key === 'office' ? officeLabel(v) : key === 'uf' && v === 'BR' ? t.national :
      key === 'census_year' ? t.censusLabel(v) : key === 'income_reference' ? t[v] || v : v;
    const defaults = {year: '2022', round: '1', office: '6', uf: 'SP', census_year: '2022'};
    function choose() {
      let subset = manifest.series.filter(s => s.status === 'available' && !(s.uf === 'BR' && referenceOf(s) === 'state'));
      for (const key of keys) {
        const values = [...new Set(subset.map(s => valueOf(s, key)))];
        if (key === 'year' || key === 'census_year') values.sort((a, b) => Number(b) - Number(a));
        else if (key === 'round' || key === 'office') values.sort((a, b) => Number(a) - Number(b));
        else if (key === 'uf') values.sort((a, b) => a === 'BR' ? -1 : b === 'BR' ? 1 : a.localeCompare(b));
        const wanted = selects[key].value || (key === 'income_reference' ? (selects.office.value === '1' ? 'national' : 'state') : defaults[key]);
        selects[key].replaceChildren();
        for (const v of values) { const o = document.createElement('option'); o.value = v; o.textContent = labelOf(key, v); selects[key].append(o); }
        selects[key].value = values.includes(wanted) ? wanted : (values[0] || '');
        subset = subset.filter(s => valueOf(s, key) === selects[key].value);
      }
      return subset.length === 1 ? subset[0] : null;
    }
    function render() {
      const shown = sortRows(filterRows(rows, {query: search.value, party: partySelect.value, minShare: Number(minSelect.value)}), sortKey, direction);
      status.textContent = shown.length ? t.count(shown.length, rows.length) : t.empty;
      const head = document.createElement('tr');
      for (const [key, text] of [['name', t.name], ['party', t.partyCol], ['ci', t.ci], ['mean_share', t.mean]]) {
        const th = document.createElement('th'); th.scope = 'col';
        const button = document.createElement('button'); button.type = 'button'; button.textContent = text;
        th.setAttribute('aria-sort', sortKey === key ? (direction === 'asc' ? 'ascending' : 'descending') : 'none');
        button.addEventListener('click', () => { direction = sortKey === key && direction === 'desc' ? 'asc' : 'desc'; sortKey = key; render(); });
        th.append(button); head.append(th);
      }
      const body = document.createElement('tbody');
      for (const r of shown) {
        const tr = document.createElement('tr');
        for (const value of [`${/^\d+$/.test(r.id) ? `${r.id} · ` : ''}${r.name}`, r.party, fmt(r.ci, 3), fmt(r.mean_share, 2)]) {
          const td = document.createElement('td'); td.textContent = value; tr.append(td);
        }
        body.append(tr);
      }
      const thead = document.createElement('thead'); thead.append(head);
      table.replaceChildren(thead, body);
      downloads.replaceChildren();
      const link = document.createElement('a'); link.textContent = t.download;
      link.href = URL.createObjectURL(new Blob([toCsv(shown)], {type: 'text/csv'}));
      link.download = `concentracao-${entry?.id || 'serie'}.csv`; downloads.append(link);
    }
    async function update() {
      const current = ++request;
      entry = choose();
      if (!entry) { status.textContent = t.error; return; }
      status.textContent = t.loading; status.classList?.remove?.('error');
      try {
        const series = await load(entry);
        if (current !== request) return;
        if (!seriesMatches(series, entry)) { rows = []; table.replaceChildren(); downloads.replaceChildren(); status.textContent = t.rejected; return; }
        rows = candidateRows(series, manifest, entry.year);
        const wanted = partySelect.value;
        const parties = [...new Set(rows.map(r => r.party).filter(Boolean))].sort((a, b) => a.localeCompare(b));
        partySelect.replaceChildren();
        for (const [value, text] of [['', t.allParties], ...parties.map(p => [p, p])]) {
          const option = document.createElement('option'); option.value = value; option.textContent = text; partySelect.append(option);
        }
        partySelect.value = parties.includes(wanted) ? wanted : '';
        render();
      } catch (error) { if (current !== request) return; status.textContent = t.error; status.classList?.add?.('error'); console.error(error); }
    }
    search.addEventListener('input', render);
    partySelect.addEventListener('change', render);
    minSelect.addEventListener('change', render);
    for (const key of keys) selects[key].addEventListener('change', update);
    read(new URL('manifest.json', base).href, {cache: 'no-cache'})
      .then(m => { manifest = m; return update(); })
      .catch(error => { status.textContent = t.error; console.error(error); });
  }
  document.querySelectorAll('.election-concentration').forEach(start);
})(typeof globalThis !== 'undefined' ? globalThis : this);
