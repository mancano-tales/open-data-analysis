/* Election difference comparator. Independent from app.js: the main chart and its
   hover stay untouched. See DATA-CONTRACT.md, section "Comparator". Pure helpers
   are exported for Node tests; the DOM part runs only in the browser. */
(function (global) {
  'use strict';

  const censusOf = entry => Number(entry.census_year || 2022);
  const referenceOf = entry => entry.income_reference || 'national';
  const contestLabel = entry => `${entry.year}-${entry.round}`;

  // Party number from the ballot number: the TSE prefixes every candidate number
  // with the two-digit party number; legend votes use the party number itself.
  function partyNumber(candidate) {
    if (candidate.party_number != null) return String(candidate.party_number);
    const id = String(candidate.id);
    if (!/^\d{2,5}$/.test(id)) return null;
    return id.slice(0, 2);
  }

  // Shares are additive within one series: every tract has the same denominator
  // for all candidates, and a mean of sums equals the sum of means.
  function partyCurve(series, number) {
    const ids = new Set(series.candidates.filter(c => partyNumber(c) === number).map(c => c.id));
    if (!ids.size) return null;
    const rows = new Map();
    for (const p of series.points) {
      if (!ids.has(p.candidate_id)) continue;
      const row = rows.get(p.centil) || {centil: p.centil, votacao: null, n_setores: p.n_setores, n_setores_total: p.n_setores_total, renda_sm: p.renda_sm};
      if (p.votacao != null) row.votacao = (row.votacao || 0) + p.votacao;
      rows.set(p.centil, row);
    }
    const curve = [...rows.values()].sort((a, b) => a.centil - b.centil);
    if (curve.length !== 100) throw new Error(`Party ${number} curve must have 100 centiles`);
    return curve;
  }

  // B minus A. A missing side stays null: no interpolation and no zero filling.
  function differenceCurve(a, b) {
    if (!a || !b || a.length !== 100 || b.length !== 100) throw new Error('Both curves need 100 centiles');
    return a.map((left, i) => {
      const right = b[i];
      if (left.centil !== right.centil) throw new Error('Centile mismatch');
      const diff = left.votacao == null || right.votacao == null ? null : right.votacao - left.votacao;
      return {centil: left.centil, a: left.votacao, b: right.votacao, diferenca_pp: diff,
        n_setores_a: left.n_setores, n_setores_b: right.n_setores, n_setores_total: right.n_setores_total};
    });
  }

  // Only series with the same office, territory, income reference and census
  // share the same tract groups; anything else must not be subtracted.
  function compatible(a, b) {
    if (!a || !b) return {ok: false, reason: 'missing'};
    if (a.id === b.id) return {ok: false, reason: 'same'};
    for (const key of ['office', 'uf']) if (String(a[key]) !== String(b[key])) return {ok: false, reason: key};
    if (referenceOf(a) !== referenceOf(b)) return {ok: false, reason: 'income_reference'};
    if (censusOf(a) !== censusOf(b)) return {ok: false, reason: 'census_year'};
    return {ok: true};
  }

  const majoritarianOffices = new Set(['1', '3', '5']);
  function matchesMetadata(series, entry) {
    const metadata = series?.metadata;
    if (!metadata || !entry) return false;
    const expectedScope = referenceOf(entry) === 'national' ? 'BR' : entry.uf;
    if (referenceOf(entry) === 'state' && entry.uf === 'BR') return false;
    if (String(metadata.income_year ?? 2022) !== String(censusOf(entry)) ||
        metadata.percentile_scope !== expectedScope ||
        (metadata.income_reference != null && metadata.income_reference !== referenceOf(entry))) return false;
    return ['office', 'uf', 'year', 'round'].every(key =>
      metadata[key] != null && String(metadata[key]) === String(entry[key]));
  }

  function partyLabel(manifest, entry, number) {
    const sigla = manifest.parties?.[String(entry.year)]?.[number];
    return sigla ? `${number} · ${sigla}` : number;
  }

  function toCsv(rows) {
    const header = ['centil', 'votacao_a', 'votacao_b', 'diferenca_pp', 'n_setores_a', 'n_setores_b', 'n_setores_total'];
    const cell = v => v == null ? '' : String(v);
    return [header.join(','), ...rows.map(r => [r.centil, r.a, r.b, r.diferenca_pp, r.n_setores_a, r.n_setores_b, r.n_setores_total].map(cell).join(','))].join('\n');
  }

  const api = {partyNumber, partyCurve, differenceCurve, compatible, matchesMetadata, partyLabel, toCsv, censusOf, referenceOf, contestLabel};
  if (typeof module === 'object' && module.exports) module.exports = api;
  if (typeof document === 'undefined') return;
  global.ElectionCompare = api;

  const copy = {
    pt: {office: 'Cargo', uf: 'Território', income_reference: 'Referência de renda', census_year: 'Censo', a: 'Eleição A (antes)', b: 'Eleição B (depois)', party: 'Partido',
      loading: 'Carregando a comparação…', error: 'Não foi possível montar a comparação.', y: 'Diferença B − A (p.p.)',
      x: 'Centis dos setores por renda (mais pobres → mais ricos)', missingParty: 'O partido escolhido não tem votos em uma das eleições.',
      incompatible: 'Essas séries não podem ser comparadas: os grupos de setores são diferentes.', download: 'Baixar a diferença (CSV)',
      notice: 'O comparador inclui só cargos majoritários (presidente, governador e senador). Nos cargos proporcionais, as séries publicadas não separam os votos de legenda, e a curva de um partido ficaria incompleta.',
      national: 'Brasil', state: 'Dentro da UF', round: n => `${n}º turno`,
      summary: (party, a, b) => `Diferença na votação do partido ${party} entre ${a} e ${b}, por centil de renda.`},
    en: {office: 'Office', uf: 'Territory', income_reference: 'Income reference', census_year: 'Census', a: 'Election A (before)', b: 'Election B (after)', party: 'Party',
      loading: 'Loading the comparison…', error: 'The comparison could not be built.', y: 'Difference B − A (p.p.)',
      x: 'Tract income percentiles (poorest → richest)', missingParty: 'The selected party has no votes in one of the elections.',
      incompatible: 'These series cannot be compared: their tract groups differ.', download: 'Download the difference (CSV)',
      notice: 'The comparator covers majoritarian offices only (president, governor and senator). For proportional offices, the published series do not separate party-list votes, so a party curve would be incomplete.',
      national: 'Brazil', state: 'Within state', round: n => `Round ${n}`,
      summary: (party, a, b) => `Change in ${party}'s vote share between ${a} and ${b}, by income percentile.`}
  };

  async function start(root) {
    const lang = root.dataset.lang === 'pt' ? 'pt' : 'en', t = copy[lang];
    const base = new URL(root.dataset.base, document.baseURI);
    const controls = root.querySelector('.compare-controls'), chart = root.querySelector('.compare-chart');
    const status = root.querySelector('.compare-status'), downloads = root.querySelector('.compare-downloads');
    const keys = ['office', 'uf', 'income_reference', 'census_year', 'a', 'b', 'party'], selects = {};
    for (const key of keys) {
      const label = document.createElement('label'); label.textContent = t[key];
      const select = document.createElement('select'); select.setAttribute('aria-label', t[key]);
      label.append(select); controls.append(label); selects[key] = select;
    }
    const notice = document.createElement('p');
    notice.className = 'compare-notice'; notice.textContent = t.notice;
    controls.after(notice);
    status.textContent = t.loading;
    const cache = new Map();
    async function read(url, options) {
      // Same decoding rule as app.js: published series are .json.gz.
      const response = await fetch(url, options); if (!response.ok) throw new Error(`HTTP ${response.status}`);
      if (!new URL(url).pathname.endsWith('.json.gz')) return response.json();
      const bytes = new Uint8Array(await response.arrayBuffer());
      if (bytes[0] !== 31 || bytes[1] !== 139) return JSON.parse(new TextDecoder().decode(bytes));
      return new Response(new Blob([bytes]).stream().pipeThrough(new DecompressionStream('gzip'))).json();
    }
    const load = entry => { const url = new URL(entry.url, base).href; if (!cache.has(url)) cache.set(url, read(url)); return cache.get(url); };
    const fill = (key, values, label, preferred) => {
      const select = selects[key], wanted = select.value;
      select.replaceChildren();
      for (const value of values) { const option = document.createElement('option'); option.value = value; option.textContent = label(value); select.append(option); }
      select.value = values.includes(wanted) ? wanted : (preferred != null && values.includes(preferred) ? preferred : values[0] || '');
      select.closest?.('label')?.toggleAttribute?.('hidden', values.length < 2 && key === 'census_year');
    };
    let manifest, request = 0;
    function entriesFor() {
      let subset = manifest.series.filter(s => s.status === 'available' && majoritarianOffices.has(String(s.office)) && !(s.uf === 'BR' && referenceOf(s) === 'state'));
      const offices = [...new Set(subset.map(s => String(s.office)))].sort((x, y) => Number(x) - Number(y));
      fill('office', offices, v => { const l = manifest.offices?.[v] || subset.find(s => String(s.office) === v)?.office_label || v; return typeof l === 'object' ? (l[lang] || l.pt) : l; }, '1');
      subset = subset.filter(s => String(s.office) === selects.office.value);
      const ufs = [...new Set(subset.map(s => s.uf))].sort((x, y) => x === 'BR' ? -1 : y === 'BR' ? 1 : x.localeCompare(y));
      fill('uf', ufs, v => v === 'BR' ? t.national : v, 'BR');
      subset = subset.filter(s => s.uf === selects.uf.value);
      fill('income_reference', [...new Set(subset.map(referenceOf))].sort(), v => t[v] || v, selects.office.value === '1' ? 'national' : 'state');
      subset = subset.filter(s => referenceOf(s) === selects.income_reference.value);
      fill('census_year', [...new Set(subset.map(s => String(censusOf(s))))].sort().reverse(), v => v, '2022');
      subset = subset.filter(s => String(censusOf(s)) === selects.census_year.value);
      const contests = subset.map(s => s.id).sort((x, y) => x.localeCompare(y));
      const label = id => { const s = subset.find(e => e.id === id); return `${s.year} · ${t.round(s.round)}`; };
      fill('a', contests, label, contests[contests.length - 2]);
      fill('b', contests, label, contests[contests.length - 1]);
      return subset;
    }
    async function update(changed) {
      const current = ++request;
      status.textContent = t.loading; status.classList.remove('error');
      try {
        const subset = entriesFor();
        const entryA = subset.find(s => s.id === selects.a.value), entryB = subset.find(s => s.id === selects.b.value);
        const check = compatible(entryA, entryB);
        if (!check.ok) { status.textContent = t.incompatible; Plotly.purge?.(chart); downloads.replaceChildren(); return; }
        const [seriesA, seriesB] = await Promise.all([load(entryA), load(entryB)]);
        if (current !== request) return;
        if (!matchesMetadata(seriesA, entryA) || !matchesMetadata(seriesB, entryB)) {
          status.textContent = t.incompatible; Plotly.purge?.(chart); downloads.replaceChildren(); return;
        }
        const numbersA = new Set(seriesA.candidates.map(partyNumber).filter(Boolean));
        const parties = [...new Set(seriesB.candidates.map(partyNumber).filter(n => n && numbersA.has(n)))].sort((x, y) => Number(x) - Number(y));
        if (changed !== 'party') fill('party', parties, n => partyLabel(manifest, entryB, n), parties.includes(selects.party.value) ? selects.party.value : '13');
        const curveA = partyCurve(seriesA, selects.party.value), curveB = partyCurve(seriesB, selects.party.value);
        if (!curveA || !curveB) { status.textContent = t.missingParty; Plotly.purge?.(chart); downloads.replaceChildren(); return; }
        const rows = differenceCurve(curveA, curveB);
        draw(rows, entryA, entryB, partyLabel(manifest, entryB, selects.party.value));
        status.textContent = t.summary(partyLabel(manifest, entryB, selects.party.value), `${entryA.year} (${t.round(entryA.round)})`, `${entryB.year} (${t.round(entryB.round)})`);
        downloads.replaceChildren();
        const link = document.createElement('a'); link.textContent = t.download;
        link.href = URL.createObjectURL(new Blob([toCsv(rows)], {type: 'text/csv'}));
        link.download = `diferenca-${entryA.id}-${entryB.id}-${selects.party.value}.csv`; downloads.append(link);
      } catch (error) { if (current !== request) return; status.textContent = t.error; status.classList.add('error'); console.error(error); }
    }
    function draw(rows, entryA, entryB, party) {
      const fmt = v => v == null ? '—' : `${v.toLocaleString(lang === 'pt' ? 'pt-BR' : 'en-US', {maximumFractionDigits: 2, minimumFractionDigits: 2})}`;
      const trace = {type: 'bar', name: party, x: rows.map(r => r.centil), y: rows.map(r => r.diferenca_pp),
        marker: {color: rows.map(r => r.diferenca_pp == null ? '#c9d1d8' : r.diferenca_pp >= 0 ? '#2f6f9f' : '#b4553d')},
        customdata: rows.map(r => [fmt(r.a), fmt(r.b), r.n_setores_a ?? '—', r.n_setores_b ?? '—', r.n_setores_total ?? '—']),
        hovertemplate: `<b>%{y:+.2f} p.p.</b><br>${entryA.year}: %{customdata[0]}%<br>${entryB.year}: %{customdata[1]}%<br>` +
          (lang === 'pt' ? 'Setores: %{customdata[2]} / %{customdata[3]} de %{customdata[4]}' : 'Tracts: %{customdata[2]} / %{customdata[3]} of %{customdata[4]}') + '<extra></extra>'};
      Plotly.react(chart, [trace], {autosize: true, height: window.innerWidth < 600 ? 340 : 400, margin: {l: 58, r: 20, t: 20, b: 70},
        paper_bgcolor: '#fff', plot_bgcolor: '#fff', font: {family: 'system-ui, sans-serif', color: '#425361', size: 12},
        xaxis: {title: {text: t.x}, range: [0.5, 100.5], tickvals: [1, 20, 40, 60, 80, 100], gridcolor: '#edf0f2'},
        yaxis: {title: {text: t.y}, ticksuffix: ' p.p.', zeroline: true, zerolinecolor: '#425361', zerolinewidth: 1.5, gridcolor: '#edf0f2'},
        bargap: 0.1, showlegend: false, hovermode: 'x unified', dragmode: false},
        {responsive: true, displaylogo: false, scrollZoom: false, modeBarButtonsToRemove: ['select2d', 'lasso2d', 'pan2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d']});
    }
    try {
      manifest = await read(new URL('manifest.json', base), {cache: 'no-cache'});
      if (typeof Plotly === 'undefined') { status.textContent = t.error; return; }
      for (const key of keys) selects[key].addEventListener('change', () => update(key));
      await update();
    } catch (error) { status.textContent = t.error; status.classList.add('error'); console.error(error); }
  }
  document.querySelectorAll('.election-compare').forEach(start);
})(typeof globalThis !== 'undefined' ? globalThis : this);
