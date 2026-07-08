(function () {
  'use strict';

  // ════════════════════════════════════════════════════════════════
  // EventBus
  // ════════════════════════════════════════════════════════════════
  class EventBus {
    constructor() { this.listeners = new Map(); this.current = new Map(); }
    on(event, cb, fireImmediately = true) {
      if (!this.listeners.has(event)) this.listeners.set(event, []);
      this.listeners.get(event).push(cb);
      if (fireImmediately && this.current.has(event)) cb(this.current.get(event));
    }
    emit(event, payload = null) {
      this.current.set(event, payload);
      (this.listeners.get(event) || []).forEach(cb => cb(payload));
    }
    get(event) { return this.current.get(event); }
  }

  // ════════════════════════════════════════════════════════════════
  // DashboardState (simplified — stores generic selection + node)
  // ════════════════════════════════════════════════════════════════
  class DashboardState {
    #selection = null;
    #node      = null;
    setSelection(v) { this.#selection = v; }
    getSelection()  { return this.#selection; }
    setNode(v)      { this.#node = v; }
    getNode()       { return this.#node; }
  }

  // ════════════════════════════════════════════════════════════════
  // Data helpers
  // ════════════════════════════════════════════════════════════════
  function _toNumber(v) {
    if (v == null || v === '') return null;
    const n = Number(v);
    return Number.isFinite(n) ? n : null;
  }
  function _fmt(v) { const n = Number(v); return Number.isFinite(n) ? n.toFixed(1) : '-'; }
  function _uniqueBy(items, key) {
    return items.filter((item, i, arr) => arr.findIndex(c => c[key] === item[key]) === i);
  }
  function readEmbeddedJson(scriptId) {
    const el = document.getElementById(scriptId);
    if (!el) throw new Error('Missing embedded JSON: #' + scriptId);
    return JSON.parse(el.textContent);
  }

  // ════════════════════════════════════════════════════════════════
  // DashboardData — N-level filter support via config
  // ════════════════════════════════════════════════════════════════
  class DashboardData {
    constructor(rows, config) {
      this.rows    = Array.isArray(rows) ? rows : [];
      this.config  = config || {};
      this.filters = config.filters || [];
      this.hierarchyCols   = config.hierarchyCols || [];
      this.scoreCol        = config.scoreCol || 'waarde';
      this.comparisonCols  = config.comparisonCols || [];
    }

    // Unique values for filter level i, constrained by parent selections
    getLevelValues(levelIndex, parentValues = {}) {
      if (levelIndex >= this.filters.length) return [];
      const col = this.filters[levelIndex].col;
      const matching = this.rows.filter(r =>
        Object.entries(parentValues).every(([k, v]) => !v || r[k] === v)
      );
      return [...new Set(matching.map(r => r[col]))]
        .filter(Boolean)
        .sort((a, b) => String(a).localeCompare(String(b)));
    }

    // Build full selection object from filter values
    buildSelection(filterValues) {
      // wijkRows = rows matching ALL filter values
      const wijkRows = this.rows.filter(r =>
        Object.entries(filterValues).every(([k, v]) => !v || r[k] === v)
      );
      if (!wijkRows.length) return null;

      // comparisonRows = all rows matching PARENT filter values (for comparison plot)
      const parentValues = Object.fromEntries(
        Object.entries(filterValues).slice(0, -1)
      );
      const comparisonRows = Object.keys(parentValues).length > 0
        ? this.rows.filter(r => Object.entries(parentValues).every(([k, v]) => !v || r[k] === v))
        : this.rows;

      // lookup: key → score
      const lookup = new Map(wijkRows.map(r => [r.key, _toNumber(r[this.scoreCol])]));

      // entityId = deepest filter value (for comparison plot highlight)
      const deepest = this.filters[this.filters.length - 1];
      const entityId = deepest ? filterValues[deepest.col] : null;

      return { filterValues, wijkRows, comparisonRows, lookup, entityId };
    }
  }

  // ════════════════════════════════════════════════════════════════
  // DashboardSelectors — N-level cascading, default selection
  // ════════════════════════════════════════════════════════════════
  class DashboardSelectors {
    constructor({ filterElements = [], data, state, eventBus }) {
      this.data     = data;
      this.state    = state;
      this.eb       = eventBus;
      this.elements = filterElements;
      this._ts      = [];

      if (this.elements.length === 0) {
        // No UI — apply default immediately if set
        this._applyDefault(data.config.defaultSelection || {});
        return;
      }
      this._initAll();
      this._applyDefault(data.config.defaultSelection || {});
    }

    _initAll() {
      // Populate first level
      this._populateLevel(0, {});
      // Attach change listeners
      this.elements.forEach((el, i) => {
        const handler = () => {
          const parentVals = this._getValues(i + 1);
          for (let j = i + 1; j < this.elements.length; j++) {
            this._populateLevel(j, this._getValues(j));
          }
          this._tryEmit();
        };
        if (this._ts[i]) this._ts[i].on('change', handler);
        else el.addEventListener('change', handler);
      });
    }

    _populateLevel(i, parentValues) {
      if (i >= this.elements.length || i >= this.data.filters.length) return;
      const el    = this.elements[i];
      const f     = this.data.filters[i];
      const label = f.label || f.col;
      const vals  = this.data.getLevelValues(i, parentValues);

      // Destroy existing TomSelect
      if (this._ts[i]) { try { this._ts[i].destroy(); } catch(e){} this._ts[i] = null; }

      // Reset native <select>
      el.innerHTML = '';
      const placeholder = document.createElement('option');
      placeholder.value = ''; placeholder.disabled = true; placeholder.selected = true;
      placeholder.textContent = 'Selecteer ' + label + '...';
      el.appendChild(placeholder);
      vals.forEach(v => { const o = document.createElement('option'); o.value = v; o.textContent = v; el.appendChild(o); });

      // Reinit TomSelect
      if (window.TomSelect) {
        this._ts[i] = new TomSelect(el, { create: false, maxItems: 1, placeholder: 'Selecteer ' + label + '...' });
      }
    }

    _getValues(upToLevel) {
      const vals = {};
      this.data.filters.slice(0, upToLevel).forEach((f, i) => {
        const el = this.elements[i];
        if (!el) return;
        const v = this._ts[i] ? this._ts[i].getValue() : el.value;
        if (v) vals[f.col] = v;
      });
      return vals;
    }

    _tryEmit() {
      const vals = this._getValues(this.data.filters.length);
      const allSet = this.data.filters.length > 0 &&
                     this.data.filters.every((f, i) => !!vals[f.col]);
      if (!allSet) {
        this.state.setSelection(null);
        this.eb.emit('wijk-selected', null);
        return;
      }
      this._emitSelection(vals);
    }

    _emitSelection(filterValues) {
      const sel = this.data.buildSelection(filterValues);
      if (!sel) { console.warn('[sunburstr] No data for selection:', filterValues); return; }
      this.state.setSelection(sel);
      this.eb.emit('wijk-selected', sel);
    }

    _applyDefault(defaults) {
      if (!defaults || Object.keys(defaults).length === 0) {
        // No default — if also no filters defined, emit all rows
        if (this.data.filters.length === 0) this._emitAllData();
        return;
      }
      if (this.elements.length === 0) {
        // No UI elements but have defaults — emit selection directly
        const vals = {};
        this.data.filters.forEach(f => { if (defaults[f.col]) vals[f.col] = defaults[f.col]; });
        if (Object.keys(vals).length > 0) this._emitSelection(vals);
        return;
      }
      // Set TomSelect values level by level
      this.data.filters.forEach((f, i) => {
        if (!defaults[f.col] || !this.elements[i]) return;
        const parentVals = this._getValues(i);
        this._populateLevel(i, parentVals);
        if (this._ts[i]) this._ts[i].setValue(defaults[f.col], true);
        else this.elements[i].value = defaults[f.col];
      });
      this._tryEmit();
    }

    _emitAllData() {
      // No filters at all — emit all rows as the selection
      const lookup = new Map(this.data.rows.map(r => [r.key, _toNumber(r[this.data.scoreCol])]));
      const sel = { filterValues: {}, wijkRows: this.data.rows, comparisonRows: this.data.rows, lookup, entityId: null };
      this.state.setSelection(sel);
      this.eb.emit('wijk-selected', sel);
    }
  }

  // ════════════════════════════════════════════════════════════════
  // Gauge — horizontal colour bar with upward-pointing arrow
  // ════════════════════════════════════════════════════════════════
  class Gauge {
    constructor(container, categories) {
      this.cats  = categories;
      const n    = categories.length;
      const segW = 80;
      const barH = 24;
      const padT = 4;
      const ptrH = 11;
      const lblH = 26;
      const totalW = n * segW;
      const totalH = padT + barH + ptrH + lblH;

      const svg = d3.select(container).append('svg')
        .attr('viewBox', `0 0 ${totalW} ${totalH}`)
        .attr('width', '100%').style('display', 'block');

      svg.selectAll('rect').data(categories).join('rect')
        .attr('x',      (d, i) => i * segW)
        .attr('y',      padT)
        .attr('width',  segW)
        .attr('height', barH)
        .attr('fill',   d => d.color)
        .attr('stroke', '#fff').attr('stroke-width', 1.5);

      svg.selectAll('text').data(categories).join('text')
        .attr('x', (d, i) => (i + 0.5) * segW)
        .attr('y', padT + barH + ptrH + 16)
        .attr('text-anchor', 'middle')
        .style('font-size', '9px').style('fill', '#444')
        .text(d => d.name);

      // Upward-pointing arrow — tip touches bottom of coloured bar
      this._ptr = svg.append('polygon')
        .attr('points', '0,0 -5,10 5,10')
        .attr('fill', '#111')
        .style('opacity', 0)
        .attr('transform', `translate(${segW / 2}, ${padT + barH})`);

      this._segW = segW;
      this._barY = padT + barH;
    }

    update(catName) {
      const i = this.cats.findIndex(d => d.name === catName);
      if (i < 0) return;
      this._ptr
        .style('opacity', 1)
        .transition().duration(300)
        .attr('transform', `translate(${this._segW * (i + 0.5)}, ${this._barY})`);
    }
  }

  // ════════════════════════════════════════════════════════════════
  // Table helpers (config-aware)
  // ════════════════════════════════════════════════════════════════
  function createIndicatorTable(rows, config) {
    const scoreCol = config.scoreCol || 'waarde';
    const hCols    = config.hierarchyCols || [];
    const indCol   = hCols.length ? hCols[hCols.length - 1].col : 'indicator';
    const compCols = config.comparisonCols || [];

    const headers = [
      hCols.length ? hCols[hCols.length - 1].label : 'Indicator',
      'Score',
      ...compCols.map(c => c.label)
    ];
    const sorted = [...rows].sort((a, b) => String(a[indCol] || '').localeCompare(String(b[indCol] || '')));
    const table = document.createElement('table');
    table.className = 'display compact';
    table.innerHTML = `<thead><tr>${headers.map(h => '<th>' + h + '</th>').join('')}</tr></thead>
      <tbody>${sorted.map(r => '<tr><td>' + (r[indCol] || '') + '</td><td>' + _fmt(r[scoreCol]) + '</td>'
        + compCols.map(c => '<td>' + _fmt(r[c.col]) + '</td>').join('') + '</tr>').join('')}</tbody>`;
    return table;
  }

  function initialiseTable(table) {
    return new DataTable(table, { paging: false, searching: false, info: false, ordering: false });
  }

  // ════════════════════════════════════════════════════════════════
  // Comparison plot (config-aware)
  // ════════════════════════════════════════════════════════════════
  function drawComparisonPlot({ elementId, rows, selectedEntityId, config }) {
    const el = document.getElementById(elementId);
    if (!el) return;
    if (!rows || !rows.length) { el.innerHTML = ''; return; }

    const filters  = config.filters || [];
    const deepest  = filters[filters.length - 1];
    const labelCol = deepest ? deepest.col : null;
    const scoreCol = config.scoreCol || 'waarde';
    const compCols = config.comparisonCols || [];

    const ordered = [...rows].sort((a, b) =>
      String(a[labelCol] || '').localeCompare(String(b[labelCol] || ''))
    );
    const colours = ordered.map(r =>
      labelCol && String(r[labelCol]) === String(selectedEntityId) ? '#2C7FB8' : '#CFCFCF'
    );
    const xLabels = ordered.map(r => String(r[labelCol] || '').replace(/ /g, '<br>'));

    // Reference lines from comparisonCols (max 2)
    const shapes = [], annotations = [];
    compCols.slice(0, 2).forEach((cc, i) => {
      const val = ordered[0][cc.col];
      if (val == null) return;
      shapes.push({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: val, y1: val,
                    line: { dash: i === 0 ? 'dash' : 'dot', width: 2 } });
      annotations.push({ x: 0.98, xref: 'paper', y: val, text: cc.label,
                         showarrow: false, xanchor: 'right', yanchor: i === 0 ? 'bottom' : 'top' });
    });

    Plotly.newPlot(elementId, [{
      type: 'bar', x: xLabels, y: ordered.map(r => r[scoreCol]),
      marker: { color: colours },
      hovertemplate: '<b>%{x}</b><br>Score: %{y:.1f}<extra></extra>'
    }], {
      margin: { l: 60, r: 30, t: 30, b: 170 },
      xaxis:  { tickangle: -45 },
      yaxis:  { title: 'Score', range: [0, 100] },
      shapes, annotations
    }, { responsive: true, displayModeBar: false });
  }

  // ════════════════════════════════════════════════════════════════
  // DetailView (config-aware, key-based filtering)
  // ════════════════════════════════════════════════════════════════
  class DetailView {
    constructor({ state, eventBus, headerElement, tableElement, plotElement, config }) {
      this.state  = state;
      this.eb     = eventBus;
      this.config = config;
      this.header = headerElement ? document.querySelector(headerElement) : null;
      this.table  = tableElement  ? document.querySelector(tableElement)  : null;
      this.plot   = plotElement   ? document.querySelector(plotElement)   : null;
      this.eb.on('wijk-selected', s    => { this._updateHeader(s); this._clear(); });
      this.eb.on('node-selected', node => this._renderNode(node));
    }

    _updateHeader(s) {
      if (!this.header) return;
      if (!s || !s.wijkRows.length) { this.header.innerHTML = ''; return; }
      // Build header from filter values
      const parts = this.config.filters.map(f => s.filterValues[f.col]).filter(Boolean);
      this.header.innerHTML = '<h2>' + parts.join(' — ') + '</h2>';
    }

    _clear() {
      if (this.table) this.table.innerHTML = '';
      if (this.plot)  this.plot.innerHTML  = '';
    }

    _renderNode(node) {
      const s = this.state.getSelection();
      this._clear();
      if (!node || !s) return;
      if (node.depth === 1)      this._renderLevel1(node, s);
      else if (node.depth === 2) this._renderLevel2(node, s);
      else if (node.depth === 3) this._renderLevel3(node, s);
    }

    _renderLevel1(node, s) {
      // Domain level: show rows grouped by level-2 (theme)
      const keyPrefix = node.data.key + '|';
      const rows = s.wijkRows.filter(r => r.key && r.key.startsWith(keyPrefix));
      // Group by level-2 key part
      const groups = new Map();
      rows.forEach(r => {
        const parts = r.key.split('|');
        const l2 = parts.length > 1 ? parts[1] : 'Overig';
        if (!groups.has(l2)) groups.set(l2, []);
        groups.get(l2).push(r);
      });
      groups.forEach((grpRows, l2Name) => {
        if (this.table) {
          const h = document.createElement('h3'); h.textContent = l2Name; this.table.appendChild(h);
          const t = createIndicatorTable(grpRows, this.config);
          this.table.appendChild(t); initialiseTable(t);
        }
      });
    }

    _renderLevel2(node, s) {
      // Theme level: show all indicators under this theme
      const keyPrefix = node.data.key + '|';
      const rows = s.wijkRows.filter(r => r.key && r.key.startsWith(keyPrefix));
      if (this.table) {
        const label = node.parent ? node.parent.data.name + ' → ' + node.data.name : node.data.name;
        const h = document.createElement('h3'); h.textContent = label; this.table.appendChild(h);
        const t = createIndicatorTable(rows, this.config);
        this.table.appendChild(t); initialiseTable(t);
      }
    }

    _renderLevel3(node, s) {
      // Indicator level: single row table + comparison plot
      const row = s.wijkRows.find(r => r.key === node.data.key);
      if (!row) return;
      if (this.table) {
        const h = document.createElement('h3'); h.textContent = node.data.name; this.table.appendChild(h);
        const t = createIndicatorTable([row], this.config);
        this.table.appendChild(t); initialiseTable(t);
      }
      if (this.plot) {
        const compRows = s.comparisonRows.filter(r => r.key === node.data.key);
        drawComparisonPlot({ elementId: this.plot.id, rows: compRows, selectedEntityId: s.entityId, config: this.config });
      }
    }
  }

  // ════════════════════════════════════════════════════════════════
  // SunburstComponent helpers
  // ════════════════════════════════════════════════════════════════
  const DEFAULT_CATEGORIES = [
    { name: 'Geen data',          color: '#bdbdbd', min: null },
    { name: 'Ongunstig',          color: '#d73027', min: 0    },
    { name: 'Beetje ongunstiger', color: '#fc8d59', min: 20   },
    { name: 'Gemiddeld',          color: '#fee08b', min: 30   },
    { name: 'Beetje gunstiger',   color: '#91cf60', min: 50   },
    { name: 'Gunstig',            color: '#1a9850', min: 70   }
  ];

  function _catFromScore(score, categories) {
    if (score == null) return categories.find(c => c.min == null) || categories[0];
    const sorted = categories.filter(c => c.min != null).sort((a, b) => b.min - a.min);
    return sorted.find(c => score >= c.min) || sorted[sorted.length - 1] || categories[0];
  }

  function _fontSize(node, dr) {
    const arc = node.x1 - node.x0;
    const r   = (dr[node.depth - 1] + dr[node.depth]) / 2;
    const w   = arc * r;
    if (node.depth === 1) { if (w > 130) return 14; if (w > 90) return 12; return 10; }
    if (w > 90) return 12; if (w > 55) return 10; if (w > 35) return 8;
    return 0;
  }

  function _wrapText(text, maxW, fs) {
    const words = text.split(/\s+/), lines = []; let line = '';
    words.forEach(w => {
      const t = line.length ? line + ' ' + w : w;
      if (t.length * fs * 0.55 <= maxW) line = t; else { if (line) lines.push(line); line = w; }
    });
    if (line) lines.push(line);
    return lines;
  }

  function _equalSpacing(root) {
    const step = (2 * Math.PI) / root.children.length;
    root.children.forEach((p, pi) => {
      p.x0 = pi * step; p.x1 = (pi + 1) * step;
      if (!p.children) return;
      const cs = (p.x1 - p.x0) / p.children.length;
      p.children.forEach((c, ci) => {
        c.x0 = p.x0 + ci * cs; c.x1 = p.x0 + (ci + 1) * cs;
        if (!c.children) return;
        const gs = (c.x1 - c.x0) / c.children.length;
        c.children.forEach((g, gi) => { g.x0 = c.x0 + gi * gs; g.x1 = c.x0 + (gi + 1) * gs; });
      });
    });
  }

  // Shared tooltip
  function _getTooltip() {
    let tip = document.getElementById('__sunburstr_tip');
    if (!tip) {
      tip = document.createElement('div');
      tip.id = '__sunburstr_tip';
      Object.assign(tip.style, {
        position: 'fixed', background: 'rgba(0,0,0,.8)', color: '#fff',
        padding: '6px 10px', borderRadius: '4px', fontSize: '13px',
        pointerEvents: 'none', whiteSpace: 'nowrap', opacity: '0',
        transition: 'opacity .12s', zIndex: '9999'
      });
      document.body.appendChild(tip);
    }
    return tip;
  }

  // ════════════════════════════════════════════════════════════════
  // SunburstComponent
  // ════════════════════════════════════════════════════════════════
  class SunburstComponent {
    constructor({ element, hierarchyData, state, eventBus, categories = DEFAULT_CATEGORIES }) {
      this.el    = d3.select(element);
      this.state = state;
      this.eb    = eventBus;
      this.cats  = categories;

      const rw = { 1: 135, 2: 110, 3: 12 };
      const total = rw[1] + rw[2] + rw[3];
      const dr    = { 0: 0, 1: rw[1], 2: rw[1] + rw[2], 3: total };
      this._dr = dr; this._total = total;

      this.root = d3.hierarchy(hierarchyData).sum(n => n.value || 0);
      d3.partition().size([2 * Math.PI, 1])(this.root);
      _equalSpacing(this.root);
      this.root.each(n => { n.score = null; n.category = categories.find(c => c.min == null) || categories[0]; });

      this._arc = d3.arc()
        .startAngle(n => n.x0).endAngle(n => n.x1)
        .innerRadius(n => dr[n.depth - 1]).outerRadius(n => dr[n.depth])
        .cornerRadius(n => n.depth === 3 ? 4 : 8).padAngle(0.006).padRadius(total);

      this._svg = this.el.append('svg')
        .attr('viewBox', [-total, -total, total * 2, total * 2])
        .attr('width', '100%').style('display', 'block').style('font', '11px sans-serif');

      this._paths  = this._svg.append('g');
      this._labels = this._svg.append('g').attr('pointer-events', 'none').attr('text-anchor', 'middle').style('user-select', 'none');
      this._svg.append('circle').attr('r', 14).attr('fill', 'white');

      this._draw();
      this.eb.on('wijk-selected', s => this._update(s));
    }

    _colour(node) { return node.category?.color ?? '#bdbdbd'; }

    _draw() {
      const self = this;
      this.paths = this._paths.selectAll('path')
        .data(this.root.descendants().filter(n => n.depth > 0))
        .join('path')
        .attr('d', this._arc).attr('fill', n => this._colour(n))
        .attr('stroke', 'white').attr('stroke-width', 2).style('cursor', 'pointer')
        .on('mouseenter', function(ev, n) {
          self.eb.emit('node-hovered', n);
          const base = self._colour(n);
          const brighter = d3.color(base)?.brighter(0.35)?.formatHex() ?? base;
          d3.select(this).raise().transition().duration(120).attr('fill', brighter);
          // Tooltip
          const tip = _getTooltip();
          const catName = n.category?.name ?? '';
          tip.innerHTML = '<strong>' + n.data.name + '</strong>' + (catName ? '<br><span style="opacity:.75">' + catName + '</span>' : '');
          tip.style.opacity = '1';
        })
        .on('mousemove', ev => {
          const tip = _getTooltip();
          tip.style.left = (ev.clientX + 14) + 'px';
          tip.style.top  = (ev.clientY - 36) + 'px';
        })
        .on('mouseleave', function(ev, n) {
          d3.select(this).transition().duration(120).attr('fill', self._colour(n));
          _getTooltip().style.opacity = '0';
        })
        .on('click', (ev, n) => { ev.stopPropagation(); this.state.setNode(n); this.eb.emit('node-selected', n); });

      this._drawLabels();
    }

    _drawLabels() {
      const dr = this._dr;
      this._labels.selectAll('text').remove();
      this.root.descendants().filter(n => n.depth === 1 || n.depth === 2).forEach(node => {
        const fs = _fontSize(node, dr);
        if (!fs) return;
        const midAngle = (node.x0 + node.x1) / 2;
        const midR     = (dr[node.depth - 1] + dr[node.depth]) / 2;
        const arcLen   = (node.x1 - node.x0) * midR;
        const lines    = _wrapText(node.data.name, arcLen * 0.85, fs);
        const lineH    = fs * 1.15;
        const totalH   = lines.length * lineH;
        lines.forEach((ln, li) => {
          const offset = -totalH / 2 + lineH / 2 + li * lineH;
          this._labels.append('text')
            .attr('transform', `rotate(${midAngle * 180 / Math.PI - 90}) translate(${midR},0) rotate(${midAngle > Math.PI ? 180 : 0})`)
            .attr('dy', offset + 'px').style('font-size', fs + 'px').text(ln);
        });
      });
    }

    _update(selection) {
      if (!selection || !selection.lookup) {
        this.root.each(n => { n.score = null; n.category = this.cats.find(c => c.min == null) || this.cats[0]; });
      } else {
        this.root.each(n => {
          if (n.depth < 3) { n.score = null; n.category = this.cats.find(c => c.min == null) || this.cats[0]; return; }
          const sc = selection.lookup.get(n.data.key);
          n.score = sc ?? null;
          n.category = _catFromScore(n.score, this.cats);
        });
        // Propagate to parents
        this.root.each(n => {
          if (n.depth !== 2) return;
          const kids = (n.children || []).filter(c => c.score != null);
          n.score = kids.length ? kids.reduce((a, c) => a + c.score, 0) / kids.length : null;
          n.category = _catFromScore(n.score, this.cats);
        });
        this.root.each(n => {
          if (n.depth !== 1) return;
          const kids = (n.children || []).filter(c => c.score != null);
          n.score = kids.length ? kids.reduce((a, c) => a + c.score, 0) / kids.length : null;
          n.category = _catFromScore(n.score, this.cats);
        });
      }
      this.paths.transition().duration(350).attr('fill', n => this._colour(n));
    }
  }

  // ════════════════════════════════════════════════════════════════
  // mountSunburstDashboard
  // ════════════════════════════════════════════════════════════════
  function _elExists(sel) { return sel && document.querySelector(sel); }

  function mountSunburstDashboard({
    configScriptId     = 'config-data',
    hierarchyScriptId  = 'hierarchy-data',
    wijkScriptId       = 'wijk-data',
    filterSelectors    = [],
    sunburstSelector   = null,
    gaugeSelector      = null,
    headerSelector     = null,
    tableSelector      = null,
    plotSelector       = null
  } = {}) {
    const config        = readEmbeddedJson(configScriptId);
    const wijkData      = readEmbeddedJson(wijkScriptId);
    const hierarchyData = readEmbeddedJson(hierarchyScriptId);

    const eventBus   = new EventBus();
    const state      = new DashboardState();
    const data       = new DashboardData(wijkData, config);
    const categories = config.categories?.length ? config.categories : DEFAULT_CATEGORIES;

    // Map filterSelectors to actual DOM elements
    const filterElements = filterSelectors.map(sel => sel && document.querySelector(sel)).filter(Boolean);

    new DashboardSelectors({ filterElements, data, state, eventBus });

    let sunburst = null;
    if (_elExists(sunburstSelector)) {
      sunburst = new SunburstComponent({ element: sunburstSelector, hierarchyData, state, eventBus, categories });
    }

    let gauge = null;
    if (_elExists(gaugeSelector)) {
      gauge = new Gauge(gaugeSelector, categories);
      eventBus.on('node-hovered', n => { if (n?.category) gauge.update(n.category.name); });
    }

    if (_elExists(headerSelector) || _elExists(tableSelector) || _elExists(plotSelector)) {
      new DetailView({ state, eventBus, headerElement: headerSelector, tableElement: tableSelector, plotElement: plotSelector, config });
    }

    return { state, data, sunburst, gauge };
  }

  // Export
  window.SunburstDashboard = { mountSunburstDashboard };
})();
