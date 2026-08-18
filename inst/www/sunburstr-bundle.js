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
      // Broadcast partial state so PolygonSelector can filter polygons
      // even before all filter levels are set.
      this.eb.emit('filter-level-changed', vals);
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
  // Gauge — horizontal colour bar with downward-pointing arrow above
  // ════════════════════════════════════════════════════════════════
  class Gauge {
    constructor(container, categories) {
      this.cats  = categories;
      const n    = categories.length;
      const segW = 80;
      const ptrH = 14;   // space above bar for the arrow
      const barH = 24;
      const lblH = 26;
      const totalW = n * segW;
      const totalH = ptrH + barH + lblH;

      const svg = d3.select(container).append('svg')
        .attr('viewBox', `0 0 ${totalW} ${totalH}`)
        .attr('width', '100%').style('display', 'block');

      // Coloured segments — start at y=ptrH, arrow lives above them
      svg.selectAll('rect').data(categories).join('rect')
        .attr('x',      (d, i) => i * segW)
        .attr('y',      ptrH)
        .attr('width',  segW)
        .attr('height', barH)
        .attr('fill',   d => d.color)
        .attr('stroke', '#fff').attr('stroke-width', 1.5);

      svg.selectAll('text').data(categories).join('text')
        .attr('x', (d, i) => (i + 0.5) * segW)
        .attr('y', ptrH + barH + 18)
        .attr('text-anchor', 'middle')
        .style('font-size', '9px').style('fill', '#444')
        .text(d => d.name);

      // Downward arrow: tip at (0,0), base 12 px above — hidden until first hover
      this._ptr = svg.append('polygon')
        .attr('points', '0,0 -5,-12 5,-12')
        .attr('fill', '#111')
        .style('opacity', 0)
        .attr('transform', `translate(${segW / 2}, ${ptrH})`);

      this._segW = segW;
      this._ptrY = ptrH;
    }

    update(catName) {
      const i = this.cats.findIndex(d => d.name === catName);
      if (i < 0) return;
      this._ptr
        .style('opacity', 1)
        .transition().duration(600).ease(d3.easeCubicInOut)
        .attr('transform', `translate(${this._segW * (i + 0.5)}, ${this._ptrY})`);
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
  // PolygonSelector — clickable SVG map that drives wijk-selected
  // ════════════════════════════════════════════════════════════════
  class PolygonSelector {
    constructor({ container, geoData, filterLevel, nameProp,
                  parentFilter, parentProp, showWhenFilter,
                  layered = false, zoomToVisible = true, backLabel = 'Terug naar hoger niveau',
                  data, state, eventBus,
                  colors = {}, selectedStrokeWidth = 2.5, showEmptyGeometries = true }) {
      this.el             = document.querySelector(container);
      this.geo            = geoData;
      this.filterLevel    = filterLevel;
      this.nameProp       = nameProp;
      this.parentFilter   = parentFilter;
      this.parentProp     = parentProp;
      this.showWhenFilter = showWhenFilter;
      this.layered        = !!layered;
      this.zoomToVisible  = zoomToVisible !== false;
      this.backLabel      = backLabel || 'Terug naar hoger niveau';
      this.data           = data;
      this.state          = state;
      this.eb             = eventBus;
      this.currentLevel   = this.layered ? 'parent' : 'child';
      this.currentParentValue = null;
      this.selectedParentValue = null;

      // Configurable colors and stroke
      this.colors = {
        fill:     colors.fill     || '#dde4eb',
        stroke:   colors.stroke   || '#ffffff',
        hover:    colors.hover    || '#a0b4c8',
        selected: colors.selected || '#2C7FB8',
        empty:    colors.empty    || '#f5f5f5'
      };
      this.selectedStrokeWidth = selectedStrokeWidth;
      this.showEmptyGeometries = showEmptyGeometries;

      this._render();
      this.eb.on('filter-level-changed', vals => this._onFilterChanged(vals), false);
      this.eb.on('wijk-selected', s => this._onWijkSelected(s), false);
      this._onFilterChanged(this.eb.get('filter-level-changed') || {});
    }

    _render() {
      const W = 500, H = 380;
      this.W = W; this.H = H;
      this.proj = d3.geoMercator().fitSize([W, H], this.geo);
      this.pathFn = d3.geoPath().projection(this.proj);
      const self   = this;

      this.el.innerHTML = '';
      this.el.classList.toggle('polygon-selector--layered', this.layered);

      this.toolbar = document.createElement('div');
      this.toolbar.className = 'polygon-selector-toolbar';
      this.el.appendChild(this.toolbar);

      this.backButton = document.createElement('button');
      this.backButton.type = 'button';
      this.backButton.className = 'polygon-selector-back';
      this.backButton.textContent = this.backLabel;
      this.backButton.style.display = 'none';
      this.backButton.addEventListener('click', () => this._goBack());
      this.toolbar.appendChild(this.backButton);

      this.message = document.createElement('div');
      this.message.className = 'polygon-selector-message';
      this.toolbar.appendChild(this.message);

      this.svg = d3.select(this.el).append('svg')
        .attr('viewBox', `0 0 ${W} ${H}`)
        .attr('width', '100%').style('display', 'block');

      this.mapLayer = this.svg.append('g').attr('class', 'polygon-selector-layer');
      this.parentFeatures = this.layered ? this._buildParentFeatures() : [];
      this._drawCurrentLayer();
    }

    _buildParentFeatures() {
      if (!this.parentProp) return [];
      const groups = d3.group(this.geo.features, f => f.properties[this.parentProp]);
      return Array.from(groups, ([parentValue, features]) => {
        const geometries = features.map(f => f.geometry).filter(Boolean);
        return {
          type: 'Feature',
          properties: {
            [this.nameProp]: parentValue,
            [this.parentProp]: parentValue,
            __level: 'parent'
          },
          geometry: geometries.length === 1
            ? geometries[0]
            : { type: 'GeometryCollection', geometries }
        };
      }).filter(f => f.properties[this.nameProp]);
    }

    _getCurrentFeatures() {
      if (this.layered && this.currentLevel === 'parent') {
        return this.parentFeatures;
      }
      const parentValue = this.currentParentValue;
      return (this.geo.features || []).filter(f => {
        if (!parentValue || !this.parentProp) return true;
        return f.properties[this.parentProp] === parentValue;
      });
    }

    // Determine which polygon names have data for the current view
    _getDataValues(parentValue) {
      const filterCol = this.data.filters[this.filterLevel]?.col;
      if (!filterCol) return new Set();
      const rows = this.data.rows.filter(r => {
        if (!parentValue || !this.parentFilter) return true;
        return r[this.parentFilter] === parentValue;
      });
      return new Set(rows.map(r => r[filterCol]).filter(Boolean));
    }

    _drawCurrentLayer() {
      const self = this;
      let features = this._getCurrentFeatures();

      // Determine which polygon names have matching data
      const dataValues = this._getDataValues(this.currentParentValue);

      // Filter out empty geometries if configured to hide them
      if (!this.showEmptyGeometries && this.currentLevel === 'child') {
        features = features.filter(f => dataValues.has(f.properties[self.nameProp]));
      }

      this.mapLayer.selectAll('path').remove();

      this.paths = this.mapLayer.selectAll('path')
        .data(features, f => `${self.currentLevel}:${f.properties[self.nameProp]}`)
        .join('path')
        .attr('d', this.pathFn)
        .attr('fill', f => {
          if (self.currentLevel === 'child' && !dataValues.has(f.properties[self.nameProp])) {
            return self.colors.empty;
          }
          return self.colors.fill;
        })
        .attr('stroke', this.colors.stroke)
        .attr('stroke-width', 0.8)
        .attr('data-has-data', f => {
          if (self.currentLevel !== 'child') return 'true';
          return dataValues.has(f.properties[self.nameProp]) ? 'true' : 'false';
        })
        .style('cursor', f => {
          if (self.currentLevel === 'child' && !dataValues.has(f.properties[self.nameProp])) {
            return 'default';
          }
          return 'pointer';
        })
        .on('mouseenter', function(ev, f) {
          const hasData = d3.select(this).attr('data-has-data') !== 'false';
          if (!hasData) return;
          if (d3.select(this).attr('data-selected') !== 'true') d3.select(this).attr('fill', self.colors.hover);
          const tip = _getTooltip();
          tip.innerHTML = '<strong>' + f.properties[self.nameProp] + '</strong>';
          tip.style.opacity = '1';
        })
        .on('mousemove', function(ev) {
          const hasData = d3.select(this).attr('data-has-data') !== 'false';
          if (!hasData) return;
          const tip = _getTooltip();
          tip.style.left = (ev.clientX + 14) + 'px';
          tip.style.top  = (ev.clientY - 36) + 'px';
        })
        .on('mouseleave', function() {
          const hasData = d3.select(this).attr('data-has-data') !== 'false';
          if (!hasData) return;
          if (d3.select(this).attr('data-selected') !== 'true') d3.select(this).attr('fill', self.colors.fill);
          _getTooltip().style.opacity = '0';
        })
        .on('click', function(ev, f) {
          const hasData = d3.select(this).attr('data-has-data') !== 'false';
          if (!hasData) return;
          ev.stopPropagation();
          self._handleClick(f);
        });

      this._updateBackButton();
      this._updateMessage();
      this._fitVisibleFeatures();
    }

    _handleClick(feature) {
      if (this.layered && this.currentLevel === 'parent') {
        this.selectedParentValue = feature.properties[this.nameProp];
        this.currentParentValue = this.selectedParentValue;
        this.currentLevel = 'child';
        this._drawCurrentLayer();
        return;
      }
      this._emitSelection(feature);
    }

    _emitSelection(feature) {
      const filterCol = this.data.filters[this.filterLevel]?.col;
      if (!filterCol) return;
      const myValue = feature.properties[this.nameProp];
      const partial = this.eb.get('filter-level-changed') || {};
      const parentValues = {};
      this.data.filters.slice(0, this.filterLevel).forEach(f => {
        if (partial[f.col]) parentValues[f.col] = partial[f.col];
      });
      if (this.parentFilter && this.currentParentValue) {
        parentValues[this.parentFilter] = this.currentParentValue;
      }
      const filterValues = { ...parentValues, [filterCol]: myValue };
      const selection = this.data.buildSelection(filterValues);
      if (!selection) { console.warn('[quartoWidgets] No data for polygon:', filterValues); return; }
      this.state.setSelection(selection);
      this.eb.emit('wijk-selected', selection);
    }

    _fitVisibleFeatures() {
      if (!this.zoomToVisible || !this.paths || !this.paths.size()) return;
      const visible = this.paths.filter(function() {
        return this.style.display !== 'none';
      });
      if (!visible.size()) return;
      let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
      visible.each((d, i, nodes) => {
        const box = nodes[i].getBBox();
        if (!box || !isFinite(box.x)) return;
        minX = Math.min(minX, box.x);
        minY = Math.min(minY, box.y);
        maxX = Math.max(maxX, box.x + box.width);
        maxY = Math.max(maxY, box.y + box.height);
      });
      if (!isFinite(minX) || maxX <= minX || maxY <= minY) {
        this.mapLayer.attr('transform', null);
        return;
      }
      const pad = 16;
      const width = maxX - minX;
      const height = maxY - minY;
      const scale = Math.min((this.W - 2 * pad) / width, (this.H - 2 * pad) / height, 8);
      const tx = (this.W - scale * (minX + maxX)) / 2;
      const ty = (this.H - scale * (minY + maxY)) / 2;
      this.mapLayer
        .transition().duration(300)
        .attr('transform', `translate(${tx},${ty}) scale(${scale})`);
    }

    _updateVisibility(vals) {
      const requiredValue = this.showWhenFilter ? vals[this.showWhenFilter] : true;
      const visible = !!requiredValue || !this.showWhenFilter;
      this.el.classList.toggle('polygon-selector-hidden', !visible);
      if (!visible) {
        this.message.textContent = 'Maak eerst een selectie om de kaart te tonen.';
        this.mapLayer.attr('transform', null);
      }
      return visible;
    }

    _updateMessage() {
      if (this.showWhenFilter && this.el.classList.contains('polygon-selector-hidden')) return;
      if (this.layered && this.currentLevel === 'parent') {
        this.message.textContent = 'Klik op een polygon om naar het volgende niveau te gaan.';
      } else if (this.layered && this.currentParentValue) {
        this.message.textContent = `Klik op een polygon binnen ${this.currentParentValue}.`;
      } else if (this.parentFilter && this.currentParentValue) {
        this.message.textContent = `Klik op een polygon binnen ${this.currentParentValue}.`;
      } else {
        this.message.textContent = 'Klik op een polygon om te selecteren.';
      }
    }

    _updateBackButton() {
      if (!this.backButton) return;
      this.backButton.style.display = this.layered && this.currentLevel === 'child' ? '' : 'none';
    }

    _goBack() {
      this.currentLevel = 'parent';
      this.currentParentValue = null;
      this.selectedParentValue = null;
      this.state.setSelection(null);
      this.eb.emit('wijk-selected', null);
      this._drawCurrentLayer();
    }

    _onFilterChanged(vals) {
      if (!this._updateVisibility(vals)) return;

      const parentValue = this.parentFilter ? vals[this.parentFilter] : null;
      if (this.layered) {
        if (parentValue && this.currentLevel === 'parent') {
          this.currentParentValue = parentValue;
          this.selectedParentValue = parentValue;
          this.currentLevel = 'child';
        } else if (parentValue && this.currentLevel === 'child') {
          this.currentParentValue = parentValue;
          this.selectedParentValue = parentValue;
        } else if (!parentValue && this.currentLevel === 'child' && !this.selectedParentValue) {
          this.currentParentValue = null;
        }
        this._drawCurrentLayer();
        this._highlightCurrentSelection();
        return;
      }

      this.currentParentValue = parentValue;
      if (this.paths) {
        this.paths.style('display', f =>
          !parentValue || !this.parentProp || f.properties[this.parentProp] === parentValue ? null : 'none'
        );
        this.paths.attr('fill', this.colors.fill).attr('data-selected', null)
          .attr('stroke-width', 0.8);
        this._fitVisibleFeatures();
      }
      this._updateMessage();
    }

    _highlightCurrentSelection() {
      const s = this.state.getSelection();
      this._onWijkSelected(s);
    }

    _onWijkSelected(s) {
      if (!this.paths) return;
      if (!s?.filterValues) {
        this.paths.attr('fill', this.colors.fill).attr('data-selected', null)
          .attr('stroke-width', 0.8).attr('stroke', this.colors.stroke);
        return;
      }
      const filterCol = this.data.filters[this.filterLevel]?.col;
      if (!filterCol) return;
      const selected = s.filterValues[filterCol];
      const self = this;
      this.paths
        .attr('fill', f => f.properties[this.nameProp] === selected ? this.colors.selected : this.colors.fill)
        .attr('data-selected', f => f.properties[this.nameProp] === selected ? 'true' : null)
        .attr('stroke-width', f => f.properties[this.nameProp] === selected ? this.selectedStrokeWidth : 0.8)
        .attr('stroke', f => f.properties[this.nameProp] === selected ? this.colors.stroke : this.colors.stroke);

      // Raise selected polygon so its outline renders on top of neighbors
      this.paths.filter(f => f.properties[self.nameProp] === selected).raise();
    }
  }

  // ════════════════════════════════════════════════════════════════
  // mountWidgets
  // ════════════════════════════════════════════════════════════════
  function _elExists(sel) { return sel && document.querySelector(sel); }

  function mountWidgets({
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

    // Build public API — expose addPolygonSelector() so polygon selector
    // boot scripts (emitted by polygon_selector() in R) can attach
    // themselves to this widget set's EventBus after DOMContentLoaded.
    const api = {
      state, data, sunburst, gauge,
      addPolygonSelector({ containerSelector, geoScriptId, filterLevel, nameProp, parentFilter, parentProp, showWhenFilter, layered, zoomToVisible, backLabel, colors, selectedStrokeWidth, showEmptyGeometries }) {
        if (!_elExists(containerSelector)) return;
        const geoData = readEmbeddedJson(geoScriptId);
        new PolygonSelector({
          container: containerSelector, geoData, filterLevel,
          nameProp, parentFilter, parentProp, showWhenFilter,
          layered, zoomToVisible, backLabel, data, state, eventBus,
          colors: colors || {}, selectedStrokeWidth, showEmptyGeometries
        });
      }
    };
    // Register globally under the widget id so polygon_selector boot scripts find it
    window.__quartoWidgets = window.__quartoWidgets || {};
    window.__quartoWidgets[config.id || configScriptId] = api;
    return api;
  }

  // Export
  window.QuartoWidgets = { mountWidgets, mountSunburstDashboard: mountWidgets };
  window.SunburstDashboard = { mountSunburstDashboard: mountWidgets };
})();
