#!/usr/bin/env python3
"""
Generate analysis_pivot_v6.html with fixes for chart ordering and axis controls.

Changes from v5:
1. Chart x-axis labels now sorted numerically when all labels are numeric (fixes years.elapsed, months.elapsed)
2. Line chart mode: both X and Y axis controls enabled (not greyed out)
3. All chart settings reset to defaults when dataset changes
"""

html_content = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>NWDC Analysis — Pivot Table & Charts</title>
<script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.3/dist/chart.umd.min.js"></script>
<style>
  :root {
    --bg: #0d1117; --bg2: #161b22; --bg3: #21262d; --border: #30363d;
    --text: #e6edf3; --text2: #8b949e; --accent: #e05c2d; --accent2: #f0a070; --blue: #58a6ff;
  }
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { background: var(--bg); color: var(--text); font-family: 'Segoe UI', system-ui, sans-serif; font-size: 14px; }

  .header { background: var(--bg2); border-bottom: 1px solid var(--border); padding: 16px 24px; }
  .header h1 { font-size: 18px; font-weight: 600; }
  .header .subtitle { font-size: 12px; color: var(--text2); margin-top: 2px; }
  .main { padding: 20px 24px; }

  .controls { background: var(--bg2); border: 1px solid var(--border); border-radius: 8px; padding: 16px; margin-bottom: 20px; }
  .controls h3 { font-size: 13px; font-weight: 600; color: var(--text2); text-transform: uppercase; letter-spacing: 0.4px; margin-bottom: 12px; }

  .dataset-selector { margin-bottom: 16px; padding-bottom: 16px; border-bottom: 2px solid var(--border); }
  .dataset-selector label { display: block; font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: var(--text2); margin-bottom: 6px; }
  .dataset-selector select { width: 100%; max-width: 400px; background: var(--bg3); border: 1px solid var(--border); color: var(--text); padding: 10px 12px; border-radius: 6px; font-size: 14px; font-weight: 500; }

  .pivot-builder { display: grid; grid-template-columns: 200px 1fr; gap: 16px; margin-bottom: 16px; }
  .field-list { background: var(--bg3); border: 1px solid var(--border); border-radius: 6px; padding: 12px; max-height: 400px; overflow-y: auto; }
  .field-list h4 { font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: var(--text2); margin-bottom: 8px; }
  .field-item { background: var(--bg2); border: 1px solid var(--border); border-radius: 4px; padding: 6px 8px; margin-bottom: 6px; font-size: 12px; cursor: move; user-select: none; }
  .field-item:hover { background: var(--bg); border-color: var(--accent); }
  .field-item.dragging { opacity: 0.5; }

  .drop-zones { display: grid; grid-template-columns: 1fr 1fr; grid-template-rows: auto auto; gap: 12px; }
  .drop-zone { background: var(--bg3); border: 2px dashed var(--border); border-radius: 6px; padding: 12px; min-height: 80px; }
  .drop-zone h4 { font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: var(--text2); margin-bottom: 8px; }
  .drop-zone.drag-over { border-color: var(--accent); background: rgba(224, 92, 45, 0.1); }
  .drop-zone.filters { grid-column: 1 / -1; }

  .zone-item { background: var(--bg2); border: 1px solid var(--border); border-radius: 4px; padding: 6px 8px; margin-bottom: 6px; font-size: 12px; display: flex; align-items: center; justify-content: space-between; cursor: move; }
  .zone-item:hover { background: var(--bg); }
  .zone-item .remove-btn { color: var(--text2); cursor: pointer; padding: 0 4px; }
  .zone-item .remove-btn:hover { color: var(--accent); }

  .filter-values { background: var(--bg); border: 1px solid var(--border); border-radius: 4px; padding: 8px; margin-top: 4px; max-height: 200px; overflow-y: auto; }
  .filter-value-item { font-size: 11px; padding: 4px; cursor: pointer; user-select: none; border-radius: 3px; }
  .filter-value-item:hover { background: var(--bg3); }
  .filter-value-item.selected { background: var(--accent); color: white; }
  .filter-controls { display: flex; gap: 8px; margin-bottom: 6px; }
  .filter-controls button { background: var(--bg2); border: 1px solid var(--border); color: var(--text2); padding: 3px 8px; border-radius: 3px; font-size: 10px; cursor: pointer; }
  .filter-controls button:hover { background: var(--bg); color: var(--text); }

  .agg-controls { display: flex; gap: 12px; margin-bottom: 12px; flex-wrap: wrap; }
  .control-group { flex: 1; min-width: 150px; }
  .control-group label { display: block; font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: var(--text2); margin-bottom: 6px; }
  .control-group.disabled { opacity: 0.5; pointer-events: none; }
  select, input[type=text], input[type=number] { width: 100%; background: var(--bg3); border: 1px solid var(--border); color: var(--text); padding: 8px 10px; border-radius: 6px; font-size: 13px; }
  select:focus, input:focus { outline: none; border-color: var(--accent); }
  input[type=number] { width: 100px; }
  input[type=checkbox] { accent-color: var(--accent); width: 16px; height: 16px; cursor: pointer; }

  .btn-secondary { background: var(--bg3); border: 1px solid var(--border); color: var(--text); padding: 8px 16px; border-radius: 6px; cursor: pointer; font-size: 13px; }
  .btn-secondary:hover { background: var(--border); }

  .results { background: var(--bg2); border: 1px solid var(--border); border-radius: 8px; padding: 16px; margin-bottom: 20px; }
  .results h3 { font-size: 13px; font-weight: 600; color: var(--text2); text-transform: uppercase; letter-spacing: 0.4px; margin-bottom: 12px; }

  .table-container { overflow-x: auto; overflow-y: auto; max-height: 600px; border: 1px solid var(--border); border-radius: 6px; margin-bottom: 20px; }
  table { width: 100%; border-collapse: collapse; font-size: 12px; }
  th, td { padding: 10px 12px; border-bottom: 1px solid var(--border); border-right: 1px solid var(--border); text-align: center; vertical-align: middle; }
  th:last-child, td:last-child { border-right: none; }
  th { background: var(--bg3); color: var(--text2); font-weight: 600; position: sticky; top: 0; z-index: 10; }
  th:first-child, td:first-child { text-align: left; }
  tr:hover { background: var(--bg3); }

  .chart-section { background: var(--bg2); border: 1px solid var(--border); border-radius: 8px; padding: 16px; }
  .chart-controls { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 12px; margin-bottom: 16px; }
  .chart-container { position: relative; height: 500px; }

  .loading-overlay { position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(13, 17, 23, 0.95); display: none; align-items: center; justify-content: center; z-index: 1000; flex-direction: column; gap: 16px; }
  .loading-overlay.active { display: flex; }
  .loading-spinner { border: 3px solid var(--border); border-top: 3px solid var(--accent); border-radius: 50%; width: 40px; height: 40px; animation: spin 1s linear infinite; }
  .loading-text { color: var(--text); font-size: 14px; font-weight: 500; }
  .loading-progress { width: 300px; height: 8px; background: var(--bg3); border: 1px solid var(--border); border-radius: 4px; overflow: hidden; margin-top: 8px; }
  .loading-progress-bar { height: 100%; background: var(--accent); transition: width 0.3s ease; width: 0%; }
  .loading-percentage { color: var(--text2); font-size: 12px; margin-top: 8px; }
  @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }

  .info { background: var(--bg3); border: 1px solid var(--border); border-radius: 6px; padding: 12px; font-size: 12px; color: var(--text2); margin-top: 12px; }
  .checkbox-group { display: flex; align-items: center; gap: 8px; }
</style>
</head>
<body>

<div class="loading-overlay" id="loading-overlay">
  <div class="loading-spinner"></div>
  <div class="loading-text" id="loading-text">Loading dataset...</div>
  <div class="loading-progress">
    <div class="loading-progress-bar" id="loading-progress-bar"></div>
  </div>
  <div class="loading-percentage" id="loading-percentage">0%</div>
</div>

<div class="header">
  <h1>🔬 Nuclear Winter Data Commons — Analysis Dashboard</h1>
  <div class="subtitle">Interactive Pivot Table & Charts for Multi-Dataset Analysis</div>
</div>

<div class="main">
  <div class="controls">
    <h3>Dataset Selection</h3>
    <div class="dataset-selector">
      <label>Select Thematic Dataset</label>
      <select id="dataset-select">
        <option value="agriculture_agmip">agriculture_agmip</option>
        <option value="agriculture_clm">agriculture_clm</option>
        <option value="fish_catch">fish_catch</option>
        <option value="precipitation">precipitation</option>
        <option value="sea_ice">sea_ice</option>
        <option value="starvation" selected>starvation</option>
        <option value="surface_solar_radiation">surface_solar_radiation</option>
        <option value="temperature">temperature</option>
        <option value="uv">uv</option>
      </select>
    </div>

    <h3>Pivot Table Configuration</h3>
    <div class="pivot-builder">
      <div class="field-list">
        <h4>Available Fields</h4>
        <div id="field-list-container"></div>
      </div>
      <div class="drop-zones">
        <div class="drop-zone" id="rows-zone" data-zone="rows">
          <h4>Rows</h4>
          <div class="zone-content" id="rows-content"></div>
        </div>
        <div class="drop-zone" id="columns-zone" data-zone="columns">
          <h4>Columns</h4>
          <div class="zone-content" id="columns-content"></div>
        </div>
        <div class="drop-zone filters" id="filters-zone" data-zone="filters">
          <h4>Filters</h4>
          <div class="zone-content" id="filters-content"></div>
        </div>
      </div>
    </div>
    <div class="agg-controls">
      <div class="control-group">
        <label>Values</label>
        <select id="values-select"></select>
      </div>
      <div class="control-group">
        <label>Aggregation</label>
        <select id="agg-select">
          <option value="mean" selected>Mean</option>
          <option value="weighted_mean">Weighted Mean</option>
          <option value="sum">Sum</option>
          <option value="count">Count</option>
        </select>
      </div>
      <div class="control-group" id="weight-group" style="display:none;">
        <label>Weight By</label>
        <select id="weight-select"></select>
      </div>
    </div>
    <div class="info"><strong>Auto-Update:</strong> Table and chart regenerate automatically when you change configuration (300ms delay).</div>
  </div>

  <div class="results">
    <h3>Pivot Table Results</h3>
    <div class="table-container">
      <table id="pivot-table"><thead id="pivot-thead"></thead><tbody id="pivot-tbody"></tbody></table>
    </div>
    <div class="info" id="result-info">Drag fields to Rows/Columns to begin.</div>
  </div>

  <div class="chart-section" id="chart-section" style="display:none;">
    <h3>Chart Visualization</h3>
    <div class="chart-controls">
      <div class="control-group">
        <label>Chart Type</label>
        <select id="chart-type">
          <option value="bar">Column (Vertical Bar)</option>
          <option value="horizontalBar">Bar (Horizontal)</option>
          <option value="line">Line</option>
        </select>
      </div>
      <div class="control-group" id="x-min-group">
        <label>X-Axis Min</label>
        <input type="number" id="x-min" placeholder="auto">
      </div>
      <div class="control-group" id="x-max-group">
        <label>X-Axis Max</label>
        <input type="number" id="x-max" placeholder="auto">
      </div>
      <div class="control-group" id="y-min-group">
        <label>Y-Axis Min</label>
        <input type="number" id="y-min" placeholder="auto">
      </div>
      <div class="control-group" id="y-max-group">
        <label>Y-Axis Max</label>
        <input type="number" id="y-max" placeholder="auto">
      </div>
      <div class="control-group checkbox-group">
        <input type="checkbox" id="show-legend" checked>
        <label for="show-legend" style="margin:0;">Show Legend</label>
      </div>
      <div class="control-group">
        <button class="btn-secondary" onclick="resetChart()">Reset Chart</button>
      </div>
    </div>
    <div class="chart-container"><canvas id="pivot-chart"></canvas></div>
  </div>
</div>

<script>
let CURRENT_DATA=null,ALL_FIELDS=[],PIVOT_RESULTS=null,CHART_INSTANCE=null,updateTimer=null;
let CURRENT_DATASET='starvation';
const pivotConfig={rows:[],columns:[],filters:{}};

// Load initial dataset
loadDataset('starvation');

// Dataset selector change handler
document.getElementById('dataset-select').addEventListener('change',function(){
  const newDataset=this.value;
  if(newDataset===CURRENT_DATASET)return;

  // Check if user has made changes
  const hasChanges=pivotConfig.rows.length>0||pivotConfig.columns.length>0||Object.keys(pivotConfig.filters).length>0;

  if(hasChanges){
    const msg='Switching datasets will reset your current pivot table configuration. Continue?';
    if(!confirm(msg)){
      this.value=CURRENT_DATASET;
      return;
    }
  }

  loadDataset(newDataset);
});

function loadDataset(datasetName){
  const progressStages={
    'unload':10,
    'fetch':30,
    'parse':60,
    'init':90,
    'done':100
  };

  showLoading(true,'Preparing to load dataset...',0);

  setTimeout(()=>{
    // Stage 1: Unload current data
    showLoading(true,'Unloading current dataset...',progressStages.unload);
    CURRENT_DATA=null;
    ALL_FIELDS=[];
    resetPivotTable();
    resetChartSettings(); // Reset chart settings when changing dataset

    setTimeout(()=>{
      // Stage 2: Fetch new dataset
      showLoading(true,`Fetching ${datasetName} data...`,progressStages.fetch);

      fetch(`data/${datasetName}.json`)
        .then(response=>{
          if(!response.ok)throw new Error(`HTTP error! status: ${response.status}`);
          return response.json();
        })
        .then(data=>{
          // Stage 3: Parse data
          showLoading(true,`Parsing ${data.length.toLocaleString()} rows...`,progressStages.parse);

          setTimeout(()=>{
            CURRENT_DATA=data;
            ALL_FIELDS=Object.keys(data[0]||{});
            CURRENT_DATASET=datasetName;

            // Stage 4: Initialize interface
            showLoading(true,'Initializing interface...',progressStages.init);

            setTimeout(()=>{
              initializeInterface();

              // Stage 5: Done
              showLoading(true,'Ready!',progressStages.done);

              setTimeout(()=>{
                showLoading(false);
                console.log(`Loaded ${datasetName}: ${data.length.toLocaleString()} rows, ${ALL_FIELDS.length} fields`);
              },300);
            },100);
          },100);
        })
        .catch(err=>{
          console.error('Error loading dataset:',err);
          showLoading(false);
          alert(`Error loading dataset: ${err.message}`);
          document.getElementById('dataset-select').value=CURRENT_DATASET;
        });
    },100);
  },100);
}

function resetPivotTable(){
  pivotConfig.rows=[];
  pivotConfig.columns=[];
  pivotConfig.filters={};
  PIVOT_RESULTS=null;

  if(CHART_INSTANCE){
    CHART_INSTANCE.destroy();
    CHART_INSTANCE=null;
  }

  document.getElementById('rows-content').innerHTML='';
  document.getElementById('columns-content').innerHTML='';
  document.getElementById('filters-content').innerHTML='';
  document.getElementById('pivot-thead').innerHTML='';
  document.getElementById('pivot-tbody').innerHTML='';
  document.getElementById('result-info').textContent='Drag fields to Rows/Columns to begin.';
  document.getElementById('chart-section').style.display='none';
}

function resetChartSettings(){
  // Reset all chart controls to defaults
  document.getElementById('chart-type').value='bar';
  document.getElementById('x-min').value='';
  document.getElementById('x-max').value='';
  document.getElementById('y-min').value='';
  document.getElementById('y-max').value='';
  document.getElementById('show-legend').checked=true;
}

function showLoading(show,text='Loading...',percentage=0){
  const overlay=document.getElementById('loading-overlay');
  overlay.classList.toggle('active',show);

  if(show){
    document.getElementById('loading-text').textContent=text;
    document.getElementById('loading-progress-bar').style.width=percentage+'%';
    document.getElementById('loading-percentage').textContent=Math.round(percentage)+'%';
  }
}

function initializeInterface(){
  // Populate field list
  const fl=document.getElementById('field-list-container');
  fl.innerHTML='';
  ALL_FIELDS.forEach(f=>{
    const d=document.createElement('div');d.className='field-item';d.textContent=f;d.draggable=true;d.dataset.field=f;
    d.addEventListener('dragstart',e=>{e.target.classList.add('dragging');e.dataTransfer.setData('field',f);});
    d.addEventListener('dragend',e=>e.target.classList.remove('dragging'));
    fl.appendChild(d);
  });

  // Populate values and weight selects
  const vs=document.getElementById('values-select'),ws=document.getElementById('weight-select');
  vs.innerHTML='';ws.innerHTML='';
  ALL_FIELDS.forEach((f,i)=>{
    const o1=document.createElement('option'),o2=document.createElement('option');
    o1.value=o2.value=f;o1.textContent=o2.textContent=f;
    if(i===0)o1.selected=true;
    if(f.includes('population'))o2.selected=true;
    vs.appendChild(o1);ws.appendChild(o2);
  });

  // Setup drag-drop zones (only once)
  if(!document.querySelector('.drop-zone[data-initialized]')){
    document.querySelectorAll('.drop-zone').forEach(z=>{
      z.dataset.initialized='true';
      z.addEventListener('dragover',e=>{e.preventDefault();e.currentTarget.classList.add('drag-over');});
      z.addEventListener('dragleave',e=>{if(e.currentTarget===e.target)e.currentTarget.classList.remove('drag-over');});
      z.addEventListener('drop',e=>{e.preventDefault();e.currentTarget.classList.remove('drag-over');
        addFieldToZone(e.dataTransfer.getData('field'),e.currentTarget.dataset.zone);});
    });

    // Setup control event listeners
    document.getElementById('agg-select').addEventListener('change',function(){
      document.getElementById('weight-group').style.display=this.value==='weighted_mean'?'block':'none';
      scheduleUpdate();
    });
    document.getElementById('values-select').addEventListener('change',scheduleUpdate);
    document.getElementById('weight-select').addEventListener('change',scheduleUpdate);
    document.getElementById('chart-type').addEventListener('change',()=>{updateAxisControlsState();updateChart();});
    document.getElementById('x-min').addEventListener('change',updateChart);
    document.getElementById('x-max').addEventListener('change',updateChart);
    document.getElementById('y-min').addEventListener('change',updateChart);
    document.getElementById('y-max').addEventListener('change',updateChart);
    document.getElementById('show-legend').addEventListener('change',updateChart);
  }
}

function updateAxisControlsState(){
  const ct=document.getElementById('chart-type').value;
  const isHorizontalBar=ct==='horizontalBar';
  const isLine=ct==='line';

  // Horizontal bar: X controls enabled (values), Y controls disabled (categories)
  // Column bar: Y controls enabled (values), X controls disabled (categories)
  // Line: BOTH enabled (both axes are numeric)
  document.getElementById('x-min-group').classList.toggle('disabled',!isHorizontalBar&&!isLine);
  document.getElementById('x-max-group').classList.toggle('disabled',!isHorizontalBar&&!isLine);
  document.getElementById('y-min-group').classList.toggle('disabled',isHorizontalBar);
  document.getElementById('y-max-group').classList.toggle('disabled',isHorizontalBar);
}

function addFieldToZone(f,z){
  if(z==='filters'){
    if(pivotConfig.filters[f])return;
    const uv=new Set();CURRENT_DATA.forEach(r=>uv.add(String(r[f]||'')));
    pivotConfig.filters[f]=uv;
  }else{if(pivotConfig[z].includes(f))return;pivotConfig[z].push(f);}
  renderZone(z);scheduleUpdate();
}

function removeFieldFromZone(f,z){
  if(z==='filters')delete pivotConfig.filters[f];else pivotConfig[z]=pivotConfig[z].filter(x=>x!==f);
  renderZone(z);scheduleUpdate();
}

function renderZone(z){
  const c=document.getElementById(`${z}-content`);c.innerHTML='';
  if(z==='filters'){
    Object.keys(pivotConfig.filters).forEach(f=>{
      const fc=document.createElement('div');fc.style.marginBottom='12px';
      const fh=document.createElement('div');fh.className='zone-item';
      fh.innerHTML=`<span><strong>${f}</strong></span><span class="remove-btn" onclick="removeFieldFromZone('${f}','${z}')">×</span>`;
      fc.appendChild(fh);
      const av=new Set();CURRENT_DATA.forEach(r=>av.add(String(r[f]||'')));
      const sv=Array.from(av).sort((a,b)=>{const an=parseFloat(a),bn=parseFloat(b);return !isNaN(an)&&!isNaN(bn)?an-bn:a.localeCompare(b);});
      const fct=document.createElement('div');fct.className='filter-controls';
      fct.innerHTML=`<button onclick="selectAllFilterValues('${f}')">Select All</button><button onclick="deselectAllFilterValues('${f}')">Deselect All</button>`;
      fc.appendChild(fct);
      const vl=document.createElement('div');vl.className='filter-values';
      sv.forEach(v=>{
        const vi=document.createElement('div');vi.className='filter-value-item';vi.textContent=v;
        if(pivotConfig.filters[f].has(v))vi.classList.add('selected');
        vi.addEventListener('click',()=>toggleFilterValue(f,v));
        vl.appendChild(vi);
      });
      fc.appendChild(vl);c.appendChild(fc);
    });
  }else{
    pivotConfig[z].forEach(f=>{
      const zi=document.createElement('div');zi.className='zone-item';
      zi.innerHTML=`<span>${f}</span><span class="remove-btn" onclick="removeFieldFromZone('${f}','${z}')">×</span>`;
      c.appendChild(zi);
    });
  }
}

function toggleFilterValue(f,v){const s=pivotConfig.filters[f];s.has(v)?s.delete(v):s.add(v);renderZone('filters');scheduleUpdate();}
function selectAllFilterValues(f){const a=new Set();CURRENT_DATA.forEach(r=>a.add(String(r[f]||'')));pivotConfig.filters[f]=a;renderZone('filters');scheduleUpdate();}
function deselectAllFilterValues(f){pivotConfig.filters[f]=new Set();renderZone('filters');scheduleUpdate();}

function scheduleUpdate(){clearTimeout(updateTimer);updateTimer=setTimeout(generatePivot,300);}

function generatePivot(){
  if(!CURRENT_DATA)return;
  const rf=pivotConfig.rows,cf=pivotConfig.columns;
  if(rf.length===0&&cf.length===0){document.getElementById('result-info').textContent='Add fields to Rows or Columns.';return;}
  showLoading(true,'Generating pivot table...',50);
  setTimeout(()=>{
    const vf=document.getElementById('values-select').value,ag=document.getElementById('agg-select').value,wf=document.getElementById('weight-select').value;
    let fd=CURRENT_DATA;
    Object.keys(pivotConfig.filters).forEach(f=>{const sv=pivotConfig.filters[f];if(sv.size>0)fd=fd.filter(r=>sv.has(String(r[f]||'')));});
    const pv=buildPivot(fd,rf,cf,vf,ag,wf);
    renderPivot(pv,rf,cf,vf,ag);
    PIVOT_RESULTS=pv;
    updateChart();
    showLoading(false);
  },10);
}

function buildPivot(d,rf,cf,vf,ag,wf){
  const pv={},ck=new Set(),hr=rf.length>0,hc=cf.length>0;
  d.forEach(r=>{
    const rk=hr?rf.map(f=>{const v=r[f];return v===null||v===undefined||v===''?'(blank)':String(v);}).join(' | '):'(Total)';
    const ck2=hc?cf.map(f=>{const v=r[f];return v===null||v===undefined||v===''?'(blank)':String(v);}).join(' | '):'(Value)';
    ck.add(ck2);if(!pv[rk])pv[rk]={};if(!pv[rk][ck2])pv[rk][ck2]={values:[],weights:[]};
    const v=r[vf];if(v!==null&&v!==undefined&&v!==''){pv[rk][ck2].values.push(parseFloat(v));
    if(ag==='weighted_mean')pv[rk][ck2].weights.push(parseFloat(r[wf])||0);}
  });
  Object.keys(pv).forEach(rk=>Object.keys(pv[rk]).forEach(ck=>{const c=pv[rk][ck];c.result=aggregate(c.values,c.weights,ag);}));
  const ca=Array.from(ck).sort((a,b)=>{const af=a.split(' | ')[0],bf=b.split(' | ')[0],an=parseFloat(af),bn=parseFloat(bf);
    return !isNaN(an)&&!isNaN(bn)?an-bn:a.localeCompare(b);});
  return {data:pv,colKeys:ca};
}

function aggregate(v,w,f){
  if(v.length===0)return null;
  switch(f){
    case'mean':return v.reduce((a,b)=>a+b,0)/v.length;
    case'weighted_mean':if(w.length===0)return null;const tw=w.reduce((a,b)=>a+b,0);return tw===0?null:v.reduce((s,val,i)=>s+(val*w[i]),0)/tw;
    case'sum':return v.reduce((a,b)=>a+b,0);
    case'count':return v.length;
    default:return null;
  }
}

function renderPivot(pv,rf,cf,vf,ag){
  const {data,colKeys}=pv,rk=Object.keys(data).sort((a,b)=>{const af=a.split(' | ')[0],bf=b.split(' | ')[0],an=parseFloat(af),bn=parseFloat(bf);
    return !isNaN(an)&&!isNaN(bn)?an-bn:a.localeCompare(b);});
  if(colKeys.length>200){alert(`Too many columns (${colKeys.length}). Limiting to 200.`);colKeys.splice(200);}
  if(rk.length>2000){alert(`Too many rows (${rk.length}). Limiting to 2000.`);rk.splice(2000);}
  const th=document.getElementById('pivot-thead');
  let hh='<tr><th>'+(rf.length>0?rf.join('<br>'):'')+'</th>';
  colKeys.forEach(c=>hh+=`<th>${c.replace(/\\|/g,'<br>')}</th>`);hh+='</tr>';th.innerHTML=hh;
  const tb=document.getElementById('pivot-tbody');let bh='';
  rk.forEach(r=>{bh+=`<tr><td><strong>${r.replace(/\\|/g,' | ')}</strong></td>`;
    colKeys.forEach(c=>{const cl=data[r][c];bh+=cl&&cl.result!==null?`<td>${formatNumber(cl.result)}</td>`:'<td>—</td>';});
    bh+='</tr>';});
  tb.innerHTML=bh;
  document.getElementById('result-info').textContent=`Table: ${rk.length} rows × ${colKeys.length} columns. Agg: ${ag}. Value: ${vf}.`;
  document.getElementById('chart-section').style.display='block';
}

function formatNumber(n){
  if(n===null||n===undefined)return'—';if(Number.isInteger(n))return n.toLocaleString();
  if(Math.abs(n)<0.01)return n.toExponential(2);if(Math.abs(n)<1)return n.toFixed(4);
  if(Math.abs(n)<100)return n.toFixed(2);return n.toFixed(1);
}

function updateChart(){
  if(!PIVOT_RESULTS)return;
  const {data,colKeys}=PIVOT_RESULTS;

  // Get row keys and sort them - detect if all are numeric
  let rk=Object.keys(data);

  // Check if all row keys (first element after split) are numeric
  const allNumeric=rk.every(k=>{
    const firstPart=k.split(' | ')[0];
    return !isNaN(parseFloat(firstPart))&&isFinite(firstPart);
  });

  // Sort numerically if all numeric, otherwise alphabetically
  if(allNumeric){
    rk.sort((a,b)=>{
      const an=parseFloat(a.split(' | ')[0]);
      const bn=parseFloat(b.split(' | ')[0]);
      return an-bn;
    });
  }else{
    rk.sort((a,b)=>a.localeCompare(b));
  }

  const ct=document.getElementById('chart-type').value;
  const xm=document.getElementById('x-min').value,xM=document.getElementById('x-max').value;
  const ym=document.getElementById('y-min').value,yM=document.getElementById('y-max').value;
  const sl=document.getElementById('show-legend').checked;

  const datasets=colKeys.map((c,i)=>({label:c,data:rk.map(r=>data[r][c]?.result||null),
    backgroundColor:`hsl(${(i*360/colKeys.length)},70%,60%)`,borderColor:`hsl(${(i*360/colKeys.length)},70%,50%)`,
    borderWidth:2,fill:false}));

  if(CHART_INSTANCE)CHART_INSTANCE.destroy();
  const ctx=document.getElementById('pivot-chart').getContext('2d');

  const isHorizontalBar=ct==='horizontalBar';
  const isLine=ct==='line';

  // For line charts with numeric x-axis, use linear scale instead of category
  const xAxisConfig={
    type:isLine&&allNumeric?'linear':'category',
    grid:{color:'#3a404a',borderColor:'#3a404a'},
    min:isHorizontalBar&&xm?parseFloat(xm):isLine&&allNumeric&&xm?parseFloat(xm):undefined,
    max:isHorizontalBar&&xM?parseFloat(xM):isLine&&allNumeric&&xM?parseFloat(xM):undefined
  };

  const yAxisConfig={
    grid:{color:'#3a404a',borderColor:'#3a404a'},
    beginAtZero:false,
    min:!isHorizontalBar&&ym?parseFloat(ym):undefined,
    max:!isHorizontalBar&&yM?parseFloat(yM):undefined
  };

  // For line charts with numeric x-axis, convert labels to numbers
  let labels=rk;
  let chartDatasets=datasets;
  if(isLine&&allNumeric){
    const numericLabels=rk.map(k=>parseFloat(k.split(' | ')[0]));
    chartDatasets=colKeys.map((c,i)=>({
      label:c,
      data:rk.map((r,idx)=>({x:numericLabels[idx],y:data[r][c]?.result||null})),
      backgroundColor:`hsl(${(i*360/colKeys.length)},70%,60%)`,
      borderColor:`hsl(${(i*360/colKeys.length)},70%,50%)`,
      borderWidth:2,
      fill:false
    }));
  }

  CHART_INSTANCE=new Chart(ctx,{
    type:ct==='horizontalBar'?'bar':ct,
    data:{labels:isLine&&allNumeric?undefined:labels,datasets:chartDatasets},
    options:{
      indexAxis:isHorizontalBar?'y':'x',
      responsive:true,
      maintainAspectRatio:false,
      plugins:{legend:{display:sl}},
      scales:{x:xAxisConfig,y:yAxisConfig}
    }
  });

  updateAxisControlsState();
}

function resetChart(){
  resetChartSettings();
  updateChart();
}
</script>
</body>
</html>
"""

with open('analysis_pivot_v6.html', 'w', encoding='utf-8') as f:
    f.write(html_content)

print("✓ Generated analysis_pivot_v6.html")
print("\nFixes:")
print("  1. Chart x-axis numeric sorting:")
print("     - Detects if all row labels are numeric (like years.elapsed, months.elapsed)")
print("     - Sorts numerically: 0, 1, 2, ..., 10, 11 (not 0, 10, 11, ..., 2)")
print("     - For line charts with numeric x-axis, uses linear scale with x/y data points")
print("  2. Line chart axis controls:")
print("     - Both X and Y axis min/max controls enabled in line chart mode")
print("     - Only horizontal bar mode disables Y-axis controls")
print("     - Only column mode disables X-axis controls")
print("  3. Dataset switching:")
print("     - All chart settings (type, axis limits, legend) reset to defaults")
print("     - resetChartSettings() called during dataset load")
print("     - Settings don't persist across dataset changes")
