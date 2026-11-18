// ---------------------------------------------------------------------------
// Poster preview: scale-to-fit, click-to-edit, drag-to-move, drag-to-resize.
// ---------------------------------------------------------------------------

function fitPoster(){
  document.querySelectorAll('.poster-wrap').forEach(function(w){
    var p = w.querySelector('.poster'); if(!p) return;
    var s = w.clientWidth / p.offsetWidth;
    p.style.transform = 'scale(' + s + ')';
    p.dataset.scale = s;
    w.style.height = (p.offsetHeight * s) + 'px';
  });
}
window.addEventListener('resize', fitPoster);
$(document).on('shiny:value', function(e){ if(e.name === 'preview') setTimeout(fitPoster, 0); });

// Scroll the editor to a block's card (sent from the server after a click)
Shiny.addCustomMessageHandler('scroll_card', function(id){
  setTimeout(function(){
    var el = document.getElementById('card_' + id);
    if(el){ el.scrollIntoView({behavior:'smooth', block:'center'}); el.classList.add('flash'); setTimeout(function(){ el.classList.remove('flash'); }, 1200); }
  }, 250);
});

(function(){
  var drag = null;   // current drag state

  function poster(){ return document.querySelector('.poster'); }
  function scale(){ var p = poster(); return p ? parseFloat(p.dataset.scale || 1) : 1; }
  function toPoster(ev){            // client coords -> unscaled poster px
    var r = poster().getBoundingClientRect(), s = scale();
    return {x: (ev.clientX - r.left) / s, y: (ev.clientY - r.top) / s};
  }
  function rectOf(el){ return {x: el.offsetLeft, y: el.offsetTop, w: el.offsetWidth, h: el.offsetHeight}; }
  function inside(pt, r){ return pt.x >= r.x && pt.x <= r.x + r.w && pt.y >= r.y && pt.y <= r.y + r.h; }
  function mk(cls){ var d = document.createElement('div'); d.className = cls; poster().appendChild(d); return d; }
  function place(el, r){ el.style.left = r.x + 'px'; el.style.top = r.y + 'px'; el.style.width = r.w + 'px'; el.style.height = r.h + 'px'; }

  // Work out where a dragged block would land
  function dropTarget(pt, selfId){
    var panels = Array.from(document.querySelectorAll('.ppanel'));
    var panel = panels.find(function(p){ return inside(pt, rectOf(p)); });
    if(!panel) return null;
    var key = panel.dataset.key;
    var items = Array.from(document.querySelectorAll('.pitem[data-key="' + key + '"]'))
                     .filter(function(i){ return i.dataset.id !== selfId; })
                     .sort(function(a, b){ return a.offsetTop - b.offsetTop; });
    for (var i = 0; i < items.length; i++){
      var r = rectOf(items[i]);
      if (inside(pt, r)){
        var rel = (pt.y - r.y) / r.h;
        if (rel < 0.3) return {mode:'before', target: items[i].dataset.id, key: key, line: r.y - 4, panel: panel};
        if (rel > 0.7) return {mode:'after', target: items[i].dataset.id, key: key, line: r.y + r.h + 4, panel: panel};
        return {mode:'swap', target: items[i].dataset.id, key: key, rect: r, panel: panel};
      }
    }
    for (var j = 0; j < items.length; j++){
      var rj = rectOf(items[j]);
      if (pt.y < rj.y + rj.h / 2) return {mode:'before', target: items[j].dataset.id, key: key, line: rj.y - 4, panel: panel};
    }
    if (items.length){ var last = rectOf(items[items.length - 1]);
      return {mode:'after', target: items[items.length - 1].dataset.id, key: key, line: last.y + last.h + 4, panel: panel}; }
    var pr = rectOf(panel);
    return {mode:'append', target: null, key: key, line: pr.y + 90, panel: panel};
  }

  document.addEventListener('mousedown', function(ev){
    if (ev.button !== 0) return;
    var h = ev.target.closest('.phandle'), it = ev.target.closest('.pitem');
    if (!h && !it) return;
    ev.preventDefault();
    var pt = toPoster(ev);
    if (h){
      drag = {kind:'resize', id:h.dataset.id, k:parseFloat(h.dataset.k), size:parseFloat(h.dataset.size),
              fw:parseFloat(h.dataset.fw), fh:parseFloat(h.dataset.fh), fx:parseFloat(h.dataset.fx), fy:parseFloat(h.dataset.fy),
              start:pt, moved:false};
      drag.box = mk('presize'); place(drag.box, {x:drag.fx, y:drag.fy, w:drag.fw, h:drag.fh});
      drag.label = mk('presize-label');
    } else {
      drag = {kind:'move', id:it.dataset.id, key:it.dataset.key, start:pt, moved:false, src:rectOf(it), el:it};
    }
  });

  document.addEventListener('mousemove', function(ev){
    if (!drag) return;
    var pt = toPoster(ev), dx = pt.x - drag.start.x, dy = pt.y - drag.start.y;
    if (!drag.moved && Math.abs(dx) + Math.abs(dy) < 5) return;
    drag.moved = true;
    if (drag.kind === 'resize'){
      var k = drag.k, nw = Math.max(20, drag.fw + Math.abs(k) * dx * (k < 0 ? -1 : 1));
      var nh = drag.fh * nw / drag.fw;
      var nx = k === 2 ? drag.fx + drag.fw / 2 - nw / 2 : (k < 0 ? drag.fx + drag.fw - nw : drag.fx);
      place(drag.box, {x:nx, y:drag.fy, w:nw, h:nh});
      drag.newSize = Math.round(drag.size * nw / drag.fw / 5) * 5;
      drag.label.textContent = drag.newSize + '%';
      drag.label.style.left = (nx + nw / 2 - 30) + 'px'; drag.label.style.top = (drag.fy + nh / 2 - 14) + 'px';
      return;
    }
    if (!drag.ghost){ drag.ghost = mk('pghost'); drag.line = mk('pdropline'); drag.swapbox = mk('pswap'); drag.el.classList.add('dragging'); }
    place(drag.ghost, {x:drag.src.x + dx, y:drag.src.y + dy, w:drag.src.w, h:drag.src.h});
    var t = dropTarget(pt, drag.id); drag.target = t;
    drag.line.style.display = 'none'; drag.swapbox.style.display = 'none';
    if (t && t.mode === 'swap'){ drag.swapbox.style.display = 'block'; place(drag.swapbox, t.rect); }
    else if (t){ var pr = rectOf(t.panel); drag.line.style.display = 'block'; place(drag.line, {x:pr.x + 12, y:t.line, w:pr.w - 24, h:5}); }
  });

  document.addEventListener('mouseup', function(ev){
    if (!drag) return;
    var d = drag; drag = null;
    ['ghost','line','swapbox','box','label'].forEach(function(n){ if (d[n]) d[n].remove(); });
    if (d.el) d.el.classList.remove('dragging');
    if (!d.moved){        // plain click: open that block's editor
      if (d.kind === 'move') Shiny.setInputValue('preview_block', {id:d.id, key:d.key}, {priority:'event'});
      return;
    }
    if (d.kind === 'resize'){
      if (d.newSize && d.newSize !== d.size) Shiny.setInputValue('drag_resize', {id:d.id, size:d.newSize}, {priority:'event'});
    } else if (d.target){
      Shiny.setInputValue('drag_move', {id:d.id, mode:d.target.mode, target:d.target.target, key:d.target.key}, {priority:'event'});
    }
  });
})();

Shiny.addCustomMessageHandler('run_js', function(code){ try { (new Function(code))(); } catch(e) { console.error(e); } });

// First-visit welcome card (remembered per browser)
function dismissWelcome(){ var w = document.getElementById('welcome_card'); if (w) w.style.display = 'none'; try { localStorage.setItem('posterWelcomeSeen', '1'); } catch(e) {} }
$(document).on('shiny:value', function(e){
  if (e.name !== 'welcome') return;
  setTimeout(function(){ var seen = false; try { seen = localStorage.getItem('posterWelcomeSeen') === '1'; } catch(e) {}
    var w = document.getElementById('welcome_card'); if (w && !seen) w.style.display = 'block'; }, 0);
});

// ---------------------------------------------------------------------------
// Editable table grid (Table block, "Type it in")
// ---------------------------------------------------------------------------
(function(){
  function esc(t){ return String(t).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }
  function initGrid(el){
    if (el.dataset.ready) return; el.dataset.ready = '1';
    var data; try { data = JSON.parse(el.dataset.grid || '[]'); } catch(e) { data = []; }
    if (!data.length) data = [['',''],['',''],['','']];
    var timer = null;
    function send(){ clearTimeout(timer); timer = setTimeout(function(){ Shiny.setInputValue(el.id, JSON.stringify(data)); }, 350); }
    function width(){ return Math.max.apply(null, data.map(function(r){ return r.length; }).concat([1])); }
    function pad(){ var w = width(); data.forEach(function(r){ while (r.length < w) r.push(''); }); }
    function render(focus){
      pad();
      var h = '<table class="tgrid-table">';
      data.forEach(function(r, i){
        h += '<tr' + (i === 0 ? ' class="first"' : '') + '>';
        r.forEach(function(c, j){ h += '<td contenteditable="true" data-r="' + i + '" data-c="' + j + '">' + esc(c) + '</td>'; });
        h += '</tr>';
      });
      h += '</table><div class="tgrid-btns">' +
           '<button type="button" class="btn btn-light btn-sm" data-act="addr">+ Row</button>' +
           '<button type="button" class="btn btn-light btn-sm" data-act="addc">+ Column</button>' +
           '<button type="button" class="btn btn-light btn-sm" data-act="delr">− Row</button>' +
           '<button type="button" class="btn btn-light btn-sm" data-act="delc">− Column</button></div>';
      el.innerHTML = h;
      if (focus){ var td = el.querySelector('td[data-r="' + focus[0] + '"][data-c="' + focus[1] + '"]'); if (td) td.focus(); }
    }
    el.addEventListener('input', function(e){
      var td = e.target.closest('td'); if (!td) return;
      data[+td.dataset.r][+td.dataset.c] = td.innerText.replace(/\n/g, ' ').trim();
      send();
    });
    el.addEventListener('keydown', function(e){
      var td = e.target.closest('td'); if (!td || e.key !== 'Enter') return;
      e.preventDefault();
      var r = +td.dataset.r + 1, c = +td.dataset.c;
      if (r >= data.length) { data.push(new Array(width()).fill('')); send(); }
      render([r, c]);
    });
    el.addEventListener('paste', function(e){
      var td = e.target.closest('td'); if (!td) return;
      var text = (e.clipboardData || window.clipboardData).getData('text');
      if (!/[\t\n]/.test(text.replace(/\n+$/, ''))) return;      // single cell: normal paste
      e.preventDefault();
      var rows = text.replace(/\r/g, '').replace(/\n+$/, '').split('\n').map(function(l){ return l.split('\t'); });
      var r0 = +td.dataset.r, c0 = +td.dataset.c;
      rows.forEach(function(row, i){
        while (data.length <= r0 + i) data.push([]);
        row.forEach(function(v, j){ var R = data[r0 + i]; while (R.length <= c0 + j) R.push(''); R[c0 + j] = v.trim(); });
      });
      render(); send();
    });
    el.addEventListener('click', function(e){
      var b = e.target.closest('button[data-act]'); if (!b) return;
      var act = b.dataset.act;
      if (act === 'addr') data.push(new Array(width()).fill(''));
      if (act === 'addc') data.forEach(function(r){ r.push(''); });
      if (act === 'delr' && data.length > 1) data.pop();
      if (act === 'delc' && width() > 1) data.forEach(function(r){ r.length = width() - 1; });
      render(); send();
    });
    render();
  }
  function scan(){ document.querySelectorAll('.tgrid').forEach(initGrid); }
  new MutationObserver(scan).observe(document.documentElement, {childList: true, subtree: true});
  $(document).on('shiny:value', function(){ setTimeout(scan, 0); });
})();
