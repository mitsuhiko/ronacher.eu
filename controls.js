/* The picture's buttons.  A tap goes on to the next place, season, weather or theme; a long press (or the
   little arrow beside a button, where there is a mouse) fans out everything it can be set to, and one is
   picked by sliding onto it and letting go, or by a tap.  While a fan is out the picture holds still. */
(() => {
'use strict';

const nav = document.querySelector('.controls');
const v = window.vienna;
if (!nav || !v || !v.choices) return;
const qs = new URLSearchParams(location.search);
const REDUCED = !!(window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches) && !qs.has('motion');
const PRESS_MS = 380, SLOP = 10;
const NAMES = { place: 'place', season: 'season', weather: 'weather', theme: 'color theme' };
const controls = Array.from(nav.querySelectorAll('.control'));

// ---------- the little arrow, for those with a mouse ----------
for (const control of controls) {
  const more = document.createElement('button');
  more.type = 'button'; more.className = 'more';
  more.setAttribute('aria-haspopup', 'menu'); more.setAttribute('aria-expanded', 'false');
  more.setAttribute('aria-label', 'Choose the ' + NAMES[control.dataset.kind]);
  more.innerHTML = '<svg viewBox="0 0 12 8" aria-hidden="true"><path d="M1.6,2.2 C3.4,3.6 4.7,4.9 6.1,6.1 C7.4,4.8 8.8,3.5 10.4,2"/></svg>';
  control.appendChild(more);
}

// ---------- room for the longest name ----------
// Season, weather and theme keep the width of the longest thing they can say, so the picture changing
// on its own does not push its neighbours about.  The place is on the outside (or on a line of its own
// on phones), so it may be as long as its name.
function reserve() {
  for (const control of controls) {
    const kind = control.dataset.kind, button = control.querySelector('.value');
    if (kind === 'place' || !v.labels) continue;
    const probe = button.cloneNode(false);
    probe.removeAttribute('id');
    probe.style.cssText = 'position:absolute;visibility:hidden;min-width:0;width:auto;';
    control.appendChild(probe);
    let widest = 0;
    for (const label of v.labels(kind)) { probe.textContent = label; widest = Math.max(widest, probe.getBoundingClientRect().width); }
    probe.remove();
    // the box of a turned element is a little bigger than the element itself; the tilt is slight enough to ignore
    button.style.minWidth = Math.ceil(widest) + 'px';
  }
}
reserve();
if (document.fonts && document.fonts.ready) document.fonts.ready.then(reserve);

// ---------- the fan ----------
let open = null;                 // { control, kind, fan, items, down }
let press = null;                // { id, x, y, control, timer, fired }
let swallowUntil = 0;            // the click that ends a long press, or a tap that only closes a fan, does nothing else

const remPx = () => parseFloat(getComputedStyle(document.documentElement).fontSize) || 16;

function fanOut(control, focus) {
  close();
  const kind = control.dataset.kind, list = v.choices(kind);
  if (!list.length) return;
  const rect = control.getBoundingClientRect(), down = rect.top + rect.height / 2 < innerHeight / 2;
  const fan = document.createElement('div');
  fan.className = 'fan ' + (down ? 'down' : 'up');
  fan.setAttribute('role', 'menu');
  fan.setAttribute('aria-label', 'Choose the ' + NAMES[kind]);
  const items = list.map((c, i) => {
    const b = document.createElement('button');
    b.type = 'button'; b.className = 'fan-item' + (c.current ? ' current' : '');
    b.textContent = c.label; b.dataset.value = c.value;
    b.setAttribute('role', 'menuitemradio'); b.setAttribute('aria-checked', String(c.current));
    b.tabIndex = -1;
    b.style.setProperty('--i', i);
    fan.appendChild(b);
    return b;
  });
  control.appendChild(fan);
  layout(control, items, down, rect);

  control.classList.add('open');
  control.querySelector('.more').setAttribute('aria-expanded', 'true');
  v.pause(true);
  open = { control, kind, fan, items, down };
  fan.getBoundingClientRect();   // so that the fan opens from the button instead of just being there
  fan.classList.add('out');
  if (focus) (items.find(b => b.classList.contains('current')) || items[0]).focus({ preventScroll: true });
}

// Down from the top of the screen, up from the bottom, along a wide arc that bends away from the edge;
// each tag is turned along the arc's radius.  When the screen is too short for all of them, the rest go
// on a second arc further in.
function layout(control, items, down, rect) {
  const rem = remPx(), gap = .45 * rem, step = 1.95 * rem, R = 56 * rem, margin = .6 * rem;
  const h = items[0].offsetHeight, ch = control.offsetHeight;
  // on phones the place has a line of its own: the fans of the others start beyond it, not over it
  const all = nav.getBoundingClientRect(), past = Math.max(0, down ? all.bottom - rect.bottom : rect.top - all.top);
  const room = down ? innerHeight - all.bottom - margin : all.top - margin;
  const perArc = Math.max(3, Math.floor((room - gap - h) / step) + 1);
  let shift = 0, widest = 0;
  items.forEach((b, i) => {
    const arc = Math.floor(i / perArc), j = i % perArc;
    if (j === 0 && arc > 0) { shift += widest + .8 * rem; widest = 0; }
    widest = Math.max(widest, b.offsetWidth);
    const d = past + gap + j * step + h / 2;                     // how far from the button, along the arc
    const th = Math.asin(Math.min(.9, d / R));
    const x = -(R - R * Math.cos(th)) - shift;
    const y = down ? ch + d - h / 2 : -d - h / 2;
    const r = (down ? th : -th) * 180 / Math.PI + ((i * 7) % 5 - 2) * .35;   // and a little of a hand's unevenness
    b.style.setProperty('--x', x.toFixed(1) + 'px');
    b.style.setProperty('--y', y.toFixed(1) + 'px');
    b.style.setProperty('--r', r.toFixed(2) + 'deg');
  });
}

function close(refocus) {
  if (!open) return;
  const { control, fan } = open;
  open = null;
  control.classList.remove('open');
  const more = control.querySelector('.more');
  more.setAttribute('aria-expanded', 'false');
  fan.classList.remove('out');
  fan.classList.add('in');
  setTimeout(() => fan.remove(), REDUCED ? 0 : 260);
  v.pause(false);
  if (refocus) (getComputedStyle(more).display !== 'none' ? more : control.querySelector('.value')).focus({ preventScroll: true });
}

function pick(item) {
  if (!open) return;
  const kind = open.kind, value = item.dataset.value;
  item.classList.add('picked');
  close(false);
  v.choose(kind, value);
}

function itemAt(x, y) {
  if (!open) return null;
  const el = document.elementFromPoint(x, y);
  return el && el.closest && open.fan.contains(el) ? el.closest('.fan-item') : null;
}

function hot(item) {
  if (!open) return;
  for (const b of open.items) b.classList.toggle('hot', b === item);
}

// ---------- pressing, sliding, letting go ----------
nav.addEventListener('pointerdown', e => {
  const button = e.target.closest('.value');
  if (!button || (e.pointerType === 'mouse' && e.button !== 0)) return;
  const control = button.closest('.control');
  if (open && open.control === control) { close(); swallowUntil = performance.now() + 800; return; }
  cancelPress();
  press = { id: e.pointerId, x: e.clientX, y: e.clientY, control, fired: false };
  press.timer = setTimeout(() => {
    press.fired = true;
    swallowUntil = performance.now() + 1500;
    fanOut(control, false);
    if (navigator.vibrate) try { navigator.vibrate(8); } catch (err) { /* not allowed */ }
  }, PRESS_MS);
});

function cancelPress() {
  if (press) clearTimeout(press.timer);
  press = null;
}

document.addEventListener('pointermove', e => {
  if (!press || e.pointerId !== press.id) return;
  if (!press.fired) {
    if (Math.hypot(e.clientX - press.x, e.clientY - press.y) > SLOP) cancelPress();
    return;
  }
  hot(itemAt(e.clientX, e.clientY));
});

document.addEventListener('pointerup', e => {
  if (!press || e.pointerId !== press.id) return;
  const fired = press.fired;
  cancelPress();
  if (!fired) return;
  const item = itemAt(e.clientX, e.clientY);
  if (item) pick(item); else hot(null);                        // let go elsewhere: the fan stays out for a tap
});

document.addEventListener('pointercancel', e => {
  if (press && e.pointerId === press.id) { cancelPress(); hot(null); }
});

// A press anywhere else puts the fan away, and does nothing more.
document.addEventListener('pointerdown', e => {
  if (!open || open.control.contains(e.target)) return;
  close();
  if (!(e.target.closest && e.target.closest('.more'))) swallowUntil = performance.now() + 800;
}, true);

document.addEventListener('click', e => {
  if (performance.now() < swallowUntil) {
    swallowUntil = 0;
    e.preventDefault();
    e.stopImmediatePropagation();
    return;
  }
  const item = e.target.closest && e.target.closest('.fan-item');
  if (item && open && open.fan.contains(item)) { pick(item); return; }
  const more = e.target.closest && e.target.closest('.more');
  if (more) {
    const control = more.closest('.control');
    if (open && open.control === control) close(true); else fanOut(control, true);
  }
}, true);

nav.addEventListener('contextmenu', e => e.preventDefault());

// ---------- keys ----------
nav.addEventListener('keydown', e => {
  const value = e.target.closest('.value');
  if (value && !open && (e.key === 'ArrowDown' || e.key === 'ArrowUp')) {
    e.preventDefault();
    fanOut(value.closest('.control'), true);
    return;
  }
  if (!open || !open.fan.contains(e.target)) return;
  const items = open.items, at = items.indexOf(e.target);
  const onward = open.down ? 'ArrowDown' : 'ArrowUp', back = open.down ? 'ArrowUp' : 'ArrowDown';
  let to = -1;
  if (e.key === onward) to = Math.min(items.length - 1, at + 1);
  else if (e.key === back) to = Math.max(0, at - 1);
  else if (e.key === 'Home') to = 0;
  else if (e.key === 'End') to = items.length - 1;
  else if (e.key === 'Escape') { e.preventDefault(); close(true); return; }
  else if (e.key === 'Tab') { close(false); return; }
  if (to >= 0) { e.preventDefault(); items[to].focus({ preventScroll: true }); }
});

let wide = innerWidth;                                         // phones resize as their bars come and go: only a new width counts
window.addEventListener('resize', () => {
  if (innerWidth === wide) return;
  wide = innerWidth; close(); reserve();
});
})();
