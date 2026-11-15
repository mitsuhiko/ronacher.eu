/* The postcard: name and message are written in, word by word, and can be put away. */
(() => {
'use strict';

const card = document.querySelector('.postcard');
if (!card) return;
const qs = new URLSearchParams(location.search);
const REDUCED = !!(window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches) && !qs.has('motion');
const STILL = REDUCED || qs.has('still');

// ---------- the message: every word gets its turn ----------
const CHAR_MS = 7;
function wrapWords(node, clock) {
  for (const child of Array.from(node.childNodes)) {
    if (child.nodeType === 3) {
      const parts = child.nodeValue.split(/(\s+)/), frag = document.createDocumentFragment();
      for (const part of parts) {
        if (!part) continue;
        if (/^\s+$/.test(part)) { frag.appendChild(document.createTextNode(' ')); continue; }
        const w = document.createElement('span'), d = Math.max(32, part.length * CHAR_MS);
        w.className = 'w'; w.textContent = part;
        w.style.setProperty('--t', Math.round(clock.t) + 'ms'); w.style.setProperty('--d', d + 'ms');
        clock.t += d + 8;
        frag.appendChild(w);
      }
      node.replaceChild(frag, child);
    } else if (child.nodeType === 1) {
      wrapWords(child, clock);
      if (/^(P|LI)$/.test(child.tagName)) clock.t += 70;
    }
  }
}
// Name and message are written at the same time and are done together.  Each block is
// stretched to end then; the short tagline simply starts late.
const WRITE_SECONDS = 2.8;
[['h1', 0], ['.tagline', .9], ['.message', 0], ['.address', 0]].forEach(([sel, own]) => {
  const el = card.querySelector(sel); if (!el) return;
  const clock = { t: 0 }; wrapWords(el, clock);
  const span = (own || WRITE_SECONDS) * 1000;
  el.style.setProperty('--k', (span / Math.max(clock.t, 1)).toFixed(3));
  el.style.setProperty('--o', Math.round(WRITE_SECONDS * 1000 - span) + 'ms');
});

// ---------- the sequence ----------
let timers = [];
function finishWriting() { card.classList.remove('writing'); card.classList.add('written'); }
function begin() {
  card.classList.add('in');
  if (STILL) { finishWriting(); return; }
  timers.push(setTimeout(() => {
    card.classList.add('writing');
    timers.push(setTimeout(finishWriting, WRITE_SECONDS * 1000 + 300));
  }, 600));
}
function skip() {                                              // impatient? a click on the card finishes it
  timers.forEach(clearTimeout); timers = [];
  card.classList.add('in');
  finishWriting();
}
card.addEventListener('click', e => {
  if (e.target.closest('a, button')) return;
  if (!card.classList.contains('written')) skip();
  if (peeking()) pullUp();
});

// ---------- small screens: the card tucked in at the foot of the picture (?card=peek, the default) ----------
const root = document.documentElement;
const SMALL = window.matchMedia ? matchMedia('only screen and (max-width: 700px), only screen and (max-height: 500px)') : { matches: false };
const PEEK = root.classList.contains('card-peek');
const SMOOTH = REDUCED ? 'auto' : 'smooth';
function peeking() { return PEEK && SMALL.matches && card.getBoundingClientRect().bottom > window.innerHeight + 4; }
function pullUp() { window.scrollTo({ top: root.scrollHeight, behavior: SMOOTH }); }

// ---------- ?card=flip: the message on one side, the addresses on the other ----------
const turnButton = card.querySelector('.card-turn');
let turning = false;
function turnOver() {
  if (turning) return;
  const turned = !card.classList.contains('turned');
  const swap = () => card.classList.toggle('turned', turned);
  if (REDUCED || !card.animate) { swap(); return; }
  const tilt = getComputedStyle(root).getPropertyValue('--tilt').trim() || '0deg';
  const at = deg => `perspective(1200px) rotate(${tilt}) rotateY(${deg}deg)`;
  const dir = turned ? 1 : -1;
  turning = true;
  card.animate([{ transform: at(0) }, { transform: at(90 * dir) }], { duration: 210, easing: 'cubic-bezier(.5, 0, .9, .6)' }).finished.then(() => {
    swap();
    return card.animate([{ transform: at(-90 * dir) }, { transform: at(0) }], { duration: 300, easing: 'cubic-bezier(.15, .5, .4, 1)' }).finished;
  }).finally(() => { turning = false; });
}
if (turnButton) turnButton.addEventListener('click', turnOver);

// ---------- putting the card away, to look at the picture, and taking it out again ----------
const closeButton = card.querySelector('.card-close'), openButton = document.getElementById('card-open');
function setOpen(open, focus) {
  if (open === !card.classList.contains('closed')) return;
  if (!open) skip();                                           // nothing is left half written
  card.classList.toggle('closed', !open);
  card.inert = !open; card.setAttribute('aria-hidden', String(!open));
  if (openButton) openButton.hidden = open;
  if (PEEK) {                                                  // away: back to the picture; out again: all of it
    root.classList.toggle('card-away', !open);
    if (SMALL.matches) { if (open) setTimeout(pullUp, 60); else window.scrollTo({ top: 0, behavior: SMOOTH }); }
  }
  if (focus) { const target = open ? closeButton : openButton; if (target) target.focus({ preventScroll: true }); }
}
if (closeButton) closeButton.addEventListener('click', () => setOpen(false, true));
if (openButton) openButton.addEventListener('click', () => setOpen(true, true));
window.addEventListener('keydown', e => { if (e.key === 'Escape' && card.classList.contains('in')) setOpen(card.classList.contains('closed'), true); });

const start = () => setTimeout(begin, STILL ? 0 : 450);
if (document.fonts && document.fonts.ready) Promise.race([document.fonts.ready, new Promise(r => setTimeout(r, 1200))]).then(start); else start();
})();
