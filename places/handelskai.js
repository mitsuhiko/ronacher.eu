/* ------------------------------------------------------------------ *
 *  Vienna, seen from the Handelskai: over the tracks of the freight
 *  line and the Danube to the Donauturm and the towers of the Donau
 *  City.  A train of containers stands on the second track.  scene.js
 *  paints the sky above it and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'handelskai', name: 'Handelskai', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, ease, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let HZ = 0, PW = 1, PH = 1, WU = 1, TS = 1; // horizon; the photo's width and height in pixels; the unit things are wide in; how big the trees are
let pal = null, traits = null, night = false, season = '';
const use = s => { pal = s.pal; traits = s.traits; night = s.night; season = s.season; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph the picture is painted after: x and y as fractions
// of its width and height.  A wide screen shows a slice of its height, a narrow one of its width
// (and pushes things together a bit).
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along2(pts, x) {                                      // a line through points, eased from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * ease((x - a[0]) / (b[0] - a[0])); }
  return pts[pts.length - 1][1];
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;

// The far bank of the Danube is the horizon: a band of trees, the town standing behind it.
const SHORE = .525, FAR_MIN = .034;
const farH = x => { const xf = fx(x); return (FAR_MIN + .02 * vnoise(xf * 16, 3.3) + .01 * vnoise(xf * 70, 9.1) + .016 * smooth(.66, .71, xf) * smooth(.8, .76, xf)) * PH; };

// The tracks all run towards one point, far off to the right on the horizon.  A line to it is
// known by the height at which it leaves the photo on the left: the tracks, the train, the road.
const VX = 2.38, VY = .502, TRAIN0 = .27, WAGON = .0477;
const lineOf = (xf, pf) => VY + (pf - VY) * VX / (VX - xf);    // which line a point lies on
const lineY = (v, xf) => VY + (v - VY) * (VX - xf) / VX;       // how high that line is at x
const along = xf => ((VX - TRAIN0) / (VX - xf) - 1) / WAGON;   // how far along the track x is, in wagons from the head of the train
const where = s => VX - (VX - TRAIN0) / (1 + WAGON * s);       // and back
const TRACKS = [.768, .8, .832, .866, .934, .988], GAUGE = .013;
const EDGE = .745, BOX = .735, DECK = .787, WHEELS = .805, FENCE = 1.045, ROADV = 1.17, LANE = 1.3;
const yardEdge = xf => EDGE + (vnoise(xf * 40, 7.7) - .5) * .01;

const QUAY = [.577, .43], LOW = [.712, .17];                   // the wall along the water: its top, its end; the low one by the tracks: its line, its end
const SHIP = [.458, .748, .566, .59, .47, .735, .545];         // the ship at the quay: hull from, to, its top, its foot; the decks from, to, their top
// where the bushes on the near bank begin: under the wall on the left, tall trees on the right
const TOPS = [[-.05, .598], [.3, .598], [.4, .596], [.445, .588], [.6, .588], [.72, .582], [.755, .555], [.79, .515], [.84, .492], [.9, .488], [.95, .497], [1.05, .505]];
const bushTop = xf => along2(TOPS, xf);

// ---------- what stands in front: the trees' limbs and the masts ----------
// Thin things other strokes have to keep clear of: a coarse mask says where they are (1 a limb, 2 a mast).
let mask = null, mCols = 0, mRows = 0, mCell = 4, frontTop = 0;
let limbs = [], limbWeight = 0, limbLength = 0, masts = [], mastLength = 0, lamps = [];
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function mark(s, v) {
  const n = Math.ceil(s.len / (mCell * .5)), rad = s.w / 2 + .3 * U, m = Math.ceil(rad / mCell);
  for (let i = 0; i <= n; i++) {
    const x = limbX(s, i / n), y = limbY(s, i / n), ix = Math.floor(x / mCell), iy = Math.floor(y / mCell);
    for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
      const cx = ix + dx, cy = iy + dy;
      if (cx < 0 || cy < 0 || cx >= mCols || cy >= mRows || Math.hypot(dx, dy) * mCell > rad + mCell * .5) continue;
      if (mask[cy * mCols + cx] < v) mask[cy * mCols + cx] = v;
    }
    if (y - rad < frontTop) frontTop = y - rad;
  }
}
// How far a stroke through (x, y) may run either way along (ca, sa) before it crosses one of them: fills reach[].
const reach = [0, 0];
function clearRun(x, y, ca, sa, l, level) {
  reach[0] = reach[1] = l;
  for (let d = mCell; d < l; d += mCell) if (maskAt(x + ca * d, y + sa * d) >= level) { reach[1] = d - mCell; break; }
  for (let d = mCell; d < l; d += mCell) if (maskAt(x - ca * d, y - sa * d) >= level) { reach[0] = d - mCell; break; }
}

// The trees on the near bank that stand out against the water and the sky: [x, foot, top, how wide they spread].
const TREES = [[.016, .612, .522, 1], [.092, .612, .516, 1], [.283, .612, .528, 1], [.454, .606, .416, 1], [.572, .6, .452, .9], [.626, .598, .468, .8]];
// The bushes along the bank are a heap of crowns over a solid mass; every tree adds a few of its own.
let crowns = [], leafy = [], leafArea = 0, strips = [], folTop = 0, folB = 0, folTree = false;
const STRIP = 28, NONE = [];                                   // the crowns are sorted into upright strips, so that a point need not ask all of them
function buildTrees() {
  crowns = []; limbs = [];
  let r = rng(5);
  for (let xf = X0 - .03; xf < fx(W) + .04;) {
    const rad = (.011 + r() * .015) * PH, top = py(bushTop(xf));
    crowns.push({ x: px(xf), y: top + rad * (.9 + r() * .5), r: rad, tree: false });
    if (r() < .5) crowns.push({ x: px(xf) + (r() - .5) * rad, y: top + rad * (2 + r()), r: rad * (.8 + r() * .5), tree: false });
    xf += rad / PW * (.7 + r() * .5);
  }
  r = rng(31);
  const add = (x1, y1, x2, y2, bend, w, depth, tip) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, tip, k: r(), len });
  };
  const grow = (x, y, a, len, w, depth, rad) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len, last = depth >= 5 || len <= 4 * U;
    add(x, y, x2, y2, (r() - .5) * .3, w, depth, last);
    crowns.push({ x: x2, y: y2, r: rad * (depth < 3 ? .8 + r() * .5 : depth < 4 ? .7 : .5), tree: true });
    if (last) return;
    const k = r() < .75 ? 2 : 3;
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .62 + (r() - .5) * .45, len * (.58 + r() * .2), Math.max(.7 * U, w * .6), depth + 1, rad);
  };
  for (const [xf, foot, top, spread] of TREES) {
    const x = px(xf), y = py(foot), h = (foot - top) * PH * TS, w = Math.max(1.5 * U, h * .024);
    add(x, y, x + (r() - .5) * h * .1, y - h * .4, (r() - .5) * .08, w, 0, false);
    const trunk = limbs[limbs.length - 1];
    for (let i = 0; i < 5; i++) {
      const a = -Math.PI / 2 + ((i + .5) / 5 - .5) * 2.2 * spread + (r() - .5) * .2, t = i === 2 ? 1 : .65 + r() * .35;
      grow(limbX(trunk, t), limbY(trunk, t), a, h * (.27 - Math.abs(a + Math.PI / 2) * .05) * (.85 + r() * .3), w * .55, 1, h * .13);
    }
  }
  folTop = H; leafArea = 0; strips = []; leafy = crowns.filter(c => c.tree);
  for (const c of crowns) {
    folTop = Math.min(folTop, c.y - c.r);
    if (c.tree) { c.at = leafArea; leafArea += c.r * c.r; }
    for (let i = Math.floor((c.x - c.r - 4 * U) / STRIP); i <= Math.floor((c.x + c.r + 4 * U) / STRIP); i++) (strips[i + 8] = strips[i + 8] || []).push(c);
  }
  folTop -= 10 * U;
}
function fol(x, y) {                                           // > 0 inside the leaves
  if (y < folTop) return -1;
  const base = py(bushTop(fx(x))), sw = wind(time) * 2 * U, bare = traits.bare, thin = season === 'autumn' ? .22 : 0;
  let d = Math.min(.6, (y - base - .012 * PH) / (.02 * PH)), tree = false;
  folB = 0;
  for (const c of strips[Math.floor(x / STRIP) + 8] || NONE) {
    const dx = x - c.x - sw; if (dx > c.r || dx < -c.r || (bare && c.tree)) continue;   // a bare tree is its limbs and nothing else
    const dy = y - c.y, q = 1 - Math.hypot(dx, dy * 1.1) / c.r - (c.tree ? thin : 0);    // in autumn half the leaves are down
    if (q > d) { d = q; folB = dy / c.r; tree = c.tree; }
  }
  folTree = tree;
  return d * 1.2 + (fbm(x * .03 / U + time * .02, y * .03 / U) - .47) * .6;
}
function folLum(x, y) {                                        // light falls in from the upper left
  const s = time * .03, k = .02 / U;
  const rel = fbm(x * k + s, y * k * 1.2) - fbm((x - 9 * U) * k + s, (y - 9 * U) * k * 1.2);
  return .45 + rel * 3.2 - folB * .25;
}

// [x, top, the line it stands on, width, what it carries]: masts of the overhead line, signals, a lamp
const MASTS = [[.301, .502, .726, 1, 'arm'], [.372, .513, .9, 1.5, 'arms'], [.343, .728, 1.4, 1, 'near'], [.484, .58, .734, .7, ''],
  [.539, .572, .734, .7, 'signal'], [.733, .51, .734, .8, 'arm'], [.97, .52, .877, 1.1, 'lamp'], [.171, .64, .76, .8, 'signal']];
function buildMasts() {
  masts = []; lamps = []; mastLength = 0;
  const seg = (x1, y1, x2, y2, w) => { const len = Math.hypot(x2 - x1, y2 - y1); masts.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2, cy: (y1 + y2) / 2, w, len }); mastLength += len; };
  for (const [xf, top, foot, wd, what] of MASTS) {
    const x = px(xf), y0 = py(top), y1 = Math.min(H + 10 * U, py(lineY(foot, xf))), w = Math.max(1.2 * U, wd * .0034 * WU), a = .012 * PH, t = Math.max(.9 * U, w * .35);
    seg(x, y0, x, y1, w);
    if (what === 'arms') { seg(x, y0 + 9 * a, x + 3 * a, y0 + 9 * a, t); seg(x, y0 + 7.8 * a, x + 3 * a, y0 + 9 * a, t * .8); }   // what holds the wire over the track
    else if (what === 'arm') { seg(x, y0 + 1.8 * a, x + 2 * a, y0 + 1.8 * a, t); seg(x, y0 + .6 * a, x + 2 * a, y0 + 1.8 * a, t * .8); }
    else if (what === 'near') seg(x - 1.2 * a, y0 + a, x + 1.2 * a, y0 + a, t);
    else if (what === 'lamp') { seg(x, y0 + 6 * a, x - 2.6 * a, y0 + 5.6 * a, t); lamps.push({ x: x - 2.6 * a, y: y0 + 5.9 * a, r: 1.5, glow: true }); }
    else if (what === 'signal') { seg(x, y0, x, y0 + 1.6 * a, w * 2.2); lamps.push({ x, y: y0 + .5 * a, r: .9, red: true }); }
  }
}
function buildMask() {
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows);
  frontTop = folTop; limbWeight = limbLength = 0;
  for (const s of limbs) { s.wt = s.len * (s.w + 2 * U); limbWeight += s.wt; limbLength += s.len; mark(s, 1); }
  for (const s of masts) mark(s, 2);
}

// ---------- the skyline ----------
// [x, width, top, colour, kind, how many windows are lit at night]: x and top on the photo, widths
// in photo widths.  From the back to the front.
const TOWERS = [
  [.358, .019, .447, 'towerLight', '', .5], [.37, .012, .456, 'wall', '', .4], [.432, .017, .432, 'towerLight', '', .5],
  [.4555, .022, .41, 'sand', '', .5], [.474, .016, .458, 'glass', '', .5], [.527, .05, .468, 'wall', '', .4],
  [.5675, .025, .428, 'sand', '', .5], [.595, .028, .424, 'glass', '', .6], [.616, .014, .436, 'towerLight', '', .5],
  [.6385, .023, .4265, 'wall', 'cap', .5], [.635, .042, .462, 'dark', '', .3], [.6765, .019, .443, 'glass', '', .6],
  [.7185, .0016, .303, 'tower', 'mast', 0], [.7165, .039, .323, 'dark', 'glass', .5],        // DC Tower 1 and its antenna
  [.7525, .035, .367, 'brown', 'stripe', .35],                                               // the dark one beside it
  [.7765, .015, .418, 'wall', '', .5], [.8075, .029, .382, 'towerLight', 'round', .6],       // Hochhaus Neue Donau
  [.858, .064, .475, 'wall', '', .4], [.93, .08, .487, 'towerLight', '', .3],
];
// The Donauturm is not a slab: [height, half its width there], from the tip of the antenna down.
const TURM_X = .151, TURM = [[.302, .0008], [.343, .0011], [.3465, .0035], [.353, .0085], [.371, .0085], [.3775, .0048], [.386, .0033], [.53, .0046]];
const turmHW = y => along2(TURM, fy(y)) * WU;
let towers = [], towerWeight = 0, turm = null, roof = null, stop = null, tops = null, landTop = 0;
function buildTowers() {
  towers = []; towerWeight = 0;
  const foot = HZ - FAR_MIN * PH * .6, n = Math.ceil(W) + 1;
  for (const [xf, wd, top, col, kind, lit] of TOWERS) {
    const w = Math.max(kind === 'mast' ? 1.2 * U : 3 * U, wd * WU), x0 = Math.round(px(xf) - w / 2);
    towers.push({ x0, x1: Math.max(x0 + 1, Math.round(px(xf) + w / 2)), y0: py(top), y1: foot, col, kind, lit });
  }
  turm = { x0: px(TURM_X) - .0085 * WU, x1: px(TURM_X) + .0085 * WU, y0: py(TURM[0][0]), y1: foot, cx: px(TURM_X), col: 'concrete', kind: 'turm', lit: .3 };
  towers.push(turm);
  landTop = H;
  for (const t of towers) { t.wt = clamp((t.x1 - t.x0) * (t.y1 - t.y0) / (90 * U * U), 1.5, 120); towerWeight += t.wt; landTop = Math.min(landTop, t.y0); }
  landTop -= 8 * U;
  tops = new Float32Array(n);                                  // the tops of the trees on the far bank, pixel by pixel
  for (let i = 0; i < n; i++) tops[i] = HZ - farH(i);
  roof = new Float32Array(n).fill(1e9);                        // the roofline: where the slabs end
  for (const t of towers) if (t !== turm) for (let i = Math.max(0, t.x0); i < Math.min(n, t.x1); i++) if (t.y0 < roof[i]) roof[i] = t.y0;
  stop = roof.slice();
  const hw = TURM[6][1] * WU;
  for (let i = Math.max(0, Math.floor(turm.cx - hw)); i < Math.min(n, Math.ceil(turm.cx + hw)); i++) if (turm.y0 < stop[i]) stop[i] = turm.y0;
}
function towerAt(x, y) {                                       // the one in front here
  for (let i = towers.length - 1; i >= 0; i--) {
    const t = towers[i];
    if (x >= t.x0 && x < t.x1 && y >= t.y0 && (t !== turm || Math.abs(x - t.cx) < turmHW(y))) return t;
  }
  return null;
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that
// thing's strokes may start there.
const SKY = 0, FAR = 1, TOWER = 2, WATER = 3, BOAT = 4, WALL = 5, BANK = 6, TREE = 7, YARD = 8, TRAIN = 9, MAST = 10, ROAD = 11;
let zx = 0, zv = 0, zf = 0, zt = null;                         // what zone() learnt on the way: x, the line through the point, the leaves there, the tower
function zone(x, y, raw) {                                     // raw: as if the limbs and masts were not there
  const m = raw ? 0 : maskAt(x, y);
  if (m === 2) return MAST;
  const xf = zx = fx(x), pf = fy(y), v = zv = lineOf(xf, pf);
  if (v >= FENCE) return ROAD;
  if (xf >= TRAIN0 && v >= BOX && v < WHEELS) return TRAIN;
  if (v >= yardEdge(xf)) return YARD;
  if (xf < LOW[1] && v >= LOW[0]) return WALL;
  const f = zf = fol(x, y);
  if (m === 1 && (traits.bare || f < .15)) return TREE;
  if (f > 0) return BANK;
  if (xf < QUAY[1] && pf >= QUAY[0]) return WALL;
  if (pf >= SHIP[6] && xf >= SHIP[0] && xf < SHIP[1] && (pf >= SHIP[2] || (xf >= SHIP[4] && xf < SHIP[5]))) return BOAT;
  if (y >= HZ) return WATER;
  if (y >= HZ - farH(x)) return FAR;
  return (zt = towerAt(x, y)) ? TOWER : SKY;
}
function levelRun(x, y, l, z) {                                // how far a level stroke may run either way before it leaves its zone
  const step = 2.5 * U;
  reach[0] = reach[1] = l;
  for (let d = step; d < l; d += step) if (zone(x + d, y) !== z) { reach[1] = d - step; break; }
  for (let d = step; d < l; d += step) if (zone(x - d, y) !== z) { reach[0] = d - step; break; }
}
const lineDir = [1, 0];                                        // the way the tracks run at a point
function dirAt(v) { const slope = -(v - VY) / VX * PH / PW, k = 1 / Math.hypot(1, slope); lineDir[0] = k; lineDir[1] = slope * k; }

// ---------- the far side ----------
function farTone(x, y, f) {                                    // the trees over there: f 0 at the water, 1 at their tops
  const n = vnoise(x * .05 / U + 11, y * .09 / U);
  let c = mix(pal.folMid, n > .5 ? pal.folLight : pal.folDark, Math.abs(n - .5) * 1.5);
  if (f < .2) c = mix(c, pal.folDeep, .45);
  return mix(c, pal.haze, .3 + traits.fog * .3);
}
const LAMPS = .043;                                            // lamps along the far shore, one about every so often
const shoreLamp = x => { const i = Math.round(fx(x) / LAMPS); return px((i + (hash1(i * 7 + 1) - .5) * .6) * LAMPS); };
function farStroke() {
  const R = PP.R, x = R() * W, h = farH(x), y = HZ - R() * h;
  if (!at(x, y) || zone(x, y) !== FAR) return;
  const f = (HZ - y) / h, k = R();
  let col = farTone(x, y, f);
  if (f > .55 && k < .4) col = mix(col, pal.folSun, .45); else if (pal.accent && k > .93) col = mix(pick(pal.accent, R()), pal.haze, .25);
  PP.dab(x, y, (1.4 + R() * 2.4) * U, col, { a: .9, den: .8, core: .6, s: 1.6 * U });
  if (night) { const lx = shoreLamp(x); if (Math.abs(x - lx) < 5 * U && R() < .5) light(lx, HZ - .004 * PH, (.6 + R() * .4) * U); }
}

function towerTone(t) { return mix(t.col === 'sand' ? pal.city[7] : pal[t.col], pal.cityFar, .2 + traits.fog * .3 + traits.rain * .08); }
function towerStroke() {
  const R = PP.R; let k = R() * towerWeight, t = towers[0];
  for (const o of towers) { t = o; k -= o.wt; if (k <= 0) break; }
  const x = t.x0 + R() * (t.x1 - t.x0), y = t.y0 + R() * (t.y1 - t.y0);
  if (!at(x, y) || zone(x, y) !== TOWER || zt !== t) return;
  const w = t.x1 - t.x0, u = (x - t.x0) / w, fh = 3.4 * U, fl = Math.floor((y - t.y0) / fh), far = .2 + traits.fog * .3 + traits.rain * .08;
  const o = { a: .92, jit: 6, dry: .04, bow: 0, sp: 1.1 * U };
  const base = t.col === 'sand' ? pal.city[7] : pal[t.col];
  if (t === turm) {
    const pf = fy(y), hw = Math.max(.7 * U, turmHW(y)), head = pf >= TURM[2][0] && pf < TURM[5][0];
    const col = pf < TURM[2][0] ? ((((pf - TURM[0][0]) / .0075) | 0) & 1 ? pal.hull : pal.cabin)                 // the antenna, red and white
      : head ? mix(pal.tower, pal.towerLight, pf > .358 && pf < .366 ? .75 : .2)                                 // the head with its ring of windows
      : mix(base, pal.tower, x > t.cx + hw * .15 ? .34 : .06);                                                   // the shaft, its right side in the shade
    if (head) PP.brush(t.cx - hw + U, y, t.cx + hw - U, y, 2.4 * U, mix(col, pal.cityFar, far), o);
    else { const l = (3 + R() * 6) * U, bw = Math.min(2 * hw, 2.6 * U), cx = clamp(x, t.cx - hw + bw / 2, t.cx + hw - bw / 2); o.sp = Math.min(1.1 * U, bw); PP.brush(cx, Math.max(t.y0, y - l), cx, Math.min(t.y1, y + l), bw, mix(col, pal.cityFar, far), o); }
    if (night && head && R() < .3) light(t.cx + (R() - .5) * 1.6 * hw, py(.362), .6 * U);
    return;
  }
  if (t.kind === 'mast' || t.kind === 'stripe') {              // upright strokes: an antenna, a front of dark and darker ribs
    const rib = Math.floor((x - t.x0) / (2.6 * U)), l = (4 + R() * 10) * U, bw = Math.min(w, 2.4 * U), cx = t.kind === 'mast' ? t.x0 + w / 2 : clamp(t.x0 + (rib + .5) * 2.6 * U, t.x0 + bw / 2, t.x1 - bw / 2);
    const col = t.kind === 'mast' ? ((fl >> 1) & 1 ? pal.hull : pal.cabin) : mix(base, rib & 1 ? pal.dark : pal.stone, .35);
    o.sp = Math.min(1.1 * U, bw);
    PP.brush(cx, Math.max(t.y0, y - l), cx, Math.min(t.y1, y + l), bw, mix(col, pal.cityFar, far), o);
  } else {                                                     // floor by floor: short level strokes, every other row a band of windows
    let col = t.kind === 'glass' ? mix(mix(pal.dark, pal.glass, .1 + .45 * vnoise(x * .03 / U + 3, y * .012 / U)), pal.towerLight, u < .42 ? .1 : 0)
      : t.kind === 'round' ? mix(pal.towerLight, pal.tower, (fl & 1 ? .5 : .05) + Math.abs(u - .38) * .5)
      : mix(base, t.col === 'dark' ? pal.glass : pal.dark, (fl & 1 ? .26 : 0) + (u > .6 ? .16 : 0));
    if (t.kind === 'glass' && (fl & 1)) col = mix(col, pal.dark, .15);
    if (t.kind === 'cap' && fl < 2) col = pal.cabin;
    const l = Math.max(.6, Math.min((2.5 + R() * 5) * U, w / 2 - .3)), cx = clamp(x, t.x0 + l, t.x1 - l);
    PP.brush(cx - l, Math.min(t.y1, t.y0 + (fl + .5) * fh), cx + l, Math.min(t.y1, t.y0 + (fl + .5) * fh), fh * .9, mix(col, pal.cityFar, far), o);
  }
  if (night && R() < t.lit * .22) light(t.x0 + U + R() * (w - 2 * U), t.y0 + 2 * U + R() * (t.y1 - t.y0 - 2 * U), (.5 + R() * .4) * U);
}

// ---------- the river ----------
let waterFoot = 0;
function waterStroke() {
  const R = PP.R, x = R() * W, y = HZ + R() * (waterFoot - HZ);
  if (!at(x, y) || zone(x, y) !== WATER) return;
  const f = (y - HZ) / (waterFoot - HZ), sz = (.6 + f * .6) * U;
  const sh = vnoise(x * .03 / U + time * .5, y * .4 / U);
  let col = mix(pal.water, sh > .5 ? pal.waterLight : pal.waterDark, Math.abs(sh - .5) * 1.5);
  if (f < .32) col = mix(col, farTone(x, 2 * HZ - y, .5), (1 - f / .32) * .5);                 // the far bank, mirrored
  const sd = shadow(x, y); if (sd) col = mix(col, pal.waterDark, sd * .5);
  levelRun(x, y, (8 + R() * 22) * sz, WATER);
  PP.brush(x - reach[0], y, x + reach[1], y + (R() - .5) * U, (1.4 + R() * 2.2) * sz, col, { a: .85, jit: 6, dry: .15, sp: 1.1 * U, bow: 0 });
  if (night) { const lx = shoreLamp(x); if (Math.abs(x - lx) < 3 * U && f < .75 && R() < .6) PP.brush(lx, y - 3 * sz, lx, y + 4 * sz, 1.5 * U, pick(pal.lights, R()), { a: .5 * (1 - f), sp: U, bow: 0 }); }
}

function shipStroke() {                                        // the river cruiser moored at the quay
  const R = PP.R, x = px(SHIP[0] + R() * (SHIP[1] - SHIP[0])), top = py(SHIP[6]), h = py(SHIP[3]) - top, y = top + R() * h;
  if (!at(x, y) || zone(x, y) !== BOAT) return;
  const t = (y - top) / h, pitch = 3.2 * U, cell = Math.floor(x / pitch), row = t > .2 && t < .4 ? 1 : t > .52 && t < .7 ? 2 : 0;
  const o = { a: .93, jit: 5, dry: .04, bow: 0, sp: 1.1 * U };
  if (row && (cell & 1)) {                                     // a window: dark by day, most of them lit at night
    const lit = night && hash1(cell * 31 + row * 7) < .65, yc = top + (row === 1 ? .3 : .61) * h;
    PP.brush((cell + .5) * pitch - U, yc, (cell + .5) * pitch + U, yc, h * .15, lit ? pal.lights[(cell + row) % pal.lights.length] : mix(pal.dark, pal.glass, .3), o);
    return;
  }
  const l = row ? U : (3 + R() * 6) * U, cx = row ? (cell + .5) * pitch : x, white = night ? mix(pal.hull, pal.dark, .45) : pal.hull;
  PP.brush(cx - l, y, cx + l, y, row ? h * .15 : (1.4 + R()) * U, t < .07 ? mix(white, pal.dark, .45) : mix(white, pal.towerLight, R() * .3), o);
}

// ---------- walls, and what is sprayed on them ----------
function nearSeg(x, y, ax, ay, bx, by) { const dx = bx - ax, dy = by - ay, t = clamp(((x - ax) * dx + (y - ay) * dy) / (dx * dx + dy * dy || 1)); return Math.hypot(x - ax - dx * t, y - ay - dy * t); }
function tag(xf, t, low) {                                     // 0 where the wall is bare, else which paint
  if (low) return vnoise(xf * 260, t * 3 + 5) > .56 ? 1 + ((hash1(Math.floor(xf * 110)) * 4) | 0) : 0;   // the low one: patches of colour
  if (t < .3 || t > .86 || xf < .03 || xf > .22) return 0;
  const cw = .0115, i = Math.floor(xf / cw), h = hash1(i * 17 + 5);
  if (h < .16) return 0;                                       // the gap between two words
  const u = xf / cw - i, v = (t - .3) / .56;
  let p = (h * 9) | 0;                                         // a letter: three strokes between points of a three by three grid
  for (let k = 1; k <= 3; k++) {
    const q = (hash1(i * 31 + k * 7) * 9) | 0;
    if (nearSeg(u, v, .18 + (p % 3) * .32, .1 + ((p / 3) | 0) * .4, .18 + (q % 3) * .32, .1 + ((q / 3) | 0) * .4) < .17) return i < 10 ? 1 : 2;
    p = q;
  }
  return 0;
}
function wallStroke() {
  const R = PP.R, low = R() < .15, xf = X0 + R() * ((low ? LOW[1] : QUAY[1]) - X0);
  const v = low ? LOW[0] + R() * (EDGE + .008 - LOW[0]) : 0, pf = low ? lineY(v, xf) : QUAY[0] + R() * .035;
  const x = px(xf), y = py(pf), t = low ? (v - LOW[0]) / (EDGE - LOW[0]) : (pf - QUAY[0]) / .035;
  if (x < 0 || !at(x, y) || zone(x, y) !== WALL) return;
  let col = mix(low ? pal.wall : pal.concrete, pal.stone, fbm(x * .02 / U, y * .08 / U + 3) * .3);
  if (t < .2 && traits.bare) col = pal.folLight;               // snow lies on it
  else if (t < .1) col = mix(col, low ? pal.towerLight : pal.dark, .5); else if (t < .2) col = mix(col, pal.towerLight, .45); else if (t > .8) col = mix(col, pal.stone, .35);
  const g = tag(xf, t, low), l = g ? 1.6 * U : (3 + R() * 7) * U;
  if (g) col = mix(pal.tag[g - 1], col, low ? .25 : .1);
  PP.brush(x - l, y, x + l, y, g ? 1.7 * U : (1.4 + R() * 1.4) * U, col, { a: .9, jit: 6, dry: .06, bow: 0, sp: 1.1 * U });
}

// ---------- the near bank ----------
function bushDab() {                                           // somewhere in the bushes along the bank
  const R = PP.R, x = R() * W, line = py(bushTop(fx(x)));
  leaf(x, line - .03 * PH + R() * (py(lineY(EDGE + .01, fx(x))) - line + .03 * PH));
}
function crownDab() {                                          // somewhere in the crown of a tree
  const R = PP.R; let k = R() * leafArea, c = leafy[0];
  for (let a = 0, b = leafy.length - 1; a <= b;) { const m = (a + b) >> 1; if (leafy[m].at <= k) { c = leafy[m]; a = m + 1; } else b = m - 1; }
  const a = R() * TAU, d = Math.sqrt(R()) * c.r * 1.1;
  leaf(c.x + Math.cos(a) * d, c.y + Math.sin(a) * d);
}
function leaf(x, y) {
  const R = PP.R, line = py(bushTop(fx(x)));
  if (!at(x, y)) return;
  const z = zone(x, y), d = zf;
  if (z === MAST || z === ROAD || z === TRAIN || z === YARD || z === TREE || (z === WALL && zx < LOW[1] && zv >= LOW[0]) || d < -.08) return;
  const tree = folTree, deep = clamp((y - line) / (.08 * PH)), sz = (tree ? .5 : .55 + deep * .2) * U, k = R();
  const lum = clamp(folLum(x, y) + (smooth(.45, .8, zx) - .6) * .3 - (tree ? -.1 : deep * .22));
  if (d < .12) {                                               // the ragged rim: single leaves
    PP.dab(x, y, (2.5 + R() * 5) * sz, traits.bare ? pal.folDark : lum > .5 ? pal.folMid : pal.folDark, { a: .85, den: .3, s: 2.4 * U });
    return;
  }
  if (traits.bare) {                                           // twigs, and the snow lying on them
    if (lum > .42 && k < .2 + lum * .6) { PP.dab(x, y, (4 + R() * 8) * sz, lum > .6 ? pal.folSun : pal.folLight, { a: .9, den: .6, core: .55, sx: 1.7, sy: .55 }); return; }
    const a = -Math.PI / 2 + (R() - .5) * 2, l = (8 + R() * 18) * sz;
    if (k > .6) PP.dab(x, y, (5 + R() * 6) * sz, mix(pal.folMid, pal.folLight, .25 + lum * .5), { a: .8, den: .5, core: .5, s: 2 * U });
    PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 4 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
      lum < .35 ? pal.folDeep : k < .8 ? pal.folDark : pal.folMid, (1 + R() * 1.2) * sz, { pass: 1, a: .8 });
    return;
  }
  let col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .6 && k < .3) col = pal.folSun; else if (pal.accent && lum > .3 && k > (tree && season === 'spring' ? .6 : .92)) col = pick(pal.accent, R());
  const a = R() * Math.PI, l = (4 + R() * 8) * sz;
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (4 + R() * 6) * sz, col, { a: .85, sp: 1.3 * U, dry: .2, jit: 10 });
}
function treeStroke() {                                        // a stretch of bark
  const R = PP.R; let k = R() * limbWeight, s = limbs[0];
  for (const t of limbs) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span;
  const mx = limbX(s, tm), my = limbY(s, tm);
  if (!at(mx, my) || zone(mx, my) !== TREE) return;            // not where its own leaves hide it
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 2 ? .16 : .06), q = R();
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (traits.bare) { if (s.w > 1.3 * U && q < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.folLight, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax - s.w * .22, ay, bx - s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .5), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });      // the sunny side
  }
}

// ---------- the yard ----------
let yardTop = 0;
function yardStroke() {
  const R = PP.R, x = R() * W, y = yardTop + R() * (H - yardTop);
  if (!at(x, y) || zone(x, y) !== YARD) return;
  const xf = zx, v = zv, q = (VX - xf) / VX, sz = (.5 + clamp((v - EDGE) / .3) * .6) * U;
  dirAt(v); const ca = lineDir[0], sa = lineDir[1];
  let tk = -1, rail = 1; for (let i = 0; i < TRACKS.length; i++) { const d = Math.abs(v - TRACKS[i]); if (d < GAUGE) tk = i; rail = Math.min(rail, Math.abs(d - GAUGE / 2)); }
  const off = rail * q * PH;                                   // how far the nearest rail is, in pixels
  if (tk >= 0 && off < 1.3 * sz) return;                       // a rail: see railStroke
  const n = fbm(x * .025 / U + 5, y * .07 / U), sd = shadow(x, y);
  let col = mix(pal.ballast, n > .5 ? pal.ballastLight : pal.ballastDark, Math.abs(n - .5) * 1.8);
  if (xf > TRAIN0 - .012 && v < WHEELS + .014) col = mix(col, pal.deck, .5);       // the train's shadow
  if (sd) col = mix(col, pal.ballastDark, sd * .6);
  if (tk >= 0 && Math.abs(v - TRACKS[tk]) < GAUGE / 2) {       // between the rails: sleepers and the ballast between them, by turns
    const per = 14 / (1 + (v - TRACKS[0]) * 2.5), i = Math.round(along(xf) * per * 2), xs = px(where(i / (per * 2))), pitch = Math.abs(px(where((i + 2) / (per * 2))) - xs);
    if (pitch > 2.6 * U) {
      if (maskAt(xs, y) === 2) return;
      const ya = py(lineY(TRACKS[tk] - GAUGE * .44, fx(xs))), yb = py(lineY(TRACKS[tk] + GAUGE * .44, fx(xs))), bw = pitch * .46;
      PP.brush(xs + (yb - ya) * .1, ya, xs - (yb - ya) * .1, yb, bw, i & 1 ? mix(col, pal.ballastDark, .3) : pal.sleeper, { a: .9, jit: 6, dry: .05, sp: Math.min(1.1 * U, bw), bow: 0 });
      return;
    }
    col = mix(col, pal.sleeper, .45);                          // too far off to tell them apart
  } else if (tk >= 0) col = mix(col, pal.ballastDark, .3);     // the shoulder of the bed
  else if (!traits.bare && vnoise(xf * 70 + 3, v * 45) > (v > .88 && v < .92 ? .64 : .79)) {      // weeds, most of them on the broad strip
    PP.dab(x, y, (1.2 + R() * 2) * sz, mix(pal.cityGreen, pal.folDark, .25 + R() * .4), { a: .9, den: .7, core: .7, s: 1.5 * U });
    if (R() < .4) PP.dab(x - sz, y - 1.3 * sz, 1.3 * sz, mix(pal.cityGreen, pal.folLight, .4), { a: .85, den: .6, core: .4, s: 1.3 * U });
    return;
  }
  const l = (5 + R() * 12) * sz;
  clearRun(x, y, ca, sa, l, 2);
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], Math.max(U, Math.min((2 + R() * 3) * sz, 2 * (off - .8 * sz))), col, { a: .8, jit: 9, dry: .2, sp: 1.1 * U, bow: .02, dash: 10 * U });
}

function railStroke() {                                        // every stroke lies on a rail: dark, and bright on top where the wheels run
  const R = PP.R, tk = (R() * TRACKS.length) | 0, vr = TRACKS[tk] + (R() < .5 ? -1 : 1) * GAUGE / 2, x = R() * W, y = py(lineY(vr, fx(x)));
  if (y >= H || !at(x, y) || zone(x, y) !== YARD) return;
  const sz = (.55 + clamp((vr - EDGE) / .3) * .6) * U, o = { a: .95, jit: 5, dry: .03, sp: Math.min(U, sz), bow: 0 };
  dirAt(vr);
  clearRun(x, y, lineDir[0], lineDir[1], (12 + R() * 26) * U, 2);
  const xa = x - lineDir[0] * reach[0];
  let xb = x + lineDir[0] * reach[1];
  if (vr >= BOX && vr < WHEELS && xa < px(TRAIN0)) xb = Math.min(xb, px(TRAIN0) - U);          // the train stands on it
  const ya = py(lineY(vr, fx(xa))), yb = py(lineY(vr, fx(xb)));
  PP.brush(xa, ya, xb, yb, 2 * sz, pal.rail, o);
  if (R() < .7) PP.brush(xa, ya - .8 * sz, xb, yb - .8 * sz, .9 * sz, pal.railShine, o);
}

function trainStroke() {
  const R = PP.R, xf = TRAIN0 + R() * (fx(W) + .01 - TRAIN0), v = BOX + R() * (WHEELS - BOX), x = px(xf), y = py(lineY(v, xf));
  if (!at(x, y) || maskAt(x, y) === 2) return;
  const s = along(xf), n = Math.floor(s), u = s - n, o = { a: .93, jit: 6, dry: .04, bow: 0, sp: 1.1 * U };
  dirAt(v); const ca = lineDir[0], sa = lineDir[1];
  if (v >= DECK) {                                             // the wagon under it: its frame, the wheels, the dark between them
    const l = (4 + R() * 8) * U, wheel = v > DECK + .005 && Math.abs(u - .5) > .26 && Math.abs(u - .5) < .42;
    clearRun(x, y, ca, sa, l, 2);
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], 1.8 * U, wheel || v < DECK + .005 ? mix(pal.deck, pal.stone, v < DECK + .005 ? .3 : 0) : mix(pal.deck, pal.ballastDark, .45), o);
    return;
  }
  if (u < .03 || u > .97) { PP.brush(x, y - 2 * U, x, y + 2 * U, 1.6 * U, mix(pal.deck, pal.folDeep, .5), o); return; }   // between two boxes
  const box = pal.box[(hash1(n * 7 + 3) * pal.box.length) | 0];
  if (v < BOX + .008) {                                        // its open top catches the light (or the snow)
    const l = Math.min((4 + R() * 8) * U, Math.abs(px(where(n + .97)) - px(where(n + .03))) * Math.min(u - .03, .97 - u));
    PP.brush(x - ca * l, y - sa * l, x + ca * l, y + sa * l, 1.8 * U, traits.bare ? pal.boxTop : mix(box, pal.boxTop, .65), o);
    return;
  }
  const ribs = 20, r = Math.floor(u * ribs), xs = px(where(n + (r + .5) / ribs)), bw = Math.max(1.1 * U, Math.abs(px(where(n + (r + 1) / ribs)) - px(where(n + r / ribs))) * .95);
  if (maskAt(xs, y) === 2) return;
  const ya = py(lineY(BOX + .008, fx(xs))), yb = py(lineY(DECK, fx(xs))), l = (3 + R() * 6) * U;
  let col = r === 0 || r === ribs - 1 ? mix(box, pal.deck, .4) : mix(box, r & 1 ? pal.deck : pal.boxTop, r & 1 ? .2 : .1);   // its sides are ribbed
  const sd = shadow(x, y); if (sd) col = mix(col, pal.deck, sd * .3);
  o.sp = Math.min(1.1 * U, bw);
  PP.brush(xs, Math.max(ya, y - l), xs, Math.min(yb, y + l), bw, col, o);
}

function mastStroke() {
  const R = PP.R; let k = R() * mastLength, s = masts[0];
  for (const t of masts) { s = t; k -= t.len; if (k <= 0) break; }
  const span = Math.min(1, (8 + R() * 18) * U / s.len), t0 = R() * (1 - span), t1 = t0 + span;
  const ax = s.x1 + (s.x2 - s.x1) * t0, ay = s.y1 + (s.y2 - s.y1) * t0, bx = s.x1 + (s.x2 - s.x1) * t1, by = s.y1 + (s.y2 - s.y1) * t1;
  if (!at((ax + bx) / 2, Math.min(H - 1, (ay + by) / 2))) return;
  PP.brush(ax, ay, bx, by, s.w, pal.mast, { a: .95, jit: 6, dry: .04, sp: Math.min(1.1 * U, s.w), bow: 0 });
  if (s.w > 2.2 * U && R() < .6) PP.brush(ax - s.w * .28, ay, bx - s.w * .28, by, s.w * .3, traits.bare ? pal.folLight : pal.mastLight, { a: .8, jit: 5, dry: .04, sp: Math.min(1.1 * U, s.w * .3), bow: 0 });   // its sunny side
}
function lampDab() {                                           // the lamp over the yard, the signals
  const R = PP.R, l = pick(lamps, R());
  if (!l || !at(l.x, l.y)) return;
  if (l.red) PP.dab(l.x, l.y, l.r * U, pal.beacon, { a: 1, den: 3, core: 1, s: U, jit: 3 });
  else if (!night) PP.dab(l.x, l.y, l.r * U, pal.towerLight, { a: 1, den: 2.5, core: .9, s: U, jit: 4 });
  else { PP.dab(l.x, l.y + 5 * U, 10 * U, pal.lights[0], { a: .12, den: .4, s: 3 * U, jit: 3 }); light(l.x, l.y, l.r * U); }
}

let roadTop = 0, roadLeft = 0;
function roadStroke() {                                        // the fence along the tracks, the pavement, the road
  const R = PP.R, x = roadLeft + R() * (W - roadLeft), y = roadTop + R() * (H - roadTop);
  if (x < 0 || !at(x, y) || zone(x, y) !== ROAD) return;
  const xf = zx, v = zv, n = fbm(x * .02 / U + 9, y * .05 / U);
  dirAt(v); const ca = lineDir[0], sa = lineDir[1];
  let col, w = (3 + R() * 4) * U, l = (8 + R() * 16) * U, yc = y;
  if (v < FENCE + .016) { const rail = Math.abs(v - FENCE - .008) > .004; col = rail ? pal.mast : mix(pal.ballastDark, pal.deck, .4); w = rail ? 1.2 * U : 2 * U; }
  else if (v < ROADV) col = mix(pal.kerb, v > ROADV - .014 ? pal.towerLight : pal.ballastDark, v > ROADV - .014 ? .4 : n * .5);
  else if (Math.abs(v - LANE) < .007 && (Math.floor(along(xf) * 1.5) & 1)) { col = pal.roadLine; w = 1.8 * U; l = 5 * U; yc = py(lineY(LANE, xf)); }
  else col = mix(pal.road, n > .5 ? pal.kerb : pal.dark, Math.abs(n - .5) * .5);
  clearRun(x, yc, ca, sa, l, 2);
  PP.brush(x - ca * reach[0], yc - sa * reach[0], x + ca * reach[1], yc + sa * reach[1], w, col, { a: .85, jit: 7, dry: .12, sp: 1.1 * U, bow: .02 });
}

// ---------- weather ----------
function fogStroke() {                                         // fog lies on the river in drifting banks, and creeps over the tracks
  const R = PP.R, x = R() * W, depth = R(), y = HZ - .07 * PH + depth * .3 * PH;
  if (!at(x, y) || !traits.fog) return;
  const z = zone(x, y);
  if (z === MAST || z === TREE || z === TRAIN || z === ROAD || (z === YARD && zv > .9)) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .22 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .5, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // snow glittering in the sun, and the river
  const R = PP.R, x = R() * W, y = HZ + R() * (H - HZ);
  if (!at(x, y) || !traits.glint) return;
  const z = zone(x, y);
  if (z !== YARD && z !== WATER) return;
  PP.dab(x, y, (.7 + R() * .9) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x, y) {                                    // a broad stroke of a fresh coat
  const R = PP.R, o = { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
  if (y < HZ) for (const t of towers) if (x > t.x0 - 32 * U && x <= t.x1 + 32 * U && y + 20 * U > t.y0) {   // every tower that stands here: one upright stroke
    const w = t === turm ? Math.max(1.5 * U, 2 * turmHW(Math.max(y, t.y0))) : t.x1 - t.x0, cx = (t.x0 + t.x1) / 2;
    PP.brush(cx, Math.max(t.y0 + Math.min(w, 8 * U) * .2, y - 24 * U), cx, Math.min(t.y1, y + 24 * U), w, towerTone(t), { a: .95, jit: 6, dry: 0, full: 1, sp: Math.min(2.2 * U, w), bow: 0 });
  }
  const z = zone(x, Math.min(y, H - 1), true), l = (40 + R() * 40) * U;
  if (y > HZ - 60 * U && y < HZ + 22 * U) { const h = farH(x); PP.brush(x - l, HZ - h * .45, x + l, HZ - h * .45, h, farTone(x, HZ - h * .45, .5), o); }   // the far bank is a narrow band: every stroke near it lays some of it
  if (z === SKY || z === TOWER || z === FAR) return;
  dirAt(zv); const ca = lineDir[0], sa = lineDir[1];
  if (z === WATER) PP.brush(x - l, y, x + l, y, Math.min(36 * U, .05 * PH), pal.water, o);
  else if (z === BOAT) PP.brush(x - l, y, x + l, y, .02 * PH, night ? mix(pal.hull, pal.dark, .45) : pal.hull, o);
  else if (z === WALL) PP.brush(x - l, y, x + l, y, .03 * PH, mix(pal.concrete, pal.stone, .15), o);
  else if (z === BANK) {
    const lum = clamp(folLum(x, y)), a = R() * Math.PI, fl = 16 * U;
    PP.brush(x - Math.cos(a) * fl, y - Math.sin(a) * fl, x + Math.cos(a) * fl, y + Math.sin(a) * fl, (22 + R() * 12) * U,
      traits.bare ? mix(pal.folMid, pal.folLight, lum * .6) : mix(pal.folDeep, lum > .5 ? pal.folMid : pal.folDark, clamp(lum * 1.3)), o);
  } else if (z === TRAIN) {
    const n = Math.floor(along(zx)), xa = px(where(n + .03)), xb = px(where(n + .97)), xm = (xa + xb) / 2, ym = py(lineY((BOX + DECK) / 2, fx(xm))), hl = (xb - xa) / 2;
    PP.brush(xm - hl * ca, ym - hl * sa, xm + hl * ca, ym + hl * sa, (DECK - BOX) * PH * (VX - fx(xm)) / VX, pal.box[(hash1(n * 7 + 3) * pal.box.length) | 0], o);
  } else if (z === YARD) PP.brush(x - ca * l, y - sa * l, x + ca * l, y + sa * l, (34 + R() * 12) * U, pal.ballast, o);
  else PP.brush(x - ca * l, y - sa * l, x + ca * l, y + sa * l, (30 + R() * 12) * U, zv < ROADV ? pal.kerb : pal.road, o);
}

// ---------- what moves ----------
function drawBeacons(P, t) {                                   // the red lights on the Donauturm and the tallest tower blink through the night
  const tips = [[turm.cx, turm.y0 + 2 * U, 0], [towers[12].x0 + .5, towers[12].y0 + 2 * U, 1.9]];
  for (let i = 0; i < tips.length; i++) {
    const [x, y, ph] = tips[i], a = Math.pow(Math.max(0, Math.sin(t * 2.1 + ph)), 2);
    if (!at(x, y) || !night || a < .05 || traits.fog) continue;
    P.R = rng(7000 + i);                                       // each keeps its own shape while it glows
    P.dab(x, y, 6 * U, pal.beacon, { a: a * .3, den: .5, s: 2.4 * U, jit: 3 });
    P.dab(x, y, 1.6 * U, pal.beacon, { a, den: 2.5, core: a, s: U, jit: 3 });
  }
}
function drawBarge(P, t) {                                     // now and then a barge comes down the river
  const u = (t % 95) / 70; if (u > 1) return;
  const len = .075 * WU, y = py(.541), x0 = W + len - u * (W + 3 * len), n = 12, h = .0045 * PH, o = { a: 1, sp: 1.1 * U, dry: 0, jit: 4, bow: 0 };
  for (let i = -4; i < n; i++) {
    const xa = x0 + i / n * len, xb = xa + len / n + 1.5 * U, xm = (xa + xb) / 2;
    if (!at(xm, y) || zone(xm, y) !== WATER) continue;         // behind the trees and the masts it is not to be seen
    if (i < 0) { P.brush(xa, y + h * .7, xb, y + h * .7 + (i & 1 ? U : 0), 1.2 * U, pal.waterLight, { a: .6 + i * .12, sp: U, dry: .2, bow: .2 }); continue; }   // the wave at its bow
    P.brush(xa, y, xb, y, h, mix(pal.dark, pal.stone, .3), o);
    if (i >= n - 2) { P.brush(xa, y - h * 1.1, xb, y - h * 1.1, h * 1.3, pal.hull, o); if (night && i === n - 2) P.dab(xm, y - h * 1.2, U, pal.lights[2], { a: 1, den: 3, core: 1, s: U }); }   // the wheelhouse at the stern
    else if (i > 0) P.brush(xa, y - h * .8, xb, y - h * .8, h * .7, pal.box[(i >> 1) % pal.box.length], o);
  }
}

// ---------- the painting, one frame's worth ----------
const RATE = { far: 9000, tower: 1, water: 7000, ship: 10000, wall: 9000, fol: 8000, tree: 1.2, yard: 6500, rail: .12, train: 9000, road: 5000, mast: 1.6, lamp: 5, glint: 90, fog: 1500 };
const place = {
  colors: {
    ballast: '#8f8b84', ballastLight: '#b5b0a5', ballastDark: '#67645f', rail: '#40373a', railShine: '#d2ccbf', sleeper: '#594d45',
    box: ['#d39d2f', '#c06c2d', '#d9aa3c', '#ae5630', '#cb8f2c'], boxTop: '#f0d98f', deck: '#2a2829',
    road: '#3d4149', roadLine: '#e6e3da', kerb: '#aaa69d', mast: '#474e58', mastLight: '#9ea6ac', concrete: '#b9b09c', brown: '#4b3d36',
    tag: ['#2f4178', '#20242e', '#a8364a', '#2f8a94'],
  },
  seasons: { winter: { ballast: '#e6eaed', ballastLight: '#ffffff', ballastDark: '#bcc6d1', sleeper: '#c2c9d1', boxTop: '#ffffff', kerb: '#e2e6e9', road: '#5d646e' } },
  ground: /^(ballast|kerb|sleeper)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const wide = clamp((W / H - 1) / .6), tall = clamp((1 - W / H) / .5), y1 = 1 - .07 * wide, x1 = 1 - .13 * tall;
    Y0 = .15 * wide; X0 = .07 * tall; PH = H / (y1 - Y0); PW = W / (x1 - X0);
    const squeeze = PW / (PH * 4 / 3);                         // how much narrower than the photo the picture is
    WU = PH * 4 / 3 * clamp(squeeze, .55, 1.25); TS = clamp(squeeze, .62, 1);
    HZ = place.hz = py(SHORE);
    waterFoot = py(.6); yardTop = py(lineY(EDGE - .01, fx(W))); roadTop = py(lineY(FENCE, fx(W)));
    roadLeft = px(VX * (1 - (fy(H) - VY) / (FENCE - VY)));
    buildTrees(); buildMasts(); buildMask(); buildTowers();
  },
  tick(t) { time = t; },
  top(x) { return Math.min(stop[x < 0 ? 0 : x >= W ? W - 1 : x | 0], HZ - farH(x) - U); },
  sky(c, pad) {                                                // down to the roofs and the trees on the far bank, and round the Donauturm
    const cx = turm.cx, dip = 4 * U - pad;
    c.moveTo(0, -60); c.lineTo(W, -60);
    let y = tops[W - 1] + dip, slab = false;
    c.lineTo(W, y);
    for (let i = W - 1; i >= 0; i--) {
      const t = tops[i] + dip, r = roof[i] < t;
      if (r || slab) { const e = r ? roof[i] : t; if (e !== y) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } slab = r; }
      else if (i % 3 === 0) { y = t; c.lineTo(i + .5, y); }
    }
    c.lineTo(0, y); c.closePath();
    let foot = 1e9; for (let i = Math.max(0, Math.floor(cx) - 7); i <= Math.min(W, Math.ceil(cx) + 7); i++) foot = Math.min(foot, tops[i] + dip - 2);
    const pts = TURM.map(p => [Math.max(.6, p[1] * WU), py(p[0])]).filter(p => p[1] < foot);
    pts.push([turmHW(foot), foot]);
    c.moveTo(cx - pts[0][0], pts[0][1]);                       // a hole: it runs the other way round
    for (const p of pts) c.lineTo(cx - p[0], p[1]);
    for (let i = pts.length - 1; i >= 0; i--) c.lineTo(cx + pts[i][0], pts[i][1]);
    c.closePath();
  },
  front(s, x, y) { if (y < frontTop) return false; use(s); return maskAt(x, y) > 0 || fol(x, y) > -.05; },   // the trees on the bank reach up into the sky
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    clipBand(landTop, H);
    spend('hk-far', RATE.far * W * .045 * PH / ua * pace, farStroke);
    spend('hk-tower', RATE.tower * towerWeight * pace, towerStroke);
    spend('hk-water', RATE.water * W * (waterFoot - HZ) / ua * pace, waterStroke);
    spend('hk-ship', RATE.ship * (SHIP[1] - SHIP[0]) * PW * (SHIP[3] - SHIP[6]) * PH / ua * pace, shipStroke);
    spend('hk-wall', RATE.wall * (QUAY[1] - X0) * PW * .04 * PH / ua * pace, wallStroke);
    spend('hk-fol', RATE.fol * W * .13 * PH / ua * pace, bushDab);
    if (!s || !s.traits.bare || o) spend('hk-crown', RATE.fol * leafArea * 1.6 / ua * pace, crownDab);
    spend('hk-tree', RATE.tree * limbLength / (14 * U) * pace, treeStroke);
    spend('hk-yard', RATE.yard * W * (H - yardTop) / ua * pace, yardStroke);
    spend('hk-rail', RATE.rail * TRACKS.length * 2 * W / U * pace, railStroke);
    spend('hk-train', RATE.train * W * .05 * PH / ua * pace, trainStroke);
    spend('hk-road', RATE.road * Math.max(0, W - roadLeft) * (H - roadTop) * .5 / ua * pace, roadStroke);
    spend('hk-mast', RATE.mast * mastLength / (14 * U) * pace, mastStroke);
    spend('hk-lamp', RATE.lamp * pace, lampDab);
    if (s && s.traits.glint) spend('hk-glint', RATE.glint * W * (H - HZ) / ua * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('hk-fog', RATE.fog * W * .3 * PH / ua * pace, fogStroke);
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) if (cells[k].y > landTop - 20 * U) coatStroke(cells[k].x, cells[k].y);
    c.restore();
  },
  sprites: [{ draw: drawBarge, t0: 4, seed: 111 }, { draw: drawBeacons, t0: 4, seed: 112 }],
};
return place;
} });
