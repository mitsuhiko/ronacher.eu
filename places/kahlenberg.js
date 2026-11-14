/* ------------------------------------------------------------------ *
 *  Vienna, seen from the Kahlenberg: the town on its plain with the
 *  Danube running through it, the vineyards rolling down towards it
 *  and the trees in front.  scene.js paints the sky above it and says
 *  what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'kahlenberg', name: 'Kahlenberg', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, ease, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let HZ = 0, BAND = 0, HILL = 0, LX = 1;     // horizon, depth of the city plain, depth of the hills, hill wavelength
let pal = null, traits = null, night = false;
const use = s => { pal = s.pal; traits = s.traits; night = s.night; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything below is measured on the photograph the picture is painted after.  Heights
// are fractions of the photo's height: horizon .419, the foot of the city plain .522.
// A wide screen shows a slice of it (Y0..Y1), losing some sky and some of the trees in front.
let Y0 = 0, Y1 = 1;
const py = f => (f - Y0) / (Y1 - Y0) * H;
const TOPS = [[0, .49], [.06, .475], [.12, .5], [.2, .54], [.28, .585], [.34, .66], [.4, .725], [.5, .745], [.6, .74], [.7, .735], [.8, .73], [.9, .735], [1, .745]];
function treeTop(xf) {                                         // where the trees in front begin, as a fraction of the screen height
  xf = clamp(xf);
  let v = TOPS[TOPS.length - 1][1];
  for (let i = 0; i < TOPS.length - 1; i++) if (xf <= TOPS[i + 1][0]) {
    const a = TOPS[i], b = TOPS[i + 1]; v = a[1] + (b[1] - a[1]) * ease((xf - a[0]) / (b[0] - a[0])); break;
  }
  return (v - Y0) / (Y1 - Y0);
}
// where the town ends and the vineyards begin: level on the left, the garden suburbs reach further down on the right
const ridgeY = x => { const xf = x / W; return HZ + BAND * (1 + smooth(.5, .85, xf) * .4 + smooth(.8, 1.05, xf) * .14 + .03 * Math.sin(xf * 9 + 1) + .015 * Math.sin(xf * 23)); };
const HT = [null, { b: .13, a: .05, f: 4.2, p: 2.1 }, { b: .3, a: .07, f: 3.1, p: 4 }, { b: .52, a: .07, f: 2.3, p: .7 }];
function hillTop(i, x) {
  if (!i) return ridgeY(x);
  const h = HT[i], q = x / LX;
  return HZ + BAND + HILL * (h.b + h.a * Math.sin(q * h.f + h.p) + h.a * .3 * Math.sin(q * h.f * 2.3 + h.p * 1.7));
}
const farH = x => (1.5 + 3 * vnoise(x * .004 / U, 3.3) + 1 * vnoise(x * .02 / U, 9.1)) * U;      // the hills beyond the city
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;

// The Danube comes towards us from the far end of the plain and turns left along its foot.
// [x, centre, half width]: x as a fraction of the width, the others of the plain's depth.
const RIVER = [[0, .86, .07], [.1, .85, .075], [.19, .8, .08], [.235, .68, .06], [.27, .58, .05], [.295, .46, .07], [.315, .35, .075], [.33, .26, .04], [.345, .2, .015], [.36, .17, 0]];
const ARM = [[0, .68, .03], [.09, .65, .03], [.145, .55, .03], [.2, .42, .02], [.235, .31, .025], [.3, .29, .022], [.315, .27, 0]];
let chC = 0, chW = 0;
function channel(pts, xf) {
  if (xf >= pts[pts.length - 1][0]) { chW = 0; return false; }
  for (let i = 0; i < pts.length - 1; i++) if (xf <= pts[i + 1][0]) {
    const a = pts[i], b = pts[i + 1], t = (xf - a[0]) / (b[0] - a[0]);
    chC = a[1] + (b[1] - a[1]) * t; chW = a[2] + (b[2] - a[2]) * t; return true;
  }
  return false;
}
function riverAt(x, f) {                                       // 0 land, 1 the river, 2 the New Danube
  const xf = Math.max(0, x / W);
  if (channel(RIVER, xf) && Math.abs(f - chC) < chW) return 1;
  return channel(ARM, xf) && Math.abs(f - chC) < chW ? 2 : 0;
}

// the vineyards: rolling ridges, one behind the other, each cut into fields
const hs = { li: 0, id: 0, type: 0, ang: 0, lum: 0, hedge: false, path: false, h: 0, slope: 0 };
function hillAt(x, y) {
  let li = 0; for (let i = 3; i >= 1; i--) if (y >= hillTop(i, x)) { li = i; break; }
  const d = y - hillTop(li, x), e = 6 * U;
  const slope = (hillTop(li, x + e) - hillTop(li, x - e)) / (2 * e);
  const fw = (70 + li * 45) * U, fh = (34 + li * 30) * U, skew = [.7, -.55, .45, -.4][li];
  const warp = (vnoise(x * .004 / U + li * 7, y * .006 / U) - .5) * 90 * U;
  const id = Math.floor((x + d * skew * 2 + warp) / fw) * 31 + Math.floor((d + warp * .3) / fh) * 17 + li * 101;
  const h = hash1(id * 7 + 3), hb = hash1(id * 13 + 5), hc = hash1(id * 29 + 11);
  hs.li = li; hs.id = id; hs.h = hb; hs.slope = slope;
  hs.type = h < .72 ? 0 : h < .93 ? 1 : 2;                     // vines, meadow, bare earth
  hs.ang = Math.atan(slope) + (hs.type === 0 ? (hc - .5) * 2.4 : 0);
  // light: the lie of the land, sun on the crest, each field its own tone, clouds passing over
  hs.lum = clamp(.52 + slope * 1.8 + (fbm(x * .003 / U + li * 3.7, y * .007 / U) - .47) * 1.2 + .1 - d / (HILL * .22) * .16 + (hb - .5) * .3 - shadow(x, y));
  hs.hedge = d < (7 + li * 6) * U * (.5 + vnoise(x * .03 / U, li * 9.1)) && vnoise(x * .007 / U + li * 13.7, 2.2) > .38;
  if (!hs.hedge) {                                             // the wooded ridge on the right, the trees around the farm
    const xf = x / W, pf = Y0 + y / H * (Y1 - Y0);
    hs.hedge = (xf > .6 && Math.abs(pf - (.548 + (xf - .62) * .25)) < (.008 + (xf - .6) * .09) * (.6 + .8 * vnoise(x * .02 / U, 5.5))) ||
      Math.hypot((xf - .53) / .04, (pf - .578) / .011) < .6 + .8 * vnoise(x * .05 / U, y * .05 / U);
  }
  hs.path = !hs.hedge && Math.abs(d - (30 + li * 26 + Math.sin(x / LX * 9 + li * 2.4) * (9 + li * 4)) * U) < (1.3 + li * .6) * U;
}
function fieldColor() {
  let c = hs.type === 0 ? mix(pal.fieldC, pal.fieldB, .35 + hs.h * .65) : hs.type === 1 ? mix(pal.fieldA, pal.fieldLight, hs.h * .6) : mix(pal.fieldB, pal.earth, .35 + hs.h * .45);
  c = hs.lum > .5 ? mix(c, pal.fieldLight, (hs.lum - .5) * .9) : mix(c, pal.fieldShadow, (.5 - hs.lum) * 1.5);
  return mix(c, pal.haze, (3 - hs.li) * .07);
}

// the trees in front: a heap of crowns over a solid mass
let crowns = [], folB = 0, treeLine = 0;
function buildCrowns() {
  crowns = []; const r = rng(5);
  let x = -30 * U;
  while (x < W + 60 * U) {
    const rad = (26 + r() * 34) * U, top = treeTop(x / W) * H;
    crowns.push({ x, y: top + rad * (.95 + r() * .5), r: rad });
    if (r() < .6) crowns.push({ x: x + (r() - .5) * rad, y: top + rad * (2 + r()), r: rad * (.8 + r() * .5) });
    x += rad * (.7 + r() * .5);
  }
  treeLine = H; for (let i = 0; i <= 40; i++) treeLine = Math.min(treeLine, treeTop(i / 40) * H);
  treeLine -= 14 * U;
}
function fol(x, y) {                                           // > 0 inside the foliage
  if (y < treeLine) return -1;
  const base = treeTop(x / W) * H;
  if (y < base - 14 * U) return -1;
  let d = Math.min(.6, (y - base - 50 * U) / (40 * U));
  folB = 0;
  const sw = wind(time) * 3 * U;
  for (const c of crowns) {
    const dx = x - c.x - sw; if (dx > c.r || dx < -c.r) continue;
    const dy = y - c.y, q = 1 - Math.hypot(dx, dy * 1.1) / c.r;
    if (q > d) { d = q; folB = dy / c.r; }
  }
  return d * 1.2 + (fbm(x * .02 / U + time * .02, y * .02 / U) - .47) * .7;
}
function folLum(x, y) {                                        // light falls in from the upper right
  const s = time * .03, k = .011 / U;
  const rel = fbm(x * k + s, y * k * 1.2) - fbm((x + 13 * U) * k + s, (y - 13 * U) * k * 1.2);
  return clamp(.42 + rel * 3.4 - folB * .25 + (smooth(.1, .55, x / W) - .5) * .3 + (y / H - .8) * .25);
}

function fogStroke() {
  const R = PP.R, x = R() * W, depth = R(), y = HZ - 12 * U + depth * BAND * 2.3;
  if (!at(x, y) || !traits.fog) return;
  if (fol(x, y) > 0 || inTree(x, y)) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .22 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .5, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}

// a bush, a tree seen from far away
function bush(x, y, r) {
  const R = PP.R;
  if (inTree(x, y)) return;
  if (traits.bare) {
    PP.dab(x, y, r, mix(pal.fieldB, pal.folMid, .3), { a: .85, den: .6, core: .7, s: 1.6 * U });
    for (let i = 0; i < 2; i++) { const a = -Math.PI / 2 + (R() - .5) * 1.6; PP.line([[x, y + r * .5], [x + Math.cos(a) * r * 1.3, y + r * .5 + Math.sin(a) * r * 1.5]], pal.folDark, Math.max(.8, r * .16), { pass: 1, a: .75, wob: .4 * U }); }
    if (R() < .45) PP.dab(x, y - r * .4, r * .6, pal.folLight, { a: .85, den: .6, core: .4, s: 1.5 * U, sx: 1.4, sy: .6 });
    return;
  }
  PP.dab(x, y, r, R() < .6 ? pal.folDark : pal.folMid, { a: .9, den: .7, core: .75, s: 2 * U });
  const k = R();
  if (k < .55) PP.dab(x + r * .3, y - r * .35, r * .55, k < .16 && pal.accent ? pick(pal.accent, R()) : pal.folLight, { a: .85, den: .6, core: .4, s: 1.6 * U });
}

// ---------- the old tree ----------
// On the left a tree reaches its bare limbs out of the leaves.  It stands in front of
// everything else, so the other strokes keep clear of it: a coarse mask says where it is.
// [angle, length (in depths of the plain), width, where on the trunk it starts]: wiry limbs, the long ones reaching right
const FAN = [[-2.7, .3, 3, .78], [-2.1, .24, 3, .92], [-1.5, .2, 3, 1], [-1, .26, 3.2, 1], [-.55, .5, 3.8, .9], [-.25, .85, 4.5, .8], [.08, .8, 4, .68]];
let limbs = [], limbWeight = 0, limbLength = 0, treeMask = null, treeCols = 0, treeCell = 4;
const treeBox = [0, 0, 0, 0], reach = [0, 0];
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
function buildTree() {
  const r = rng(31); limbs = [];
  const add = (x1, y1, x2, y2, bend, w, depth, tip) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, tip, k: r(), len });
  };
  const grow = (x, y, a, len, w, depth) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len, last = depth >= 5 || len <= 8 * U;
    add(x, y, x2, y2, (r() - .5) * .4, w, depth, last);
    if (last) return;
    const k = (r() < .72 ? 2 : 1) + (r() < .12 ? 1 : 0);
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .55 + (r() - .5) * .5, len * (.64 + r() * .2), Math.max(U, w * .7), depth + 1);
  };
  const bx = .172 * W, by = H + 12 * U, fx = .187 * W, fy = py(.66);
  add(bx, by, fx, fy, -.05, 8 * U, 0, false);                 // the trunk
  const trunk = limbs[0];
  for (const [a, len, w, at] of FAN) grow(limbX(trunk, at), limbY(trunk, at), a, BAND * len, w * U, 1);
  treeCell = 1.5 * U; treeCols = Math.ceil(W / treeCell) + 1;
  const rows = Math.ceil(H / treeCell) + 1;
  treeMask = new Uint8Array(treeCols * rows);
  let x0 = W, y0 = H, x1 = 0, y1 = 0;
  limbWeight = limbLength = 0;
  for (const s of limbs) {
    s.wt = s.len * (s.w + 2 * U); limbWeight += s.wt; limbLength += s.len;
    const n = Math.ceil(s.len / (treeCell * .5)), rad = s.w / 2 + .3 * U, m = Math.ceil(rad / treeCell);
    for (let i = 0; i <= n; i++) {
      const px = limbX(s, i / n), qy = limbY(s, i / n), ix = Math.floor(px / treeCell), iy = Math.floor(qy / treeCell);
      for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
        const cx = ix + dx, cy = iy + dy;
        if (cx < 0 || cy < 0 || cx >= treeCols || cy >= rows || Math.hypot(dx, dy) * treeCell > rad + treeCell * .5) continue;
        treeMask[cy * treeCols + cx] = 1;
      }
      if (px - rad < x0) x0 = px - rad; if (px + rad > x1) x1 = px + rad; if (qy - rad < y0) y0 = qy - rad; if (qy + rad > y1) y1 = qy + rad;
    }
  }
  treeBox[0] = Math.max(0, x0); treeBox[1] = Math.max(0, y0); treeBox[2] = Math.min(W, x1); treeBox[3] = Math.min(H, y1);
}
function inTree(x, y) {
  if (x < treeBox[0] || x >= treeBox[2] || y < treeBox[1] || y >= treeBox[3]) return false;
  return treeMask[((y / treeCell) | 0) * treeCols + ((x / treeCell) | 0)] === 1;
}
// How far a stroke through (x, y) may run either way along (ca, sa) before it crosses a limb:
// fills reach[], false if the point itself is on the tree.
function treeReach(x, y, ca, sa, l) {
  reach[0] = reach[1] = l;
  if (x + l < treeBox[0] || x - l > treeBox[2] || y + l < treeBox[1] || y - l > treeBox[3]) return true;
  if (inTree(x, y)) return false;
  for (let d = treeCell; d < l; d += treeCell) if (inTree(x + ca * d, y + sa * d)) { reach[1] = d - treeCell; break; }
  for (let d = treeCell; d < l; d += treeCell) if (inTree(x - ca * d, y - sa * d)) { reach[0] = d - treeCell; break; }
  return true;
}
function treeStroke() {                                        // a stretch of bark
  const R = PP.R; let k = R() * limbWeight, s = limbs[0];
  for (const t of limbs) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (10 + R() * 22) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span;
  const scene = at(limbX(s, tm), Math.min(H - 1, limbY(s, tm))); if (!scene) return;
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 4 ? .34 : .2), q = R();
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.3 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (traits.bare) { if (s.w > 1.5 * U && q < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.folLight, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax + s.w * .22, ay, bx + s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .55), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });   // the sunny side
  }
  if (s.tip && t1 > .8 && !traits.bare) {                      // what the season hangs on the twigs
    const se = scene.season;
    if (se === 'autumn' ? s.k < .4 : s.k < .8)
      PP.dab(s.x2, s.y2, (2.2 + s.k * 2) * U, se === 'spring' ? pick(pal.accent, s.k) : se === 'autumn' ? pick(pal.bits, s.k * 2.5) : s.k < .4 ? pal.folLight : pal.folMid, { a: .9, den: 1.2, core: .7, s: 1.4 * U });
  }
}

// ---------- landmarks ----------
// The few buildings one can tell apart from up here.  They are part of the painting like
// everything else: patches of the town where the same small strokes take another colour.
// Positions and sizes are those of the photograph: x as a fraction of the width, heights
// as fractions of the plain's depth (0 the horizon, 1 its foot), widths in the same unit.
// Nothing rises above the horizon: even the Donauturm only just reaches it.
const BRIDGES = [[0, .237, .725, 1], [.104, .144, .573, 0], [.194, .273, .573, 0], [.235, .302, .514, 0], [.262, .325, .432, 2], [.246, .347, .335, 0]];
const SLABS = [
  [.075, .25, .34, .07, 'towerLight'], [.09, .24, .34, .05, 'towerLight'], [.103, .25, .34, .05, 'tower'], [.132, .25, .35, .08, 'towerLight'],   // Donau City
  [.152, .2, .35, .1, 'glass'], [.167, .19, .35, .09, 'towerLight'], [.185, .26, .35, .12, 'tower'], [.204, .22, .35, .1, 'glass'],
  [.218, .03, .32, .09, 'dark'],                                                                       // DC Tower
  [.33, .17, .44, .065, 'glass'],                                                                      // Millennium Tower
  [.437, .39, .55, .045, 'towerLight'], [.4625, .41, .53, .035, 'dark'], [.595, .42, .53, .1, 'tower'], [.55, .6, .66, .08, 'yellow'],
  [.683, .17, .3, .05, 'tower'], [.705, .19, .3, .06, 'glass'], [.815, .05, .13, .022, 'towerLight'], [.823, .06, .13, .022, 'towerLight'],
];
let town = [], townWeight = 0;
function buildTown() {
  const b = BAND, yb = f => HZ + b * f, lw = Math.max(.7, b / 130);
  town = [];
  const add = (kind, x0, y0, x1, y1, col, f, o) => town.push(Object.assign({ kind, x0, y0, x1, y1, col, f, lw }, o));
  const block = (xf, fTop, fBase, w, col, o) => add('block', xf * W - w * b / 2, yb(fTop), xf * W + w * b / 2, yb(fBase), col, fBase, o);
  const thin = (x, fTop, fBase, w, col, o) => add('block', x - w * lw / 2, yb(fTop), x + w * lw / 2, yb(fBase), col, fBase, o);
  const dot = (x, y, r, col, f, o) => add('dab', x - r, y - r, x + r, y + r, col, f, o);
  for (const [x0, x1, f, kind] of BRIDGES) {                   // bridges across the Danube
    const h = (kind === 1 ? 1.8 : 1.2) * lw;
    add('block', x0 * W, yb(f) - h / 2, x1 * W, yb(f) + h / 2, kind === 2 ? 'cabin' : 'towerLight', f, { piers: kind === 1, lit: .5 });
  }
  block(.011, .34, .58, .12, 'towerLight', { lit: .6 });       // Florido Tower, far left
  block(.017, .565, .61, .36, 'towerLight');
  for (const [xf, top, base, w, col] of SLABS) block(xf, top, base, w, col, { lit: .6 });
  let x = .116 * W;                                            // Donauturm
  thin(x, .19, .385, 2, 'tower'); dot(x, yb(.175), b * .022, 'tower', .3, { lit: .5 }); thin(x, .02, .16, .9, 'tower');
  dot(x, yb(.02), 1.3 * U, 'beacon', 1, { night: true });
  dot(.218 * W, yb(.03), 1.3 * U, 'beacon', 1, { night: true });
  x = .33 * W; thin(x, .1, .17, .8, 'tower'); dot(x, yb(.1), 1.3 * U, 'beacon', 1, { night: true });
  for (const xf of [.494, .505]) thin(xf * W, .04, .18, 1, 'towerLight');        // two chimneys beyond the Prater
  add('ring', .512 * W - b * .034, yb(.29) - b * .034, .512 * W + b * .034, yb(.29) + b * .034, 'towerLight', 1);   // Riesenrad
  x = .446 * W; thin(x, .38, .5, 1.3, 'towerLight'); dot(x, yb(.36), 1.7 * lw, 'gold', 1);   // the golden ball left of it
  x = .773 * W;                                                // Stephansdom: a slender dark spire, a pale roof beside it
  add('block', x - b * .085, yb(.5), x - b * .012, yb(.55), 'dark', .55);
  add('spire', x - b * .018, yb(.32), x + b * .018, yb(.55), 'stone', .7, { lit: .15 });
  add('block', x + b * .035, yb(.515), x + b * .165, yb(.555), 'towerLight', .55);
  for (const dx of [-1, 1]) thin(.822 * W + dx * b * .012, .63, .77, 1, 'sand');   // two churches in the suburbs in front
  x = .596 * W; add('block', x - b * .05, yb(.85), x + b * .045, yb(.9), 'roof', 1); thin(x - b * .04, .79, .86, 1, 'towerLight');
  const s = b / 120, hx = .52 * W, hy = py(.597);              // the farm among the vineyards
  add('block', hx - 9 * s, hy - 7 * s, hx + 9 * s, hy + s, 'wall', 1, { lit: .5 });
  add('block', hx - 10.5 * s, hy - 12 * s, hx + 10.5 * s, hy - 6.5 * s, 'farmRoof', 1);
  thin(hx + 4.3 * s, 0, 0, 2, 'stone'); { const c = town[town.length - 1]; c.y0 = hy - 15 * s; c.y1 = hy - 11 * s; c.f = 1; }
  townWeight = 0;                                              // bigger ones need more strokes to stay covered
  for (const t of town) { t.w = clamp((t.x1 - t.x0) * (t.y1 - t.y0) / (30 * U * U), 1, 7); townWeight += t.w; }
}
function townAt(x, y, padX, padY) {                            // the landmark here (or within reach), if any
  for (const s of town) if (x > s.x0 - padX && x < s.x1 + padX && y > s.y0 - padY && y < s.y1 + padY && !(s.night && !night)) return s;
  return null;
}
function townReach(x, y, l) {                                  // how far a level stroke may run before it paints over a landmark
  for (const s of town) if (y > s.y0 && y < s.y1 && !(s.night && !night)) {
    if (x < s.x0) { if (s.x0 - x < l) l = s.x0 - x; } else if (x - s.x1 < l) l = x - s.x1;
  }
  return l;
}
// one stroke of a landmark, at a point inside it
function landmark(s, x, y) {
  const R = PP.R, w = s.x1 - s.x0, h = s.y1 - s.y0, cx = s.x0 + w / 2, cy = s.y0 + h / 2, lw = s.lw;
  if (fol(x, y) > -.05) return;
  let col = s.col === 'farmRoof' ? (traits.bare ? pal.fieldLight : mix(pal.roof, pal.dark, .55)) : s.col === 'sand' ? pal.city[7] : pal[s.col];
  if (s.col !== 'beacon' && s.col !== 'gold') col = mix(col, pal.haze, .1 + clamp(1 - s.f) * .32);     // blue with distance
  const sz = (.5 + clamp(s.f) * 1.1) * U, o = { a: .9, jit: 9, dry: .05, bow: 0, sp: 1.1 * U };
  if (s.kind === 'dab') PP.dab(cx, cy, w / 2, col, { a: 1, den: 2.5, core: .9, s: U, jit: 4 });
  else if (s.kind === 'ring') {                                // a bit of the rim, now and then a spoke or a leg
    const a = R() * TAU, r = w / 2, px = cx + Math.cos(a) * r, qy = cy + Math.sin(a) * r, l = r * .5, k = R();
    if (night) { PP.dab(px, qy, .6 * U, pal.lights[1], { a: 1, den: 3, core: 1, s: U }); return; }
    if (k > .8) PP.dab(cx, cy, r * .6, mix(pal.folDark, pal.haze, .3), { a: .8, den: 1.5, core: .7, s: U });   // the trees of the Prater show through
    o.sp = lw;
    PP.brush(px - Math.sin(a) * l, qy + Math.cos(a) * l, px + Math.sin(a) * l, qy - Math.cos(a) * l, lw, col, o);
    o.sp = .45 * lw; o.a = .6;
    if (k < .25) PP.brush(cx, cy, px, qy, .45 * lw, col, o);
    else if (k < .4) PP.brush(cx, cy, cx + (k < .32 ? -1 : 1) * r * .5, cy + r * 1.25, .6 * lw, col, o);
  } else if (s.kind === 'spire' || w <= 3 * lw) {              // slender: short upright strokes
    const top = s.kind === 'spire' ? s.y0 + Math.abs(x - cx) / (w / 2) * h * .85 : s.y0, l = (3 + R() * 6) * sz;
    if (y < top) y = top + R() * (s.y1 - top);
    const bw = s.kind === 'spire' ? Math.max(lw, w * .3) : w;
    o.sp = bw;
    PP.brush(s.kind === 'spire' ? x : cx, Math.max(top, y - l), s.kind === 'spire' ? x : cx, Math.min(s.y1, y + l), bw, col, o);
  } else if (h <= 3 * lw) {                                    // a bridge: short level strokes, a pier now and then
    const l = (6 + R() * 12) * sz, xa = Math.max(s.x0, x - l);
    o.sp = h;
    PP.brush(xa, cy, Math.min(s.x1, x + l), cy, h, col, o);
    if (s.piers && R() < .5) { o.sp = lw; PP.brush(xa, cy, xa, cy + 3.4 * lw, lw, col, o); }
  } else {                                                     // a building: the same little dashes the town is made of
    const l = Math.min((2 + R() * 5) * sz, w / 2), t = Math.min((1.6 + R() * 2.4) * sz, h);
    const px = clamp(x, s.x0 + l, s.x1 - l), qy = clamp(y, s.y0 + t / 2, s.y1 - t / 2);
    PP.brush(px - l, qy, px + l, qy, t, mix(col, R() < .5 ? pal.towerLight : pal.dark, R() * .16), o);
  }
  if (night && s.lit && R() < s.lit * .3) light(s.x0 + R() * w, s.y0 + R() * h, (.5 + R() * .4) * U);
}
function townStroke() {
  const R = PP.R; let k = R() * townWeight, s = town[0];
  for (const t of town) { s = t; k -= t.w; if (k <= 0) break; }
  const x = s.x0 + R() * (s.x1 - s.x0), y = s.y0 + R() * (s.y1 - s.y0);
  if (at(x, y) && !(s.night && !night)) landmark(s, x, y);
}

function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y0 = HZ + BAND, y = y0 + R() * (H - y0);
  if (!at(x, y) || !traits.glint || y < ridgeY(x) || fol(x, y) > -.05 || inTree(x, y)) return;
  hillAt(x, y);
  if (hs.hedge || hs.lum < .45) return;
  PP.dab(x, y, (.7 + R() * .9) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

function farStroke() {
  const R = PP.R, x = R() * W, fh = farH(x), y = HZ - R() * fh;
  if (!at(x, y)) return;
  const l = (14 + R() * 40) * U;
  PP.brush(x - l, y, x + l, y + (R() - .5) * U, (1.2 + R() * 1.6) * U, mix(pal.haze, pal.skyLow, R() * .3 + (HZ - y) / fh * .2), { a: .8, jit: 5, dry: .15, sp: 1.2 * U, bow: .02 });
}

function cityStroke() {
  const R = PP.R, x = R() * W, ry = ridgeY(x), y = HZ + R() * (ry - HZ);
  if (!at(x, y)) return;
  if (fol(x, y) > -.1 || inTree(x, y)) return;
  const f = clamp((y - HZ) / (ry - HZ)), sz = (.5 + f * 1.1) * U;
  const mark = townAt(x, y, 0, 0);
  if (mark) { landmark(mark, x, y); return; }
  if (riverAt(x, f)) {
    const sh = vnoise(x * .03 / U + time * .5, y * .4 / U), l = townReach(x, y, (8 + R() * 26) * sz);
    PP.brush(x - l, y, x + l, y + (R() - .5) * 1.5 * U, (1.5 + R() * 2.5) * sz, mix(pal.water, sh > .5 ? pal.waterLight : pal.waterDark, Math.abs(sh - .5) * 1.5),
      { a: .85, jit: 6, dry: .15, sp: 1.1 * U, bow: 0 });
    if (night && R() < .1) PP.brush(x, y - 2 * sz, x, y + 3 * sz, 1.4 * U, pick(pal.lights, R()), { a: .5, sp: U, bow: 0 });
    return;
  }
  // beyond the river lie woods and gardens with pale houses in them, all of it blue with distance
  const xf = x / W, haze = clamp(1 - f) * .5, beyond = channel(RIVER, xf) && f < chC;
  if (f < .16) {                                               // the far end of the plain dissolves in the haze
    const l = townReach(x, y, (6 + R() * 18) * U), woods = beyond || R() < smooth(.62, .5, xf), k = R();   // dark woods behind the river and the Prater
    PP.brush(x - l, y, x + l, y, (1.4 + R() * 2) * U, woods ? mix(mix(pal.cityGreen, pal.haze, .7), pal.cityFar, R() * .35) : mix(pal.cityFar, k < .4 ? pal.city[0] : k < .7 ? pal.haze : pal.cityGreen, R() * (k < .7 ? .6 : .3)), { a: .8, jit: 8, dry: .1, sp: 1.1 * U, bow: 0 });
    if (R() < (woods ? .15 : .6) && !townAt(x, y, 2 * U, 2 * U)) PP.dab(x, y, (1 + R()) * U, R() < .7 ? pal.city[0] : pal.cityFar, { a: .6, den: 1.2 });
    if (night && R() < (woods ? .15 : .5)) light(x + (R() - .5) * l, y, (.5 + R() * .5) * U);
    return;
  }
  let g = fbm(x * .008 / U + 3, y * .03 / U + 7) + smooth(.62, 1, f) * .28 + (beyond ? .2 : 0) + (R() - .5) * .22;   // garden suburbs in front
  if (xf > .43 && xf < .55 && f > .17 && f < .31) g += .5;     // the Prater
  const green = g > .56;
  if (green) {                                                 // parks, gardens, the woods along the river
    const r = (1.6 + R() * 2.4) * sz;
    if (townAt(x, y, r * .7, r * .7)) return;
    PP.haze = [pal.haze, haze * (beyond ? 1.3 : 1)];
    bush(x, y, r);
    PP.haze = null;
  } else {
    let c = pick(pal.city, R());
    if ((c === pal.city[2] || c === pal.city[3]) && (beyond || R() > f * .75)) c = pal.city[R() < .6 ? 0 : 5];   // red roofs only show close by
    const l = townReach(x, y, (2 + R() * 5) * sz);
    PP.brush(x - l, y, x + l, y, (1.6 + R() * 2.4) * sz, mix(c, pal.cityFar, haze * .9), { a: .9, jit: 9, dry: .05, sp: 1.1 * U, bow: 0 });
    if (!beyond && R() < f * f * .3) PP.brush(x - l, y - 1.6 * sz, x + l * .6, y - 1.6 * sz, 1.3 * sz, pal.city[2 + (R() * 2 | 0)], { a: .8, sp: U, bow: 0 });   // a roof
  }
  if (night && R() < (green ? .12 : beyond ? .3 : .55)) light(x + (R() - .5) * 6 * sz, y + (R() - .5) * 3 * sz, (.55 + R() * .7) * U);
}

function hillStroke() {
  const R = PP.R, x = R() * W, y0 = HZ + BAND * .8, y = y0 + R() * (H - y0);
  if (!at(x, y)) return;
  const ry = ridgeY(x);
  if (y < ry || fol(x, y) > -.05 || townAt(x, y, 10 * U, 4 * U) || inTree(x, y)) return;
  hillAt(x, y);
  const li = hs.li, id = hs.id, sz = (.55 + li * .3) * U;
  if (hs.hedge) { bush(x, y, (2.5 + R() * 3.5) * sz); return; }
  if (hs.path) {                                               // a track along the slope
    const a = Math.atan(hs.slope), l = (5 + R() * 9) * sz;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (1.6 + li * .7) * U, mix(pal.earth, pal.fieldLight, .25 + R() * .3), { a: .8, jit: 6, dry: .1, sp: 1.1 * U, bow: .03 });
    return;
  }
  const col = fieldColor();
  if (hs.type === 0) {                                         // vines: every stroke sits on a row
    const sp = (3.6 + li * 2) * U, ca = Math.cos(hs.ang), sa = Math.sin(hs.ang);
    const n = -x * sa + y * ca, k = Math.round(n / sp), off = k * sp - n;
    const px = x - sa * off, py = y + ca * off; let l = (12 + R() * 28) * sz;
    hillAt(px + ca * l, py + sa * l); if (hs.id !== id) l *= .4;
    else { hillAt(px - ca * l, py - sa * l); if (hs.id !== id) l *= .4; }
    if (py - Math.abs(sa) * l < ry + 2 * U) l *= .4;
    if (!treeReach(px, py, ca, sa, l)) return;
    PP.brush(px - ca * reach[0], py - sa * reach[0], px + ca * reach[1], py + sa * reach[1], (k & 1) ? sp * .42 : sp * .8, (k & 1) ? mix(col, pal.row, .78) : mix(col, pal.fieldLight, .18), { a: .84, jit: 8, dry: .08, sp: 1.2 * U, bow: .02, dash: 12 * U });
  } else {
    const a = hs.ang + (R() - .5) * .25; let l = (14 + R() * 34) * sz;
    const ca = Math.cos(a), sa = Math.sin(a);
    hillAt(x + ca * l, y + sa * l); if (hs.id !== id) l *= .4;
    if (y - Math.abs(sa) * l < ry + 2 * U) l *= .4;
    if (!treeReach(x, y, ca, sa, l)) return;
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (5 + R() * 8) * sz, mix(col, R() < .5 ? pal.fieldLight : pal.fieldShadow, R() * .22), { a: .62, jit: 9, dry: .3, dash: 16 * U });
  }
}

function foliageDab() {
  const R = PP.R, x = R() * W, y = treeLine + R() * (H - treeLine);
  if (!at(x, y)) return;
  const d = fol(x, y); if (d < -.08 || inTree(x, y)) return;
  const lum = folLum(x, y), sz = (.8 + (y / H - .5) * 1.5) * U, k = R();
  if (d < .12) {                                               // the ragged rim: single leaves
    PP.dab(x, y, (3 + R() * 6) * sz, traits.bare ? pal.folDark : lum > .5 ? pal.folMid : pal.folDark, { a: .85, den: .3, s: 3 * U });
    return;
  }
  if (traits.bare) {                                           // twigs, and the snow lying on them
    if (lum > .42 && k < .2 + lum * .6) { PP.dab(x, y, (4 + R() * 8) * sz, lum > .6 ? pal.folSun : pal.folLight, { a: .9, den: .6, core: .55, sx: 1.7, sy: .55 }); return; }
    const a = -Math.PI / 2 + (R() - .5) * 2, l = (8 + R() * 20) * sz;
    if (inTree(x + Math.cos(a) * l * .5, y + Math.sin(a) * l * .5) || inTree(x + Math.cos(a) * l, y + Math.sin(a) * l)) return;
    if (k > .6) PP.dab(x, y, (5 + R() * 6) * sz, mix(pal.folMid, pal.folLight, .25 + lum * .5), { a: .8, den: .5, core: .5, s: 2 * U });
    PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 5 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
      lum < .35 ? pal.folDeep : k < .8 ? pal.folDark : pal.folMid, (1 + R() * 1.5) * sz, { pass: 1, a: .8 });
    return;
  }
  let col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .6 && k < .3) col = pal.folSun; else if (k > .92 && pal.accent && lum > .3) col = pick(pal.accent, R());
  const a = R() * Math.PI, l = (4 + R() * 9) * sz, ca = Math.cos(a), sa = Math.sin(a);
  if (!treeReach(x, y, ca, sa, l)) return;
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (4 + R() * 7) * sz, col, { a: .85, sp: 1.5 * U, dry: .2, jit: 10 });
}

function coatStroke(x, y) {                                    // a broad stroke of a fresh coat
  const R = PP.R;
  if (fol(x, y) > 0) {
    const lum = folLum(x, y), a = R() * Math.PI, l = 26 * U;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (40 + R() * 20) * U,
      traits.bare ? mix(pal.folMid, pal.folLight, clamp(lum * 1.1) * .6) : mix(pal.folDeep, lum > .5 ? pal.folMid : pal.folDark, clamp(lum * 1.3)), { a: .93, jit: 9, dry: 0, full: 1, sp: 2.4 * U });
  } else if (y < ridgeY(x)) {
    const f = (y - HZ) / (ridgeY(x) - HZ), l = (40 + R() * 40) * U;
    const col = riverAt(x, f) ? pal.water : mix(mix(pal.city[1], pal.cityGreen, .4), pal.cityFar, clamp(1 - f) * .6);
    PP.brush(x - l, y, x + l, y, (36 + R() * 12) * U, col, { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .03 });
  } else {
    hillAt(x, y);
    const a = Math.atan(hs.slope), l = (40 + R() * 40) * U;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (42 + R() * 14) * U, hs.hedge ? pal.folDark : fieldColor(), { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .04 });
  }
}

function drawSmoke(P, t, cur) {                                // the farm has its stove going when it is cold
  if (cur.season !== 'winter' && cur.season !== 'autumn') return;
  const s = BAND / 120, x0 = .52 * W + 4.3 * s, y0 = py(.597) - 15 * s, g = wind(t);
  if (!at(x0, y0) || fol(x0, y0) > -.1) return;
  for (let j = 1; j <= 8; j++) {
    const x = x0 + (Math.sin(t * 1.6 + j * .9) * (1 + j) - j * j * .5 - g * j * 1.6) * s, y = y0 - j * 5.5 * s;
    P.dab(x, y, (2 + j * 1.2) * s, pal.cloud, { a: .55 * (1 - j / 9), den: .6, core: .22 * (1 - j / 9), s: 2.2 * s, jit: 4 });
  }
}

// ---------- the painting, one frame's worth ----------
const RATE = { fog: 1500, glint: 90, far: 260, city: 9000, town: 1.3, hill: 3600, fol: 3200, tree: 1.2 };
const place = {
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const wide = clamp((W / H - 1) / .6); Y0 = .1 * wide; Y1 = 1 - .15 * wide;
    HZ = place.hz = py(.419); BAND = py(.522) - HZ; HILL = H - HZ - BAND; LX = Math.max(W, H * 1.2);
    buildCrowns(); buildTown(); buildTree();
  },
  tick(t) { time = t; },
  top: x => HZ - farH(x) - U,                                  // the sky ends here
  sky(c, pad) { c.rect(0, 0, W, Math.max(0, HZ - pad)); },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    clipBand(HZ - 8 * U, H);
    spend('far', RATE.far * W / (1000 * U) * pace, farStroke);
    spend('city', RATE.city * W * BAND / ua * pace, cityStroke);
    spend('town', RATE.town * townWeight * pace, townStroke);
    spend('hill', RATE.hill * W * HILL * .6 / ua * pace, hillStroke);
    spend('fol', RATE.fol * W * (H - treeLine) * .7 / ua * pace, foliageDab);
    spend('tree', RATE.tree * limbLength / (14 * U) * pace, treeStroke);
    if (s && s.traits.glint) spend('glint', RATE.glint * W * HILL / ua * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('fog', RATE.fog * W * BAND * 2.3 / ua * pace, fogStroke);
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat
    use(s);
    clipBand(HZ, H);
    for (let k = i; k < j; k++) if (cells[k].y >= HZ - 30 * U) coatStroke(cells[k].x, Math.max(cells[k].y, HZ));
    PP.c.restore();
  },
  sprites: [{ draw: drawSmoke, t0: 4, seed: 103 }],
};
return place;
} });
