/* ------------------------------------------------------------------ *
 *  Vienna, at the foot of the Thurnstiege: the stairs the Thurngasse
 *  climbs to the Währinger Straße, seen from the Wasagasse on a day in
 *  March.  Rails zigzag up between the flights, a lamp hangs over them
 *  on its wires, a tree stands at their head.  The sun is on the old
 *  house on the right; the stairs lie in its shade.
 *
 *  Like the Freie Mitte this one is painted after a photograph:
 *  thurngasse-map.js, loaded before this file, says for every point
 *  what there is (a wall, a window, a step), how light it is there
 *  without the sun, and where the sun falls when it shines.  The rails
 *  are too thin for that: the map has them as lines, and the corners of
 *  the windows that are lit at night.  The trees, the lamp and the
 *  street sign are drawn by hand.  Nobody is on the stairs.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'thurngasse', name: 'Thurngasse', make(kit) {
'use strict';
const { PP, clamp, smooth, mix, pick, rng, vnoise, fbm, wind, spend, clipBand } = kit;
const MAP = window.viennaMaps.thurngasse;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false, sunny = false;
const use = s => { pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; sunny = !night && !traits.veiled; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph, which is square: x and y as fractions of its width and height.  A wide
// screen shows most of its height and goes on beyond it on both sides, a narrow one shows a slice of its width.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
const HOR = .355;                                              // the roof at the end of the street
const HEAD = .487;                                             // the head of the stairs: the trees stand behind it
const corner = pf => pf < .095 ? .2545 + pf * .195 : .2728;   // the corner of the house on the left: it hides what is behind it
// Beyond the photograph everything goes on as it ended: on the right the bare wall of the house, the steps, the
// pavement.  On the left the balconies and the stairs end where the photograph does: from there on it is the wall
// of the house, and its dark ground floor down to the pavement.
const BESIDE = .06, FOOT = .556;
const fold = xf => xf < 0 ? BESIDE : xf > .992 ? .992 : xf;
const foldY = (xf, pf) => xf < 0 && pf > FOOT && pf < .8 ? FOOT : pf;

// ---------- the map ----------
// One character a cell: what it is, times eight, plus how light it is, from 0 to 7.
const SKY = 0, WALL = 1, CREAM = 2, YELLOW = 3, WARM = 4, GLASS = 5, STEP = 6, PAVE = 7, ROAD = 8, RAIL = 10, LIMB = 11, LEAVES = 12, PLATE = 13, PANE = 14;
const CODE = new Uint8Array(128);
for (let c = 35, v = 0; c < 127; c++) if (c !== 92) CODE[c] = v++;
const MW = MAP.w, MH = MAP.h, SWD = MAP.sun.w, SHT = MAP.sun.h;
const cells = new Uint8Array(MW * MH), suns = new Uint8Array(SWD * SHT);
for (let i = 0; i < cells.length; i++) cells[i] = CODE[MAP.cells.charCodeAt(i)];
for (let i = 0; i < suns.length; i++) suns[i] = MAP.sun.cells.charCodeAt(i) - 48;
const roofs = new Uint16Array(MW);                             // how far down the sky comes in each column
for (let i = 0; i < MW; i++) { let j = 0; while (j < MH && cells[j * MW + i] >> 3 === SKY) j++; roofs[i] = j; }

let lev = 0, cellJ = 0;                                        // what what() learnt on the way: how light it is, which row
function what(xf, pf) {                                        // what there is at a point of the photo
  const i = clamp(fold(xf) * MW | 0, 0, MW - 1), j = clamp(foldY(xf, pf) * MH | 0, 0, MH - 1), v = cells[j * MW + i];
  cellJ = clamp(pf * MH | 0, 0, MH - 1); lev = v & 7; return v >> 3;
}
function sunAt(xf, pf) {                                       // how much of the sun gets there, when it shines
  const u = clamp(fold(xf) * SWD - .5, 0, SWD - 1.001), v = clamp(foldY(xf, pf) * SHT - .5, 0, SHT - 1.001), i = u | 0, j = v | 0, a = u - i, b = v - j, o = j * SWD + i;
  return ((suns[o] * (1 - a) + suns[o + 1] * a) * (1 - b) + (suns[o + SWD] * (1 - a) + suns[o + SWD + 1] * a) * b) / 7;
}

// ---------- the light ----------
// By day the sun is on the houses on the right, unless a cloud is in its way; under a grey sky everything is as
// light as everything else.  At night the two lamps each throw a pool of light on the stairs, in which stone is
// stone again, only yellow; and where the sun was, the moon is, if it is out.
const LAMP = [.463, .192], FAR_LAMP = [.4325, .327];
const POOLS = [[.455, .66, .27, .75, .95], [.432, .5, .1, 1, .6]];     // [x, y, how far around, how flat, how bright]
const shadow = (x, y) => traits.shadow ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
function glowAt(xf, pf) {
  let g = 0;
  for (const l of POOLS) { const d = Math.hypot(xf - l[0], (pf - l[1]) * l[3]) / l[2]; if (d < 1.8) g += Math.exp(-d * d * 1.7) * l[4]; }
  return g > 1 ? 1 : g;
}
let sunK = 0, glow = 0, moon = 0, tl = 0;                      // what toneOf() learnt on the way: the sun, the lamplight and the moon there; how light the thing is
function toneOf(x, y, xf, pf, l) {                             // where on its scale a thing of lightness l (0 to 8) is painted
  const t = tl = l / 8;
  sunK = glow = moon = 0;
  if (night) {
    glow = glowAt(xf, pf);
    if (!traits.veiled) moon = sunAt(xf, pf) * .6;
    return t * (.6 + moon * .3) + .07;
  }
  if (!sunny) return t * .7 + .05;
  sunK = sunAt(xf, pf);
  if (sunK) sunK *= 1 - clamp(shadow(x, y) * 2.2);
  return t * (1 / MAP.gain + (1 - 1 / MAP.gain) * sunK) + .03;
}
const EMBER = [36, 23, 14];                                    // what is dark is dark under a lamp too
function lightOn(col) {
  if (sunK > 0) return mix(col, pal.sunlight, sunK * .1);
  if (moon > 0) col = mix(col, pal.moonGlow, moon * .14);
  return glow > 0 ? mix(col, mix(EMBER, pal.lights[0], clamp(tl + .06) * .72), Math.min(.85, glow * 1.1)) : col;
}
// what is far up the street is paler, and in fog or rain all the more
const farOff = (xf, pf) => smooth(.3, .03, Math.hypot(xf - .41, (pf - .43) * 1.25));
function hazed(col, xf, pf) {
  const f = farOff(xf, pf), h = f * .2 + traits.fog * (.14 + f * .62) + traits.rain * f * .25;
  return h > 0 ? mix(col, pal.cityFar, clamp(h, 0, .85)) : col;
}
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}
const scaleOf = k => k === WALL ? pal.walls : k === CREAM ? pal.creams : k === YELLOW ? pal.yellows : k === WARM ? pal.reds
  : k === STEP ? pal.steps : k === PAVE ? pal.pave : k === ROAD ? pal.road : pal.panes;

// ---------- the trees ----------
// One at the head of the stairs, one behind the house on the left: [x, foot, top, how wide it spreads].  Leaves
// are a heap of crowns, one at the end of every limb.  Both are kept in coarse grids: where the limbs are (1), which
// other strokes have to keep clear of as they do of the rails (2), and how deep in the leaves a point is.
const TREES = [[.413, .494, .165, .78], [.298, .5, .2, .62]];
let limbs = [], limbWeight = 0, limbLength = 0, mask = null, mCols = 0, mRows = 0, mCell = 2;
let crown = null, cCols = 0, cRows = 0, cCell = 4, cX = 0, cY = 0, leafArea = 0;
let treeX0 = 0, treeX1 = 0, treeTop = 0, treeFoot = 0;       // the box they stand in
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const hidden = (x, y) => { const xf = fx(x), pf = fy(y); return pf > HEAD || xf < corner(pf) + .001 || xf > .52; };
const inside = (p, x, y) => { let o = false; for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o; return o; };
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function buildTrees() {
  const crowns = []; limbs = [];
  let r = rng(5);
  const add = (x1, y1, x2, y2, bend, w, depth) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, len });
  };
  const grow = (x, y, a, len, w, depth, rad) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len, last = depth >= 5 || len <= 4 * U;
    add(x, y, x2, y2, (r() - .5) * .3, w, depth);
    crowns.push({ x: x2, y: y2, r: rad * (depth < 3 ? .8 + r() * .5 : depth < 4 ? .7 : .5) });
    if (last) return;
    const k = r() < .75 ? 2 : 3;
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .62 + (r() - .5) * .45, len * (.58 + r() * .2), Math.max(.7 * U, w * .6), depth + 1, rad);
  };
  for (const t of TREES) {
    r = rng(31 + Math.round(t[0] * 1000));                     // every tree keeps its shape, whatever the view
    const x = px(t[0]), y = py(t[1]), h = (t[1] - t[2]) * PH, w = Math.max(1.5 * U, h * .017), rad = h * .068;
    add(x, y, x + (r() - .5) * h * .05, y - h * .42, (r() - .5) * .06, w, 0);
    const trunk = limbs[limbs.length - 1];
    for (let i = 0; i < 5; i++) {
      const a = -Math.PI / 2 + ((i + .5) / 5 - .5) * 2.2 * t[3] + (r() - .5) * .2, from = i === 2 ? 1 : .6 + r() * .4;
      grow(limbX(trunk, from), limbY(trunk, from), a, h * (.27 - Math.abs(a + Math.PI / 2) * .05) * (.85 + r() * .3), w * .55, 1, rad);
    }
  }
  let x0 = 1e9, x1 = -1e9, y0 = 1e9;
  for (const c of crowns) { x0 = Math.min(x0, c.x - c.r); x1 = Math.max(x1, c.x + c.r); y0 = Math.min(y0, c.y - c.r); }
  treeX0 = x0 - 12 * U; treeX1 = x1 + 12 * U; treeTop = y0 - 12 * U; treeFoot = py(HEAD);
  // how deep in the leaves: 1 in the middle of a crown, 0 at its edge
  cCell = 2.5 * U; cX = treeX0; cY = treeTop; cCols = Math.ceil((treeX1 - treeX0) / cCell) + 1; cRows = Math.max(1, Math.ceil((treeFoot - treeTop) / cCell) + 1);
  crown = new Float32Array(cCols * cRows).fill(-1); leafArea = 0;
  for (const c of crowns) {
    const i0 = Math.max(0, Math.floor((c.x - c.r - cX) / cCell)), i1 = Math.min(cCols - 1, Math.ceil((c.x + c.r - cX) / cCell));
    const j0 = Math.max(0, Math.floor((c.y - c.r - cY) / cCell)), j1 = Math.min(cRows - 1, Math.ceil((c.y + c.r - cY) / cCell));
    for (let j = j0; j <= j1; j++) for (let i = i0; i <= i1; i++) {
      const q = 1 - Math.hypot(cX + (i + .5) * cCell - c.x, (cY + (j + .5) * cCell - c.y) * 1.1) / c.r;
      if (q > crown[j * cCols + i]) crown[j * cCols + i] = q;
    }
  }
  for (let j = 0; j < cRows; j++) for (let i = 0; i < cCols; i++) {
    if (hidden(cX + (i + .5) * cCell, cY + (j + .5) * cCell)) crown[j * cCols + i] = -1; else if (crown[j * cCols + i] > 0) leafArea += cCell * cCell;
  }
  // where the limbs are
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows); limbWeight = limbLength = 0;
  for (const s of limbs) {
    s.wt = s.len * (s.w + 2 * U); limbWeight += s.wt; limbLength += s.len;
    for (let i = 0, n = Math.ceil(s.len / (mCell * .5)); i <= n; i++) { const x = limbX(s, i / n), y = limbY(s, i / n); if (!hidden(x, y)) mark(x, y, s.w / 2 + .3 * U, 1); }
  }
}
function mark(x, y, rad, v) {                                  // something thin is here
  const ix = Math.floor(x / mCell), iy = Math.floor(y / mCell), m = Math.ceil(rad / mCell);
  for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
    const cx = ix + dx, cy = iy + dy;
    if (cx >= 0 && cy >= 0 && cx < mCols && cy < mRows && Math.hypot(dx, dy) * mCell <= rad + mCell * .5) mask[cy * mCols + cx] = v;
  }
}

// ---------- the windows ----------
// By day a window is what the map has there.  At night it has four corners and is lit or dark: the map has those
// too, corner by corner in ten-thousandths of the photograph, for the houses near by.  Other strokes keep clear
// of them then (3 and up: which window), and they are painted like everything else, stroke by stroke: upright
// ones for the panes, dark ones for the bars between them.
let panes = [], paneArea = 0;
function buildWindows() {
  const q = MAP.windows;
  panes = []; paneArea = 0;
  for (let i = 0; i < q.length && panes.length < 250; i += 9) {
    const pts = []; let x0 = 1e9, x1 = -1e9, y0 = 1e9, y1 = -1e9;
    for (let k = 0; k < 8; k += 2) { const x = px(q[i + k] / 1e4), y = py(q[i + k + 1] / 1e4); pts.push([x, y]); x0 = Math.min(x0, x); x1 = Math.max(x1, x); y0 = Math.min(y0, y); y1 = Math.max(y1, y); }
    if (x1 < 0 || x0 > W || y1 < 0 || y0 > H) continue;
    const w = { pts, lit: q[i + 8] === 1, x: (x0 + x1) / 2, y: (y0 + y1) / 2, w: Math.max(1, pts[1][0] - pts[0][0]), h: Math.max(1, pts[3][1] - pts[0][1]), k: (i * .377) % 1 };
    w.area = w.w * w.h; paneArea += w.area;
    for (let cy = Math.max(0, Math.floor(y0 / mCell)); cy <= Math.min(mRows - 1, y1 / mCell); cy++) for (let cx = Math.max(0, Math.floor(x0 / mCell)); cx <= Math.min(mCols - 1, x1 / mCell); cx++)
      if (inside(pts, (cx + .5) * mCell, (cy + .5) * mCell)) mask[cy * mCols + cx] = 3 + panes.length;
    panes.push(w);
  }
}
const along = (a, b, t) => [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t];
const CROSS = .36;                                             // how far down a window the bar across it is
function paneStroke(w) {                                       // somewhere in a window
  const R = PP.R, p = w.pts, xf = fx(w.x), pf = fy(w.y), bars = w.lit && w.w > 6 * U, bw = Math.max(U, w.w * .08);
  if (bars && R() < .22) {                                     // the bars between its panes, dark against the light
    const bar = hazed(mix(pal.dark, pal.lights[0], .22), xf, pf), o = { a: .92, jit: 3, dry: 0, bow: 0, sp: Math.min(1.1 * U, bw) };
    let a, b;
    if (R() < .5) { const t = R() * .6, c = along(p[0], p[1], .5), d = along(p[3], p[2], .5); a = along(c, d, t); b = along(c, d, t + .4); }
    else { a = along(p[0], p[3], CROSS); b = along(p[1], p[2], CROSS); }
    PP.brush(a[0], a[1], b[0], b[1], bw, bar, o);
    return;
  }
  // a pane: an upright stroke, from the edge of the window or the bar across it
  const sw = Math.min(w.w, (1.8 + R() * 1.6) * U), m = sw / 2 / w.w, u = m + R() * (1 - 2 * m), top = along(p[0], p[1], u), foot = along(p[3], p[2], u);
  let v0 = 0, v1 = 1;
  if (bars) { if (R() < CROSS) v1 = CROSS - bw / 2 / w.h; else v0 = CROSS + bw / 2 / w.h; }
  const span = v1 - v0, l = Math.min(span, (7 + R() * 10) * U / w.h), q = R(), v = v0 + (q < .3 ? 0 : q < .6 ? span - l : R() * (span - l));
  const a = along(top, foot, v), b = along(top, foot, v + l), k = R();
  const col = w.lit ? mix(mix(pal.lights[0], pal.lights[2], w.k), k < .5 ? pal.lights[4] : pal.lights[1], R() * .35)
    : mix(ramp(pal.panes, .06 + k * .1), pal.skyMid, .08 + R() * .12);   // nobody at home: the sky in its panes, what there is of it
  PP.brush(a[0], a[1], b[0], b[1], sw, hazed(col, xf, pf), { a: .95, jit: 5, dry: .03, bow: 0, sp: Math.min(1.1 * U, sw) });
}
function paneDab() {                                           // the windows once more: they are small, and the night is long
  const R = PP.R; let k = R() * paneArea, w = panes[0];
  if (!w) return;
  for (const t of panes) { w = t; k -= t.area; if (k <= 0) break; }
  if (at(clamp(w.x, 0, W - 1), clamp(w.y, 0, H - 1)) && night) paneStroke(w);
}

// ---------- the rails ----------
// The map has them as lines: from, to, how wide, how light, in ten-thousandths of the photograph.  The bollards
// on the pavement are among them.  A rail that leaves the photograph goes on as it left.
let rails = [], railWeight = 0, railLength = 0;
function buildRails() {
  const q = MAP.rails, left = X0 - .02, right = fx(W) + .02;
  rails = []; railWeight = railLength = 0;
  for (let i = 0; i < q.length; i += 6) {
    let xa = q[i] / 1e4, ya = q[i + 1] / 1e4, xb = q[i + 2] / 1e4, yb = q[i + 3] / 1e4;
    if (xa > xb) { let t = xa; xa = xb; xb = t; t = ya; ya = yb; yb = t; }
    if (Math.abs(yb - ya) < (xb - xa) * .3) { if (xb > .994 && right > 1) xb = right; if (xa < .006 && left < 0) xa = left; }
    const s = { x1: px(xa), y1: py(ya), x2: px(xb), y2: py(yb), w: Math.max(1.1 * U, q[i + 4] / 1e4 * PW), l: q[i + 5] };
    if (Math.max(s.x1, s.x2) < 0 || Math.min(s.x1, s.x2) > W || Math.min(s.y1, s.y2) > H) continue;
    s.len = Math.hypot(s.x2 - s.x1, s.y2 - s.y1) || 1; s.flat = Math.abs(s.y2 - s.y1) < Math.abs(s.x2 - s.x1);
    s.wt = s.len * (s.w + 2 * U); railWeight += s.wt; railLength += s.len;
    rails.push(s);
    for (let k = 0, n = Math.ceil(s.len / (mCell * .5)); k <= n; k++) mark(s.x1 + (s.x2 - s.x1) * k / n, s.y1 + (s.y2 - s.y1) * k / n, s.w / 2, 2);
  }
}
function railStroke() {                                        // a stretch of steel
  const R = PP.R; let k = R() * railWeight, s = rails[0];
  if (!s) return;
  for (const t of rails) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (8 + R() * 18) * U / s.len), t0 = R() * (1 - span), t1 = t0 + span;
  const ax = s.x1 + (s.x2 - s.x1) * t0, ay = s.y1 + (s.y2 - s.y1) * t0, bx = s.x1 + (s.x2 - s.x1) * t1, by = s.y1 + (s.y2 - s.y1) * t1, mx = (ax + bx) / 2, my = (ay + by) / 2;
  if (mx < 0 || mx >= W || my < 0 || my >= H || !at(mx, my)) return;
  const xf = fx(mx), pf = fy(my), behind = what(xf, pf);
  // what is behind it first, a little broader: no other stroke comes this close to a rail
  if (behind !== SKY) PP.brush(ax, ay, bx, by, s.w + 2.4 * U, hazed(lightOn(ramp(scaleOf(behind), toneOf(mx, my, xf, pf, lev + .5))), xf, pf), { a: .92, jit: 3, dry: 0, bow: 0, sp: 1.1 * U });
  const tone = toneOf(mx, my, xf, pf, s.l + .3 + R() * .6) + .14;
  tl = Math.min(1, tl + .3);                                   // steel shines under a lamp
  const col = hazed(lightOn(ramp(pal.steel, tone)), xf, pf);
  PP.brush(ax, ay, bx, by, s.w, col, { a: .95, jit: 4, dry: 0, bow: 0, sp: Math.min(1.1 * U, s.w) });
  if (traits.snow && s.flat && R() < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, Math.max(U, s.w * .5), pal.snowy, { a: .9, jit: 3, dry: .05, bow: 0, sp: U });   // snow lies on it
}
function fol(x, y) {                                           // > 0 inside the leaves
  const i = ((x - wind(time) * 2 * U - cX) / cCell) | 0, j = ((y - cY) / cCell) | 0;
  if (i < 0 || j < 0 || i >= cCols || j >= cRows) return -1;
  const q = crown[j * cCols + i];
  if (q <= -1) return -1;
  return (q - (season === 'autumn' ? .26 : season === 'spring' ? .16 : .08)) * 1.2 + (fbm(x * .03 / U + time * .02, y * .03 / U) - .47) * .6;   // in autumn half the leaves are down
}
function folLum(x, y) {                                        // light falls in from the upper right
  const s = time * .03, k = .02 / U;
  return .45 + (fbm(x * k + s, y * k * 1.2) - fbm((x + 9 * U) * k + s, (y - 9 * U) * k * 1.2)) * 3.2 + (sunny ? .08 : 0);
}
const inTrees = (x, y) => y < treeFoot && y > treeTop && x > treeX0 && x < treeX1 && fx(x) > corner(fy(y));

// ---------- whose a point is ----------
const SIGN = [.0795, .1285, .5445, .5625];                     // the street sign on the house on the left: left, right, top, foot
let zx = 0, zy = 0, zf = 0, zw = 0;                            // what zone() learnt on the way: x and y on the photo, how deep in the leaves, which window
function zone(x, y) {
  const xf = zx = fx(x), pf = zy = fy(y), m = maskAt(x, y);
  if (m === 2) return RAIL;
  if (m >= 3 && night) { zw = m - 3; return PANE; }
  if (inTrees(x, y)) {
    const f = zf = bare ? -1 : fol(x, y);
    if (m === 1 && f < .15) return LIMB;
    if (f > 0) return LEAVES;
  }
  if (xf > SIGN[0] && xf < SIGN[1] && pf > SIGN[2] && pf < SIGN[3]) return PLATE;
  return what(xf, pf);
}
const reach = [0, 0];
function run(x, y, ca, sa, l, ok, step) {                      // how far a stroke may go either way along (ca, sa) while ok() says so
  reach[0] = reach[1] = l;
  for (let d = step; d < l + step; d += step) if (!ok(zone(x + ca * d, y + sa * d))) { reach[1] = Math.min(l, d - step * .5); break; }
  for (let d = step; d < l + step; d += step) if (!ok(zone(x - ca * d, y - sa * d))) { reach[0] = Math.min(l, d - step * .5); break; }
}

// ---------- walls, windows, steps ----------
// Everything that is built is painted in level strokes, a cell high: floor by floor, step by step.
let cw = 1, ch = 1;                                            // how big a cell of the map is in the picture
const LONG = [0, 9, 9, 8, 5, 3, 11, 13, 15];                  // how long a stroke each kind of thing gets
function flatStroke(x, y, k) {
  const R = PP.R, xf = zx, pf = zy, l0 = lev, j = cellJ;
  // Stroke by stroke a cell high, in rows; on a big screen, where a cell is more than a stroke, the rows would show as steps
  const rows = ch < 1.7 * U, w = rows ? Math.max(1.1 * U, ch) : 1.5 * U, yc = rows ? py((j + .5) / MH) : y;
  run(x, yc, 1, 0, (1.5 + R() * (LONG[k] - 1.5)) * Math.max(U, cw * .7), k < STEP ? q => q === k && lev === l0 : q => q === k && Math.abs(lev - l0) < 2, Math.max(1, cw * .5));   // stone is not as sharp as a window
  const a = reach[0], b = reach[1], l = k === STEP ? 3.2 + (l0 - 3.2) * (1 - farOff(xf, pf) * .5) : l0;   // far up the stairs light and dark are closer to each other
  let col = ramp(scaleOf(k), toneOf(x, y, xf, pf, l + .2 + R() * .6));
  if (k >= STEP) {
    const q = R();
    if (traits.rain) col = mix(col, pal.skyLow, l0 * .03);                                           // wet stone shines
    if (q > .985 && pf > .5 && Math.abs(xf - .4) < .3 && !night && vnoise(xf * 30, pf * 60) > .5) {     // what the tree has let fall
      if (season === 'autumn') col = pick(pal.bits, R()); else if (season === 'spring') col = mix(col, pick(pal.accent, R()), .7);
    }
  } else if (k === WARM && pf < .385 && traits.snow && R() < .85) col = pal.snowy;                   // the roof at the end of the street
  col = hazed(lightOn(col), xf, pf);
  PP.brush(x - a, yc, x + b, yc, w, col, { a: .96, jit: 3, dry: 0, bow: 0, sp: Math.min(1.1 * U, w) });
}
function landStroke(y0, y1) {                                  // somewhere between two heights
  const R = PP.R, x = R() * W, y = y0 + R() * (y1 - y0);
  if (!at(x, y)) return;
  const k = zone(x, y);
  if (k === LEAVES) leafStroke(x, y); else if (k === PANE) paneStroke(panes[zw]); else if (k !== SKY && k < RAIL) flatStroke(x, y, k);
}
const BANDS = [() => landStroke(0, H / 3), () => landStroke(H / 3, H * 2 / 3), () => landStroke(H * 2 / 3, H)];
function signStroke() {                                        // "9., Wasagasse": white on blue, as they are in Vienna
  const R = PP.R, u = R(), v = R(), x = px(SIGN[0] + u * (SIGN[1] - SIGN[0])), y = py(SIGN[2] + v * (SIGN[3] - SIGN[2]));
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y)) return;
  const rim = u < .05 || u > .95 || v < .14 || v > .86, text = !rim && v > .36 && v < .66 && u > .12 && u < .88 && vnoise(u * 34, 3) > .3;
  const l = Math.min(3 * U, (SIGN[1] - SIGN[0]) * PW * .12), xa = Math.max(px(SIGN[0]), x - l), xb = Math.min(px(SIGN[1]), x + l);
  toneOf(x, y, fx(x), fy(y), 4);
  PP.brush(xa, y, xb, y, 1.1 * U, lightOn(rim || text ? pal.signWhite : pal.sign), { a: .95, jit: 3, dry: 0, bow: 0, sp: U });
}

// ---------- leaves and limbs ----------
function leafStroke(x, y) {
  const R = PP.R, d = zf, xf = zx, pf = zy, k = R(), sz = .5 * U, lum = clamp(folLum(x, y));
  if (d < .12) {                                               // the ragged rim: single leaves
    PP.dab(x, y, (2.5 + R() * 5) * sz, hazed(lum > .5 ? pal.folMid : pal.folDark, xf, pf), { a: .85, den: .3, s: 2.4 * U });
    return;
  }
  let col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .6 && k < .3) col = pal.folSun; else if (pal.accent && lum > .3 && k > (season === 'spring' ? .72 : .92)) col = pick(pal.accent, R());
  if (night) col = mix(col, pal.lights[0], glowAt(xf, pf + .12) * .25);
  const a = R() * Math.PI, ca = Math.cos(a), sa = Math.sin(a);
  let l = (4 + R() * 8) * sz;
  if (hidden(x + ca * l, y + sa * l) || hidden(x - ca * l, y - sa * l)) l *= .25;   // not across the corner of the house
  PP.brush(x - ca * l, y - sa * l, x + ca * l, y + sa * l, (4 + R() * 6) * sz, hazed(col, xf, pf), { a: .85, sp: 1.3 * U, dry: .2, jit: 10 });
}
function leafDab() {                                           // somewhere in the crowns
  const R = PP.R, x = treeX0 + R() * (treeX1 - treeX0), y = treeTop + R() * (treeFoot - treeTop);
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y) || bare) return;
  if (zone(x, y) === LEAVES) leafStroke(x, y);
  else if (zf > -.08 && maskAt(x, y) === 0 && inTrees(x, y) && fy(y) < HEAD - .01) PP.dab(x, y, (1.2 + R() * 2.5) * U, pal.folMid, { a: .8, den: .3, s: 2.2 * U });   // a leaf or two beyond them
}
function limbStroke() {                                        // a stretch of bark
  const R = PP.R; let k = R() * limbWeight, s = limbs[0];
  if (!s) return;
  for (const t of limbs) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span;
  const mx = limbX(s, tm), my = limbY(s, tm);
  if (mx < 0 || mx >= W || my < 0 || my >= H || !at(mx, my) || zone(mx, my) !== LIMB) return;   // not where leaves or a house hide it
  const xf = zx, pf = zy, bark = hazed(mix(pal.trunk, pal.towerLight, s.depth < 2 ? .16 : .06), xf, pf), q = R();
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    if (hidden(ax, ay) || hidden(bx, by)) continue;
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (traits.snow) { if (s.w > 1.3 * U && q < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snowy, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (sunny && s.w > 4 * U && q < .4) PP.brush(ax + s.w * .22, ay, bx + s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .5), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });   // the sunny side
  }
}

// ---------- weather ----------
function fogStroke() {                                         // fog comes down the stairs in drifting banks
  const R = PP.R, x = px(.2 + R() * .5), depth = R(), y = py(.42 + depth * .26);
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y) || !traits.fog || zone(x, y) === SKY) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .36 - depth * .16, den: .2, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}

// ---------- the coat ----------
function coatStroke(x0, y0, leaves) {                          // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R;
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y < -12 * U || y > H + 12 * U) continue;
    const xc = clamp(x, 0, W - 1), yc = clamp(y, 0, H - 1), l = (14 + R() * 8) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
    let k = zone(xc, yc); const xf = zx, pf = zy;
    if (leaves) {                                              // the trees: they reach up into the sky, where the rest of the land has no business
      if (k !== LEAVES) continue;
      const lum = clamp(folLum(x, y)), a = (R() - .5) * 1.2;
      PP.brush(x - Math.cos(a) * l * .7, y - Math.sin(a) * l * .7, x + Math.cos(a) * l * .7, y + Math.sin(a) * l * .7, 20 * U, hazed(mix(pal.folDeep, lum > .5 ? pal.folMid : pal.folDark, clamp(lum * 1.3)), xf, pf), o);
      continue;
    }
    if (k >= RAIL) k = what(xf, pf);                           // what is behind a rail, a tree or the sign
    if (k === SKY) continue;
    let lv = Math.max(lev, 2);
    if (k === GLASS) { k = WALL; lv = 3; }
    const col = hazed(lightOn(ramp(scaleOf(k), toneOf(xc, yc, xf, pf, lv + .5))), xf, pf);
    if (k >= STEP) PP.brush(x - l, y, x + l, y, 22 * U, col, o);
    else PP.brush(x, y - l * .8, x, y + l * .8, 24 * U, col, o);                                    // a house: an upright stroke
  }
}

// ---------- what moves ----------
// The lamp hangs over the stairs on wires from house to house, and sways when the wind gets at it.  There is a
// second one farther up.  They are too thin for the brush that paints the houses, so they are drawn anew every frame.
function drawLamps(P, t) {
  const x0 = px(LAMP[0]), y0 = py(LAMP[1]);
  if (!at(clamp(x0, 0, W - 1), clamp(y0, 0, H - 1))) return;
  const sway = wind(t) * (1 + traits.gust * 2) * .0016 * PW, x = x0 + sway, y = y0 - Math.abs(sway) * .08, s = .0155 * PW;   // s: half as wide as its shade
  const ink = mix(pal.ink, pal.dark, .5), wire = { a: .75, pass: 1, wob: .15 * U, step: 999, jit: 2 };
  P.line([[px(.155), py(.151)], [px(.31), py(.176)], [x, y]], ink, Math.max(.7, .7 * U), wire);      // across the street, from house to house
  P.line([[x, y], [px(.6), py(.168)], [px(.73), py(.14)]], ink, Math.max(.7, .7 * U), wire);
  P.line([[px(.4975), py(-.01)], [px(.482), py(.09)], [x, y]], ink, 1.5 * U, { a: .9, pass: 1, wob: .15 * U, step: 999, jit: 2 });   // and from one behind us
  const o = { a: .97, sp: Math.max(.7 * U, s * .12), dry: 0, jit: 3, bow: 0 };
  P.brush(x, y - s * .05, x, y + s * .5, s * .34, ink, o);                                           // what it hangs by
  P.brush(x - s * .5, y + s * .12, x + s * .5, y + s * .12, s * .1, ink, o);
  for (let n = 0; n < 2; n++) P.brush(x - s * .82, y + s * .86, x + s * .82, y + s * .86, s * .6, ink, o);   // its shade: a flat bell
  P.brush(x - s * .5, y + s * .52, x + s * .5, y + s * .52, s * .36, ink, o);
  if (night) {                                                 // lit: its glass, and the air round it
    const far = 1 + traits.fog * .6 + traits.rain * .15;
    P.dab(x, y + s * 1.3, s * 5 * far, pal.lights[0], { a: .13, den: .22, s: 3 * U, jit: 3 });
    P.dab(x, y + s * 1.3, s * 2.4 * far, pal.lights[0], { a: .28, den: .7, s: 2 * U, jit: 3 });
    P.dab(x, y + s * 1.22, s * .8, pal.lights[2], { a: 1, den: 3, core: 1, s: 1.2 * U, sx: 1.25, sy: .55, jit: 2 });
  } else P.brush(x - s * .6, y + s * 1.17, x + s * .6, y + s * 1.17, s * .14, mix(ink, pal.towerLight, .55), o);   // the glass under it
  // the far one
  const fx0 = px(FAR_LAMP[0]) + sway * .35, fy0 = py(FAR_LAMP[1]), f = s * .42;
  P.line([[px(.345), py(.318)], [fx0, fy0 - f * .6], [px(.468), py(.31)]], ink, Math.max(.6, .5 * U), { a: .5, pass: 1, wob: .1 * U, step: 999, jit: 2 });
  P.brush(fx0, fy0 - f * .6, fx0, fy0 + f * .2, f * .4, ink, o);
  P.brush(fx0 - f * .85, fy0 + f * .55, fx0 + f * .85, fy0 + f * .55, f * .7, ink, o);
  if (night) { P.dab(fx0, fy0 + f * 1.2, f * 3.2, pal.lights[0], { a: .16, den: .5, s: 2 * U, jit: 3 }); P.dab(fx0, fy0 + f * 1.05, f * .5, pal.lights[2], { a: 1, den: 2.5, core: 1, s: U, jit: 2 }); }
}

// ---------- the painting, one frame's worth ----------
let tops = null, skyX0 = 0, skyX1 = 0;
const RATE = { land: 20000, rail: 1.5, pane: 60000, leaf: 9000, limb: 1.2, sign: 9000, fog: 1500 };
const place = {
  colors: {
    walls: ['#2c3037', '#585c64', '#8a8a89', '#b4afa7', '#dfd9cd', '#fcf8ef'], creams: ['#2e2a26', '#5e5548', '#8f8370', '#b8a98f', '#dfcfb0', '#f6ead0'],
    yellows: ['#3b3122', '#74603a', '#a88a4c', '#cfaa5c', '#ecca78', '#fbe3a0'], reds: ['#3a221e', '#6e342a', '#9c4a38', '#bf634a', '#d98566', '#eeb096'],
    panes: ['#161d27', '#2b3846', '#4a5d70', '#7b93a8', '#b9cbd8', '#e8f0f2'],
    steps: ['#2a2521', '#5e5347', '#93856f', '#b9aa92', '#d6c9b1', '#ebe2cd'], pave: ['#2a2c30', '#4c4f55', '#74777c', '#9d9fa1', '#c6c6c2', '#e8e6de'],
    road: ['#17191d', '#2e3137', '#4b4f56', '#6d7178', '#9a9c9f', '#c9c8c2'], steel: ['#33332f', '#6e6b65', '#a8a399', '#d6d1c5', '#f3efe4'],
    sunlight: '#ffe6b0', snowy: '#eef2f4', sign: '#23417e', signWhite: '#f1f1ea',
  },
  weathers: {                                                  // white is what snow makes it, and frost a little
    snow: { steps: ['#5b6676', '#98a5b5', '#cdd5de', '#e9edf1', '#f8fafb', '#ffffff'], pave: ['#5d6878', '#99a5b3', '#ccd4dc', '#e8ecf0', '#f7f9fa', '#ffffff'],
      road: ['#3a424e', '#5f6975', '#8a95a1', '#b9c2cb', '#dfe5ea', '#f6f8f9'] },
    frost: { steps: ['#2d2c2c', '#55524f', '#827e78', '#aeaaa2', '#d3d0c8', '#f0eee8'], pave: ['#30333a', '#565b63', '#838990', '#b0b5b9', '#d8dbdb', '#f2f3f0'],
      road: ['#1c2026', '#373c44', '#5a6069', '#838a92', '#b3b9be', '#dfe2e2'] },
  },
  ground: /^(steps|pave|road|snowy)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const r = W / H;
    if (r >= 1) {                                              // most of its height: the road at our feet goes first, then some of the sky; and a little wider than it is
      const high = clamp(1 - (r - 1) * .28, .7, 1);
      PH = H / high; PW = PH * (1 + .1 * clamp(r - 1)); Y0 = (1 - high) * .3;
      X0 = .5 - .04 * clamp(r - 1) - W / PW / 2;
    } else {                                                   // all of its height, a slice of its width, pushed together a bit
      PH = H; PW = PH * clamp(r, .85, 1); Y0 = 0;
      X0 = clamp(.43 - W / PW / 2, 0, Math.max(0, 1 - W / PW));
    }
    place.hz = py(HOR);
    cw = PW / MW; ch = PH / MH;
    const n = Math.ceil(W) + 1;
    tops = new Float32Array(n); skyX0 = n; skyX1 = -1;
    for (let i = 0; i < n; i++) {
      const xf = fx(i), roof = xf < 0 || xf >= 1 ? 0 : roofs[xf * MW | 0];
      tops[i] = roof ? py(roof / MH) : -1e4;
      if (roof) { if (i < skyX0) skyX0 = i; skyX1 = i; }
    }
    buildTrees(); buildWindows(); buildRails();
  },
  tick(t) { time = t; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // the gap between the houses, down to the roofs at the end of the street
    if (skyX1 < skyX0) { c.rect(0, -60, 0, 0); return; }
    const dip = 3 * U - pad;
    let y = tops[skyX1] + dip;
    c.moveTo(skyX0, -60); c.lineTo(skyX1 + 1, -60); c.lineTo(skyX1 + 1, y);
    for (let i = skyX1 - 1; i >= skyX0; i--) { const e = tops[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(skyX0, y); c.closePath();
  },
  front(s, x, y) { if (!inTrees(x, y)) return false; use(s); return maskAt(x, y) === 1 || (!bare && fol(x, y) > -.05); },   // the trees reach up into the sky
  orb(o) { o.x = px(.36); o.y = Math.max(o.r * 1.5, py(.088)); },   // the sun and the moon look in between the houses, over the trees
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6, leafy = (s && !s.traits.bare) || (o && !o.traits.bare);
    clipBand(0, H);
    for (let n = 0; n < 3; n++) spend('tg-land' + n, RATE.land * W * H / 3 / ua * pace, BANDS[n]);
    spend('tg-rail', RATE.rail * railLength / (13 * U) * pace, railStroke);
    if ((s && s.night) || (o && o.night)) spend('tg-pane', RATE.pane * paneArea / ua * pace, paneDab);
    spend('tg-limb', RATE.limb * limbLength / (14 * U) * pace, limbStroke);
    if (leafy) spend('tg-leaf', RATE.leaf * leafArea / ua * pace, leafDab);
    spend('tg-sign', RATE.sign * (SIGN[1] - SIGN[0]) * PW * (SIGN[3] - SIGN[2]) * PH / ua * pace, signStroke);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('tg-fog', RATE.fog * .5 * PW * .26 * PH / ua * pace, fogStroke);
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) for (let n = -1; n < 2; n++) coatStroke(cells[k].x + n * 22 * U, cells[k].y + n * 14 * U, false);   // thrice over: nothing of the old picture is to show through
    c.restore();
    if (!bare) for (let k = i; k < j; k++) if (cells[k].y < treeFoot + 20 * U && cells[k].x > treeX0 - 40 * U && cells[k].x < treeX1 + 40 * U) coatStroke(cells[k].x, cells[k].y, true);
  },
  sprites: [{ draw: drawLamps, t0: 2, seed: 131 }],
};
return place;
} });
