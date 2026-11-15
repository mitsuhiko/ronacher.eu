/* ------------------------------------------------------------------ *
 *  Vienna, on the Waldbachsteig: St. John of Nepomuk on his pedestal,
 *  the crucifix in one arm and the palm in the other, five stars round
 *  his head.  He stands in a bed nobody weeds any more, behind a
 *  wrought-iron fence full of scrolls on an old wall; a walnut leans
 *  over him from the left, a hedge has grown up on his right, and
 *  behind it all lies the wooded hill.  In front of the wall a bench
 *  is weathering away.  By night a candle burns in a red lantern on
 *  the wall, and his stars glimmer.
 *
 *  It is painted after a photograph taken in May, measured by hand:
 *  the saint part by part, the fence bay by bay, the wall, the bench.
 *  What is green in it is as light and as dark as the photograph has
 *  it, point by point: that is read off it (waldbachsteig-map.js).
 *  The cable that crossed its sky is not in it, nor what is cut into
 *  the pedestal, nor what there is of the fence and the wall beyond the
 *  gate: a bush has grown up in front of that.  The lantern is made up,
 *  and so is what lies beyond the photograph: more trees and bushes on
 *  the left, where the wall has its corner, bush beside bush under the
 *  hill on the right, the near end of the bench and the path at our
 *  feet.  There the leaves are those of the photograph once more, in a
 *  mirror.  It is an upright one: a wide screen has it in its middle.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'waldbachsteig', name: 'Waldbachsteig', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, ease, mix, pick, rng, hash1, vnoise, fbm, wind, spend } = kit;
const MAP = window.viennaMaps.waldbachsteig;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0, sway = 0;   // the view (CSS pixels, the stroke unit), the clock and how far the wind pushes the leaves
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let cur = null, own = null, pal = null, traits = null, night = false, season = '', bare = false, wet = false, evening = false, white = 0, thin = 0, sun = 1;   // own: the picture's colours, pal: those at the point being painted; sun: how hard the shadows are
const use = s => {
  cur = s; own = pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; wet = traits.rain > 0; evening = s.weather === 'golden';
  white = traits.snow ? 1 : season !== 'winter' ? 0 : s.weather === 'sunny' ? .7 : s.weather === 'frost' ? .45 : .3;   // how much snow lies
  thin = season === 'autumn' ? .34 : season === 'spring' ? .14 : 0;   // how thin the leaves are on the trees
  sun = night ? .45 : traits.shadow > 0 ? (bare ? .6 : 1) : .25;
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph: x and y as fractions of its width and height.  It is an upright one:
// a narrow screen shows a slice of its width, a wide one a slice of its height and what lies to either side of it.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along2(pts, x) {                                      // a line through points, eased from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * ease((x - a[0]) / (b[0] - a[0])); }
  return pts[pts.length - 1][1];
}
const inPoly = (p, x, y) => { let o = false; for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o; return o; };
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
// ---------- the map ----------
// What grows at every point of the photograph, and how light it is there, was read off it (tools/waldbachsteig-map.py):
// one character a cell, what it is times eight plus how light it is, from 0 to 7.  For a wisp of leaves that is how
// much of the cell they fill.  Where things begin and end is measured, further down; from the map comes the light in
// all that is green, and the walnut, leaf by leaf.
const M_SKY = 0, M_LEAF = 1, M_WISP = 2, M_GRASS = 5;
const MW = MAP.w, MH = MAP.h, cells = new Uint8Array(MW * MH);
{ const code = new Uint8Array(128);
  for (let c = 35, v = 0; c < 127; c++) if (c !== 92) code[c] = v++;
  for (let i = 0; i < cells.length; i++) cells[i] = code[MAP.cells.charCodeAt(i)]; }
// Beyond the photograph it goes on as its own mirror image, drawn out and pulled out of shape so that it does not show.
function fold(xf, pf) {
  const d = xf < 0 ? -xf : xf - 1;
  let m = d * .75 + smooth(0, .25, d) * (vnoise(xf * 1.7 + 7.3, pf * 2.5) - .5) * .11;   // never so much that it would turn back
  if (m < 0) m = -m;
  m %= 2; if (m > 1) m = 2 - m;
  return xf < 0 ? m : 1 - m;
}
let lev = 0, mxf = 0;                                           // what what() learnt on the way: how light it is, and where on the photograph the point lies
function what(xf, pf) {                                        // what there is at a point
  const a = xf * MW * .8, b = pf * MH * .8;                    // a cell has no corners
  xf += (vnoise(a, b) - .5) * 1.5 / MW; pf += (vnoise(a + 31.7, b + 17.3) - .5) * 1.5 / MH;
  if (xf < 0 || xf > 1) xf = fold(xf, pf);
  if (pf > 1) pf = 2 - pf;                                     // and below it, at our feet
  mxf = xf;
  const i = xf * MW | 0, j = pf * MH | 0, v = cells[(j < 0 ? 0 : j >= MH ? MH - 1 : j) * MW + (i >= MW ? MW - 1 : i)];
  lev = v & 7; return v >> 3;
}
function light(xf, pf) {                                       // how light what grows at a point is, from 0 to 7
  const k = what(xf, pf);
  return k === M_SKY ? 2 + vnoise(xf * 40, pf * 50) * 4 : k === M_WISP ? 6 : lev;   // where the photograph has sky and the picture has not: neither light nor dark
}
// The bushes stand where the photograph has the wall, the bench, the grass: they are as light as the hedge is, further
// up, where its leaves are thickest, and that over and over, each time the other way up.
function bushLight(xf, pf) { let t = ((pf - .5) / .21) % 2; if (t < 0) t += 2; return light(xf + pf * .37, .39 + (t > 1 ? 2 - t : t) * .21); }
const tone = l => clamp(.5 + ((l + .1 + PP.R() * .8) / 8 - .5) * (.7 + .5 * sun));   // without the sun there is less to tell light from shade
const leafHere = (x, y) => vnoise(x * .13 / U, y * .13 / U) < .32 + lev * .06;   // a wisp of leaves is leaves here and sky there
const CROWN = [[0, .5], [.33, .5], [.36, .455], [.4, .435], [.46, .43], [.5, .4], [.56, .36], [.6, .3]];   // how far to the right the walnut reaches at a height

// The wall runs away from us to the right, and the fence on it: whatever is level along them runs to a point far
// off on that side (VX), the wall's a little lower (WV) than the fence's (FV).  A thing there is as big as far()
// says, of what it is at the photograph's left edge.
const VX = 2.09, WV = .7, FV = .655;
const far = xf => (VX - xf) / VX;
const BACK = .8, EDGE = .845, DRIP = .865, FOOT = 1.04;        // the coping: where it begins, its front edge, its underside; the foot of the wall
const wallY = (k, xf) => WV + (k - WV) * far(xf);
const WEND = .895, WBEG = -.16;                                // where the wall ends, at the gate, and where it begins: its corner lies a little beyond the photograph
// The fence: its upper and its lower rail, and where its posts stand.  They are evenly spaced, which is how this counts them.
const RAIL0 = .68, RAIL1 = .782;
const postX = n => (.2113 * n + .075) / (.1013 * n + 1), bayOf = xf => (xf - .075) / (.2113 - .1013 * xf);
const ironY = (v, xf, deep) => FV + (RAIL0 - FV + (deep && v > 0 ? v * 1.4 : v) * (RAIL1 - RAIL0)) * far(xf);   // v: 0 on the upper rail, 1 on the lower; the gate reaches further down

// The lie of the land: the ridge of the hill, the top of the hedge, of what grows in the bed, of the grass in front.
const RIDGE = [[-3, .41], [-.5, .408], [0, .405], [.3, .4], [.44, .39], [.47, .379], [.5, .369], [.55, .361], [.6, .355], [.7, .343], [.8, .33], [.9, .325], [1, .328],
  [1.15, .322], [1.4, .335], [1.7, .365], [1.95, .38], [2.3, .36], [2.7, .35], [3.1, .37], [3.6, .385]];
const hillTop = xf => along2(RIDGE, xf) + (vnoise(xf * 70, 2.2) - .5) * .008 + (vnoise(xf * 230, 6.1) - .5) * .004;
const BUSHES = [[-3, .62], [.3, .6], [.36, .5], [.42, .46], [.5, .45], [.53, .44], [.57, .43], [.62, .405], [.67, .385], [.75, .375], [.8, .36], [.84, .343], [.87, .35], [.9, .365], [1, .365],
  [1.1, .385], [1.25, .43], [1.45, .415], [1.7, .45], [1.95, .425], [2.3, .455], [4, .44]];
const hedgeTop = xf => along2(BUSHES, xf) + (vnoise(xf * 13, 4.4) - .5) * .045 * smooth(.9, 1.1, xf) + (vnoise(xf * 45, 4.4) - .5) * .03 + (vnoise(xf * 160, 8.3) - .5) * .012;
const BEDTOP = [[-3, .6], [0, .6], [.1, .605], [.2, .59], [.3, .575], [.36, .555], [.42, .565], [.447, .6], [.456, .648], [.54, .652], [.56, .64], [.6, .632], [.7, .625], [.8, .63], [.9, .635], [1.04, .64]];
const reeds = xf => smooth(.27, .33, xf) * smooth(.455, .43, xf);                   // the long grass by the pedestal
const bedTop = xf => along2(BEDTOP, xf) + (vnoise(xf * 60, 1.7) - .5) * .02 + (vnoise(xf * 210, 3.3) - .5) * .012 - reeds(xf) * .04 * vnoise(xf * 420, 5.5);
function grassTop(xf) {                                        // long grass at the foot of the wall, the meadow beyond it
  const wild = .815 + (vnoise(xf * 6, 9.1) - .5) * .03 + (vnoise(xf * 90, 2.9) - .5) * .022;   // beyond the wall, at the foot of the bushes
  if (xf > WEND + .015) return wild;
  const foot = wallY(FOOT, xf) - .008 - (.015 + .07 * smooth(.26, .42, xf)) * (.35 + .65 * vnoise(xf * 14, 7.7)) - .014 * vnoise(xf * 150, 1.1);
  return foot + (wild - foot) * smooth(WEND, WEND + .015, xf);
}
const NEAR = 1.14;                                             // from here down it is the path we stand on

// ---------- the trees ----------
// A crown is a heap of rounds: [x, y, half as wide, half as high].  The walnut's are measured, and so are the
// places where one sees through it.  Where its leaves are is kept in a coarse grid, ragged edge and all.  For
// winter, and where the leaves are thin, the trees are grown: a trunk, limbs, twigs, none of them beyond the crown.
const WALNUT = [[.09, .21, .14, .15], [.3, .27, .17, .135], [.42, .24, .075, .09], [.38, .4, .09, .08], [.22, .46, .2, .13], [.05, .5, .15, .14], [-.05, .1, .12, .12], [-.2, .22, .2, .2], [-.25, .46, .2, .16]];
const GAPS = [[.1, .32, .1, .055], [.17, .365, .05, .025], [.245, .155, .035, .028]];
let rounds = [], crown = null, cCols = 0, cRows = 0, cCell = 2, cFoot = 0;
let limbs = [], limbSum = null, limbWeight = 0, limbLength = 0, mask = null;
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
function body(xf, pf) {                                        // how deep in a crown: 1 in the middle of a round, 0 at its edge
  let q = -1;
  for (const b of rounds) { const dx = (xf - b[0]) / b[2]; if (dx > 2 || dx < -2) continue; const d = 1 - Math.hypot(dx, (pf - b[1]) / b[3]); if (d > q) q = d; }
  return q;
}
function buildTrees() {
  const x0 = fx(0) - .5, x1 = fx(W) + .5, trees = [];
  rounds = [];
  const tree = (t, list) => { if (t[0] > x0 && t[0] < x1) { trees.push(t); for (const b of list) rounds.push(b); } };   // [x, foot, top, how wide it spreads, how it leans, how thick]
  tree([.085, .7, .05, 1.15, .1, .02], WALNUT);
  for (const side of [0, 1]) {                                 // beyond the photograph the map is its own mirror image: there it stands again, the other way round
    const m = xf => side ? 1 + (1 - xf) / .75 : -xf / .75;
    tree([m(.085), .7, .05, 1.15, -.1, .02], WALNUT.map(b => [m(b[0]), b[1], b[2] / .75, b[3]]));
  }
  // the leaves: how deep in them a point is
  cCell = 2 * U; cFoot = Math.min(H, py(.7)); cCols = Math.ceil(W / cCell) + 2; cRows = Math.max(1, Math.ceil(cFoot / cCell) + 1);
  crown = new Float32Array(cCols * cRows);
  for (let j = 0; j < cRows; j++) for (let i = 0; i < cCols; i++) {
    const xf = fx((i + .5) * cCell), pf = fy((j + .5) * cCell), q = body(xf, pf);
    if (q < -.4) { crown[j * cCols + i] = -9; continue; }
    let gap = 0;
    for (const g of GAPS) gap = Math.max(gap, 1 - Math.hypot((xf - g[0]) / g[2], (pf - g[1]) / g[3]));
    crown[j * cCols + i] = q * 2 - gap * 1.6 + (fbm(xf * 8 + 3, pf * 11) - .48) * 1.5 + (vnoise(xf * 55, pf * 70) - .5) * .6;
  }
  // their wood
  limbs = [];
  let r = rng(5);
  const leafy = (x, y) => { const pf = fy(y), k = pf < .7 ? what(fx(x), pf) : M_SKY; return (k === M_LEAF || k === M_WISP) && mxf < along2(CROWN, pf); };   // no limb beyond the leaves
  const add = (xa, ya, xb, yb, bend, w, depth) => {
    const len = Math.hypot(xb - xa, yb - ya), a = Math.atan2(yb - ya, xb - xa);
    limbs.push({ x1: xa, y1: ya, x2: xb, y2: yb, cx: (xa + xb) / 2 - Math.sin(a) * len * bend * 2, cy: (ya + yb) / 2 + Math.cos(a) * len * bend * 2, w, depth, len });
  };
  const grow = (x, y, a, len, w, depth) => {
    let x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len;
    if (!leafy(x2, y2)) { len *= .5; x2 = x + Math.cos(a) * len; y2 = y + Math.sin(a) * len; if (!leafy(x2, y2)) return; }
    add(x, y, x2, y2, (r() - .5) * .3, w, depth);
    if (depth >= 5 || len <= 5 * U) return;
    const k = r() < .7 ? 2 : 3;
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .62 + (r() - .5) * .45, len * (.6 + r() * .2), Math.max(.6 * U, w * .6), depth + 1);
  };
  for (const t of trees) {
    r = rng(31 + Math.round(t[0] * 1000));                     // every tree keeps its shape, whatever else is to be seen
    const x = px(t[0]), y = py(t[1]), h = (t[1] - t[2]) * PH, w = Math.max(1.3 * U, h * t[5]);
    add(x, y, x + t[4] * h, y - h * .4, (r() - .5) * .08, w, 0);
    const trunk = limbs[limbs.length - 1];
    for (let i = 0; i < 5; i++) {
      const a = -Math.PI / 2 + ((i + .5) / 5 - .5) * 2.2 * t[3] + (r() - .5) * .2 + t[4], from = i === 2 ? 1 : .6 + r() * .4;
      grow(limbX(trunk, from), limbY(trunk, from), a, h * (.27 - Math.abs(a + Math.PI / 2) * .05) * (.85 + r() * .3), w * .55, 1);
    }
  }
  mask = sheet(0, 0, W, H, parts.cell); limbWeight = limbLength = 0; limbSum = new Float32Array(limbs.length);   // where a limb is (1), one of the bigger ones (4)
  limbs.forEach((s, i) => {
    limbWeight += s.len * (s.w + 2 * U); limbSum[i] = limbWeight; limbLength += s.len;
    for (let k = 0, n = Math.ceil(s.len / (mask.cell * .5)); k <= n; k++) stamp(mask, s.depth < 3 ? 5 : 1, limbX(s, k / n), limbY(s, k / n), s.w / 2 + .3 * U, true);
  });
}
const folAt = (x, y) => { const i = ((x - sway) / cCell) | 0, j = (y / cCell) | 0; return i < 0 || j < 0 || i >= cCols || j >= cRows ? -9 : crown[j * cCols + i]; };
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask.of[((y / mask.cell) | 0) * mask.cols + ((x / mask.cell) | 0)];

// ---------- the bushes ----------
// Where the wall ends, at the gate, a bush has grown up in front of the fence and hides what there is left of it,
// another one stands before its corner on the left, and beyond the photograph there is bush beside bush: those on
// the left are closer to us, and bigger.  Each is a heap of rounds like a crown; how deep in one a point is, ragged
// edge and all, is kept in a coarse grid.
const BUSH = [[1.035, .665, .115, .135], [.965, .78, .07, .055], [1.1, .6, .09, .1], [-.3, .77, .19, .2], [-.2, .64, .1, .1], [-.21, .98, .14, .15], [-.42, .97, .17, .18]];
let shrubs = [], shrub = null, sCols = 0, sRows = 0, sCell = 2;
let stop = 0;                                                  // what inBush() learnt on the way: the top of the round the point is in
function inBush(xf, pf) {                                      // how deep in a bush: 1 in the middle of a round, 0 at its edge
  let q = -9;
  for (const b of shrubs) {
    const dx = (xf - b[0]) / b[2]; if (dx > 1.3 || dx < -1.3) continue;
    const dy = (pf - b[1]) / b[3], d = 1 - Math.hypot(dx, dy);
    if (d > q) { q = d; stop = b[1] - b[3]; }
  }
  return q;
}
function buildBushes() {
  shrubs = BUSH.slice();
  for (let i = 1, x = 1.2, x1 = fx(W) + .3; x < x1; i++, x += .1 + hash1(i * 17) * .09) {   // a big round and a smaller one on its shoulder, and what grows at its foot
    const k = .75 + hash1(i * 5) * .6, y = .71 - .07 * k + (hash1(i * 29) - .5) * .03, side = hash1(i * 11) < .5 ? -1 : 1;
    shrubs.push([x, y, .12 * k, .14 * k], [x + side * .07 * k, y - .07 * k, .07 * k, .08 * k], [x + (hash1(i * 3) - .5) * .1, .79, .13, .055]);
  }
  for (let i = 1, x = -.58, x0 = fx(0) - .4; x > x0; i++, x -= .16 + hash1(i * 19) * .12) {
    const k = 1.1 + hash1(i * 7) * .5, y = .86 - .04 * k + (hash1(i * 31) - .5) * .04, side = hash1(i * 13) < .5 ? -1 : 1;
    shrubs.push([x, y, .15 * k, .15 * k], [x + side * .09 * k, y - .08 * k, .09 * k, .08 * k], [x + (hash1(i * 3) - .5) * .1, 1.02, .2, .16]);
  }
  sCell = 2 * U; sCols = Math.ceil(W / sCell) + 1; sRows = Math.ceil(H / sCell) + 1;
  shrub = new Float32Array(sCols * sRows).fill(-9);
  for (let i = 0; i < sCols; i++) {
    const xf = fx((i + .5) * sCell);
    if (xf > -.04 && xf < .86) continue;                       // none of them in the photograph, but the one at the gate
    for (let j = 0; j < sRows; j++) { const pf = fy((j + .5) * sCell), q = inBush(xf, pf);
    if (q > -.3) shrub[j * sCols + i] = q + (fbm(xf * 9 + 1, pf * 12) - .5) * .22 + (vnoise(xf * 60, pf * 80) - .5) * .1; }
  }
}
const bushAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? -9 : shrub[((y / sCell) | 0) * sCols + ((x / sCell) | 0)];

// ---------- what is drawn by hand ----------
// The saint, the lantern, the fence and the bench are kept in a grid: which part of which of them a cell belongs
// to, the one nearest to us that covers it.  The saint has a finer one to himself: his head is no bigger than a
// few strokes of the brush, and they have to know where to end.
const STAR = 1, CLOAK = 2, BASE = 3, CASSOCK = 4, ROCHET = 5, CAPE = 6, NECK = 7, HAIR = 8, FACE = 9, BEARD = 10, CAP = 11, SHADE = 12, PALM = 13, HAND = 14, CROSS = 15, PLINTH = 20, LEDGE = 21, DIE = 22, SIDE = 23,
  IRON = 40, LID = 50, PANE = 51, SLAT = 60, SEAT = 63, RIM = 64, TUBE = 65;
let parts = null, fine = null;
const sheet = (x, y, w, h, cell) => { const cols = Math.ceil(w / cell) + 1, rows = Math.ceil(h / cell) + 1; return { x, y, cell, cols, rows, x1: x + cols * cell, y1: y + rows * cell, of: new Uint8Array(cols * rows) }; };
function partAt(x, y) {
  if (x < 0 || y < 0 || x >= W || y >= H) return 0;
  const g = parts.of[((y / parts.cell) | 0) * parts.cols + ((x / parts.cell) | 0)];
  if (g || x < fine.x || y < fine.y || x >= fine.x1 || y >= fine.y1) return g;
  return fine.of[(((y - fine.y) / fine.cell) | 0) * fine.cols + (((x - fine.x) / fine.cell) | 0)];
}
function stamp(g, id, x, y, rad, or) {                         // a round spot of it
  const c = g.cell, m = Math.ceil(rad / c), ix = Math.floor((x - g.x) / c), iy = Math.floor((y - g.y) / c);
  for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
    const cx = ix + dx, cy = iy + dy;
    if (cx < 0 || cy < 0 || cx >= g.cols || cy >= g.rows || Math.hypot(dx, dy) * c > rad + c * .35) continue;
    if (or) g.of[cy * g.cols + cx] |= id; else g.of[cy * g.cols + cx] = id;
  }
}
function stampLine(g, id, xa, ya, xb, yb, rad) { for (let k = 0, n = Math.ceil(Math.hypot(xb - xa, yb - ya) / (g.cell * .5)) || 1; k <= n; k++) stamp(g, id, xa + (xb - xa) * k / n, ya + (yb - ya) * k / n, rad); }
function stampPoly(g, id, pts) {                               // a patch of the photograph
  const p = pts.map(q => [px(q[0]), py(q[1])]), c = g.cell;
  let xa = 1e9, xb = -1e9, ya = 1e9, yb = -1e9;
  for (const q of p) { xa = Math.min(xa, q[0]); xb = Math.max(xb, q[0]); ya = Math.min(ya, q[1]); yb = Math.max(yb, q[1]); }
  for (let j = Math.max(0, Math.floor((ya - g.y) / c)); j <= Math.min(g.rows - 1, Math.floor((yb - g.y) / c)); j++)
    for (let i = Math.max(0, Math.floor((xa - g.x) / c)); i <= Math.min(g.cols - 1, Math.floor((xb - g.x) / c)); i++)
      if (inPoly(p, g.x + (i + .5) * c, g.y + (j + .5) * c)) g.of[j * g.cols + i] = id;
}

// The saint, from the back to the front: his cloak hanging from either arm, the cassock, the rochet over it and its
// sleeves, the fur cape on his shoulders, his neck, hair, face and beard under the biretta, the palm and the hand
// that holds it, the crucifix and the hand that holds that; under him the pedestal, its front turned a little to
// the left.  His eye and his mouth are a dark stroke each.
const FIGURE = [
  [CLOAK, [[.4588, .4344], [.445, .443], [.433, .4536], [.4256, .4712], [.428, .484], [.438, .492], [.45, .497], [.4555, .486], [.4534, .47], [.459, .458]]],
  [CLOAK, [[.5196, .4344], [.5325, .4488], [.5385, .4712], [.5389, .4968], [.533, .5045], [.526, .508], [.5154, .4872], [.512, .4744], [.524, .4696]]],
  [BASE, [[.462, .5765], [.5385, .5785], [.5395, .5845], [.461, .584]]],
  [CASSOCK, [[.464, .526], [.5, .5195], [.5282, .5205], [.5197, .536], [.526, .558], [.5367, .5725], [.538, .5795], [.4685, .578], [.4705, .559], [.466, .54]]],
  [ROCHET, [[.4566, .474], [.462, .452], [.49, .45], [.514, .452], [.516, .488], [.527, .508], [.5295, .519], [.524, .5245], [.5, .5205], [.48, .5225], [.4555, .529], [.4545, .5]]],
  [ROCHET, [[.4395, .4605], [.4435, .4578], [.452, .458], [.4535, .4625], [.4535, .4695], [.4495, .4725], [.4435, .4705], [.4398, .466]]],
  [ROCHET, [[.5195, .4605], [.5248, .4625], [.5262, .47], [.525, .4772], [.5198, .4772]]],
  [CAPE, [[.4816, .4222], [.4609, .4272], [.4588, .4344], [.463, .4552], [.4737, .4616], [.4823, .4568], [.4897, .4696], [.5004, .4688], [.5068, .4616], [.5132, .4552], [.5196, .4344], [.517, .4264], [.5004, .4214]]],
  [NECK, [[.4812, .4212], [.4895, .4205], [.4975, .4222], [.4992, .4262], [.4905, .4288], [.4805, .4292]]],
  [HAIR, [[.4905, .404], [.4955, .4062], [.4993, .409], [.4998, .4125], [.4992, .417], [.5005, .4215], [.4975, .4225], [.493, .42], [.4905, .416]]],
  [FACE, [[.4768, .4052], [.4855, .404], [.4934, .4046], [.4925, .4105], [.4915, .416], [.4875, .4172], [.4805, .4152], [.476, .4137], [.4737, .4127], [.4757, .4096], [.4763, .4078]]],
  [BEARD, [[.476, .4137], [.4805, .4152], [.4875, .4172], [.4915, .416], [.4925, .4185], [.4895, .421], [.4835, .4222], [.4766, .4213], [.4754, .4186], [.4768, .4161], [.4751, .4148]]],
  [CAP, [[.4682, .3982], [.4925, .3951], [.4936, .4046], [.485, .404], [.477, .4048], [.4722, .4062]]],
  [PALM, [[.5143, .4616], [.511, .4488], [.5143, .4344], [.5197, .424], [.526, .4344], [.5303, .4424], [.525, .4552], [.5197, .4632]]],
  [HAND, [[.5085, .4665], [.512, .4625], [.5185, .4615], [.5205, .466], [.52, .4745], [.515, .478], [.5095, .4765], [.508, .471]]],
];
const EYE = [.4786, .4078, .4828, .4084], MOUTH = [.4772, .4162, .4814, .4167];
const FIST = [[.4528, .4625], [.456, .4598], [.462, .4598], [.4642, .4635], [.4635, .4685], [.4585, .4705], [.4535, .4695]];   // the hand round the crucifix
const PEDESTAL = [
  [PLINTH, [[.4605, .5835], [.5385, .5835], [.539, .5905], [.4585, .5905]]], [LEDGE, [[.446, .5895], [.54, .5895], [.5405, .5995], [.4465, .5995]]],
  [DIE, [[.451, .5995], [.52, .5995], [.52, .67], [.451, .67]]], [SIDE, [[.52, .5995], [.539, .5995], [.539, .67], [.52, .67]]],
];
const CR = [.4485, .424, .468, .476], ARM = [.431, .4328, .466, .4283];   // the crucifix: its upright and its arm
const crX = pf => CR[0] + (pf - CR[1]) / (CR[3] - CR[1]) * (CR[2] - CR[0]), armY = xf => ARM[1] + (xf - ARM[0]) / (ARM[2] - ARM[0]) * (ARM[3] - ARM[1]);
const HALO = [.488, .396, .0215, .018], STARS = [-1.57, -2.7, -.44, .35, 2.75];  // the ring round his head, and where on it the stars sit
const starAt = i => [HALO[0] + Math.cos(STARS[i]) * HALO[2], HALO[1] + Math.sin(STARS[i]) * HALO[3]];
const SB = [.42, .37, .545, .672];                             // the box all of him stands in
const LAMP = [.578, .781], LB = [.571, .764, .585, .792];      // the lantern on the wall: its flame, its box

// The fence, bay by bay: from post to post it is u wide, from the upper rail (v = 0) to the lower one (v = 1) high.
// Round a flower in its middle four scrolls curl into the corners, two more ride on the upper rail.  It was forged
// by hand: no two bays came out alike.  [points, how thick, where snow lies (1 along it, 2 on top of it), what it
// is: a bar, a scroll, a length of rail, the flower]
const BAR = 0, SCROLL = 1, RAIL = 2, FLOWER = 3;
const curl = (cu, cv, ru, rv, a0, turns, tail, n = 12) => { const p = tail.slice(); for (let i = 0; i <= n; i++) { const t = i / n, a = a0 + t * turns * TAU, r = 1 - t * .72; p.push([cu + Math.cos(a) * ru * r, cv + Math.sin(a) * rv * r]); } return p; };
const BAY = [];
for (const side of [0, 1]) {
  const m = p => side ? p.map(q => [1 - q[0], q[1]]) : p;
  BAY.push([m(curl(.39, -.13, .075, .12, -1.9, 1.2, [[.02, -.04], [.1, -.1], [.2, -.2]])), 2.2, 2, SCROLL]);
  for (const up of [1, 0]) {                                   // a small curl by the flower, a big one in the corner, and from the one to the other along the rail
    const v = q => up ? q : [q[0], 1 - q[1]];
    BAY.push([m(curl(.2, .27, .12, .17, 1.75, 1.2, curl(.425, .39, .032, .05, 1.57, -.7, [], 6).reverse()).map(v)), 2.2, up ? 2 : 0, SCROLL]);
  }
}
for (let i = 0; i < 3; i++) BAY.push([[[i / 3, 0], [(i + 1) / 3, 0]], 2.8, 1, RAIL], [[[i / 3, 1], [(i + 1) / 3, 1]], 2.8, 1, RAIL], [[[i / 3, .5], [(i + 1) / 3, .5]], 2, 0, RAIL]);
BAY.push([[[.5, 0], [.5, 1]], 2.2, 0, BAR], [[[.5, .5]], 14, 0, FLOWER]);
// The bench stands on bent tube: at either end a loop for an arm and a strut the back is screwed to, and legs that
// are lost in the grass.  The one near us we look down on.  [points, how thick, whether it stands in front of the
// boards (1), or only in the grass, which grows over it (2)]
const TUBES = [
  [[[.4575, 1.072], [.459, 1], [.4612, .975], [.4741, .9549], [.4973, .9514], [.5186, .9578], [.5245, .975], [.5262, 1], [.5285, 1.072], [.521, 1.09], [.493, 1.096], [.465, 1.09], [.4575, 1.072]], .0095, 1],
  [[[.869, .914], [.8745, .895], [.879, .881], [.889, .8745], [.938, .873], [.9495, .8805], [.9485, .891], [.938, .8955], [.9, .8965], [.8765, .8955]], .0075, 1],
  [[[.4836, .875], [.499, .985], [.503, 1.004]], .0085, 0], [[[.851, .842], [.853, .9], [.858, .912], [.866, .9165]], .0085, 0],
  [[[.489, 1.116], [.489, 1.15]], .008, 1], [[[.489, 1.15], [.489, 1.168]], .008, 2], [[[.972, .962], [.972, .985]], .0065, 1], [[[.972, .985], [.972, 1.01]], .0065, 2],
];
let els = [], elSum = null, elWeight = 0, buckets = [], bucketW = 30, stampN = 1, iy0 = 0, iy1 = 0;
function buildIron() {
  els = [];
  const add = (pts, w, kind, mark, o) => {                     // o: what a piece of the fence is; whether a tube of the bench stands in front of its boards, or in the grass
    const e = { id: els.length, pts, w, kind, mark, cum: [0], len: 0, x0: 1e9, x1: -1e9, y0: 1e9, y1: -1e9, seen: 0, top: 0, tx: 0, ty: 1e9,
      taper: kind === IRON && o === SCROLL ? .45 : 0, knob: kind === IRON && o === SCROLL, lit: kind === TUBE ? 1 : o === SCROLL ? .4 : o === RAIL ? .75 : .6, front: kind === TUBE && o === 1, free: kind === TUBE && o === 2 };
    for (let i = 0; i < pts.length; i++) {
      const p = pts[i]; e.x0 = Math.min(e.x0, p[0]); e.x1 = Math.max(e.x1, p[0]); e.y0 = Math.min(e.y0, p[1]); e.y1 = Math.max(e.y1, p[1]);
      if (i) { e.len += Math.hypot(p[0] - pts[i - 1][0], p[1] - pts[i - 1][1]); e.cum.push(e.len); }
      if (p[1] < e.ty) { e.ty = p[1]; e.tx = p[0]; e.top = e.len; }
    }
    if (e.x1 < -20 * U || e.x0 > W + 20 * U || e.y0 > H + 20 * U) return;
    els.push(e);
    if (e.free) return;
    const id = kind === TUBE ? TUBE : IRON, rad = w / 2 + .3 * U;
    if (pts.length === 1) stamp(parts, id, pts[0][0], pts[0][1], w * .5);
    for (let i = 1; i < pts.length; i++) stampLine(parts, id, pts[i - 1][0], pts[i - 1][1], pts[i][0], pts[i][1], rad);
  };
  for (let n = -1; n <= 7; n++) {                            // from the corner to the gate, and a bay more, for what shows at the edge of the bush
    const gate = n === 6, big = n === 3 || n === 6 || n === 7 || n === 9, xp = postX(n), k = far(xp) * PH / 1100, thick = b => Math.max(.85 * U, b * k);
    const pt = (u, v, deep) => { const xf = postX(n + u); return [px(xf), py(ironY(v, xf, deep))]; };
    const odd = (j, i) => hash1((n + 20) * 131 + j * 17 + i) - .5, sag = (i, v) => (hash1((n * 3 + i + 90) * 7 + v * 40) - .5) * .028;   // how far a piece is off, how far a rail where two lengths of it meet
    BAY.forEach(([pts, w, mark, what], j) => {
      const a = pts[0], last = pts.length - 1, s = 1 + odd(j, 1) * .12, du = odd(j, 2) * .02, dv = odd(j, 3) * .035;
      add(pts.map((q, i) => what === SCROLL ? pt(a[0] + (q[0] - a[0]) * s + du * i / last, a[1] + (q[1] - a[1]) * s + dv * i / last, gate)
        : what === RAIL ? pt(q[0], q[1] + sag(Math.round(q[0] * 3), q[1]), gate) : pt(q[0], q[1], gate)), thick(w * (what === SCROLL ? 1 + odd(j, 4) * .3 : 1)), IRON, mark, what);
    });
    const lean = odd(40, 1) * .016;
    if (big) {                                                 // a tall post, twisted towards its tip, a scroll on its shoulder
      add([pt(lean * .5, -.5), pt(0, n === 3 ? 1.3 : 1.75)], thick(9), IRON, 0, BAR);
      add([pt(lean, -.95), pt(lean * .5, -.5)], thick(4.5), IRON, 0, BAR);
      add(curl(-.085, -.5, .055, .09, .2, -1.2, [[0, -.22], [-.04, -.34]]).map(q => pt(q[0], q[1])), thick(2.2), IRON, 2, SCROLL);
    } else {
      add([pt(lean, n === 1 ? -.32 : -.4), pt(0, 1.3)], thick(3.2), IRON, 0, BAR);
      if (n !== 1) for (const s of [-1, 1]) add([pt(lean, -.25), pt(lean + s * .04, -.33), pt(lean + s * .055, -.27)], thick(2), IRON, 0, BAR);   // a lily on top
    }
  }
  for (const [pts, w, o] of TUBES) {                           // bent, not folded: every corner is rounded off
    const p = pts.map(q => [px(q[0]), py(q[1])]), loop = p.length > 2 && p[0][0] === p[p.length - 1][0] && p[0][1] === p[p.length - 1][1], r = loop ? [] : [p[0]];
    for (let i = 0; i < p.length - 1; i++) { const a = p[i], b = p[i + 1]; r.push([a[0] * .75 + b[0] * .25, a[1] * .75 + b[1] * .25], [a[0] * .25 + b[0] * .75, a[1] * .25 + b[1] * .75]); }
    r.push(loop ? r[0] : p[p.length - 1]);
    add(p.length > 2 ? r : p, Math.max(1.6 * U, w * PW), TUBE, 1, o);
  }
  elWeight = 0; elSum = new Float32Array(els.length);
  bucketW = 30 * U; buckets = []; for (let i = 0, n = Math.ceil(W / bucketW) + 1; i < n; i++) buckets.push([]);
  els.forEach((e, i) => {
    elWeight += e.len + 6 * U; elSum[i] = elWeight;
    if (e.kind === IRON) for (let b = Math.max(0, (e.x0 / bucketW) | 0), n = Math.min(buckets.length - 1, (e.x1 / bucketW) | 0); b <= n; b++) buckets[b].push(e);
  });
  iy0 = py(.575); iy1 = py(.86);
}
// The bench: three slats to lean against and the seat, each by its four corners (left and right end of its upper
// or back edge, then right and left end of its lower or front one), and the edge of the seat that is turned to us.
const SLATS = [[.4656, .8737, .855, .8186, .8616, .8447, .478, .9085], [.4819, .9105, .8686, .8499, .8724, .8635, .4838, .9268], [.4838, .9294, .8724, .865, .8751, .8766, .4865, .9468]];
const SEATQ = [.478, 1.003, .8937, .9114, .9945, .947, .476, 1.1], RIMQ = [.476, 1.1, .9945, .947, .9925, .959, .479, 1.12];
const board = q => [[q[0], q[1]], [q[2], q[3]], [q[4], q[5]], [q[6], q[7]]];
function buildThings() {
  parts = sheet(0, 0, W, H, 1.5 * U);
  fine = sheet(px(SB[0]), py(SB[1]), (SB[2] - SB[0]) * PW, (SB[3] - SB[1]) * PH, Math.max(.5, .5 * U));
  for (let i = 0; i < 5; i++) { const s = starAt(i); stamp(fine, STAR, px(s[0]), py(s[1]), Math.max(1.5 * U, .0042 * PW)); }
  for (const [id, pts] of FIGURE) stampPoly(fine, id, pts);
  for (const s of [EYE, MOUTH]) stampLine(fine, SHADE, px(s[0]), py(s[1]), px(s[2]), py(s[3]), Math.max(.5 * U, .0009 * PH));
  stampLine(fine, CROSS, px(ARM[0]), py(ARM[1]), px(ARM[2]), py(ARM[3]), Math.max(U, .0036 * PW));
  stampLine(fine, CROSS, px(CR[0]), py(CR[1]), px(CR[2]), py(CR[3]), Math.max(U, .0038 * PW));
  stampPoly(fine, HAND, FIST);
  for (const [id, pts] of PEDESTAL) stampPoly(fine, id, pts);
  buildIron();
  stampPoly(parts, LID, [[LB[0], LB[1] + .007], [LAMP[0], LB[1]], [LB[2], LB[1] + .007], [LB[2], LB[1] + .0095], [LB[0], LB[1] + .0095]]);
  stampPoly(parts, PANE, [[LB[0] + .0015, LB[1] + .0095], [LB[2] - .0015, LB[1] + .0095], [LB[2] - .0015, LB[3] - .0035], [LB[0] + .0015, LB[3] - .0035]]);
  stampPoly(parts, LID, [[LB[0], LB[3] - .0035], [LB[2], LB[3] - .0035], [LB[2], LB[3]], [LB[0], LB[3]]]);
  SLATS.forEach((s, i) => stampPoly(parts, SLAT + i, board(s)));
  stampPoly(parts, SEAT, board(SEATQ)); stampPoly(parts, RIM, board(RIMQ));
  for (const e of els) if (e.front) for (let i = 1; i < e.pts.length; i++) stampLine(parts, TUBE, e.pts[i - 1][0], e.pts[i - 1][1], e.pts[i][0], e.pts[i][1], e.w / 2 + .3 * U);
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes may
// start there.  Where the land begins is kept column by column.
const SKY = 0, HILL = 1, HEDGE = 2, LEAVES = 3, LIMB = 4, BEDS = 5, STATUE = 6, FENCE = 7, WALL = 8, GRASS = 9, PATH = 10, LANTERN = 11, BENCH = 12, SHRUB = 13;
let ridgeY = null, hedgeY = null, bedY = null, grassY = null, coping = null, nCol = 0;
let zx = 0, zy = 0, zp = 0, zf = 0;                            // what zone() learnt on the way: the point on the photo, the part of what is drawn by hand, what kind of leaves the map has there
const col = x => x < 0 ? 0 : x >= nCol ? nCol - 1 : x | 0;
function treeAt(x, y) {                                        // leaves, a limb, or neither
  if (y >= cFoot) return 0;
  const v = folAt(x, y);
  if (!bare && v > -8) {                                       // the map has it leaf by leaf; in spring and in autumn there are fewer of them
    const k = what(zx, zy);
    if (mxf < along2(CROWN, zy) && (k === M_LEAF ? !thin || lev > 1 || vnoise(x * .2 / U, y * .2 / U) > thin * 1.3 : k === M_WISP && leafHere(x, y))) { zf = k; return LEAVES; }
  }
  return maskAt(x, y) & (bare || v < -8 ? 1 : 4) ? LIMB : 0;    // among leaves the twigs are not to be seen
}
// While it is in leaf the bed grows through the fence: here and there something stands in front of it, the more the nearer its foot.
const grown = (xf, pf) => { const a = ironY(.3, xf); return pf > a && xf < WEND && vnoise(xf * 34 + 7, pf * 50) > .86 - .17 * clamp((pf - a) / (.075 * far(xf))); };
function zone(x, y) {
  const i = col(x);
  let g = zp = partAt(x, y);
  zx = fx(x); zy = fy(y);
  if (g >= SLAT) return BENCH;
  if (y > grassY[i]) return zy > 1.1 && zy > NEAR + (vnoise(zx * 20, 3.3) - .5) * .03 ? PATH : GRASS;
  if (bushAt(x, y) > 0) { zp = 0; return SHRUB; }
  if (g >= LID) return LANTERN;
  if (g === IRON) { if (bare || y > coping[i] || !grown(zx, zy)) return FENCE; g = zp = 0; }
  if (y > coping[i]) return WALL;
  if (y > bedY[i]) return BEDS;
  if (g) return STATUE;
  const t = treeAt(x, y); if (t) return t;
  if (y > hedgeY[i] + (bare ? vnoise(x * .2 / U, y * .03 / U) * 34 * U : 0)) return HEDGE;   // bare, its twigs stand up one by one
  return y > ridgeY[i] ? HILL : SKY;
}
const reach = [0, 0];
function run(x, y, ca, sa, l, ok, step) {                      // how far a stroke may go either way along (ca, sa) while ok() says so
  reach[0] = reach[1] = l;
  for (let d = step; d < l + step; d += step) if (!ok(zone(x + ca * d, y + sa * d))) { reach[1] = Math.min(l, d - step * .5); break; }
  for (let d = step; d < l + step; d += step) if (!ok(zone(x - ca * d, y - sa * d))) { reach[0] = Math.min(l, d - step * .5); break; }
}

// ---------- the light ----------
// By night the candle in the lantern throws its light on whatever is near it, the wall, the fence, the foot of the
// pedestal: there things have the colours they have by day, only yellow.  The picture's colours are mixed for
// eight strengths of that light.
const KEEP = /^(sky|cloud|star|moon|beacon|firefly|sun|lights|haze|fog|rain|flake|paper|cityFar)/;
const warm = c => [c[0] * .98, c[1] * .78, c[2] * .5];
function litPal(i) {
  const set = cur.waldbachLit || (cur.waldbachLit = []);
  if (set[i]) return set[i];
  const day = kit.palette(place, cur.season, cur.weather, false), g = i / 7 * .85, out = {};
  for (const key in own) {
    const a = own[key], b = day[key];
    out[key] = !a || !b || KEEP.test(key) ? a : Array.isArray(a[0]) ? a.map((c, n) => mix(c, warm(b[n] || c), g)) : mix(a, warm(b), g);
  }
  return set[i] = out;
}
function lightAt(xf, pf) {                                     // sets the colours for a point
  pal = own;
  if (!night) return;
  const d = Math.hypot(xf - LAMP[0], (pf - LAMP[1]) * PH / PW), q = d * d, g = Math.min(1, .9 * Math.exp(-q / .0007) + .42 * Math.exp(-q / .005));
  if (g > .03) { const i = Math.min(7, (g * 7 + PP.R() * .9) | 0); if (i) pal = litPal(i); }
}
// Fog is between us and everything, and the more of it the farther off a thing is; rain does a little of the same.
const fogCol = () => night ? mix(own.skyLow, own.fog, .35) : own.fog;
const veil = d => { const a = d * (traits.fog * 1.3 + traits.rain * .2); PP.haze = a > .01 ? [fogCol(), clamp(a, 0, .85)] : null; };
const snowDab = (x, y, r) => PP.dab(x, y, r, pal.snow, { a: .93, den: .9, core: .65, s: 1.6 * U, sx: 1.7, sy: .6, jit: 3 });
const green = t => t < .25 ? mix(pal.folDeep, pal.folDark, t / .25) : t < .55 ? mix(pal.folDark, pal.folMid, (t - .25) / .3) : t < .85 ? mix(pal.folMid, pal.folLight, (t - .55) / .3) : mix(pal.folLight, pal.folSun, (t - .85) / .15 * (pal.accent ? .6 : .9));

// ---------- the hill, the hedge, the trees ----------
let fx0 = 0, fx1 = 0, fy0 = 0, fy1 = 0;                        // the box the saint stands in, in the picture, and a little around it
const hewn = g => g > 0 && g < IRON;
let ax = 0, ay = 0;
function byFigure(x, y, r) {                                   // whether a stroke as long as r would run over him, and which way (ax, ay) lies clear of him
  ax = ay = 0;
  if (x <= fx0 || x >= fx1 || y <= fy0 || y >= fy1) return false;
  for (let d = r; d > U; d *= .5) {
    const a = hewn(partAt(x - d, y)), b = hewn(partAt(x + d, y)), c = hewn(partAt(x, y - d)), e = hewn(partAt(x, y + d));
    if (a || b || c || e) { ax = (a ? 1 : 0) - (b ? 1 : 0); ay = (c ? 1 : 0) - (e ? 1 : 0); return true; }
  }
  return false;
}
function hillStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, k = R(), sz = .75 * U, by = byFigure(x, y, 5.5 * sz);   // beside the saint the dabs are small and keep off him: his outline is to stay clean
  veil(.8);
  if (by) { x += ax * 1.2 * U; y += ay * 1.2 * U; }
  if (bare) {                                                  // a bare wood on a slope: the ground shows through, brown or white
    const g = white ? mix(pal.twigs[2], pal.snow, white * .85) : mix(pal.twigs[2], pal.meadow[2], .3);
    if (k < .55 || by) PP.dab(x, y, by ? 1.3 * sz : (2.5 + R() * 4) * sz, mix(mix(g, pal.twigs[1], .3 + .4 * vnoise(xf * 40, pf * 90)), pal.haze, .35), { a: .85, den: by ? 1.2 : .6, core: .5, s: by ? U : 2 * U });
    else { const l = (4 + R() * 7) * sz; PP.line([[x, y], [x + (R() - .5) * 3 * U, y - l]], mix(pal.twigs[R() < .5 ? 0 : 1], pal.haze, .35), (.8 + R() * .8) * U, { pass: 1, a: .75 }); }
    return;
  }
  // crown beside crown, each light on top and dark underneath
  let c = green(clamp(.2 + light(xf, pf) / 7 * .55 + (R() - .5) * .2));
  if (pal.accent && k > (season === 'spring' ? .985 : .92)) c = pick(pal.accent, R());   // a wild cherry in flower here and there
  const sd = shadow(x, y); if (sd) c = mix(c, pal.folDark, sd * .4);
  PP.dab(x, y, by ? 1.3 * sz : (1.8 + R() * 2.6) * sz, mix(c, pal.haze, .3), { a: .88, den: by ? 1.2 : .8, core: .6, s: by ? U : 1.5 * U, sx: 1.2, sy: .85 });
}
function thicket(x, y, top, behind) {                          // bare twigs, and what shows between them
  const R = PP.R, q = R(), depth = clamp((y - top) / (.2 * PH));
  if (byFigure(x, y, 7 * U)) { PP.dab(x + ax * 1.2 * U, y + ay * 1.2 * U, 1.3 * U, white && q < .2 * white ? pal.snow : mix(behind, pal.twigs[1], .3 + depth * .45), { a: .85, den: 1.2, core: .6, s: U }); return; }   // no twig across the saint
  if (white && q < .2 * white + .06) { snowDab(x, y, (2 + R() * 3.5) * U); return; }
  if (q < .42) { PP.dab(x, y, (3 + R() * 5) * U, mix(behind, pal.twigs[1], .3 + depth * .45), { a: .8, den: .5, core: .4, s: 2.2 * U }); return; }
  const a = -Math.PI / 2 + (R() - .5) * (q < .72 ? .5 : 2.2), l = Math.min((6 + R() * 14) * U, (y - top) * 1.2 + 5 * U), ca = Math.cos(a) * l, sa = Math.sin(a) * l;
  PP.line([[x, y], [x + ca * .5 + (R() - .5) * 3 * U, y + sa * .5], [x + ca, y + sa]], ramp(pal.twigs, R() * .75 + (1 - depth) * .2), (.8 + R() * 1.2) * U, { pass: 1, a: .8 });
}
const GREEN = z => z === LEAVES || z === HEDGE || z === SHRUB || z === FENCE;   // the fence stands in front of them: a stroke may run on behind it
function leafStroke(x, y, k) {                                 // the walnut, the hedge, the bushes: leaves, as light as the photograph has them, the smaller the farther off
  const R = PP.R, q = R(), xf = zx, pf = zy, sz = (k === LEAVES ? .95 : k === SHRUB ? .85 : .7) * U, wisp = k === LEAVES && zf === M_WISP, fenced = y > iy0 - 8 * U;
  veil(k === HEDGE ? .5 : k === SHRUB ? .2 : .3);
  if (bare) {                                                  // the walnut has no leaves then, and is not painted here
    if (k === SHRUB) inBush(xf, pf);
    thicket(x, y, k === SHRUB ? py(stop) : hedgeY[col(x)], mix(mix(pal.twigs[2], pal.haze, k === SHRUB ? .2 : .5), white ? pal.snow : pal.meadow[2], .3));
    if (fenced) ironOver(x - 9 * U, y - 9 * U, x + 9 * U, y + 9 * U);
    return;
  }
  const l0 = k === SHRUB ? bushLight(xf, pf) : light(xf, pf), by = byFigure(x, y, 7 * sz);       // beside the saint the dabs are small and keep off him
  if (wisp || (k === HEDGE && y - hedgeY[col(x)] < .006 * PH)) {   // thin leaves with the sun in them, and the ragged rim of the hedge
    if (!by) PP.dab(x, y, (2 + R() * 3) * sz, pal.accent && season === 'autumn' && q > .6 ? pick(pal.accent, R()) : q < .45 ? pal.folLight : q < .7 ? pal.folSun : pal.folMid, { a: .88, den: .8, core: .6, s: 1.5 * U });
    return;
  }
  let c = green(tone(l0));
  if (pal.bloom && k !== LEAVES && l0 > 3 && q > (season === 'spring' ? .9 : .97)) { PP.dab(x, y, (1.6 + R() * 1.6) * sz, pal.bloom, { a: .9, den: 1.2, core: .7, s: 1.2 * U, jit: 4 }); return; }   // it flowers in May
  if (pal.accent && season === 'autumn' && l0 > 2 && q > .9) c = pick(pal.accent, R());
  const sd = shadow(x, y); if (sd) c = mix(c, pal.folDeep, sd * .4);
  if (by) { PP.dab(x + ax * 1.2 * U, y + ay * 1.2 * U, 1.3 * sz, c, { a: .9, den: 1.3, core: .7, s: U, jit: 8 }); return; }
  const a = R() * Math.PI, ca = Math.cos(a), sa = Math.sin(a), l = (2.5 + R() * 5) * sz, wd = (3 + R() * 4.5) * sz;
  run(x, y, ca, sa, l, GREEN, Math.max(1.5, 2 * sz));
  const r0 = reach[0], r1 = reach[1];
  PP.brush(x - ca * r0, y - sa * r0, x + ca * r1, y + sa * r1, wd, c, { a: .87, sp: 1.25 * U, dry: .18, jit: 9 });
  if (fenced) { const ex = Math.abs(ca) * l + wd * .6, ey = Math.abs(sa) * l + wd * .6; ironOver(x - ex, y - ey, x + ex, y + ey); }   // what of the fence it ran over goes back in front of it
}
function limbStroke() {                                        // a stretch of bark
  const R = PP.R, k = R() * limbWeight;
  let a = 0, b = limbs.length - 1;
  if (b < 0) return;
  while (a < b) { const m = (a + b) >> 1; if (limbSum[m] < k) a = m + 1; else b = m; }
  const s = limbs[a], span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span, mx = limbX(s, tm), my = limbY(s, tm);
  if (mx < 0 || my < 0 || mx >= W || my >= H || !at(mx, my) || zone(mx, my) !== LIMB) return;   // not where leaves or the hedge hide it
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 2 ? .2 : .08), q = R(), o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  veil(.3);
  for (const [u, v] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, u), ay = limbY(s, u), bx = limbX(s, v), by = limbY(s, v);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (white) { if (s.w > 1.3 * U && q < white * .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snow, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax - s.w * .22, ay, bx - s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .5), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });   // the light side
  }
}

// ---------- the bed ----------
function bedStroke(x, y) {                                     // whatever has grown where a garden was: peonies, nettles, long grass
  const R = PP.R, xf = zx, pf = zy, depth = (y - bedY[col(x)]) / PH, sz = U * clamp(.8 + (pf - .6) * 2.4, .75, 1.35), k = R();
  let xe = x, ye = y, r = 5 * sz;                              // where the stroke ends and how wide it is: the fence wants to know
  lightAt(xf, pf); veil(.22);
  if (white && k < white * .8) { r = (2 + R() * 3) * sz; snowDab(x, y, r); r *= 1.8; }
  else if (bare || (k < .6 && reeds(xf) > .5 && depth < .075)) {   // blades, or the stalks the winter has left
    const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .08, l = (bare ? 4 + R() * 8 : 7 + R() * 11) * sz;
    const c = bare ? ramp(pal.bed, .15 + R() * .7) : mix(ramp(pal.bed, .3 + R() * .55), pal.straw, R() * .3);
    xe = x + Math.cos(a) * l; ye = y + Math.sin(a) * l; r = 2 * sz + l * .1;
    PP.brush(x, y, xe, ye, (1 + R() * .9) * sz, c, { a: .88, jit: 8, dry: .12, sp: 1.1 * U, bow: .2 });
  } else if (pal.peony && k > .96 && xf < .47 && depth < .06) {   // the peonies are out
    const r = (2.2 + R() * 1.8) * sz;
    PP.dab(x, y, r, pal.peony[R() < .5 ? 0 : 1], { a: .95, den: 1.6, core: .85, s: 1.2 * U, jit: 5 }); PP.dab(x - r * .2, y - r * .25, r * .5, pal.peony[2], { a: .9, den: 1.4, core: .7, s: U, jit: 4 });
  } else if (pal.cranesbill && k > .93 && xf < .16 && pf > wallY(BACK, xf) - .035) PP.dab(x, y, (.9 + R() * .6) * sz, pal.cranesbill, { a: .95, den: 2, core: .9, s: U, jit: 5 });
  else {                                                       // leaves: light on top, dark in among them
    let c = ramp(pal.bed, clamp(tone(light(xf, pf)) + .12 * smooth(.035, 0, depth)));
    const sd = shadow(x, y); if (sd) c = mix(c, pal.bed[0], sd * .45);
    const a = -Math.PI / 2 + (R() - .5) * 2.4, l = (2.5 + R() * 4) * sz, wd = (1.6 + R() * 2) * sz;
    xe = x + Math.cos(a) * l; ye = y + Math.sin(a) * l; r = wd * .6 + l * .13 + .5 * U;
    PP.brush(x, y, xe, ye, wd, c, { a: .9, jit: 8, dry: .1, sp: 1.15 * U, bow: .25 });
  }
  if (y > iy0 - 8 * U) ironOver(Math.min(x, xe) - r, Math.min(y, ye) - r, Math.max(x, xe) + r, Math.max(y, ye) + r);
}

// ---------- the saint ----------
// He is small, and all of him is stone: every part gets strokes that follow its form and end where it ends, light
// where it turns to the light and dark where it turns away.
function figureStroke(x, y) {
  const R = PP.R, p = zp, xf = zx, pf = zy, side = clamp((xf - .49) / .05, -1, 1) * (evening ? -1 : 1);   // the light comes from the left; in the evening from the right, and low
  lightAt(xf, pf); veil(.26);
  if (p === STAR) {                                            // rusty gold: a ray of it, from its middle
    let sx = 0, sy = 0, near = 1e9;
    for (let i = 0; i < 5; i++) { const s = starAt(i), d = Math.hypot(px(s[0]) - x, py(s[1]) - y); if (d < near) { near = d; sx = px(s[0]); sy = py(s[1]); } }
    const a = ((R() * 5) | 0) / 5 * TAU - Math.PI / 2, r = Math.max(1.7 * U, .0046 * PW);
    if (partAt(sx + Math.cos(a) * r, sy + Math.sin(a) * r) > STAR) return;   // not across his cap
    PP.brush(sx - Math.cos(a) * r * .2, sy - Math.sin(a) * r * .2, sx + Math.cos(a) * r, sy + Math.sin(a) * r, U, night ? mix(pal.starGold, pal.star, .25) : pal.starGold, { a: .95, jit: 5, dry: 0, bow: 0, sp: U });
    return;
  }
  if (white && R() < white && !partAt(x, y - Math.max(2.2 * U, .0055 * PH))) { snowDab(x, y - U, (1.5 + R()) * U); return; }   // snow lies on whatever of him looks up
  const mine = z => z === STATUE && zp === p, e = Math.max(1.6 * U, .0028 * PW), up = Math.max(U, .0017 * PH);
  let lum, set = pal.figure, ca = 0, sa = 1, l = (2 + R() * 4) * U, w = (1.3 + R() * .9) * U, small = p >= NECK && p <= CROSS;
  const grime = smooth(.45, .8, fbm(xf * 150 + 3, pf * 190));
  if (p === CLOAK) {                                           // what paint is left on it is red; its inside is in the shade
    set = pal.robe; lum = .6 + (vnoise(xf * 380 + pf * 90, 1.3) - .5) * .5 - .34 * (xf < .49 ? smooth(.444, .455, xf) : smooth(.531, .521, xf));
    ca = xf < .49 ? -.25 : .25; sa = .97;
  } else if (p === CASSOCK) {                                  // it falls in long folds from his left knee
    lum = .76 - .36 * smooth(.012, .002, Math.abs(xf - (.487 + (pf - .52) * .62))) + (vnoise((xf + pf * .5) * 600, 7) - .5) * .3; ca = .45; sa = .89;
  } else if (p === ROCHET) {                                   // white linen, a hem of lace
    lum = .92 + (vnoise(xf * 700, 2.2 + pf * 8) - .5) * .42;
    if (pf > .513 + (xf - .49) * .12) { lum = Math.floor(xf * 330) & 1 ? .96 : .58; l = 1.6 * U; w = 1.2 * U; }
  } else if (p === CAPE) {                                     // fur, grey and black with weather: short strokes, all hanging down
    set = pal.fur; lum = .62 + (fbm(xf * 260, pf * 300) - .5) * 1.3 + (R() - .5) * .16; ca = .12; sa = .99; l = (1.2 + R() * 2) * U; w = (1.1 + R() * .8) * U;
  } else if (p === CAP) {                                      // the biretta: the horn it turns to us in the shade, its front in the light, dark again where it turns away
    lum = (xf < .4728 ? .1 : .3 + smooth(.473, .486, xf) * .16 - smooth(.4885, .493, xf) * .3 + (vnoise(xf * 800, pf * 800) - .5) * .12) - (partAt(x, y + up) === CAP ? 0 : .1);
    if (R() < .6) { ca = .986; sa = -.168; } else { ca = .36; sa = .93; }   // along its upper edge, or down its side
    l = (2 + R() * 3) * U; w = (1.1 + R() * .5) * U;
  } else if (p === FACE) {                                     // he looks to the left: brow and nose in the light, the cheek turning away
    lum = .97 - smooth(.481, .493, xf) * .26;
    ca = -.06; l = (1.5 + R() * 2.5) * U; w = (1.1 + R() * .6) * U;
  } else if (p === BEARD) { lum = .5 + (vnoise(xf * 2600, pf * 500) - .5) * .3 - smooth(.484, .492, xf) * .16; ca = -.28; sa = .96; l = (1.5 + R() * 2) * U; w = (1 + R() * .5) * U; }
  else if (p === SHADE) { lum = .04; ca = .98; sa = .17; l = 2 * U; w = .8 * U; }   // his eye under the brow, his mouth in the beard
  else if (p === HAIR) { lum = .2 + (vnoise(xf * 2600, pf * 400) - .5) * .2; ca = .1; sa = .99; l = (2 + R() * 3) * U; w = (1 + R() * .6) * U; }   // it falls on his collar
  else if (p === NECK) { lum = .44 - smooth(.4245, .4205, pf) * .2 - smooth(.492, .499, xf) * .12; w = (1.1 + R() * .6) * U; }
  else if (p === PALM) {                                       // a frond: leaflet beside leaflet, all of them pointing up
    lum = .92 - (((xf * 1.3 + pf) * 130) % 1 < .28 ? .24 : 0); ca = .5; sa = -.866; l = (2 + R() * 2.5) * U; w = (1 + R() * .4) * U;
  } else if (p === HAND) {                                     // fingers, one above the other
    const q = (pf - (xf < .49 ? .4598 : .4615)) / (xf < .49 ? .0036 : .0055);
    lum = q - Math.floor(q) > .72 ? .46 : .95; ca = .99; sa = xf < .49 ? -.1 : .12; l = (1.5 + R() * 2) * U; w = Math.max(.8 * U, Math.min(1.3 * U, .0019 * PH));
  } else if (p === CROSS) {                                    // dark, its upper faces in the light; on it the body, small and pale
    const dx = Math.abs(xf - crX(pf)), arm = dx > .0042, dy = Math.abs(pf - armY(xf));
    if (arm) { ca = .996; sa = -.09; lum = partAt(x, y - up) === CROSS ? .12 : .4; if (xf > .437 && xf < .461 && dy < .0012) lum = .66; }
    else { ca = (CR[2] - CR[0]) * PW; sa = (CR[3] - CR[1]) * PH; const q = 1 / Math.hypot(ca, sa); ca *= q; sa *= q; lum = partAt(x - up, y) === CROSS ? .12 : .38; if (pf > .431 && pf < .456 && dx < .002) lum = .76; }
    lum += (R() - .5) * .1; w = (1 + R() * .4) * U; l = (2 + R() * 3) * U;
  } else if (p === BASE || p === PLINTH) { lum = .5 + (fbm(xf * 90, pf * 300) - .5) * .5; ca = 1; sa = 0; l = (3 + R() * 5) * U; }
  else if (p === LEDGE) { lum = pf > .5965 ? .48 : .84; ca = 1; sa = 0; l = (3 + R() * 6) * U; w = 1.5 * U; }
  else if (p === SIDE) { lum = (evening ? .8 : .5) + (fbm(xf * 200, pf * 90) - .5) * .3; l = (3 + R() * 5) * U; }
  else { lum = .8 + (fbm(xf * 120 + 7, pf * 160) - .5) * .3 - (xf < .455 ? .2 : 0) - smooth(.608, .6, pf) * .12; ca = 1; sa = 0; }   // the front of the pedestal: bare stone, the ledge's shade along its top
  // it is round: every part of him is lighter where it turns to the light and darker where it turns away
  if (small) lum -= side * .06;
  else if (p < PLINTH) lum += (partAt(x - e, y) === p ? 0 : .07) - (partAt(x + e, y) === p ? 0 : .2) - (partAt(x, y + e) === p ? 0 : .1) - side * .1;
  lum -= grime * (p === PLINTH || p === BASE ? .3 : small ? .06 : .16) + (wet ? .1 : 0);
  let c = ramp(set, lum);
  if (p === PLINTH || p === BASE) c = mix(c, pal.moss, .2 + grime * .3);
  if (night && pal === own) c = mix(c, traits.veiled ? pal.skyLow : pal.moonGlow, .1 + lum * .13);   // the moon is on him, or what light the clouds send back
  const sd = shadow(x, y); if (sd) c = mix(c, set[0], sd * .22);
  run(x, y, ca, sa, l, mine, small ? .8 * U : U);
  const a = reach[0], b = reach[1];
  run(x, y, -sa, ca, w * .62, mine, Math.max(.5, .4 * U));       // and no wider than there is room for it
  if (reach[0] + reach[1] < w) { const sh = (reach[1] - reach[0]) / 2; x -= sa * sh; y += ca * sh; w = Math.max(.8, reach[0] + reach[1]); }
  PP.brush(x - ca * a, y - sa * a, x + ca * b, y + sa * b, w, c, small ? { a: 1.3, jit: 3, dry: 0, bow: 0, sp: w * .7, full: 1 } : { a: .95, jit: 4, dry: .03, bow: 0, sp: Math.min(1.1 * U, w) });   // the small parts with a loaded brush
  if (xf > .53 && pf > .575) ironOver(x - a - w, y - a - w, x + b + w, y + b + w);
}
function lanternStroke(x, y) {                                 // red glass under a little roof of tin
  const R = PP.R, p = zp, top = py(LB[1]);
  lightAt(zx, zy); veil(.16);
  if (p === LID) {
    if (white && y < top + .01 * PH && R() < white) { snowDab(x, y - U, 1.4 * U); return; }
    PP.dab(x, y, 1.1 * U, mix(own.iron, own.tube, R() * .6), { a: .95, den: 1.8, core: .9, s: .9 * U, jit: 3 });
    return;
  }
  if (night) PP.haze = null;
  const d = Math.hypot(x - px(LAMP[0]), (y - py(LAMP[1])) * .6) / (.006 * PW);
  PP.dab(x, y, 1.1 * U, night ? mix(own.lights[2], own.beacon, clamp(.25 + d * .45)) : mix(own.lampRed, own.hull, zx < LAMP[0] - .002 ? .3 : 0), { a: .96, den: 1.8, core: .9, s: .9 * U, jit: 3 });
}

// ---------- the fence ----------
// Wrought iron is a few dark strokes: each bar and scroll is brushed a stretch at a time, thick and thin as the
// brush happens to take it, with a lighter stroke on the side the sky is on.
function ironPiece(e, s0, s1) {
  const R = PP.R, pts = e.pts, cum = e.cum, tube = e.kind === TUBE;
  veil(tube ? .05 : .18);
  if (!e.len) {                                                // a flower: two short fat strokes across each other, a glint on it
    const x = pts[0][0], y = pts[0][1], r = e.w * .5, o = { a: .95, jit: 4, dry: 0, bow: 0, sp: U };
    if (zone(x, y) !== FENCE) return;
    lightAt(zx, zy);
    PP.brush(x - r, y, x + r, y, r * .8, pal.iron, o); PP.brush(x, y - r, x, y + r, r * .8, mix(pal.iron, pal.tube, .3), o);
    PP.brush(x - r * .5, y - r * .45, x - r * .1, y - r * .5, U, mix(pal.tube, pal.towerLight, .34), { a: .8, jit: 4, dry: 0, bow: 0, sp: U });
    return;
  }
  for (let i = 0; i < pts.length - 1; i++) {
    if (cum[i + 1] <= s0) continue;
    if (cum[i] >= s1) break;
    const d = cum[i + 1] - cum[i], t0 = Math.max(0, (s0 - cum[i]) / d), t1 = Math.min(1, (s1 - cum[i]) / d), p = pts[i], q = pts[i + 1];
    const xa = p[0] + (q[0] - p[0]) * t0, ya = p[1] + (q[1] - p[1]) * t0, xb = p[0] + (q[0] - p[0]) * t1, yb = p[1] + (q[1] - p[1]) * t1, mx = (xa + xb) / 2, my = (ya + yb) / 2;
    if (mx < 0 || my < 0 || mx >= W || my >= H) continue;
    const z = zone(mx, my);                                    // not where a leaf, the bench's boards or the grass are in front of it
    if (e.free ? z !== GRASS && z !== PATH : tube ? z !== BENCH || zp !== TUBE : z !== FENCE) continue;
    lightAt(zx, zy);
    const h = hash1(e.id * 37 + i), w = e.w * (1 - e.taper * (cum[i] + d * (t0 + t1) / 2) / e.len) * (.85 + R() * .3), bw = Math.max(w * .62, w - 1.3 * U), flat = Math.abs(yb - ya) < Math.abs(xb - xa);
    PP.brush(xa, ya, xb, yb, bw, tube ? mix(pal.tube, pal.dark, h * .4) : mix(pal.iron, pal.tube, h * .35 + R() * .12), bw < 2 * U ? { a: 1.1, jit: 4, dry: 0, bow: 0, sp: bw } : { a: 1.1, jit: 4, dry: 0, bow: 0, sp: U, full: 1 });   // a thin one in one go, a thick one filled in
    if (white && e.mark === 1 && flat && R() < white) PP.brush(xa, ya - w * .55, xb, yb - w * .55, w * .6, pal.snow, { a: .95, jit: 3, dry: 0, bow: 0, sp: Math.min(1.1 * U, w * .6) });   // snow lies along the rails
    else if (hash1(e.id * 53 + i * 7) < e.lit) {
      const lw = Math.max(.7 * U, w * .34), k = flat ? .34 : .22;
      PP.brush(xa - w * (flat ? .08 : .3), ya - w * k, xb - w * (flat ? .08 : .3), yb - w * k, lw, mix(pal.tube, pal.towerLight, (tube ? .5 : .34) + (wet ? .14 : 0)), { a: tube ? .95 : .8, jit: 4, dry: 0, bow: 0, sp: lw });
    }
  }
  if (e.knob && s1 >= e.len && s0 < e.len) {                   // a scroll ends in a knob
    const q = pts[pts.length - 1];
    if (q[0] >= 0 && q[1] >= 0 && q[0] < W && q[1] < H && zone(q[0], q[1]) === FENCE) PP.dab(q[0], q[1], e.w * .45, pal.iron, { a: .95, den: 2, core: .9, s: .8 * U, jit: 3 });
  }
  if (white && e.mark === 2 && s0 <= e.top && s1 >= e.top && R() < white) PP.dab(e.tx, e.ty - e.w * .5, e.w * 1.1, pal.snow, { a: .95, den: 1.6, core: .8, s: U, sx: 1.7, sy: .6, jit: 3 });   // and a cap of it on every scroll
}
function ironStroke() {                                        // it is all thin: every piece gets strokes of its own
  const R = PP.R, k = R() * elWeight;
  let a = 0, b = els.length - 1;
  if (b < 0) return;
  while (a < b) { const m = (a + b) >> 1; if (elSum[m] < k) a = m + 1; else b = m; }
  const e = els[a], span = (5 + R() * 9) * U, s0 = e.len > span ? R() * (e.len - span) : 0, sm = Math.min(e.len, s0 + span / 2);
  let i = 0; while (i < e.pts.length - 2 && e.cum[i + 1] < sm) i++;
  const p = e.pts[i], q = e.pts[Math.min(i + 1, e.pts.length - 1)], t = e.len ? (sm - e.cum[i]) / (e.cum[i + 1] - e.cum[i]) : 0;
  if (at(clamp(p[0] + (q[0] - p[0]) * t, 0, W - 1), clamp(p[1] + (q[1] - p[1]) * t, 0, H - 1))) ironPiece(e, s0, s0 + span);
}
function ironOver(xa, ya, xb, yb) {                            // something has been painted behind the fence: what it ran over goes back in front of it at once
  if (yb < iy0 || ya > iy1) return;
  const keep = pal;
  for (let b = Math.max(0, (xa / bucketW) | 0), n = Math.min(buckets.length - 1, (xb / bucketW) | 0); b <= n; b++) for (const e of buckets[b]) {
    if (e.seen === stampN || e.x1 < xa || e.x0 > xb || e.y1 < ya || e.y0 > yb) continue;
    e.seen = stampN;
    if (!e.len) { ironPiece(e, 0, 0); continue; }
    let s0 = 1e9, s1 = -1;
    for (let i = 0, pts = e.pts; i < pts.length - 1; i++) {      // the stretch of it that lies in the box
      const p = pts[i], q = pts[i + 1], dx = q[0] - p[0], dy = q[1] - p[1];
      let t0 = 0, t1 = 1;
      if (dx) { const u = (xa - p[0]) / dx, v = (xb - p[0]) / dx; t0 = Math.max(t0, Math.min(u, v)); t1 = Math.min(t1, Math.max(u, v)); } else if (p[0] < xa || p[0] > xb) continue;
      if (dy) { const u = (ya - p[1]) / dy, v = (yb - p[1]) / dy; t0 = Math.max(t0, Math.min(u, v)); t1 = Math.min(t1, Math.max(u, v)); } else if (p[1] < ya || p[1] > yb) continue;
      if (t1 <= t0) continue;
      const d = e.cum[i + 1] - e.cum[i];
      s0 = Math.min(s0, e.cum[i] + t0 * d); s1 = Math.max(s1, e.cum[i] + t1 * d);
    }
    if (s1 > s0) ironPiece(e, s0 - 1.5 * U, s1 + 1.5 * U);
  }
  stampN++; pal = keep;
}

// ---------- the wall ----------
const bandOf = (xf, pf) => pf < wallY(EDGE, xf) ? 0 : pf < wallY(DRIP, xf) ? 1 : 2;   // the top of the coping, its front, the wall under it
function wallStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, b = bandOf(xf, pf), k = R(), u = -Math.log(VX - xf) * 9;   // u: along it; a stone far off is as long as one close by
  const sl = -(pf - WV) / (VX - xf) * PH / PW, q = 1 / Math.hypot(1, sl), ca = q, sa = sl * q, o = { a: .92, jit: 6, dry: .08, bow: 0, sp: 1.1 * U };
  lightAt(xf, pf); veil(.14);
  let c, l = (3 + R() * 6) * U, w = (1.5 + R() * 1.6) * U;
  if (b === 0) {                                               // moss and old leaves lie on it, dry grass
    const t = (pf - wallY(BACK, xf)) / (wallY(EDGE, xf) - wallY(BACK, xf));
    if (white && k < white * .95) c = pal.snow;
    else if (k < .12 && !wet) { const a = (R() - .5) * 1.2; PP.line([[x, y], [x + Math.cos(a) * 7 * U, y + Math.sin(a) * 4 * U]], pal.straw, .9 * U, { pass: 1, a: .7 }); ironOver(x - 8 * U, y - 5 * U, x + 8 * U, y + 5 * U); return; }
    else { c = mix(mix(pal.cope[0], pal.moss, .25 + fbm(u * 3, pf * 90) * .7), pal.cope[2], smooth(.7, 1, t) * .6); if (wet) c = mix(c, pal.skyLow, .2 + R() * .12); }
  } else if (b === 1) {                                        // concrete, spotted with lichen
    const t = (pf - wallY(EDGE, xf)) / (wallY(DRIP, xf) - wallY(EDGE, xf));
    if (k < .2 && t < .8) { PP.dab(x, y, (.8 + R() * 1.2) * U, pal.lichen, { a: .9, den: 1.3, core: .7, s: U, jit: 5 }); return; }
    c = white && t < .22 ? pal.snow : t > .82 ? mix(pal.cope[0], pal.dark, .5) : ramp(pal.cope, .45 + fbm(u * 6 + 3, t * 2) * .7);
    w = 1.5 * U; l *= 1.5;
  } else {                                                     // rubble in what is left of its render: stone by stone, damp at the foot
    const h = (pf - wallY(DRIP, xf)) / (wallY(FOOT, xf) - wallY(DRIP, xf)), su = u * 2.1 + (vnoise(u * 3, h * 3) - .5) * .9, sh = h * 3.4 + (vnoise(u * 3 + 9, h * 3 + 4) - .5) * .8;
    const row = Math.floor(sh), i = Math.floor(su + hash1(row * 7 + 1)), fu = su + hash1(row * 7 + 1) - i, fv = sh - row, t = hash1(i * 131 + row * 17);
    const joint = Math.min(fu, 1 - fu) < .09 + .1 * t || Math.min(fv, 1 - fv) < .1 + .12 * t || h < .1;   // between the stones, and under the coping, the render
    if (k < (joint ? .16 : .07)) { PP.dab(x, y, (.9 + R() * 1.5) * U, pal.lichen, { a: .8, den: 1.2, core: .5, s: U, jit: 5 }); return; }
    if (joint && k > .975) { const a = R() * Math.PI; PP.line([[x, y], [x + Math.cos(a) * 6 * U, y + Math.sin(a) * 3 * U], [x + Math.cos(a) * 11 * U, y + Math.sin(a + .6) * 6 * U]], mix(pal.rubble[0], pal.dark, .4), .9 * U, { pass: 1, a: .6 }); return; }   // a crack
    c = joint ? ramp(pal.rubble, .72 + (fbm(u * 5, h * 7) - .5) * .5 - smooth(.06, 0, h) * .45) : mix(ramp(pal.rubble, .12 + t * .5 + (fv - .5) * .2 + (R() - .5) * .12), pal.ochre, t > .6 ? .4 : .08);
    c = mix(c, pal.moss, smooth(.72, 1, h) * .4);
    if (wet) c = mix(c, pal.dark, .16);
  }
  if (wet && b) c = mix(c, pal.dark, .1);
  const sd = shadow(x, y); if (sd) c = mix(c, pal.rubble[0], sd * .25);
  run(x, y, ca, sa, l, z => z === WALL && bandOf(zx, zy) === b, 1.5 * U);
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], w, c, o);
  if (b === 0) ironOver(x - reach[0] - U, y - w * .6 - 2 * U, x + reach[1] + U, y + w * .6 + 2 * U);
}

// ---------- the bench ----------
// Its boards are painted along the grain, every stroke inside its board: where in one a point is, how far along
// it (qf) and how far across (qv), and the point at such a place.
let qf = 0, qv = 0;
function inBoard(q, xf, pf) {
  let f = clamp((xf - q[0]) / (q[2] - q[0])), v = 0;
  for (let n = 0; n < 3; n++) {
    const top = q[1] + (q[3] - q[1]) * f, foot = q[7] + (q[5] - q[7]) * f; v = clamp((pf - top) / (foot - top));
    const xl = q[0] + (q[6] - q[0]) * v, xr = q[2] + (q[4] - q[2]) * v; f = clamp((xf - xl) / (xr - xl));
  }
  qf = f; qv = v;
}
const boardX = (q, f, v) => px((q[0] + (q[2] - q[0]) * f) * (1 - v) + (q[6] + (q[4] - q[6]) * f) * v);
const boardY = (q, f, v) => py((q[1] + (q[3] - q[1]) * f) * (1 - v) + (q[7] + (q[5] - q[7]) * f) * v);
function benchShade(xf, pf) {                                  // how dark it is on the ground under the bench, and behind its back
  if (xf < .46 || xf > 1 || pf < .82 || pf > 1.2) return 0;
  const f = clamp((xf - SEATQ[6]) / (SEATQ[4] - SEATQ[6])), front = RIMQ[7] + (RIMQ[5] - RIMQ[7]) * f, back = SEATQ[1] + (SEATQ[3] - SEATQ[1]) * clamp((xf - SEATQ[0]) / (SEATQ[2] - SEATQ[0]));
  if (pf > front) return .75 * smooth(front + .05, front + .008, pf);
  const s = SLATS[0], top = s[1] + (s[3] - s[1]) * clamp((xf - s[0]) / (s[2] - s[0]));
  return pf < back + .01 && pf > top && xf > s[0] && xf < s[4] + .01 ? .55 : 0;
}
function benchStroke(x, y) {
  const R = PP.R, p = zp, xf = zx, pf = zy;
  if (p === TUBE) return;
  lightAt(xf, pf); veil(.05);
  const q = p === SEAT ? SEATQ : p === RIM ? RIMQ : SLATS[p - SLAT];
  inBoard(q, xf, pf);
  const f = qf, deep = Math.hypot(boardX(q, f, 1) - boardX(q, f, 0), boardY(q, f, 1) - boardY(q, f, 0)), long = Math.hypot(boardX(q, 1, .5) - boardX(q, 0, .5), boardY(q, 1, .5) - boardY(q, 0, .5));
  let v = qv, v0 = 0, v1 = 1, c, w = (1.3 + R() * 1.1) * U, l = (5 + R() * 10) * U;
  if (p === SEAT) {                                            // five boards, grey with weather, lichen on them
    const i = Math.min(4, (v * 5) | 0);
    if (i && v * 5 - i < .1) { w = Math.max(.8 * U, deep * .014); v0 = i / 5 - w / 2 / deep; v1 = i / 5 + w / 2 / deep; c = mix(pal.plank[0], pal.dark, .3); }   // the gap between two of them
    else if (white && R() < white * .93) c = pal.snow;
    else {
      v0 = i ? i / 5 + .012 : 0; v1 = (i + 1) / 5 - (i < 4 ? .012 : 0);
      c = ramp(pal.plank, .56 + (vnoise(v * 24 + i * 9, f * 5) - .5) * .8 + (hash1(i * 7 + 3) - .5) * .2 + (R() - .5) * .12 - (1 - v) * .1 + (f > .985 ? .2 : 0));
      if (vnoise(f * 46 + 3, v * 9) > .68) c = mix(c, pal.lichen, .28);
      if (wet) c = mix(mix(c, pal.plank[0], .35), pal.skyLow, .2);
    }
  } else if (p === RIM) {                                      // its edge, in its own shade
    c = white && v < .35 && R() < white ? pal.snow : v < .3 ? ramp(pal.plank, .45 + R() * .2) : mix(pal.plank[0], pal.dark, .2 + R() * .3);
    w = Math.min(w, deep * .5);
  } else {                                                     // a slat: light on its upper edge, dark under it, and what paint is left on it peels
    const i = p - SLAT;
    if (white && v < .4 && R() < white) c = pal.snow;
    else if (v < .17) c = ramp(pal.plank, .74 + R() * .24);
    else if (v > .86) c = mix(pal.plank[0], pal.dark, .35);
    else {
      const painted = fbm(f * 7 + i * 7, v * 2.2 + i) > (i ? .5 : .38) && vnoise(f * 90 + i * 31, v * 9) < .78;
      c = painted ? mix(pal.paint, pal.plank[R() < .5 ? 0 : 1], R() * .3) : ramp(pal.plank, .52 + (vnoise(f * 12 + i * 5, v * 14) - .5) * .5 + R() * .2);
      if (!painted) l *= .6;
    }
    if (wet) c = mix(mix(c, pal.plank[0], .3), pal.skyLow, .15);
  }
  if ((v1 - v0) * deep < w) w = Math.max(.8, (v1 - v0) * deep);
  const m = Math.min((v1 - v0) / 2, w / 2 / deep);
  v = clamp(v, v0 + m, v1 - m);
  const sd = shadow(x, y); if (sd) c = mix(c, pal.plank[0], sd * .25);
  const xc = boardX(q, f, v), yc = boardY(q, f, v), fa = Math.max(0, f - l / long), fb = Math.min(1, f + l / long);
  let xa = boardX(q, fa, v), ya = boardY(q, fa, v), xb = boardX(q, fb, v), yb = boardY(q, fb, v);
  if (p >= SEAT && xf < .56) {                                 // the arm near us is in front of the seat
    const la = Math.hypot(xa - xc, ya - yc), lb = Math.hypot(xb - xc, yb - yc), ca = (xb - xa) / (la + lb || 1), sa = (yb - ya) / (la + lb || 1);
    run(xc, yc, ca, sa, Math.max(la, lb), z => z === BENCH && zp === p, 1.5 * U);
    xa = xc - ca * Math.min(la, reach[0]); ya = yc - sa * Math.min(la, reach[0]); xb = xc + ca * Math.min(lb, reach[1]); yb = yc + sa * Math.min(lb, reach[1]);
  }
  PP.brush(xa, ya, xb, yb, w, c, { a: .93, jit: 6, dry: .06, bow: 0, sp: Math.min(1.1 * U, w) });
}

// ---------- the grass ----------
const nearG = pf => clamp((pf - .6) / .42, .12, 1.3);          // how close the ground is at this height: the size of its strokes
function grassStroke(x, y, path) {
  const R = PP.R, xf = zx, pf = zy, sz = (.5 + nearG(pf) * .8) * U, k = R(), dim = benchShade(xf, pf);
  lightAt(xf, pf); veil(.5 - nearG(pf) * .4);
  if (white && k < white * .9 * (1 - dim)) { snowDab(x, y, (2 + R() * 3.5) * sz); return; }
  const sd = shadow(x, y);
  if (path) {                                                  // trodden earth, grass creeping in from either side
    let c = mix(pal.gravel, pal.meadow[3], smooth(NEAR + .04, NEAR, pf) * .6 + R() * .12);
    if (wet) c = mix(mix(c, pal.dark, .25), pal.skyLow, .18);
    if (dim) c = mix(c, pal.meadow[0], dim * .7);
    const l = (3 + R() * 7) * sz;
    PP.brush(x - l, y, x + l, y + (R() - .5) * U, (1.4 + R() * 1.8) * sz, sd ? mix(c, pal.meadow[0], sd * .3) : c, { a: .88, jit: 7, dry: .14, sp: 1.1 * U, bow: .03 });
    return;
  }
  if (pal.daisy && k > (season === 'spring' ? .975 : .99) && pf > .66 && !dim) { PP.dab(x, y, (.8 + R() * .7) * sz, pick(pal.daisy, R()), { a: .95, den: 1.8, core: .85, s: U, jit: 4 }); return; }
  if (season === 'autumn' && k > .965) { PP.dab(x, y, (1.2 + R() * 1.4) * sz, pick(pal.bits, R()), { a: .9, den: 1.4, core: .8, s: 1.3 * U }); return; }   // leaves the walnut has dropped
  let c = ramp(pal.meadow, .12 + tone(what(xf, pf) === M_GRASS ? lev : 7 * clamp(fbm(xf * 22 + 5, pf * 30) * 1.3 - .15)) * .8);   // where the photograph has no grass, it is as light as it happens to be
  if (k < .08) c = mix(c, pal.straw, .5);
  if (sd) c = mix(c, pal.meadow[0], sd * .45);
  if (dim) c = mix(c, mix(pal.meadow[0], pal.dark, .3), dim);   // no sun gets under the bench
  const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .07, ca = Math.cos(a), sa = Math.sin(a), l = (2.5 + R() * 5) * sz;
  run(x, y, ca, sa, l, z => z !== BENCH && z !== LANTERN, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * Math.min(reach[0], l * .3), y - sa * Math.min(reach[0], l * .3), x + ca * reach[1], y + sa * reach[1], (1.2 + R() * 2) * sz, c, { a: .87, jit: 8, dry: .18, sp: 1.1 * U, bow: .12 });
}

// ---------- every stroke where it belongs ----------
function paintAt(x, y, k) {
  if (k === SKY || k === LIMB || k === FENCE) return;
  if (k === HILL) hillStroke(x, y); else if (k === HEDGE || k === SHRUB || k === LEAVES) leafStroke(x, y, k); else if (k === BEDS) bedStroke(x, y);
  else if (k === STATUE) figureStroke(x, y); else if (k === WALL) wallStroke(x, y); else if (k === BENCH) benchStroke(x, y); else if (k === LANTERN) lanternStroke(x, y);
  else grassStroke(x, y, k === PATH);
}
const within = (xa, ya, xb, yb) => () => {                      // a stroke somewhere in a box
  const R = PP.R, x = xa + R() * (xb - xa), y = ya + R() * (yb - ya);
  if (x >= 0 && x < W && y >= 0 && y < H && at(x, y)) paintAt(x, y, zone(x, y));
};
let boxes = [], landTop = 0;

// ---------- weather, and the night ----------
function fogStroke() {                                         // fog hangs under the hill in drifting banks
  const R = PP.R, x = R() * W, depth = R(), y = py(.42 + depth * .26);
  if (y < 0 || y >= H || !at(x, y) || !traits.fog) return;
  const z = zone(x, y);
  if ((z !== HILL && z !== HEDGE && z !== SHRUB && z !== GRASS && z !== PATH) || fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.haze = null;
  PP.dab(x, y, (8 + R() * 16) * U, fogCol(), { a: .4 - depth * .15, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y = py(.6) + R() * (H - py(.6));
  if (y < 0 || y >= H || !at(x, y) || !traits.glint) return;
  const z = zone(x, y);
  if (z !== GRASS && z !== BEDS && z !== PATH) return;
  PP.haze = null;
  PP.dab(x, y, (.6 + R() * .8) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}
function lampDab() {                                           // the candle's light on the wall around it
  const R = PP.R, x = px(LAMP[0]), y = py(LAMP[1]);
  if (x < 0 || y < 0 || x >= W || y >= H || !at(x, y) || !night) return;
  PP.haze = null;                                              // a light is seen through any fog, and all the wider for it
  PP.dab(x, y + 4 * U, (7 + traits.fog * 5) * U, mix(pal.lights[1], pal.beacon, .35), { a: .08, den: .4, s: 2.4 * U, jit: 3 });
}

// ---------- the coat ----------
function coatStroke(x0, y0, leaves, again) {                   // a fresh coat: broad strokes of what is there, where the brush comes by
  const R = PP.R;
  for (let n = 0; n < 9; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .5) * 21 * U, y = y0 + ((n / 3 | 0) - 1 + (R() - .5) * .5) * 13 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U || y < -12 * U) continue;
    const xc = clamp(x, 0, W - 1), yc = clamp(y, 0, H - 1), k = zone(xc, yc), xf = zx, pf = zy, l = (20 + R() * 10) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
    pal = own;
    if (leaves) {                                              // the trees: they stand in front of the sky
      if (k !== LEAVES || (again && yc > ridgeY[col(xc)] + 4 * U)) continue;
      const a = (R() - .5) * 1.2;
      veil(.3); PP.brush(x - Math.cos(a) * l * .7, y - Math.sin(a) * l * .7, x + Math.cos(a) * l * .7, y + Math.sin(a) * l * .7, zf === M_WISP ? 9 * U : 20 * U, green(.3 + light(xf, pf) / 7 * .3), o);
      continue;
    }
    if (k === SKY || k === LIMB) continue;
    if (k === HILL) { veil(.8); PP.brush(x - l, y, x + l, y - l * .1, 16 * U, mix(bare ? mix(pal.twigs[1], white ? pal.snow : pal.meadow[2], .4) : green(.5), pal.haze, .3), o); }
    else if (k === HEDGE || k === LEAVES || k === SHRUB) { const a = (R() - .5) * 1.2; veil(k === HEDGE ? .5 : k === SHRUB ? .2 : .3); PP.brush(x - Math.cos(a) * l * .8, y - Math.sin(a) * l * .8, x + Math.cos(a) * l * .8, y + Math.sin(a) * l * .8, 20 * U, bare ? mix(mix(pal.twigs[2], pal.haze, .5), pal.twigs[1], .5) : green(.32), o); }
    else if (k === BEDS || k === FENCE) { veil(.22); PP.brush(x - l, y, x + l, y, 16 * U, white ? mix(pal.bed[1], pal.snow, white * .7) : pal.bed[1], o); }
    else if (k === STATUE || k === LANTERN) { veil(.26); PP.brush(x, y - 7 * U, x, y + 7 * U, 5 * U, k === LANTERN ? pal.lampRed : zp === CAPE ? pal.fur[1] : zp === CLOAK ? pal.robe[1] : ramp(pal.figure, zp === STAR || zp === CAP || zp === CROSS || zp === HAIR ? .35 : .62), o); }
    else if (k === WALL) {                                     // the coping is narrow: a stroke right along the middle of its top, and of its front
      const b = bandOf(xf, pf), sl = -(pf - WV) / (VX - xf) * PH / PW, q = 1 / Math.hypot(1, sl), ya = wallY(b ? EDGE : BACK, xf), yb = wallY(b ? DRIP : EDGE, xf), yc = b === 2 ? y : py((ya + yb) / 2);
      veil(.14); PP.brush(x - q * l, yc - sl * q * l, x + q * l, yc + sl * q * l, b === 2 ? 16 * U : (yb - ya) * PH * .9, b === 2 ? pal.rubble[2] : b ? pal.cope[1] : white ? mix(pal.cope[0], pal.snow, white) : mix(pal.cope[0], pal.moss, .4), o);
    } else if (k === BENCH) {
      if (zp === TUBE) continue;
      const sl = -.16 * PH / PW, q = 1 / Math.hypot(1, sl);
      veil(.05); PP.brush(x - q * l * .7, y - sl * q * l * .7, x + q * l * .7, y + sl * q * l * .7, 7 * U, zp === SEAT && white ? mix(pal.plank[2], pal.snow, white) : pal.plank[zp === SEAT ? 2 : zp === RIM ? 0 : 1], o);
    } else { veil(.5 - nearG(pf) * .4); PP.brush(x - l, y, x + l, y, 16 * U, white ? mix(pal.meadow[2], pal.snow, white * .85) : k === PATH ? pal.gravel : pal.meadow[2], o); }
  }
}
let lagCells = null, lagAt = 0;                                // the cells of the coat that is being brushed, how many of them have had their second one

// ---------- what moves ----------
// The candle and the stars are too small and too bright for the brush that paints the rest: they are drawn anew
// every frame.  By day, when it is warm and dry, butterflies are about over the bed.
function drawLife(P, t) {
  const lx = px(LAMP[0]), ly = py(LAMP[1]);
  if (!at(clamp(px(.49), 0, W - 1), clamp(py(.6), 0, H - 1))) return;
  P.haze = null;
  if (night) {
    const fog = traits.fog;
    if (lx > -30 * U && lx < W + 30 * U && ly < H + 30 * U) {
      const a = .8 + .14 * Math.sin(t * 9 + Math.sin(t * 3.1) * 2) + .06 * Math.sin(t * 23), s = Math.max(1.2 * U, .0028 * PH), red = mix(pal.lights[1], pal.beacon, .45);
      P.dab(lx, ly, s * (9 + fog * 5), red, { a: .1 * a, den: .25, s: 3 * U, jit: 3 });                   // the air round it
      P.dab(lx, ly, s * (4.5 + fog * 2), red, { a: .22 * a, den: .6, s: 2 * U, jit: 3 });
      P.dab(lx, ly - s * .3 * a, s * .8, pal.lights[2], { a, den: 2.5, core: a, s: U, sx: .8, sy: 1.3, jit: 2 });   // the flame
    }
    for (let i = 0; i < 5; i++) {                              // his five stars
      const s = starAt(i), x = px(s[0]), y = py(s[1]), a = (.3 + .7 * Math.pow(.5 + .5 * Math.sin(t * (1.1 + i * .23) + i * 2.1), 2)) * (1 - fog * .4);
      if (x < 0 || x >= W || y < 0 || y >= H) continue;
      P.R = rng(7400 + i);                                     // each keeps its own shape while it glows
      P.dab(x, y, 5 * U, pal.star, { a: a * .22, den: .5, s: 2.2 * U, jit: 3 });
      P.dab(x, y, 1.5 * U, pal.star, { a, den: 2.5, core: a, s: U, jit: 3 });
    }
    return;
  }
  if (season === 'winter' || season === 'autumn' || traits.rain || traits.fog) return;
  for (let i = 0; i < 3; i++) {                                // cabbage whites, and a brimstone
    const ph = i * 2.3, w = .8 + i * .27, xf = .2 + i * .22 + .12 * Math.sin(t * .31 * w + ph) + .03 * Math.sin(t * 1.7 + ph), pf = .62 - i * .012 + .045 * Math.sin(t * .47 * w + ph * 2) + .012 * Math.sin(t * 2.3 + ph);
    const x = px(xf), y = py(pf), s = Math.max(2 * U, .0042 * PH), fl = Math.abs(Math.sin(t * 13 + ph)), c = i === 1 ? mix(pal.yellow, pal.hull, .3) : pal.hull;
    if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y)) continue;
    P.R = rng(7500 + i);
    for (const d of [-1, 1]) P.brush(x, y, x + d * s * (.35 + .65 * fl), y - s * (1 - fl * .6), s * .75, c, { a: .95, sp: U, dry: 0, jit: 3, bow: .2 });
  }
}

// ---------- the painting, one frame's worth ----------
const RATE = { land: 11500, tree: 9000, figure: 30000, bench: 9000, iron: 1.5, limb: 1.2, lamp: 6, fog: 1500, glint: 60 };
const place = {
  colors: {
    figure: ['#45433f', '#7b7870', '#aeaaa0', '#d6d2c5', '#f3f0e6'], fur: ['#3a3a3a', '#605f5c', '#8c8a83', '#bcb8ad'], robe: ['#5c4440', '#8d6860', '#b28e83', '#d2b8ab'],
    starGold: '#8f7038', iron: '#1c1f23', tube: '#3d454d', lampRed: '#a52c26',
    rubble: ['#4a4943', '#6f6d64', '#96938a', '#bbb7a9', '#d8d4c5'], ochre: '#b29f78', lichen: '#dfe3d2', cope: ['#55544c', '#7b7970', '#a19e93', '#c4c1b4'], moss: '#59603c', straw: '#b8a877',
    plank: ['#34363a', '#5a5a57', '#86837a', '#aca798', '#cbc6b8'], paint: '#555b63',
    bed: ['#16351c', '#2c6428', '#55982f', '#8fc93e', '#cbe86a'], peony: null, cranesbill: '#c66fd2', bloom: '#f3f1dc', daisy: ['#ffffff', '#f6e9a6', '#ffffff'],
    meadow: ['#1d381c', '#365e2a', '#547f38', '#779944', '#a2b85a'], gravel: '#b7ab93',
    twigs: ['#2b2724', '#463e37', '#685e53', '#8b8073'], snow: '#eef2f4',
  },
  seasons: {
    spring: { bed: ['#1d3f1f', '#3a7a2c', '#6aae38', '#a4d84a', '#d6f07a'], meadow: ['#24481f', '#487c2e', '#72a83c', '#9cc64e', '#c6de70'], peony: ['#b33a62', '#dc7296', '#f2b4c8'], daisy: ['#ffffff', '#f4d23c', '#ffffff'] },
    autumn: { bed: ['#3a3418', '#6d6126', '#a08a32', '#c9ae48', '#e3d078'], meadow: ['#3d3320', '#6f602c', '#9c8838', '#c0a84c', '#e0cc74'], cranesbill: null, bloom: null, daisy: null },
    winter: { bed: ['#3a3327', '#5c5139', '#84754f', '#a8986b', '#c4b88f'], meadow: ['#35382a', '#5a6243', '#878c5a', '#aeab76', '#c9c296'], cranesbill: null, bloom: null, daisy: null },
  },
  ground: /^snow/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    // An upright screen shows the photograph from top to bottom and some of the grass at our feet, and a slice of its
    // width; a wide one loses some of the sky and has the photograph in its middle, what was made up to either side.
    const a = W / H, wide = clamp((a - .75) / .65), tall = clamp((.75 - a) / .29), shown = 1.05 - .19 * wide + .15 * tall;
    Y0 = .12 * wide; PH = H / shown; PW = PH * .75;
    if (W / PW > 3.2) { PW = W / 3.2; PH = PW / .75; Y0 = .98 - H / PH; }                 // a very wide one loses some more of it
    const n = W / PW;                                          // how many photos wide the picture is
    X0 = (1 - n) * (n < 1 ? .7 : .5);
    place.hz = py(.42);
    buildThings(); buildTrees(); buildBushes();
    nCol = Math.ceil(W) + 1;
    ridgeY = new Float32Array(nCol); hedgeY = new Float32Array(nCol); bedY = new Float32Array(nCol); grassY = new Float32Array(nCol); coping = new Float32Array(nCol);
    landTop = H;
    for (let i = 0; i < nCol; i++) {
      const xf = fx(i + .5);
      ridgeY[i] = py(hillTop(xf)); hedgeY[i] = Math.max(ridgeY[i] + 2 * U, py(hedgeTop(xf))); grassY[i] = py(grassTop(xf));
      bedY[i] = xf > WBEG && xf < 1.04 ? py(bedTop(xf)) : 1e9; coping[i] = xf > WBEG && xf < WEND ? py(wallY(BACK, xf)) : 1e9;
      if (ridgeY[i] < landTop) landTop = ridgeY[i];
    }
    landTop = Math.max(0, landTop - 8 * U);
    fx0 = px(SB[0]) - 12 * U; fx1 = px(SB[2]) + 12 * U; fy0 = py(SB[1]) - 12 * U; fy1 = py(SB[3]);
    const ua = U * U * 1e6, split = py(.42), tx = Math.min(W, px(.52));
    const box = (key, rate, xa, ya, xb, yb) => ({ key, n: rate * Math.max(0, Math.min(W, xb) - Math.max(0, xa)) * Math.max(0, Math.min(H, yb) - Math.max(0, ya)) / ua, fn: within(Math.max(0, xa), Math.max(0, ya), Math.min(W, xb), Math.min(H, yb)) });
    boxes = [
      box('wb-tree', RATE.tree, 0, 0, tx, split),                                       // the walnut
      box('wb-ridge', RATE.land, tx, landTop, W, split),                                // the hill and the top of the hedge
      box('wb-mid', RATE.land, 0, split, W, py(.7)), box('wb-low', RATE.land, 0, py(.7), W, H),
      box('wb-figure', RATE.figure, px(SB[0]), py(SB[1]), px(SB[2]), py(SB[3])), box('wb-bust', RATE.figure * 2, px(.426), py(.392), px(.534), py(.48)),   // his head and hands once more: they are small
      box('wb-lantern', RATE.figure * 2, px(LB[0]), py(LB[1]), px(LB[2]), py(LB[3])),
      box('wb-bench', RATE.bench, px(.46), py(.815), px(1), py(1.125)),
    ];
  },
  tick(t) { time = t; sway = wind(t) * 2 * U; },
  top(x) { return ridgeY[col(x)] - U; },
  sky(c, pad) {                                                // down to the ridge of the hill
    const dip = 3 * U - pad, n = nCol;
    let y = ridgeY[n - 1] + dip;
    c.moveTo(0, -60); c.lineTo(W, -60); c.lineTo(W, y);
    for (let i = n - 2; i >= 0; i--) { const e = ridgeY[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(0, y); c.closePath();
  },
  front(s, x, y) { use(s); zx = fx(x); zy = fy(y); return treeAt(x, y) !== 0; },       // the walnut stands in front of the sky, leaves and limbs
  orb(o) {                                                     // the sun and the moon stand clear of the hill
    let top = 1e9;
    for (let i = Math.max(0, (o.x - o.r * 1.6) | 0); i < Math.min(nCol, o.x + o.r * 1.6); i++) top = Math.min(top, ridgeY[i]);
    if (top < 1e9) o.y = Math.max(o.r * 1.4, Math.min(o.y, top - o.r * 1.7));
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    for (const b of boxes) spend(b.key, b.n * pace, b.fn);
    spend('wb-iron', RATE.iron * elWeight / (10 * U) * pace, ironStroke);
    spend('wb-limb', RATE.limb * limbLength / (14 * U) * pace, limbStroke);
    if ((s && s.night) || (o && o.night)) spend('wb-lamp', RATE.lamp * pace, lampDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('wb-fog', RATE.fog * W * .26 * PH / ua * pace, fogStroke);
    if (s && s.traits.glint) spend('wb-glint', RATE.glint * W * (H - py(.6)) / ua * pace, glintDab);
    PP.haze = null;
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) if (cells[k].y > landTop - 50 * U) for (let n = 0; n < 2; n++) coatStroke(cells[k].x + n * 31 * U, cells[k].y + n * 19 * U, false);   // twice over: nothing of the wash under it is to show through
    c.restore();
    // The trees stand in front of the sky, and the sky's coat is broad and long: what it brushes later reaches back
    // over leaves this one has just put in front of it.  They get a second coat once the sky's has passed for good.
    if (!bare) for (let k = i; k < j; k++) if (cells[k].y < cFoot + 30 * U) coatStroke(cells[k].x, cells[k].y, true);
    if (cells !== lagCells) { lagCells = cells; lagAt = 0; }
    const n = cells.length, passed = j >= n ? Infinity : j > 0 ? cells[j - 1].k - (cells[n - 1].k - cells[0].k) / 6 - 170 * U : -Infinity;
    for (; lagAt < n && cells[lagAt].k <= passed; lagAt++) if (!bare) coatStroke(cells[lagAt].x, cells[lagAt].y, true, true);
    PP.haze = null;
  },
  sprites: [{ draw: drawLife, t0: 4, seed: 141 }],
};
return place;
} });
