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
 *  The cable that crossed its sky is not in it.  The lantern is made
 *  up, and so is what lies beyond the photograph: more of the wall and
 *  the trees on the left, the meadow, the path and the fruit trees
 *  under the hill on the right, the path at our feet.  It is an
 *  upright one: a wide screen keeps the saint on its left.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'waldbachsteig', name: 'Waldbachsteig', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, ease, mix, pick, rng, hash1, vnoise, fbm, wind, spend } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0, sway = 0;   // the view (CSS pixels, the stroke unit), the clock and how far the wind pushes the leaves
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let cur = null, own = null, pal = null, traits = null, night = false, season = '', bare = false, wet = false, evening = false, white = 0, thin = 0;   // own: the picture's colours, pal: those at the point being painted
const use = s => {
  cur = s; own = pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; wet = traits.rain > 0; evening = s.weather === 'golden';
  white = traits.snow ? 1 : season !== 'winter' ? 0 : s.weather === 'sunny' ? .7 : s.weather === 'frost' ? .45 : .3;   // how much snow lies
  thin = season === 'autumn' ? .34 : season === 'spring' ? .14 : 0;   // how thin the leaves are on the trees
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph: x and y as fractions of its width and height.  It is an upright one:
// a narrow screen shows a slice of its width, a wide one a slice of its height and what lies to its right.
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
function folLum(x, y) {                                        // light falls in from the upper left
  const s = time * .03, k = .02 / U;
  return .45 + (fbm(x * k + s, y * k * 1.2) - fbm((x - 9 * U) * k + s, (y - 9 * U) * k * 1.2)) * 3.2;
}

// The wall runs away from us to the right, and the fence on it: whatever is level along them runs to a point far
// off on that side (VX), the wall's a little lower (WV) than the fence's (FV).  A thing there is as big as far()
// says, of what it is at the photograph's left edge.
const VX = 2.09, WV = .7, FV = .655;
const far = xf => (VX - xf) / VX;
const BACK = .8, EDGE = .845, DRIP = .865, FOOT = 1.04;        // the coping: where it begins, its front edge, its underside; the foot of the wall
const wallY = (k, xf) => WV + (k - WV) * far(xf);
const WEND = .895, GATE = .925, END = 1.034;                   // where the wall ends at the steps, where it goes on beyond them, and where it ends for good
const walled = xf => xf < WEND || (xf > GATE && xf < END);
// The fence: its upper and its lower rail, and where its posts stand.  They are evenly spaced, which is how this counts them.
const RAIL0 = .68, RAIL1 = .782;
const postX = n => (.2113 * n + .075) / (.1013 * n + 1), bayOf = xf => (xf - .075) / (.2113 - .1013 * xf);
const ironY = (v, xf, deep) => FV + (RAIL0 - FV + (deep && v > 0 ? v * 1.4 : v) * (RAIL1 - RAIL0)) * far(xf);   // v: 0 on the upper rail, 1 on the lower; the gate reaches further down

// The lie of the land: the ridge of the hill, the top of the hedge, of what grows in the bed, of the grass in front.
const RIDGE = [[-3, .41], [-.5, .408], [0, .405], [.3, .4], [.44, .39], [.47, .379], [.5, .369], [.55, .361], [.6, .355], [.7, .343], [.8, .33], [.9, .325], [1, .328],
  [1.15, .322], [1.4, .345], [1.7, .392], [1.95, .405], [2.3, .378], [2.7, .36], [3.1, .385], [3.6, .4]];
const hillTop = xf => along2(RIDGE, xf) + (vnoise(xf * 70, 2.2) - .5) * .008 + (vnoise(xf * 230, 6.1) - .5) * .004;
const BUSHES = [[-3, .62], [.3, .6], [.36, .5], [.42, .46], [.5, .45], [.53, .44], [.57, .43], [.62, .405], [.67, .385], [.75, .375], [.8, .36], [.84, .343], [.87, .35], [.9, .365], [1, .365],
  [1.1, .385], [1.2, .43], [1.3, .5], [1.4, .575], [1.47, .612], [4, .612]];
const hedgeTop = xf => along2(BUSHES, xf) + (vnoise(xf * 13, 4.4) - .5) * .045 * smooth(.9, 1.1, xf) + (vnoise(xf * 45, 4.4) - .5) * .03 + (vnoise(xf * 160, 8.3) - .5) * .012;
const BEDTOP = [[-3, .6], [0, .6], [.1, .605], [.2, .59], [.3, .575], [.36, .555], [.42, .565], [.447, .6], [.456, .648], [.54, .652], [.56, .64], [.6, .632], [.7, .625], [.8, .63], [.9, .635], [1.04, .64]];
const reeds = xf => smooth(.27, .33, xf) * smooth(.455, .43, xf);                   // the long grass by the pedestal
const bedTop = xf => along2(BEDTOP, xf) + (vnoise(xf * 60, 1.7) - .5) * .02 + (vnoise(xf * 210, 3.3) - .5) * .012 - reeds(xf) * .04 * vnoise(xf * 420, 5.5);
function grassTop(xf) {                                        // long grass at the foot of the wall, the meadow beyond it
  if (xf > END) return .64 + (vnoise(xf * 30, 6.6) - .5) * .012;
  if (xf > WEND) return (xf > GATE ? .8 : .775) + (vnoise(xf * 90, 2.9) - .5) * .028;
  return wallY(FOOT, xf) - .008 - (.015 + .07 * smooth(.26, .42, xf)) * (.35 + .65 * vnoise(xf * 14, 7.7)) - .014 * vnoise(xf * 150, 1.1);
}
const NEAR = 1.14;                                             // from here down it is the path we stand on
const pathX = pf => 1.33 + (1 - pf) * 1.35 + .05 * Math.sin(pf * 14), pathW = pf => .012 + Math.max(0, pf - .64) * .28;   // the path: where it runs at a height, half its width

// ---------- the trees ----------
// A crown is a heap of rounds: [x, y, half as wide, half as high].  The walnut's are measured, and so are the
// places where one sees through it.  Where its leaves are is kept in a coarse grid, ragged edge and all.  For
// winter, and where the leaves are thin, the trees are grown: a trunk, limbs, twigs, none of them beyond the crown.
const WALNUT = [[.09, .21, .14, .15], [.3, .27, .17, .135], [.42, .24, .075, .09], [.38, .4, .09, .08], [.22, .46, .2, .13], [.05, .5, .15, .14], [-.05, .1, .12, .12], [-.2, .22, .2, .2], [-.25, .46, .2, .16]];
const GAPS = [[.1, .32, .1, .055], [.17, .365, .05, .025], [.245, .155, .035, .028]];
const ORCHARD = [[1.47, 1], [1.62, .7], [2.12, 1.1], [2.5, .8], [2.62, 1], [3.05, .9]];   // fruit trees at the foot of the hill: [x, how big]
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
  for (let i = 1, x = -.66; x > x0; i++, x -= .5 + hash1(i * 13) * .25) {             // more of them beyond the photograph
    const k = .8 + hash1(i * 7) * .4;
    tree([x, .7, .3 - .2 * k, 1, 0, .016], [[x + .02, .3, .26 * k, .2 * k], [x - .2 * k, .46, .2 * k, .15], [x + .2 * k, .45, .16 * k, .13]]);
  }
  for (const [x, k] of ORCHARD) tree([x, .652, .652 - .105 * k, .9, 0, .013], [[x, .652 - .06 * k, .05 * k, .042 * k], [x - .028 * k, .652 - .045 * k, .034 * k, .03 * k], [x + .03 * k, .652 - .05 * k, .034 * k, .03 * k]]);
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
  const leafy = (x, y) => { const pf = fy(y); return pf < .7 && body(fx(x), pf) > .03; };
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
  mask = new Uint8Array(gCols * gRows); limbWeight = limbLength = 0; limbSum = new Float32Array(limbs.length);   // where a limb is (1), one of the bigger ones (4)
  limbs.forEach((s, i) => {
    limbWeight += s.len * (s.w + 2 * U); limbSum[i] = limbWeight; limbLength += s.len;
    for (let k = 0, n = Math.ceil(s.len / (gCell * .5)); k <= n; k++) stamp(mask, s.depth < 3 ? 5 : 1, limbX(s, k / n), limbY(s, k / n), s.w / 2 + .3 * U, true);
  });
}
const folAt = (x, y) => { const i = ((x - sway) / cCell) | 0, j = (y / cCell) | 0; return i < 0 || j < 0 || i >= cCols || j >= cRows ? -9 : crown[j * cCols + i]; };
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / gCell) | 0) * gCols + ((x / gCell) | 0)];

// ---------- what is drawn by hand ----------
// The saint, the lantern, the fence and the bench are kept in a grid: which part of which of them a cell belongs
// to, the one nearest to us that covers it.
const STAR = 1, CLOAK = 2, BASE = 3, CASSOCK = 4, ROCHET = 5, CAPE = 6, HEAD = 7, CAP = 8, PALM = 9, HAND = 10, CROSS = 11, PLINTH = 12, LEDGE = 13, DIE = 14, SIDE = 15, WRIT = 16,
  IRON = 40, LID = 50, PANE = 51, SLAT = 60, SEAT = 63, TUBE = 64;
let parts = null, gCols = 0, gRows = 0, gCell = 2;
const partAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : parts[((y / gCell) | 0) * gCols + ((x / gCell) | 0)];
function stamp(grid, id, x, y, rad, or) {                      // a round spot of it
  const m = Math.ceil(rad / gCell), ix = Math.floor(x / gCell), iy = Math.floor(y / gCell);
  for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
    const cx = ix + dx, cy = iy + dy;
    if (cx < 0 || cy < 0 || cx >= gCols || cy >= gRows || Math.hypot(dx, dy) * gCell > rad + gCell * .35) continue;
    if (or) grid[cy * gCols + cx] |= id; else grid[cy * gCols + cx] = id;
  }
}
function stampLine(id, xa, ya, xb, yb, rad) { for (let k = 0, n = Math.ceil(Math.hypot(xb - xa, yb - ya) / (gCell * .5)) || 1; k <= n; k++) stamp(parts, id, xa + (xb - xa) * k / n, ya + (yb - ya) * k / n, rad); }
function stampPoly(id, pts) {                                  // a patch of the photograph
  const p = pts.map(q => [px(q[0]), py(q[1])]);
  let xa = 1e9, xb = -1e9, ya = 1e9, yb = -1e9;
  for (const q of p) { xa = Math.min(xa, q[0]); xb = Math.max(xb, q[0]); ya = Math.min(ya, q[1]); yb = Math.max(yb, q[1]); }
  for (let j = Math.max(0, Math.floor(ya / gCell)); j <= Math.min(gRows - 1, Math.floor(yb / gCell)); j++)
    for (let i = Math.max(0, Math.floor(xa / gCell)); i <= Math.min(gCols - 1, Math.floor(xb / gCell)); i++)
      if (inPoly(p, (i + .5) * gCell, (j + .5) * gCell)) parts[j * gCols + i] = id;
}

// The saint, from the back to the front: his cloak hanging from either arm, the cassock, the rochet over it, the fur
// cape on his shoulders, head and biretta, the palm, the crucifix; under him the pedestal, its front turned a little
// to the left.
const FIGURE = [
  [CLOAK, [[.4588, .4344], [.445, .443], [.433, .4536], [.4256, .4712], [.428, .484], [.438, .492], [.45, .497], [.4555, .486], [.4534, .47], [.459, .458]]],
  [CLOAK, [[.5196, .4344], [.5325, .4488], [.5385, .4712], [.5389, .4968], [.533, .5045], [.526, .508], [.5154, .4872], [.512, .4744], [.524, .4696]]],
  [BASE, [[.462, .5765], [.5385, .5785], [.5395, .5845], [.461, .584]]],
  [CASSOCK, [[.464, .526], [.5, .5195], [.5282, .5205], [.5197, .536], [.526, .558], [.5367, .5725], [.538, .5795], [.4685, .578], [.4705, .559], [.466, .54]]],
  [ROCHET, [[.4566, .474], [.462, .452], [.49, .45], [.514, .452], [.516, .488], [.527, .508], [.5295, .519], [.524, .5245], [.5, .5205], [.48, .5225], [.4555, .529], [.4545, .5]]],
  [CAPE, [[.4816, .4222], [.4609, .4272], [.4588, .4344], [.463, .4552], [.4737, .4616], [.4823, .4568], [.4897, .4696], [.5004, .4688], [.5068, .4616], [.5132, .4552], [.5196, .4344], [.517, .4264], [.5004, .4214]]],
  [HEAD, [[.4752, .4072], [.4935, .4035], [.4995, .408], [.4998, .4165], [.4955, .422], [.4895, .4235], [.4795, .421], [.476, .4165], [.4742, .4108]]],
  [CAP, [[.4688, .3985], [.4923, .3952], [.4938, .4042], [.4748, .4078]]],
  [PALM, [[.5143, .4616], [.511, .4488], [.5143, .4344], [.5197, .424], [.526, .4344], [.5303, .4424], [.525, .4552], [.5197, .4632]]],
];
const PEDESTAL = [
  [PLINTH, [[.4605, .5835], [.5385, .5835], [.539, .5905], [.4585, .5905]]], [LEDGE, [[.446, .5895], [.54, .5895], [.5405, .5995], [.4465, .5995]]],
  [DIE, [[.451, .5995], [.52, .5995], [.52, .67], [.451, .67]]], [SIDE, [[.52, .5995], [.539, .5995], [.539, .67], [.52, .67]]],
  [WRIT, [[.468, .6215], [.51, .6215], [.51, .6495], [.468, .6495]]],
];
const CR = [.4487, .4224, .4684, .4744], ARM = [.431, .4296, .4662, .4272];   // the crucifix: its upright and its arm
const HALO = [.488, .396, .019, .018], STARS = [-1.57, -2.7, -.44, .35, 2.8];  // the ring round his head, and where on it the stars sit
const starAt = i => [HALO[0] + Math.cos(STARS[i]) * HALO[2], HALO[1] + Math.sin(STARS[i]) * HALO[3]];
const SB = [.42, .37, .545, .672];                             // the box all of him stands in
const LAMP = [.578, .781], LB = [.571, .764, .585, .792];      // the lantern on the wall: its flame, its box
// What is written on the pedestal, letter by letter: each a few lines in a box.
const GLYPHS = {
  S: [[[1, .12], [.55, 0], [.05, .22], [.5, .5], [.95, .78], [.5, 1], [0, .88]]], T: [[[0, 0], [1, 0]], [[.5, 0], [.5, 1]]], J: [[[.25, 0], [1, 0]], [[.7, 0], [.7, .8], [.4, 1], [.1, .8]]],
  O: [[[.5, 0], [1, .3], [1, .7], [.5, 1], [0, .7], [0, .3], [.5, 0]]], H: [[[0, 0], [0, 1]], [[1, 0], [1, 1]], [[0, .5], [1, .5]]], N: [[[0, 1], [0, 0], [1, 1], [1, 0]]],
  E: [[[1, 0], [0, 0], [0, 1], [1, 1]], [[0, .5], [.7, .5]]], P: [[[0, 1], [0, 0], [.8, 0], [1, .25], [.8, .5], [0, .5]]], M: [[[0, 1], [0, 0], [.5, .6], [1, 0], [1, 1]]], '.': [[[.35, .86], [.65, 1]]],
};
const WRITING = [['ST.JOH.', .4726, .5036, .6232, .631], ['NEPOM.', .4737, .5068, .633, .6408]];
let letters = [];

// The fence, bay by bay: from post to post it is u wide, from the upper rail (v = 0) to the lower one (v = 1) high.
// From a flower in its middle four scrolls go out to the corners, two more ride on the upper rail.  [points, how thick]
const curl = (cu, cv, ru, rv, a0, turns, tail) => { const p = tail.slice(); for (let i = 0; i <= 12; i++) { const t = i / 12, a = a0 + t * turns * TAU, r = 1 - t * .72; p.push([cu + Math.cos(a) * ru * r, cv + Math.sin(a) * rv * r]); } return p; };
const BAY = [];
for (const side of [0, 1]) {
  const m = p => side ? p.map(q => [1 - q[0], q[1]]) : p;
  BAY.push([m(curl(.39, -.13, .075, .12, -1.9, 1.2, [[.02, -.04], [.1, -.1], [.2, -.2]])), 2.2, 2]);
  BAY.push([m(curl(.2, .27, .12, .17, .7, -1.25, [[.47, .46], [.38, .4]])), 2.2, 2]);
  BAY.push([m(curl(.2, .73, .12, .17, -.7, 1.25, [[.47, .54], [.38, .6]])), 2.2, 0]);
}
for (let i = 0; i < 3; i++) BAY.push([[[i / 3, 0], [(i + 1) / 3, 0]], 2.8, 1], [[[i / 3, 1], [(i + 1) / 3, 1]], 2.8, 1], [[[i / 3, .5], [(i + 1) / 3, .5]], 2, 0]);
BAY.push([[[.5, 0], [.5, 1]], 2.2, 0], [[[.5, .5]], 7, 0]);
const TUBES = [                                                // the bench's arms, each a loop of bent tube, and what it stands on; the first four stand in front of the seat
  [[.4618, .9694], [.4741, .9549], [.4973, .9514], [.5186, .9578], [.5252, .9723], [.5245, 1], [.515, 1.022], [.49, 1.027], [.468, 1.018], [.4612, .995], [.4618, .9694]], [[.478, .985], [.513, .988]],
  [[.8763, .89], [.8898, .8737], [.94, .8722], [.9505, .8824], [.9459, .8925], [.9034, .8934]], [[.853, .845], [.855, .9027], [.8686, .9143]],
  [[.499, .985], [.4836, .875]], [[.482, 1], [.482, 1.085]], [[.603, 1.055], [.603, 1.14]], [[.93, .945], [.93, .995]], [[.975, .95], [.975, 1]],
];
let els = [], elSum = null, elWeight = 0, buckets = [], bucketW = 30, stampN = 1, iy0 = 0, iy1 = 0;
function buildIron() {
  els = [];
  const add = (pts, w, kind, mark) => {                        // mark: where snow lies on a piece of the fence (1 along it, 2 on top of it); whether a tube of the bench stands in front of its seat
    const e = { pts, w, kind, mark, x0: 1e9, x1: -1e9, y0: 1e9, y1: -1e9, len: 0, mx: 0, my: 0, seen: 0 };
    for (let i = 0; i < pts.length; i++) {
      const p = pts[i]; e.x0 = Math.min(e.x0, p[0]); e.x1 = Math.max(e.x1, p[0]); e.y0 = Math.min(e.y0, p[1]); e.y1 = Math.max(e.y1, p[1]);
      if (i) e.len += Math.hypot(p[0] - pts[i - 1][0], p[1] - pts[i - 1][1]);
    }
    if (e.x1 < -20 * U || e.x0 > W + 20 * U || e.y0 > H + 20 * U) return;
    const m = pts[pts.length >> 1]; e.mx = pts.length === 2 ? (pts[0][0] + pts[1][0]) / 2 : m[0]; e.my = pts.length === 2 ? (pts[0][1] + pts[1][1]) / 2 : m[1];
    els.push(e);
    const id = kind === TUBE ? TUBE : IRON, rad = w / 2 + .3 * U;
    if (pts.length === 1) stamp(parts, id, pts[0][0], pts[0][1], w * .6);
    for (let i = 1; i < pts.length; i++) stampLine(id, pts[i - 1][0], pts[i - 1][1], pts[i][0], pts[i][1], rad);
  };
  const n0 = Math.max(-9, Math.floor(bayOf(Math.max(-3.5, fx(0)))) - 1);
  for (let n = n0; n <= 9; n++) {
    const gate = n === 6, big = n === 3 || n === 6 || n === 7 || n === 9, xp = postX(n), k = far(xp) * PH / 1100, thick = b => Math.max(.85 * U, b * k);
    const pt = (u, v, deep) => { const xf = postX(n + u); return [px(xf), py(ironY(v, xf, deep))]; };
    if (n < 9) for (const [pts, w, mark] of BAY) add(pts.map(q => pt(q[0], q[1], gate)), thick(w), IRON, mark);
    if (big) {                                                 // a tall post, twisted towards its tip, a scroll on its shoulder
      add([pt(0, -.5), pt(0, n === 3 ? 1.3 : 1.75)], thick(9), IRON, 0);
      add([pt(0, -.95), pt(0, -.5)], thick(4.5), IRON, 0);
      add(curl(-.085, -.5, .055, .09, .2, -1.2, [[0, -.22], [-.04, -.34]]).map(q => pt(q[0], q[1])), thick(2.2), IRON, 2);
    } else {
      add([pt(0, n === 1 ? -.32 : -.4), pt(0, 1.3)], thick(3.2), IRON, 0);
      if (n !== 1) for (const s of [-1, 1]) add([pt(0, -.25), pt(s * .04, -.33), pt(s * .055, -.27)], thick(2), IRON, 0);   // a lily on top
    }
  }
  TUBES.forEach((t, i) => add(t.map(q => [px(q[0]), py(q[1])]), Math.max(1.6 * U, .0075 * PW), TUBE, i < 4 ? 1 : 0));
  elWeight = 0; elSum = new Float32Array(els.length);
  bucketW = 30 * U; buckets = []; for (let i = 0, n = Math.ceil(W / bucketW) + 1; i < n; i++) buckets.push([]);
  els.forEach((e, i) => {
    elWeight += e.len + 12 * U; elSum[i] = elWeight;
    if (e.kind === IRON) for (let b = Math.max(0, (e.x0 / bucketW) | 0), n = Math.min(buckets.length - 1, (e.x1 / bucketW) | 0); b <= n; b++) buckets[b].push(e);
  });
  iy0 = py(.575); iy1 = py(.86);
}
// The bench: three slats to lean against, each by its four corners (left and right end of its upper edge, then of
// its lower one), and the seat.
const SLATS = [[.4656, .8737, .855, .8186, .8616, .8447, .478, .9085], [.4819, .9105, .8686, .8499, .8724, .8635, .4838, .9268], [.4838, .9294, .8724, .865, .8751, .8766, .4865, .9468]];
const SEAT0 = [.478, 1.003], SEAT1 = [.8937, .9114], SEAT2 = [.6056, 1.0608], SEAT3 = [.9923, .9491];   // the ends of its back edge, of its front edge
function buildThings() {
  gCell = 1.5 * U; gCols = Math.ceil(W / gCell) + 1; gRows = Math.ceil(H / gCell) + 1;
  parts = new Uint8Array(gCols * gRows);
  for (let i = 0; i < 5; i++) { const s = starAt(i); stamp(parts, STAR, px(s[0]), py(s[1]), Math.max(1.5 * U, .0042 * PW)); }
  for (const [id, pts] of FIGURE) stampPoly(id, pts);
  stamp(parts, HAND, px(.5155), py(.468), Math.max(1.5 * U, .0078 * PW));
  stampLine(CROSS, px(ARM[0]), py(ARM[1]), px(ARM[2]), py(ARM[3]), Math.max(U, .0034 * PW));
  stampLine(CROSS, px(CR[0]), py(CR[1]), px(CR[2]), py(CR[3]), Math.max(U, .0036 * PW));
  stamp(parts, HAND, px(.4585), py(.4635), Math.max(1.4 * U, .0062 * PW));
  for (const [id, pts] of PEDESTAL) stampPoly(id, pts);
  buildIron();
  stampPoly(LID, [[LB[0], LB[1] + .007], [LAMP[0], LB[1]], [LB[2], LB[1] + .007], [LB[2], LB[1] + .0095], [LB[0], LB[1] + .0095]]);
  stampPoly(PANE, [[LB[0] + .0015, LB[1] + .0095], [LB[2] - .0015, LB[1] + .0095], [LB[2] - .0015, LB[3] - .0035], [LB[0] + .0015, LB[3] - .0035]]);
  stampPoly(LID, [[LB[0], LB[3] - .0035], [LB[2], LB[3] - .0035], [LB[2], LB[3]], [LB[0], LB[3]]]);
  SLATS.forEach((s, i) => stampPoly(SLAT + i, [[s[0], s[1]], [s[2], s[3]], [s[4], s[5]], [s[6], s[7]]]));
  stampPoly(SEAT, [SEAT0, SEAT1, [.9884, .9352], [.9962, .9433], SEAT3, SEAT2]);
  for (const e of els) if (e.kind === TUBE && e.mark) for (let i = 1; i < e.pts.length; i++) stampLine(TUBE, e.pts[i - 1][0], e.pts[i - 1][1], e.pts[i][0], e.pts[i][1], e.w / 2 + .3 * U);
  letters = [];
  for (const [text, xa, xb, ya, yb] of WRITING) {
    let n = 0; for (const ch of text) n += ch === '.' ? .5 : 1;
    const slot = (xb - xa) / n;
    let x = xa;
    for (const ch of text) {
      const w = ch === '.' ? slot * .5 : slot, bx = px(x + w * .14), bw = w * .72 * PW, by = py(ya), bh = (yb - ya) * PH;
      letters.push({ x0: bx, x1: bx + bw, y0: by, y1: by + bh, lines: GLYPHS[ch].map(l => l.map(q => [bx + q[0] * bw, by + q[1] * bh])) });
      x += w;
    }
  }
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes may
// start there.  Where the land begins is kept column by column.
const SKY = 0, HILL = 1, HEDGE = 2, LEAVES = 3, LIMB = 4, BEDS = 5, STATUE = 6, FENCE = 7, WALL = 8, GRASS = 9, PATH = 10, LANTERN = 11, BENCH = 12;
let ridgeY = null, hedgeY = null, bedY = null, grassY = null, coping = null, nCol = 0;
let zx = 0, zy = 0, zp = 0, zf = 0;                            // what zone() learnt on the way: the point on the photo, the part of what is drawn by hand, how deep in the leaves
const col = x => x < 0 ? 0 : x >= nCol ? nCol - 1 : x | 0;
function treeAt(x, y) {                                        // leaves, a limb, or neither
  if (y >= cFoot) return 0;
  const v = zf = folAt(x, y);
  if (!bare && v > thin) return LEAVES;
  return maskAt(x, y) & (bare || v < -8 ? 1 : 4) ? LIMB : 0;    // among leaves the twigs are not to be seen
}
function zone(x, y) {
  const i = col(x), g = zp = partAt(x, y);
  zx = fx(x); zy = fy(y);
  if (g >= SLAT) return BENCH;
  if (y > grassY[i]) return (zx > 1.05 && Math.abs(zx - pathX(zy)) < pathW(zy)) || (zy > 1.1 && zy > NEAR + (vnoise(zx * 20, 3.3) - .5) * .03) ? PATH : GRASS;
  if (g >= LID) return LANTERN;
  if (g === IRON) return FENCE;
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
const byFigure = (x, y, r) => x > fx0 && x < fx1 && y > fy0 && y < fy1 && (hewn(partAt(x - r, y)) || hewn(partAt(x + r, y)) || hewn(partAt(x, y - r)) || hewn(partAt(x, y + r)));   // whether a stroke as long as r would run over him
function hillStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, k = R(), sz = .75 * U;
  veil(.8);
  if (bare) {                                                  // a bare wood on a slope: the ground shows through, brown or white
    const g = white ? mix(pal.twigs[2], pal.snow, white * .85) : mix(pal.twigs[2], pal.meadow[2], .3);
    if (k < .55) PP.dab(x, y, (2.5 + R() * 4) * sz, mix(mix(g, pal.twigs[1], .3 + .4 * vnoise(xf * 40, pf * 90)), pal.haze, .35), { a: .85, den: .6, core: .5, s: 2 * U });
    else { const l = (4 + R() * 7) * sz; PP.line([[x, y], [x + (R() - .5) * 3 * U, y - l]], mix(pal.twigs[R() < .5 ? 0 : 1], pal.haze, .35), (.8 + R() * .8) * U, { pass: 1, a: .75 }); }
    return;
  }
  // crown beside crown, each light on top and dark underneath
  let c = green(clamp(.2 + vnoise(xf * 110, pf * 150 + 3) * .65 + (fbm(xf * 9 + 1, pf * 14) - .5) * .5 + (R() - .5) * .2));
  if (pal.accent && k > (season === 'spring' ? .985 : .92)) c = pick(pal.accent, R());   // a wild cherry in flower here and there
  const sd = shadow(x, y); if (sd) c = mix(c, pal.folDark, sd * .4);
  PP.dab(x, y, (1.8 + R() * 2.6) * sz, mix(c, pal.haze, .3), { a: .88, den: .8, core: .6, s: 1.5 * U, sx: 1.2, sy: .85 });
}
function thicket(x, y, top, behind) {                          // bare twigs, and what shows between them
  const R = PP.R, q = R(), depth = clamp((y - top) / (.2 * PH));
  if (white && q < .2 * white + .06) { snowDab(x, y, (2 + R() * 3.5) * U); return; }
  if (q < .42) { PP.dab(x, y, (3 + R() * 5) * U, mix(behind, pal.twigs[1], .3 + depth * .45), { a: .8, den: .5, core: .4, s: 2.2 * U }); return; }
  const a = -Math.PI / 2 + (R() - .5) * (q < .72 ? .5 : 2.2), l = Math.min((6 + R() * 14) * U, (y - top) * 1.2 + 5 * U), ca = Math.cos(a) * l, sa = Math.sin(a) * l;
  PP.line([[x, y], [x + ca * .5 + (R() - .5) * 3 * U, y + sa * .5], [x + ca, y + sa]], ramp(pal.twigs, R() * .75 + (1 - depth) * .2), (.8 + R() * 1.2) * U, { pass: 1, a: .8 });
}
function hedgeStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, top = hedgeY[col(x)], k = R(), sz = .85 * U;
  veil(.5);
  if (bare) thicket(x, y, top, mix(mix(pal.twigs[2], pal.haze, .5), white ? pal.snow : pal.meadow[2], .3));
  else {
    // it is dark under the bushes, and darkest behind the saint
    const lum = clamp(folLum(x, y) + .12 - smooth(.5, .64, pf) * .4 - .32 * smooth(.36, .42, xf) * smooth(.58, .53, xf));
    if (y - top < .006 * PH) PP.dab(x, y, (2.5 + R() * 4) * sz, lum > .45 ? pal.folLight : pal.folMid, { a: .85, den: .35, s: 2.2 * U });   // the ragged rim: single leaves
    else if (pal.bloom && lum > .4 && k > (season === 'spring' ? .88 : .965)) PP.dab(x, y, (1.6 + R() * 1.6) * sz, pal.bloom, { a: .9, den: 1.2, core: .7, s: 1.2 * U, jit: 4 });   // it flowers in May
    else {
      let c = green(lum);
      if (pal.accent && season === 'autumn' && lum > .3 && k > .9) c = pick(pal.accent, R());
      const sd = shadow(x, y); if (sd) c = mix(c, pal.folDeep, sd * .4);
      const a = R() * Math.PI, l = (3 + R() * 6) * sz;
      if (byFigure(x, y, l)) PP.dab(x, y, 1.5 * sz, c, { a: .9, den: 1.2, core: .7, s: 1.2 * U, jit: 8 });
      else PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (3 + R() * 4.5) * sz, c, { a: .86, sp: 1.3 * U, dry: .2, jit: 10 });
    }
  }
  if (y > iy0 - 8 * U) ironOver(x - 9 * U, y - 9 * U, x + 9 * U, y + 9 * U);
}
const treeOff = xf => xf > 1.1 ? .62 : .3;                      // the fruit trees stand far off, under the hill
function leafStroke(x, y) {                                    // the walnut and its neighbours
  const R = PP.R, d = zf - thin, k = R(), sz = (zx > 1.1 ? .7 : 1.05) * U;
  veil(treeOff(zx));
  if (d < .22) {                                               // thin leaves with the sun in them
    if (!byFigure(x, y, 5 * sz)) PP.dab(x, y, (2 + R() * 3) * sz, pal.accent && season === 'autumn' && k > .6 ? pick(pal.accent, R()) : k < .45 ? pal.folLight : k < .7 ? pal.folSun : pal.folMid, { a: .88, den: .8, core: .6, s: 1.5 * U });
    return;
  }
  let c = green(clamp(folLum(x, y) + .08 + (R() - .5) * .2 - smooth(.42, .6, zy) * .3));
  if (pal.accent && season === 'autumn' && k > .9) c = pick(pal.accent, R());
  const sd = shadow(x, y); if (sd) c = mix(c, pal.folDeep, sd * .4);
  const a = R() * Math.PI, l = (3 + R() * 6) * sz;
  if (byFigure(x, y, l)) PP.dab(x, y, 1.5 * sz, c, { a: .9, den: 1.2, core: .7, s: 1.2 * U, jit: 8 });
  else PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (3 + R() * 4.5) * sz, c, { a: .87, sp: 1.3 * U, dry: .18, jit: 9 });
}
function limbStroke() {                                        // a stretch of bark
  const R = PP.R, k = R() * limbWeight;
  let a = 0, b = limbs.length - 1;
  if (b < 0) return;
  while (a < b) { const m = (a + b) >> 1; if (limbSum[m] < k) a = m + 1; else b = m; }
  const s = limbs[a], span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span, mx = limbX(s, tm), my = limbY(s, tm);
  if (mx < 0 || my < 0 || mx >= W || my >= H || !at(mx, my) || zone(mx, my) !== LIMB) return;   // not where leaves or the hedge hide it
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 2 ? .2 : .08), q = R(), o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  veil(treeOff(zx));
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
  lightAt(xf, pf); veil(.22);
  if (white && k < white * .8) snowDab(x, y, (2 + R() * 3) * sz);
  else if (bare || (k < .6 && reeds(xf) > .5 && depth < .075)) {   // blades, or the stalks the winter has left
    const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .08, l = (bare ? 4 + R() * 8 : 7 + R() * 11) * sz;
    const c = bare ? ramp(pal.bed, .15 + R() * .7) : mix(ramp(pal.bed, .3 + R() * .55), pal.straw, R() * .3);
    PP.brush(x, y, x + Math.cos(a) * l, y + Math.sin(a) * l, (1 + R() * .9) * sz, c, { a: .88, jit: 8, dry: .12, sp: 1.1 * U, bow: .2 });
  } else if (pal.peony && k > .96 && xf < .47 && depth < .06) {   // the peonies are out
    const r = (2.2 + R() * 1.8) * sz;
    PP.dab(x, y, r, pal.peony[R() < .5 ? 0 : 1], { a: .95, den: 1.6, core: .85, s: 1.2 * U, jit: 5 }); PP.dab(x - r * .2, y - r * .25, r * .5, pal.peony[2], { a: .9, den: 1.4, core: .7, s: U, jit: 4 });
  } else if (pal.cranesbill && k > .93 && xf < .16 && pf > wallY(BACK, xf) - .035) PP.dab(x, y, (.9 + R() * .6) * sz, pal.cranesbill, { a: .95, den: 2, core: .9, s: U, jit: 5 });
  else {                                                       // leaves: light on top, dark in among them
    let c = ramp(pal.bed, clamp(.5 + (fbm(xf * 30 + 2, pf * 40) - .5) * 1.7 + (R() - .5) * .3 + .25 * smooth(.035, 0, depth) - .25 * smooth(.03, .12, depth)));
    const sd = shadow(x, y); if (sd) c = mix(c, pal.bed[0], sd * .45);
    const a = -Math.PI / 2 + (R() - .5) * 2.4, l = (2.5 + R() * 4) * sz;
    PP.brush(x, y, x + Math.cos(a) * l, y + Math.sin(a) * l, (1.6 + R() * 2) * sz, c, { a: .9, jit: 8, dry: .1, sp: 1.15 * U, bow: .25 });
  }
  if (y > iy0 - 8 * U) ironOver(x - 8 * sz, y - 14 * sz, x + 8 * sz, y + 8 * sz);
}

// ---------- the saint ----------
function writStroke(x, y) {                                    // the stone, and what is cut into it: ST. JOH. NEPOM.
  const R = PP.R, l = (2 + R() * 3) * U, w = 1.8 * U, c = ramp(pal.figure, .7 + (R() - .5) * .12 - (wet ? .08 : 0));
  run(x, y, 1, 0, l, z => z === STATUE && zp === WRIT, U);
  const xa = x - reach[0], xb = x + reach[1];
  PP.brush(xa, y, xb, y, w, c, { a: .94, jit: 4, dry: .04, bow: 0, sp: 1.1 * U });
  const lw = Math.max(.8, .0011 * PH), ink = mix(pal.writ, pal.figure[0], .3);
  for (const g of letters) if (g.x1 > xa - U && g.x0 < xb + U && g.y1 > y - w && g.y0 < y + w) for (const ln of g.lines) PP.line(ln, ink, lw, { pass: 1, wob: .1 * U, a: .9, sharp: 1, step: 999, jit: 3 });
  if (fy(y) > .643 && R() < .3) PP.line([[x, y - 1.5 * U], [x + (R() - .5) * U, y + 1.5 * U]], ink, lw, { pass: 1, wob: .1 * U, a: .6, step: 999 });   // a year under it, worn away
}
function figureStroke(x, y) {
  const R = PP.R, p = zp, xf = zx, pf = zy, side = clamp((xf - .49) / .05, -1, 1) * (evening ? -1 : 1);   // the light comes from the left; in the evening from the right, and low
  lightAt(xf, pf); veil(.26);
  if (p === STAR) { PP.dab(x, y, Math.max(1.2 * U, .003 * PW), night ? mix(pal.starGold, pal.star, .25) : pal.starGold, { a: .95, den: 2.2, core: .9, s: U, jit: 4 }); return; }
  if (white && R() < white && !partAt(x, y - Math.max(2.2 * U, .0055 * PH))) { snowDab(x, y - U, (1.5 + R()) * U); return; }   // snow lies on whatever of him looks up
  if (p === WRIT) { writStroke(x, y); return; }
  const mine = z => z === STATUE && zp === p, o = { a: .95, jit: 4, dry: .03, bow: 0, sp: 1.1 * U }, e = Math.max(1.6 * U, .0028 * PW);
  let lum, set = pal.figure, ca = 0, sa = 1, l = (2 + R() * 4) * U, w = (1.3 + R() * .9) * U, dab = 0;
  const grime = smooth(.45, .8, fbm(xf * 150 + 3, pf * 190));
  if (p === CLOAK) {                                           // what paint is left on it is red; its inside is in the shade
    set = pal.robe; lum = .6 + (vnoise(xf * 380 + pf * 90, 1.3) - .5) * .5 - .34 * (xf < .49 ? smooth(.444, .455, xf) : smooth(.531, .521, xf));
    ca = xf < .49 ? -.25 : .25; sa = .97;
  } else if (p === CASSOCK) {                                  // it falls in long folds from his left knee
    lum = .76 - .36 * smooth(.012, .002, Math.abs(xf - (.487 + (pf - .52) * .62))) + (vnoise((xf + pf * .5) * 600, 7) - .5) * .3; ca = .45; sa = .89;
  } else if (p === ROCHET) {                                   // white linen, a hem of lace
    lum = .92 + (vnoise(xf * 700, 2.2 + pf * 8) - .5) * .42;
    if (pf > .513 + (xf - .49) * .12) { lum = Math.floor(xf * 900) & 1 ? .95 : .55; l = 1.5 * U; }
  } else if (p === CAPE) { set = pal.fur; lum = .64 + (fbm(xf * 260, pf * 300) - .5) * 1.2; dab = 1.25; }   // fur, grey and black with weather
  else if (p === HEAD) {                                       // he looks to the left: the brow in the shade of his cap, the beard; his hair behind
    lum = xf > .4925 ? .5 + (R() - .5) * .2 : pf > .4135 + (xf - .4775) * .31 ? .7 + (vnoise(xf * 900, pf * 900) - .5) * .5 : pf > .4082 && pf < .41 && xf < .486 ? .32 : .95;
    dab = 1;
  } else if (p === CAP) { lum = pf < .3995 + (.4923 - xf) * .12 ? .7 : .4; ca = 1; sa = -.14; }
  else if (p === PALM) { lum = .9 - (((xf * 1.3 + pf) * 420) % 1 < .3 ? .34 : 0); ca = .2; sa = -.98; }
  else if (p === HAND) { lum = .9; dab = 1.2; }
  else if (p === CROSS) {                                      // the upright, or the arm; on it the body, small and pale
    const arm = xf < .4455 || (xf < .466 && pf < .433 && Math.abs(pf - (ARM[1] + (xf - ARM[0]) / (ARM[2] - ARM[0]) * (ARM[3] - ARM[1]))) < .0034);
    lum = .3 + (R() - .5) * .12;
    if (arm) { ca = 1; sa = -.09; } else { ca = (CR[2] - CR[0]) * PW; sa = (CR[3] - CR[1]) * PH; const q = 1 / Math.hypot(ca, sa); ca *= q; sa *= q; }
    if (pf > .431 && pf < .456 && Math.abs(xf - (CR[0] + (pf - CR[1]) / (CR[3] - CR[1]) * (CR[2] - CR[0]))) < .0022) { lum = .8; dab = 1; }
  } else if (p === BASE || p === PLINTH) { lum = .5 + (fbm(xf * 90, pf * 300) - .5) * .5; ca = 1; sa = 0; l = (3 + R() * 5) * U; }
  else if (p === LEDGE) { lum = pf > .5965 ? .48 : .84; ca = 1; sa = 0; l = (3 + R() * 6) * U; w = 1.5 * U; }
  else if (p === SIDE) { lum = (evening ? .8 : .5) + (fbm(xf * 200, pf * 90) - .5) * .3; l = (3 + R() * 5) * U; }
  else {                                                       // the front of the pedestal: leaves and scrolls carved over what is written
    lum = .8 + (fbm(xf * 120 + 7, pf * 160) - .5) * .3 - (xf < .455 ? .2 : 0);
    if (pf > .603 && pf < .6195) { const q = Math.sin((xf - .4855) * 520) * Math.cos((pf - .611) * 620); lum += q > .35 ? .12 : q < -.35 ? -.26 : 0; l = 2 * U; }
    ca = 1; sa = 0;
  }
  // it is round: every part of him is lighter where it turns to the light and darker where it turns away
  if (p < PLINTH) lum += p === HEAD || p === HAND || p === CAP ? -side * .06 : (partAt(x - e, y) === p ? 0 : .07) - (partAt(x + e, y) === p ? 0 : .2) - (partAt(x, y + e) === p ? 0 : .1) - side * .1;
  lum -= grime * (p === PLINTH || p === BASE ? .3 : .16) + (wet ? .1 : 0);
  let c = ramp(set, lum);
  if (p === PLINTH || p === BASE) c = mix(c, pal.moss, .2 + grime * .3);
  if (night && pal === own) c = mix(c, traits.veiled ? pal.skyLow : pal.moonGlow, .1 + lum * .13);   // the moon is on him, or what light the clouds send back
  const sd = shadow(x, y); if (sd) c = mix(c, set[0], sd * .22);
  if (dab) PP.dab(x, y, dab * Math.max(U, .0016 * PH), c, { a: .95, den: 1.5, core: .8, s: U, jit: 4 });
  else { run(x, y, ca, sa, l, mine, U); PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], w, c, o); }
  if (xf > .53 && pf > .575) ironOver(x - 6 * U, y - 6 * U, x + 6 * U, y + 6 * U);
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
function drawIron(e) {                                         // a bar of it, a scroll, a flower
  const R = PP.R;
  lightAt(fx(e.mx), fy(e.my)); veil(.18);
  const c = e.kind === TUBE ? mix(pal.tube, pal.dark, R() * .3) : mix(pal.iron, pal.tube, R() * .25);
  if (e.pts.length === 1) { PP.dab(e.mx, e.my, e.w * .55, c, { a: 1, den: 2.4, core: 1, s: .9 * U, jit: 3 }); return; }
  PP.line(e.pts, c, e.w, { pass: 1, wob: .2 * U, a: .95, step: 999, jit: 5 });
  if (e.kind === TUBE) { if (R() < .5) PP.line(e.pts.map(q => [q[0] - e.w * .2, q[1] - e.w * .25]), mix(pal.tube, pal.towerLight, .35), e.w * .3, { pass: 1, wob: .2 * U, a: .7, step: 999 }); return; }   // the light on it
  if (!white || !e.mark || R() > white) return;
  if (e.mark === 1) PP.line(e.pts.map(q => [q[0], q[1] - e.w * .7]), pal.snow, e.w * .8, { pass: 1, wob: .2 * U, a: .95, step: 999, jit: 3 });   // snow lies along the rails, and a cap of it on every scroll
  else { let t = e.pts[0]; for (const q of e.pts) if (q[1] < t[1]) t = q; PP.dab(t[0], t[1] - e.w * .5, e.w * 1.2, pal.snow, { a: .95, den: 1.6, core: .8, s: U, sx: 1.7, sy: .6, jit: 3 }); }
}
function ironStroke() {                                        // it is all thin: every piece gets strokes of its own
  const R = PP.R, k = R() * elWeight;
  let a = 0, b = els.length - 1;
  if (b < 0) return;
  while (a < b) { const m = (a + b) >> 1; if (elSum[m] < k) a = m + 1; else b = m; }
  const e = els[a], x = clamp(e.mx, 0, W - 1), y = clamp(e.my, 0, H - 1);
  if (!at(x, y)) return;
  const z = zone(x, y);
  if (e.kind === TUBE ? z === BENCH && zp === TUBE : z === FENCE) drawIron(e);
}
function ironOver(xa, ya, xb, yb) {                            // something has been painted behind the fence: it goes back in front of it at once
  if (yb < iy0 || ya > iy1) return;
  const keep = pal;
  for (let b = Math.max(0, (xa / bucketW) | 0), n = Math.min(buckets.length - 1, (xb / bucketW) | 0); b <= n; b++) for (const e of buckets[b]) {
    if (e.seen === stampN || e.x1 < xa || e.x0 > xb || e.y1 < ya || e.y0 > yb) continue;
    e.seen = stampN; drawIron(e);
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
  if (b === 0) ironOver(x - reach[0] - U, y - 4 * U, x + reach[1] + U, y + 4 * U);
}

// ---------- the bench ----------
const off = (a, b, x, y) => ((px(b[0]) - px(a[0])) * (y - py(a[1])) - (py(b[1]) - py(a[1])) * (x - px(a[0]))) / Math.hypot(px(b[0]) - px(a[0]), py(b[1]) - py(a[1]));   // how far below a line a point is
function benchStroke(x, y) {
  const R = PP.R, p = zp, xf = zx, pf = zy, o = { a: .93, jit: 6, dry: .06, bow: 0, sp: 1.1 * U };
  if (p === TUBE) return;
  lightAt(xf, pf); veil(.05);
  let sl, c, w = (1.4 + R() * 1.2) * U, l = (4 + R() * 9) * U;
  if (p === SEAT) {                                            // five boards, grey with weather, lichen on them
    const db = off(SEAT0, SEAT1, x, y), df = -off(SEAT2, SEAT3, x, y), t = clamp(db / (db + df)), i = Math.min(4, (t * 5) | 0), fr = t * 5 - i;
    sl = (SEAT1[1] - SEAT0[1]) / (SEAT1[0] - SEAT0[0]) + ((SEAT3[1] - SEAT2[1]) / (SEAT3[0] - SEAT2[0]) - (SEAT1[1] - SEAT0[1]) / (SEAT1[0] - SEAT0[0])) * t;
    if (fr < .1 && i) { c = mix(pal.plank[0], pal.dark, .3); w = U; }
    else if (white && R() < white * .93) c = pal.snow;
    else {
      c = ramp(pal.plank, .5 + (fbm(xf * 60 + i * 3, t * 9) - .5) * 1.1 + (hash1(i * 7 + 3) - .5) * .2 + (R() - .5) * .15);
      if (vnoise(xf * 40 + 3, t * 6) > .62) c = mix(c, pal.lichen, .3);
      if (wet) c = mix(mix(c, pal.plank[0], .35), pal.skyLow, .2);
    }
  } else {                                                     // a slat: what paint is left on it peels
    const s = SLATS[p - SLAT], f = clamp((xf - s[0]) / (s[2] - s[0])), top = s[1] + (s[3] - s[1]) * f, v = clamp((pf - top) / (s[7] + (s[5] - s[7]) * f - top));
    sl = (s[3] - s[1]) / (s[2] - s[0]);
    if (white && v < .4 && R() < white) c = pal.snow;
    else {
      const painted = fbm(f * 16 + p * 7, v * 2.5 + p) > (p === SLAT ? .3 : .5) + v * .16;
      c = painted ? mix(pal.paint, pal.plank[R() < .5 ? 0 : 2], R() * .25) : ramp(pal.plank, .55 + R() * .45);
      if (v < .16) c = mix(c, pal.plank[4], .3); else if (v > .86) c = mix(c, pal.plank[0], .5);
      if (wet) c = mix(mix(c, pal.plank[0], .3), pal.skyLow, .15);
      if (!painted) l *= .5;
    }
  }
  sl *= PH / PW; const q = 1 / Math.hypot(1, sl), ca = q, sa = sl * q;
  const sd = shadow(x, y); if (sd) c = mix(c, pal.plank[0], sd * .25);
  run(x, y, ca, sa, l, z => z === BENCH && zp === p, 1.5 * U);
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], w, c, o);
}

// ---------- the grass ----------
const nearG = pf => clamp((pf - .6) / .42, .12, 1.3);          // how close the ground is at this height: the size of its strokes
function grassStroke(x, y, path) {
  const R = PP.R, xf = zx, pf = zy, sz = (.5 + nearG(pf) * .8) * U, k = R();
  lightAt(xf, pf); veil(.5 - nearG(pf) * .4);
  if (white && k < white * .9) { snowDab(x, y, (2 + R() * 3.5) * sz); return; }
  const sd = shadow(x, y);
  if (path) {                                                  // trodden earth, grass creeping in from either side
    let c = mix(pal.gravel, pal.meadow[3], (xf > 1.05 && pf < NEAR ? smooth(.55, 1, Math.abs(xf - pathX(pf)) / pathW(pf)) : smooth(NEAR + .04, NEAR, pf)) * .6 + R() * .12);
    if (wet) c = mix(mix(c, pal.dark, .25), pal.skyLow, .18);
    const l = (3 + R() * 7) * sz;
    PP.brush(x - l, y, x + l, y + (R() - .5) * U, (1.4 + R() * 1.8) * sz, sd ? mix(c, pal.meadow[0], sd * .3) : c, { a: .88, jit: 7, dry: .14, sp: 1.1 * U, bow: .03 });
    return;
  }
  if (pal.daisy && k > (season === 'spring' ? .975 : .99) && pf > .66) { PP.dab(x, y, (.8 + R() * .7) * sz, pick(pal.daisy, R()), { a: .95, den: 1.8, core: .85, s: U, jit: 4 }); return; }
  if (season === 'autumn' && k > .965) { PP.dab(x, y, (1.2 + R() * 1.4) * sz, pick(pal.bits, R()), { a: .9, den: 1.4, core: .8, s: 1.3 * U }); return; }   // leaves the walnut has dropped
  let c = ramp(pal.meadow, .2 + fbm(xf * 22 + 5, pf * 30) * .75 + (R() - .5) * .3);
  if (k < .08) c = mix(c, pal.straw, .5);
  if (sd) c = mix(c, pal.meadow[0], sd * .45);
  const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .07, ca = Math.cos(a), sa = Math.sin(a), l = (2.5 + R() * 5) * sz;
  run(x, y, ca, sa, l, z => z !== BENCH && z !== LANTERN, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * Math.min(reach[0], l * .3), y - sa * Math.min(reach[0], l * .3), x + ca * reach[1], y + sa * reach[1], (1.2 + R() * 2) * sz, c, { a: .87, jit: 8, dry: .18, sp: 1.1 * U, bow: .12 });
}

// ---------- every stroke where it belongs ----------
function paintAt(x, y, k) {
  if (k === SKY || k === LIMB || k === FENCE) return;
  if (k === HILL) hillStroke(x, y); else if (k === HEDGE) hedgeStroke(x, y); else if (k === LEAVES) leafStroke(x, y); else if (k === BEDS) bedStroke(x, y);
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
  if ((z !== HILL && z !== HEDGE && z !== GRASS && z !== PATH) || fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
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
      veil(treeOff(xf)); PP.brush(x - Math.cos(a) * l * .7, y - Math.sin(a) * l * .7, x + Math.cos(a) * l * .7, y + Math.sin(a) * l * .7, zf - thin < .3 ? 9 * U : 20 * U, green(.3 + clamp(folLum(x, y)) * .3), o);
      continue;
    }
    if (k === SKY || k === LIMB) continue;
    if (k === HILL) { veil(.8); PP.brush(x - l, y, x + l, y - l * .1, 16 * U, mix(bare ? mix(pal.twigs[1], white ? pal.snow : pal.meadow[2], .4) : green(.5), pal.haze, .3), o); }
    else if (k === HEDGE || k === LEAVES) { const a = (R() - .5) * 1.2; veil(k === HEDGE ? .5 : treeOff(xf)); PP.brush(x - Math.cos(a) * l * .8, y - Math.sin(a) * l * .8, x + Math.cos(a) * l * .8, y + Math.sin(a) * l * .8, 20 * U, bare ? mix(mix(pal.twigs[2], pal.haze, .5), pal.twigs[1], .5) : green(.32), o); }
    else if (k === BEDS || k === FENCE) { veil(.22); PP.brush(x - l, y, x + l, y, 16 * U, white ? mix(pal.bed[1], pal.snow, white * .7) : pal.bed[1], o); }
    else if (k === STATUE || k === LANTERN) { veil(.26); PP.brush(x, y - 7 * U, x, y + 7 * U, 5 * U, k === LANTERN ? pal.lampRed : zp === CAPE ? pal.fur[1] : zp === CLOAK ? pal.robe[1] : ramp(pal.figure, zp === STAR ? .3 : .62), o); }
    else if (k === WALL) {                                     // the coping is narrow: a stroke right along the middle of its top, and of its front
      const b = bandOf(xf, pf), sl = -(pf - WV) / (VX - xf) * PH / PW, q = 1 / Math.hypot(1, sl), ya = wallY(b ? EDGE : BACK, xf), yb = wallY(b ? DRIP : EDGE, xf), yc = b === 2 ? y : py((ya + yb) / 2);
      veil(.14); PP.brush(x - q * l, yc - sl * q * l, x + q * l, yc + sl * q * l, b === 2 ? 16 * U : (yb - ya) * PH * .9, b === 2 ? pal.rubble[2] : b ? pal.cope[1] : white ? mix(pal.cope[0], pal.snow, white) : mix(pal.cope[0], pal.moss, .4), o);
    } else if (k === BENCH) {
      if (zp === TUBE) continue;
      const sl = -.16 * PH / PW, q = 1 / Math.hypot(1, sl);
      veil(.05); PP.brush(x - q * l * .7, y - sl * q * l * .7, x + q * l * .7, y + sl * q * l * .7, 7 * U, zp === SEAT && white ? mix(pal.plank[2], pal.snow, white) : pal.plank[zp === SEAT ? 2 : 1], o);
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
    starGold: '#8f7038', writ: '#4a463f', iron: '#1c1f23', tube: '#3d454d', lampRed: '#a52c26',
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
    // width; a wide one loses some of the sky and has the saint on its left, the hill and the meadow beyond him.
    const a = W / H, wide = clamp((a - .75) / .65), tall = clamp((.75 - a) / .29), shown = 1.05 - .19 * wide + .15 * tall;
    Y0 = .12 * wide; PH = H / shown; PW = PH * .75;
    if (W / PW > 3.2) { PW = W / 3.2; PH = PW / .75; Y0 = .98 - H / PH; }                 // a very wide one loses some more of it
    const n = W / PW;                                          // how many photos wide the picture is
    X0 = n < 1 ? (1 - n) * .7 : .49 - (.49 - .28 * smooth(1, 1.6, n)) * n;
    place.hz = py(.42);
    buildThings(); buildTrees();
    nCol = Math.ceil(W) + 1;
    ridgeY = new Float32Array(nCol); hedgeY = new Float32Array(nCol); bedY = new Float32Array(nCol); grassY = new Float32Array(nCol); coping = new Float32Array(nCol);
    landTop = H;
    for (let i = 0; i < nCol; i++) {
      const xf = fx(i + .5);
      ridgeY[i] = py(hillTop(xf)); hedgeY[i] = Math.max(ridgeY[i] + 2 * U, py(hedgeTop(xf))); grassY[i] = py(grassTop(xf));
      bedY[i] = xf < END ? py(bedTop(xf)) : 1e9; coping[i] = walled(xf) ? py(wallY(BACK, xf)) : 1e9;
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
      box('wb-figure', RATE.figure, px(SB[0]), py(SB[1]), px(SB[2]), py(SB[3])),
      box('wb-lantern', RATE.figure * 2, px(LB[0]), py(LB[1]), px(LB[2]), py(LB[3])),
      box('wb-bench', RATE.bench, px(.46), py(.815), px(1), py(1.07)),
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
  front(s, x, y) { use(s); return treeAt(x, y) !== 0; },       // the walnut stands in front of the sky, leaves and limbs
  orb(o) {                                                     // the sun and the moon stand clear of the hill
    let top = 1e9;
    for (let i = Math.max(0, (o.x - o.r * 1.6) | 0); i < Math.min(nCol, o.x + o.r * 1.6); i++) top = Math.min(top, ridgeY[i]);
    if (top < 1e9) o.y = Math.max(o.r * 1.4, Math.min(o.y, top - o.r * 1.7));
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    for (const b of boxes) spend(b.key, b.n * pace, b.fn);
    spend('wb-iron', RATE.iron * els.length * pace, ironStroke);
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
