/* ------------------------------------------------------------------ *
 *  Vienna, on the Robinson playground: the tree house, seen from
 *  below.  A hut of boards with a round hole under its gable and a
 *  window of two halves under that, a roof of dark shingles and, on a
 *  pole above it, a flag in all the colours of the rainbow.  It stands
 *  on a cone of old grey planks, and the bushes have all but taken it
 *  back: a shrub hung with old man's beard on the left, an elder on
 *  the right, ash at its feet and ivy over the top of the cone.
 *
 *  It is painted after a photograph, measured by hand: the hut, the
 *  cone and the flag where they are on it, the bushes as a heap of
 *  crowns.  On it children looked out of the window and a girl stood
 *  in the bushes: the window is empty and dark, and nobody is about.
 *  The photograph is an upright one: the clearing a wide screen shows
 *  to the left of it, its trees and the wood behind them, is made up.
 *  The flag flies as the wind blows, and at night somebody has left a
 *  lantern burning in the window.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'robinson', name: 'Robinson Playground', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, spend } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0, sway = 0;   // the view (CSS pixels, the stroke unit), the clock, how far the wind has pushed the leaves
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false, wet = false, white = 0, thin = 0, sun = 1;   // white: how much snow lies; thin: how many of the leaves are missing
const use = s => {
  pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; wet = traits.rain > 0;
  white = traits.snow ? 1 : season !== 'winter' ? 0 : s.weather === 'sunny' ? .7 : s.weather === 'frost' ? .45 : .25;
  thin = season === 'autumn' ? .3 : season === 'spring' ? .16 : .04;
  sun = night ? .5 : traits.shadow > 0 ? 1 : .4;                // without the sun there is less to tell light from shade
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph: x and y as fractions of its width and height.  It is an upright one:
// a narrow screen shows a slice of its width, a wide one a slice of its height and the clearing that lies to its
// left (see layout).
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along(pts, x) {                                       // a line through points
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * (x - a[0]) / (b[0] - a[0]); }
  return pts[pts.length - 1][1];
}
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
// Fog is between us and everything, and the more of it the farther off a thing is: the leaves that hang in front
// of the hut are close, the trees behind it are not.  Rain does the same, a little.
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;   // by night it is no lighter than the sky
const hazeOf = d => { const a = d * (traits.fog * 1.3 + traits.rain * .22); return a > .01 ? [fogCol(), clamp(a, 0, .82)] : null; };
const veil = d => { PP.haze = hazeOf(d); };

// ---------- the hut ----------
// The photograph looks up at it: its walls lean in a little towards the top, its boards lie level.  The roof is a
// band of dark boards with the ends of the shingles standing over it like a saw; one sees more of it on the left.
const HMID = .5145, BOT = .553, BH = .0146;                    // its middle, the foot of its wall, how high a board is
const wallL = pf => .3434 + (.5175 - pf) * .075, wallR = pf => .6853 - (.5175 - pf) * .075;
const RIDGE = [.52, .2632], APEX = [.518, .288], RX0 = .335, RX1 = .692, STEP = .0185;
function roofTop(xf) {
  const d = xf - RIDGE[0], sl = d < 0 ? -.49 : .58, c = (Math.floor(d / STEP) + .5) * STEP;
  return RIDGE[1] + c * sl + (d - c) * sl * .35;
}
const gableTop = xf => APEX[1] + (xf < APEX[0] ? (APEX[0] - xf) * .56 : (xf - APEX[0]) * .49);
const HOLE = [.5163, .3355, .0298, .0171];                     // the round hole: it is an oval from down here
const WX = [.4457, .5049, .5229, .5839], WY = [.428, .466], BAR = [.448, .4535];   // the window: its two halves and the post between them, the bar across
const LAMP = [.476, .4375];                                    // where the lantern hangs
const ROOF = 1, WALL = 2, ROUND = 3, OPEN = 4, RAIL = 5, POST = 6, BEAM = 7, UNDER = 8, PROP = 9;
function hutPart(xf, pf) {
  if (pf < BOT) {
    if (pf < gableTop(xf)) return xf > RX0 && xf < RX1 && pf >= roofTop(xf) ? ROOF : 0;
    if (xf < wallL(pf) || xf > wallR(pf)) return 0;
    const hx = (xf - HOLE[0]) / HOLE[2], hy = (pf - HOLE[1]) / HOLE[3];
    if (hx * hx + hy * hy < 1) return ROUND;
    if (pf > WY[0] && pf < WY[1] && xf > WX[0] && xf < WX[3]) return xf > WX[1] && xf < WX[2] ? POST : pf > BAR[0] && pf < BAR[1] ? RAIL : OPEN;
    return WALL;
  }
  if (xf < .335 || xf > .69) return 0;
  if (pf < BOT + .012) return BEAM;                            // the floor it stands on
  if (xf > .345 && xf < .68 && (pf < .603 || (xf > .39 && xf < .6))) return xf > .662 && xf < .673 ? PROP : UNDER;   // and the dark under that
  return 0;
}

// The cone it stands on: planks leaning together, every one running up to the same point inside the hut.  A
// plank is known by s, how far it goes sideways for every step down from there.  [from, to, where its top is, what
// wood it is]; the later ones lean against the others from outside, and are shorter.
const CA = [.48, .49], GREY = 0, PALE = 1, TAN = 2, BROWN = 3, DIM = 4, CAP = .012;
const PLANKS = [[-.66, -.55, .655, BROWN], [-.55, -.42, .652, TAN], [-.42, -.226, .655, TAN], [-.226, -.05, .645, PALE], [-.05, .105, .632, GREY], [.105, .395, .625, GREY], [.395, .64, .64, DIM],
  [.2, .3, .7, PALE], [-.7, -.6, .83, BROWN], [-.6, -.49, .75, GREY], [-.49, -.36, .8, DIM], [-.2, -.07, .84, GREY], [-.36, -.28, .9, PALE], [.02, .13, .79, PALE], [.3, .44, .86, DIM], [.44, .55, .76, GREY], [.55, .67, .8, DIM]];
let zc = 0, zu = 0;                                            // what conePart() learnt: which plank, and where across it (-1 .. 1)
function conePart(xf, pf) {                                    // 1 a plank, 2 the gap beside it
  const s = (xf - CA[0]) / (pf - CA[1]);
  if (s < -.7 || s > .67) return 0;
  for (let i = PLANKS.length - 1; i >= 0; i--) {
    const p = PLANKS[i];
    if (s < p[0] || s >= p[1]) continue;
    const u = (s - p[0]) / (p[1] - p[0]) * 2 - 1;
    if (pf < p[2] + CAP * (1 - Math.sqrt(1 - u * u))) continue;   // its top is round
    zc = i; zu = u;
    return u < -.86 || u > .9 || pf < p[2] + CAP * (1 - Math.sqrt(1 - u * u)) + .003 ? 2 : 1;
  }
  return 0;
}

// The pole of the flag, and the ivy: a band of it over the top of the cone, and what climbs on from there.
const POLE = [.457, .294, .4768, .153], FLAG = [.4768, .161];
const poleX = pf => POLE[0] + (POLE[2] - POLE[0]) * (POLE[1] - pf) / (POLE[1] - POLE[3]);
let poleMin = .003, leafSz = 6, LF = .1;                       // half the pole, never less than this: a stroke; how big a leaf is, and how close they sit
const poleHW = pf => Math.max(poleMin, .0026 + (pf - POLE[3]) * .012);
const CLIMB = [[.432, .47], [.565, .478], [.59, .5], [.36, .5]];   // where it goes up the wall: [x, how high]
function ivy(xf, pf, x, y) {                                   // > 0 where it grows
  const c = .603 + (vnoise(xf * 31, 2.2) - .5) * .02;
  let d = -1;
  if (pf > c - .056) { const h = (.03 + .022 * vnoise(xf * 17, 5.1)) * smooth(.31, .36, xf) * smooth(.66, .6, xf); if (h > 0) d = 1 - Math.abs(pf - c) / h; }
  if (pf < c && d < .6) for (const t of CLIMB) {
    if (pf < t[1] - .012 || Math.abs(xf - t[0]) > .014) continue;
    const e = 1 - Math.abs(xf - t[0] - Math.sin(pf * 110 + t[0] * 40) * .007) / (.0055 * smooth(t[1] - .01, t[1] + .02, pf)) - smooth(.45, .3, vnoise(pf * 120, t[0] * 9));
    if (e > d) d = e;
  }
  return d < -.5 ? -1 : d * .9 + (vnoise(x * LF, y * LF) - .5) * .7;
}

// ---------- the bushes ----------
// A heap of crowns: [x, y, how big, of what kind: a bush, the elder, ash].  Those in the back stand behind the hut and round it, those in
// front hang in front of it.  They are kept in coarse grids: how deep in the leaves a point is, and of what kind
// they are there.
const BUSH = 0, ELDER = 1, ASH = 2;
const BACK = [
  [.03, .17, .07, 0], [.12, .19, .07, 0], [.22, .2, .07, 0], [.31, .22, .06, 0], [.385, .25, .05, 0], [.43, .285, .035, 0],
  [.05, .3, .11, 0], [.18, .31, .11, 0], [.3, .34, .09, 0], [.05, .5, .12, 0], [.2, .5, .12, 0], [.31, .5, .08, 0], [.04, .68, .11, 0], [.18, .66, .11, 0], [.3, .64, .07, 0], [.1, .8, .07, 0],
  [.77, .04, .05, 1], [.71, .13, .07, 1], [.83, .1, .08, 1], [.94, .04, .07, 1], [.95, .16, .09, 1], [.79, .23, .1, 1], [.92, .3, .11, 1],
  [.72, .34, .07, 1], [.82, .42, .11, 1], [.97, .45, .08, 1], [.74, .52, .09, 1], [.9, .56, .12, 1], [.76, .67, .09, 1], [.92, .72, .11, 1], [.78, .82, .09, 1], [.93, .88, .1, 1], [.8, .96, .1, 2], [.97, 1, .08, 2],
];
const FRONT = [
  [.365, .305, .05, 0], [.335, .36, .05, 0], [.39, .355, .038, 0], [.345, .42, .05, 0], [.395, .408, .032, 0], [.385, .452, .036, 0], [.425, .443, .02, 0], [.31, .5, .04, 0],
  [.672, .328, .03, 1], [.69, .37, .05, 1], [.64, .39, .028, 1], [.613, .403, .016, 1], [.665, .445, .03, 1], [.716, .44, .035, 1], [.712, .527, .04, 1], [.66, .532, .034, 1], [.625, .548, .028, 1],
  [.36, .562, .042, 2], [.415, .556, .028, 2], [.465, .55, .026, 2], [.52, .552, .028, 2], [.575, .538, .03, 2], [.548, .515, .016, 2],
  [.3, .75, .05, 2], [.38, .765, .04, 2], [.44, .775, .028, 2], [.37, .84, .048, 2], [.42, .9, .036, 2], [.345, .97, .05, 2], [.45, 1, .05, 2], [.55, .99, .06, 2],
  [.6, .7, .04, 2], [.57, .76, .034, 2], [.635, .78, .05, 2], [.52, .8, .028, 2], [.6, .86, .05, 2], [.52, .905, .034, 2], [.66, .93, .06, 2], [.585, .945, .04, 2],
  [.66, .62, .05, 1], [.7, .7, .06, 1], [.725, .6, .05, 1], [.75, .84, .07, 1], [.83, .92, .08, 1], [.93, .96, .08, 1], [.72, .96, .06, 2], [.035, .93, .028, 2],
];
// The shoots that stand up out of them into the sky, and hang into it: [from x, y, how big, to x, y, how big, kind],
// crown by crown along them.
for (const t of [[.335, .022, .012, .275, .17, .04, 0], [.205, .055, .012, .205, .14, .034, 0], [.13, .07, .012, .13, .14, .03, 0], [.06, .085, .014, .02, .115, .028, 0], [.375, .105, .012, .36, .15, .028, 0],
  [.415, .165, .014, .445, .262, .024, 0], [.605, .045, .013, .7, .055, .036, 1], [.585, .078, .013, .655, .118, .04, 1], [.502, .13, .012, .6, .185, .046, 1], [.527, .21, .013, .63, .262, .05, 1], [.562, .283, .014, .605, .3, .03, 1]]) {
  for (let f = 0; f <= 1;) { const r = t[2] + (t[5] - t[2]) * f; BACK.push([t[0] + (t[3] - t[0]) * f, t[1] + (t[4] - t[1]) * f, r, t[6]]); f += r * .9 / Math.hypot(t[3] - t[0], (t[4] - t[1]) * 4 / 3); }
}
// Where they stand, and what stands in them without leaves of its own: [x, y].  Every crown grows from the nearest
// one that is closer to where its bush stands.
const SHRUB = [[.1, .94]], TREE = [[.75, 1.02], [.735, .85], [.72, .7], [.705, .55], [.695, .42], [.705, .3], [.73, .18]];
const BRIAR = [[.27, .7]], TWINE = [[.705, .56]], SPRAYS = [[.3, 1.03], [.47, 1.04], [.62, 1.03], [.76, 1.02]];
// Beyond the photograph lies the clearing, and that is made up: trees stand round it, [x, where the crown is, how
// big it is, where its foot is], a wood closes it off far behind them.  The elder goes on to the right.
const TREES = [[-.27, .52, .2, .9], [-.13, .75, .13, .93], [-.66, .41, .16, .83], [-1.1, .2, .34, .9], [-1.64, .32, .25, .85], [-2.06, .45, .15, .81], [-2.56, .2, .34, .9], [-3.1, .36, .22, .84], [-3.6, .22, .32, .9]];
const DENSE = [[0, .27], [.3, .33], [.45, .34], [.6, .34], [.75, .37], [1, .3]];
const farTop = xf => .49 + (vnoise(xf * 3.1, 1.7) - .5) * .12 + (vnoise(xf * 14, 4.2) - .5) * .04;
const here = xf => smooth(-.12, -.03, xf);                     // 1 on the photograph, among the bushes round the hut; 0 out in the clearing
// The bushes on the photograph's left end in a big round one just beyond it: where its top is, and how deep inside it a point is
const dome = xf => xf > -.17 ? DENSE[0][1] + (1 - Math.sqrt(1 - xf * xf / .0289)) * .45 : 9;
const within = (xf, pf) => xf >= 0 ? 1 : xf < -.17 ? 0 : smooth(-.015, .02, pf - dome(xf) - (vnoise(pf * 40, xf * 30) - .5) * .03);
function dense(xf) {                                           // below it no sky is to be seen between the trees, leaves or no leaves
  const y = xf > 1 ? -.3 : xf >= 0 ? along(DENSE, xf) : Math.min(farTop(xf), dome(xf));
  return y + (vnoise(xf * 30, 5.5) - .5) * .03 + (vnoise(xf * 110, 2.5) - .5) * .01;
}
const groundTop = xf => .895 - .14 * smooth(-.1, -.55, xf) + (vnoise(xf * 7, 3.3) - .5) * .035;
let gCell = 3, gCols = 0, gRows = 0, backQ = null, backK = null, backV = null, frontQ = null, frontK = null, frontV = null;
let limbs = [], limbSum = null, stoutN = 0, stoutLength = 0, limbLength = 0, mask = null, mCols = 0, mRows = 0, mCell = 2;
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function buildWood() {
  gCell = 2.5 * U; gCols = Math.ceil(W / gCell) + 1; gRows = Math.ceil(H / gCell) + 1;
  backQ = new Float32Array(gCols * gRows).fill(-1); frontQ = new Float32Array(gCols * gRows).fill(-1);
  backK = new Uint8Array(gCols * gRows); frontK = new Uint8Array(gCols * gRows); backV = new Int8Array(gCols * gRows); frontV = new Int8Array(gCols * gRows);
  limbs = [];
  const x0 = fx(0) - .15, x1 = fx(W) + .15;
  // what stands beyond the photograph: a tree is a heap of crowns round the middle of its own
  const far = [], stands = [];
  TREES.forEach((t, i) => {
    if (t[0] + t[2] < x0) return;
    const r = rng(1000 + i), kind = i % 3 === 2 ? ELDER : BUSH, crowns = [];
    for (let n = 6 + Math.round(t[2] * 70); n > 0; n--) {
      const a = r() * TAU, d = Math.sqrt(r()) * .78;
      crowns.push([t[0] + Math.cos(a) * d * t[2], t[1] + Math.sin(a) * d * t[2] * .62, t[2] * (.28 + r() * .22), kind]);
    }
    far.push(...crowns); stands.push([t, crowns, 30 + i]);
  });
  for (let i = 0; 1.04 + i * .06 < x1; i++) {
    const r = rng(1500 + i), xf = 1.04 + i * .06;
    for (let y = -.04; y < 1.05; y += .08 + r() * .04) far.push([xf + (r() - .5) * .05, y, .08 + r() * .04, y > .85 ? ASH : ELDER]);
  }
  const stamp = (Q, K, V, c) => {
    const cx = px(c[0]), cy = py(c[1]), r = c[2] * PW;
    const i0 = Math.max(0, Math.floor((cx - r) / gCell)), i1 = Math.min(gCols - 1, Math.ceil((cx + r) / gCell)), j0 = Math.max(0, Math.floor((cy - r) / gCell)), j1 = Math.min(gRows - 1, Math.ceil((cy + r) / gCell));
    for (let j = j0; j <= j1; j++) for (let i = i0; i <= i1; i++) {
      const q = 1 - Math.hypot((i + .5) * gCell - cx, ((j + .5) * gCell - cy) * .92) / r, n = j * gCols + i;
      if (q > Q[n]) { Q[n] = q; K[n] = c[3]; V[n] = clamp((cy - (j + .5) * gCell) / r, -1, 1) * 100; }   // and how high up in its crown
    }
  };
  const seen = c => c[0] + c[2] > x0 && c[0] - c[2] < x1;
  for (const c of BACK) if (seen(c)) stamp(backQ, backK, backV, c);
  for (const c of far) stamp(backQ, backK, backV, c);
  for (const c of FRONT) if (seen(c)) stamp(frontQ, frontK, frontV, c);
  // Their wood.  Every crown hangs on a limb that comes from the next one nearer to where the bush stands, and
  // carries twigs of its own; a limb is as stout as what hangs on it is many.
  const add = (xa, ya, xb, yb, bend, w, front, red, k) => {
    const len = Math.hypot(xb - xa, yb - ya) || 1, a = Math.atan2(yb - ya, xb - xa);
    return limbs[limbs.push({ x1: xa, y1: ya, x2: xb, y2: yb, cx: (xa + xb) / 2 - Math.sin(a) * len * bend, cy: (ya + yb) / 2 + Math.cos(a) * len * bend, w, front, red, len, k }) - 1];
  };
  const sprig = (r, x, y, a, len, w, depth, front, red) => {     // a twig, and the twigs on it
    const s = add(x, y, x + Math.cos(a) * len, y + Math.sin(a) * len, (r() - .5) * .35, w, front, red, r());
    if (depth > 1 || len < 5 * U) return;
    for (let v = r() < .4 ? 3 : 2; v > 0; v--) { const f = .4 + r() * .6; sprig(r, limbX(s, f), limbY(s, f), a + (r() - .5) * 1.8, len * (.45 + r() * .3), Math.max(.7 * U, w * .65), depth + 1, front, red); }
  };
  const grove = (crowns, stand, front, red, seed) => {
    const r = rng(seed), roots = stand.map(p => ({ x: px(p[0]), y: py(p[1]), r: 0, n: 1, d: 0 })), root = roots[0];
    const nodes = roots.slice(1).concat(crowns.map(c => ({ x: px(c[0]), y: py(c[1]), r: c[2] * PW, n: 1 })));
    for (const n of nodes) n.d = Math.hypot(n.x - root.x, n.y - root.y);
    nodes.sort((a, b) => a.d - b.d);
    for (let i = 0; i < nodes.length; i++) {                   // rather from below than from the side, rather from the trunk than from a crown
      const n = nodes[i]; let best = root, bd = n.d * 1.15;
      for (let j = 0; j < i; j++) { const m = nodes[j], d = Math.hypot(n.x - m.x, (n.y - m.y) * .6) * (m.r ? 1 : .7) * (m.y < n.y - n.r * .3 ? 1.8 : 1); if (d < bd) { bd = d; best = m; } }
      n.up = best;
    }
    for (let i = nodes.length - 1; i >= 0; i--) nodes[i].up.n += nodes[i].n;
    for (const n of nodes) {
      const w = clamp(Math.pow(n.n, .6) * (front ? .0015 : .003) * PW, .9 * U, .04 * PW);
      add(n.up.x, n.up.y, n.x, n.y, (r() - .5) * (front ? .5 : .4), w, front, red, r());
      for (let t = n.r ? 4 + (r() * 3 | 0) : 0; t > 0; t--) sprig(r, n.x, n.y, -Math.PI / 2 + (r() - .5) * 3.8, n.r * (.5 + r() * .55), Math.max(.8 * U, w * .5), 0, front, red);
    }
  };
  grove(BACK.filter(c => c[0] < .5 && seen(c)), SHRUB, false, false, 11);
  grove(BACK.filter(c => c[0] >= .5 && seen(c)), TREE, false, false, 12);
  grove(FRONT.filter(c => c[3] === BUSH && seen(c)), BRIAR, true, false, 13);
  grove(FRONT.filter(c => c[3] === ELDER && seen(c)), TWINE, true, false, 14);
  SPRAYS.forEach((p, i) => grove(FRONT.filter(c => c[3] === ASH && Math.abs(c[0] - p[0]) < .09 && c[0] > .2 && seen(c)), [p], true, i > 1, 15 + i));
  for (const [t, crowns, seed] of stands) grove(crowns, [[t[0], t[3] + .03], [t[0] + .012, (t[1] + t[3]) / 2], [t[0] - .01, t[1] + t[2] * .3]], false, false, seed);
  // where the wood is: 1 behind the hut, 2 in front of it; 4 and 8 the same for what is too stout for leaves to hide
  limbs.sort((a, b) => b.w - a.w);
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows);
  limbSum = new Float32Array(limbs.length); limbLength = stoutLength = 0; stoutN = 0;
  let sum = 0;
  limbs.forEach((s, i) => {
    const stout = s.w > (s.front ? .003 : .007) * PW, bits = (s.front ? 2 : 1) | (stout ? s.front ? 8 : 4 : 0), rad = s.w / 2 + .3 * U, m = Math.ceil(rad / mCell);
    sum += s.len * (s.w + 2 * U); limbSum[i] = sum; limbLength += s.len;
    if (stout) { stoutN = i + 1; stoutLength += s.len; }
    for (let k = 0, n = Math.ceil(s.len / (mCell * .5)); k <= n; k++) {
      const ix = Math.floor(limbX(s, k / n) / mCell), iy = Math.floor(limbY(s, k / n) / mCell);
      for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
        const cx = ix + dx, cy = iy + dy;
        if (cx >= 0 && cy >= 0 && cx < mCols && cy < mRows && Math.hypot(dx, dy) * mCell <= rad + mCell * .5) mask[cy * mCols + cx] |= bits;
      }
    }
  });
}
let zf = 0, zk = 0, zv = 0;                                    // what fol() learnt: how deep in the leaves, of what kind they are, how high up in their crown
function fol(Q, K, V, x, y) {                                  // > 0 inside the leaves
  const i = ((x - sway) / gCell) | 0, j = (y / gCell) | 0;
  if (i < 0 || j < 0 || i >= gCols || j >= gRows) return -1;
  const n = j * gCols + i, q = Q[n];
  if (q <= -1) return -1;
  zk = K[n]; zv = V[n] / 100;
  const lace = (vnoise(x * LF, y * LF) - .5) * (Q === frontQ ? 1.1 : .7) + (vnoise(x * .03 / U + 7, y * .03 / U) - .5) * .6;   // leaf by leaf at its rim
  return zf = (zk === ASH ? q * 1.05 - .08 : q * 1.3) - thin + lace;                                // the ash is all sprays, light shows through it everywhere
}
function leafLum(x, y) {                                       // light and shade lie on them in patches
  const k = .13 / leafSz, a = x * k + time * .02, b = y * k;
  return .5 + (vnoise(a, b) * .65 + vnoise(a * 2.3, b * 2.3) * .35 - .5) * 2;
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes may
// start there.
const SKY = 0, LEAF = 1, NEAR = 2, IVY = 3, HUT = 4, CONE = 5, STAFF = 6, LIMB = 7, STEM = 8, DUSK = 9, THICKET = 10, GROUND = 11;
let zx = 0, zy = 0, zp = 0;                                    // what zone() learnt on the way: the point on the photo, the part of the hut or the cone
function zone(x, y) {
  const xf = zx = fx(x), pf = zy = fy(y);
  if (!bare && fol(frontQ, frontK, frontV, x, y) > 0) return NEAR;
  const m = maskAt(x, y);
  if (m & 2 && (bare || m & 8)) return STEM;
  if (pf > .46 && pf < .66 && xf > .3 && xf < .67 && ivy(xf, pf, x, y) > 0) return IVY;
  if (pf > .6 && (zp = conePart(xf, pf))) return CONE;
  if (xf > .33 && xf < .695 && pf > .258 && pf < .665 && (zp = hutPart(xf, pf))) return HUT;
  if (pf > POLE[3] && pf < POLE[1] && Math.abs(xf - poleX(pf)) < poleHW(pf)) return STAFF;
  if (pf > groundTop(xf)) return GROUND;
  if (!bare && fol(backQ, backK, backV, x, y) > 0) return LEAF;
  if (m & 1 && (bare || m & 4)) return LIMB;
  const d = pf - dense(xf);
  if (d > 0) return bare ? THICKET : DUSK;
  if (bare && d > -Math.max(0, vnoise(x * .22 / U, y * .035 / U) - .4) * .1) return THICKET;   // twigs stand up out of the wood
  return SKY;
}
const reach = [0, 0];
function run(x, y, ca, sa, l, ok, step) {                      // how far a stroke may go either way along (ca, sa) while ok() says so
  reach[0] = reach[1] = l;
  for (let d = step; d < l + step; d += step) if (!ok(zone(x + ca * d, y + sa * d))) { reach[1] = Math.min(l, d - step * .5); break; }
  for (let d = step; d < l + step; d += step) if (!ok(zone(x - ca * d, y - sa * d))) { reach[0] = Math.min(l, d - step * .5); break; }
}
function room(x, y, nx, ny, w, ok) {                           // how broad a stroke may be here: no broader than to the next thing of another kind
  const h = w / 2;
  return ok(zone(x + nx * h, y + ny * h)) && ok(zone(x - nx * h, y - ny * h)) ? w : ok(zone(x + nx * h * .5, y + ny * h * .5)) && ok(zone(x - nx * h * .5, y - ny * h * .5)) ? w * .5 : Math.min(w, 1.2 * U);
}
function skyTop(xf) {                                          // where the sky ends at x: at the wood, or at the roof
  const y = dense(xf);
  return xf > RX0 && xf < RX1 ? Math.min(y, roofTop(xf)) : y;
}

// ---------- the hut, board by board ----------
const glowAt = (xf, pf) => Math.exp(-((xf - LAMP[0]) * (xf - LAMP[0]) + (pf - LAMP[1]) * (pf - LAMP[1]) * 1.78) / .0016);   // how much of the lantern's light gets there
const isHut = m => z => z === HUT && zp === m;
function hutStroke(x, y) {
  const R = PP.R, m = zp, xf = zx, pf = zy, q = R(), o = { a: .94, jit: 5, dry: .05, bow: 0, sp: 1.1 * U }, same = isHut(m), u = HU;
  veil(.32);
  if (m === WALL) {                                            // every stroke lies in a board
    const i = Math.floor((BOT - pf) / BH), bw = Math.max(1.4 * u, BH * PH * .82), sd = shadow(x, y);
    let yc = py(BOT - (i + .5) * BH), wd = bw;
    if (!same(zone(x, yc)) || !same(zone(x, yc - bw * .45)) || !same(zone(x, yc + bw * .45))) { yc = y; wd = room(x, y, 0, 1, bw * .5, same); }   // by the hole, the window, the gable: what there is of it
    if (q < .13 && wd === bw) {                                // the shade in the joint under it
      run(x, yc + bw * .58, 1, 0, (5 + R() * 10) * u, same, 2 * u);
      PP.brush(x - reach[0], yc + bw * .58, x + reach[1], yc + bw * .58, Math.max(.8 * u, bw * .16), mix(pal.wood[0], pal.ink, .55), { a: .75, jit: 4, dry: .1, bow: 0, sp: .8 * U });
      return;
    }
    const h = hash1(i * 131 + 7), g = vnoise(xf * 150 + i * 9.7, i * 3.1 + pf * 40);
    let col = ramp(pal.wood, .3 + h * .3 + (g - .5) * .55 + (R() - .5) * .14);
    if (i === 5) col = mix(col, pal.woodPale, .6);              // the broad one under the window
    col = mix(col, pal.wood[0], smooth(.035, 0, pf - gableTop(xf)) * .45 + smooth(.45, .53, pf) * smooth(.6, .68, xf) * .28 + smooth(.5, .55, pf) * .15);   // darker under the eaves, and where the rain runs down
    if (wet) col = mix(col, pal.wood[0], .4);
    if (white) col = mix(col, pal.snow, white * .1);
    if (sd) col = mix(col, pal.wood[0], sd * .25);
    if (q > .985 && wd === bw) { PP.dab(x, yc, .28 * bw, mix(pal.wood[0], pal.ink, .3), { a: .8, den: 1.6, core: .7, s: U, jit: 4 }); return; }   // a knot
    run(x, yc, 1, 0, (5 + R() * 12) * u, same, 2 * u);
    let xa = x - reach[0], xb = x + reach[1];                  // a brush ends ragged: at the corner of the hut it is drawn past the edge and cut off there
    if (fx(xa) - .005 < wallL(pf)) xa -= 5 * u;
    if (fx(xb) + .005 > wallR(pf)) xb += 5 * u;
    o.sp = Math.min(1.1 * U, wd);
    PP.brush(xa, yc, xb, yc, wd, col, o);
    return;
  }
  if (m === ROOF) {
    const top = roofTop(xf), t = (pf - top) / (gableTop(xf) - top), sl = (xf < RIDGE[0] ? -.49 : .58) * PH / PW, k = 1 / Math.hypot(1, sl);
    if (t < .3 || q < .12) {                                   // the ends of the shingles, along the slope; snow lies on them
      run(x, y, k, sl * k, (3 + R() * 5) * u, same, 1.5 * u);
      PP.brush(x - k * reach[0], y - sl * k * reach[0], x + k * reach[1], y + sl * k * reach[1], 1.7 * u, t < .34 && white && R() < white * .92 ? pal.snow : mix(pal.shingle[0], pal.shingle[1], R() * .8), o);
      return;
    }
    const b = Math.floor(xf / .0125), cx = clamp(px((b + .5) * .0125), x - 2 * u, x + 2 * u);   // the boards under them, upright
    run(cx, y, 0, 1, (2.5 + R() * 4) * u, same, 1.5 * u);
    PP.brush(cx, y - reach[0], cx, y + reach[1], Math.min(2.4 * u, .011 * PW), mix(pal.shingle[1], pal.shingle[2], hash1(b * 7 + 1) * .9 * smooth(.2, .8, t)), o);
    return;
  }
  if (m === ROUND || m === OPEN || m === UNDER) {              // what is dark: the hole, the window, the room under the floor
    let col = mix(pal.hole, pal.wood[0], m === UNDER ? .12 + R() * .2 : R() * .12);
    if (m === OPEN) col = mix(col, pal.wood[1], smooth(.5, .1, (xf - (xf < WX[1] ? WX[0] : WX[2])) / .06) * .22);   // a little of the wall inside
    if (night && m !== UNDER) {                                // the lantern: its light on the boards inside
      const g = m === ROUND ? .1 + .2 * smooth(.348, .322, pf) : .34 + .66 * glowAt(xf, pf);
      col = mix(col, mix(pal.lights[3], pal.lights[2], g * g), g * .88);
    }
    run(x, y, 1, 0, (3 + R() * 5) * u, same, 1.5 * u);
    PP.brush(x - reach[0], y, x + reach[1], y, room(x, y, 0, 1, 3 * u, same), col, o);
    return;
  }
  if (m === RAIL || m === POST) {                              // the bar across the window, the post in its middle: pale wood, black against the lantern
    let col = m === POST ? mix(pal.woodPale, pal.wood[2], vnoise(xf * 400, pf * 60) * .5) : mix(pal.wood[1], pal.wood[2], R() * .6);
    if (night) col = mix(col, pal.ink, .55);
    else if (m === POST && pf < .4376) col = mix(col, pal.wood[0], .5);
    run(x, y, 1, 0, (2 + R() * 5) * u, same, 1.5 * u);
    PP.brush(x - reach[0], y, x + reach[1], y, room(x, y, 0, 1, 1.8 * u, same), col, o);
    return;
  }
  if (m === BEAM) {                                            // the floor: a grey beam, snow on its edge
    run(x, y, 1, 0, (4 + R() * 9) * u, same, 2 * u);
    PP.brush(x - reach[0], y, x + reach[1], y, 1.8 * u, white && pf < BOT + .004 && R() < white ? pal.snow : mix(pal.beam, pal.grey[1], R() * .5), o);
    return;
  }
  run(x, y, 0, 1, (3 + R() * 5) * u, same, 1.5 * u);           // a prop under it
  PP.brush(x, y - reach[0], x, y + reach[1], 1.8 * u, mix(pal.wood[0], pal.wood[1], R() * .6), o);
}
function coneStroke(x, y) {                                    // a plank: old wood, grey from the weather, its grain running down it
  const R = PP.R, part = zp, k = zc, u = zu, xf = zx, pf = zy, p = PLANKS[k], ax = px(CA[0]), ay = py(CA[1]), d = Math.hypot(x - ax, y - ay), ca = (x - ax) / d, sa = (y - ay) / d;
  const same = z => z === CONE && zc === k && zp === part;
  veil(.24);
  if (part === 2) {                                            // the dark between two of them, the shade under its top
    run(x, y, ca, sa, (3 + R() * 7) * U, same, 1.5 * U);
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], 1.3 * U, white && u * u < .7 && R() < white * .8 ? pal.snow : mix(pal.hole, pal.grey[0], .25 + R() * .3), { a: .9, jit: 4, dry: .05, bow: 0, sp: U });
    return;
  }
  const s = (xf - CA[0]) / (pf - CA[1]), g = vnoise(s * 230 + k * 7.3, pf * 7 + k), kind = p[3];
  let t = .45 + (g - .5) * .9 + (R() - .5) * .22 - u * u * .16 - smooth(.15, .62, s) * .22 - (kind === DIM ? .2 : 0);
  let col = kind === TAN ? ramp(pal.tan, t + .1) : kind === BROWN ? mix(ramp(pal.grey, t), pal.wood[1], .5) : ramp(pal.grey, kind === PALE ? t + .15 : t);
  col = mix(col, pal.moss, smooth(.5, .8, fbm(s * 9 + 3, pf * 6)) * smooth(.7, 1, pf) * .35);   // green where it is damp
  if (wet) col = mix(col, pal.grey[0], .4);
  const sd = shadow(x, y); if (sd) col = mix(col, pal.grey[0], sd * .25);
  if (white && R() < white * (.1 + .25 * smooth(.6, .9, g))) col = pal.snow;   // snow in its furrows
  run(x, y, ca, sa, (6 + R() * 14) * U, same, 2 * U);
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], room(x, y, -sa, ca, (1.5 + R() * 2.2) * U, same), col, { a: .92, jit: 6, dry: .1, bow: 0, sp: 1.1 * U });
}
function staffStroke() {                                       // the pole of the flag: too thin to be met by chance
  const R = PP.R, t = R(), pf = POLE[1] + (POLE[3] - POLE[1]) * t, xf = poleX(pf), x = px(xf), y = py(pf), l = .008 * PH;
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y) || zone(x, y) !== STAFF) return;
  const k = (POLE[2] - POLE[0]) / (POLE[3] - POLE[1]) * PW / PH, cap = pf < POLE[3] + .012;   // its knob is ringed
  veil(.4);
  const wd = Math.max(1.3 * U, poleHW(pf) * 2 * PW * (cap ? 1.3 : .9));
  PP.brush(x - k * l, y - l, x + k * l, Math.min(y + l, py(POLE[1])), wd, cap && Math.floor(pf / .003) & 1 ? pal.woodPale : white && R() < white * .3 ? pal.snow : mix(pal.grey[1], pal.grey[2], R()), { a: .94, jit: 4, dry: 0, bow: 0, sp: Math.min(1.1 * U, wd) });
}

// ---------- leaves, ivy, wood ----------
const dark = () => mix(pal.folDeep, pal.folDark, .25);
function green(lum) { return lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4); }
function leafStroke(x, y, k) {
  const R = PP.R, kind = zk, d = zf, up = zv, xf = zx, pf = zy, q = R(), near = k === NEAR, off = 1 - here(xf), sz = leafSz * (near ? 1.15 : 1 - off * .25);
  const same = z => z === k;
  // light and shade in patches, the top of every crown in the light, the wood darker the further down one looks into it
  let lum = clamp(.5 + ((leafLum(x, y) - .5) * .8 + (R() - .5) * .5 + up * (kind === ASH ? 0 : .14 + off * .14)) * (.6 + .5 * sun) + (kind === ELDER ? .1 : kind === ASH ? .08 : 0) - (near ? 0 : .06) - smooth(.55, .9, pf) * (near ? .06 : .22) * (1 - off));
  veil(near ? .1 : .5 + off * .15);
  if (white && lum > .45 && q < white * .35) { PP.dab(x, y, (.5 + R() * .5) * sz, pal.snow, { a: .92, den: .9, core: .6, s: 1.5 * U, sx: 1.6, sy: .6, jit: 3 }); return; }
  if (lum < .17 && d > .12) {                                  // the dark between the leaves
    const a = R() * Math.PI, l = (.4 + R() * .5) * sz;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (.7 + R() * .5) * sz, dark(), { a: .85, sp: 1.5 * U, dry: .1, jit: 5, bow: .2 });
    return;
  }
  if (kind === ELDER && d > .1 && lum > .3) {                  // the elder: plates of cream in May, black berries on red stalks from August
    const n = vnoise(x * .45 / leafSz + 3, y * .45 / leafSz);
    if (season === 'spring' && n > .72 && up > -.2) { PP.dab(x, y, (.7 + R() * .5) * sz, pal.bloom, { a: .9, den: 1.1, core: .5, s: 1.3 * U, sx: 1.7, sy: .7, jit: 5 }); return; }
    if (n > .8 && q < .5 && (season === 'summer' || season === 'autumn')) {
      if (q < .15) PP.brush(x, y - sz * .4, x + (R() - .5) * sz * .6, y + sz * .2, .8 * U, pal.stem, { a: .8, jit: 5, dry: .1, sp: .8 * U, bow: .2 });
      else for (let i = 0; i < 3; i++) PP.dab(x + (R() - .5) * sz * .6, y + (R() - .2) * sz * .45, Math.max(.8 * U, .1 * sz), pal.berry, { a: .95, den: 2, core: .9, s: .7 * U, jit: 3 });
      return;
    }
  }
  if (kind === BUSH && near && season !== 'spring' && vnoise(x * .3 / leafSz + 9, y * .3 / leafSz) > .76 && q < .6) {   // old man's beard
    PP.dab(x, y, (.5 + R() * .4) * sz, mix(pal.fluff, pal.folMid, .15 + R() * .2), { a: .7, den: .7, core: .25, s: 1.3 * U, jit: 5 });
    return;
  }
  let col = kind === ASH ? ramp(pal.ash, lum) : green(lum);
  if (kind !== ASH) { if (lum > .62 && q < .28) col = mix(col, pal.folSun, .7); else if (pal.accent && lum > .3 && q > (kind === ELDER ? .95 : .9)) col = pick(pal.accent, R()); }
  if (kind === ASH && d > .12) {                               // the ash: a leaf is a row of leaflets
    const a = Math.PI / 2 + (R() - .5) * 2.6, ca = Math.cos(a), sa = Math.sin(a), n = q < .7 ? 2 : 3, st = .44 * sz, l = (.42 + R() * .22) * sz;
    const o = { a: .9, sp: 1.2 * U, dry: .12, jit: 8, bow: .2 };
    if (!same(zone(x + ca * st * n * .5, y + sa * st * n * .5)) || !same(zone(x - ca * st * n * .5, y - sa * st * n * .5))) return;
    for (let j = 0; j < n; j++) {
      const cx = x + ca * st * (j - n / 2 + .5), cy = y + sa * st * (j - n / 2 + .5);
      for (const sd of [-1, 1]) { const b = a + sd * (1 + R() * .3), cb = Math.cos(b) * l, sb = Math.sin(b) * l; PP.brush(cx + cb * .15, cy + sb * .15, cx + cb, cy + sb, (.22 + R() * .1) * sz, j & 1 ? col : mix(col, pal.ash[1], .25), o); }
    }
    PP.brush(x + ca * st * (n / 2 - .3), y + sa * st * (n / 2 - .3), x + ca * st * (n / 2 + .7), y + sa * st * (n / 2 + .7), (.22 + R() * .1) * sz, col, o);
    return;
  }
  const a = R() * Math.PI, ca = Math.cos(a), sa = Math.sin(a), l = (.45 + R() * .5) * sz;   // a leaf: a short broad stroke, any way it likes
  let wd = (.5 + R() * .4) * sz;
  if (d > .45) reach[0] = reach[1] = l;                        // deep among the leaves there is nothing it could get in the way of
  else { run(x, y, ca, sa, l, same, Math.max(1.5, .3 * sz)); const r0 = reach[0], r1 = reach[1]; wd = room(x, y, -sa, ca, wd, same); reach[0] = r0; reach[1] = r1; }
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], wd, col, { a: .9, sp: 1.4 * U, dry: .12, jit: 9, bow: .2 });
}
function ivyStroke(x, y) {                                     // ivy: dark leaves, each with a shine on it; it keeps them all winter
  const R = PP.R, lum = clamp(.3 + (fbm(zx * 60 + 3, zy * 80) - .5) * 1.3 + (R() - .5) * .4), r = (.28 + R() * .26) * leafSz;
  veil(.2);
  if (white && R() < white * (.2 + lum * .5)) { PP.dab(x, y, r, pal.snow, { a: .92, den: .9, core: .6, s: 1.4 * U, sx: 1.6, sy: .6, jit: 3 }); return; }
  PP.dab(x, y, r, ramp(pal.ivy, lum), { a: .9, den: .9, core: .75, s: 1.4 * U, sx: 1.25, sy: .8, jit: 7 });
  if (lum > .45 && R() < .3) PP.dab(x - r * .25, y - r * .3, r * .4, pal.ivy[4], { a: .7, den: .8, core: .5, s: U, jit: 5 });
}
function limbStroke(stout) {                                   // a stretch of bark
  const R = PP.R, n = stout ? stoutN : limbs.length;
  if (!n) return;
  const k = R() * limbSum[n - 1];
  let a = 0, b = n - 1;
  while (a < b) { const m = (a + b) >> 1; if (limbSum[m] < k) a = m + 1; else b = m; }
  const s = limbs[a], span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span, mx = limbX(s, tm), my = limbY(s, tm);
  if (mx < 0 || my < 0 || mx >= W || my >= H || !at(mx, my) || zone(mx, my) !== (s.front ? STEM : LIMB)) return;   // not where leaves or the hut hide it
  const bark = s.red ? mix(pal.stem, pal.trunk, s.k * .5) : mix(pal.trunk, pal.grey[2], s.w > 3 * U ? .12 + s.k * .2 : .05), q = R();
  veil(s.front ? .1 : .5);
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (let i = s.w < 2 * U ? 1 : 0; i < 2; i++) {               // a twig in one stroke, a limb in two: it bends
    const u = i ? s.w < 2 * U ? t0 : tm : t0, v = i ? t1 : tm, ax = limbX(s, u), ay = limbY(s, u), bx = limbX(s, v), by = limbY(s, v);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (white) { if (s.w > 1.3 * U && q < white * .75 && Math.abs(by - ay) < Math.abs(bx - ax) * 1.6) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snow, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax - s.w * .22, ay, bx - s.w * .22, by, s.w * .28, mix(bark, pal.grey[3], .4), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });   // the light on its side
  }
}
const stoutStroke = () => limbStroke(true);
function thicketStroke(x, y) {                                 // the bare wood: twigs, what light comes through them, an old man's beard still hanging
  const R = PP.R, depth = clamp((zy - dense(zx)) / .4), q = R(), off = 1 - within(zx, zy), sz = (1 - off * .45) * U;
  veil(.6 + off * .25);
  if (white && q < white * .2) { PP.dab(x, y, (2 + R() * 3.5) * sz, pal.snow, { a: .9, den: .6, core: .5, sx: 1.7, sy: .5 }); return; }
  if (q < .42) { PP.dab(x, y, (3 + R() * 5) * sz, mix(mix(pal.skyLow, pal.haze, .5), pal.twigs[off < .5 && fbm(zx * 14, zy * 9) > .5 ? 0 : 1], (.3 + depth * .5) * (1 - off * .45)), { a: .8, den: .5, core: .4, s: 2.2 * U }); return; }
  if (q > .985 && zx > .2 && zx < .45 && zy < .55) { PP.dab(x, y, .5 * leafSz, pal.fluff, { a: .7, den: .7, core: .25, s: 1.3 * U, jit: 5 }); return; }
  const a = -Math.PI / 2 + (R() - .5) * (q < .72 ? .5 : 2.2), l = (6 + R() * 14) * sz, ca = Math.cos(a) * l, sa = Math.sin(a) * l;
  PP.line([[x, y], [x + ca * .5 + (R() - .5) * 3 * U, y + sa * .5], [x + ca, y + sa]], mix(ramp(pal.twigs, R() * .75 + (1 - depth) * .2), pal.haze, off * .35), (.8 + R() * 1.2) * sz, { pass: 1, a: .8 });
}
function duskStroke(x, y) {                                    // under the bushes it is dark, a leaf here and there catches some light; the wood beyond the clearing is far off and pale
  const R = PP.R, off = 1 - within(zx, zy), a = (R() - .5) * 1.4, l = (4 + R() * 8) * U * (1 - off * .35);
  const t = clamp((zy - dense(zx)) / .22), lum = clamp(.62 - t * .45 + (fbm(zx * 26, zy * 30) - .5) * 1.1 + (R() - .5) * .25);
  let col = R() < .12 ? mix(pal.folDark, pal.folMid, R() * .5) : mix(dark(), pal.soil[0], R() * .5);
  if (off > 0) col = mix(col, pal.accent && R() > .93 ? mix(pick(pal.accent, R()), pal.haze, .4) : mix(green(lum), pal.haze, .42), off);
  veil(.6 + off * .25);
  if (t < .2) run(x, y, Math.cos(a), Math.sin(a), l, z => z === DUSK, 2 * U); else reach[0] = reach[1] = l;
  PP.brush(x - Math.cos(a) * reach[0], y - Math.sin(a) * reach[0], x + Math.cos(a) * reach[1], y + Math.sin(a) * reach[1], Math.min((3 + R() * 5) * U * (1 - off * .25), Math.max(1.2 * U, t * .22 * PH * 1.5)), col, { a: .87, sp: 1.4 * U, dry: .2, jit: 7 });   // and no broader than it is far to the sky
}

// ---------- the ground ----------
function groundStroke(x, y) {                                  // bare earth, trodden hard, grass where nobody walks: what the trees drop lies on it
  const R = PP.R, xf = zx, pf = zy, top = groundTop(xf), depth = clamp((pf - top) / .2), sz = (.8 + depth * .9) * U, q = R(), same = z => z === GROUND;
  veil(.45 - depth * .35);
  if (white && (white > .95 ? q < .93 : fbm(xf * 6 + 1, pf * 24 + 5) + (q - .5) * .12 < .22 + white * .5)) { PP.dab(x, y, (2 + R() * 3) * sz, mix(pal.snow, pal.haze, smooth(.4, .7, fbm(xf * 8, pf * 30)) * .3), { a: .92, den: .5, core: .7, s: 1.8 * U, sx: 1.8, sy: .55, jit: 3 }); return; }
  if (season === 'autumn' && q < .3) {                         // leaves
    const a = R() * Math.PI, l = (1.5 + R() * 2) * sz;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l * .6, x + Math.cos(a) * l, y + Math.sin(a) * l * .6, (1.2 + R() * 1.2) * sz, pick(pal.bits, R()), { a: .92, jit: 7, dry: .05, sp: 1.1 * U, bow: .2 });
    return;
  }
  const sd = shadow(x, y), turf = fbm(xf * 5 + 9, pf * 22) + (1 - here(xf)) * .12 - depth * .1;
  if (turf > .58 || q > .95) {                                 // grass: every blade upright, the sod under them
    const a = q < .45 ? 0 : -Math.PI / 2 + (R() - .5) * .9 + wind(time) * .08, l = (2 + R() * 3) * sz * (q < .45 ? 1.6 : 1);
    let col = bare ? mix(pal.soil[2], pal.folMid, .3) : mix(pal.folDark, R() < .5 ? pal.folMid : pal.folLight, q < .45 ? .3 : R() * .8);
    if (sd) col = mix(col, pal.folDeep, sd * .4);
    PP.brush(x - (q < .45 ? l : 0), y, x + Math.cos(a) * l, y + Math.sin(a) * l, (q < .45 ? 2.4 : 1 + R()) * sz, col, { a: .87, jit: 8, dry: .15, sp: 1.1 * U, bow: .2 });
    return;
  }
  let col = ramp(pal.soil, .25 + fbm(xf * 9 + 4, pf * 40) * .9 + (R() - .5) * .25 + (1 - here(xf)) * .25 - smooth(.03, 0, pf - top) * .3);   // out in the clearing the light falls on it
  if (wet) col = mix(mix(col, pal.soil[0], .3), pal.skyLow, .12 + R() * .12);
  if (sd) col = mix(col, pal.soil[0], sd * .4);
  run(x, y, 1, 0, (3 + R() * 8) * sz, same, 2 * U);
  PP.brush(x - reach[0], y, x + reach[1], y + (R() - .5) * U, (1.4 + R() * 2.2) * sz, col, { a: .88, jit: 7, dry: .14, sp: 1.1 * U, bow: .04 });
}

// ---------- every stroke where it belongs ----------
function paintAt(x, y, k) {
  if (k === LEAF || k === NEAR) leafStroke(x, y, k);
  else if (k === CONE) coneStroke(x, y); else if (k === IVY) ivyStroke(x, y);   // the hut has strokes of its own
  else if (k === THICKET) thicketStroke(x, y); else if (k === DUSK) duskStroke(x, y); else if (k === GROUND) groundStroke(x, y);
}
function landStroke(y0, y1) {                                  // somewhere between two heights
  const R = PP.R, x = R() * W, y = y0 + R() * (y1 - y0);
  if (at(x, y)) paintAt(x, y, zone(x, y));
}
const BANDS = [() => landStroke(0, H / 3), () => landStroke(H / 3, H * 2 / 3), () => landStroke(H * 2 / 3, H)];
let bx0 = 0, bx1 = 0, by0 = 0, by1 = 0, HU = 1;                // the box the hut stands in; the unit its strokes are measured by: no smaller than the boards are
function builtStroke() {                                       // it is made of small things: it gets strokes of its own
  const R = PP.R, x = bx0 + R() * (bx1 - bx0), y = by0 + R() * (by1 - by0);
  if (at(x, y) && zone(x, y) === HUT) hutStroke(x, y);
}

// ---------- lights, weather ----------
function lampDab() {                                           // the lantern in the window, and the air round it
  const R = PP.R, x = px(LAMP[0]), y = py(LAMP[1]);
  if (x < 0 || y < 0 || x >= W || y >= H || !at(x, y) || !night || zone(x, y) !== HUT) return;
  PP.haze = null;                                              // a light is seen through any fog, and all the wider for it
  if (R() < .4) PP.dab(x, y, (.03 + traits.fog * .02) * PW, pal.lights[0], { a: .07, den: .35, s: 2.4 * U, jit: 3 });
  PP.dab(x, y, Math.max(1.2 * U, .0045 * PW), pal.lights[2], { a: 1, den: 3, core: 1, s: 1.2 * U, jit: 4 });
}
function fogStroke() {                                         // fog hangs between the trees, in banks that drift
  const R = PP.R, x = R() * W, depth = R(), y = py(.3 + depth * .5);
  if (y < 0 || y >= H || !at(x, y) || !traits.fog) return;
  const z = zone(x, y);
  if (z === SKY || z === HUT || z === CONE || z === NEAR || z === IVY || z === STEM || z === STAFF || fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.haze = null;
  PP.dab(x, y, (8 + R() * 16) * U, fogCol(), { a: .4, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y = R() * H;
  if (!at(x, y) || !traits.glint) return;
  const z = zone(x, y);
  if (z !== GROUND && !(z === HUT && zp === ROOF && zy - roofTop(zx) < .008)) return;
  PP.haze = null;
  PP.dab(x, y, (.6 + R() * .8) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
let OUT = [];                                                  // all round the hut and the cone
function hutPath(c) { c.moveTo(OUT[0][0], OUT[0][1]); for (let i = 1; i < OUT.length; i++) c.lineTo(OUT[i][0], OUT[i][1]); c.closePath(); }
function outline() {
  const o = [[.335, BOT + .012], [.335, BOT], [wallL(BOT), BOT], [wallL(.38), .38], [RX0, gableTop(RX0)], [RX0, roofTop(RX0) - .004], [RIDGE[0], RIDGE[1] - .004], [RX1, roofTop(RX1) - .004], [RX1, gableTop(RX1)],
    [wallR(.364), .364], [wallR(BOT), BOT], [.69, BOT], [.69, BOT + .012], [.68, BOT + .012], [.68, .62]], foot = fy(H) + .1;
  o.push([CA[0] + .67 * (.72 - CA[1]), .72], [CA[0] + .67 * (foot - CA[1]), foot], [CA[0] - .7 * (foot - CA[1]), foot], [CA[0] - .7 * (.72 - CA[1]), .72], [.345, .64], [.345, BOT + .012]);
  return o.map(p => [px(p[0]), py(p[1])]);
}
let lagCells = null, lagAt = 0;                                // the cells of the coat that is being brushed, how many of them have had their second one
const LAND = 0, BUILT = 1, ALOFT = 2, HUNG = 3;                 // whose turn it is: the wood and the ground, the hut and the cone, the leaves in front of the sky, those in front of the hut
function coatStroke(x0, y0, what) {                            // a fresh coat: broad strokes of what is there, where the brush comes by.  Nothing of the wash under it is to show through
  const R = PP.R;
  for (let n = 0; n < 9; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .5) * 21 * U, y = y0 + ((n / 3 | 0) - 1 + (R() - .5) * .5) * 13 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U || y < -12 * U) continue;
    const k = zone(clamp(x, 0, W - 1), clamp(y, 0, H - 1)), xf = zx, pf = zy, l = (22 + R() * 10) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
    if (what === BUILT) {                                      // the hut and the cone: their wood, the boards come after
      if (k === HUT) {
        const m = zp;
        veil(.32);
        PP.brush(x - l * .5, y, x + l * .5, y, 14 * U, m === ROOF ? pal.shingle[1] : m === WALL || m === POST || m === RAIL ? mix(pal.wood[1], pal.wood[2], .4) : m === BEAM ? pal.beam : pal.hole, o);
      } else if (k === CONE) {
        const ax = px(CA[0]), ay = py(CA[1]), d = Math.hypot(x - ax, y - ay), kind = PLANKS[zc][3];
        veil(.24);
        PP.brush(x - (x - ax) / d * l * .6, y - (y - ay) / d * l * .6, x + (x - ax) / d * l * .6, y + (y - ay) / d * l * .6, 13 * U, kind === TAN ? pal.tan[1] : kind === DIM ? pal.grey[0] : pal.grey[1], o);
      }
      continue;
    }
    if (what === ALOFT && ((k !== LEAF && k !== NEAR) || pf > dense(xf) + .03)) continue;
    if (what === HUNG && k !== NEAR && k !== IVY) continue;
    if (k === LEAF || k === NEAR) {
      const a = (R() - .5) * 1.2, same = z => z === k, wd = room(x, y, -Math.sin(a), Math.cos(a), 24 * U, same), ll = wd < 24 * U ? l * .4 : l * .9;
      const lum = clamp(.3 + zf * .25 + (zk === ELDER ? .12 : 0) - smooth(.55, .9, pf) * .2);
      veil(k === NEAR ? .1 : .5);
      PP.brush(x - Math.cos(a) * ll, y - Math.sin(a) * ll, x + Math.cos(a) * ll, y + Math.sin(a) * ll, wd, zk === ASH ? ramp(pal.ash, lum) : green(lum), o);
    } else if (k === IVY) { veil(.2); PP.brush(x - l * .4, y, x + l * .4, y, 9 * U, pal.ivy[1], o); }
    else if (k === DUSK) { const off = 1 - within(xf, pf); veil(.6 + off * .25); PP.brush(x - l, y, x + l, y, 18 * U, mix(mix(dark(), pal.soil[0], .3), mix(green(.42), pal.haze, .42), off), o); }
    else if (k === THICKET) { const off = 1 - within(xf, pf); veil(.6 + off * .25); PP.brush(x - l, y, x + l, y, 18 * U, mix(mix(pal.skyLow, pal.haze, .5), pal.twigs[1], .55 - off * .2), o); }
    else if (k === GROUND) { veil(.2); PP.brush(x - l, y, x + l, y, 16 * U, white ? mix(pal.soil[1], pal.snow, .25 + white * .65) : pal.soil[1], o); }
  }
}

// ---------- what moves ----------
// The flag hangs from the top of its pole, all the colours of the rainbow down its length.  It is never still for
// long: it lifts with every breath of wind, stands out in a storm and hangs heavy in the rain.
function drawFlag(P, t) {
  const x0 = px(FLAG[0]), y0 = py(FLAG[1]);
  if (x0 < -60 * U || x0 > W + 60 * U || y0 < -40 * U || y0 > H || !at(clamp(x0, 0, W - 1), clamp(y0, 0, H - 1))) return;
  const g = wind(t), n = 12, L = .112 * PH / n, cols = pal.flag;
  const blow = traits.gust ? 1.15 + .12 * g : traits.rain || traits.snow || traits.fog ? .06 + .05 * Math.max(0, g) : clamp(.5 + .42 * g, .08, 1.05);
  P.haze = hazeOf(.4);
  let x = x0, y = y0;
  for (let i = 0; i < n; i++) {                                // a little further out with every span of it, and a wave running down it
    const s = (i + .5) / n, a = blow * (.6 + .4 * s) + Math.sin(s * 5.5 - t * (2 + blow * 5)) * (.04 + blow * .34) * s;
    const dx = -Math.sin(a) * L, dy = Math.cos(a) * L, wd = .042 * PW * (.3 + .7 * Math.sqrt(s)) * (.62 + .38 * Math.min(1, blow / .5));
    const f = s * (cols.length - 1), c = mix(cols[f | 0], cols[Math.min(cols.length - 1, (f | 0) + 1)], f - (f | 0));
    P.brush(x - dx * .2, y - dy * .2, x + dx * 1.2, y + dy * 1.2, wd, c, { a: 1, sp: 1.2 * U, dry: 0, full: 1, jit: 5, bow: 0 });
    x += dx; y += dy;
  }
  P.haze = null;
}
function drawLantern(P, t) {                                   // its flame is never still either
  const x = px(LAMP[0]), y = py(LAMP[1]);
  if (x < 0 || y < 0 || x >= W || y >= H || !at(x, y) || !night || zone(x, y) !== HUT) return;
  const a = .72 + .16 * Math.sin(t * 7.3) + .12 * Math.sin(t * 12.1 + 1), r = Math.max(1.3 * U, .005 * PW);
  P.dab(x, y, r * (5 + traits.fog * 4), pal.lights[0], { a: .2 * a, den: .5, s: 2.4 * U, jit: 2 });
  P.dab(x, y, r, pal.lights[2], { a, den: 3, core: a, s: U, jit: 2 });
}

// ---------- the painting, one frame's worth ----------
let tops = null, open = null;                                  // where the sky ends, column by column; and how far down it is open while the leaves are on
const RATE = { land: 11000, built: 14000, limb: .5, staff: 50, lamp: 5, fog: 1100, glint: 50 };
const place = {
  colors: {
    wood: ['#6d3f24', '#9c5f37', '#c08350', '#dba874'], woodPale: '#cdb590', shingle: ['#16161a', '#2b2421', '#43342b'], hole: '#131010', beam: '#77848f',
    grey: ['#4c4f50', '#74787a', '#9a9c98', '#bdbdb4'], tan: ['#8a6a4a', '#b8966c', '#d9bd92'], moss: '#5f7355',
    ivy: ['#0f2118', '#1b3a26', '#2c5a33', '#4a7a3e', '#8fb07c'],
    ash: ['#132a20', '#1f4a36', '#33704f', '#56966a', '#8fc090'],
    soil: ['#211b18', '#3d3229', '#5b4a3a', '#7c6950'], twigs: ['#2b2724', '#463e37', '#685e53', '#8b8073'],
    stem: '#7a3f30', berry: '#17151f', bloom: '#f1edd6', fluff: '#d6d4c6',
    flag: ['#8a4fb0', '#e8792c', '#f4c738', '#57b356', '#2aa79c', '#2f7bd2', '#5b48ae'],
    snow: '#eef2f4',
  },
  seasons: {
    spring: { ash: ['#17301f', '#2c5f3a', '#4c9150', '#82bf66', '#c2e28e'] },
    autumn: { ash: ['#3a3418', '#6a6222', '#a5952c', '#d6c040', '#f0e27c'], ivy: ['#101f17', '#1c3826', '#2d5632', '#4b743c', '#8ca676'] },
    winter: { ivy: ['#101f18', '#1c3626', '#2b4f32', '#44693c', '#7f9c78'], soil: ['#262220', '#40372f', '#5b4e41', '#786a57'] },
  },
  ground: /^snow/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    // An upright screen shows the photograph from top to bottom, and of its width what there is room for, the hut in
    // the middle.  A wide one loses some of its sky and of the foot of the cone; the hut stands on the right then,
    // and the clearing lies to the left of it.  A very wide one comes closer.
    const wide = clamp((W / H - .75) / .6);
    let shown = .97 - .1 * wide;
    Y0 = .012 + .068 * wide; PH = H / shown; PW = PH * .75;
    if (W / PW > 2.5) { PW = W / 2.5; PH = PW / .75; shown = H / PH; Y0 = .08 + (.87 - shown) * .18; }
    X0 = HMID - (.5 + .32 * wide) * W / PW;
    if (W < PW) X0 = clamp(X0, 0, 1 - W / PW);
    place.hz = py(.5);
    poleMin = Math.max(.0022, .8 * U / PW);
    leafSz = clamp(.0125 * PW, 3.2 * U, 10 * U); LF = .55 / leafSz;
    bx0 = Math.max(0, px(.33)); bx1 = Math.min(W, px(.7)); by0 = Math.max(0, py(.255)); by1 = Math.min(H, py(.61));
    HU = Math.max(U, .0018 * PW);
    OUT = outline();
    buildWood();
    const n = Math.ceil(W) + 1;
    tops = new Float32Array(n); open = new Float32Array(n);
    for (let i = 0; i < n; i++) {
      tops[i] = py(skyTop(fx(i + .5)));
      const c = Math.min(gCols - 1, (i / gCell) | 0); let j = 0;
      while (j < gRows && j * gCell < tops[i] && backQ[j * gCols + c] < .15 && frontQ[j * gCols + c] < .15) j++;
      open[i] = j * gCell;
    }
  },
  tick(t) { time = t; sway = wind(t) * 1.5 * U; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to where the wood is dense, and to the roof
    const dip = 3 * U - pad, n = tops.length;
    let y = tops[n - 1] + dip;
    c.moveTo(0, -60); c.lineTo(W, -60); c.lineTo(W, y);
    for (let i = n - 2; i >= 0; i--) { const e = tops[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(0, y); c.closePath();
  },
  front(s, x, y) { use(s); return zone(x, y) !== SKY; },       // leaves, limbs and the flag's pole stand in front of the sky
  orb(o) {                                                     // the sun and the moon stand where the sky is open: over the clearing, or beside the flag
    o.x = clamp(px(X0 < -.62 ? -.44 : .405), o.r * 1.6, W - o.r * 1.6);
    let clear = 1e9;
    for (let i = Math.max(0, (o.x - o.r * 1.3) | 0); i < Math.min(open.length, o.x + o.r * 1.3); i++) clear = Math.min(clear, open[i]);
    o.y = Math.max(o.r * 1.3, Math.min(o.y, clear - o.r * 1.5));
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6, leafless = (s && s.traits.bare) || (o && o.traits.bare);
    for (let n = 0; n < 3; n++) spend('rb-land' + n, RATE.land * W * H / 3 / ua * pace, BANDS[n]);
    PP.c.save(); PP.c.beginPath(); hutPath(PP.c); PP.c.clip();   // nothing of the hut beyond its walls
    spend('rb-built', RATE.built * Math.max(0, bx1 - bx0) * Math.max(0, by1 - by0) / (HU * HU * 1e6) * pace, builtStroke);
    PP.c.restore();
    if (leafless) spend('rb-limb', RATE.limb * limbLength / (14 * U) * pace, limbStroke);
    else spend('rb-stout', RATE.limb * 3 * stoutLength / (14 * U) * pace, stoutStroke);
    spend('rb-staff', RATE.staff * pace, staffStroke);
    if ((s && s.night) || (o && o.night)) spend('rb-lamp', RATE.lamp * pace, lampDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('rb-fog', RATE.fog * W * .5 * PH / ua * pace, fogStroke);
    if (s && s.traits.glint) spend('rb-glint', RATE.glint * W * H / ua * pace, glintDab);
    PP.haze = null;
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');   // the wood and the ground, up to where the sky begins
    for (let k = i; k < j; k++) for (let n = -1; n < 2; n += 2) coatStroke(cells[k].x + n * 10 * U, cells[k].y + n * 6 * U, LAND);   // twice over: nothing of the wash under it is to show through
    c.restore();
    c.save(); c.beginPath(); hutPath(c); c.clip();             // the hut and the cone, and none of them beyond their wood; then what hangs in front of them
    for (let k = i; k < j; k++) if (cells[k].x > bx0 - 80 * U && cells[k].x < bx1 + 80 * U) for (let n = -1; n < 2; n += 2) coatStroke(cells[k].x + n * 10 * U, cells[k].y + n * 6 * U, BUILT);
    c.restore();
    for (let k = i; k < j; k++) if (cells[k].x > bx0 - 80 * U && cells[k].x < bx1 + 80 * U) coatStroke(cells[k].x, cells[k].y, HUNG);
    // The leaves that stand in front of the sky.  The sky's coat is broad and long: what it brushes later reaches back
    // over leaves this one has just put in front of it.  They get a second coat once the sky's has passed for good.
    for (let k = i; k < j; k++) if (cells[k].y < place.hz + 40 * U) coatStroke(cells[k].x, cells[k].y, ALOFT);
    if (cells !== lagCells) { lagCells = cells; lagAt = 0; }
    const n = cells.length, passed = j >= n ? Infinity : j > 0 ? cells[j - 1].k - (cells[n - 1].k - cells[0].k) / 6 - 170 * U : -Infinity;
    for (; lagAt < n && cells[lagAt].k <= passed; lagAt++) coatStroke(cells[lagAt].x, cells[lagAt].y, ALOFT);
    PP.haze = null;
  },
  sprites: [{ draw: drawFlag, t0: 3, seed: 151 }, { draw: drawLantern, t0: 4, seed: 152 }],
};
return place;
} });
