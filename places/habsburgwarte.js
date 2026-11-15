/* ------------------------------------------------------------------ *
 *  Vienna, on top of the Hermannskogel: the Habsburgwarte, the old
 *  lookout tower in its clearing among the trees, hung with the dishes
 *  and aerials it has to carry these days, on a bright day in May.
 *
 *  It is painted after a photograph, measured by hand: the tower stone
 *  by stone, how high the wood stands against the sky, the boughs that
 *  hang in from above, the young trees with hardly a leaf on them, where
 *  the grass ends and the gravel begins, and where in all that the sun
 *  lies and where the shade.  Crown by crown the wood is made up, and so
 *  is how it goes on beyond the photograph on either side.  The people
 *  who were about that day are not in it.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'habsburgwarte', name: 'Habsburgwarte', make(kit) {
'use strict';
const { PP, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0, sway = 0;   // the view (CSS pixels, the stroke unit), the clock and how far the wind pushes the leaves
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false, snow = false, sun = 1;   // sun: how hard the shadows are
const use = s => {
  pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; snow = traits.snow > 0;
  sun = night ? .45 : traits.shadow > 0 ? (bare ? .6 : 1) : .25;
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph: x and y as fractions of its width and height.  It is an upright one: a
// narrow screen shows a slice of its width, a wide one a slice of its height and what lies to its left and right.
// The photograph looks up, and the tower leans in it.  The picture stands it upright: every row of the photograph
// is pushed a little further to the right the higher up it is.
let X0 = 0, Y0 = 0;
const LEAN = .16, FOOT = .703;
const px = (f, g) => (f - LEAN * (g - FOOT) - X0) * PW, py = f => (f - Y0) * PH, fy = y => Y0 + y / PH, fx = (x, y) => X0 + x / PW + LEAN * (fy(y) - FOOT);
function along(pts, x) {                                       // a line through points
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * (x - a[0]) / (b[0] - a[0]); }
  return pts[pts.length - 1][1];
}
function inPoly(p, x, y) {
  let o = false;
  for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o;
  return o;
}
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;   // by night it is no lighter than the sky
const veil = d => { PP.haze = traits.fog ? [fogCol(), clamp(d * 1.4 - .1, 0, .85) * traits.fog] : null; };   // fog: the farther off, the paler

// ---------- the wood ----------
// It stands all round the clearing.  How high its leaves stand against the sky is measured along its top: [x, y].
// Beyond the photograph it goes on as it likes, its trees taller here and lower there.  In winter it is bare: sky
// between the limbs of the trees, down to where trunks and twigs stand too close (DENSE).
const GROUND = .703;                                           // where the trees end and the clearing begins
const RIDGE = [[0, .4], [.1, .405], [.115, .44], [.15, .455], [.17, .49], [.23, .5], [.26, .51], [.3, .525], [.4, .535], [.44, .55], [.47, .56], [.5, .59], [.55, .6], [.62, .6], [.7, .595],
  [.76, .585], [.8, .588], [.82, .58], [.84, .555], [.86, .5], [.875, .48], [.9, .475], [1, .47]];
const DENSE = [[0, .6], [.2, .615], [.45, .64], [.62, .645], [.8, .64], [1, .605]];
const out = xf => xf < 0 ? -xf : xf > 1 ? xf - 1 : 0;          // how far beyond the photograph a point lies
function ridge(xf) {
  const d = out(xf);
  let y = along(RIDGE, xf);
  if (d) y += (.52 - .26 * smooth(.5, .85, vnoise(xf * 1.9 + 3.3, 1.7)) + (vnoise(xf * 7, 4.2) - .5) * .09 - y) * smooth(0, .15, d);
  return y;
}
const dense = xf => along(DENSE, xf) + (vnoise(xf * 30, 5.5) - .5) * .022 + (vnoise(xf * 110, 2.5) - .5) * .008;
const close = (xf, pf) => clamp((.52 - pf) / .2) * (xf > .66 || xf < .22 ? 1 : .35);   // the boughs overhead are near, the wood is not
// A crown is a round: [x, y, how big].  Those of the boughs that hang in from above are measured: at the top on the
// left, further down on the left, over the tower, and all that the tree close by on the right holds out.  The trees
// they belong to stand beyond the photograph, and there is more of them there.
const WOODS = 0, BOUGH = 1, SHOOT = 2;
const OVERHEAD = [[.03, .02, .06], [.09, .02, .03], [.02, .08, .04], [.14, .02, .02], [.15, .04, .012], [-.07, .03, .07], [-.17, 0, .08], [-.11, .1, .05], [-.24, .07, .05],
  [-.01, .285, .07], [.045, .29, .035], [.005, .355, .025], [.02, .385, .02], [-.06, .2, .05], [-.1, .26, .08], [-.17, .31, .08], [-.09, .36, .07], [-.2, .39, .07],
  [.66, 0, .05], [.74, .005, .055],
  [.93, .01, .08], [1, .03, .08], [.88, .09, .085], [.99, .1, .1], [.8, .113, .04], [.9, .17, .075], [1, .2, .1], [.95, .24, .065], [.91, .29, .065], [1, .31, .07], [.89, .335, .04], [.86, .14, .05], [.87, .22, .045], [.9, .385, .045], [.975, .42, .045],
  [1.1, .03, .1], [1.3, .05, .06], [1.2, .09, .09], [1.12, .15, .09], [1.25, .2, .07], [1.15, .26, .08], [1.22, .32, .06], [1.1, .34, .06], [1.08, .42, .04]];
// Young trees stand up out of the wood with hardly a leaf on them: [from x, y, how big, to x, y, how big], crown
// by crown along them.
const SHOOTS = [[.3, .415, .006, .31, .47, .012], [.285, .44, .006, .3, .5, .013], [.33, .445, .006, .315, .5, .012], [.31, .47, .012, .31, .525, .018],
  [.245, .432, .006, .24, .47, .011], [.225, .455, .006, .24, .5, .013], [.26, .465, .006, .245, .505, .012], [.195, .44, .005, .2, .49, .012],
  [.385, .42, .006, .38, .47, .012], [.365, .445, .006, .375, .5, .012], [.41, .44, .006, .39, .5, .013], [.445, .47, .006, .42, .5, .011], [.4, .5, .012, .42, .545, .016],
  [.79, .54, .006, .8, .59, .013], [.815, .55, .006, .805, .585, .011], [.835, .55, .006, .84, .58, .012], [.585, .572, .007, .575, .6, .015], [.52, .575, .007, .515, .6, .013]];
// The wood itself is a heap of crowns under its top, made up: they are kept in a coarse grid, how deep in the
// leaves a point is, whose they are and how high up in their crown.  It reaches a little beyond the picture (gPad),
// as a stroke may.
let rounds = [], gCell = 3, gPad = 0, gCols = 0, gRows = 0, gQ = null, gK = null, gV = null;
function deep(xf, pf) {                                        // how deep in a crown: 1 in its middle, 0 at its edge
  let q = -1;
  for (const c of rounds) {
    const dx = (xf - c[0]) / c[2]; if (dx > 1 || dx < -1) continue;
    const d = 1 - Math.hypot(dx, (pf - c[1]) / (c[2] * .75)); if (d > q) q = d;
  }
  return q;
}
function buildWood() {
  const x0 = Math.min(fx(0, 0), fx(0, H)) - .15, x1 = Math.max(fx(W, 0), fx(W, H)) + .15;
  rounds = [];
  for (const c of OVERHEAD) if (c[0] + c[2] > x0 && c[0] - c[2] < x1) rounds.push([c[0], c[1], c[2], BOUGH]);
  for (const t of SHOOTS) for (let f = 0; f <= 1;) { const r = t[2] + (t[5] - t[2]) * f; rounds.push([t[0] + (t[3] - t[0]) * f, t[1] + (t[4] - t[1]) * f, r, SHOOT]); f += r * .9 / Math.hypot(t[3] - t[0], (t[4] - t[1]) * 4 / 3); }
  for (let n = Math.floor(x0 / .03); n <= x1 / .03; n++) {     // one stands up to the top of the wood, more of them under it
    const r = rng(7000 + n), x = (n + r()) * .03, top = ridge(x) + (r() - .4) * .03, s = .02 + r() * .035;
    rounds.push([x, top + s * .6, s, WOODS]);
    for (let y = top + .03 + r() * .04; y < GROUND + .04; y += .03 + r() * .04) rounds.push([x + (r() - .5) * .04, y, .035 + r() * .035, WOODS]);
  }
  gCell = 2.5 * U; gPad = 12 * gCell; gCols = Math.ceil((W + 2 * gPad) / gCell) + 1; gRows = Math.ceil((leafFoot + gPad) / gCell) + 1;
  gQ = new Float32Array(gCols * gRows).fill(-1); gK = new Uint8Array(gCols * gRows); gV = new Int8Array(gCols * gRows);
  for (const c of rounds) {
    const cx = px(c[0], c[1]) + gPad, cy = py(c[1]) + gPad, r = c[2] * PW;
    const i0 = Math.max(0, Math.floor((cx - r * 1.15) / gCell)), i1 = Math.min(gCols - 1, Math.ceil((cx + r * 1.15) / gCell)), j0 = Math.max(0, Math.floor((cy - r) / gCell)), j1 = Math.min(gRows - 1, Math.ceil((cy + r) / gCell));
    for (let j = j0; j <= j1; j++) for (let i = i0; i <= i1; i++) {
      const x = (i + .5) * gCell - gPad, y = (j + .5) * gCell - gPad, v = (c[1] - fy(y)) / (c[2] * .75), q = 1 - Math.hypot((fx(x, y) - c[0]) / c[2], v), n = j * gCols + i;
      if (q > gQ[n]) { gQ[n] = q; gK[n] = c[3]; gV[n] = clamp(v, -1, 1) * 100; }
    }
  }
  for (let j = 0; j < gRows; j++) for (let i = 0; i < gCols; i++) {   // under its top the wood is leaves all through
    const x = (i + .5) * gCell - gPad, y = (j + .5) * gCell - gPad, n = j * gCols + i;
    if (gQ[n] < .6 && fy(y) > ridge(fx(x, y)) + .02) { if (gQ[n] < 0) gK[n] = WOODS; gQ[n] = .6; }
  }
}
let zk = 0, zv = 0;                                            // what fol() learnt: whose the leaves are, how high up in their crown
function fol(x, y) {                                           // > 0 inside the leaves
  const i = Math.floor((x - sway + gPad) / gCell), j = Math.floor((y + gPad) / gCell);
  if (i < 0 || j < 0 || i >= gCols || j >= gRows) return -1;
  const n = j * gCols + i, q = gQ[n];
  if (q <= -1) return -1;
  zk = gK[n]; zv = gV[n] / 100;
  const lace = (vnoise(x * .13 / U, y * .13 / U) - .5) * .8 + (vnoise(x * .03 / U + 7, y * .03 / U) - .5) * .45;   // leaf by leaf at its rim
  return (zk === SHOOT ? q - .25 : q * 1.3) + lace;   // a young tree is all sprays, light shows through it everywhere
}

// ---------- grass and gravel ----------
// The clearing is gravel.  Long grass stands at the foot of the wood, and weeds at the foot of the tower: how far
// down it reaches, [x, y].  In front on the right, where nobody walks, it grows in tufts (a patch of it all round),
// and from there the meadow goes on to the right.
const VERGE = [[0, .731], [.05, .729], [.28, .724], [.31, .713], [.545, .709], [.552, .731], [.622, .731], [.63, .709], [.683, .709], [.69, .731], [.795, .733], [.81, .752], [.83, .762], [.845, .75], [.86, .722],
  [.94, .72], [.95, .742], [1, .742]];
const TUFTS = [[1, .765], [.95, .79], [.88, .81], [.9, .84], [.83, .852], [.76, .872], [.71, .89], [.66, .905], [.72, .925], [.74, .95], [.75, 1.2], [4, 1.2], [4, .75], [1.25, .75]];
function grassy(xf, pf) {
  if (pf < along(VERGE, xf) + (vnoise(xf * 60, 7.7) - .5) * .008) return true;
  return pf > .74 && xf > .6 && vnoise(xf * 45, pf * 70 + 5) > .2 && inPoly(TUFTS, xf + (vnoise(xf * 40, pf * 55) - .5) * .03, pf + (vnoise(xf * 40 + 31.7, pf * 55 + 17.3) - .5) * .02);
}
// The trees behind us throw their shade on it: deep on the left (all round it), with the sun in a patch that runs
// down the middle, half and half on the right.
const SHADOW = [[-.5, .8], [-.25, .77], [0, .737], [.05, .74], [.1, .755], [.18, .775], [.1, .79], [.3, .8], [.42, .812], [.4, .84], [.47, .857], [.4, .89], [.37, .9], [.33, .93], [.31, .95], [.33, .965],
  [.43, .975], [.45, 1], [.45, 1.2], [-.45, 1.2]];
const SUNLIT = [[.4, .8], [.82, .795], [.8, .825], [.68, .83], [.6, .85], [.62, .88], [.56, .9], [.56, .99], [.57, 1.2], [.45, 1.2], [.44, .97], [.33, .955], [.33, .93], [.4, .9], [.42, .88], [.48, .855],
  [.42, .835], [.43, .81]];

// ---------- light and shade ----------
// How light a thing is, from 0, the dark in among the leaves, to 1, the sun on it, is measured as well.  The wood is
// light along its top and dark at its foot, a crown light on top.  On that lie patches of sun and of shade: [x, y,
// half as wide, half as high, how much lighter it is there].  The rest is leaves: clumps of them with the light on
// their upper left, and no leaf quite as light as the next.
const SPOTS = [[.06, .655, .13, .055, -.24], [.06, .5, .07, .04, -.1], [.38, .57, .1, .04, .08], [.93, .63, .1, .07, -.14], [.62, .66, .16, .03, -.05],   // in the wood: darkest at its foot on the left
  [.04, .05, .1, .07, -.22], [.02, .29, .07, .07, .14], [.64, 0, .03, .02, .3], [.72, .02, .07, .03, .06],                                               // on the boughs overhead
  [.91, .14, .07, .07, .2], [.81, .09, .04, .05, -.2], [.99, .08, .03, .1, -.1], [.95, .4, .08, .05, -.2]];
const clumps = (xf, pf) => { const a = xf * 44 + 3, b = pf * 56, f = fbm(a, b); return (f - .47) * .6 + (f - fbm(a - .3, b - .34)) * .5; };
function lightAt(k, xf, pf) {                                  // how light a thing (k) is at a point
  if (k === LEAF) {
    const top = ridge(xf);
    let t = (zk === BOUGH ? .42 : .54 - .28 * clamp((pf - top) / (GROUND - top))) + .1 * zv;
    for (const p of SPOTS) { const d = Math.hypot((xf - p[0]) / p[2], (pf - p[1]) / p[3]); if (d < 1.5) t += p[4] * smooth(1.5, .5, d); }
    t += (vnoise(xf * 6 + 1.3, pf * 5) - .5) * .2;            // a tree is lighter than the next, or darker
    return t + (clumps(xf, pf) * 1.2 + (vnoise(xf * 230, pf * 300 + 9) - .5)) * clamp(.3 + t * 1.4);   // in the shade they are all alike
  }
  const a = xf + (fbm(xf * 20, pf * 28) - .47) * .12, b = pf + (fbm(xf * 20 + 31.7, pf * 28 + 17.3) - .47) * .07 + (vnoise(xf * 95, pf * 130) - .5) * .012;   // the shade of leaves has no straight edge
  if (k === GRASS) return b < .775 ? .76 + (vnoise(xf * 30, pf * 90) - .5) * .25 : inPoly(SHADOW, a, b) ? .1 : .05 + .6 * smooth(.25, .75, vnoise(xf * 24 + 3, pf * 60));
  if (inPoly(SHADOW, a, b)) return .1 + .5 * smooth(.62, .85, vnoise(xf * 30 + 5, pf * 110));   // a fleck of sun here and there
  const dapple = fbm(xf * 18 + 2, pf * 50);
  if (b < .772 || inPoly(SUNLIT, a, b)) return .92 - .5 * smooth(.6, .72, dapple);
  return .3 + .6 * smooth(.5, .38, dapple - .08 + smooth(.78, .95, b) * .18);   // further down there is less of the sun
}

// ---------- the tower ----------
// What it is made of, part by part, from the tip of its mast to the table in front of it.  It leans: the photograph
// looks up at it, so what is upright on it runs to a point high above, and what is level on its round top is an arch.
const ROUGH = 1, ASHLAR = 2, DARK = 3, MOULD = 4, DISH = 5, GREY = 6, DOOR = 7, GRILLE = 8, BOARD = 9, STEP = 10, TABLE = 11, RELIEF = 12, SHADE = 13;
const TB = [.455, .2, .8, .76];                                // the box all of it stands in
let tLt = 1, tC = 0, tA = 0, tRound = false, tDir = 0, tBig = false, tU = 0;   // what towerPart() learnt: how light, which course and how far along it, ...
const iso = (ax, ay, bx, by) => Math.hypot((bx - ax) * .75, by - ay);          // a distance, in heights of the photo
// The round top.  A ring round it is an arch in the picture, the higher up the steeper: [its height in the middle, how far its ends hang down].
const SAG = [[.335, .048], [.358, .035], [.39, .028], [.42, .028], [.47, .022]];
const sag = c => along(SAG, c), turX = v => .6075 + Math.max(0, v - .4) * .12, turW = v => .0925 - Math.max(0, v - .4) * .03;
function turretPt(th, c) {                                     // the point at an angle round it (0 faces us), on the ring of a height
  const pf = c + sag(c) * (1 - Math.cos(th));
  return [turX(pf) + turW(pf) * Math.sin(th), pf];
}
const merlon = th => { const f = (th + .12) / .34; return f - Math.floor(f) < .6; };
const WINDOWS = [[-.73, .09, 0], [-.108, .14, 1], [.48, .1, 0]];   // in the turret: [where, how wide, whether it is boarded up]
function turret(xf, pf) {
  const hw = turW(pf), q = (xf - turX(pf)) / hw;
  if (q <= -1 || q >= 1) return 0;
  const s = 1 - Math.sqrt(1 - q * q);
  let c = pf - sag(pf) * s; c = pf - sag(c) * s;
  if (c < .335) return 0;
  const th = Math.asin(q);
  tRound = true; tDir = 0; tBig = false; tC = c; tA = th;
  tLt = .52 + .48 * smooth(-.9, -.2, q) - .1 * smooth(.72, 1, q);   // the sun stands on the right, in front
  if (c < .347) return merlon(th) ? ROUGH : 0;                 // the battlements
  if (c < .358) return ROUGH;
  if (c < .392) {                                              // they stand out on corbels: dark between them
    const f = th / .3 + .5, g = f - Math.floor(f) - .5, top = .3605 + .007 * (1 - Math.sqrt(Math.max(0, 1 - (g / .29) * (g / .29))));
    if (Math.abs(g) < .29 && c > top && c < .389) return DARK;
    if (c > .365 && Math.abs(g) > .33) { const n = (c - .365) / .0105; if (n - Math.floor(n) > .68) return DARK; }   // and the notches in them
    return ROUGH;
  }
  if (c < .414) {
    if (c < .397) tLt *= .82;                                  // in their shade
    for (const w of WINDOWS) {
      const d = Math.abs(q - w[0]) / w[1];
      if (d < 1 && c > .3945 + .006 * d * d && c < .4125) { tU = w[0]; return w[2] && c > .401 ? BOARD : DARK; }
    }
    return ROUGH;
  }
  if (c < .4165) { tLt *= .7; return SHADE; }                  // a band round it
  if (c < .4225) return MOULD;
  return ROUGH;
}
// The mast on its little stone cap, a grey dome beside it
const CAP = [[.569, .294], [.587, .294], [.596, .312], [.594, .342], [.562, .342], [.561, .312]];
const mastX = pf => .566 + (pf - .212) * .1446, mastW = pf => .0022 + (pf - .212) * .012;
// The dishes: [x, y, half its length, half its breadth (in heights of the photo), which way its length lies, whether it is a white one]
const DISHES = [[.4885, .398, .024, .0145, -.9, 1], [.708, .453, .035, .019, 1.31, 1], [.681, .456, .023, .011, 1.4, 0], [.702, .506, .022, .013, 1.45, 0]];
function dishes(xf, pf) {
  for (const d of DISHES) {
    const dx = (xf - d[0]) * .75, dy = pf - d[1];
    if (dx > .04 || dx < -.04 || dy > .04 || dy < -.04) continue;
    const ca = Math.cos(d[4]), sa = Math.sin(d[4]), u = (dx * ca + dy * sa) / d[2], v = (-dx * sa + dy * ca) / d[3];
    if (u * u + v * v >= 1) continue;
    tRound = false; tDir = 0; tU = Math.max(.01, Math.hypot(u, v));
    if (d[5]) { tLt = d[0] < .6 ? clamp(.5 - v * .55 - u * .2) : clamp(.85 - u * .25 + v * .2); return DISH; }
    tLt = d[1] > .49 ? .8 + .12 * v : .55 - v * .25; return GREY;
  }
  return 0;
}
// The gable over the door: its tip and its feet.  The arch under it.
const GA = [.615, .474], GL = [.551, .592], GR = [.718, .584], MT = .0075;
const off = (a, b, xf, pf) => ((b[0] - a[0]) * .75 * (pf - a[1]) - (b[1] - a[1]) * (xf - a[0]) * .75) / iso(a[0], a[1], b[0], b[1]);
const archX = pf => .6275 + (pf - .6) * .06;
function arch(xf, pf) {
  if (pf < .543) return 0;
  const d = Math.abs(xf - archX(pf));
  const wi = pf < .59 ? (pf < .553 ? 0 : .0365 * (1 - Math.pow((.59 - pf) / .037, 2.4))) : .0365;
  if (d < wi) { tDir = 0; if (pf < .585) return GRILLE; if (pf < .594) return MOULD; return xf < archX(pf) + .001 ? DOOR : DARK; }
  if (pf < .6) {
    const wo = pf < .59 ? .0435 * (1 - Math.pow((.59 - pf) / .047, 2.4)) : .0435;
    if (d < wo) { tDir = 4; if (d - wi < .003) { tLt *= .75; return SHADE; } return MOULD; }
  }
  return 0;
}
function body(xf, pf) {                                        // from the foot of the turret down to the landing
  const xl = .506 + (pf - .455) * .075, xr = .713 + (pf - .455) * .245;
  if (xf < xl || xf > xr) return 0;
  tRound = false; tDir = 0; tBig = false; tLt = 1; tU = 0; tC = pf; tA = xf - (pf - .5) * .13;
  const sh = (xf - .703) * (xf - .703) / .0012 + (pf - .492) * (pf - .492) / .0018;   // the shade of the big dish
  if (sh < 1 && xf > .655) tLt = .72 + .28 * smooth(.3, 1, sh);
  if (pf > GA[1] && pf < .592) {
    const dl = -off(GA, GL, xf, pf), dr = off(GA, GR, xf, pf);
    if (dl > 0 && dr > 0) {
      const e = Math.min(dl, dr);                               // its frame: a pale band, a dark line either side of it
      if (e < MT + .0035) { tDir = dl < dr ? 2 : 3; if (e > .0022 && e < MT) return MOULD; tLt *= e < MT ? .45 : .6; return SHADE; }
      const a = arch(xf, pf); if (a) return a;
      if (pf > .5 && pf < .5345 && dl > MT + .012 && dr > MT + .012) return RELIEF;
      if (pf >= .5345 && pf < .5405 && dl > MT + .005 && dr > MT + .005) return MOULD;
      return ASHLAR;
    }
  }
  if (pf < .513) {                                             // the balcony
    if (pf >= .492 && xf > .548 && xf < .7) { if (pf < .5025) return MOULD; tLt *= .5; return SHADE; }
    if (pf > .463 && pf < .492 && Math.abs(xf - .612) < .0135 * Math.min(1, Math.sqrt((pf - .463) / .008))) return DARK;   // the door out to it
    if (pf > .479 && pf < .492) for (const t of [.576, .598, .652]) if (Math.abs(xf - t) < .0105 - (pf - .479) * .2) { tLt = .3; return GREY; }   // tubs of flowers
    if (pf < .4595 && xf < .545) return MOULD;
    return ROUGH;
  }
  if (pf >= .592 && xf > .549 && xf < .716) {                  // the porch under the gable: two columns, the door between them
    const a = arch(xf, pf); if (a) return a;
    const col = Math.abs(xf < .63 ? (xf - .5705) / .0145 : (xf - .6965) / .0125);
    if (pf < .618) {
      if (pf > .598 && col < 1.5) { if (pf > .6145) { tLt *= .6; return SHADE; } tDir = 5; return MOULD; }
      if (col < 1.5 || pf < .598) return ASHLAR;
    } else if (col < 1) { tLt *= .66 + .34 * Math.sqrt(1 - col * col); return ASHLAR; }
    else if (col < 1.22) { tLt *= .5; return SHADE; }
    if (xf > .556 && xf < .709) { tLt *= .86; return ASHLAR; }
  }
  if (xf < xl + .012) { if (pf > .523 && pf < .529) return MOULD; tLt *= .86; return ASHLAR; }   // its left side
  if (xf < xl + .047 || xf > xr - .045 || pf > .592) return ROUGH;   // rough stone at the corners
  return ASHLAR;
}
const weeds = xf => { const w = Math.min(smooth(.552, .57, xf) * smooth(.625, .6, xf) + smooth(.688, .705, xf) * smooth(.8, .775, xf), 1); return w ? w * (.004 + .024 * vnoise(xf * 85, 1.7)) : 0; };
const TREAD0 = .6785, RISE = .0105, GOING = .0145, STAIR = .548;      // the stairs go down to the left
function foot(xf, pf) {                                        // the landing, the stairs up to it, the wall under it
  tRound = false; tDir = 0; tLt = 1; tU = 0; tBig = true; tC = pf; tA = xf;
  if (xf < STAIR) {
    if (xf < STAIR - 5 * GOING) return 0;
    const tread = TREAD0 + Math.floor((STAIR - xf) / GOING) * RISE;
    if (pf >= tread) { if (pf < tread + .004) return MOULD; tLt = .7; return STEP; }
    if (xf >= .506 + (pf - .455) * .075) { tBig = false; tA = xf - (pf - .5) * .13; return ROUGH; }   // the tower behind them
    return 0;
  }
  if (xf > .776 + Math.max(0, pf - .682) * .2) return 0;
  if (pf < .682) return MOULD;
  if (pf < .687) { tLt = .5; return SHADE; }
  if (pf > .727 - weeds(xf)) return 0;
  if (xf > .668 && xf < .686 && pf > .689 && pf < .702) { tLt = .45; return GREY; }   // a plaque
  return ROUGH;
}
function table(xf, pf) {                                       // a table and a bench in front of it
  tRound = false; tBig = false; tDir = 0; tLt = 1;
  if (pf >= .722 && pf < .7265 && xf > .546 && xf < .697) return TABLE;
  if (pf >= .7365 && pf < .7405 && xf > .556 && xf < .7) { tLt = .9; return TABLE; }
  if (pf >= .7265 && pf < .752 && (Math.abs(xf - .578) < .003 || Math.abs(xf - .668) < .003)) { tDir = 1; tLt = .45; return TABLE; }
  return 0;
}
function towerPart(xf, pf) {
  if (pf > .72) { const t = table(xf, pf); if (t) return t; }
  if (pf > .37 && pf < .54) { const d = dishes(xf, pf); if (d) return d; }
  if (pf < .296) {                                             // the mast: white and grey, ring by ring
    if (pf > .212 && Math.abs(xf - mastX(pf)) < mastW(pf)) { tRound = false; tDir = 1; tU = 0; tLt = Math.floor(pf / .014) & 1 ? 1 : .6; return GREY; }
    return 0;
  }
  if (pf < .342) {
    if (((xf - .578) * (xf - .578)) / .000196 + ((pf - .331) * (pf - .331)) / .000121 < 1) { tRound = false; tDir = 0; tU = .5; tLt = clamp(.75 + (xf - .578) * 14 - (pf - .331) * 20); return GREY; }
    if (inPoly(CAP, xf, pf)) { tRound = false; tDir = 0; tLt = xf < .571 + (pf - .3) * .1 ? .72 : 1; return ASHLAR; }
  }
  if (pf < .455) return turret(xf, pf);
  if (pf < .668) return body(xf, pf);
  if (pf < .728) return foot(xf, pf);
  return 0;
}
function outline() {                                           // all round its stone, from the foot of the stairs over the top and down again
  const o = [[STAIR - 5 * GOING, .727]];
  for (let i = 4; i >= 1; i--) o.push([STAIR - (i + 1) * GOING, TREAD0 + i * RISE], [STAIR - i * GOING, TREAD0 + i * RISE]);
  for (let v = TREAD0 + RISE; v > .455; v -= .01) o.push([.506 + (v - .455) * .075, v]);
  o.push([.506, .455], [turX(.455) - turW(.455), .455]);
  for (let v = .445; v > .384; v -= .01) o.push([turX(v) - turW(v), v]);
  const N = 60; let cap = 0;
  for (let i = 0; i <= N; i++) {
    const th = -Math.PI / 2 + Math.PI * i / N, a = turretPt(th, .335), b = turretPt(th, .347), up = merlon(th);
    if (!cap && a[0] > .562) {                                 // up the cap, up the mast and down again
      cap = 1;
      o.push([.562, .342], [.561, .312], [.569, .294], [mastX(.294) - mastW(.294), .294], [mastX(.212) - mastW(.212), .212], [mastX(.212) + mastW(.212), .212],
        [mastX(.294) + mastW(.294), .294], [.587, .294], [.596, .312], [.594, .342]);
    }
    if (a[0] > .562 && a[0] < .594) continue;
    if (i && up !== merlon(th - Math.PI / N)) o.push(up ? b : a);
    o.push(up ? a : b);
  }
  for (let v = .39; v < .455; v += .01) o.push([turX(v) + turW(v), v]);
  o.push([turX(.455) + turW(.455), .455]);
  for (let v = .455; v < .668; v += .01) o.push([.713 + (v - .455) * .245, v]);
  o.push([.765, .668], [.776, .668], [.776, .682], [.785, .727]);
  return o;
}
// What is too thin to be anything but a line: railings, the aerials, the racks they are bolted to.  [kind, width, from, to]
const IRON = 0, AERIAL = 1, STEEL = 2;
function thinList() {
  const s = [], add = (k, w, x1, y1, x2, y2) => s.push([k, w, x1, y1, x2, y2]);
  for (const [y, l] of [[.232, .022], [.249, .024], [.266, .024]]) {       // up the mast
    const x = mastX(y);
    add(AERIAL, 1, x, y, x + l, y - .011);
    for (const f of [.45, .75, 1]) add(AERIAL, .8, x + l * f - .004, y - .011 * f - .006, x + l * f + .004, y - .011 * f + .007);
  }
  add(AERIAL, 1.1, .577, .284, .604, .277); add(AERIAL, 1.2, .604, .277, .6065, .313); add(AERIAL, .9, .564, .283, .577, .286);
  add(AERIAL, 1, .652, .336, .668, .328); add(AERIAL, 1, .666, .319, .6695, .341);   // on the turret, right
  add(AERIAL, 1, .69, .361, .713, .352); add(AERIAL, 1.2, .7125, .395, .7145, .428); add(AERIAL, 1, .698, .411, .7145, .408);
  add(AERIAL, 1, .493, .366, .52, .3695); add(AERIAL, 1, .5085, .345, .5085, .378);  // and left
  for (const [xa, xb, y0, y1] of [[.5, .521, .402, .456], [.699, .717, .366, .44]]) {
    add(STEEL, 1, xa, y0, xa + (y1 - y0) * .13, y1); add(STEEL, 1, xb, y0, xb + (y1 - y0) * .13, y1);
    for (let y = y0 + .006; y < y1; y += .0085) add(STEEL, .7, xa + (y - y0) * .13, y, xb + (y - y0) * .13, y);
  }
  add(IRON, 1, .553, .4685, .606, .4675); add(IRON, 1, .624, .4672, .7, .466);       // the balcony
  for (let x = .556; x < .7; x += .0105) if (Math.abs(x - .615) > .012) add(IRON, .7, x, .468, x + .003, .492);
  add(IRON, 1.1, .553, .6285, .716, .627); add(IRON, 1, .553, .662, .716, .661);     // round the landing
  for (let x = .5585; x < .716; x += .0135) add(IRON, .7, x, .6285, x + .0025, .662);
  add(IRON, 1.2, .553, .6285, .4835, .672);                                          // down the stairs
  for (let i = 0; i < 5; i++) { const x = STAIR - (i + .5) * GOING; add(IRON, .8, x, .6285 + (.553 - x) / .0695 * .0435, x, TREAD0 + i * RISE); }
  add(IRON, 1.4, .4835, .668, .4835, .7205);
  add(IRON, 1.1, .716, .627, .754, .632); add(IRON, 1.3, .7535, .616, .7555, .668);  // a gate of crossed bars on the right
  add(IRON, .9, .72, .631, .751, .666); add(IRON, .9, .751, .633, .72, .666); add(IRON, 1, .716, .661, .754, .666);
  return s;
}
const THINS = thinList();
let OUT = [], skyA = 0, skyB = 0, thins = [], thinLength = 0;

// ---------- the trees ----------
// The crowns say where their leaves are, not how they stand.  For winter, and where the leaves are thin, they are
// grown: a trunk, limbs, twigs, none of them beyond the crowns.
// [x, foot, top, how wide it spreads, how it leans]
const TREES = [[-.03, .71, .22, 1.1, .12], [.1, .71, .38, .9, .04], [.245, .71, .455, .5, 0], [.3, .71, .405, .45, -.02], [.385, .71, .41, .5, .1], [.445, .71, .55, .7, 0],
  [.8, .71, .515, .8, 0], [.885, .71, .44, .8, -.05], [1.0, .71, .42, .9, -.08]];
// One stands close by, just beyond the right edge of the photograph: [x, foot].  All there is of it in the photograph
// are the boughs that hang in from above: [how high up the trunk, which way it grows, how long].
const NEAR = [1.05, .765], BOUGHS = [[-.04, 2.2, .26], [.04, 2.55, .3], [.17, 2.75, .27], [.33, 2.95, .23]];
let limbs = [], limbSum = null, limbWeight = 0, limbLength = 0, mask = null, mCols = 0, mRows = 0, mCell = 4;
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const girth = (s, t) => s.w * (1 - .5 * t) * (t < .05 ? 1.6 - t * 12 : 1);   // the trunk close by: thinner higher up, spreading where it enters the ground
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function buildTrees() {
  limbs = [];
  let r = rng(5);
  const leafy = (x, y) => { const pf = fy(y), xf = fx(x, y); return pf < GROUND + .01 && (pf > dense(xf) || deep(xf, pf) > .05); };
  const add = (x1, y1, x2, y2, bend, w, depth, near) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, near, k: r(), len });
  };
  const grow = (x, y, a, len, w, depth, near) => {
    let x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len;
    if (!leafy(x2, y2)) { len *= .5; x2 = x + Math.cos(a) * len; y2 = y + Math.sin(a) * len; if (!leafy(x2, y2)) return; }
    const last = depth >= 5 || len <= 5 * U;
    add(x, y, x2, y2, (r() - .5) * .3, w, depth, near);
    if (last) return;
    const k = r() < .7 ? 2 : 3;
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .62 + (r() - .5) * .45, len * (.6 + r() * .2), Math.max(.6 * U, w * .6), depth + 1, near);
  };
  const x0 = fx(-60 * U, H * .5) - .1, x1 = fx(W + 60 * U, H * .5) + .1, trees = TREES.filter(t => t[0] > x0 && t[0] < x1);
  for (let n = Math.floor(x0 / .15); n <= x1 / .15; n++) {     // beyond the photograph one stands every so often, as tall as the wood is there
    if (n >= -1 && n <= 7) continue;
    r = rng(900 + n);
    const xf = (n + .2 + r() * .6) * .15;
    trees.push([xf, .71, ridge(xf) + r() * .03, .5 + r() * .6, (r() - .5) * .2]);
  }
  for (const t of trees) {
    const xf = t[0], lean = t[4];
    r = rng(31 + Math.round(xf * 1000));                       // every tree keeps its shape, whatever else is to be seen
    const x = px(xf, t[1]), y = py(t[1]), h = (t[1] - t[2]) * PH, w = Math.max(1.3 * U, h * .011);
    add(x, y, x + lean * h, y - h * .42, (r() - .5) * .08, w, 0, false);
    const trunk = limbs[limbs.length - 1];
    for (let i = 0; i < 5; i++) {
      const a = -Math.PI / 2 + ((i + .5) / 5 - .5) * 2.2 * t[3] + (r() - .5) * .2 + lean, from = i === 2 ? 1 : .6 + r() * .4;
      grow(limbX(trunk, from), limbY(trunk, from), a, h * (.27 - Math.abs(a + Math.PI / 2) * .05) * (.85 + r() * .3), w * .5, 1, false);
    }
  }
  if (NEAR[0] < x1) {                                          // the one close by: its trunk, right up out of the picture, a bough to either side
    r = rng(53);
    const tx = px(NEAR[0], NEAR[1]), ty = py(NEAR[1]), top = py(Y0 - .1);
    add(tx, ty, tx - .03 * PW, top, .03, Math.max(6 * U, .036 * PW), 0, true);
    const trunk = limbs[limbs.length - 1]; trunk.trunk = true;
    for (const b of BOUGHS) for (let side = 0; side < 2; side++) {
      const a = side ? Math.PI - b[1] : b[1], from = (ty - py(b[0])) / (ty - top), x = limbX(trunk, from), y = limbY(trunk, from), len = b[2] * PH * (side ? .8 : 1), w = Math.max(1.5 * U, len * .016);
      add(x, y, x + Math.cos(a) * len * .4, y + Math.sin(a) * len * .4, (r() - .5) * .1, w, 0, true);
      const stem = limbs[limbs.length - 1];
      for (let i = 0; i < 5; i++) grow(limbX(stem, .5 + i * .125), limbY(stem, .5 + i * .125), a + ((i + .5) / 5 - .5) * 1.9 + (r() - .5) * .2, len * (.3 + r() * .14), w * .6, 1, true);
    }
  }
  // the mask: where a limb is (1), one of the bigger ones (4), the trunk close by (8); where one of the tower's thin things is (2)
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows);
  const stamp = (x, y, rad, v, foot) => {
    const m = Math.ceil(rad / mCell), ix = Math.floor(x / mCell), iy = Math.floor(y / mCell);
    for (let dy = -m; dy <= (foot ? 0 : m); dy++) for (let dx = -m; dx <= m; dx++) {   // a trunk does not go on below where it stands
      const cx = ix + dx, cy = iy + dy;
      if (cx < 0 || cy < 0 || cx >= mCols || cy >= mRows || Math.hypot(dx, dy) * mCell > rad + mCell * .5) continue;
      mask[cy * mCols + cx] |= v;
    }
  };
  limbWeight = limbLength = 0; limbSum = new Float32Array(limbs.length);
  limbs.forEach((s, i) => {
    limbWeight += s.len * (s.w + 2 * U); limbSum[i] = limbWeight; limbLength += s.len;
    const n = Math.ceil(s.len / (mCell * .5)), rad = s.w / 2 + .3 * U;
    for (let k = 0; k <= n; k++) stamp(limbX(s, k / n), limbY(s, k / n), s.trunk ? girth(s, k / n) / 2 : rad, s.trunk ? 13 : s.depth < 3 ? 5 : 1, s.trunk && k < n * .04);
  });
  thins = []; thinLength = 0;
  for (const [kind, w, xa, ya, xb, yb] of THINS) {
    const s = { kind, w: Math.max(.9, w * PH / 1100) * U, x1: px(xa, ya), y1: py(ya), x2: px(xb, yb), y2: py(yb) };
    s.len = Math.hypot(s.x2 - s.x1, s.y2 - s.y1); thinLength += s.len; s.at = thinLength; thins.push(s);
    for (let k = 0, n = Math.ceil(s.len / (mCell * .5)); k <= n; k++) stamp(s.x1 + (s.x2 - s.x1) * k / n, s.y1 + (s.y2 - s.y1) * k / n, s.w / 2 + .2 * U, 2);
  }
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes may start there.
const SKY = 0, LEAF = 1, WISP = 2, GRASS = 3, GRAVEL = 4, TOWER = 5, THIN = 6, LIMB = 7, THICKET = 8;
let zx = 0, zy = 0, zp = 0;                                    // what zone() learnt on the way: the point on the photo, the part of the tower
let tops = null;                                               // where the trees stand too close for any sky, column by column
function zone(x, y) {
  const pf = zy = fy(y), xf = zx = fx(x, y);
  if (xf > TB[0] && xf < TB[2] && pf > TB[1] && pf < TB[3]) {
    if (maskAt(x, y) & 2) return THIN;
    if ((zp = towerPart(xf, pf))) return TOWER;
  }
  const m = maskAt(x, y);
  if (m & 8) return LIMB;                                      // the trunk close by stands in front of everything but the tower
  if (pf > GROUND + (vnoise(xf * 90, 2.3) - .5) * .006) return grassy(xf, pf) ? GRASS : GRAVEL;
  const top = tops[x < 0 ? 0 : x >= W ? W | 0 : x | 0], low = y > top;
  if (bare) return m & 1 ? LIMB : low || y > top - Math.max(0, vnoise(x * .22 / U, y * .035 / U) - .4) * 55 * U ? THICKET : SKY;   // twigs stand up out of the wood
  const f = fol(x, y);
  if (low || (f > .22 && zk !== SHOOT)) return LEAF;
  if (f > 0) return WISP;                                      // at the rim of a crown the leaves are thin, and so are a young tree's
  if (m & 4) return LIMB;                                      // among leaves the twigs are not to be seen
  return SKY;
}
const GREEN = k => k === LEAF || k === WISP;
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

// ---------- leaves, twigs, limbs ----------
let leafFoot = 0;
function leafStroke() {                                        // somewhere above the ground
  const R = PP.R, x = R() * W, y = R() * leafFoot;
  if (!at(x, y)) return;
  const k = zone(x, y);
  if (k === LEAF || k === WISP) paintLeaf(x, y, k); else if (k === THICKET) paintThicket(x, y);
}
function paintLeaf(x, y, k) {
  const R = PP.R, xf = zx, pf = zy, near = close(xf, pf), sz = (.5 + near * .6) * U, q = R();
  veil(.6 - near * .5);
  if (k === WISP) {                                            // thin leaves with the sun in them
    const col = pal.accent && q > (season === 'spring' ? .55 : .8) ? pick(pal.accent, R()) : q < .45 ? pal.folLight : q < .75 ? pal.folSun : pal.folMid;
    PP.dab(x, y, (1.8 + R() * 2.6) * sz, col, { a: .9, den: .9, core: .65, s: 1.4 * U });
    return;
  }
  const l0 = lightAt(LEAF, xf - sway / PW, pf), t = clamp(.5 + (l0 + (R() - .5) * .1 - .5) * (.7 + .5 * sun));   // the wind moves the light about in them; without the sun there is less to tell light from shade
  let col = t < .28 ? mix(pal.folDeep, pal.folDark, t / .28) : t < .55 ? mix(pal.folDark, pal.folMid, (t - .28) / .27) : t < .82 ? mix(pal.folMid, pal.folLight, (t - .55) / .27) : mix(pal.folLight, pal.folSun, (t - .82) / .18 * (pal.accent ? .6 : .9));
  if (pal.accent && l0 > .3 && q > (season === 'spring' ? .88 : .93)) col = pick(pal.accent, R());
  const sd = shadow(x, y); if (sd) col = mix(col, pal.folDeep, sd * .4);
  const a = R() * Math.PI, ca = Math.cos(a), sa = Math.sin(a), l = (2.5 + R() * 5) * sz;
  run(x, y, ca, sa, l, GREEN, Math.max(1.5, 2 * sz));
  const r0 = reach[0], r1 = reach[1];
  PP.brush(x - ca * r0, y - sa * r0, x + ca * r1, y + sa * r1, room(x, y, -sa, ca, (3 + R() * 4.5) * sz, GREEN), col, { a: .87, sp: 1.25 * U, dry: .18, jit: 9 });
}
function paintThicket(x, y) {                                  // the bare wood: trunks and twigs, what light comes through them
  const R = PP.R, top = tops[x | 0], depth = clamp((y - top) / Math.max(1, leafFoot - top)), q = R();
  veil(.6);
  if (snow && q < .2) { PP.dab(x, y, (2 + R() * 3.5) * U, pal.snow, { a: .9, den: .6, core: .5, sx: 1.7, sy: .5 }); return; }
  if (q < .42) {
    PP.dab(x, y, (3 + R() * 5) * U, mix(mix(pal.skyLow, pal.haze, .5), pal.twigs[1], .3 + depth * .45), { a: .8, den: .5, core: .4, s: 2.2 * U });
    return;
  }
  const a = -Math.PI / 2 + (R() - .5) * (q < .72 ? .5 : 2.2), l = Math.min((6 + R() * 14) * U, (y - top) * 1.2 + 5 * U), ca = Math.cos(a) * l, sa = Math.sin(a) * l;
  PP.line([[x, y], [x + ca * .5 + (R() - .5) * 3 * U, y + sa * .5], [x + ca, y + sa]], ramp(pal.twigs, R() * .75 + (1 - depth) * .2), (.8 + R() * 1.2) * U, { pass: 1, a: .8 });
}
function limbStroke() {                                        // a stretch of bark
  const R = PP.R, k = R() * limbWeight;
  let a = 0, b = limbs.length - 1;
  if (b < 0) return;
  while (a < b) { const m = (a + b) >> 1; if (limbSum[m] < k) a = m + 1; else b = m; }
  const s = limbs[a], span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span, wide = s.trunk ? girth(s, tm) : s.w, side = s.trunk ? (R() - .5) * wide * .96 : 0;
  const mx = limbX(s, tm) + side, my = limbY(s, tm);
  if (mx < 0 || my < 0 || mx >= W || my >= H || !at(mx, my) || zone(mx, my) !== LIMB) return;   // not where leaves or the tower hide it
  const bark = s.trunk ? mix(pal.trunk, pal.dark, .25 + R() * .3) : mix(pal.trunk, pal.towerLight, s.depth < 2 ? .2 : .08), q = R();
  if (s.trunk) {                                               // furrowed bark, lighter towards the sun
    veil(.05);
    const lum = side / wide + .15 + (R() - .5) * .3;
    PP.brush(limbX(s, t0) + side, limbY(s, t0), limbX(s, t1) + side, limbY(s, t1), (2.5 + R() * 3) * U, snow && lum < -.2 && R() < .5 ? pal.snow : lum > 0 ? mix(bark, pal.towerLight, lum * .5) : mix(bark, pal.ink, -lum), { a: .93, jit: 8, dry: .1, sp: 1.3 * U, bow: .04 });
    return;
  }
  veil(s.near ? .1 : .55);
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (const [u, v] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, u), ay = limbY(s, u), bx = limbX(s, v), by = limbY(s, v);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (snow) { if (s.w > 1.3 * U && q < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snow, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax + s.w * .22, ay, bx + s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .5), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });   // the sunny side
  }
}

// ---------- the clearing ----------
let groundTop = 0;
const nearG = pf => clamp((pf - .68) / .3, .1, 1.2);           // how close the ground is at this height: the size of its strokes
const lit = t => .65 + (t - .65) * sun;                        // under a grey sky the trees throw no shade
function groundStroke() {
  const R = PP.R, x = R() * W, y = groundTop + R() * (H - groundTop);
  if (!at(x, y)) return;
  const k = zone(x, y);
  if (k === GRASS) paintGrass(x, y); else if (k === GRAVEL) paintGravel(x, y);
}
function paintGravel(x, y) {                                   // gravel, and the shade of the leaves lying on it
  const R = PP.R, pf = zy, sz = (.3 + nearG(pf)) * U, l0 = lightAt(GRAVEL, zx, pf), q = R(), sd = shadow(x, y);
  veil(.5 - nearG(pf) * .4);
  let col = ramp(pal.gravels, lit(l0) + (R() - .5) * .12);
  if (sd) col = mix(col, pal.gravels[0], sd * .35);
  if (q < .16) { PP.dab(x, y, (.8 + R() * 1.3) * sz, mix(col, q < .08 ? pal.gravels[5] : pal.gravels[1], .35), { a: .8, den: 1, core: .5, s: 1.2 * U }); return; }   // stones
  if (season === 'autumn' && q > .975) { PP.dab(x, y, (1.3 + R() * 1.5) * sz, pick(pal.bits, R()), { a: .9, den: 1.4, core: .8, s: 1.3 * U }); return; }   // leaves
  const a = (R() - .5) * .5, ca = Math.cos(a), sa = Math.sin(a), l = (3 + R() * 8) * sz;
  run(x, y, ca, sa, l, z => z === GRAVEL && Math.abs(lightAt(GRAVEL, zx, zy) - l0) < .25, Math.max(1.5, 1.6 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (1.4 + R() * 2.2) * sz, col, { a: .87, jit: 7, dry: .14, sp: 1.1 * U, bow: .03 });
}
function paintGrass(x, y) {                                    // grass nobody mows, every blade upright
  const R = PP.R, pf = zy, sz = (.3 + nearG(pf) * .8) * U, l0 = lightAt(GRASS, zx, pf), q = R(), sd = shadow(x, y);
  veil(.5 - nearG(pf) * .4);
  let col = ramp(pal.meadow, lit(l0) + (R() - .5) * .12);
  if (l0 > .35) { const dry = smooth(.5, .7, fbm(x * .006 / U + 11, y * .012 / U)); if (dry > 0) col = mix(col, pal.straw, dry * .3); if (q > .92) col = mix(col, pal.straw, .4); }
  if (sd) col = mix(col, pal.meadow[0], sd * .45);
  const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .07, ca = Math.cos(a), sa = Math.sin(a), l = (2 + R() * 4.5) * sz;
  run(x, y, ca, sa, l, z => z === GRASS || z === GRAVEL, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (1.3 + R() * 2.3) * sz, col, { a: .87, jit: 8, dry: .18, sp: 1.1 * U, bow: .12 });
}

// ---------- the tower, stone by stone ----------
let TX0 = 0, TX1 = 0, TY0 = 0, TY1 = 0;                        // the box it stands in, in the picture
const CHF = .0062;                                             // how high a course of its stones is
const isTower = m => z => z === TOWER && zp === m;
function towerStroke() {
  const R = PP.R, x = TX0 + R() * (TX1 - TX0), y = TY0 + R() * (TY1 - TY0);
  if (!at(x, y) || zone(x, y) !== TOWER) return;
  const m = zp, xf = zx, pf = zy, lt = tLt, round = tRound, dir = tDir, big = tBig, c = tC, a = tA, u = tU;
  const o = { a: .94, jit: 4, dry: .04, bow: 0, sp: 1.1 * U }, sd = shadow(x, y);
  const tone = col => { if (lt < 1) col = mix(col, pal.stoneShade, (1 - lt) * .85); return sd ? mix(col, pal.stoneShade, sd * .3) : col; };
  veil(.3);
  if (m === ROUGH || m === STEP) {                             // every stroke is a stone in its course
    const ch = big ? CHF * 1.5 : CHF, i = Math.floor(c / ch), cc = (i + .5) * ch, h = hash1(i * 131 + 7), wd = Math.max(1.3 * U, ch * PH * .86);
    let xa, ya, xb, yb, j;
    if (round) {
      const bl = ch * (2 + h) / (turW(pf) * .75); j = Math.floor(a / bl + h);
      const a0 = Math.max(-1.5, (j - h) * bl), a1 = Math.min(1.5, a0 + bl), p = turretPt(a0 + .06 * bl, cc), e = turretPt(a1 - .06 * bl, cc);
      xa = p[0]; ya = p[1]; xb = e[0]; yb = e[1];
    } else {
      const bl = ch * (big ? 3.4 : 2.9) * (.8 + h * .5); j = Math.floor(a / bl + h);
      const lean = big ? 0 : (cc - .5) * .13;
      xa = (j - h) * bl + lean + .0008; xb = xa + bl - .0016; ya = yb = cc;
    }
    const ok = (ex, ey) => towerPart(ex, ey) === m, X1 = px(xa, ya), Y1 = py(ya), X2 = px(xb, yb), Y2 = py(yb);
    if (!ok(xa, ya)) { xa = (xa + xf) / 2; ya = (ya + pf) / 2; if (!ok(xa, ya)) { xa = xf; ya = round ? pf : cc; } }
    if (!ok(xb, yb)) { xb = (xb + xf) / 2; yb = (yb + pf) / 2; if (!ok(xb, yb)) { xb = xf; yb = round ? pf : cc; } }
    let col = pal.stones[(hash1(i * 131 + j * 17) * pal.stones.length) | 0];
    col = mix(col, pal.stoneStain, smooth(.52, .8, fbm(xf * 46 + 3, c * 60)) * .38);   // weather
    if (m === STEP) col = mix(col, pal.stoneStain, .25);
    // A stone is one stroke where there is room for all of it.  Where something stands in front of a part of it (the
    // table, the weeds at the foot of the wall, the plaque, a dish) it is put in bit by bit, round that.
    const x1 = px(xa, ya), y1 = py(ya), x2 = px(xb, yb), y2 = py(yb), e = ch * .38;
    let whole = x2 - x1 >= 1.2 * U;
    for (let k = 0, n = Math.ceil((x2 - x1) / (3 * U)); k <= n && whole; k++) { const ex = xa + (xb - xa) * k / n, ey = ya + (yb - ya) * k / n; whole = ok(ex, ey - e) && ok(ex, ey) && ok(ex, ey + e); }
    col = tone(col);
    if (!whole) {
      const d = Math.hypot(X2 - X1, Y2 - Y1), ca = (X2 - X1) / d, sa = (Y2 - Y1) / d, mine = isTower(m);
      const t = (x - X1) * ca + (y - Y1) * sa, w = room(x, y, -sa, ca, wd * .5, mine);
      run(x, y, ca, sa, (3 + R() * 5) * U, mine, 1.5 * U);
      const r0 = Math.min(reach[0], t), r1 = Math.min(reach[1], d - t);   // and no further than the stone goes
      if (r0 + r1 < U) return;
      o.sp = Math.min(1.1 * U, w);
      PP.brush(x - ca * r0, y - sa * r0, x + ca * r1, y + sa * r1, w, col, o);
      return;
    }
    o.sp = Math.min(1.1 * U, wd);
    PP.brush(x1, y1, x2, y2, wd, col, o);
    if (R() < .3) PP.brush(x1, y1 + wd * .55, x2, y2 + wd * .55, .7 * U, tone(pal.joint), { a: .65, jit: 4, dry: .1, bow: 0, sp: .7 * U });   // the shade in the joint under it
    thinsOver(x1 - 2 * U, Math.min(y1, y2) - wd * .5 - 2 * U, x2 + 2 * U, Math.max(y1, y2) + wd * .7 + 2 * U);
    return;
  }
  if (m === ASHLAR || m === RELIEF) {                          // smooth stone: long level strokes, a joint now and then
    const q = R();
    if (m === RELIEF && q < .3) {                              // what is carved into it: a shield, leaves around it
      const dx = (xf - .6225) * .75, dy = pf - .517, on = Math.abs(Math.hypot(dx, dy * .8) - .0105) < .002;
      PP.dab(x, y, (on ? 1.2 : 1.6) * U, tone(mix(pal.ashlar, pal.joint, on ? .8 : .45)), { a: .85, den: 1.4, core: .6, s: U, jit: 4 });
      return;
    }
    run(x, y, 1, 0, (4 + R() * 9) * U, isTower(m), 2 * U);
    const n = pf / .0195, joint = n - Math.floor(n) < .1;
    PP.brush(x - reach[0], y, x + reach[1], y, (joint ? 1.1 : 2 + R() * 1.6) * U, tone(joint ? mix(pal.ashlar, pal.joint, .4) : mix(pal.ashlar, pal.stones[(q * 6) | 0], .35)), o);
    return;
  }
  if (m === MOULD) {                                           // ledges, bands, the frame of the gable: along the way they run
    let ca = 1, sa = 0;
    if (dir === 2 || dir === 3) { const e = dir === 2 ? GL : GR; ca = px(e[0], e[1]) - px(GA[0], GA[1]); sa = (e[1] - GA[1]) * PH; }
    else if (dir === 4) { ca = -(pf - .6) * PH; sa = x - px(.6275, .6); if (pf > .59) { ca = 0; sa = 1; } }
    else if (round) { const p = turretPt(a - .05, c), e = turretPt(a + .05, c); ca = px(e[0], e[1]) - px(p[0], p[1]); sa = (e[1] - p[1]) * PH; }
    const q = 1 / (Math.hypot(ca, sa) || 1); ca *= q; sa *= q;
    if (dir === 5 && R() < .3) { PP.dab(x, y, 1.3 * U, tone(mix(pal.ledge, pal.joint, .7)), { a: .85, den: 1.5, core: .6, s: U, jit: 4 }); return; }   // leaves carved into a capital
    run(x, y, ca, sa, (3 + R() * 6) * U, isTower(MOULD), 1.5 * U);
    const white = snow && dir < 4 && R() < .7;                 // snow lies on it
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], 1.7 * U, white ? pal.snow : tone(mix(pal.ledge, pal.stones[2], R() * .4)), o);
    return;
  }
  if (m === DARK || m === SHADE || m === GRILLE) {
    if (m === GRILLE && R() < .3) {                            // the bars in the arch over the door
      const up = R() < .5, l = (2 + R() * 3) * U, gx = up ? (Math.round(x / (3.2 * U)) * 3.2 * U) : x, gy = up ? y : Math.round(y / (3.2 * U)) * 3.2 * U;
      run(gx, gy, up ? 0 : 1, up ? 1 : 0, l, isTower(GRILLE), 1.5 * U);
      PP.brush(gx - (up ? 0 : reach[0]), gy - (up ? reach[0] : 0), gx + (up ? 0 : reach[1]), gy + (up ? reach[1] : 0), .8 * U, night ? mix(pal.iron, pal.lights[1], .5) : mix(pal.iron, pal.towerLight, .45), { a: .8, jit: 4, dry: 0, bow: 0, sp: .8 * U });
      return;
    }
    run(x, y, 1, 0, (2 + R() * 4) * U, isTower(m), 1.5 * U);
    let col = m === SHADE ? mix(pal.stoneShade, pal.stones[2], lt * .45) : mix(pal.opening, pal.dark, R() * .3);
    if (m === GRILLE && night) col = mix(col, pal.lights[1], .22);   // a lamp burns in the porch
    PP.brush(x - reach[0], y, x + reach[1], y, 1.6 * U, col, o);
    return;
  }
  if (m === DOOR) {                                            // a door of dark wood, studded
    if (R() < .08) { PP.dab(x, y, .9 * U, mix(pal.door, pal.towerLight, .4), { a: .9, den: 2, core: .8, s: .8 * U, jit: 3 }); return; }
    const l = (3 + R() * 6) * U, cx = (Math.floor(x / (2.4 * U)) + .5) * 2.4 * U;
    run(cx, y, 0, 1, l, isTower(DOOR), 1.5 * U);
    PP.brush(cx, y - reach[0], cx, y + reach[1], 2 * U, mix(pal.door, night ? pal.lights[1] : pal.opening, night ? .15 : hash1(Math.floor(x / (2.4 * U))) * .5), o);
    return;
  }
  if (m === BOARD) {                                           // the one window with a board in it
    const l = (1.5 + R() * 3) * U;
    run(x, y, 0, 1, l, isTower(BOARD), U);
    PP.brush(x, y - reach[0], x, y + reach[1], 1.5 * U, night ? pick(pal.lights, R()) : tone(mix(pal.board, pal.stones[4], R() * .3)), o);
    return;
  }
  if (m === DISH || m === GREY) {                              // dishes, the dome, the mast, the tubs on the balcony
    const up = dir === 1, l = (2 + R() * 4) * U;
    let col = m === DISH ? mix(pal.dishShade, pal.dish, clamp(lt * 1.25)) : mix(pal.dark, pal.zinc, clamp(lt));
    if (m === DISH && u > .88) col = mix(col, pal.dishShade, .45);     // its rim
    if (sd) col = mix(col, pal.stoneShade, sd * .25);
    if (m === DISH || !up && u) { PP.dab(x, y, 2.4 * U, u > .85 && m === GREY ? mix(col, pal.dark, .3) : col, { a: .95, den: 1.5, core: .85, s: 1.2 * U, jit: 3 }); return; }   // what is round is dabbed on
    run(x, y, up ? 0 : .94, up ? 1 : .34, l, isTower(m), 1.5 * U);
    PP.brush(x - (up ? 0 : .94) * reach[0], y - (up ? 1 : .34) * reach[0], x + (up ? 0 : .94) * reach[1], y + (up ? 1 : .34) * reach[1], (up ? 2.2 : 1.8) * U, col, o);
    return;
  }
  if (m === TABLE) {                                           // weathered boards
    const up = dir === 1, l = (3 + R() * 7) * U;
    run(x, y, up ? 0 : 1, up ? 1 : 0, l, isTower(TABLE), 1.5 * U);
    const col = snow && !up && R() < .6 ? pal.snow : mix(pal.wood, lt < .6 ? pal.dark : pal.stones[0], lt < .6 ? .55 : R() * .2);
    PP.brush(x - (up ? 0 : reach[0]), y - (up ? reach[0] : 0), x + (up ? 0 : reach[1]), y + (up ? reach[1] : 0), 1.6 * U, sd ? mix(col, pal.stoneShade, sd * .3) : col, o);
  }
}
function thinStroke() {                                        // a stretch of a railing, of an aerial
  const R = PP.R, k = R() * thinLength;
  let a = 0, b = thins.length - 1;
  while (a < b) { const m = (a + b) >> 1; if (thins[m].at < k) a = m + 1; else b = m; }
  const s = thins[a], span = Math.min(1, (6 + R() * 10) * U / s.len), t0 = R() * (1 - span), t1 = t0 + span, dx = s.x2 - s.x1, dy = s.y2 - s.y1;
  const mx = s.x1 + dx * (t0 + t1) / 2, my = s.y1 + dy * (t0 + t1) / 2;
  if (mx < 0 || my < 0 || mx >= W || my >= H || !at(mx, my) || zone(mx, my) !== THIN) return;
  veil(.3);
  thinPiece(s, t0, t1);
}
function thinPiece(s, t0, t1) {
  const R = PP.R, dx = s.x2 - s.x1, dy = s.y2 - s.y1;
  const col = s.kind === IRON ? mix(pal.iron, pal.dark, R() * .3) : s.kind === STEEL ? mix(pal.steel, pal.zinc, R() * .5) : snow && R() < .3 ? pal.snow : mix(pal.aerial, pal.zinc, R() * .5);
  PP.brush(s.x1 + dx * t0, s.y1 + dy * t0, s.x1 + dx * t1, s.y1 + dy * t1, s.w, col, { a: .93, jit: 4, dry: 0, bow: 0, sp: Math.min(U, s.w) });
}
function thinsOver(x0, y0, x1, y1) {                           // a stone has been painted behind railings: they go back in front of it at once
  for (const s of thins) {
    if (Math.max(s.x1, s.x2) < x0 || Math.min(s.x1, s.x2) > x1 || Math.max(s.y1, s.y2) < y0 || Math.min(s.y1, s.y2) > y1) continue;
    const dx = s.x2 - s.x1, dy = s.y2 - s.y1;
    let t0 = 0, t1 = 1;                                        // the stretch of it that lies in the box
    if (dx) { const a = (x0 - s.x1) / dx, b = (x1 - s.x1) / dx; t0 = Math.max(t0, Math.min(a, b)); t1 = Math.min(t1, Math.max(a, b)); }
    if (dy) { const a = (y0 - s.y1) / dy, b = (y1 - s.y1) / dy; t0 = Math.max(t0, Math.min(a, b)); t1 = Math.min(t1, Math.max(a, b)); }
    if (t1 > t0) thinPiece(s, t0, t1);
  }
}
function lampDab() {                                           // by night: the red light on the mast, the lamp in the porch
  const R = PP.R, porch = R() < .5, x = porch ? px(.628, .6) : px(mastX(.214), .214), y = py(porch ? .6 : .214);
  if (x < 0 || y < 0 || x >= W || y >= H || !at(x, y) || !night) return;
  PP.haze = null;                                              // a light is seen through any fog, and all the wider for it
  if (porch) { PP.dab(x, y + 6 * U, (9 + traits.fog * 5) * U, pal.lights[0], { a: .09, den: .4, s: 2.4 * U, jit: 3 }); light(x, y, 1.1 * U); return; }
  PP.dab(x, y, (5 + traits.fog * 4) * U, pal.beacon, { a: .12, den: .5, s: 2.2 * U, jit: 3 });
  PP.dab(x, y, 1.5 * U, pal.beacon, { a: 1, den: 3, core: 1, s: U, jit: 3 });
}
function fogStroke() {                                         // fog hangs between the trees, in banks that drift
  const R = PP.R, x = R() * W, depth = R(), y = py(.56 + depth * .18);
  if (!at(x, y) || !traits.fog) return;
  const z = zone(x, y);
  if (z === TOWER || z === THIN || fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.haze = null;
  PP.dab(x, y, (8 + R() * 16) * U, fogCol(), { a: .42, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}

// ---------- the coat ----------
function towerPath(c) { c.moveTo(OUT[0][0], OUT[0][1]); for (let i = 1; i < OUT.length; i++) c.lineTo(OUT[i][0], OUT[i][1]); c.closePath(); }
let lagCells = null, lagAt = 0;                                // the cells of the coat that is being brushed, how many of them have had their second one
function coatStroke(x0, y0, tower, again) {                    // a fresh coat: broad strokes of what is there, where the brush comes by.  Nothing of the wash under it is to show through
  const R = PP.R;
  for (let n = 0; n < 9; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .5) * 21 * U, y = y0 + ((n / 3 | 0) - 1 + (R() - .5) * .5) * 13 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U || y < -12 * U) continue;
    const k = zone(clamp(x, 0, W - 1), clamp(y, 0, H - 1)), pf = zy, l = (22 + R() * 10) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };   // long enough that the ragged ends of two strokes overlap
    if (tower) {                                               // the tower: the colour of its joints, the stones come after
      if (k !== TOWER) continue;
      const m = zp, col = m === DARK || m === DOOR || m === GRILLE ? pal.opening : m === DISH ? pal.dish : m === GREY ? pal.zinc : mix(mix(pal.joint, pal.stones[2], .6), pal.stoneShade, (1 - tLt) * .8);
      veil(.3); PP.brush(x - l * .6, y, x + l * .6, y, 16 * U, col, o);
      continue;
    }
    if (again && (k !== LEAF || y > tops[clamp(x | 0, 0, tops.length - 1)] + 4 * U)) continue;   // the second time round: only leaves the sky's coat may have got at
    if (k === LIMB && maskAt(x, y) & 8) { veil(.05); PP.brush(x, y - l * .7, x, y + l * .7, 7 * U, mix(pal.trunk, pal.dark, .4), o); continue; }   // the trunk close by
    if (k === SKY || k === TOWER || k === THIN || k === LIMB || k === WISP) continue;
    if (k === LEAF) {
      const near = close(zx, pf), a = (R() - .5) * 1.2, col = mix(pal.folDark, pal.folMid, .15 + clamp(lightAt(LEAF, zx, pf)) * .63), wd = room(x, y, -Math.sin(a), Math.cos(a), 26 * U, GREEN), ll = wd < 26 * U ? l * .4 : l * .9;
      veil(.6 - near * .5);
      PP.brush(x - Math.cos(a) * ll, y - Math.sin(a) * ll, x + Math.cos(a) * ll, y + Math.sin(a) * ll, wd, col, o);
    } else if (k === THICKET) { veil(.6); PP.brush(x - l, y, x + l, y, 18 * U, mix(mix(pal.skyLow, pal.haze, .5), pal.twigs[1], .55), o); }
    else { veil(.5 - nearG(pf) * .4); PP.brush(x - l, y, x + l, y, 18 * U, ramp(k === GRASS ? pal.meadow : pal.gravels, lit(lightAt(k, zx, pf))), o); }
  }
}

// ---------- what moves ----------
// Jackdaws live in the tower and wheel round its top when the weather is fit for it.  By night the light on its mast blinks.
function drawLife(P, t) {
  const tx = px(mastX(.214), .214), ty = py(.214);
  if (!at(clamp(tx, 0, W - 1), clamp(ty, 0, H - 1))) return;
  if (night) {
    const a = Math.pow(.5 + .5 * Math.sin(t * 2.4), 3);
    P.dab(tx, ty, 9 * U, pal.beacon, { a: .22 * a, den: .5, s: 2.6 * U, jit: 2 });
    P.dab(tx, ty, 2 * U, pal.beacon, { a, den: 3, core: a, s: U, jit: 2 });
    return;
  }
  if (traits.rain || traits.snow || traits.fog) return;
  for (let i = 0; i < 4; i++) {
    const ph = i * 1.9, w = .42 + i * .06, a = t * w + ph, far = Math.sin(a) < 0, s = (far ? 4 : 5.8) * U * clamp(PH / 1100, .7, 1.4);
    const pf = .33 + Math.sin(a) * .035 + Math.sin(t * .47 + ph * 2) * .03 - (far ? .03 : 0), x = px(.6 + Math.cos(a) * (.17 + .03 * Math.sin(t * .31 + ph)), pf), y = py(pf);
    if (x < 0 || y < 0 || x >= W || y >= H || !at(x, y)) continue;
    const glide = Math.sin(t * .9 + ph) > .3, fl = glide ? -s * .25 : Math.sin(t * 11 + ph) * s * .7, col = mix(pal.ink, pal.dark, .5), o = { a: .9, sp: 1.1 * U, dry: 0, jit: 4, bow: .25 };
    P.R = rng(8200 + i);                                       // each keeps its own shape, whoever else is about
    P.brush(x - s, y - fl, x, y, 1.9 * U, col, o);
    P.brush(x, y, x + s, y - fl, 1.9 * U, col, o);
    P.dab(x, y + .3 * U, 1.2 * U, col, { a: .95, den: 2, core: .9, s: U, jit: 3 });
  }
}

// ---------- the painting, one frame's worth ----------
const RATE = { leaf: 11000, ground: 10000, tower: 20000, thin: 1.6, limb: 1.2, lamp: 5, fog: 1500 };
const place = {
  colors: {
    stones: ['#efe8d6', '#e9e0cb', '#e0d6bf', '#f3edde', '#d9cfb8', '#ece4d0'], stoneStain: '#a89f8b', stoneShade: '#63636a', joint: '#7d7566',
    ashlar: '#f0eadb', ledge: '#f7f2e6', opening: '#1c1e24', iron: '#2a2c31', door: '#2c2828', board: '#dfe2e0',
    dish: '#f8f8f4', dishShade: '#939da8', zinc: '#aab1b6', steel: '#8492a8', aerial: '#e6eaec', wood: '#857b6b',
    gravels: ['#2f3237', '#484a4a', '#6f6d67', '#a09a8b', '#cbc2ae', '#e4dcc8'],
    meadow: ['#1d381c', '#365e2a', '#547f38', '#779944', '#a2b85a'], straw: '#b9ab62',
    twigs: ['#2b2724', '#463e37', '#685e53', '#8b8073'], snow: '#eef1f2',
  },
  seasons: {
    spring: { meadow: ['#24481f', '#487c2e', '#72a83c', '#9cc64e', '#c6de70'], straw: '#b9c66a' },
    autumn: { meadow: ['#3d3320', '#6f602c', '#9c8838', '#c0a84c', '#e0cc74'], straw: '#d6b45a' },
    // Winter by itself is bare and dull, not white: the grass has gone the colour of straw.
    winter: { meadow: ['#35382a', '#5a6243', '#878c5a', '#aeab76', '#c9c296'], straw: '#bcab80' },
  },
  weathers: {                                                  // white is what snow makes it, and frost a little
    snow: { meadow: ['#8597ae', '#aebccc', '#d0d9e2', '#e9edf1', '#ffffff'], straw: '#ffffff', gravels: ['#77869b', '#9aa8ba', '#bdc8d4', '#d9e0e7', '#eef1f3', '#ffffff'] },
    frost: { meadow: ['#4c5648', '#74806c', '#a0ab97', '#c9d1c4', '#e6ebe5'], straw: '#d5d6c4', gravels: ['#3a3e45', '#5b5e60', '#888984', '#b7b6ad', '#dad8cf', '#f1f0ea'] },
  },
  ground: /^(gravels|meadow|straw|snow)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const wide = clamp((W / H - .75) / .55), y1 = 1 - .13 * wide;   // a wide screen loses some sky and the shade at its feet
    Y0 = .15 * wide; PH = H / (y1 - Y0); PW = PH * .75;
    X0 = .645 - (.5 + .14 * wide) * W / PW;                    // the tower stands right of the middle
    if (W < PW) X0 = clamp(X0, .02, 1.04 - W / PW);
    place.hz = py(GROUND);
    leafFoot = Math.min(H, py(GROUND) + 8 * U); groundTop = Math.max(0, py(GROUND) - 8 * U);
    TX0 = Math.max(0, px(TB[0], TB[3])); TX1 = Math.min(W, px(TB[2], TB[1])); TY0 = Math.max(0, py(TB[1])); TY1 = Math.min(H, py(TB[3]));
    const n = Math.ceil(W) + 2;
    tops = new Float32Array(n);
    for (let i = 0; i < n; i++) tops[i] = py(dense(X0 + i / PW + LEAN * (.61 - FOOT)));
    OUT = outline().map(p => [px(p[0], p[1]), py(p[1])]);
    const above = p => p[1] < tops[clamp(p[0] | 0, 0, n - 1)];
    skyA = 0; while (skyA < OUT.length - 1 && !above(OUT[skyA])) skyA++;
    skyB = OUT.length - 1; while (skyB > skyA && !above(OUT[skyB])) skyB--;
    buildWood(); buildTrees();
  },
  tick(t) { time = t; sway = wind(t) * 1.5 * U; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to where the wood is dense, and round the tower
    const dip = 3 * U - pad, n = tops.length, g = x => tops[x < 0 ? 0 : x >= n ? n - 1 : x | 0] + dip;
    let x = W + 1;
    c.moveTo(-1, -60); c.lineTo(x, -60); c.lineTo(x, g(x));
    if (OUT[skyB][0] > 0 && OUT[skyA][0] < W) {
      for (x -= 3; x > OUT[skyB][0]; x -= 3) c.lineTo(x, g(x));
      for (let i = skyB; i >= skyA; i--) c.lineTo(OUT[i][0], OUT[i][1]);
      x = Math.min(x, OUT[skyA][0]);
    }
    for (x -= 3; x > 0; x -= 3) c.lineTo(x, g(x));
    c.lineTo(-1, g(0)); c.closePath();
  },
  front(s, x, y) { use(s); return zone(x, y) !== SKY; },       // leaves, limbs, dishes and aerials stand in front of the sky
  orb(o) {                                                     // the sun and the moon stand left of the tower, where the sky is open
    o.x = clamp(px(.3, .2), o.r * 1.6, W - o.r * 1.6);
    o.y = clamp(o.y * .8, Math.max(o.r * 1.8, py(.1)), py(.34));
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    spend('hw-leaf', RATE.leaf * W * leafFoot / ua * pace, leafStroke);
    spend('hw-ground', RATE.ground * W * (H - groundTop) / ua * pace, groundStroke);
    spend('hw-tower', RATE.tower * Math.max(0, TX1 - TX0) * Math.max(0, TY1 - TY0) / ua * pace, towerStroke);
    spend('hw-thin', RATE.thin * thinLength / (10 * U) * pace, thinStroke);
    spend('hw-limb', RATE.limb * limbLength / (14 * U) * pace, limbStroke);
    if ((s && s.night) || (o && o.night)) spend('hw-lamp', RATE.lamp * pace, lampDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('hw-fog', RATE.fog * W * .18 * PH / ua * pace, fogStroke);
    PP.haze = null;
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); towerPath(c); c.clip('evenodd');
    for (let k = i; k < j; k++) coatStroke(cells[k].x, cells[k].y, false);
    // The sky's coat is broad and long: what it brushes later reaches back over leaves this one has just put in front
    // of it.  They get a second coat once the sky's has passed for good.
    if (cells !== lagCells) { lagCells = cells; lagAt = 0; }
    const n = cells.length, passed = j >= n ? Infinity : j > 0 ? cells[j - 1].k - (cells[n - 1].k - cells[0].k) / 6 - 170 * U : -Infinity;
    for (; lagAt < n && cells[lagAt].k <= passed; lagAt++) coatStroke(cells[lagAt].x, cells[lagAt].y, false, true);
    c.restore();
    c.save(); c.beginPath(); towerPath(c); c.clip();           // the tower, and none of it beyond its walls
    for (let k = i; k < j; k++) if (cells[k].x > TX0 - 40 * U && cells[k].x < TX1 + 40 * U && cells[k].y > TY0 - 40 * U) coatStroke(cells[k].x, cells[k].y, true);
    c.restore();
    PP.haze = null;
  },
  sprites: [{ draw: drawLife, t0: 4, seed: 141 }],
};
return place;
} });
