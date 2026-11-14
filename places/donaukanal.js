/* ------------------------------------------------------------------ *
 *  Vienna, on the Donaukanal: from the Urania across the water to the
 *  glass tower at the head of the Praterstraße.  The Aspernbrücke comes
 *  in from the left, the walls along the far quay are sprayed from end
 *  to end, and the canal below is as green as ever.  scene.js paints
 *  the sky above it and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'donaukanal', name: 'Donaukanal', make(kit) {
'use strict';
const { PP, clamp, smooth, ease, mix, pick, rng, hash1, vnoise, fbm, wind, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', white = 0, sun = false, warm = false;   // white: how much snow lies (frost is a little of it); sun: whether it throws shadows; warm: it is low, on the left
const use = s => { pal = s.pal; traits = s.traits; night = s.night; season = s.season; white = traits.snow ? 1 : s.weather === 'frost' ? .4 : 0; sun = !night && traits.shadow > 0; warm = sun && s.weather === 'golden'; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph the picture is painted after: x and y as fractions
// of its width and height.  It is an upright one: a narrow screen shows a slice of its width,
// a wide one a slice of its height, and what lies to the left and right of it as well.
let X0 = 0, Y0 = 0, X1 = 1;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along2(pts, x) {                                      // a line through points, eased from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * ease((x - a[0]) / (b[0] - a[0])); }
  return pts[pts.length - 1][1];
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
function nearSeg(x, y, ax, ay, bx, by) { const dx = bx - ax, dy = by - ay, t = clamp(((x - ax) * dx + (y - ay) * dy) / (dx * dx + dy * dy || 1)); return Math.hypot(x - ax - dx * t, y - ay - dy * t); }

// We stand a little above the street.  The canal runs away to the left: whatever lies along it runs towards
// a point far off on that side.  Such a line is known by the height at which it leaves the photo on the right.
const HOR = .6, SKYLINE = .53, CVX = -2.82;
const lineY = (c, xf) => HOR + (c - HOR) * (xf - CVX) / (1 - CVX);   // how high that line is at x
const lineOf = (xf, pf) => HOR + (pf - HOR) * (1 - CVX) / (xf - CVX);   // which line a point lies on
const near = xf => (xf - CVX) / (1 - CVX);                     // how big things on the far bank are at x
const STREET = .594, FOOT = .686, COPING = .715, WATERLINE = .757;   // the street up there; the foot of the upper wall, the edge of the promenade, the water
const WALLTOP = [[.3, .612], [.54, .608], [.68, .615], [.8, .634], [1, .638]];
const wallTop = xf => xf < .3 ? HOR + .012 * (xf - CVX) / (.3 - CVX) : xf > 1 ? HOR + .038 * near(xf) : along2(WALLTOP, xf);
// The bridge comes towards us on the left: its lines run to a point behind the tower.  One of them is known by
// its height at the left edge of the photo.
const BVX = .64, BVY = .603;
const bridgeY = (v, xf) => BVY + (v - BVY) * (BVX - xf) / BVX;
const bridgeOf = (xf, pf) => BVY + (pf - BVY) * BVX / (BVX - xf);
const FARRAIL = .618, RAIL = .637, DECKV = .652, ABUT = .45, RAILEND = .54;   // its far railing, the top of the near one, the top of the girder; where the girder and the railing end
const ZM = 2.25, ZF = 3.57;                                    // how far along it the girder is thinnest, and where it meets the far bank
function depthAt(xf) {                                         // how deep the girder is: an arch, flat in the middle
  if (xf >= .36) return .03;
  const z = 1 / (BVX - xf);
  return z > ZM ? .025 + .037 * ((z - ZM) / (ZF - ZM)) ** 2 : .025 + .02 * Math.pow((ZM - z) / ZM, 1.3);
}
const cdir = [1, 0], bdir = [1, 0];                            // the way the canal's lines run at a point, and the bridge's
function canalDir(c) { const s = (c - HOR) / (1 - CVX) * PH / PW, k = 1 / Math.hypot(1, s); cdir[0] = k; cdir[1] = s * k; }
function bridgeDir(v) { const s = -(v - BVY) / BVX * PH / PW, k = 1 / Math.hypot(1, s); bdir[0] = k; bdir[1] = s * k; }

// ---------- the tower ----------
// A slab that leans, a round body of glass beside it, a dark slot between the two.  The slab stands on a
// slanting leg and a column, a hall of glass behind them.
const SLAB = { crown: .126, top: .151, foot: .508, l0: .579, l1: .615, r0: .674, r1: .685 }, FH = .021;
const slabL = pf => SLAB.l0 + (SLAB.l1 - SLAB.l0) * (pf - SLAB.top) / (SLAB.foot - SLAB.top);
const slabR = pf => SLAB.r0 + (SLAB.r1 - SLAB.r0) * (pf - SLAB.top) / (SLAB.foot - SLAB.top);
const CL0 = .705, CL1 = .686, CR0 = .872, CR1 = .881, SLOT = .158, BASE = .52;
const CYL_TOP = [[0, .15], [.1, .129], [.28, .112], [.44, .108], [.62, .112], [.82, .127], [1, .146]];   // its roof is cut at a slant
const cylL = pf => CL0 - (CL0 - CL1) * clamp((pf - .166) / .334);
const cylR = pf => CR0 + (CR1 - CR0) * clamp((pf - .144) / .456);
const cylTop = xf => xf < CL0 ? .166 + (CL0 - xf) / (CL0 - CL1) * .334 : xf > CR0 ? .146 + (xf - CR0) / (CR1 - CR0) * .454 : along2(CYL_TOP, (xf - CL0) / (CR0 - CL0));
const CK = .052, Q0 = .1, bend = u => 1 - ((u - .8) / .8) ** 2;   // its floors are rings: seen from below they rise towards the middle
const legX = pf => SLAB.l1 - .052 * (pf - SLAB.foot) / .09;    // the slanting leg: its left edge
const TOP_OF_ALL = [.778, .108], LED = [178, 214, 255];       // its highest point; the colour of the lights that wander over it at night

// ---------- the houses ----------
// [from, to, the eaves, the ridge (0: a flat roof), what kind, its walls, its roof, floors, windows in a row, hipped]
const up = (xf, k) => HOR - .158 * k * (xf - CVX) / 2.97;      // how high a house k times the usual height reaches at x
const WALLS = ['hCream', 'hGrey', 'hTaupe', 'hOchre', 'hRose'];
const PHOTO = [
  [-.2, .064, .392, 0, 'dark', 'hDark', '', 9, 15],
  [.062, .25, .442, .433, 'balcony', 'hCream', 'roofGrey', 6, 9],
  [.235, .34, .461, .444, 'old', 'hGrey', 'roofGrey', 5, 5, true],
  [.43, .52, .474, 0, 'modern', 'hWhite', '', 6, 6],
  [.34, .452, .478, .461, 'old', 'hTaupe', 'roofGrey', 5, 6],
  [.518, .58, .522, .508, 'old', 'hCream', 'roofRed', 3, 4],
  [.578, .622, .474, 0, 'glass', 'uGreen', '', 6, 3],
  [.856, .944, .352, 0, 'glass', 'uGreen', '', 12, 5],
  [.941, 1.1, .368, 0, 'modern', 'hWhite', '', 8, 7],
];
let houses = [];
function buildHouses() {                                       // those on the photo, and a row made up on either side
  houses = [];
  const add = (x0, x1, top, ridge, style, wall, roof, floors, bays, hip) => {
    const mid = (x0 + x1) / 2, sc = (mid - CVX) / 2.97, id = Math.round(x0 * 997) + 4000;
    const h = { id, x0, x1, top, ridge, style, wall, roof, floors, bays, hip: !!hip, sc, far: clamp(.46 - (mid - CVX) / 7.6, 0, .45), lit: style === 'old' || style === 'balcony' ? .3 : .22, chim: [] };
    if (ridge) { const r = rng(id); for (let i = 0, n = 2 + (r() * 3 | 0); i < n; i++) h.chim.push([x0 + (x1 - x0) * (.1 + .8 * (i + .2 + r() * .6) / n), (.0035 + r() * .003) * sc, ridge - (.007 + r() * .009) * sc]); }
    houses.push(h);
  };
  const made = (r, x0, x1) => {                                // the high ones only far off: none is to stand up to the tower
    const mid = (x0 + x1) / 2, sc = (mid - CVX) / 2.97, tall = r() < .2 && mid < 0, k = tall ? 1.4 + r() * .35 : .9 + r() * (mid < 0 ? .3 : .2), top = up(mid, k), bays = Math.max(3, Math.round((x1 - x0) / sc / .026));
    if (tall) add(x0, x1, top, 0, r() < .5 ? 'modern' : 'dark', r() < .5 ? 'hWhite' : 'hDark', '', Math.round(k * 5.5), bays);
    else add(x0, x1, top, top - (.011 + r() * .01) * sc, r() < .25 ? 'balcony' : 'old', pick(WALLS, r()), r() < .4 ? 'roofRed' : 'roofGrey', k > 1.06 ? 6 : 5, bays, r() < .2);
  };
  let r = rng(91);
  for (let x = -.2; x > CVX + .5;) { const w = (.16 + r() * .1) * (x - CVX) / 2.97; made(r, x - w, x + .004); x -= w; }
  for (const p of PHOTO) add(...p);
  r = rng(57);
  for (let x = 1.1; x < 3.4;) { const w = (.17 + r() * .09) * (x - CVX) / 2.97; made(r, x - .004, x + w); x += w; }
}
function topAt(h, xf) {                                        // where a house ends at x: its ridge, a chimney
  let t = h.top;
  if (h.ridge) {
    t = h.ridge;
    if (h.hip) { const inset = (h.x1 - h.x0) * .22, e = Math.min(xf - h.x0, h.x1 - xf); if (e < inset) t = h.top + (h.ridge - h.top) * e / inset; }
    for (const c of h.chim) if (Math.abs(xf - c[0]) < c[1]) t = Math.min(t, c[2]);
  }
  return t;
}

// ---------- the trees along the far quay ----------
// Each a heap of leaves on a trunk: [x, y, how big], and where it stands: on the pavement along the wall, this side
// of the road.  They all stand in front of a house.
let trees = [], treeTop = 1;
function buildTrees() {
  trees = [];
  const add = (x, y, r) => { const foot = HOR + (wallTop(x) - HOR) * .86; trees.push({ x, y: Math.min(y, foot - r * 1.5), r, foot, hw: r * .11, far: clamp(.4 - (x - CVX) / 8, 0, .4), k: trees.length }); };   // no crown so low that its trunk does not show
  for (const t of [[-.035, .57, .03], [.018, .573, .03], [.06, .58, .022], [.455, .586, .012], [.472, .58, .018], [.497, .585, .014], [.566, .589, .009]]) add(...t);
  let r = rng(23);
  for (let x = -.11; x > CVX + .6;) { const s = (x - CVX) / 2.82, rad = (.017 + r() * .012) * s; if (r() < .8) add(x, HOR - rad * (.7 + r() * .5), rad); x -= rad * (1.6 + r() * 1.6); }
  r = rng(29);
  for (let x = 1.03; x < 3.4;) { const s = (x - CVX) / 3.82, rad = (.028 + r() * .018) * s; add(x, HOR + .012 * s - rad * (.8 + r() * .4), rad); x += rad * (1.5 + r() * 1.3); }
  treeTop = 1; for (const t of trees) treeTop = Math.min(treeTop, t.y - t.r * 1.3);
}
let zt = null;                                                 // the tree a point was found in
const trunkTop = t => t.y - (traits && traits.bare ? t.r * .55 : 0);   // bare, it shows right up into the crown
function trunkAt(xf, pf) {
  if (pf < treeTop || pf > .68) return false;
  for (const t of trees) if (pf < t.foot && pf > trunkTop(t) && Math.abs(xf - t.x) < t.hw * (.75 + .5 * (pf - t.y) / (t.foot - t.y))) { zt = t; return true; }
  return false;
}
const span = [0, 0];
function clearOfTrunks(x, pf, a, b) {                          // a level stroke through x from a to b: as much of it as crosses no trunk
  if (pf > treeTop && pf < .68) for (const t of trees) {
    if (pf >= t.foot || pf <= t.y - t.r * .55) continue;
    const xa = px(t.x - t.hw * 1.4) - .5 * U, xb = px(t.x + t.hw * 1.4) + .5 * U;
    if (xb <= a || xa >= b) continue;
    if (x <= xa) b = xa; else if (x >= xb) a = xb;
  }
  span[0] = a; span[1] = b;
}
function treeAt(xf, pf) {
  if (pf < treeTop || pf > .66) return false;
  let best = 0;
  for (const t of trees) {
    const dx = (xf - t.x) * .75; if (dx > t.r * 1.3 || dx < -t.r * 1.3) continue;
    const q = 1 - Math.hypot(dx, (pf - t.y) * 1.15) / t.r; if (q > best) { best = q; zt = t; }
  }
  if (!best) return false;
  if (traits && traits.bare) return best > .12 && vnoise(xf * 420, pf * 330) > .6;   // bare: a few twigs, the house behind them
  return best + (vnoise(xf * 170, pf * 130) - .5) * .5 > .12;
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that
// thing's strokes may start there.
const SKY = 0, HOUSE = 1, SLABZ = 2, CYL = 3, SLOTZ = 4, HALL = 5, LEG = 6, TREE = 7, TRUNK = 8, GROUND = 9, UWALL = 10, CRATEZ = 11, PROM = 12, QUAY = 13, WATER = 14, DECK = 15, RAILING = 16, GIRDER = 17;
const CRATE = [.526, .605, .652, .681];                        // the blue container on the promenade: left, right, top, foot
let zx = 0, zy = 0, zv = 0, zh = null;                         // what zone() learnt on the way: x and y on the photo, the line through the point, the house
function zone(xf, pf, raw) {                                   // raw: as if the bridge were not there (the water mirrors what is behind it)
  zx = xf; zy = pf;
  if (!raw && xf < RAILEND) {                                  // the bridge is nearest
    const v = bridgeOf(xf, pf);
    if (v >= FARRAIL) {
      zv = v;
      if (v < RAIL) return DECK;
      if (v < DECKV) return RAILING;
      if (xf < ABUT && v < DECKV + depthAt(xf)) return GIRDER;
    }
  }
  const c = zv = lineOf(xf, pf);
  if (c >= WATERLINE) return WATER;
  if (c >= COPING) return QUAY;
  if (xf >= CRATE[0] && xf < CRATE[1] && pf >= CRATE[2] && pf < CRATE[3]) return CRATEZ;
  if (c >= FOOT) return PROM;
  if (pf >= wallTop(xf)) return UWALL;
  const stem = trunkAt(xf, pf), tree = stem ? zt : null;
  if (stem && traits && traits.bare) return TRUNK;
  if (treeAt(xf, pf)) return TREE;
  if (stem) { zt = tree; return TRUNK; }
  if (pf >= STREET) return GROUND;
  if (xf > .56 && xf < CR1) {
    if (xf >= CL1 && pf >= cylTop(xf)) return CYL;
    if (pf >= SLAB.crown && pf < SLAB.foot) {
      const q = Math.max(pf, SLAB.top);
      if (xf >= slabL(q) && xf < slabR(q)) return SLABZ;
      if (pf >= SLOT && xf >= slabR(q) && xf < cylL(pf)) return SLOTZ;
    } else if (pf >= SLAB.foot) {
      const lx = legX(pf);
      if (pf < BASE ? xf >= lx && xf < .69 : (xf >= lx && xf < lx + .016) || (xf >= .663 && xf < .674)) return LEG;
      if (xf >= lx + .016 && xf < .69) return HALL;
    }
  }
  for (let i = houses.length - 1; i >= 0; i--) { const h = houses[i]; if (xf >= h.x0 && xf < h.x1 && pf >= topAt(h, xf)) { zh = h; return HOUSE; } }
  return SKY;
}
function roofAt(xf) {                                          // where the sky ends at x
  let t = STREET;
  for (const h of houses) if (xf >= h.x0 && xf < h.x1) t = Math.min(t, topAt(h, xf));
  if (xf >= SLAB.l1 && xf < slabR(SLAB.crown)) t = Math.min(t, SLAB.crown);   // left of that the slab hangs over open sky: see front()
  else if (xf >= slabR(SLAB.crown) && xf < CL0) t = Math.min(t, SLOT);
  if (xf >= CL0 && xf < CR1) t = Math.min(t, cylTop(xf));
  return t;
}
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;   // by night it is no lighter than the sky
const veil = d => { const a = d * (traits.fog * 1.3 + traits.rain * .2); PP.haze = a > .01 ? [fogCol(), clamp(a, 0, .85)] : null; };   // fog and rain pale what is far off
const crisp = () => ({ a: .94, jit: 5, dry: .04, bow: 0, sp: 1.1 * U });

// ---------- the houses ----------
function houseStroke(x, y) {
  const R = PP.R, h = zh, xf = zx, pf = zy, o = crisp(), far = c => h.far ? mix(c, pal.cityFar, h.far) : c;
  veil(.4 + h.far);
  const wall = pal[h.wall], xa = px(h.x0) + .4 * U, xb = px(h.x1) - .4 * U, l = (2 + R() * 5) * U;
  if (pf < h.top) {                                            // the roof, and the chimneys on it
    for (const c of h.chim) if (Math.abs(xf - c[0]) < c[1] && pf < h.ridge + .003) {
      PP.brush(px(c[0]), Math.max(py(c[2]), y - 3 * U), px(c[0]), Math.min(py(h.ridge), y + 3 * U), Math.max(1.4 * U, c[1] * 2 * PW), far(white && pf < c[2] + .003 ? pal.snow : mix(wall, pal.stone, .55)), o);
      return;
    }
    const n = fbm(x * .04 / U + h.id, y * .15 / U), bw = (h.x1 - h.x0) / h.bays, b = (xf - h.x0) / bw | 0, u = (xf - h.x0) / bw - b, t = (pf - h.ridge) / (h.top - h.ridge);
    if (!h.hip && b % 2 === 1 && t > .3 && Math.abs(u - .5) < .3) {   // a dormer: a little house of its own up on the roof
      const pane = Math.abs(u - .5) < .15 && t > .5, xm = px(h.x0 + (b + .5) * bw), hw = (pane ? .15 : .3) * bw * PW;
      PP.brush(xm - hw + .3 * U, y, xm + hw - .3 * U, y, 1.5 * U, far(pane ? mix(pal.dark, pal.glass, .3) : white && t < .5 ? pal.snow : mix(wall, pal.stone, .25)), o);
      return;
    }
    const col = white && R() < .5 + white * .45 ? mix(pal.snow, pal[h.roof], n * .15) : mix(pal[h.roof], n > .5 ? pal.towerLight : pal.dark, Math.abs(n - .5) * .5);
    PP.brush(Math.max(xa, x - l), y, Math.min(xb, x + l), y, 1.8 * U, far(t > .88 ? mix(col, pal.dark, .3) : col), o);
    return;
  }
  const n = h.floors, fh = (STREET - h.top) / n, f = Math.min(n - 1, (pf - h.top) / fh | 0), t = (pf - h.top) / fh - f;
  const bw = (h.x1 - h.x0) / h.bays, b = clamp((xf - h.x0) / bw | 0, 0, h.bays - 1), u = (xf - h.x0) / bw - b;
  const lit = night && hash1(h.id * 131 + f * 17 + b * 7) < h.lit, own = hash1(h.id * 53 + f * 29 + b * 11);   // own: no two windows are alike
  const pane = k => lit ? pal.lights[(h.id + f + b) % pal.lights.length] : own < .1 && h.ridge ? mix(pal.wall, pal.dark, .3)   // dark by day, the sky in its upper half; a curtain drawn here and there
    : mix(mix(pal.dark, pal.glass, .15 + own * .4), pal.skyMid, k + (own > .82 ? .22 : 0));
  const tex = fbm(x * .03 / U + h.id, y * .05 / U);
  let col = mix(wall, tex > .5 ? pal.towerLight : pal.stone, Math.abs(tex - .5) * .3), la = 0, lb = h.bays, w = Math.min(1.9 * U, fh * PH * .3);
  const win = (u0, u1, c) => { col = c; la = b + u0; lb = b + u1; }, pier = (u0, u1) => { if (u < u0) { la = b - 1 + u1; lb = b + u0; } else { la = b + u1; lb = b + 1 + u0; } };
  if (h.style === 'modern') {                                  // white bands, ribbons of glass between them
    if (t > .32 && t < .78) { if (u < .08) win(0, .08, mix(wall, pal.stone, .25)); else win(.08, 1, pane(t < .5 ? .28 : .1)); }
    else if (f === 0 && t < .1) col = mix(wall, pal.stone, .4);
  } else if (h.style === 'dark') {                             // a dark grid, glass in every field
    col = mix(wall, pal.stone, .3 + tex * .2);
    if (t > .2 && t < .84) { if (u > .12 && u < .88) win(.12, .88, mix(pane(.3), wall, .2)); else pier(.12, .88); }
  } else if (h.style === 'glass') {                            // nothing but glass
    if (u < .08 || t < .1) { col = mix(pal.uRing, wall, .55); if (t >= .1) { la = b; lb = b + .08; } }
    else win(.08, 1, lit ? mix(pal.lights[2], wall, .45) : mix(mix(wall, pal.skyMid, .15 + .35 * vnoise(xf * 60 + 3, pf * 50)), pal.uDark, .12));
  } else if (f === n - 1) {                                    // shops down on the street
    const shop = hash1(h.id * 7 + (b >> 1) * 13);
    if (t < .24) col = shop < .35 ? pal.shops[(shop * 20 | 0) % pal.shops.length] : mix(wall, pal.stone, .35);
    else if (u > .1 && u < .9) win(.1, .9, night && shop < .8 ? pal.lights[(b >> 1) % pal.lights.length] : mix(pal.dark, wall, .22)); else pier(.1, .9);
  } else if (h.style === 'balcony' && b % 3) {                 // balconies run along it: a pale slab, deep shade behind, a parapet
    if (t < .14) col = mix(wall, pal.wall, .55);
    else if (t < .58) { if (u > .06 && u < .94) win(.06, .94, lit ? pal.lights[(h.id + f + b) % pal.lights.length] : mix(pal.dark, wall, .3)); else pier(.06, .94); }
    else if (t < .9) col = mix(wall, pal.towerLight, .2);
  } else if (t > .29 && t < .8) {                              // a window in its frame of stone, or the wall between two
    const d = Math.abs(u - .5);
    if (d < .18) win(.32, .68, pane(t < .5 ? .25 : .06)); else if (d < .245) win(u < .5 ? .255 : .68, u < .5 ? .32 : .745, mix(wall, pal.wall, .55)); else pier(.255, .745);
  } else if (t < .09) col = f ? mix(col, pal.wall, .4) : mix(col, pal.dark, .32);   // the ledge between two floors, the shade under the eaves
  else if (Math.abs(u - .5) < .27 && (t >= .8 ? t < .86 : t > .23)) win(.23, .77, white && t >= .8 && R() < white ? pal.snow : mix(wall, pal.wall, .6));   // its sill and its lintel, snow on the sill
  else if (b === 0 && u < .09) win(0, .09, mix(col, pal.dark, .16));                // where the next house begins
  const sd = shadow(x, y); if (sd && !lit) col = mix(col, pal.dark, sd * .15);
  const A = Math.max(xa, px(h.x0 + la * bw) + .3 * U, x - l), B = Math.min(xb, px(h.x0 + lb * bw) - .3 * U, x + l);
  clearOfTrunks(x, pf, Math.min(A, B), Math.max(A, B, A + .6));
  PP.brush(span[0], y, span[1], y, w, lit ? col : far(col), o);
}

// ---------- the tower ----------
const glass = () => mix(pal.uGlass, pal.skyMid, .45);           // it is the colour of the sky it stands in
function slabStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, o = crisp();
  veil(.45);
  if (pf < SLAB.top) {                                         // up on its roof: posts and panes
    const xl = px(slabL(SLAB.top)), xr = px(slabR(SLAB.top));
    if (R() < .4) { const xp = px(SLAB.l0 + Math.round((xf - SLAB.l0) / .0095) * .0095); PP.brush(xp, py(SLAB.crown) + U, xp, py(SLAB.top), .9 * U, pal.uRing, o); }
    else PP.brush(Math.max(xl, x - 4 * U), y, Math.min(xr, x + 4 * U), y, 1.6 * U, pf < SLAB.crown + .004 ? pal.uRing : mix(pal.skyMid, pal.uRing, .3), { a: .6, jit: 4, dry: .1, bow: 0, sp: 1.1 * U });
    return;
  }
  const xl = px(slabL(pf)) + .5 * U, xr = px(slabR(pf)) - .5 * U, u = clamp((x - xl) / (xr - xl)), f = (pf - SLAB.top) / FH | 0, t = (pf - SLAB.top) / FH - f, seg = u * 5 | 0;
  let col, a = xl, b = xr;
  if (t > .8) col = t > .92 ? mix(pal.uRing, pal.uDark, .3) : mix(pal.uDark, pal.uGlass, .1);   // the edge of a floor
  else if (night && hash1(f * 57 + seg * 13 + 3) < .3) { col = pal.lights[(f + seg) % pal.lights.length]; a = xl + (xr - xl) * seg / 5 + U; b = xl + (xr - xl) * (seg + 1) / 5 - U; }
  else {
    col = mix(mix(glass(), pal.uDark, .1 + u * .24), pal.cloud, smooth(.5, .7, fbm(xf * 26 + 2, pf * 34 + time * .004)) * (night ? .08 : .4));
    if (warm) col = mix(col, pal.sunLow, .3 * (1 - u));
    if (R() < .1) { const xm = xl + (xr - xl) * Math.round(u * 5) / 5; PP.brush(xm, py(SLAB.top + f * FH), xm, py(SLAB.top + (f + .8) * FH), .9 * U, mix(col, pal.uRing, .5), o); return; }   // a post between two panes
  }
  const l = (3 + R() * 6) * U;
  PP.brush(Math.max(a, x - l), y, Math.max(Math.min(b, x + l), a + 1), y, 2 * U, col, o);
}
function cylStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, o = crisp();
  veil(.45);
  const xl = cylL(pf), xr = cylR(pf), u = clamp((xf - xl) / (xr - xl)), pl = px(xl) + .5 * U, pr = px(xr) - .5 * U;
  const shade = c => mix(c, pal.uDark, smooth(.3, 0, u) * .55 + smooth(.84, 1, u) * .28);     // it turns away from the light on either side
  if (pf >= BASE) {                                            // the hall it stands in: tall panes
    const cell = (xf - CL1) / .0115, post = cell - Math.floor(cell) < .16, xm = post ? px(CL1 + (Math.floor(cell) + .08) * .0115) : x, l = (4 + R() * 8) * U;
    let col = pf < BASE + .007 ? pal.uRing : post ? mix(pal.uRing, pal.uGreen, .4) : mix(mix(pal.uGreen, pal.skyMid, .2 + .25 * vnoise(xf * 80, pf * 30)), pal.uDark, .2);
    if (night && !post) col = mix(col, pal.lights[0], .55);
    PP.brush(clamp(xm, pl, pr), Math.max(py(BASE), y - l), clamp(xm, pl, pr), Math.min(py(STREET), y + l), post ? 1.1 * U : 2.2 * U, night && !post ? col : shade(col), o);
    return;
  }
  const lift = CK * bend(u), q = pf + lift * (HOR - pf), f = Math.floor((q - Q0) / FH), t = (q - Q0) / FH - f;
  const s = CK * 2 * (u - .8) / .64 * (HOR - pf) / (1 - lift) * PH / ((xr - xl) * PW), k = 1 / Math.hypot(1, s), ca = k, sa = s * k;   // the way a floor runs here
  let col, a = 1e9, b = 1e9;
  if (pf < cylTop(xf) + .011) col = mix(mix(pal.skyMid, pal.uRing, .5), pal.uDark, .06);     // the glass round its roof
  else if (t < .13) col = shade(mix(pal.uRing, glass(), .3));                                 // a bright ring under every floor
  else if (t > .9) col = mix(pal.uDark, pal.uGlass, .2);
  else {
    const seg = u * 16 | 0;
    if (night && hash1(f * 61 + seg * 7 + 1) < .2) { col = pal.lights[(f + seg) % pal.lights.length]; a = (xf - xl - (xr - xl) * seg / 16) * PW - U; b = (xl + (xr - xl) * (seg + 1) / 16 - xf) * PW - U; }
    else {
      col = shade(mix(glass(), pal.cloud, smooth(.48, .68, fbm(u * 2.4 + 3, q * 9 + time * .004)) * smooth(.1, .32, u) * (night ? .06 : .55)));   // the clouds, mirrored
      if (night) col = mix(col, LED, smooth(.46, .62, fbm(u * 3 + time * .03, q * 8 - time * .05)) * .6);                                   // and after dark the lights that wander over it
      else if (sun) col = warm ? mix(col, pal.sunLow, .5 * Math.exp(-(((u - .3) / .13) ** 2))) : mix(col, pal.cloud, .25 * Math.exp(-(((u - .37) / .07) ** 2)));   // by day a streak of sun
      if (R() < .15) {                                         // a post between two panes
        const xm = px(xl + (xr - xl) * Math.round(u * 24) / 24), h = FH * .34 * PH;
        PP.brush(xm, y - h, xm, y + h, .8 * U, mix(col, pal.uRing, .45), o);
        return;
      }
    }
  }
  const l = (2.5 + R() * 4) * U, l0 = Math.max(.4, Math.min(l, a, (x - pl) / ca)), l1 = Math.max(.4, Math.min(l, b, (pr - x) / ca));
  PP.brush(x - ca * l0, y - sa * l0, x + ca * l1, y + sa * l1, 1.9 * U, col, o);
}
function towerFootStroke(x, y, k) {                            // the slot between the two; the leg, the column and the hall behind them
  const R = PP.R, xf = zx, pf = zy, o = crisp(), l = (3 + R() * 5) * U;
  veil(.45);
  if (k === SLOTZ) {
    const q = Math.max(pf, SLAB.top), t = (pf - SLAB.top) / FH % 1, xl = px(slabR(q)), xr = px(cylL(pf));
    PP.brush(Math.max(xl, x - l), y, Math.max(xl + 1, Math.min(xr, x + l)), y, 2 * U, mix(pal.uDark, t > .8 ? pal.ink : pal.uGreen, t > .8 ? .4 : .12 + .14 * vnoise(xf * 90, pf * 40)), o);
    return;
  }
  const lx = legX(pf);
  if (k === HALL) {
    const cx = (xf - .59) / .0125, cy = (pf - BASE) / .0195, post = cx - Math.floor(cx) < .16 || cy - Math.floor(cy) < .14;
    let col = post ? mix(pal.uRing, pal.uGreen, .5) : mix(pal.uGreen, pal.uDark, .3 + .2 * vnoise(xf * 90, pf * 60));
    if (night && !post) col = mix(col, pal.lights[0], .5);
    PP.brush(x, Math.max(py(BASE), y - 3 * U), x, Math.min(py(STREET), y + 3 * U), 1.5 * U, col, o);
    return;
  }
  const col = mix(pal.uConcrete, pal.stone, .1 + fbm(x * .05 / U, y * .05 / U) * .25);
  if (pf < BASE) PP.brush(Math.max(px(lx), x - l), y, Math.min(px(.69), x + l), y, 1.8 * U, pf > BASE - .004 ? mix(col, pal.dark, .4) : col, o);            // the beam the slab rests on
  else if (xf >= .663) PP.brush(px(.6685), Math.max(py(BASE), y - l), px(.6685), Math.min(py(STREET), y + l), .011 * PW, mix(col, pal.dark, .12), o);
  else { const dx = -.052 * PW, dy = .09 * PH, n = Math.hypot(dx, dy), xm = px(lx + .008); PP.brush(xm - dx / n * l, y - dy / n * l, xm + dx / n * l, y + dy / n * l, .014 * PW, xf > lx + .011 ? mix(col, pal.dark, .25) : col, o); }
}

// ---------- the trees ----------
function treeStroke(x, y) {
  const R = PP.R, t = zt, sz = clamp(t.r / .03, .42, 1.25) * U, far = c => t.far ? mix(c, pal.cityFar, t.far) : c;
  veil(.4 + t.far);
  if (traits.bare) {                                           // twigs, and the snow on them
    const a = -Math.PI / 2 + (R() - .5) * 2.2, l = (5 + R() * 10) * sz;
    if (white && R() < white * .4) PP.dab(x, y, (2 + R() * 3) * sz, pal.snow, { a: .9, den: .6, core: .5, sx: 1.6, sy: .6 });
    else PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 3 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]], far(R() < .6 ? pal.folDeep : pal.folDark), (.8 + R() * .9) * sz, { pass: 1, a: .8 });
    return;
  }
  const s = time * .03, k = .022 / U, sw = wind(time) * 1.5 * sz, q = R();
  const lum = clamp(.5 + (fbm((x - sw) * k + s, y * k * 1.2) - fbm((x - sw - 8 * U) * k + s, (y - 8 * U) * k * 1.2)) * 3.2 - (zy - t.y) / t.r * .3 - shadow(x, y) * .3);
  let col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .6 && q < .3) col = pal.folSun; else if (pal.accent && lum > .3 && q > (season === 'spring' ? .7 : .9)) col = pick(pal.accent, R());
  const a = R() * Math.PI, l = (3 + R() * 5) * sz;
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (3 + R() * 4.5) * sz, far(col), { a: .87, sp: 1.25 * U, dry: .18, jit: 9 });
}
function trunkStroke(x, y) {
  const R = PP.R, t = zt, xm = px(t.x), w = Math.max(1.7 * U, t.hw * 2 * PW), l = (3 + R() * 6) * U, y0 = py(trunkTop(t)), y1 = py(t.foot), o = { a: .95, jit: 7, dry: .04, bow: .02, sp: Math.min(1.1 * U, w) };
  const far = c => t.far ? mix(c, pal.cityFar, t.far) : c, a = Math.max(y0, y - l), b = Math.min(y1, y + l);
  veil(.4 + t.far);
  PP.brush(xm, a, xm, b, w, far(mix(pal.trunk, pal.towerLight, .1)), o);
  if (w > 2.6 * U && R() < .6) PP.brush(xm - w * .26, a, xm - w * .26, b, w * .3, far(white ? pal.snow : mix(pal.trunk, pal.towerLight, .45)), { a: .7, jit: 5, dry: .04, bow: .02, sp: Math.min(1.1 * U, w * .3) });   // its sunny side
}
function anyTrunk() {                                          // they are thin: each gets strokes of its own
  const R = PP.R, t = pick(trees, R()), x = px(t.x), y = py(trunkTop(t) + R() * (t.foot - trunkTop(t)));
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y)) return;
  if (zone(fx(x), fy(y)) === TRUNK && zt === t) trunkStroke(x, y);
}

// ---------- the far bank ----------
function shadeAt(xf, pf) {                                     // the bridge keeps the light off what lies under it
  if (xf > .46 || bridgeOf(xf, pf) < DECKV) return 0;
  return smooth(.46, .37, xf) * (sun ? .62 : night ? .2 : .34);
}
// The street up there, seen almost level: a pavement along the houses, the roadway, a pavement along the wall with
// the trees and a railing.  How far down from the horizon to the top of the wall each begins:
const KERB = .1, ROADWAY = .16, LINE = .36, KERB2 = .66, WALK = .72, RAILS = .8;
function groundStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, wt = wallTop(xf), t = (pf - HOR) / (wt - HOR), sc = near(xf), o = crisp();
  veil(.4);
  const n = fbm(x * .03 / U + 5, y * .1 / U), e = .01, sl = (wallTop(xf + e) - wallTop(xf - e)) / (2 * e) * clamp(t) * PH / PW, k = 1 / Math.hypot(1, sl);   // it climbs to the bridge
  const paving = mix(mix(pal.pave, pal.paveLight, .45), n > .5 ? pal.paveLight : pal.paveDark, Math.abs(n - .5) * .5);
  let col, l = (4 + R() * 8) * U, w = 1.5 * U, yc = y;
  if (t < KERB || t >= WALK) {                                 // the pavements
    col = white ? mix(paving, pal.snow, white * .85) : paving;
    if (t >= RAILS && xf > RAILEND) {                          // the railing over the wall: a rail, and a post every so often
      const post = Math.floor(xf / (.005 * sc)) % 3 === 0;
      if (t < RAILS + .05) col = pal.girderDark; else if (post) { PP.brush(x, Math.max(py(HOR + (wt - HOR) * RAILS), y - 3 * U), x, Math.min(py(wt), y + 3 * U), U, pal.girderDark, o); return; }
    }
  } else if (t < ROADWAY || t >= KERB2) col = white ? mix(pal.quayLight, pal.snow, white) : mix(pal.quayLight, pal.paveLight, .5);   // the kerbs
  else {                                                       // the roadway, a broken line down its middle
    col = mix(mix(pal.asphalt, pal.shade, .3), n > .5 ? pal.pave : pal.shade, Math.abs(n - .5) * .35);
    if (white) col = mix(col, pal.snow, white * .3);
    if (Math.abs(t - LINE) < .05 && (wt - HOR) * PH > 14 * U && (Math.floor(xf / (.014 * sc)) & 1)) { col = pal.roadLine; yc = py(HOR + (wt - HOR) * LINE); l = 2.5 * U; w = U; }
  }
  if (sun) for (const tr of trees) {                           // the shade of the trees lies across it
    const dx = ((xf - tr.x) * .75 - tr.r * .8) / (tr.r * 1.25), dy = (pf - tr.foot) / (tr.r * .2);
    if (dx > -1 && dx < 1 && dx * dx + dy * dy < 1) { col = mix(col, pal.shade, .3); break; }
  }
  if (night) col = mix(col, pal.lights[1], .13);
  clearOfTrunks(x, pf, x - k * l, x + k * l);
  PP.brush(span[0], yc + (span[0] - x) * sl, span[1], yc + (span[1] - x) * sl, w, col, o);
}
// What is sprayed on the upper wall: piece beside piece, each a field of colour with a few fat letters in another.
function piece(xf, t) {                                        // 0 where the wall is bare, else which paint
  if (t < .14 || t > .97) return 0;
  const pw = .046, i = Math.floor(xf / pw), h = hash1(i * 53 + 11);
  if (h < .1) return 0;
  const u = xf / pw - i, lu = u * 2 - Math.floor(u * 2), li = i * 2 + Math.floor(u * 2), v = (t - .14) / .83;
  let p = (hash1(li * 17 + 5) * 9) | 0, d = 1;
  for (let k = 1; k <= 3; k++) { const q = (hash1(li * 31 + k * 7) * 9) | 0; d = Math.min(d, nearSeg(lu, v, .2 + (p % 3) * .3, .14 + ((p / 3) | 0) * .36, .2 + (q % 3) * .3, .14 + ((q / 3) | 0) * .36)); p = q; }
  if (d < .12) return 1 + ((hash1(i * 29 + 3) * 9) | 0);
  if (d < .2) return 7;                                        // its outline, black
  return vnoise(xf * 150 + i * 7, t * 4 + i) > .32 ? 1 + ((h * 9) | 0) : 0;
}
const WORDS = [5, 5, 5, 3, 7, 5, 8, 5];
function word(xf, t) {                                         // on the wall by the water: words in white, here and there
  if (t < 0 || t > 1) return 0;
  const sw = .19, i = Math.floor(xf / sw), h = hash1(i * 91 + 7);
  if (h < .3) return 0;
  const u = (xf / sw - i - .1) / .72; if (u < 0 || u >= 1) return 0;
  const n = 4 + (h * 3 | 0), k = Math.floor(u * n), lu = u * n - k, li = i * 8 + k;
  let p = (hash1(li * 17 + 5) * 9) | 0;
  for (let j = 1; j <= 3; j++) {
    const q = (hash1(li * 31 + j * 7) * 9) | 0;
    if (nearSeg(lu, t, .18 + (p % 3) * .32, .1 + ((p / 3) | 0) * .4, .18 + (q % 3) * .32, .1 + ((q / 3) | 0) * .4) < .15) return 1 + WORDS[(h * 40 | 0) % WORDS.length];
    p = q;
  }
  return 0;
}
function wallStroke(x, y) {                                    // the upper wall, under the street
  const R = PP.R, xf = zx, pf = zy, wt = wallTop(xf), t = (pf - wt) / (lineY(FOOT, xf) - wt), o = crisp();
  veil(.35);
  const n = fbm(x * .03 / U + 7, y * .09 / U), g = t < .09 ? 0 : piece(xf, t);
  let col = mix(pal.quay, n > .5 ? pal.quayLight : pal.quayDark, Math.abs(n - .5) * .9), l = (3 + R() * 6) * U;
  if (t < .09) col = white ? mix(pal.quayLight, pal.snow, white) : pal.quayLight;      // its coping
  else if (g) { col = mix(pal.tag[g - 1], col, .1); l = 1.8 * U; }
  const sh = shadeAt(xf, pf); if (sh) col = mix(col, pal.shade, sh);
  PP.brush(x - l, y, x + l, y, 1.7 * U, col, o);
}
const PROM_LAMPS = [-.9, -.45, -.1, .68, .86, 1.06, 1.3, 1.58, 1.9, 2.3];   // the lamps along the promenade
function promStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, c = zv, t = (c - FOOT) / (COPING - FOOT), o = { a: .88, jit: 6, dry: .1, bow: .02, sp: 1.1 * U };
  veil(.32);
  const n = fbm(x * .02 / U + 9, y * .08 / U);
  let col = mix(pal.pave, n > .5 ? pal.paveLight : pal.paveDark, Math.abs(n - .5) * .8);
  if (sun) col = xf > .37 && t < .5 * smooth(.72, .5, xf) ? mix(col, pal.shade, .35) : mix(col, pal.paveLight, .3);   // the wall's shadow lies on it by the bridge
  if (t > .92) col = white ? pal.snow : pal.quayLight;                                 // the stone edge
  else if (white) col = mix(col, pal.snow, white * (.6 + n * .4));
  if (night) for (const lx of PROM_LAMPS) { const d = (xf - lx) / (.05 * near(lx)); if (d > -1.6 && d < 1.6) col = mix(col, pal.lights[0], .3 * Math.exp(-d * d * 2)); }   // a pool of light under every lamp
  const sh = shadeAt(xf, pf), sd = shadow(x, y); if (sh) col = mix(col, pal.shade, sh); if (sd) col = mix(col, pal.paveDark, sd * .4);
  canalDir(c);
  const l = (5 + R() * 10) * U;
  PP.brush(x - cdir[0] * l, y - cdir[1] * l, x + cdir[0] * l, y + cdir[1] * l, (1.5 + R() * 1.3) * U, col, o);
}
function quayStroke(x, y) {                                    // the wall down to the water: old stone, weathered, green where it is wet
  const R = PP.R, xf = zx, pf = zy, c = zv, t = (c - COPING) / (WATERLINE - COPING), o = crisp();
  veil(.3);
  const n = fbm(x * .035 / U + 3, y * .07 / U), g = t > .16 && t < .8 ? word(xf, (t - .2) / .56) : 0;
  let col = mix(pal.quay, n > .5 ? pal.quayLight : pal.quayDark, .15 + Math.abs(n - .5) * 1.1), l = (3 + R() * 7) * U;
  if (vnoise(xf * 230, 3.3) > .7) col = mix(col, pal.quayDark, .3 * t);                // where water runs down it
  if (t < .1) col = white ? mix(pal.quayLight, pal.snow, white) : mix(pal.quayLight, pal.paveLight, .4);
  else if (t > .82) col = mix(pal.quayDark, pal.moss, .35 + n * .4);
  else if (g) { col = mix(pal.tag[g - 1], col, .15); l = 1.7 * U; }
  const sh = shadeAt(xf, pf), sd = shadow(x, y); if (sh) col = mix(col, pal.shade, sh); if (sd) col = mix(col, pal.quayDark, sd * .3);
  canalDir(c);
  PP.brush(x - cdir[0] * l, y - cdir[1] * l, x + cdir[0] * l, y + cdir[1] * l, 1.7 * U, col, o);
}
function crateStroke(x, y) {                                   // the container: blue, ribbed
  const xf = zx, pf = zy, top = pf < CRATE[2] + .004, rib = Math.floor((xf - CRATE[0]) / .0028), o = crisp();
  const col = top ? (white ? pal.snow : pal.crateTop) : mix(pal.crate, pal.dark, (rib & 1 ? .16 : 0) + (xf > CRATE[1] - .02 ? .3 : 0));
  if (top) PP.brush(Math.max(px(CRATE[0]), x - 3 * U), y, Math.min(px(CRATE[1]), x + 3 * U), y, 1.4 * U, col, o);
  else PP.brush(x, Math.max(py(CRATE[2] + .004), y - 3 * U), x, Math.min(py(CRATE[3]), y + 3 * U), 1.5 * U, col, o);
}

// ---------- the canal ----------
function mirrored(k) {                                         // the colour of a thing as the water shows it
  if (k === SKY) return mix(pal.waterLight, pal.skyLow, .5);
  if (k === CYL || k === SLABZ) return night ? mix(glass(), LED, .3) : mix(glass(), pal.cloud, .25);
  if (k === HOUSE) return zh.style === 'dark' || zh.style === 'glass' ? mix(pal[zh.wall], pal.skyMid, .2) : pal[zh.wall];
  if (k === TREE) return pal.folMid;
  if (k === SLOTZ || k === HALL) return pal.uDark;
  return mix(pal.quayDark, pal.waterDark, .5);
}
function waterStroke(x, y) {
  const R = PP.R, xf = zx, pf = zy, wl = lineY(WATERLINE, xf), d = pf - wl, sz = (.65 + clamp(d / .2) * .75) * U;   // d: how far out from the wall
  veil(.3 * (1 - clamp(d / .15)));
  const rip = vnoise(x * .028 / U + time * .45, y * .32 / U + time * .1);
  let col = mix(pal.water, rip > .5 ? pal.waterLight : pal.waterDark, Math.min(1, Math.abs(rip - .5) * 2.1));
  // what stands on the far bank, upside down and drawn out by the ripples
  const k = zone(xf + (vnoise(x * .05 / U, y * .2 / U + time * .3) - .5) * .014, wl - d * 2.7 - (rip - .5) * .06, true);
  col = mix(col, mirrored(k), (k === SKY ? .34 : .46) * (.55 + .45 * smooth(.2, .5, rip) * smooth(.8, .5, rip)) * (1 - traits.fog * .5));
  if (xf < .62 && pf < .8 - .097 * xf) col = mix(col, pal.waterDark, sun ? .5 : .25);          // the shadow of the bridge
  const sd = shadow(x, y); if (sd) col = mix(col, pal.waterDark, sd * .45);
  let l = (8 + R() * 20) * sz, w = (1.4 + R() * 2.2) * sz;
  if (R() < .2) { col = mix(col, R() < .55 ? pal.waterDark : pal.waterLight, .45); l *= .6; w = (.9 + R() * .5) * sz; }   // a ripple, drawn sharp
  if (zone(fx(x - l), pf) !== WATER || zone(fx(x + l), pf) !== WATER) l *= .25;
  PP.brush(x - l, y, x + l, y + (R() - .5) * U, w, col, { a: .85, jit: 6, dry: .15, sp: 1.1 * U, bow: 0 });
  if (night && R() < .5) for (const lx of PROM_LAMPS) {        // the lamps of the promenade, drawn out long
    const xl = px(lx); if (Math.abs(x - xl) > (3 + d * 60) * U || zone(lx, pf) !== WATER) continue;
    const xs = xl + (R() - .5) * (2 + d * 50) * U;
    PP.brush(xs - 3 * sz, y, xs + 3 * sz, y, 1.4 * U, pick(pal.lights, R()), { a: .55 * (1 - clamp(d / .2)), sp: U, bow: 0, jit: 4 });
  }
}

// ---------- the bridge ----------
function deckStroke(x, y) {                                    // the roadway, seen over the railing
  const R = PP.R, xf = zx, v = zv, s = (BVX - xf) / BVX, sz = clamp(s, .45, 2) * U, o = { a: .9, jit: 6, dry: .08, bow: 0, sp: Math.min(1.1 * U, sz) };
  veil(.3 / (1 + s));
  const n = fbm(x * .03 / U + 2, y * .1 / U);
  let col = mix(pal.asphalt, n > .5 ? pal.pave : pal.paveDark, Math.abs(n - .5) * .6);
  if (v < FARRAIL + .0035) col = Math.floor((1 / (BVX - xf)) * 60) & 1 ? pal.girder : pal.girderDark;      // the railing on its far side
  else if (v < FARRAIL + .0065 || v > RAIL - .0035) col = white ? mix(pal.paveLight, pal.snow, white) : pal.paveLight;   // the kerbs
  else if (Math.abs(v - .628) < .0009 && (Math.floor((1 / (BVX - xf)) * 9) & 1)) col = pal.roadLine;
  else if (white) col = mix(col, pal.snow, white * .3);
  if (night) col = mix(col, pal.lights[1], .12);
  bridgeDir(v);
  const l = (4 + R() * 9) * sz, l1 = Math.min(l, (px(RAILEND) - x) / bdir[0]);
  PP.brush(x - bdir[0] * l, y - bdir[1] * l, x + bdir[0] * l1, y + bdir[1] * l1, 1.5 * sz, col, o);
}
const BARS = 80;                                               // how many bars the railing has to a stretch of the bridge
function railStroke(x, y) {                                    // its near railing: a hand rail, bars, the roadway between them
  const R = PP.R, xf = zx, v = zv, s = (BVX - xf) / BVX, sz = clamp(s, .45, 2.2) * U, o = crisp();
  veil(.25 / (1 + s));
  bridgeDir(v);
  const l = (5 + R() * 10) * sz, l1 = Math.min(l, (px(RAILEND) - x) / bdir[0]), along = col => PP.brush(x - bdir[0] * l, y - bdir[1] * l, x + bdir[0] * l1, y + bdir[1] * l1, 1.3 * sz, col, o);
  if (v < RAIL + .003) { along(white ? mix(pal.girderLight, pal.snow, white) : pal.girderLight); return; }
  if (v > DECKV - .003) { along(pal.girderDark); return; }
  const i = Math.round(BARS / (BVX - xf)), xb = BVX - BARS / i, pitch = px(BVX - BARS / (i + 1)) - px(xb);
  if (pitch < 2.6 * U) { along(mix(pal.girder, pal.asphalt, .45)); return; }       // too far off to tell them apart
  if (R() < .6) { const xp = px(xb); PP.brush(xp, py(bridgeY(RAIL + .002, xb)), xp, py(bridgeY(DECKV - .002, xb)), Math.max(.8 * U, pitch * .2), pal.girder, o); return; }
  const xm = px(BVX - BARS / (i + (xf > xb ? .5 : -.5))), hw = pitch * .3;
  PP.brush(xm - hw, y - bdir[1] * hw, xm + hw, y + bdir[1] * hw, 1.3 * sz, mix(pal.asphalt, pal.dark, .35 + (night ? 0 : R() * .2)), o);
}
function girderStroke(x, y) {                                  // green steel: a web between two flanges, a rib every so often
  const R = PP.R, xf = zx, v = zv, dep = depthAt(xf), t = (v - DECKV) / dep, s = (BVX - xf) / BVX, sz = clamp(s, .5, 2) * U, o = crisp();
  veil(.25 / (1 + s));
  const n = fbm(x * .02 / U + 13, y * .06 / U), fl = .08 * .03 / dep;
  let col = mix(pal.girder, n > .5 ? pal.girderLight : pal.girderDark, Math.abs(n - .5) * .45);
  if (t < fl * 1.7) col = white ? mix(pal.paveLight, pal.snow, white) : mix(pal.paveLight, pal.quayLight, .5);   // the edge of the roadway lies on it
  else if (t < fl * 2.9) col = mix(pal.girderDark, pal.shade, .4);                     // and shades its top
  else if (t > 1 - fl) col = mix(pal.girderLight, pal.girder, .35);                    // the lower flange catches the light
  else if (t > 1 - fl * 1.9) col = pal.girderDark;
  else if (xf < .36) {
    const i = Math.round(14 / (BVX - xf)), xr = BVX - 14 / i;
    if (Math.abs(x - px(xr)) < 2.4 * sz && R() < .75) {
      const lt = R() < .4, xp = px(xr) - (lt ? 1.4 * sz : 0), y0 = py(bridgeY(DECKV + depthAt(xr) * fl * 2.9, xr)), y1 = py(bridgeY(DECKV + depthAt(xr) * (1 - fl * 1.9), xr));
      PP.brush(xp, Math.max(y0, y - 9 * sz), xp, Math.min(y1, y + 9 * sz), (lt ? 1 : 1.4) * sz, lt ? pal.girderLight : mix(pal.girderDark, pal.girder, .2), o);
      return;
    }
  }
  if (night) col = mix(col, pal.shade, .15);
  const e = .01, bot = f => bridgeY(DECKV + depthAt(f), f), sl = (-(DECKV - BVY) / BVX * (1 - t) + (bot(xf + e) - bot(xf - e)) / (2 * e) * t) * PH / PW, k = 1 / Math.hypot(1, sl);   // along the flange it is nearer to
  const l = (5 + R() * 10) * sz, l1 = Math.min(l, (px(ABUT) - x) / k);
  PP.brush(x - k * l, y - sl * k * l, x + k * l1, y + sl * k * l1, (1.6 + R()) * sz, col, o);
}

// ---------- every stroke where it belongs ----------
function paintAt(x, y) {
  const k = zone(fx(x), fy(y));
  if (k === HOUSE) houseStroke(x, y); else if (k === CYL) cylStroke(x, y); else if (k === SLABZ) slabStroke(x, y);
  else if (k === SLOTZ || k === HALL || k === LEG) towerFootStroke(x, y, k); else if (k === TREE) treeStroke(x, y); else if (k === TRUNK) trunkStroke(x, y);
  else if (k === GROUND) groundStroke(x, y); else if (k === UWALL) wallStroke(x, y); else if (k === PROM) promStroke(x, y);
  else if (k === QUAY) quayStroke(x, y); else if (k === CRATEZ) crateStroke(x, y); else if (k === WATER) waterStroke(x, y);
  else if (k === DECK) deckStroke(x, y); else if (k === RAILING) railStroke(x, y); else if (k === GIRDER) girderStroke(x, y);
}
let landTop = 0, townFoot = 0, canalFoot = 0, waterTop = 0, tw = [0, 0, 0, 0], br = [0, 0, 0];   // where the town ends, the bank, where the water begins; the tower's and the bridge's corners
const within = (xa, ya, xb, yb) => () => { const R = PP.R, x = xa + R() * (xb - xa), y = ya + R() * (yb - ya); if (x >= 0 && x < W && y < H && at(x, y)) paintAt(x, y); };
let townStroke = null, towerStroke = null, canalStroke = null, bridgeStroke = null, waterArea = null;

// ---------- weather ----------
function fogStroke() {                                         // fog lies on the canal in drifting banks
  const R = PP.R, x = R() * W, depth = R(), y = py(.6 + depth * .28);
  if (!at(x, y) || !traits.fog) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .45, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // the sun on the water
  const R = PP.R, x = R() * W, y = waterTop + R() * (H - waterTop);
  if (!at(x, y) || !sun || zone(fx(x), fy(y)) !== WATER || (zx < .62 && zy < .8 - .097 * zx)) return;
  if (vnoise(x * .01 / U + time * .1, y * .05 / U) < .55) return;
  PP.brush(x - 2.5 * U, y, x + 2.5 * U, y, 1.1 * U, pal.sun, { a: .8, jit: 2, dry: 0, bow: 0, sp: U });
}

// ---------- the coat ----------
function tone(k) {                                             // the colour of a thing, all in all
  if (k === HOUSE) { const h = zh, c = zy < h.top ? (white ? pal.snow : pal[h.roof]) : mix(pal[h.wall], pal.dark, h.style === 'old' || h.style === 'balcony' ? .1 : .2); return h.far ? mix(c, pal.cityFar, h.far) : c; }
  if (k === CYL || k === SLABZ) return mix(glass(), pal.uDark, .18);
  if (k === SLOTZ || k === HALL) return mix(pal.uDark, pal.uGreen, .2);
  if (k === LEG) return pal.uConcrete;
  if (k === TREE) return traits.bare ? mix(pal.stone, pal.folDark, .4) : mix(pal.folDark, pal.folMid, .45);
  if (k === TRUNK) return pal.trunk;
  if (k === GROUND) { const t = (zy - HOR) / (wallTop(zx) - HOR); return t >= ROADWAY && t < KERB2 ? mix(pal.asphalt, pal.shade, .3) : white ? mix(pal.pave, pal.snow, white * .7) : mix(pal.pave, pal.paveLight, .4); }
  if (k === GROUND || k === PROM) return white ? mix(pal.pave, pal.snow, white * .7) : pal.pave;
  if (k === UWALL) return mix(mix(pal.quay, pal.tag[(hash1(zx * 22 | 0) * 9) | 0], .35), pal.shade, shadeAt(zx, zy));
  if (k === QUAY) return mix(mix(pal.quay, pal.quayDark, .35), pal.shade, shadeAt(zx, zy));
  if (k === CRATEZ) return pal.crate;
  if (k === WATER) return zx < .62 && zy < .8 - .097 * zx ? mix(pal.water, pal.waterDark, .5) : pal.water;
  return k === DECK ? pal.asphalt : k === RAILING ? mix(pal.girder, pal.asphalt, .4) : mix(pal.girder, pal.girderDark, .3);
}
function coatStroke(x0, y0) {                                  // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U) continue;
    const k = zone(fx(clamp(x, 0, W - 1)), fy(Math.min(y, H - 1))), l = (14 + R() * 8) * U;
    if (k === SKY || k === TRUNK || (k >= GROUND && k !== WATER)) continue;   // the bank and the bridge are narrow things: see below
    veil(k === WATER ? .1 : .45);
    if (k === WATER) PP.brush(x - l * 1.6, y, x + l * 1.6, y, 26 * U, tone(k), o);
    else if (k === TREE) { const a = (R() - .5) * 1.2; PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, 22 * U, tone(k), o); }
    else if (k === HOUSE && zy < zh.top) PP.brush(x - l, y, x + l, y, 10 * U, tone(k), o);
    else PP.brush(x, y - l * .8, x, y + l * .8, k === HOUSE ? 24 * U : 14 * U, tone(k), o);      // a house, the tower: an upright stroke
  }
  if (y0 < townFoot - 30 * U || y0 > canalFoot + 30 * U) return;
  for (let n = 0; n < 18; n++) {                               // narrow strokes, close together, each along the thing it lies on
    const x = x0 + (n % 3 - 1 + (R() - .5) * .5) * 22 * U, y = y0 + ((n / 3 | 0) - 2.5 + (R() - .5) * .5) * 6.6 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 8 * U) continue;
    const k = zone(fx(clamp(x, 0, W - 1)), fy(Math.min(y, H - 1))), l = 15 * U;
    if (k === TRUNK) { PP.brush(px(zt.x), y - 8 * U, px(zt.x), Math.min(py(zt.foot), y + 8 * U), Math.max(2 * U, zt.hw * 2 * PW), tone(k), o); continue; }
    if (k < GROUND || k === WATER) continue;
    veil(.3);
    if (k >= DECK) { bridgeDir(zv); PP.brush(x - bdir[0] * l, y - bdir[1] * l, x + bdir[0] * l, y + bdir[1] * l, 9 * U, tone(k), o); }
    else if (k === PROM || k === QUAY) { canalDir(zv); PP.brush(x - cdir[0] * l, y - cdir[1] * l, x + cdir[0] * l, y + cdir[1] * l, 9 * U, tone(k), o); }
    else PP.brush(x - l, y, x + l, y, 9 * U, tone(k), o);
  }
}

// ---------- what moves ----------
const BRIDGE_LAMPS = [.452, .368, .25, .088, -.19, -.62, -1.3];
function drawBridgeLamps(P) {                                  // the lamps on the railing of the bridge: too thin for the brush that paints it
  for (let i = 0; i < BRIDGE_LAMPS.length; i++) {
    const xf = BRIDGE_LAMPS[i], x = px(xf), s = (BVX - xf) / BVX, y0 = py(bridgeY(RAIL, xf)), y1 = py(bridgeY(.6, xf)), r = Math.max(1.6 * U, .0045 * s * PH);
    if (x < -r * 4 || x > W + r * 4 || !at(clamp(x, 0, W - 1), y1)) continue;
    P.R = rng(7400 + i);
    P.brush(x, y0, x, y1, Math.max(1.1 * U, .0026 * s * PW), pal.girderDark, { a: .97, sp: Math.max(.8 * U, .0013 * s * PW), dry: 0, jit: 3, bow: 0 });
    if (night) P.dab(x, y1 - r, r * 5, pal.lights[0], { a: .3, den: .6, s: 2.4 * U, jit: 3 });
    P.dab(x, y1 - r, r, night ? pal.lights[2] : white ? pal.snow : pal.hull, { a: 1, den: 3, core: 1, s: Math.max(U, r * .5), jit: 3 });
  }
}
// People stroll along the promenade, a cyclist passes: [when, how often, how long the way takes, which way, how far
// out from the wall, coat, on a bicycle].  The lamps stand there too.
const WALKERS = [[0, 131, 110, 1, .3, 0, 0], [21, 157, 120, -1, .7, 1, 0], [47, 113, 100, 1, .55, 2, 0], [60, 67, 34, -1, .42, 3, 1], [5, 149, 115, -1, .25, 2, 0]];
function drawPromenade(P, t) {
  for (let i = 0; i < PROM_LAMPS.length; i++) {
    const xf = PROM_LAMPS[i], x = px(xf), s = near(xf), y0 = py(lineY(FOOT + .004, xf)), h = .05 * s * PH, r = Math.max(1.3 * U, .0034 * s * PH);
    if (x < -h || x > W + h || !at(clamp(x, 0, W - 1), y0) || zone(xf, fy(y0 - h * .4)) >= DECK) continue;   // not those the bridge hides
    P.R = rng(7500 + i);
    P.brush(x, y0, x, y0 - h, Math.max(U, .002 * s * PW), pal.ink, { a: .95, sp: Math.max(.7 * U, .001 * s * PW), dry: 0, jit: 3, bow: 0 });
    if (night) P.dab(x, y0 - h, r * 5, pal.lights[0], { a: .3, den: .6, s: 2.4 * U, jit: 3 });
    P.dab(x, y0 - h, r, night ? pal.lights[2] : pal.towerLight, { a: 1, den: 3, core: 1, s: Math.max(U, r * .5), jit: 3 });
  }
  const ga = 1 / (Math.max(X0, -1.9) - .08 - CVX), gb = 1 / (X1 + .08 - CVX);
  for (const [t0, every, dur, dir, lane, k, bike] of WALKERS) {
    let u = ((t - t0) % every + every) % every / dur; if (u >= 1) continue;
    if (dir < 0) u = 1 - u;
    const xf = CVX + 1 / (ga + (gb - ga) * u), s = near(xf), h = .021 * s * PH, x = px(xf) + (bike ? 0 : Math.sin(t * 2.7 + k * 2) * h * .02), y0 = py(lineY(FOOT + (COPING - FOOT) * lane, xf));
    if (x < -h || x > W + h || !at(clamp(x, 0, W - 1), Math.min(y0, H - 1)) || zone(xf, fy(y0 - h * .5)) >= DECK) continue;
    const y = y0 - (bike ? 0 : Math.abs(Math.sin(t * 5.4 + k * 2)) * h * .035), o = { a: .96, sp: Math.max(.6 * U, h * .06), dry: 0, jit: 3, bow: 0 };
    P.R = rng(7100 + k + bike * 9);
    if (sun) P.brush(x - h * .05, y0, x - h * .5, y0 - h * .1, h * .12, pal.dark, { a: .4, sp: U, dry: 0, jit: 2, bow: 0 });
    if (bike) { P.brush(x - h * .45, y - h * .16, x + h * .45, y - h * .16, h * .07, pal.ink, o); for (const d of [-1, 1]) P.dab(x + d * h * .4, y - h * .16, h * .17, pal.ink, { a: .9, den: 1.6, core: 0, s: Math.max(.5 * U, h * .05), jit: 2 }); }
    else P.brush(x, y - h * .4, x, y, h * .16, pal.ink, { a: .96, sp: h * .16, dry: 0, jit: 3, bow: 0 });
    for (let n = 0; n < 2; n++) P.brush(x + (bike ? dir * h * .1 : 0), y - h * .84, x, y - h * .36, h * .25, pal.coats[k % pal.coats.length], o);
    P.dab(x + (bike ? dir * h * .14 : 0), y - h * .96, h * .1, pal.skin, { a: 1, den: 2, core: .9, s: Math.max(.5 * U, h * .05), jit: 3 });
  }
}
function belowBridge(c) {                                      // the water as far as the bridge does not hide it
  const xe = px(ABUT), bot = f => py(bridgeY(DECKV + depthAt(f), f));
  c.beginPath(); c.moveTo(W, 0); c.lineTo(W, H); c.lineTo(Math.min(0, xe), H);
  if (xe > 0) { for (let x = 0; x < xe; x += 6 * U) c.lineTo(x, bot(fx(x))); c.lineTo(xe, bot(ABUT - .001)); }
  c.lineTo(Math.max(0, xe), 0); c.closePath();
}
function drawBoat(P, t) {                                      // now and then a boat comes down the canal, and the light on the tower blinks
  const u = (t % 150) / 75, c = .83;                           // the line it keeps to
  const ga = 1 / (Math.max(X0, -1.9) - .35 - CVX), gb = 1 / (X1 + .35 - CVX), xa = CVX + 1 / (ga + (gb - ga) * u), len = .2 * near(xa), xm = xa + len / 2;
  if (u <= 1 && px(xa + len) > -60 * U && px(xa - len) < W && at(clamp(px(xm), 0, W - 1), clamp(py(lineY(c, xm)), 0, H - 1))) {
    // A point of it: f from its stern (0) to its bow (1), k how high above the water, in hulls
    const pt = (f, k) => { const x = xa + f * len; return [px(x), py(lineY(c, x)) - k * .0125 * near(x) * PH + Math.sin(t * 1.1) * .4 * U]; };
    const h = .0125 * near(xm) * PH, solid = { hd: 0, wob: .2 * U, jit: 2 }, thin = w => ({ a: .95, sp: Math.max(.7 * U, w), dry: 0, jit: 3, bow: 0 });
    const hull = mix(pal.hull, pal.dark, night ? .5 : 0), keel = mix(pal.crate, pal.dark, .45), pane = night ? pal.lights[0] : mix(pal.dark, pal.glass, .45);
    P.R = rng(7600);
    P.c.save(); belowBridge(P.c); P.c.clip();
    const foam = mix(pal.waterLight, pal.hull, night ? .15 : .45);
    for (const side of [.55, -1]) for (let i = 0; i < 4; i++) {   // its wake: two lines that part astern, the nearer one the longer
      const a = pt(-.02 - i * .2, -.3 + side * i * .3), b = pt(-.24 - i * .2, -.3 + side * (i + 1.1) * .3);
      P.brush(a[0], a[1], b[0], b[1], (1.9 - i * .25) * U, foam, { a: .75 - i * .16, sp: U, dry: .08, jit: 3, bow: .05 });
    }
    { const a = pt(1.01, -.28), b = pt(.8, -.72); P.brush(a[0], a[1], b[0], b[1], 1.5 * U, foam, { a: .65, sp: U, dry: .08, jit: 3, bow: .1 }); }   // the wave at its bow
    { const a = pt(.02, -.55), b = pt(.93, -.55); P.brush(a[0], a[1], b[0], b[1], h * .45, pal.waterDark, { a: .4, sp: Math.max(U, h * .2), dry: 0, jit: 3, bow: 0 }); }   // its shade on the water
    P.shape([pt(0, .58), pt(.86, .6), pt(1, .82), pt(.94, -.3), pt(.015, -.3)], hull, solid);                  // the hull, its bow drawn up
    P.shape([pt(.008, .08), pt(.975, .1), pt(.94, -.3), pt(.015, -.3)], keel, solid);                           // dark blue down to the water
    P.shape([pt(.15, .55), pt(.15, 1.72), pt(.72, 1.72), pt(.79, .55)], hull, solid);                           // the saloon
    for (let i = 0; i < 6; i++) { const f = .175 + i * .094; P.shape([pt(f, .95), pt(f, 1.45), pt(f + .068, 1.45), pt(f + .068 + (i === 5 ? .022 : 0), .95)], pane, solid); }
    { const a = pt(.13, 1.8), b = pt(.745, 1.8); P.brush(a[0], a[1], b[0], b[1], Math.max(U, h * .16), mix(hull, pal.dark, .4), thin(h * .16)); }   // its roof
    { const a = pt(0, .95), b = pt(.15, .95), d = pt(0, .58); P.brush(a[0], a[1], b[0], b[1], Math.max(.7 * U, h * .08), mix(hull, pal.dark, .5), thin(h * .08)); P.brush(a[0], a[1], d[0], d[1], Math.max(.7 * U, h * .08), mix(hull, pal.dark, .5), thin(h * .08)); }   // the rail round the after deck
    { const a = pt(.03, .58), b = pt(.03, 1.7), f = pt(.055, 1.5); P.brush(a[0], a[1], b[0], b[1], Math.max(.7 * U, h * .07), pal.ink, thin(h * .07)); P.dab(f[0], f[1], Math.max(1.1 * U, h * .2), pal.cabin, { a: 1, den: 2.5, core: 1, s: Math.max(.6 * U, h * .12), jit: 2 }); }   // and the flag over it
    if (night) { const b = pt(.9, .95); P.dab(b[0], b[1], Math.max(1.1 * U, h * .14), pal.lights[2], { a: 1, den: 2.5, core: 1, s: U, jit: 2 }); }
    P.c.restore();
  }
  const x = px(TOP_OF_ALL[0]), y = py(TOP_OF_ALL[1]) - 2 * U, a = Math.pow(Math.max(0, Math.sin(t * 2.1)), 2);
  if (x < 0 || x >= W || y < 0 || !at(x, y) || !night || a < .05 || traits.fog) return;
  P.R = rng(7700);
  P.dab(x, y, 6 * U, pal.beacon, { a: a * .3, den: .5, s: 2.4 * U, jit: 3 });
  P.dab(x, y, 1.6 * U, pal.beacon, { a, den: 2.5, core: a, s: U, jit: 3 });
}

// ---------- the painting, one frame's worth ----------
let tops = null;
const RATE = { town: 15000, tower: 17000, canal: 15000, bridge: 9000, water: 6500, trunk: 10, glint: 70, fog: 1500 };
const place = {
  colors: {
    water: '#67764b', waterLight: '#a9b48b', waterDark: '#3a4a32',
    hCream: '#dbd0b9', hGrey: '#b9b3a8', hTaupe: '#a59b8e', hOchre: '#d7bd87', hRose: '#d6b3a1', hWhite: '#ebeae4', hDark: '#454c57',
    roofGrey: '#6f6c6b', roofRed: '#a8523f', shops: ['#2f6b4a', '#8a2f2a', '#2c4f7c', '#c9a23a'],
    uGlass: '#7697b2', uDark: '#1d2932', uRing: '#d3dde2', uGreen: '#3d7068', uConcrete: '#cfd0cb',
    quay: '#8d8576', quayLight: '#bcb4a2', quayDark: '#544e45', moss: '#4f5f38', shade: '#22262d',
    pave: '#a09d97', paveLight: '#cdcac1', paveDark: '#66655f', asphalt: '#6b6c6f', roadLine: '#e6e3da',
    girder: '#7c9c72', girderLight: '#aac59a', girderDark: '#48634a',
    tag: ['#7a3fa8', '#f2c230', '#e2562d', '#2f9aa0', '#2f55a6', '#f4f1e6', '#1c1e24', '#3f9a4f', '#e86aa0'],
    crate: '#2f55a6', crateTop: '#8fa3cc',
    coats: ['#b8453a', '#2c4f7c', '#e6dcc0', '#3f7a58'], skin: '#e0b89a', snow: '#eef2f4',
  },
  seasons: {
    spring: { water: '#6a7f4f', waterLight: '#b2c093', waterDark: '#3c5035' },
    autumn: { water: '#6f704a', waterLight: '#c3b68a', waterDark: '#40432f', moss: '#5c5a35' },
    winter: { water: '#5c6a61', waterLight: '#a5b1ac', waterDark: '#36433f', moss: '#47503c', girder: '#74917a' },
  },
  weathers: {                                                  // white is what snow makes it, and frost a little
    snow: { pave: '#e2e6e9', paveLight: '#ffffff', paveDark: '#b4bec9', asphalt: '#8b9198', quayLight: '#dfe3e4' },
    frost: { pave: '#b4b6b6', paveLight: '#e2e6e6', asphalt: '#7b7f85' },
  },
  ground: /^(pave|snow)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const wide = clamp((W / H - .75) / .6), y1 = 1 - .1 * wide; // a wide screen loses some of the water at our feet
    Y0 = .03 + .015 * wide; PH = H / (y1 - Y0); PW = PH * .75;
    const n = W / PW;                                          // how many photos wide the picture is
    X0 = n <= 1 ? 1 - n : Math.max(-2.1, -(n - 1) * .6); X1 = X0 + n;   // the tower stands right of the middle, the bridge has the left
    place.hz = py(SKYLINE);
    buildHouses(); buildTrees();
    const m = Math.ceil(W) + 1;
    tops = new Float32Array(m); landTop = H;
    for (let i = 0; i < m; i++) { tops[i] = py(roofAt(fx(i))); if (tops[i] < landTop) landTop = tops[i]; }
    landTop = Math.max(0, landTop - 8 * U);
    townFoot = py(HOR); waterTop = py(lineY(WATERLINE, Math.max(X0, CVX + .3)));
    const xb = Math.max(X0, -2.2), girderFoot = xb < ABUT ? py(bridgeY(DECKV + depthAt(xb), xb)) : 0;
    canalFoot = Math.min(H, Math.max(py(lineY(WATERLINE, X1)), girderFoot));
    tw = [Math.max(0, px(.56)), Math.max(0, py(.105)), Math.min(W, px(CR1)), py(STREET)];
    br = [Math.min(W, px(RAILEND)), py(bridgeY(FARRAIL, RAILEND)), Math.min(H, girderFoot)];
    townStroke = within(0, landTop, W, townFoot); towerStroke = within(tw[0], tw[1], tw[2], tw[3]);
    canalStroke = within(0, py(.59), W, canalFoot); bridgeStroke = within(0, br[1], br[0], br[2]); waterArea = within(0, waterTop, W, H);
  },
  tick(t) { time = t; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to the roofs, and round the slab where it hangs over
    const dip = 3 * U - pad, n = tops.length;
    let y = tops[n - 1] + dip;
    c.moveTo(0, -60); c.lineTo(W, -60); c.lineTo(W, y);
    for (let i = n - 2; i >= 0; i--) { const e = tops[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(0, y); c.closePath();
    const xr = px(SLAB.l1) + 1, g = pad - 3 * U;               // a hole: it runs the other way round
    c.moveTo(px(slabL(SLAB.top)) - g, py(SLAB.crown) - g); c.lineTo(px(SLAB.l1) - g, py(SLAB.foot)); c.lineTo(xr, py(SLAB.foot)); c.lineTo(xr, py(SLAB.crown) - g); c.closePath();
  },
  front(s, x, y) {                                             // the slab leans out over the sky
    const xf = fx(x), pf = fy(y);
    return xf >= SLAB.l0 - .004 && xf < SLAB.l1 && pf >= SLAB.crown && pf < SLAB.foot && xf >= slabL(Math.max(pf, SLAB.top)) - 1.5 * U / PW;
  },
  orb(o) {                                                     // the sun and the moon stand on the left, over the low roofs, clear of the tower
    const free = x => { for (let i = Math.max(0, x - o.r * 2 | 0); i < Math.min(tops.length, x + o.r * 2); i += 2) if (tops[i] < o.y + o.r * 2.2) return false; return x > o.r * 1.5 && x < W - o.r * 1.5; };
    const x0 = Math.min(W - o.x, px(.5));
    for (let d = 0; d < W; d += 4 * U) { if (free(x0 - d)) { o.x = x0 - d; return; } if (free(x0 + d)) { o.x = x0 + d; return; } }
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    clipBand(landTop, H);
    spend('dk-trunk', RATE.trunk * W / PW * pace, anyTrunk);
    spend('dk-town', RATE.town * W * (townFoot - landTop) * .5 / ua * pace, townStroke);
    if (tw[2] > tw[0]) spend('dk-tower', RATE.tower * (tw[2] - tw[0]) * (tw[3] - tw[1]) / ua * pace, towerStroke);
    spend('dk-canal', RATE.canal * W * (canalFoot - py(.59)) / ua * pace, canalStroke);
    if (br[0] > 0) spend('dk-bridge', RATE.bridge * br[0] * (br[2] - br[1]) / ua * pace, bridgeStroke);
    spend('dk-water', RATE.water * W * (H - waterTop) / ua * pace, waterArea);
    if (s && !s.night && s.traits.shadow) spend('dk-glint', RATE.glint * W * (H - waterTop) / ua * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('dk-fog', RATE.fog * W * .28 * PH / ua * pace, fogStroke);
    PP.haze = null;
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) if (cells[k].y > landTop - 30 * U) coatStroke(cells[k].x, cells[k].y);
    PP.haze = null;
    c.restore();
  },
  sprites: [{ draw: drawBoat, t0: 4, seed: 131 }, { draw: drawPromenade, t0: 3, seed: 132 }, { draw: drawBridgeLamps, t0: 3, seed: 133 }],
};
return place;
} });
