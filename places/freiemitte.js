/* ------------------------------------------------------------------ *
 *  Vienna, over the Freie Mitte: the wild park where the yard of the
 *  Nordbahnhof used to be, seen from above on a May evening.  The old
 *  water tower stands on the left, the new towers of the
 *  Nordbahnviertel behind the park, the town beyond them.
 *
 *  This one is painted after a photograph more closely than the other
 *  places: freiemitte-map.js, loaded before this file, says for every
 *  point under the sky what there is (leaves, grass, a path, a wall)
 *  and how light it is there, and every stroke asks it.  Only the water
 *  tower is drawn by hand, as the photograph has it in scaffolding.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'freiemitte', name: 'Freie Mitte', make(kit) {
'use strict';
const { PP, clamp, smooth, mix, pick, hash1, vnoise, fbm, wind, light, spend, clipBand } = kit;
const MAP = window.viennaMaps.freiemitte;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false;
const use = s => { pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph: x and y as fractions of its width and height.  A wide screen shows all
// of its width and a slice of its height, a narrow one a slice of its width, pushed together a bit.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
const HOR = .3845;                                             // where the town ends
const near = pf => clamp((pf - .3) / .7, .22, 1.3);           // how close the ground is at this height: the size of its strokes
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;

// ---------- the map ----------
// One character a cell: what it is, times eight, plus how light it is, from 0 to 7.  The town is mapped finer than
// the park, column by column from the roofs down: the sky above them is not in it.
const SKY = 0, FOL = 1, GRASS = 2, HARD = 3, EARTH = 4, WALL = 5, CREAM = 6, WARM = 7, TEAL = 8, YELLOW = 9, BLOSSOM = 10, TOWER = 11, BOX = 12, NET = 13, BARS = 14;
const CODE = new Uint8Array(128);
for (let c = 35, v = 0; c < 127; c++) if (c !== 92) CODE[c] = v++;
const unpack = s => { const a = new Uint8Array(s.length); for (let i = 0; i < s.length; i++) a[i] = CODE[s.charCodeAt(i)]; return a; };
const TOP = MAP.top, SPLIT = MAP.split, TWD = MAP.town.w, THT = MAP.town.h, PWD = MAP.park.w, PHT = MAP.park.h;
const town = unpack(MAP.town.cells), park = unpack(MAP.park.cells);
const roofs = new Uint16Array(TWD), colAt = new Uint32Array(TWD + 1);           // where each column of the town begins: its row, its place in the cells
{ const t = unpack(MAP.town.tops); for (let i = 0; i < TWD; i++) { roofs[i] = t[i * 2] * 91 + t[i * 2 + 1]; colAt[i + 1] = colAt[i] + THT - roofs[i]; } }
const roofAt = xf => TOP + roofs[clamp(xf * TWD | 0, 0, TWD - 1)] / THT * (SPLIT - TOP);   // the skyline

let lev = 0, cellI = 0, cellJ = 0;                             // what what() learnt on the way: how light it is, which cell
function what(xf, pf) {                                        // what there is at a point of the photo
  if (pf >= SPLIT) {
    const i = cellI = clamp(xf * PWD | 0, 0, PWD - 1), j = cellJ = clamp((pf - SPLIT) / (1 - SPLIT) * PHT | 0, 0, PHT - 1), v = park[j * PWD + i];
    lev = v & 7; return v >> 3;
  }
  const i = cellI = clamp(xf * TWD | 0, 0, TWD - 1), j = cellJ = (pf - TOP) / (SPLIT - TOP) * THT | 0;
  if (pf < TOP || j < roofs[i]) return SKY;
  const v = town[colAt[i] + Math.min(j, THT - 1) - roofs[i]];
  lev = v & 7; return v >> 3;
}

// ---------- the water tower ----------
// The photograph has it in scaffolding.  It is painted as it stands without: brown brick under a hipped roof of
// slate, seen from a little above and almost corner on, its left wall towards the evening sun.
const WL = [.118, .4605], WF = [.171, .4635], WR = [.209, .4575], WP = [.1625, .4425];   // the corners of the eaves: left, the near one, right; the peak
// The two walls: where each begins and ends under the eaves and on the ground.  They get lower as they go away from us.
const LWALL = [[.1215, .4625], [.171, .4645], [.172, .534], [.1235, .5275]], RWALL = [[.171, .4645], [.2065, .459], [.2055, .5245], [.172, .534]];
const side = (a, b, x, y) => (b[0] - a[0]) * (y - a[1]) - (b[1] - a[1]) * (x - a[0]);
const inTri = (a, b, c, x, y) => { const d1 = side(a, b, x, y), d2 = side(b, c, x, y), d3 = side(c, a, x, y); return !((d1 < 0 || d2 < 0 || d3 < 0) && (d1 > 0 || d2 > 0 || d3 > 0)); };
const CHIM = [.1725, .1755, .4425, .452];                      // the chimney: left, right, top, foot
const LEFTWALL = 1, RIGHTWALL = 2, LEFTROOF = 3, RIGHTROOF = 4, CHIMNEY = 5, VANE = 6;
let wu = 0, wv = 0;                                            // where on a wall a point is: along it, and down it
function onWall(q, xf, pf) {
  let u = (xf - q[0][0]) / (q[1][0] - q[0][0]), v = 0;
  for (let n = 0; n < 2; n++) {                                // its edges lean a little: twice round is close enough
    const top = q[0][1] + (q[1][1] - q[0][1]) * u, foot = q[3][1] + (q[2][1] - q[3][1]) * u;
    v = (pf - top) / (foot - top);
    const xl = q[0][0] + (q[3][0] - q[0][0]) * v, xr = q[1][0] + (q[2][0] - q[1][0]) * v;
    u = (xf - xl) / (xr - xl);
  }
  wu = clamp(u, 0, .999); wv = v;
  return u >= -.03 && u < 1.03 && v >= 0 && v < 1;      // a little more than all of it: no gap at the corner
}
function wallPoint(q, u, v) {                                  // and back: the point of the photo
  const xl = q[0][0] + (q[3][0] - q[0][0]) * v, xr = q[1][0] + (q[2][0] - q[1][0]) * v, top = q[0][1] + (q[1][1] - q[0][1]) * u, foot = q[3][1] + (q[2][1] - q[3][1]) * u;
  return [xl + (xr - xl) * u, top + (foot - top) * v];
}
function towerPart(xf, pf) {
  if (xf < WL[0] || xf > WR[0] || pf < WP[1] - .012 || pf > .536) return 0;
  if (Math.abs(xf - WP[0]) < .0006 && pf < WP[1] && pf > WP[1] - .011) return VANE;
  if (xf >= CHIM[0] && xf < CHIM[1] && pf >= CHIM[2] && pf < CHIM[3]) return CHIMNEY;
  if (inTri(WL, WF, WP, xf, pf)) return LEFTROOF;
  if (inTri(WF, WR, WP, xf, pf)) return RIGHTROOF;
  return onWall(LWALL, xf, pf) ? LEFTWALL : onWall(RWALL, xf, pf) ? RIGHTWALL : 0;
}
// Each wall has two bays.  In each: three arches above, the middle one a window, the others bricked up; a belt of
// stone; a big arch with a fan light in it and a door under that.  A drain pipe runs down between the bays.
const BRICK = 0, RECESS = 1, STONE = 2, GLASS = 3, DARK = 4, FRAME = 5, PIPE = 6;
function towerFeature(left, u, v) {
  if (v < .045) return DARK;                                   // the shade under the eaves
  if (left && Math.abs(u - .5) < .008) return PIPE;
  if (u < .035 || u > .965) return BRICK;
  const bu = u * 2 - Math.floor(u * 2);
  if (v < .09) return (Math.floor(bu * 14) & 1) ? RECESS : BRICK;            // a frieze under the cornice
  if (v >= .435 && v < .47) return STONE;                                     // the belt between the floors
  if (v < .435) {
    for (const c of [.21, .5, .79]) {
      const d = Math.abs(bu - c), hw = .088;
      if (d >= hw) continue;
      if (v >= .37 && v < .395) return STONE;                                 // the sill
      if ((v >= .21 && v < .37) || (v >= .15 && v < .21 && Math.hypot(d / hw, (.21 - v) / .06) < 1))
        return c !== .5 ? RECESS : d > hw * .7 || Math.abs(v - .29) < .01 ? FRAME : GLASS;
    }
    return BRICK;
  }
  const d = Math.abs(bu - .5);
  if (d < .36 && (v >= .62 || (v >= .52 && Math.hypot(d / .36, (.62 - v) / .1) < 1))) {   // the big arch
    if (d < .22 && v >= .6 && v < .705 && (v >= .66 || Math.hypot(d / .22, (.66 - v) / .06) < 1)) return d < .007 ? FRAME : GLASS;   // its fan light
    if (d < .22 && v >= .705 && v < .72) return FRAME;
    if (d < .115 && (v >= .82 || (v >= .78 && Math.hypot(d / .115, (.82 - v) / .04) < 1))) return DARK;   // the door
    return RECESS;
  }
  return BRICK;
}

const CRATE = [.705, .7315, .692, .7235];                      // the container with the graffiti on it: left, right, top, foot
// The football cage down on the right: walls of steel bars, a green net over the court.  It is drawn too: bars are
// too thin for the map.  The net; the wall along the promenade and the one towards us, which meet at CORNER.
const NETS = [[.805, .733], [.9, .737], [.915, .766], [1.01, .772], [1.01, .868], [.895, .862]], WALLS = [[.8, .733], [.895, .862], [1.01, .868], [1.01, 1.01], [.9, 1.01], [.86, .93], [.8, .8]], CORNER = .895;
function inPoly(p, x, y) {
  let o = false;
  for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o;
  return o;
}

// ---------- whose a point is ----------
let zx = 0, zy = 0, zp = 0;                                   // what zone() learnt on the way: x and y on the photo, the part of the tower
function zone(x, y) {
  const xf = zx = fx(x), pf = zy = fy(y);
  if ((zp = towerPart(xf, pf))) return TOWER;
  if (xf >= CRATE[0] && xf < CRATE[1] && pf >= CRATE[2] && pf < CRATE[3]) return BOX;
  if (xf > .8 && pf > .73 && what(xf, pf) !== FOL) { if (inPoly(NETS, xf, pf)) return NET; if (inPoly(WALLS, xf, pf)) return BARS; }
  return what(xf, pf);
}
const GREEN = k => k === FOL || k === GRASS || k === BLOSSOM;
const reach = [0, 0];
function run(x, y, ca, sa, l, ok, step) {                      // how far a stroke may go either way along (ca, sa) while ok() says so
  reach[0] = reach[1] = l;
  for (let d = step; d < l + step; d += step) if (!ok(zone(x + ca * d, y + sa * d))) { reach[1] = Math.min(l, d - step * .5); break; }
  for (let d = step; d < l + step; d += step) if (!ok(zone(x - ca * d, y - sa * d))) { reach[0] = Math.min(l, d - step * .5); break; }
}
function room(x, y, nx, ny, w) {                               // how broad a green stroke may be here: no broader than to the next thing that is not green
  const h = w / 2;
  return GREEN(zone(x + nx * h, y + ny * h)) && GREEN(zone(x - nx * h, y - ny * h)) ? w : GREEN(zone(x + nx * h * .5, y + ny * h * .5)) && GREEN(zone(x - nx * h * .5, y - ny * h * .5)) ? w * .5 : Math.min(w, 1.2 * U);
}
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}

// ---------- the town ----------
// Houses are painted in level strokes, a cell high: floor by floor, window by window.
let cw = 1, ch = 1;                                            // how big a cell of the town map is in the picture
const HERO = xf => (xf > .246 && xf < .413) || (xf > .654 && xf < .787);       // the two towers stand close by
function farOff(xf, pf) { return pf > .5 ? 0 : HERO(xf) ? .04 : pf < .392 && !(xf > .93 && xf < .985) ? .5 : pf < .42 ? .2 : .1; }
const hazed = (col, xf, pf) => { const f = farOff(xf, pf), h = f * .7 + traits.fog * (.15 + f) + traits.rain * f * .3; return h > 0 ? mix(col, pal.cityFar, clamp(h, 0, .85)) : col; };
function wallCol(k, l, r) {
  const t = (l + r) / 8;
  if (k === WARM) return l < 4 ? ramp(pal.pinks, (l + r) / 4) : ramp(pal.reds, (l - 4 + r) / 4);
  return ramp(k === CREAM ? pal.creams : k === YELLOW ? pal.yellows : k === TEAL ? pal.teals : pal.walls, t);
}
function wallStroke(x, y, k) {
  const R = PP.R, xf = zx, pf = zy, l0 = lev, fine = pf < SPLIT, i = cellI, j = cellJ;
  // Stroke by stroke a cell high, in rows; on a big screen, where a cell is more than a stroke, the rows would show as steps
  const rows = fine && ch < 1.7 * U, w = rows ? Math.max(1.1 * U, ch) : 1.5 * U, yc = rows ? py(TOP + (j + .5) / THT * (SPLIT - TOP)) : y;
  run(x, yc, 1, 0, (1.5 + R() * 3.5) * Math.max(U, cw * .7), q => q === k && lev === l0, Math.max(1, cw * .5));
  let col = wallCol(k, l0, .3 + R() * .4);
  // a window is dark by day; at night there is a light in some
  const wi = i / 3 | 0, wj = j / 3 | 0;
  if (night && l0 <= 2 && k !== TEAL && pf < .535 && hash1(wi * 7919 + wj * 131) < (HERO(xf) ? .17 : .13)) col = mix(pal.lights[(hash1(wi * 13 + wj * 7) * pal.lights.length) | 0], pal.dark, farOff(xf, pf) * .5);
  else { col = hazed(col, xf, pf); const sd = shadow(x, y); if (sd) col = mix(col, pal.dark, sd * .16); }
  PP.brush(x - reach[0], yc, x + reach[1], yc, w, col, { a: .96, jit: 3, dry: 0, bow: 0, sp: Math.min(1.1 * U, w) });
}

// ---------- the park ----------
const EVERGREEN = [[.425, .612, .014], [.5, .59, .012], [.535, .64, .014], [.012, .58, .02]];   // the pines keep their needles: [x, y, how far around]
const evergreen = (xf, pf) => { for (const e of EVERGREEN) if (Math.hypot(xf - e[0], (pf - e[1]) * .75) < e[2]) return true; return false; };
function leafStroke(x, y, k) {
  const R = PP.R, xf = zx, pf = zy, sz = (.42 + near(pf) * .34) * U, q = R();
  let l0 = lev;
  if (GREEN(zone(x - wind(time) * 1.5 * sz, y))) l0 = lev;      // the wind moves the light about in them
  const sd = shadow(x, y), far = pf < .5 ? hazed : c => c;
  if (bare && !evergreen(xf, pf)) {                            // winter: twigs, and the snow under and on them
    if (q < .3 + l0 * .07) { PP.dab(x, y, (2.5 + R() * 5) * sz, ramp(pal.meadow, (l0 + R()) / 8), { a: .9, den: .6, core: .5, sx: 1.6, sy: .6 }); return; }
    const a = -Math.PI / 2 + (R() - .5) * 2.6, l = (5 + R() * 10) * sz;
    PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 3 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
      far(l0 < 3 ? pal.folDeep : q < .8 ? pal.folDark : pal.folMid, xf, pf), (.8 + R() * .9) * sz, { pass: 1, a: .8 });
    return;
  }
  if (k === BLOSSOM && !bare && season !== 'autumn') {         // the elder in flower: flat white umbels
    PP.dab(x, y, (1.6 + R() * 2.2) * sz, mix(pal.blossom, pal.folLight, l0 < 3 ? .35 : q * .15), { a: .95, den: .9, core: .7, s: 1.3 * U });
    return;
  }
  const t = clamp(.5 + ((l0 + .15 + R() * .7) / 8 - .5) * 1.3);                                   // a bit harder than the camera saw it: deep shade, bright tops
  let col = t < .3 ? mix(pal.folDeep, pal.folDark, t / .3) : t < .58 ? mix(pal.folDark, pal.folMid, (t - .3) / .28) : t < .86 ? mix(pal.folMid, pal.folLight, (t - .58) / .28) : mix(pal.folLight, pal.folSun, (t - .86) / .14 * (pal.accent ? .5 : .8));
  if (bare) col = l0 > 5 && q < .5 ? pal.snowy : mix(col, pal.pine, .6);                         // a pine, snow on its boughs
  else if (pal.accent && l0 > 2 && q > (season === 'spring' ? .86 : .93)) col = pick(pal.accent, R());
  if (sd) col = mix(col, pal.folDeep, sd * .45);
  const a = R() * Math.PI, ca = Math.cos(a), sa = Math.sin(a), l = (2.5 + R() * 5) * sz;
  run(x, y, ca, sa, l, GREEN, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], room(x, y, -sa, ca, (3 + R() * 4.5) * sz), far(col, xf, pf), { a: .87, sp: 1.25 * U, dry: .18, jit: 9 });
}
function grassStroke(x, y) {                                   // long grass, every blade upright
  const R = PP.R, pf = zy, sz = (.25 + near(pf) * .6) * U, l0 = lev, q = R(), sd = shadow(x, y);
  let col = ramp(pal.meadow, (l0 + R()) / 8);
  if (!bare && l0 > 2) { const dry = smooth(.52, .7, fbm(x * .006 / U + 11, y * .012 / U)); if (dry > 0) col = mix(col, pal.straw, dry * .3); if (q > .92) col = mix(col, pal.straw, .4); else if (q < .1) col = mix(col, pal.meadow[1], .4); }
  if (sd) col = mix(col, pal.meadow[0], sd * .5);
  const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .07, ca = Math.cos(a), sa = Math.sin(a), l = (2 + R() * 4.5) * sz;
  run(x, y, ca, sa, l, GREEN, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], room(x, y, -sa, ca, (1.3 + R() * 2.3) * sz), col, { a: .87, jit: 8, dry: .18, sp: 1.1 * U, bow: .12 });
}
const DIRS = []; for (let i = 0; i < 8; i++) DIRS.push([Math.cos(i * Math.PI / 8), Math.sin(i * Math.PI / 8)]);
function groundStroke(x, y, k) {                               // paths, boards, sand: every stroke the way the thing runs
  const R = PP.R, xf = zx, pf = zy, sz = near(pf) * U, l0 = lev, same = q => q === k, step = Math.max(1.5, 1.6 * sz), sd = shadow(x, y);
  let best = 0, bl = -1; const o = (R() * 8) | 0, far = (5 + R() * 9) * sz;
  for (let n = 0; n < 8; n++) { const dd = DIRS[(n + o) & 7]; run(x, y, dd[0], dd[1], far, same, step); if (reach[0] + reach[1] > bl) { bl = reach[0] + reach[1]; best = (n + o) & 7; } }
  const d = DIRS[best]; run(x, y, d[0], d[1], (3 + R() * 6) * sz, same, step);
  zx = xf; zy = pf;
  let col = ramp(k === EARTH ? pal.earths : k === TEAL ? pal.teals : pal.hards, (l0 + R()) / 8);
  if (sd) col = mix(col, (k === EARTH ? pal.earths : pal.hards)[0], sd * .3);
  if (pf < .5) col = hazed(col, xf, pf);
  if (night && k !== HARD && xf > .8 && pf > .72) col = mix(col, pal.lights[0], .1);                 // the cage under its floodlight
  PP.brush(x - d[0] * reach[0], y - d[1] * reach[0], x + d[0] * reach[1], y + d[1] * reach[1], (1.3 + R() * 1.7) * sz, col, { a: .88, jit: 6, dry: .1, sp: Math.min(1.1 * U, 1.3 * sz + .3), bow: .03 });
}
function paintAt(x, y, k) {
  if (k === FOL || k === BLOSSOM) leafStroke(x, y, k);
  else if (k === GRASS) grassStroke(x, y);
  else if (k === HARD || k === EARTH || (k === TEAL && zy > .6)) groundStroke(x, y, k);
  else if (k !== SKY) wallStroke(x, y, k);
}
function townStroke() {                                        // somewhere in the town: every cell of its map as often as any other
  const R = PP.R, n = (R() * town.length) | 0;
  let a = 0, b = TWD - 1; while (a < b) { const m = (a + b + 1) >> 1; if (colAt[m] <= n) a = m; else b = m - 1; }
  const x = px((a + R()) / TWD), y = py(TOP + (roofs[a] + n - colAt[a] + R()) / THT * (SPLIT - TOP));
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y)) return;
  const k = zone(x, y);
  if (k < TOWER) paintAt(x, y, k);
}
let parkTop = 0;
function parkStroke() {
  const R = PP.R, x = R() * W, y = parkTop + R() * (H - parkTop);
  if (!at(x, y)) return;
  const k = zone(x, y);
  if (k < TOWER) paintAt(x, y, k);
}

// ---------- the water tower ----------
let CH = 2, wtX0 = 0, wtX1 = 0, wtY0 = 0, wtY1 = 0;
function brickCol(row, x) {                                    // brown bricks, a few lighter, a few darker, weathered pale in patches
  const h = hash1(row * 97 + Math.floor(x / (3.5 * U)) * 13), n = fbm(x * .05 / U, row * .7);
  const c = pal.brick[(h * pal.brick.length) | 0];
  return n > .58 ? mix(c, pal.ledge, .16 + (n - .58) * 1.1) : c;
}
function towerStroke() {
  const R = PP.R, x = wtX0 + R() * (wtX1 - wtX0), y = wtY0 + R() * (wtY1 - wtY0);
  if (!at(x, y) || zone(x, y) !== TOWER) return;
  const xf = zx, pf = zy, part = zp, u = wu, v = wv, o = { a: .94, jit: 5, dry: .04, bow: 0, sp: Math.min(1.1 * U, CH) };
  const lit = part === LEFTWALL || part === LEFTROOF;           // the evening sun is on its left
  const tone = c => hazed(lit ? mix(c, pal.brickSun, .3) : mix(c, pal.brickShade, .3), xf, pf);
  if (part === VANE) { PP.brush(x, py(WP[1] - .011), x, py(WP[1]), Math.max(.8 * U, .0009 * PW), pal.ink, o); if (R() < .3) PP.brush(x - 2 * U, py(WP[1] - .008), x + 2.4 * U, py(WP[1] - .0085), .9 * U, pal.ink, o); return; }
  if (part === CHIMNEY) {
    const yc = Math.round(y / CH) * CH;
    PP.brush(px(CHIM[0]) + .3 * U, yc, px(CHIM[1]) - .3 * U, yc, CH * .9, tone(pf < CHIM[2] + .0016 ? (bare ? pal.snowy : pal.ledge) : brickCol(yc / CH | 0, x)), o);
    return;
  }
  if (part === LEFTROOF || part === RIGHTROOF) {               // slate, laid in courses along the eaves; zinc on the hips, snow on it in winter
    const A = lit ? WL : WF, B = lit ? WF : WR, a = Math.atan2((B[1] - A[1]) * PH, (B[0] - A[0]) * PW);
    const ca = Math.cos(a), sa = Math.sin(a), n = fbm(x * .05 / U + 4, y * .2 / U);
    const hip = Math.abs(side(WF, WP, xf, pf)) / Math.hypot((WP[0] - WF[0]) * PW, (WP[1] - WF[1]) * PH) * PW * PH;
    const eave = Math.abs(side(A, B, xf, pf)) / Math.hypot((B[0] - A[0]) * PW, (B[1] - A[1]) * PH) * PW * PH;
    let col = lit ? mix(pal.slate, pal.slateLight, .4 + n * .4) : mix(pal.slate, pal.slateLight, n * .25);
    if (eave < 1.5 * U) col = mix(pal.zinc, pal.slate, .35);
    else if (hip < 1.2 * U) col = mix(pal.zinc, pal.slateLight, .4);
    else if (bare && R() < .8) col = lit ? pal.snowy : mix(pal.snowy, pal.slate, .18);
    run(x, y, ca, sa, (2 + R() * 4) * U, q => q === TOWER && zp === part, 1.5 * U);
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], 1.5 * U, hazed(col, xf, pf), o);
    return;
  }
  // a wall: course by course, each stroke as long as the same thing goes on
  const left = part === LEFTWALL, q = left ? LWALL : RWALL, wide = (q[1][0] - q[0][0]) * PW, high = (q[3][1] - q[0][1]) * PH;
  const rows = Math.max(8, Math.round(high / CH)), row = Math.min(rows - 1, v * rows | 0), vc = (row + .5) / rows;
  const f = towerFeature(left, u, v), du = 1 / Math.max(1, wide), lim = (2 + R() * 5) * U * du;
  let ua = u, ub = u;
  while (ua - du > 0 && u - ua < lim && towerFeature(left, ua - du, v) === f) ua -= du;
  while (ub + du < 1 && ub - u < lim && towerFeature(left, ub + du, v) === f) ub += du;
  const a = wallPoint(q, ua, vc), b = wallPoint(q, Math.max(ub, ua + du * .5), vc);
  let col = f === BRICK ? brickCol(row, x) : f === RECESS ? mix(brickCol(row, x), pal.brickShade, .55)
    : f === STONE ? (bare && v < .5 && R() < .5 ? pal.snowy : mix(pal.ledge, pal.stone, R() * .3)) : f === FRAME ? mix(pal.frame, pal.brick[0], .3)
    : f === GLASS ? (night && hash1((u * 4 | 0) + (left ? 9 : 0) + (v * 3 | 0) * 5) < .4 ? pal.lights[1] : mix(mix(pal.dark, pal.glass, .55), pal.skyMid, .15 + R() * .25)) : f === PIPE ? pal.ink : pal.brickShade;
  const sd = shadow(x, y); if (sd) col = mix(col, pal.brickShade, sd * .25);
  PP.brush(px(a[0]), py(a[1]), px(b[0]), py(b[1]), high / rows * .95, f === GLASS && night ? col : tone(col), o);
}
function crateStroke() {                                       // the container: blue, ribbed, a tag sprayed across it
  const R = PP.R, x = px(CRATE[0]) + R() * (CRATE[1] - CRATE[0]) * PW, y = py(CRATE[2]) + R() * (CRATE[3] - CRATE[2]) * PH;
  if (!at(x, y) || zone(x, y) !== BOX) return;
  const xf = zx, pf = zy, top = pf < CRATE[2] + .004, rib = Math.floor((xf - CRATE[0]) / .0016), o = { a: .92, jit: 4, dry: 0, bow: 0, sp: U };
  let col = top ? (bare ? pal.snowy : pal.crateTop) : mix(pal.crate, pal.dark, (rib & 1 ? .2 : 0) + (xf < CRATE[0] + .004 ? .35 : 0));
  if (!top && xf > CRATE[0] + .006 && pf > CRATE[2] + .01 && pf < CRATE[3] - .008 && vnoise(xf * 900, pf * 700) > .5) col = pick(pal.graffiti, vnoise(xf * 300 + 7, pf * 300));
  if (top) PP.brush(x - 2 * U, y, x + 2 * U, y, 1.4 * U, col, o);
  else PP.brush(x, Math.max(py(CRATE[2] + .004), y - 2 * U), x, Math.min(py(CRATE[3]), y + 2 * U), 1.4 * U, col, o);
}

let cageX = 0, cageY = 0;
function cageStroke() {
  const R = PP.R, x = cageX + R() * (W - cageX), y = cageY + R() * (H - cageY);
  if (x < 0 || y >= H || !at(x, y)) return;
  const z = zone(x, y);
  if (z !== NET && z !== BARS) return;
  const xf = zx, pf = zy, sz = near(pf) * U, l0 = lev, t = (l0 + R()) / 8, k = what(xf, pf), glow = night ? .14 : 0, same = q => q === z;
  if (z === NET) {                                             // the net sags between its ropes; the court shows through it
    if (R() < .3) {                                            // its mesh
      const a = R() < .5 ? .5 : -1, ca = Math.cos(a), sa = Math.sin(a);
      run(x, y, ca, sa, (4 + R() * 8) * sz, same, 2 * sz);
      PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], .7 * U, mix(ramp(pal.teals, t + .3), pal.lights[0], glow), { a: .5, jit: 4, dry: 0, sp: .7 * U, bow: 0 });
      return;
    }
    const col = k === TEAL ? ramp(pal.teals, t * .8 + .15) : mix(ramp(pal.earths, t), pal.teals[2], .45);
    run(x, y, 1, .06, (3 + R() * 6) * sz, same, 2 * sz);
    PP.brush(x - reach[0], y - reach[0] * .06, x + reach[1], y + reach[1] * .06, (1.5 + R() * 2) * sz, bare && l0 > 3 ? mix(col, pal.snowy, .5) : mix(col, pal.lights[0], glow), { a: .88, jit: 6, dry: .1, sp: U, bow: 0 });
    return;
  }
  // a wall: every bar upright, a post now and then, the court behind them
  const pitch = .0068, ref = xf < CORNER ? xf - (pf - .733) * .7 : xf, m = ((ref - CORNER) / pitch % 1 + 1) % 1, n = Math.round((ref - CORNER) / pitch);
  const post = Math.abs(xf - CORNER) < .002 || (m > .78 && n % 5 === 0), rail = xf > CORNER && Math.abs(((pf - .862) / .017 % 1 + 1) % 1 - .5) > .44;
  if (post || m < .3) {
    const w = (post ? 2.3 : 1) * U * clamp(sz / U + .2, .7, 1.3);
    run(x, y, 0, 1, (4 + R() * 9) * sz, same, 2 * sz);
    PP.brush(x, y - reach[0], x, y + reach[1], w, mix(mix(pal.steel[0], pal.steel[1], clamp(t * 1.2 + (post ? .2 : 0))), pal.lights[0], glow), { a: .92, jit: 4, dry: 0, sp: Math.min(U, w), bow: 0 });
    return;
  }
  const col = rail ? mix(pal.steel[0], pal.steel[1], t) : bare ? ramp(pal.meadow, t) : mix(ramp(pal.earths, t * .8 + .1), pal.hards[1], xf < CORNER ? .3 : .12);
  run(x, y, 1, 0, (2 + R() * 4) * sz, same, 2 * sz);
  PP.brush(x - reach[0], y, x + reach[1], y, (1.2 + R() * 1.4) * sz, mix(col, pal.lights[0], glow), { a: .88, jit: 6, dry: .1, sp: U, bow: 0 });
}

// ---------- lamps, weather ----------
const LAMPS = [[.7075, .574], [.7275, .588], [.787, .725], [.759, .606], [.925, .886], [.655, .54], [.48, .515], [.215, .578], [.118, .598]];
const FLOOD = [.985, .43];
function lampDab() {                                           // the lamps along the paths, the floodlight over the cage
  const R = PP.R, f = R() < .25, l = f ? FLOOD : pick(LAMPS, R()), x = px(l[0]), y = py(l[1]);
  if (x < 0 || x >= W || !at(x, y) || !night) return;
  PP.dab(x, y + (f ? 6 : 3) * U, (f ? 16 : 6) * U * near(l[1]) * 1.4, pal.lights[0], { a: .1, den: .4, s: 3 * U, jit: 3 });
  light(x, y, (f ? 2.2 : 1.1) * U);
}
function fogStroke() {                                         // fog lies over the park in drifting banks
  const R = PP.R, x = R() * W, depth = R(), y = py(.5 + depth * .24);
  if (!at(x, y) || !traits.fog || zone(x, y) >= TOWER) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .45, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y = parkTop + R() * (H - parkTop);
  if (!at(x, y) || !traits.glint || zone(x, y) !== GRASS || lev < 4) return;
  PP.dab(x, y, (.6 + R() * .8) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x0, y0) {                                  // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R;
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U) continue;
    const k = zone(clamp(x, 0, W - 1), Math.min(y, H - 1)), xf = zx, pf = zy, l = (14 + R() * 8) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
    if (k === SKY) continue;
    if (k === TOWER) { PP.brush(x, y - 10 * U, x, y + 10 * U, 14 * U, zp >= LEFTROOF ? pal.slate : mix(pal.brick[0], zp === LEFTWALL ? pal.brickSun : pal.brickShade, .25), o); continue; }
    if (k === BOX) { PP.brush(x - 4 * U, y, x + 4 * U, y, 6 * U, pal.crate, o); continue; }
    if (k === NET || k === BARS) { PP.brush(x - l, y, x + l, y, 22 * U, k === NET ? pal.teals[2] : mix(pal.earths[2], pal.steel[0], .5), o); continue; }
    const t = k === FOL ? .34 : k === GRASS ? .5 : k === BLOSSOM ? .5 : (Math.max(lev, 3) + .5) / 8;
    const col = k === FOL || k === BLOSSOM ? (bare && !evergreen(xf, pf) ? mix(pal.meadow[1], pal.folDark, .3) : mix(pal.folDark, pal.folMid, .45)) : k === GRASS ? ramp(pal.meadow, t)
      : k === HARD ? ramp(pal.hards, t) : k === EARTH ? ramp(pal.earths, t) : wallCol(k, Math.max(lev, 3), .5);
    if (k === FOL || k === BLOSSOM) { const a = (R() - .5) * 1.2; PP.brush(x - Math.cos(a) * l * .9, y - Math.sin(a) * l * .9, x + Math.cos(a) * l * .9, y + Math.sin(a) * l * .9, 26 * U, col, o); }
    else if (k === GRASS || k === HARD || k === EARTH) PP.brush(x - l, y, x + l, y, 22 * U, pf < .5 ? hazed(col, xf, pf) : col, o);
    else PP.brush(x, y - l * .8, x, y + l * .8, 24 * U, hazed(col, xf, pf), o);      // a house: an upright stroke
  }
}

// ---------- what moves ----------
// People walk up and down the promenade, now and then.
const WALK = [[.668, .56], [.69, .6], [.71, .65], [.725, .7], [.752, .75], [.785, .81], [.82, .86], [.86, .92], [.9, .98]];
function onWalk(u) {                                           // a point along the promenade, 0 at its far end
  const n = WALK.length - 1, f = clamp(u) * n, i = Math.min(n - 1, Math.floor(f)), t = f - i;
  return [WALK[i][0] + (WALK[i + 1][0] - WALK[i][0]) * t, WALK[i][1] + (WALK[i + 1][1] - WALK[i][1]) * t];
}
const WALKERS = [[0, 70, 1, .35, 0], [23, 80, -1, -.3, 1], [41, 62, 1, .1, 2], [55, 90, -1, .3, 3]];   // [when, how long the walk takes, which way, which side, coat]
function drawWalkers(P, t) {
  for (const [t0, dur, dir, lane, k] of WALKERS) {
    const u = ((t - t0) % 100 + 100) % 100 / dur;
    if (u > 1) continue;
    const [xf, pf] = onWalk(dir > 0 ? u * .9 + .04 : .94 - u * .9), d = near(pf), x = px(xf + lane * .02 * d), y = py(pf);
    if (x < 0 || x > W || y < 0 || y > H || !at(x, y) || zone(x, y) !== HARD) continue;
    const h = .03 * PH * d, step = Math.sin(t * 7 + k) * h * .16, o = { a: .95, sp: Math.max(.6 * U, h * .06), dry: 0, jit: 4, bow: 0 };
    if (!night && traits.shadow) P.brush(x, y, x + h * .6, y - h * .1, h * .14, pal.hards[0], { a: .3, sp: U, dry: 0, jit: 2, bow: 0 });   // the evening sun throws it out long
    P.brush(x, y - h * .45, x - step, y, h * .1, pal.ink, o);
    P.brush(x, y - h * .45, x + step, y, h * .1, pal.ink, o);
    P.brush(x, y - h * .85, x, y - h * .42, h * .2, pal.coats[k % pal.coats.length], o);
    P.dab(x, y - h * .95, h * .09, pal.skin, { a: 1, den: 2, core: .9, s: Math.max(.5 * U, h * .05), jit: 3 });
  }
}

// ---------- the painting, one frame's worth ----------
let tops = null, landTop = 0, townArea = 0;
const RATE = { town: 19000, park: 13000, tower: 17000, lamp: 6, crate: 14000, cage: 10000, glint: 90, fog: 1500 };
const place = {
  colors: {
    walls: ['#353e4a', '#6b7584', '#abb3be', '#d5d6d4', '#efede6', '#fcfaf3'], creams: ['#2d323a', '#6b6558', '#a89f8a', '#d5c9ae', '#ebe0c6', '#f8f0da'],
    pinks: ['#6e4a44', '#b98274', '#e0a996', '#f3cbba'], reds: ['#6a2f25', '#ad4630', '#d25c3d', '#ec8259'],
    yellows: ['#8f7436', '#c9a655', '#e6cb7c'], teals: ['#123f38', '#237565', '#3f9d88', '#7cc4ad', '#c2ddd6'],
    hards: ['#2e3138', '#4d5361', '#767e8e', '#a2a8b1', '#d0d0cb', '#edece6'], earths: ['#6a563b', '#9c8663', '#c4ae86', '#dfcda5', '#f0e4c6'],
    meadow: ['#1d381c', '#365e2a', '#547f38', '#779944', '#a2b85a'], straw: '#b9ab62',
    brick: ['#6a4a3a', '#5a4034', '#7a5644', '#4f392e', '#86604c'], brickShade: '#2a1f1b', brickSun: '#c08a68', ledge: '#c2b6a2', frame: '#ece8de', stone: '#8c8579',
    slate: '#484c55', slateLight: '#8a8f98', zinc: '#aeb2b6', snowy: '#eef2f4', pine: '#2f5a3c', blossom: '#f6f3e2',
    steel: ['#6f7884', '#d5d8da'],
    crate: '#2f55a6', crateTop: '#9aabcb', graffiti: ['#ffffff', '#d83a3a', '#7ec8f0', '#f2d03b'],
    coats: ['#b8453a', '#2f3e5c', '#e2d6b8', '#4f7a5a'], skin: '#e0b89a',
  },
  seasons: {
    spring: { meadow: ['#24481f', '#487c2e', '#72a83c', '#9cc64e', '#c6de70'], straw: '#b9c66a' },
    autumn: { meadow: ['#3d3320', '#6f602c', '#9c8838', '#c0a84c', '#e0cc74'], straw: '#d6b45a' },
    winter: {
      meadow: ['#8597ae', '#aebccc', '#d0d9e2', '#e9edf1', '#ffffff'], straw: '#ffffff',
      hards: ['#4a505a', '#7b848f', '#aeb7c1', '#d8dee4', '#eef1f3', '#ffffff'], earths: ['#8b97a8', '#b7c2ce', '#d8dfe6', '#edf0f3', '#ffffff'], crateTop: '#f2f4f5',
    },
  },
  ground: /^(meadow|hards|earths)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const r = W / H;
    if (r >= 4 / 3) {                                          // all of its width, and as much of its height as fits: the sky above the high tower stays
      PW = W; PH = PW * .75; X0 = 0;
      Y0 = clamp(.075, 0, Math.max(0, 1 - H / PH));            // a very wide one loses the near end of the park
    } else {                                                   // all of its height, a slice of its width, pushed together a bit
      PH = H; PW = PH * 4 / 3 * Math.max(.5, r * .75); Y0 = 0;
      X0 = clamp(.445 - W / PW / 2, 0, Math.max(0, 1 - W / PW));   // from the water tower to the high one
    }
    place.hz = py(HOR);
    cw = PW / TWD; ch = PH * (SPLIT - TOP) / THT; CH = Math.max(1.5 * U, .0023 * PH);
    const n = Math.ceil(W) + 1;
    tops = new Float32Array(n); landTop = H;
    for (let i = 0; i < n; i++) { tops[i] = py(roofAt(fx(i))); if (tops[i] < landTop) landTop = tops[i]; }
    landTop -= 6 * U;
    townArea = town.length * cw * ch * clamp(W / PW);
    parkTop = py(SPLIT);
    cageX = Math.max(0, px(.8)); cageY = py(.73);
    wtX0 = px(WL[0]); wtX1 = px(WR[0]); wtY0 = py(WP[1] - .012); wtY1 = py(.536);
  },
  tick(t) { time = t; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to the roofs
    const dip = 3 * U - pad, n = tops.length;
    let y = tops[n - 1] + dip;
    c.moveTo(0, -60); c.lineTo(W, -60); c.lineTo(W, y);
    for (let i = n - 2; i >= 0; i--) { const e = tops[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(0, y); c.closePath();
  },
  orb(o) {                                                     // the sun and the moon keep clear of the towers
    const free = x => { for (let i = Math.max(0, x - o.r * 2 | 0); i < Math.min(tops.length, x + o.r * 2); i += 2) if (tops[i] < o.y + o.r * 2) return false; return true; };
    for (let d = 0; d < W; d += 4 * U) { if (free(o.x - d)) { o.x -= d; return; } if (o.x + d < W - o.r * 2 && free(o.x + d)) { o.x += d; return; } }
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    clipBand(landTop, H);
    spend('fm-town', RATE.town * townArea / ua * pace, townStroke);
    spend('fm-park', RATE.park * W * (H - parkTop) / ua * pace, parkStroke);
    spend('fm-tower', RATE.tower * (wtX1 - wtX0) * (wtY1 - wtY0) / ua * pace, towerStroke);
    if (cageX < W && cageY < H) spend('fm-cage', RATE.cage * (W - cageX) * (H - cageY) / ua * pace, cageStroke);
    spend('fm-crate', RATE.crate * (CRATE[1] - CRATE[0]) * PW * (CRATE[3] - CRATE[2]) * PH / ua * pace, crateStroke);
    if ((s && s.night) || (o && o.night)) spend('fm-lamp', RATE.lamp * pace, lampDab);
    if (s && s.traits.glint) spend('fm-glint', RATE.glint * W * (H - parkTop) / ua * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('fm-fog', RATE.fog * W * .25 * PH / ua * pace, fogStroke);
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) if (cells[k].y > landTop - 30 * U) coatStroke(cells[k].x, cells[k].y);
    c.restore();
  },
  sprites: [{ draw: drawWalkers, t0: 3, seed: 121 }],
};
return place;
} });
