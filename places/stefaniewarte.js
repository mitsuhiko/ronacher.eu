/* ------------------------------------------------------------------ *
 *  Vienna, on top of the Kahlenberg: the Stefaniewarte, the old
 *  lookout tower in its clearing among the trees, hung with the dishes
 *  and aerials it has to carry these days, on a bright day in May.
 *
 *  Like the Freie Mitte this one is painted after a photograph closely:
 *  stefaniewarte-map.js, loaded before this file, says for every point
 *  of it what there is (sky, leaves, grass, gravel) and how light it is
 *  there.  The tower is not in the map: it is drawn by hand, stone by
 *  stone.  Nor are the people who were about that day.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'stefaniewarte', name: 'Stefaniewarte', make(kit) {
'use strict';
const { PP, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend } = kit;
const MAP = window.viennaMaps.stefaniewarte;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
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

// ---------- the map ----------
// One character a cell: what it is, times eight, plus how light it is, from 0 to 7.  For a wisp of leaves that is
// how much of the cell they fill.
const SKY = 0, LEAF = 1, WISP = 2, GRASS = 3, GRAVEL = 4, TOWER = 5, THIN = 6, LIMB = 7, THICKET = 8;
const MW = MAP.w, MH = MAP.h, GROUND = MAP.ground, cells = new Uint8Array(MW * MH);
{ const code = new Uint8Array(128);
  for (let c = 35, v = 0; c < 127; c++) if (c !== 92) code[c] = v++;
  for (let i = 0; i < cells.length; i++) cells[i] = code[MAP.cells.charCodeAt(i)]; }
// Beyond the photograph the wood goes on as its own mirror image: drawn out, pulled out of shape, its trees taller
// here and lower there, so that it does not show.
function fold(xf, pf) {
  const d = xf < 0 ? -xf : xf - 1;
  let m = d * .75 + smooth(0, .25, d) * (vnoise(xf * 1.7 + 7.3, pf * 2.5) - .5) * .11;   // never so much that it would turn back
  if (m < 0) m = -m;
  m %= 2; if (m > 1) m = 2 - m;
  return xf < 0 ? m : 1 - m;
}
const stretch = (xf, pf) => pf < GROUND ? GROUND - (GROUND - pf) * (1 + smooth(0, .22, xf < 0 ? -xf : xf - 1) * (vnoise(xf * 2.3, 1.1) - .5) * .7) : pf;
let lev = 0;                                                   // what what() learnt on the way: how light it is
function what(xf, pf, raw) {                                   // what there is at a point of the photo
  if (!raw) {                                                  // a cell has no corners
    const a = xf * MW * .8, b = pf * MH * .8;
    xf += (vnoise(a, b) - .5) * 1.5 / MW; pf += (vnoise(a + 31.7, b + 17.3) - .5) * 1.5 / MH;
  }
  if (xf < 0 || xf > 1) { const f = fold(xf, pf); pf = stretch(xf, pf); xf = f; }
  const i = xf * MW | 0, j = pf * MH | 0, v = cells[(j < 0 ? 0 : j >= MH ? MH - 1 : j) * MW + (i >= MW ? MW - 1 : i)];
  lev = v & 7; return v >> 3;
}
// In winter the wood is bare: sky between the limbs of the trees, down to where trunks and twigs stand too close.
const DENSE = [[0, .6], [.2, .615], [.45, .64], [.62, .645], [.8, .64], [1, .605]];
const dense = xf => along(DENSE, xf < 0 || xf > 1 ? fold(xf, .6) : xf) + (vnoise(xf * 30, 5.5) - .5) * .022 + (vnoise(xf * 110, 2.5) - .5) * .008;
const leafHere = (x, y) => vnoise(x * .13 / U, y * .13 / U) < .32 + lev * .06;   // a wisp of leaves is leaves here and sky there
const close = (xf, pf) => { const m = xf < 0 || xf > 1 ? fold(xf, pf) : xf; return clamp((.52 - pf) / .2) * (m > .66 || m < .22 ? 1 : .35); };   // the boughs overhead are near, the wood is not

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
// The map knows where their leaves are, not how they stand.  For winter, and where the leaves are thin, they are
// grown: a trunk, limbs, twigs, none of them beyond where the map has leaves.
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
  const leafy = (x, y) => { const pf = fy(y), xf = fx(x, y); return pf < GROUND + .01 && (pf > dense(xf) || what(xf, pf, true) !== SKY); };
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
  const x0 = fx(-60 * U, H * .5) - .1, x1 = fx(W + 60 * U, H * .5) + .1;
  for (let side = -1; side <= 1; side++) {                     // and their mirror images beyond the photograph
    for (const t of TREES) {
      const xf = side < 0 ? -t[0] : side > 0 ? 2 - t[0] : t[0], lean = side ? -t[4] : t[4];
      if (xf < x0 || xf > x1 || (side < 0 && xf > -.04) || (side > 0 && xf < 1.04)) continue;
      r = rng(31 + Math.round(xf * 1000));                     // every tree keeps its shape, whatever else is to be seen
      const x = px(xf, t[1]), y = py(t[1]), h = (t[1] - t[2]) * PH, w = Math.max(1.3 * U, h * .011);
      add(x, y, x + lean * h, y - h * .42, (r() - .5) * .08, w, 0, false);
      const trunk = limbs[limbs.length - 1];
      for (let i = 0; i < 5; i++) {
        const a = -Math.PI / 2 + ((i + .5) / 5 - .5) * 2.2 * t[3] + (r() - .5) * .2 + lean, from = i === 2 ? 1 : .6 + r() * .4;
        grow(limbX(trunk, from), limbY(trunk, from), a, h * (.27 - Math.abs(a + Math.PI / 2) * .05) * (.85 + r() * .3), w * .5, 1, false);
      }
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
  const k = what(xf, pf);
  if (k >= GRASS) return k;
  const top = tops[x < 0 ? 0 : x >= W ? W | 0 : x | 0], low = y > top;
  if (bare) return m & 1 ? LIMB : low || y > top - Math.max(0, vnoise(x * .22 / U, y * .035 / U) - .4) * 55 * U ? THICKET : SKY;   // twigs stand up out of the wood
  if (k === LEAF) return LEAF;
  if (k === WISP && leafHere(x, y)) return WISP;
  if (m & 4) return LIMB;                                      // among leaves the twigs are not to be seen
  if (low) { lev = 2; return LEAF; }
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
  let l0 = lev;
  if (k === LEAF && zone(x - wind(time) * 2 * sz, y) === LEAF) l0 = lev;   // the wind moves the light about in them
  veil(.6 - near * .5);
  if (k === WISP) {                                            // thin leaves with the sun in them
    const col = pal.accent && q > (season === 'spring' ? .55 : .8) ? pick(pal.accent, R()) : q < .45 ? pal.folLight : q < .75 ? pal.folSun : pal.folMid;
    PP.dab(x, y, (1.8 + R() * 2.6) * sz, col, { a: .9, den: .9, core: .65, s: 1.4 * U });
    return;
  }
  const t = clamp(.5 + ((l0 + .1 + R() * .8) / 8 - .5) * (.7 + .5 * sun));   // without the sun there is less to tell light from shade
  let col = t < .28 ? mix(pal.folDeep, pal.folDark, t / .28) : t < .55 ? mix(pal.folDark, pal.folMid, (t - .28) / .27) : t < .82 ? mix(pal.folMid, pal.folLight, (t - .55) / .27) : mix(pal.folLight, pal.folSun, (t - .82) / .18 * (pal.accent ? .6 : .9));
  if (pal.accent && l0 > 2 && q > (season === 'spring' ? .88 : .93)) col = pick(pal.accent, R());
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
const lit = l => 5.2 + (l - 5.2) * sun;                        // under a grey sky the trees throw no shade
function groundStroke() {
  const R = PP.R, x = R() * W, y = groundTop + R() * (H - groundTop);
  if (!at(x, y)) return;
  const k = zone(x, y);
  if (k === GRASS) paintGrass(x, y); else if (k === GRAVEL) paintGravel(x, y);
}
function paintGravel(x, y) {                                   // gravel, and the shade of the leaves lying on it
  const R = PP.R, pf = zy, sz = (.3 + nearG(pf)) * U, l0 = lev, q = R(), sd = shadow(x, y);
  veil(.5 - nearG(pf) * .4);
  let col = ramp(pal.gravels, (lit(l0) + R()) / 8);
  if (sd) col = mix(col, pal.gravels[0], sd * .35);
  if (q < .16) { PP.dab(x, y, (.8 + R() * 1.3) * sz, mix(col, q < .08 ? pal.gravels[5] : pal.gravels[1], .35), { a: .8, den: 1, core: .5, s: 1.2 * U }); return; }   // stones
  if (season === 'autumn' && q > .975) { PP.dab(x, y, (1.3 + R() * 1.5) * sz, pick(pal.bits, R()), { a: .9, den: 1.4, core: .8, s: 1.3 * U }); return; }   // leaves
  const a = (R() - .5) * .5, ca = Math.cos(a), sa = Math.sin(a), l = (3 + R() * 8) * sz;
  run(x, y, ca, sa, l, z => z === GRAVEL && Math.abs(lev - l0) < 2, Math.max(1.5, 1.6 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (1.4 + R() * 2.2) * sz, col, { a: .87, jit: 7, dry: .14, sp: 1.1 * U, bow: .03 });
}
function paintGrass(x, y) {                                    // grass nobody mows, every blade upright
  const R = PP.R, pf = zy, sz = (.3 + nearG(pf) * .8) * U, l0 = lev, q = R(), sd = shadow(x, y);
  veil(.5 - nearG(pf) * .4);
  let col = ramp(pal.meadow, (lit(l0) + R()) / 8);
  if (l0 > 2) { const dry = smooth(.5, .7, fbm(x * .006 / U + 11, y * .012 / U)); if (dry > 0) col = mix(col, pal.straw, dry * .3); if (q > .92) col = mix(col, pal.straw, .4); }
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
      const near = close(zx, pf), a = (R() - .5) * 1.2, col = mix(pal.folDark, pal.folMid, .15 + lev * .09), wd = room(x, y, -Math.sin(a), Math.cos(a), 26 * U, GREEN), ll = wd < 26 * U ? l * .4 : l * .9;
      veil(.6 - near * .5);
      PP.brush(x - Math.cos(a) * ll, y - Math.sin(a) * ll, x + Math.cos(a) * ll, y + Math.sin(a) * ll, wd, col, o);
    } else if (k === THICKET) { veil(.6); PP.brush(x - l, y, x + l, y, 18 * U, mix(mix(pal.skyLow, pal.haze, .5), pal.twigs[1], .55), o); }
    else { veil(.5 - nearG(pf) * .4); PP.brush(x - l, y, x + l, y, 18 * U, k === GRASS ? ramp(pal.meadow, (lit(lev) + .5) / 8) : ramp(pal.gravels, (lit(lev) + .5) / 8), o); }
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
    buildTrees();
  },
  tick(t) { time = t; },
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
    spend('sw-leaf', RATE.leaf * W * leafFoot / ua * pace, leafStroke);
    spend('sw-ground', RATE.ground * W * (H - groundTop) / ua * pace, groundStroke);
    spend('sw-tower', RATE.tower * Math.max(0, TX1 - TX0) * Math.max(0, TY1 - TY0) / ua * pace, towerStroke);
    spend('sw-thin', RATE.thin * thinLength / (10 * U) * pace, thinStroke);
    spend('sw-limb', RATE.limb * limbLength / (14 * U) * pace, limbStroke);
    if ((s && s.night) || (o && o.night)) spend('sw-lamp', RATE.lamp * pace, lampDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('sw-fog', RATE.fog * W * .18 * PH / ua * pace, fogStroke);
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
