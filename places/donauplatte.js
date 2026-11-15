/* ------------------------------------------------------------------ *
 *  Vienna, looking at the Donauplatte: from a wooden deck on the
 *  Donauinsel across the Neue Donau to the towers of the Donau City.
 *  The DC Tower with its folded side, the black DC 2 beside it, the
 *  Hochhaus Neue Donau and the white rings of the Danube Flats; on the
 *  far shore the beach with the yellow legs of the footbridge.  The
 *  Reichsbrücke crosses on the right, behind the little lighthouse,
 *  and along the deck hang the nets one lies in over the water.
 *
 *  It is painted after a photograph, measured by hand.  On that the
 *  DC 2 was a building site still: it is painted as it stands now, and
 *  neither its cranes nor the containers on the shore nor anybody who
 *  was on the deck that evening.  What lies beyond the photograph on
 *  either side is made up.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'donauplatte', name: 'Donauplatte', make(kit) {
'use strict';
const { PP, clamp, smooth, ease, hex, mix, pick, rng, hash1, vnoise, fbm, spend, clipBand, skyCol, dens } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false, wet = false, sun = false, white = 0;   // white: how much snow lies
const use = s => {
  pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; wet = traits.rain > 0; sun = !night && traits.shadow > 0;
  white = traits.snow ? 1 : season !== 'winter' ? 0 : s.weather === 'frost' ? .5 : s.weather === 'sunny' ? .75 : .3;
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph the picture is painted after: x and y as fractions of its
// width and height.  A wide screen shows what lies to the left and right of it as well.  A narrow one
// has no room for all of it: the towers keep their place, and the near bank with the deck, the trees
// and the lighthouse is pushed in from the right (by SH), over the bridge and the open water.
let X0 = 0, Y0 = 0, SH = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
const tri = u => { u -= Math.floor(u); return 1 - Math.abs(2 * u - 1); };
function along2(pts, x) {                                      // a line through points, eased from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * ease((x - a[0]) / (b[0] - a[0])); }
  return pts[pts.length - 1][1];
}
const SHORE = .578;                                            // the far waterline: the horizon

// How far off a thing is: in fog everything pales by that.  The water and the deck are level: there it is a
// matter of how close to the horizon a point lies.
const flatOff = d => .05 / (.05 + d);
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;
const hazeOf = (d, k) => traits.fog ? [fogCol(), clamp(d * 1.4 - .1, 0, .85) * traits.fog * (k || 1) * (night ? .55 : 1)] : null;
const veil = d => { PP.haze = hazeOf(d); };
const glow = d => { PP.haze = hazeOf(d, .45); };                // a light carries through the fog

// ---------- the skyline ----------
// from, to, top, colour, kind, how many windows are lit at night, how far off it is, its top if that is not level
const T = (x0, x1, y0, col, kind, lit, far, top) => ({ x0, x1, y0, col, kind, lit, far, top: top || null, c: (x0 + x1) / 2 });
// The DC Tower: its roof slopes up to the corner between the front and the folded side, its left edge leans a
// little (the photo looks up at it), its right one runs in and out with the folds.
const dcLeft = pf => .055 - Math.max(0, pf - .066) * .0217, dcFold = pf => .117 + (pf - .03) * .012, dcEdge = pf => .153 + .0045 * tri((pf - .07) / .17);
const DC1 = T(.045, .158, .03, 'glassDark', 'dc1', .5, .06, xf => xf < .055 ? .066 + (.055 - xf) * 46 : xf < .117 ? .066 - (xf - .055) * .58 : .03 + (xf - .117) * 1.1);
const TOWERS = [                                               // from the back to the front
  T(.376, .402, .512, 'citySand', 'flat', .4, .45), T(.398, .492, .496, 'wall', 'flat', .4, .5), T(.49, .575, .517, 'citySand', 'flat', .4, .5),
  T(-.63, -.56, .478, 'wall', 'bands', .4, .35), T(-.5, -.44, .452, 'glass', 'glass', .5, .3),               // made up: the photo ends at the high tower
  T(-.345, -.28, .468, 'towerLight', 'bands', .5, .3), T(-.26, -.195, .418, 'glass', 'glass', .6, .25), T(-.18, -.12, .446, 'wall', 'bands', .4, .3),
  T(-.108, -.052, .39, 'glass', 'glass', .6, .2), T(-.042, .05, .405, 'cityPane', 'bands', .7, .15),         // the last one is on it, cut by its edge
  T(.1044, .1066, -.035, 'tower', 'mast', 0, .05), DC1,
  T(.162, .257, .222, 'towerDc2', 'dc2', .45, .08, xf => .222 + Math.max(0, .197 - xf) * .12),
  T(.2835, .2857, .345, 'towerLight', 'spire', 0, .12), T(.262, .298, .372, 'towerLight', 'hnd', .45, .14),
  T(.3272, .329, .272, 'towerLight', 'spire', 0, .1),
  T(.316, .374, .292, 'towerWhite', 'df', .45, .08, xf => .292 + .012 * Math.pow(clamp((Math.abs(xf - .345) - .021) / .008), 2)),
  T(-.03, .034, .49, 'stone', 'flat', .3, .2), T(.03, .126, .508, 'cityPodium', 'flat', .5, .1),
];
const THIN = TOWERS.filter(t => t.kind === 'mast' || t.kind === 'spire');

// The far shore: trees, the beach under them, the wall along the water.  Right of the yellow legs the bridge
// stands in front of it, and under the bridge one sees the bank beyond.
const UNDER = .487, BR0 = .47, BRH = .012, PIER = .575, PIERW = .012, LAMP0 = .492, LAMPS = .031, LAMPH = .024;
const brTop = xf => .533 - (xf - .5) * .04;
let lampHW = .001;                                             // half a lamp post, in photo widths: never less than a stroke
const lampAt = xf => { const i = Math.round((xf - LAMP0) / LAMPS); return i >= 0 && Math.abs(xf - LAMP0 - i * LAMPS) < lampHW; };
const shoreTop = xf => xf >= UNDER ? brTop(xf) + BRH - .001
  : .534 + (vnoise(xf * 38, 3.3) - .5) * .012 + (vnoise(xf * 130, 9.1) - .5) * .005 - .006 * smooth(.42, .47, xf);
// the legs of the footbridge: [foot x, foot y, top x, top y]
const LEGS = [[.411, .574, .423, .503], [.424, .574, .433, .507], [.461, .574, .452, .503], [.476, .574, .463, .499]], LEGW = .003;
let legs = true;                                               // on a narrow screen the trees stand in front of them

// The near bank, on the right: the lighthouse, the trees round its foot, the wall along the water and a
// pavilion that floats in front of it.  All of it by where it is on the photo (x plus SH).
const LX = .8205, BANK0 = .775, BANKX = .852, TREE0 = .785, PAV0 = .702, PAV1 = .79;
let finHW = .0011;
const lhHW = pf => pf < .424 ? 0 : pf < .432 ? finHW : pf < .437 ? .0048 : pf < .451 ? .0056 : pf < .457 ? .0105 : .0072 + (pf - .457) * .045;
const bankTop = xr => SHORE - .025 * smooth(BANK0, .83, xr);
const TOPS = [[.785, .562], [.795, .524], [.806, .513], [.818, .521], [.832, .508], [.85, .492], [.866, .465], [.885, .432], [.905, .44], [.925, .468],
  [.945, .448], [.96, .425], [.98, .402], [1, .39], [1.06, .376], [1.15, .388], [1.3, .37], [1.6, .384]];
const treeTop = xr => along2(TOPS, xr) + (vnoise(xr * 90, 5.5) - .5) * .018 + (vnoise(xr * 260, 1.5) - .5) * .008;
const pavTop = xr => .552 - .008 * tri((xr - PAV0) / .0293);

// The deck is level ground, and so are the nets along it.  Its boards all run towards one point on the horizon
// (VX): a line to it is known by its slope m, how far it goes sideways for every step down.  What lies across
// them, the ends of the boards and the beams of the nets, runs towards another one far off to the left (V2X):
// such a line is known by cv, how far below the horizon it passes under VX.
const VX = .866, V2X = -1, K = VX - V2X, EDGE = -.975, NL = -.875, NR = .85, BEAM = .07, DM = .066, GL = 3.2;
const CV = [.205, .15, .106, .077, .057, .043, .033, .026, .021];   // from one net to the next
const CEND = CV[CV.length - 1], DEND = .016;                  // where the nets end, and the deck
const dOf = (m, cv) => cv * K / (K - m * cv);                  // how far below the horizon a board's line and a cross line meet
function deckLeft(d) {                                         // where the water ends at a level, among the things on the near bank
  if (d < DEND) return d < .004 ? PAV0 : BANKX;
  const xa = VX + EDGE * d;
  return d * K / (xa - V2X) >= CV[0] ? xa : VX + NL * d;
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes
// may start there.
const SKY = 0, TOWER = 1, SHOREZ = 2, PYLON = 3, BRIDGE = 4, LHOUSE = 5, TREE = 6, BANK = 7, PAV = 8, WATER = 9, PLANK = 10, NET = 11;
let zt = null, zb = 0, zl = 0, zm = 0, zc = 0;                 // what zone() learnt on the way: the tower, the part of the bridge, the leg, the lines through the point
function zone(xf, pf) {
  const xr = xf + SH, d = pf - SHORE;
  if (d >= 0) {
    if (d < .004 && xr >= PAV0 && xr < PAV1) return PAV;
    if (xr >= BANK0 && d < (xr >= BANKX ? DEND : .003)) return BANK;
    if (d >= DEND) {
      const m = zm = (xr - VX) / d;
      if (m >= EDGE) { const cv = zc = d * K / (xr - V2X); if (cv >= CV[0] || m >= NR) return PLANK; if (m >= NL) return cv < CEND ? PLANK : NET; }
    }
    return WATER;
  }
  if (xr >= BANK0) { if (pf >= bankTop(xr)) return BANK; if (xr >= TREE0 && pf >= treeTop(xr)) return TREE; }
  if (pf >= .424) { const dl = Math.abs(xr - LX); if (dl < .0105 && dl < (pf >= .451 ? .0105 : lhHW(pf))) return LHOUSE; }
  if (xr >= PAV0 && xr < PAV1 && pf >= pavTop(xr)) return PAV;
  if (legs && pf >= .497 && xf > .4 && xf < .485) for (let i = 0; i < 4; i++) {
    const g = LEGS[i], t = (g[1] - pf) / (g[1] - g[3]);
    if (t >= 0 && t <= 1 && Math.abs(xf - g[0] - (g[2] - g[0]) * t) < LEGW * (1.25 - .45 * t)) { zl = i; return PYLON; }
  }
  const st = shoreTop(xf);
  if (xf < UNDER && pf >= st) return SHOREZ;
  if (xf >= BR0) {
    const bt = brTop(xf);
    if (pf >= bt) { if (pf < bt + BRH) { zb = 0; return BRIDGE; } if (Math.abs(xf - PIER) < PIERW / 2 && pf < SHORE - .004) { zb = 2; return BRIDGE; } }
    else if (pf >= bt - LAMPH && lampAt(xf)) { zb = 1; return BRIDGE; }
  }
  if (pf >= st) return SHOREZ;
  for (let i = TOWERS.length - 1; i >= 0; i--) { const t = TOWERS[i]; if (xf >= t.x0 && xf < t.x1 && pf >= (t.top ? t.top(xf) : t.y0)) { zt = t; return TOWER; } }
  return SKY;
}
function skyTop(xf) {                                          // where the sky ends at x
  const xr = xf + SH; let y = shoreTop(xf);
  for (const t of TOWERS) if (xf >= t.x0 && xf < t.x1) y = Math.min(y, t.top ? t.top(xf) : t.y0);
  if (xf >= BR0) y = Math.min(y, brTop(xf) - (lampAt(xf) ? LAMPH : 0));
  if (xr >= BANK0) y = Math.min(y, bankTop(xr));
  if (xr >= TREE0) y = Math.min(y, treeTop(xr));
  const dl = Math.abs(xr - LX); if (dl < .0105) y = Math.min(y, dl < finHW ? .424 : dl < .0048 ? .432 : dl < .0056 ? .437 : .451);
  if (xr >= PAV0 && xr < PAV1) y = Math.min(y, pavTop(xr));
  return y;
}
function tone(k) {                                             // the colour of a thing, all in all: for a fresh coat, and for the water to mirror
  switch (k) {
    case TOWER: {
      const t = zt, c = t.kind === 'dc1' ? mix(pal.glassDark, pal.glassBlue, .18) : t.kind === 'dc2' ? mix(pal.towerDc2, pal.towerDc2Frame, .25) : t.kind === 'df' ? mix(pal.towerWhite, pal.glassDark, .28)
        : t.kind === 'glass' ? mix(pal.dark, pal.glass, .4) : t.kind === 'bands' ? mix(pal[t.col], pal.glass, .3) : t.kind === 'mast' ? pal.cabin : mix(pal[t.col], pal.dark, .1);
      return mix(c, pal.cityFar, t.far);
    }
    case SHOREZ: return mix(mix(pal.folMid, pal.cityBeach, .35), pal.haze, .25);
    case PYLON: return pal.cityPylon;
    case BRIDGE: return mix(pal.stoneBridge, pal.stoneBridgeDark, .25);
    case LHOUSE: return mix(pal.hull, pal.tower, .1);
    case TREE: return bare ? mix(pal.folDark, pal.skyLow, .35) : mix(pal.folDark, pal.folMid, .45);
    case BANK: return mix(pal.stone, pal.folDeep, .4);
    case PAV: return mix(pal.hull, pal.dark, .3);
    case PLANK: return white ? mix(pal.plank, pal.snow, white * .7) : pal.plank;
    case NET: return mix(pal.net, pal.beam, .25);
  }
  return pal.water;
}

// ---------- the towers ----------
const OFFICE = hex('#dcefe4');                                 // the light of an office
const lit = () => ({ a: 1, den: 3, core: 1, s: 1.2 * U, jit: 4 });
// The lit ones among the windows a stroke has gone over: which are lit is a matter of the window, not of chance.
function windows(t, x0, xa, xb, y, pitch, row, share, cold) {
  for (let i = Math.ceil((xa - x0) / pitch - .5); x0 + (i + .5) * pitch <= xb; i++) {
    const h = hash1(i * 131 + row * 1777 + Math.round(t.x0 * 9973));
    if (h < share) PP.dab(x0 + (i + .5) * pitch, y, .55 * U, cold && h < share * .6 ? OFFICE : pal.lights[(h / share * pal.lights.length) | 0], lit());
  }
}
function towerStroke(x, y) {
  const R = PP.R, t = zt, xf = fx(x), pf = fy(y), xa = px(t.x0), xb = px(t.x1), w = xb - xa, u = (x - xa) / w, top = py(t.y0), foot = py(SHORE);
  const far = t.far + traits.rain * .08, off = .8 - .25 * clamp((SHORE - pf) / .3), away = c => mix(c, pal.cityFar, far);
  const o = { a: .92, jit: 6, dry: .04, bow: 0, sp: 1.1 * U };
  veil(off);
  if (t.kind === 'mast' || t.kind === 'spire') {                // an antenna, red and white; the tip of a roof
    const band = Math.floor((pf - t.y0) / .009), bw = Math.max(1.2 * U, w), cx = (xa + xb) / 2, yc = py(t.y0 + (band + .5) * .009), l = .0045 * PH;
    o.sp = Math.min(1.1 * U, bw);
    PP.brush(cx, Math.max(top, yc - l), cx, yc + l, bw, away(t.kind === 'spire' ? mix(pal.towerLight, pal.tower, .3) : band & 1 ? pal.hull : pal.cabin), o);
  } else if (t.kind === 'dc1') {
    const er = dcEdge(pf), xe = dcFold(pf);
    if (xf >= er) {                                            // where a fold goes in, the sky shows
      const g = (t.x1 - er) * PW;
      if (g > 1) { PP.haze = null; PP.brush(px(er) + g / 2 + .5, y - 3 * U, px(er) + g / 2 + .5, y + 3 * U, g, skyCol(y), { a: .9, jit: 4, dry: 0, bow: 0, sp: Math.min(1.1 * U, g) }); }
      return;
    }
    // upright strokes: the ribs of the front; on the side short ones, light and dark by the folds
    const side = xf >= xe, l = (side ? 2 + R() * 4 : 5 + R() * 12) * U, pa = fy(y - l), pb = fy(y + l), bw = 2.4 * U, pitch = 2.6 * U;
    const rib = Math.floor((x - xa) / pitch), cx = clamp(xa + (rib + .5) * pitch, px(Math.max(dcLeft(pa), dcLeft(pb))) + bw / 2, px(Math.min(dcEdge(pa), dcEdge(pb))) - bw / 2);
    const ya = Math.max(py(t.top(fx(cx))) + .5 * U, y - l), yb = Math.min(foot, y + l);
    let col;
    if (side) {
      const dk = (xf - xe) / (er - xe) > .25 + .6 * tri((pf - .07) / .17 + .5);
      col = dk ? mix(pal.glassDark, pal.ink, .5) : mix(mix(pal.glassDark, pal.glassBlue, .5), pal.skyMid, .14);
    } else {
      col = mix(pal.glassDark, pal.glassBlue, .06 + .3 * smooth(.3, .52, pf) + .3 * vnoise(x * .02 / U + 3, y * .008 / U));   // lighter towards its foot
      if (rib & 1) col = mix(col, pal.towerRib, rib % 4 === 1 ? .42 : .26);
    }
    if (yb > ya) PP.brush(cx, ya, cx, yb, bw, away(col), o);
    if (night && !side) {                                      // offices: lit in clusters, most of them high up
      const fh = 3.2 * U; glow(off);
      for (let fl = Math.ceil((ya - top) / fh - .5); top + (fl + .5) * fh <= yb; fl++)
        if (vnoise(rib * .14 + 7, fl * .45 + 3) > .56 + .24 * smooth(.12, .5, fy(top + fl * fh)) && hash1(rib * 131 + fl * 1777) < .6) PP.dab(cx, top + (fl + .5) * fh, .55 * U, (rib + fl) % 3 ? pal.lights[2] : OFFICE, lit());
    }
  } else if (t.kind === 'dc2') {                               // black, a grid of frames with the windows deep in it; two sides of it show
    const xc = px(.197), left = x < xc, fa = left ? xa : xc, fb = left ? xc : xb, fh = Math.max(2.4 * U, .0059 * PH), cw = fh * .9;
    const ty = py(left ? .2245 : .222), fl = Math.floor((y - ty) / fh), k = R();
    const frame = left ? mix(pal.towerDc2Frame, pal.towerLight, .14) : mix(pal.towerDc2Frame, pal.towerDc2, .3);
    if (fl < 1 || k < .2) {                                    // the frame between two floors; the top one is all frame
      const l = Math.min((4 + R() * 8) * U, (fb - fa) / 2 - .3), cx = clamp(x, fa + l, fb - l), yl = fl < 1 ? ty + fh * .5 : ty + fl * fh;
      PP.brush(cx - l, yl, cx + l, yl, fl < 1 ? fh : Math.max(.8 * U, fh * .28), away(frame), o);
    } else if (k < .42) {                                      // an upright of it
      const cx = clamp(fa + Math.round((x - fa) / cw) * cw, fa + .5 * U, fb - .5 * U), l = (5 + R() * 9) * U, bw = Math.max(.8 * U, cw * .26);
      o.sp = Math.min(1.1 * U, bw);
      PP.brush(cx, Math.max(ty + fh, y - l), cx, Math.min(foot, y + l), bw, away(frame), o);
    } else {                                                   // a row of windows
      const l = Math.min((3 + R() * 6) * U, (fb - fa) / 2 - .3), cx = clamp(x, fa + l, fb - l), yr = ty + (fl + .56) * fh;
      PP.brush(cx - l, yr, cx + l, yr, fh * .62, away(mix(pal.towerDc2, pal.glassBlue, (left ? .18 : .05) + .16 * vnoise(x * .03 / U + 9, y * .02 / U))), o);
      if (night) { glow(off); windows(t, fa, cx - l, cx + l, yr, cw, fl, t.lit * .5, false); }
    }
  } else if (t.kind === 'hnd') {                               // white, its windows in upright rows
    const cw = 2.4 * U, ci = Math.floor((x - xa) / cw), cx = clamp(xa + (ci + .5) * cw, xa + 1.2 * U, xb - 1.2 * U), fh = 3 * U, fl = Math.floor((y - top) / fh);
    const win = ci % 3 === 1 && pf > t.y0 + .008, yc = win ? top + (fl + .5) * fh : y, l = win ? fh * .45 : (3 + R() * 6) * U;
    const col = win ? mix(pal.glass, pal.towerLight, fl & 1 ? .2 : .5) : mix(pal.hull, pal.towerLight, .25 + (u > .6 ? .4 : 0));
    PP.brush(cx, Math.max(top, yc - l), cx, Math.min(foot, yc + l), cw * .9, away(night ? mix(col, pal.glassDark, .35) : col), o);
    if (night && win && hash1(ci * 131 + fl * 1777) < t.lit * .6) { glow(off); PP.dab(cx, yc, .55 * U, pal.lights[(ci + fl) % pal.lights.length], lit()); }
  } else if (t.kind === 'df') {                                // ring on ring of white balconies, glass between them
    const fp = Math.max(2.8 * U, .0056 * PH), fl = Math.floor((y - top) / fp), fr = (y - top) / fp - fl;
    const cap = pf < .3, recess = pf >= .355 && pf < .369, glassy = !cap && (recess || fr > .58);
    const col = glassy ? mix(pal.glassDark, pal.glassBlue, recess ? .1 : .3) : mix(pal.towerWhite, pal.tower, u > .74 ? .4 : 0);   // its right side is in the shade
    // a ring is one stroke from side to side, or from the edge to where the shade begins
    const bw = cap || recess ? 2.4 * U : fp * (glassy ? .36 : .58), split = xa + w * .74, x0 = glassy || cap ? xa + .5 * U : u > .74 ? split : xa + .5 * U, x1 = glassy || cap ? xb - .5 * U : u > .74 ? xb - .5 * U : split;
    const yc = Math.max(cap || recess ? y : top + (fl + (glassy ? .8 : .29)) * fp, py(Math.max(t.top(fx(x0)), t.top(fx(x1)))) + bw / 2);
    o.dry = 0; o.jit = 4;
    if (cap) PP.brush(x0 + bw, yc, x1 - bw, yc, bw, away(col), o);
    else {                                                     // a brush ends ragged: these are drawn past the edge and cut off there
      const c = PP.c, e = (x1 - x0) * .3;
      c.save(); c.beginPath(); c.rect(x0, yc - bw, x1 - x0, 2 * bw); c.clip();
      PP.brush(x0 - e, yc, x1 + e, yc, bw, away(col), o);
      c.restore();
    }
    if (night && glassy && !recess) { glow(off); windows(t, xa, x0, x1, yc, 3 * U, fl, t.lit * .6, false); }
  } else {                                                     // a slab, floor by floor: short level strokes, every other row a band of windows
    const fh = (t.far > .4 ? 2.6 : 3.4) * U, fl = Math.floor((y - top) / fh);
    let col = t.kind === 'glass' ? mix(mix(pal.dark, pal.glass, .15 + .5 * vnoise(x * .03 / U + 3, y * .012 / U)), pal.towerLight, u < .4 ? .12 : 0)
      : t.kind === 'bands' ? mix(pal[t.col], pal.glass, (fl & 1 ? .6 : .08) + (u > .65 ? .12 : 0))
      : mix(pal[t.col], pal.dark, (fl & 1 ? .24 : 0) + (u > .6 ? .12 : 0));
    if (t.kind === 'glass' && (fl & 1)) col = mix(col, pal.dark, .15);
    const l = Math.max(.6, Math.min((2.5 + R() * 5) * U, w / 2 - .3)), cx = clamp(x, xa + l, xb - l), yr = Math.min(foot, top + (fl + .5) * fh);
    PP.brush(cx - l, yr, cx + l, yr, fh * .9, away(col), o);
    if (night && ((fl & 1) || t.kind === 'glass')) { glow(off); windows(t, xa, cx - l, cx + l, yr, 3.2 * U, fl, t.lit * .45, t.kind !== 'flat'); }
  }
}

// ---------- the far shore ----------
function shoreStroke(x, y) {
  const R = PP.R, xf = fx(x), pf = fy(y), top = shoreTop(xf), k = R(), o = { a: .9, jit: 6, dry: .06, bow: 0, sp: 1.1 * U };
  veil(.8);
  if (pf > SHORE - .0022) { const l = (4 + R() * 8) * U; PP.brush(x - l, y, x + l, y, 1.4 * U, mix(pal.waterDark, pal.ink, .35), o); return; }   // where the wall stands in the water
  const leaves = () => {
    const n = vnoise(x * .05 / U + 11, y * .09 / U);
    let c = mix(pal.folMid, n > .5 ? pal.folLight : pal.folDark, Math.abs(n - .5) * 1.5);
    if (pf - top < .007 && k < .4) c = mix(c, pal.folSun, .45); else if (pal.accent && k > .93) c = pick(pal.accent, R());
    return mix(c, pal.haze, bare ? .45 : .28);
  };
  const snowy = c => white ? mix(c, pal.snow, white * .75) : c;
  if (xf >= UNDER) {                                           // under the bridge: the bank beyond it, in its shade
    const t = (pf - top) / (SHORE - top);
    if (t < .62) PP.dab(x, y, (1.4 + R() * 2.2) * U, mix(leaves(), pal.haze, .25 + (t < .2 ? .2 : 0)), { a: .9, den: .8, core: .6, s: 1.6 * U });
    else { const l = (3 + R() * 7) * U; PP.brush(x - l, y, x + l, y, (1.3 + R()) * U, mix(snowy(pal.stoneBridge), pal.haze, .3), o); }
    return;
  }
  if (pf < .548) PP.dab(x, y, (1.4 + R() * 2.4) * U, leaves(), { a: .9, den: .8, core: .6, s: 1.6 * U });
  else if (pf < .561) {                                        // the beach: pale ground, white things standing on it, a red umbrella here and there
    const l = (3 + R() * 6) * U;
    PP.brush(x - l, y, x + l, y, (1.4 + R()) * U, snowy(mix(pal.cityBeach, pal.towerLight, R() * .4)), o);
    if (k < .1) PP.brush(x, py(.55), x, py(.558), 1.3 * U, pal.hull, o);
    else if (k > .93 && season !== 'winter') PP.dab(x, py(.556), 1.3 * U, pal.cabin, { a: .95, den: 2, core: .9, s: U, jit: 5 });
  } else if (pf < .571) PP.dab(x, y, (1.3 + R() * 2) * U, snowy(mix(pal.cityGreen, pal.folMid, R() * .6)), { a: .9, den: .8, core: .6, s: 1.5 * U });
  else { const l = (4 + R() * 8) * U; PP.brush(x - l, y, x + l, y, (1.3 + R()) * U, mix(pal.stoneBridge, pal.stone, .15 + .3 * fbm(x * .02 / U, y * .08 / U + 3)), o); }
}
function pylonStroke(x, y) {                                   // a yellow leg, its right side in the shade
  const R = PP.R, g = LEGS[zl], bx = px(g[0]), by = py(g[1]), dx = px(g[2]) - bx, dy = py(g[3]) - by, len = Math.hypot(dx, dy);
  const t = (by - y) / -dy, cx = bx + dx * t, hw = LEGW * (1.25 - .45 * t) * PW, right = x > cx, l = (3 + R() * 6) * U;
  const xs = cx + (right ? .45 : -.45) * hw, la = Math.min(l, (1 - t) * len), lb = Math.min(l, t * len), bw = Math.max(1.2 * U, hw * .95);
  veil(.75);
  PP.brush(xs - dx / len * lb, y - dy / len * lb, xs + dx / len * la, y + dy / len * la, bw, right ? mix(pal.cityPylon, pal.cityPylonDark, .7) : pal.cityPylon, { a: .93, jit: 5, dry: .04, bow: 0, sp: Math.min(1.1 * U, bw) });
}
function bridgeStroke(x, y) {
  const R = PP.R, xf = fx(x), pf = fy(y), top = brTop(xf), part = zb, o = { a: .92, jit: 6, dry: .05, bow: 0, sp: 1.1 * U };
  veil(.65);
  if (part === 1) {                                            // a lamp post
    const lx = LAMP0 + Math.round((xf - LAMP0) / LAMPS) * LAMPS, cx = px(lx);
    PP.line([[cx, py(brTop(lx) - LAMPH) + .5 * U], [cx, py(brTop(lx))]], mix(pal.tower, pal.cityFar, .2), 1.3 * U, { pass: 1, wob: .15 * U, a: .95, step: 999 });
    return;
  }
  if (part === 2) {                                            // the pier
    const l = (2 + R() * 4) * U, cx = clamp(x, px(PIER - PIERW / 2) + 1.1 * U, px(PIER + PIERW / 2) - 1.1 * U);
    const col = mix(pal.stoneBridge, pal.stoneBridgeDark, (x > px(PIER) ? .5 : .12) + (pf < top + BRH + .005 ? .35 : 0));
    PP.brush(cx, Math.max(py(top + BRH), y - l), cx, Math.min(py(SHORE - .004), y + l), 2.2 * U, col, o);
    return;
  }
  // along the bridge: its railing, the pale edge of the roadway, the girder under it and its dark underside
  const t = (pf - top) / BRH, sl = -.04 * PH / PW, k = 1 / Math.hypot(1, sl);
  let tc, th, col;
  if (t < .14) { tc = .07; th = .14; col = mix(pal.tower, pal.stoneBridge, .35); }
  else if (t < .5) { tc = .32; th = .36; col = white ? mix(pal.stoneBridge, pal.snow, white * .6) : mix(pal.stoneBridge, pal.towerLight, .25); }
  else if (t < .6) { tc = .55; th = .1; col = pal.stoneBridgeDark; }
  else if (t < .88) { tc = .74; th = .28; col = mix(pal.stoneBridge, pal.stoneBridgeDark, .4 + .2 * vnoise(x * .03 / U, 7)); }
  else { tc = .94; th = .12; col = mix(pal.stoneBridgeDark, pal.ink, .45); }
  let l = (4 + R() * 9) * U;
  const yc = py(top + tc * BRH), ok = s => zone(fx(x + s * k * l), fy(yc + s * sl * k * l)) === BRIDGE && zb === 0;
  if (!ok(-1) || !ok(1)) l *= .25;
  PP.brush(x - k * l, yc - sl * k * l, x + k * l, yc + sl * k * l, Math.max(U, th * BRH * PH * .9), col, o);
}

// ---------- the near bank ----------
function lighthouseStroke(x, y) {
  const R = PP.R, pf = fy(y), cx = px(LX - SH), hw = lhHW(pf) * PW, dx = x - cx, o = { a: .93, jit: 5, dry: .04, bow: 0, sp: 1.1 * U };
  if (Math.abs(dx) >= hw) {                                    // under its gallery the sky shows
    const g = .0105 * PW - hw, xs = cx + (dx < 0 ? -1 : 1) * (hw + g / 2);
    if (g > 1) { PP.haze = null; PP.brush(xs, y - 2 * U, xs, y + 2 * U, g, skyCol(y), { a: .9, jit: 4, dry: 0, bow: 0, sp: Math.min(1.1 * U, g) }); }
    return;
  }
  veil(.5);
  if (pf < .432) { o.sp = Math.min(1.1 * U, hw * 2); PP.brush(cx, py(.4245), cx, py(.432), Math.max(1.2 * U, hw * 2), pal.tower, o); }
  else if (pf < .437) PP.brush(cx - hw + U, py(.4345), cx + hw - U, py(.4345), .0045 * PH, white ? mix(pal.lhCap, pal.snow, white * .6) : pal.lhCap, o);
  else if (pf < .451) {                                        // the lantern: panes between white bars
    const i = Math.floor((dx + hw) / (2 * hw) * 5), xs = cx - hw + (i + .5) / 5 * 2 * hw, bw = Math.max(U, hw * .36);
    o.sp = Math.min(1.1 * U, bw);
    PP.brush(xs, py(.4375), xs, py(.4505), bw, i & 1 ? (night ? pal.lights[2] : mix(pal.glass, pal.dark, .35)) : pal.hull, o);
  } else if (pf < .457) PP.brush(cx - hw + U, y, cx + hw - U, y, 1.4 * U, pf < .4535 ? mix(pal.tower, pal.dark, .3) : mix(pal.hull, pal.tower, .2), o);
  else {                                                       // the shaft, its right side in the shade; two slits for windows
    const slit = Math.abs(dx) < hw * .22 && (Math.abs(pf - .478) < .004 || Math.abs(pf - .506) < .004);
    const l = Math.min((1.5 + R() * 3) * U, hw - .4), xs = slit ? cx : clamp(x, cx - hw + l, cx + hw - l), ls = slit ? hw * .2 : l;
    PP.brush(xs - ls, y, xs + ls, y, (1.4 + R() * 1.2) * U, slit ? pal.dark : mix(pal.hull, pal.tower, dx > hw * .25 ? .38 : dx < -hw * .6 ? .12 : 0), o);
  }
}
function folLum(x, y) {                                        // light falls in from the upper left
  const s = time * .03, k = .02 / U;
  return .45 + (fbm(x * k + s, y * k * 1.2) - fbm((x - 9 * U) * k + s, (y - 9 * U) * k * 1.2)) * 3.2;
}
function treeStroke(x, y) {
  const R = PP.R, xr = fx(x) + SH, pf = fy(y), top = treeTop(xr), deep = clamp((pf - top) / .1), k = R(), sz = (.6 + deep * .25) * U;
  const lum = clamp(folLum(x, y) + .1 - deep * .3);
  veil(.4);
  if (bare) {                                                  // the sky between the twigs, the twigs, and the snow lying on them
    if (k < .4) PP.dab(x, y, (4 + R() * 6) * sz, mix(skyCol(y), pal.folMid, .25 + deep * .45), { a: .85, den: .5, core: .5, s: 2 * U });
    else if (white && lum > .45 && k < .55) PP.dab(x, y, (3 + R() * 6) * sz, mix(pal.folMid, pal.snow, white), { a: .9, den: .6, core: .5, sx: 1.7, sy: .55 });
    else {
      const a = -Math.PI / 2 + (R() - .5) * 1.6, l = (8 + R() * 18) * sz;
      PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 4 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
        lum < .4 ? pal.folDeep : k < .85 ? pal.folDark : pal.folMid, (1 + R() * 1.2) * sz, { pass: 1, a: .8 });
    }
    return;
  }
  if (pf - top < .005) { PP.dab(x, y, (2.5 + R() * 5) * sz, lum > .5 ? pal.folMid : pal.folDark, { a: .85, den: .3, s: 2.4 * U }); return; }   // the ragged rim: single leaves
  let col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .6 && k < .3) col = pal.folSun; else if (pal.accent && lum > .3 && k > .92) col = pick(pal.accent, R());
  const a = R() * Math.PI, l = (4 + R() * 8) * sz;
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (4 + R() * 6) * sz, col, { a: .85, sp: 1.3 * U, dry: .2, jit: 10 });
}
function bankStroke(x, y) {                                    // under the trees it is dark; the wall along the water is pale
  const R = PP.R, xr = fx(x) + SH, pf = fy(y), top = bankTop(xr), foot = SHORE + (xr >= BANKX ? DEND : .003), t = (pf - top) / (foot - top);
  let l = (4 + R() * 9) * U, col;
  veil(.4);
  if (t > .9) col = mix(pal.waterDark, pal.ink, .4);
  else if (t > .52) col = mix(pal.stoneBridge, pal.stone, .2 + .35 * fbm(x * .02 / U, y * .08 / U + 3));
  else if (t > .42) col = white ? mix(pal.stoneBridge, pal.snow, white * .8) : mix(pal.stoneBridge, pal.towerLight, .4);
  else col = mix(mix(pal.folDeep, pal.stone, .45), pal.folDark, R() * .5);
  if (zone(fx(x - l), pf) !== BANK || zone(fx(x + l), pf) !== BANK) l *= .25;
  PP.brush(x - l, y, x + l, y, (1.4 + R() * 1.4) * U, col, { a: .9, jit: 6, dry: .06, bow: 0, sp: 1.1 * U });
}
function pavStroke(x, y) {                                     // the pavilion on its float: three white roofs on posts
  const R = PP.R, xr = fx(x) + SH, pf = fy(y), xa = px(PAV0 - SH) + U, xb = px(PAV1 - SH) - U, o = { a: .92, jit: 5, dry: .05, bow: 0, sp: 1.1 * U };
  veil(.45);
  if (pf < .556) {
    const l = Math.min((2 + R() * 3) * U, (xb - xa) / 2), cx = clamp(x, xa + l, xb - l), u = (xr - PAV0) / .0293;
    const yc = Math.max(y, py(Math.max(pavTop(fx(cx - l) + SH), pavTop(fx(cx + l) + SH))) + .8 * U);
    PP.brush(cx - l, yc, cx + l, yc, 1.6 * U, white ? pal.snow : mix(pal.hull, pal.towerLight, u - Math.floor(u) > .5 ? .4 : .05), o);
  } else if (pf < .572) {
    const i = Math.round((xr - PAV0) / .0074), post = Math.abs(xr - PAV0 - i * .0074) < .0012;
    if (post) PP.brush(px(PAV0 + i * .0074 - SH), py(.556), px(PAV0 + i * .0074 - SH), py(.572), 1.3 * U, pal.hull, o);
    else { const l = Math.min((2 + R() * 3) * U, (xb - xa) / 2), cx = clamp(x, xa + l, xb - l); PP.brush(cx - l, y, cx + l, y, 1.5 * U, pf > .567 ? mix(pal.hull, pal.tower, .3) : night ? mix(pal.lights[0], pal.dark, .45) : mix(pal.dark, pal.stone, .25 + R() * .25), o); }
  } else {
    const l = Math.min((3 + R() * 6) * U, (xb - xa) / 2), cx = clamp(x, xa + l, xb - l);
    PP.brush(cx - l, y, cx + l, y, 1.4 * U, pf < .5745 ? pal.hull : mix(pal.dark, pal.ink, .3), o);
  }
}

// ---------- the water ----------
function mirrored(xf, pf) {                                    // the colour of what stands at a point, as the water shows it
  const k = zone(xf, pf);
  if (k !== SKY) return mix(tone(k), pal.waterDark, .12);
  const x = px(xf), y = py(pf), c = dens(x, y);
  return mix(pal.waterLight, c > .15 ? mix(pal.cloud, pal.cloudShade, .4) : skyCol(y), .55);
}
function waterStroke(x, y) {
  const R = PP.R, xf = fx(x), pf = fy(y), d = pf - SHORE, sz = (.6 + clamp(d / .3) * .8) * U;
  const edge = px(deckLeft(d) - SH);
  if (x > edge - U) return;
  veil(flatOff(d) * .9);
  const rip = vnoise(x * .028 / U + time * .45, y * .32 / U + time * .1);
  let col = mix(pal.water, rip > .5 ? pal.waterLight : pal.waterDark, Math.min(1, Math.abs(rip - .5) * 2.1));
  // what stands over there, upside down and drawn out by the ripples
  const mx = xf + (vnoise(x * .05 / U, y * .2 / U + time * .3) - .5) * (.006 + d * .05), my = SHORE - d - (rip - .5) * (.01 + d * .12);
  const up = zone(mx, my), tw = up === TOWER ? zt : null;
  col = mix(col, mirrored(mx, my), (up === SKY ? .42 : .7) * (.6 + .4 * smooth(.2, .5, rip) * smooth(.8, .5, rip)) * (1 - traits.fog * .5));
  col = mix(col, pal.waterDark, .3 * smooth(.05, .4, d));                              // close by one looks down into it
  if (x > edge - (3 + d * 40) * U) col = mix(col, pal.waterDark, .45);                // and it is dark under the deck
  let l = (8 + R() * 22) * sz, w = (1.4 + R() * 2.2) * sz;
  if (R() < .2) { col = mix(col, R() < .55 ? pal.waterDark : pal.waterLight, .45); l *= .6; w = (.9 + R() * .5) * sz; }   // a ripple, drawn sharp
  PP.brush(x - l, y, x + Math.min(l, edge - x - U), y + (R() - .5) * U, w, col, { a: .85, jit: 6, dry: .15, sp: 1.1 * U, bow: 0 });
  if (night && tw && R() < tw.lit * .1) { glow(.5); PP.brush(x - 2 * sz, y, x + 2 * sz, y, 1.2 * U, pick(pal.lights, R()), { a: .45, sp: U, bow: 0, jit: 4 }); }   // its windows, glittering
}

// ---------- the deck ----------
function plankStroke(x, y) {
  const R = PP.R, m = zm, cv = zc, d = fy(y) - SHORE, i = Math.floor(m / DM), u = m / DM - i, h = hash1(i * 7 + 11);
  const j = Math.floor(1 / (cv * GL) - h), g0 = (j + h) * GL;                         // which board of its row: it lies from g0 to g0 + GL out
  const beyond = cv < CEND && m < NR;                                                  // the bit of deck beyond the last net
  const dMin = m >= NR || beyond ? DEND : dOf(m, CV[0]), dMax = beyond ? dOf(m, CEND) : 9;
  const wide = DM * d * PW, cos = PH / Math.hypot(m * PW, PH);                         // how wide a board is here, and how much of a stroke along it goes down
  const o = { a: .9, jit: 7, dry: .12, bow: 0, sp: 1.1 * U };
  const gap = white ? mix(pal.gap, pal.snow, white * .35) : pal.gap;
  veil(flatOff(d) * .7);
  const run = (mm, da, db, bw, col) => { if (db > da) { o.sp = Math.min(1.2 * U, bw); PP.brush(px(VX + mm * da - SH), py(SHORE + da), px(VX + mm * db - SH), py(SHORE + db), bw, col, o); } };
  const l = (6 + R() * 16) * U * (.45 + d * 2.2) * cos / PH;
  if (u < .12) { run((i + .05) * DM, Math.max(dMin, d - l), Math.min(dMax, d + l), Math.max(U, wide * .11 * cos), gap); return; }   // the gap between two boards
  const dFar = dOf(m, 1 / (g0 + GL)), dNear = g0 > 0 ? dOf(m, 1 / g0) : 9, jg = Math.max(.6 * U, wide * .035) / PH;
  if (dFar > dMin && R() < .08) {                                                      // where it ends and the next one begins
    const ya = py(SHORE + dFar);
    PP.line([[px(VX + (i + .14) * DM * dFar - SH), ya], [px(VX + (i + .98) * DM * dFar - SH), ya]], gap, Math.max(.9 * U, wide * .045), { pass: 1, wob: .2 * U, a: .85, step: 999 });
    return;
  }
  const hb = hash1(i * 131 + j * 1777), bw = Math.max(1.2 * U, wide * cos * (.14 + R() * .22)), hu = Math.min(.4, bw / (2 * wide * cos));
  let col = mix(pal.plank, hb > .5 ? pal.plankLight : pal.plankDark, Math.abs(hb - .5) * 1.3);
  col = mix(col, R() < .5 ? pal.plankLight : pal.plankDark, R() * .22);               // its grain
  col = mix(col, pal.plankLight, .22 * flatOff(d));                                   // far off the light glances off them
  if (wet) col = mix(mix(col, pal.plankDark, .35), pal.skyLow, .22);
  if (white) col = mix(col, pal.snow, white * (.5 + .45 * vnoise(x * .012 / U, y * .02 / U + 5)));
  run((i + clamp(u, .12 + hu, 1 - hu)) * DM, Math.max(dMin, dFar + jg, d - l), Math.min(dMax, dNear - jg, d + l), bw, col);
}
function netStroke(x, y) {                                     // a net in its frame: pale beams, the yellow one at its far end it is laced to
  const R = PP.R, m = zm, cv = zc, d = fy(y) - SHORE, o = { a: .92, jit: 6, dry: .06, bow: 0, sp: 1.1 * U };
  let k = 0; while (k < CV.length - 2 && cv < CV[k + 1]) k++;
  const c0 = CV[k], dc = c0 - CV[k + 1], t = (c0 - cv) / dc, deep = dc * PH;
  const snowy = (c, n) => white ? mix(c, pal.snow, white * n) : c;
  const xy = (mm, cc) => { const dd = dOf(mm, cc); return [px(VX + mm * dd - SH), py(SHORE + dd)]; };
  veil(flatOff(d) * .7);
  if (m < NL + BEAM || m > NR - BEAM) {                        // the beams along its sides
    const left = m < 0, mm = left ? NL + BEAM / 2 : NR - BEAM / 2, cos = PH / Math.hypot(mm * PW, PH), l = (4 + R() * 10) * U * (.4 + d * 3) * cos / PH;
    const da = Math.max(dOf(mm, CEND), d - l), db = Math.min(dOf(mm, CV[0]), d + l), bw = Math.max(1.2 * U, BEAM * d * PW * cos * .85);
    o.sp = Math.min(1.1 * U, bw);
    if (db > da) PP.brush(px(VX + mm * da - SH), py(SHORE + da), px(VX + mm * db - SH), py(SHORE + db), bw, snowy(mix(pal.beam, pal.beamDark, left ? .3 + R() * .2 : .1), .8), o);
    return;
  }
  let tc, th, col, m0 = NL, m1 = NR;
  if (t < .13) { tc = .065; th = .13; col = snowy(mix(pal.beam, pal.beamDark, .15 + R() * .2), .8); }
  else if (t > .91) { tc = .955; th = .09; col = snowy(mix(pal.beam, pal.plankLight, .2), .8); }
  else if (t > .7) {                                           // the yellow one, dark where the net hangs from it
    tc = clamp(t, .73, .88); th = Math.min(.2, 3 * U / deep); col = t < .75 ? mix(pal.bolsterDark, pal.net, .35) : snowy(mix(pal.bolster, pal.bolsterDark, R() * .3), t > .86 ? .8 : 0);
    m0 += BEAM * .6; m1 -= BEAM * .6;
  } else {
    m0 += BEAM; m1 -= BEAM;
    const lace = R() < .42;
    if (lace && R() < .5) {                                    // a cord of it, running away from us
      const a = xy(clamp(m, m0, m1), c0 - dc * .14), b = xy(clamp(m, m0, m1), c0 - dc * .7);
      PP.brush(a[0], a[1], b[0], b[1], .8 * U, pal.netLine, { a: .6, jit: 5, dry: .1, bow: 0, sp: .8 * U });
      return;
    }
    tc = clamp(t, .16, .68); th = lace ? .8 * U / deep : Math.min(.3, 3 * U / deep);
    col = lace ? pal.netLine : mix(pal.net, pal.waterDark, .22);
    if (white > .4 && !lace && vnoise(x * .02 / U, y * .04 / U + 9) > .5) col = mix(col, pal.snow, white * .8);   // snow lies in it
    if (lace) o.a = .6;
  }
  const dm = (4 + R() * 9) * U * (.5 + d * 2.5) / Math.max(1, d * PW), a = xy(clamp(m - dm, m0, m1), c0 - dc * tc), b = xy(clamp(m + dm, m0, m1), c0 - dc * tc);
  o.sp = Math.min(1.1 * U, Math.max(.8 * U, th * deep * .9));
  PP.brush(a[0], a[1], b[0], b[1], Math.max(.8 * U, th * deep * .9), col, o);
}

// ---------- every stroke where it belongs ----------
const STROKES = [null, towerStroke, shoreStroke, pylonStroke, bridgeStroke, lighthouseStroke, treeStroke, bankStroke, pavStroke, waterStroke, plankStroke, netStroke];
const within = (xa, ya, xb, yb, lo, hi) => () => {              // a stroke somewhere in a box, if what is there is of the kind asked for
  const R = PP.R, x = xa + R() * (xb - xa), y = ya + R() * (yb - ya);
  if (x < 0 || x >= W || y >= H || !at(x, y)) return;
  const k = zone(fx(x), fy(y));
  if (k >= lo && k <= hi) STROKES[k](x, y);
};
let boxes = [], lampN = 0, landTop = 0, tops = null;
function thinStroke() {                                        // what is too thin to be met by chance: the antennas, the lamps on the bridge, the yellow legs
  const R = PP.R, n = THIN.length + lampN + (legs ? 4 : 0) + 1, i = (R() * n) | 0;
  let xf, pf, want = TOWER;
  if (i < THIN.length) { const t = THIN[i]; xf = t.c; pf = t.y0 + R() * .085; }
  else if (i < THIN.length + lampN) { xf = LAMP0 + (i - THIN.length) * LAMPS; pf = brTop(xf) - R() * LAMPH; want = BRIDGE; }
  else if (i < n - 1) { const g = LEGS[i - THIN.length - lampN], t = R(); xf = g[0] + (g[2] - g[0]) * t; pf = g[1] + (g[3] - g[1]) * t; want = PYLON; }
  else { xf = LX - SH; pf = .424 + R() * .033; want = LHOUSE; }
  const x = px(xf), y = py(pf);
  if (x < 0 || x >= W || !at(x, y) || zone(xf, pf) !== want) return;
  STROKES[want](x, y);
}

// ---------- lights ----------
// The lamps along the far shore, the lit columns at the foot of the high tower, the lamps of the bridge, the
// pavilion and the bank: each a dab of light by night, and a long glitter on the water under it.
const WHITE_LIGHT = hex('#f1f6ff');
let lights = [];
function buildLights() {
  lights = [];
  const add = (xf, pf, kind, k, mirror) => {
    const x = px(xf), z = zone(xf, pf);
    if (x > -10 * U && x < W + 10 * U && (z === SHOREZ || z === BRIDGE || z === BANK || z === PAV)) lights.push({ x, y: py(pf), kind, k, mirror, h: hash1(lights.length * 13 + 5) });
  };
  for (let i = Math.ceil((X0 - .02) / .034); i * .034 < .47; i++) add((i + (hash1(i * 7 + 1) - .5) * .5) * .034, .567, 0, .55, true);
  for (let i = 0; i < 8; i++) add(.062 + i * .0112, .557, 1, 1, true);
  for (let i = 0; i < lampN; i++) add(LAMP0 + i * LAMPS, brTop(LAMP0 + i * LAMPS) - LAMPH + .0015, 2, .85, true);
  for (let i = 0; i < 6; i++) add(.713 + i * .0128 - SH, .5625, 0, .6, true);
  for (let i = 0; .8 + i * .023 < fx(W) + SH + .02; i++) { const xr = .8 + (i + (hash1(i * 5 + 3) - .5) * .5) * .023; add(xr - SH, bankTop(xr) + .009, 0, .5, xr < BANKX - .01); }
}
const lightCol = l => l.kind === 1 ? WHITE_LIGHT : l.kind === 2 ? pal.lights[l.h < .5 ? 1 : 3] : pal.lights[l.h < .4 ? 0 : l.h < .7 ? 4 : 2];
function lampDab() {
  const R = PP.R, l = pick(lights, R());
  if (!l || !at(l.x, l.y) || !night) return;
  PP.haze = null;                                              // a light is seen through any fog
  if (R() < .35) PP.dab(l.x, l.y, (4 + 5 * l.k) * U, lightCol(l), { a: .1, den: .4, s: 2.4 * U, jit: 3 });
  PP.dab(l.x, l.y, (.6 + .6 * l.k) * U, lightCol(l), lit());
}
function streak() {                                            // a lamp on the water: drawn out long, broken by the ripples
  const R = PP.R, l = pick(lights, R());
  if (!l || !l.mirror) return;
  const f = Math.pow(R(), 1.6), reach = (.04 + .2 * l.k) * PH, y = place.hz + 2 * U + f * reach, x = l.x + (R() - .5) * (3 * U + f * reach * .14);
  if (x < 0 || x >= W || !at(x, y) || !night || zone(fx(x), fy(y)) !== WATER) return;
  const w = (2 + R() * 4 + f * 5) * U;
  glow(.5);
  PP.brush(x - w, y, x + w, y, (1 + R() * .8) * U, lightCol(l), { a: .6 * (1 - f) * (.5 + .5 * l.k), sp: U, bow: 0, jit: 4, dry: .1 });
}

// ---------- weather ----------
function fogStroke() {                                         // fog lies on the water in drifting banks
  const R = PP.R, x = R() * W, d = R() * .1 - .012, y = py(SHORE + d);
  if (!at(x, y) || !traits.fog) return;
  const z = zone(fx(x), fy(y));
  if ((z !== WATER && z !== SHOREZ) || fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) < .42 + Math.abs(d - .02) * 3) return;
  const l = (14 + R() * 30) * U, e = px(deckLeft(Math.max(0, d)) - SH);
  if (x > e - U) return;
  PP.haze = null;
  PP.brush(x - l, y, Math.min(x + l, e - U), y + (R() - .5) * 2 * U, (3 + R() * 6) * U, fogCol(), { a: night ? .17 : .3, jit: 3, dry: .25, sp: 2 * U, bow: .03 });
}
function glintDab() {                                          // the sun on the water, and on the snow
  const R = PP.R, x = R() * W, y = place.hz + R() * (H - place.hz);
  if (!at(x, y) || !sun) return;
  const z = zone(fx(x), fy(y));
  PP.haze = null;
  if (z === WATER) { if (vnoise(x * .01 / U + time * .1, y * .05 / U) > .56) PP.brush(x - 2.5 * U, y, x + 2.5 * U, y, 1.1 * U, pal.sun, { a: .8, jit: 2, dry: 0, bow: 0, sp: U }); }
  else if (z === PLANK && traits.glint && R() < .3) PP.dab(x, y, (.7 + R() * .9) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x0, y0) {                                  // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U) continue;
    const xf = fx(clamp(x, 0, W - 1)), pf = fy(Math.min(y, H - 1)), k = zone(xf, pf), l = (14 + R() * 8) * U, d = pf - SHORE;
    if (k === TOWER) {                                         // an upright stroke, as broad as the tower if it is a slender one
      const t = zt; if (t.kind === 'mast' || t.kind === 'spire') continue;
      const xa = px(t.x0), xb = px(t.x1), w = Math.min(24 * U, xb - xa), cx = clamp(x, xa + w / 2, xb - w / 2);
      const ya = t.top ? py(Math.max(t.top(fx(cx - w / 2)), t.top(fx(cx)), t.top(fx(cx + w / 2 - 1)))) : py(t.y0);
      veil(.7);
      if (y + l > ya) PP.brush(cx, Math.max(ya + 2 * U, y - l), cx, Math.min(py(SHORE), y + l), w, tone(k), o);
    } else if (k === WATER) {
      const e = px(deckLeft(d) - SH) - 2 * U; if (x > e) continue;
      const up = zone(xf, SHORE - d);
      veil(flatOff(d) * .9);
      PP.brush(x - l * 1.6, y, Math.min(x + l * 1.6, e), y, 26 * U, mix(pal.water, up === SKY ? pal.waterLight : tone(up), up === SKY ? .3 : .45), o);
    } else if (k === PLANK) {                                  // along the boards
      const w = 22 * U, m = Math.max(zm, EDGE + w * .6 / (d * PW)), cos = PH / Math.hypot(m * PW, PH), dl = l * 1.4 * cos / PH;
      const da = Math.max(d - dl, m >= NR || zc < CEND ? DEND : dOf(m, CV[0])), db = d + dl;
      veil(flatOff(d) * .7);
      if (db > da) PP.brush(px(VX + m * da - SH), py(SHORE + da), px(VX + m * db - SH), py(SHORE + db), w, tone(k), o);
    } else if (k === NET) { veil(flatOff(d) * .7); PP.brush(x - l * .6, y, x + l * .6, y, Math.min(12 * U, d * PH * .5), tone(k), o); }
    else if (k === TREE) { const a = (R() - .5) * 1.2; veil(.4); PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, 22 * U, tone(k), o); }
  }
  if (y0 < py(.49) - 30 * U || y0 > py(SHORE + DEND) + 30 * U) return;
  for (let n = 0; n < 18; n++) {                               // narrow strokes, close together: the shore, the bridge and the bank are narrow things
    const x = x0 + (n % 3 - 1 + (R() - .5) * .5) * 22 * U, y = y0 + ((n / 3 | 0) - 2.5 + (R() - .5) * .5) * 6.6 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 8 * U) continue;
    const k = zone(fx(clamp(x, 0, W - 1)), fy(Math.min(y, H - 1))), l = 15 * U;
    if (k === LHOUSE) { const hw = Math.max(lhHW(fy(y)), .004) * PW, cx = px(LX - SH); veil(.5); PP.brush(cx, y - 8 * U, cx, y + 8 * U, hw * 2, tone(k), o); }
    else if (k === SHOREZ || k === BRIDGE || k === BANK || k === PAV) { veil(k === SHOREZ ? .8 : .5); PP.brush(x - l, y, x + l, y, 9 * U, tone(k), o); }
  }
}

// ---------- what moves ----------
// The red lights on the tall ones, that aircraft keep clear of them: [x, y, how big].  The one on top of the antenna
// blinks, the others burn: down the antenna, at the corners of the roofs, down the edges of the high tower and
// at every fold of its side.
const BEACONS = [[.1055, -.034, 1], [.1055, -.012, .45], [.1055, .012, .45], [.057, .067, .6], [.117, .031, .6], [.152, .071, .6],
  [.164, .2265, .6], [.197, .2225, .5], [.2555, .2225, .6], [.163, .37, .4], [.2565, .37, .4], [.2846, .346, .7], [.263, .373, .4], [.297, .373, .4],
  [.3281, .273, .7], [.32, .296, .45], [.37, .296, .45], [-.08, .39, .6], [.004, .405, .5], [-.2275, .418, .6], [-.3125, .468, .5], [-.47, .452, .5]]
  .concat([.2, .33, .46].map(pf => [dcLeft(pf), pf, .4]), [.155, .24, .325, .41, .495].map(pf => [dcEdge(pf), pf, .45]), [.11, .2, .285, .37, .455].map(pf => [dcFold(pf), pf, .3]));
function drawLights(P, t) {
  for (let i = 0; i < BEACONS.length; i++) {
    const b = BEACONS[i], x = px(b[0]), y = py(b[1]);
    if (x < 0 || x >= W || !at(x, y) || !night || zone(b[0], b[1] + .004) > TOWER) continue;   // not behind a tree
    const fog = traits.fog > 0, a = fog ? .8 + .2 * Math.sin(t * .9 + i * 1.9) : b[2] < 1 ? .85 : Math.pow(Math.max(0, Math.sin(t * 2.1)), 2), s = Math.sqrt(b[2]);
    if (a < .05) continue;
    P.R = rng(7000 + i);                                       // each keeps its own shape while it glows
    P.dab(x, y, (fog ? 12 : 8) * U * s, pal.beacon, { a: a * .34, den: .5, s: 2.4 * U, jit: 3 });
    P.dab(x, y, 2.3 * U * s, fog ? mix(pal.beacon, pal.lights[2], .25) : pal.beacon, { a, den: 2.5, core: a, s: 1.1 * U, jit: 3 });
  }
  const x = px(LX - SH), y = py(.444);                          // and the lamp of the lighthouse
  if (x < 0 || x >= W || !at(x, y) || !night) return;
  const a = .75 + .25 * Math.sin(t * .8);
  P.R = rng(7100);
  P.dab(x, y, 10 * U, pal.lights[2], { a: a * .22, den: .5, s: 2.6 * U, jit: 3 });
  P.dab(x, y, 2.2 * U, pal.lights[2], { a, den: 2.5, core: a, s: U, jit: 3 });
}
function drawSwans(P, t) {                                     // now and then two swans come by
  const u = (t % 190) / 150; if (u > 1) return;
  for (let i = 0; i < 2; i++) {
    const pf = .69 + i * .024, s = (.0075 + i * .0012) * PH, x = -40 * U + (u * 1.1 - i * .045) * (W + 80 * U), y = py(pf) + Math.sin(t * 1.3 + i * 2) * .5 * U;
    if (x < 0 || x >= W || !at(x, y) || traits.gust || zone(fx(x - s), pf) !== WATER || zone(fx(x + 2.2 * s), pf) !== WATER) continue;
    const body = night ? mix(pal.hull, pal.water, .3) : pal.hull;
    P.haze = hazeOf(flatOff(pf - SHORE) * .8);
    P.R = rng(8100 + i);
    P.brush(x - s * 3.2, y + s * .4, x - s * 1.1, y + s * .34, 1.1 * U, pal.waterLight, { a: .5, sp: U, dry: .2, bow: .1 });            // what it leaves behind on the water
    P.dab(x, y + s * .7, s * .75, mix(body, pal.water, .6), { a: .45, den: 1.4, core: .3, sx: 1.5, sy: .4, s: 1.2 * U, jit: 3 });      // and its image in it
    P.dab(x, y, s * .85, body, { a: 1, den: 2.6, core: 1, sx: 1.5, sy: .62, s: 1.2 * U, jit: 3 });
    P.brush(x - s * 1.25, y - s * .3, x - s * .5, y - s * .05, s * .5, body, { a: 1, sp: U, dry: 0, jit: 3, bow: .1 });
    P.line([[x + s * .8, y - s * .1], [x + s * 1.2, y - s * .7], [x + s * 1, y - s * 1.3], [x + s * 1.3, y - s * 1.55]], body, Math.max(1.3 * U, s * .3), { pass: 2, a: 1, wob: .15 * U, step: 999 });
    P.dab(x + s * 1.55, y - s * 1.45, Math.max(.8 * U, s * .16), pal.cityBeak, { a: 1, den: 2.5, core: 1, s: U, jit: 3 });
  }
  P.haze = null;
}

// ---------- the painting, one frame's worth ----------
const RATE = { town: 15000, water: 6500, deck: 9000, thin: 6, lamp: 4, streak: 40, glint: 80, fog: 1500 };
const place = {
  colors: {
    glassDark: '#27303c', glassBlue: '#4f6a86', towerRib: '#66778a',
    towerDc2: '#26282c', towerDc2Frame: '#575b62', towerWhite: '#f1efe8',
    citySand: '#d9cdb2', cityPane: '#a9c0b8', cityPodium: '#9a6a4a', cityBeach: '#ddd4bc', cityPylon: '#f0d43a', cityPylonDark: '#bf9e2a', cityBeak: '#e58a2c',
    stoneBridge: '#d5d0c2', stoneBridgeDark: '#8f8b80', lhCap: '#56655d',
    plank: '#878e97', plankLight: '#afb3b7', plankDark: '#5f6670', gap: '#1e2228',
    beam: '#cbbd9a', beamDark: '#8f846c', bolster: '#e0b233', bolsterDark: '#a87d22', net: '#16181c', netLine: '#4c5057', snow: '#eef2f4',
  },
  ground: /^snow/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    // A wide screen loses some of the boards at our feet, an upright one gets more sky and is pushed together a bit.
    const a = W / H, wide = clamp(a - 4 / 3), tall = clamp((4 / 3 - a) / .87), shown = 1.05 - .12 * wide + .17 * tall;
    Y0 = -.05 - .17 * tall; PH = H / shown; PW = PH * 4 / 3 * (1 - .32 * tall);
    const n = W / PW;                                          // how many photos wide the picture is
    if (n >= 1) { X0 = -(n - 1) * .25; SH = 0; } else { X0 = .02 * tall; SH = Math.max(0, .985 - X0 - n); }   // what there is more room for is mostly deck and trees, on the right
    legs = SH < .25;
    lampHW = Math.max(.0009, .8 * U / PW); finHW = Math.max(.0011, .8 * U / PW);
    for (const t of THIN) { const hw = Math.max(.0011, .8 * U / PW); t.x0 = t.c - hw; t.x1 = t.c + hw; }
    place.hz = py(SHORE);
    const m = Math.ceil(W) + 1;
    tops = new Float32Array(m); landTop = H;
    for (let i = 0; i < m; i++) { tops[i] = py(skyTop(fx(i + .5))); if (tops[i] < landTop) landTop = tops[i]; }
    landTop = Math.max(0, landTop - 8 * U);
    lampN = Math.max(0, Math.ceil((fx(W) - LAMP0) / LAMPS));
    const ua = U * U * 1e6, hz = place.hz, foot = py(SHORE + DEND), split = py(.497), dx = Math.max(0, px(VX + EDGE * (fy(H) - SHORE) - SH));
    const box = (key, rate, xa, ya, xb, yb, lo, hi) => ({ key, n: rate * Math.max(0, xb - xa) * Math.max(0, yb - ya) / ua, fn: within(xa, ya, xb, yb, lo, hi) });
    boxes = [
      box('dp-high', RATE.town, 0, landTop, Math.min(W, px(.385)), split, TOWER, PAV),                 // the towers
      box('dp-right', RATE.town, Math.max(0, px(.78 - SH)), py(.36), W, split, TOWER, PAV),             // the trees and the lighthouse
      box('dp-low', RATE.town, 0, split, W, foot, TOWER, PAV),                                         // the shores, the bridge
      box('dp-water', RATE.water, 0, hz, W, H, WATER, WATER),
      box('dp-deck', RATE.deck, dx, foot, W, H, PLANK, NET),
    ];
    buildLights();
  },
  tick(t) { time = t; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to the roofs, the bridge and the trees
    const dip = 3 * U - pad, n = tops.length;
    let y = tops[n - 1] + dip;
    c.moveTo(0, -60); c.lineTo(W, -60); c.lineTo(W, y);
    for (let i = n - 2; i >= 0; i--) { const e = tops[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(0, y); c.closePath();
  },
  orb(o) {                                                     // the sun and the moon stand clear of the trees
    let top = 1e9;
    for (let i = Math.max(0, (o.x - o.r * 1.6) | 0); i < Math.min(tops.length, o.x + o.r * 1.6); i++) top = Math.min(top, tops[i]);
    if (top < 1e9) o.y = Math.min(o.y, top - o.r * 1.6);
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    clipBand(landTop, H);
    for (const b of boxes) spend(b.key, b.n * pace, b.fn);
    spend('dp-thin', RATE.thin * (THIN.length + lampN + 5) * pace, thinStroke);
    if ((s && s.night) || (o && o.night)) { spend('dp-lamp', RATE.lamp * lights.length * pace, lampDab); spend('dp-streak', RATE.streak * lights.length * pace, streak); }
    if (s && !s.night && s.traits.shadow) spend('dp-glint', RATE.glint * W * (H - place.hz) / (U * U * 1e6) * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('dp-fog', RATE.fog * W * .1 * PH / (U * U * 1e6) * pace, fogStroke);
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
  sprites: [{ draw: drawSwans, t0: 6, seed: 141 }, { draw: drawLights, t0: 4, seed: 142 }],
};
return place;
} });
