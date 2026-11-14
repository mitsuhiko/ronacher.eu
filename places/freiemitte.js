/* ------------------------------------------------------------------ *
 *  Vienna, over the Freie Mitte: the wild park where the yard of the
 *  Nordbahnhof used to be, seen from above on a May evening.  Paths,
 *  boardwalks and trails through the meadow and the bushes, the old
 *  water tower on the left, the new towers of the Nordbahnviertel
 *  behind it all and the town beyond.  scene.js paints the sky above
 *  it and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'freiemitte', name: 'Freie Mitte', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let HZ = 0, PW = 1, PH = 1;                 // horizon; the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false;
const use = s => { pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph the picture is painted after: x and y as fractions of its
// width and height.  A wide screen shows all of its width and a slice of its height, a narrow one a
// slice of its width, pushed together a bit.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along2(pts, x) {                                      // a line through points, from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * (x - a[0]) / (b[0] - a[0]); }
  return pts[pts.length - 1][1];
}
function inPoly(p, x, y) {
  let o = false;
  for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o;
  return o;
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
const near = pf => clamp((pf - .3) / .7, .22, 1.3);           // how close the ground is at this height: the size of its strokes
const WHITE = [255, 255, 255];

// The town ends at the horizon in a ragged line of roofs, a few towers standing out of it.
const HOR = .385;
const farTop = xf => { const i = Math.floor(xf * 150); return HOR - .003 - hash1(i * 13 + 5) * .011 * (.4 + vnoise(xf * 9, 2.2)); };
const DIST = [[.3595, .3685, .378, 'tower'], [.4285, .4297, .371, 'chimney'], [.516, .525, .365, 'mosaic'], [.56, .573, .371, 'glass'], [.5732, .5745, .361, 'mast'],
  [.574, .583, .372, 'tower'], [.7962, .7985, .352, 'column'], [.8183, .8196, .367, 'chimney'], [.852, .861, .367, 'tower'], [.861, .869, .369, 'glass'],
  [.869, .878, .371, 'tower'], [.908, .919, .372, 'tower']];
const distAt = (xf, pf) => { for (const d of DIST) if (xf >= d[0] && xf < d[1] && pf >= d[2]) return d; return null; };

// ---------- the new town ----------
// [left, right, top at the left, top at the right, kind, wall, how much in the shade, floor height, column width, how far off],
// from the back to the front.  Kinds: grid (windows in rows), bands (balconies all along), checker (the white tower:
// a box for every flat, staggered), tall (the high one: balconies on top, windows below), red (frame and glass), slab
// (shutters on the left, balconies on the right), dark (dark bands).
const BLD = [
  [0, .031, .379, .379, 'bands', 'cityWhite', .1, .0095, .01, .35], [.031, .072, .381, .381, 'grid', 'cityPink', 0, .0095, .0068, .3],
  [.072, .094, .381, .383, 'bands', 'cityPink', .4, .0095, .011, .3], [.12, .2, .3825, .3825, 'grid', 'cityCream', .05, .0088, .0062, .5],
  [.09, .125, .394, .394, 'bands', 'cityWhite', .15, .009, .01, .45], [.094, .125, .431, .431, 'grid', 'citySand', .2, .009, .007, .35],
  [.167, .25, .392, .392, 'bands', 'cityWhite', .1, .0092, .012, .4],
  [.546, .595, .4, .4, 'grid', 'cityCream', .1, .0085, .006, .55], [.595, .645, .395, .395, 'bands', 'cityWhite', .1, .0085, .01, .55],
  [.64, .662, .393, .393, 'grid', 'cityWhite', .25, .0085, .006, .5],
  [.4225, .49, .415, .415, 'bands', 'cityWhite', 0, .009, .01, .4], [.49, .546, .41, .41, 'grid', 'cityCream', .1, .009, .0065, .4],
  [.247, .331, .265, .255, 'checker', 'cityWhite', 0, .0099, .0094, .12], [.331, .4125, .255, .273, 'checker', 'cityWhite', .35, .0099, .009, .12],
  [.4075, .494, .468, .47, 'red', 'cityRed', 0, .0105, .011, .18], [.52, .655, .4375, .4375, 'slab', 'cityGray', .1, .0095, .0075, .18],
  [.659, .6925, .117, .11, 'tall', 'cityWhite', .25, .0102, .0085, .08], [.6925, .78, .11, .113, 'tall', 'cityWhite', 0, .0102, .0073, .08],
  [.925, 1.02, .39, .39, 'grid', 'cityCream', .1, .009, .007, .5], [.869, .925, .395, .395, 'grid', 'cityWhite', .1, .009, .007, .5],
  [.775, .846, .39, .39, 'bands', 'cityWhite', .05, .009, .01, .4], [.844, .899, .3925, .3925, 'bands', 'cityCream', .1, .009, .01, .45],
  [.932, .981, .336, .336, 'bands', 'cityWhite', 0, .0095, .012, .35], [.839, .887, .415, .415, 'bands', 'cityWhite', 0, .0092, .01, .3],
  [.773, .8275, .4225, .4225, 'dark', 'cityDark', .1, .0095, .01, .2],
].map(([x0, x1, t0, t1, kind, wall, shade, fh, cw, far], id) => ({ x0, x1, t0, t1, kind, wall, shade, fh, far, id, n: Math.max(1, Math.round((x1 - x0) / cw)), side: kind === 'tall' && x1 < .7 }));
const topOf = (b, xf) => b.t0 + (b.t1 - b.t0) * (xf - b.x0) / (b.x1 - b.x0);
function bldAt(xf, pf) { for (let i = BLD.length - 1; i >= 0; i--) { const b = BLD[i]; if (xf >= b.x0 && xf < b.x1 && pf >= topOf(b, xf)) return b; } return null; }
const PODIUM = .483, UPPER = .325;                            // where the white tower stands on its shops; where the balconies of the high one end

// What is at a point of a front: the colour, and how far it goes on across (fu0, fu1, in columns) and up and down (fv0, fv1, in floors).
let fc = null, fu0 = 0, fu1 = 1, fv0 = 0, fv1 = 1, fLit = false;
function windowCol(b, h, k) {                                  // a window: the sky in it by day, a light in it at night now and then
  fLit = night && h < (b.far > .3 ? .32 : .26);
  if (fLit) return pal.lights[(k * pal.lights.length) | 0];
  return night ? mix(pal.dark, pal.glass, .15) : mix(mix(mix(pal.dark, pal.glass, .45 + k * .35), pal.skyMid, k > .75 ? .35 : 0), pal[b.wall], .3 + b.far * .3);
}
function facade(b, fl, ci, uu, vv, u, pf) {
  const wall = pal[b.wall], h = hash1(b.id * 7919 + fl * 131 + ci * 17), k = hash1(b.id * 31 + fl * 7 + ci * 911);
  const band = (a, z, c) => { fv0 = a; fv1 = z; fu0 = -9; fu1 = 10; fc = c; };          // the same all along the floor
  const cell = (a, z, ua, ub, c) => { fv0 = a; fv1 = z; fu0 = ua; fu1 = ub; fc = c; };
  fLit = false;
  switch (b.kind) {
    case 'grid':
      if (vv < .2) return band(0, .2, mix(wall, pal.hull, .3));
      if (vv < .3 || vv >= .82) return vv < .3 ? band(.2, .3, wall) : band(.82, 1, wall);
      if (uu < .2) return cell(.3, .82, -.2, .2, wall);
      if (uu >= .8) return cell(.3, .82, .8, 1.2, wall);
      return cell(.3, .82, .2, .8, windowCol(b, h, k));
    case 'bands':
      if (vv < .3) return band(0, .3, mix(wall, pal.hull, .4));
      if (uu < .08) return cell(.3, 1, 0, .08, mix(wall, pal.dark, .15));
      return cell(.3, 1, .08, 1, h < .6 && !night ? mix(wall, pal.dark, .3 + k * .1) : windowCol(b, h, k));
    case 'checker': {
      if (pf >= PODIUM) {                                      // the shops it stands on: glass between piers
        if (uu < .16) return cell(0, 1, 0, .16, mix(wall, pal.hull, .3));
        return cell(0, 1, .16, 1, night ? (h < .5 ? mix(pal.lights[(k * 3) | 0], pal.dark, .25) : mix(pal.dark, pal.glass, .2)) : mix(mix(pal.dark, pal.glass, .55), wall, .35));
      }
      if (vv < .18) return band(0, .18, mix(wall, pal.hull, .6));
      if (uu < .16) return cell(.18, 1, 0, .16, mix(wall, pal.hull, .6));
      if (uu >= .86) return cell(.18, 1, .86, 1, mix(wall, pal.dark, .16));                // its box stands out: the shade beside it
      if (vv >= .74) return cell(.74, 1, .16, .86, mix(pal.hull, pal.glass, .2));          // the glass of the balustrade
      return cell(.18, .74, .16, .86, k > .95 && !night ? mix(pick(pal.curtain, h), wall, .3) : night ? windowCol(b, h, k) : mix(windowCol(b, h, k), mix(wall, pal.glass, .25), .35 + h * .3));
    }
    case 'tall':
      if (pf >= .533) {                                        // its lobby
        if (uu < .2) return cell(0, 1, 0, .2, wall);
        return cell(0, 1, .2, 1, night ? mix(pal.lights[0], pal.dark, .3) : mix(mix(pal.dark, pal.glass, .55), wall, .3));
      }
      if (!b.side && pf < UPPER) {                             // the balconies
        if (vv < .22) return band(0, .22, mix(wall, pal.hull, .4));
        if (vv < .58) return cell(.22, .58, 0, 1, (ci % 3) === 1 ? mix(pal.cityYellow, wall, .15) : mix(pal.cityGreenGlass, wall, .1 + k * .2));
        return cell(.58, 1, 0, 1, windowCol(b, h, k));
      }
      if (h < .55 && uu >= .22 && uu < .78 && vv >= .22 && vv < .8) return cell(.22, .8, .22, .78, windowCol(b, h * 1.8, k));
      return vv < .22 || vv >= .8 ? (vv < .22 ? band(0, .22, wall) : band(.8, 1, wall)) : uu < .22 ? cell(.22, .8, -.22, .22, wall) : cell(.22, .8, .78, 1.22, wall);
    case 'red':
      if (fl === 0 && vv < .5) return band(0, .5, mix(pal.folMid, pal.folDark, k * .5));      // the garden on its roof
      if (vv < .2) return band(0, .2, wall);
      if (uu < .16) return cell(.2, 1, 0, .16, wall);
      return cell(.2, 1, .16, 1, windowCol(b, h, k));
    case 'slab':
      if (u < .32) {                                           // windows, some with yellow shutters
        if (vv < .25 || vv >= .8) return vv < .25 ? band(0, .25, wall) : band(.8, 1, wall);
        if (uu < .22 || uu >= .78) return uu < .22 ? cell(.25, .8, -.22, .22, wall) : cell(.25, .8, .78, 1.22, wall);
        return cell(.25, .8, .22, .78, h < .45 && !night ? mix(pal.cityYellow, wall, .1) : windowCol(b, h, k));
      }
      if (vv < .3) return band(0, .3, mix(wall, pal.hull, .3));
      return cell(.3, 1, 0, 1, h < .6 && !night ? mix(wall, pal.dark, .4) : windowCol(b, h, k));
    default:                                                   // dark: dark bands, white between
      if (vv < .4) return band(0, .4, wall);
      if (uu < .12) return cell(.4, 1, 0, .12, pal.hull);
      return cell(.4, 1, .12, 1, windowCol(b, h, k));
  }
}

// ---------- the water tower ----------
// Seen from above: the left wall in the shade and the front in the sun, under a hipped roof of slate.
const WL = [.122, .456], WF = [.1365, .4615], WR = [.205, .4525], WP = [.162, .4345], WH = .09;   // eave corners: left, front, right; the peak; how high the walls are
const eaveY = xf => xf < WF[0] ? WL[1] + (WF[1] - WL[1]) * (xf - WL[0]) / (WF[0] - WL[0]) : WF[1] + (WR[1] - WF[1]) * (xf - WF[0]) / (WR[0] - WF[0]);
const side = (a, b, x, y) => (b[0] - a[0]) * (y - a[1]) - (b[1] - a[1]) * (x - a[0]);
const inTri = (a, b, c, x, y) => { const d1 = side(a, b, x, y), d2 = side(b, c, x, y), d3 = side(c, a, x, y); return !((d1 < 0 || d2 < 0 || d3 < 0) && (d1 > 0 || d2 > 0 || d3 > 0)); };
const CHIM = [.1705, .1735, .4355];                            // the chimney: left, right, top
const LEFTWALL = 1, FRONTWALL = 2, LEFTROOF = 3, FRONTROOF = 4, CHIMNEY = 5, VANE = 6;
function wtPart(xf, pf) {
  if (xf < WL[0] || xf > WR[0] || pf < WP[1] - .014 || pf > WF[1] + WH + .01) return 0;
  if (Math.abs(xf - WP[0]) < .0006 && pf < WP[1] && pf > WP[1] - .013) return VANE;
  if (xf >= CHIM[0] && xf < CHIM[1] && pf >= CHIM[2] && pf < WP[1] + (xf - WP[0]) * .3 + .006) return CHIMNEY;
  if (inTri(WL, WF, WP, xf, pf)) return LEFTROOF;
  if (inTri(WF, WR, WP, xf, pf)) return FRONTROOF;
  const e = eaveY(xf);
  if (pf < e || pf > e + WH) return 0;
  if (xf >= WL[0] + .0015 && xf < WF[0]) return LEFTWALL;
  if (xf >= WF[0] && xf < WR[0] - .0015) return FRONTWALL;
  return 0;
}
// The front has two bays, the left wall is seen at a slant and shows one: three arches over each, one of them a window,
// a band of stone, then a big arch with a fan light and the door under it.
const BRICK = 0, RECESS = 1, STONE = 2, GLASS = 3, DARK = 4, FRAME = 5, PIPE = 6;
function wtFeature(front, u, v) {
  if (v < .05) return DARK;                                    // the shadow under the eaves
  if (front && Math.abs(u - .5) < .007) return PIPE;
  if (u < .035 || u > .965) return BRICK;
  const bays = front ? 2 : 1, bu = u * bays - Math.floor(u * bays);
  if (v < .095) return (Math.floor(bu * 14) & 1) ? RECESS : BRICK;           // a frieze under the cornice
  if (v >= .43 && v < .465) return STONE;                                     // the belt between the floors
  if (v < .43) {
    for (const c of [.21, .5, .79]) {
      const d = Math.abs(bu - c), hw = .085;
      if (d < .016 && v >= .115 && v < .15) return STONE;                     // a keystone
      if (d >= hw) continue;
      if (v >= .37 && v < .395) return c === .5 ? FRAME : STONE;             // the sill
      const inArch = v >= .21 && v < .37 || (v >= .15 && v < .21 && Math.hypot(d / hw, (.21 - v) / .06) < 1);
      if (inArch) return c === .5 ? (d > hw * .62 || Math.abs(v - .29) < .012 ? FRAME : GLASS) : RECESS;
    }
    return BRICK;
  }
  const d = Math.abs(bu - .5);
  if (d < .03 && v >= .48 && v < .52) return STONE;
  if (d < .36 && (v >= .62 || (v >= .52 && Math.hypot(d / .36, (.62 - v) / .1) < 1))) {   // the big arch
    if (d < .21 && v >= .6 && v < .705 && (v >= .66 || Math.hypot(d / .21, (.66 - v) / .06) < 1)) return Math.abs(bu - .5) < .012 || (v > .66 && v < .672) ? FRAME : GLASS;   // its fan light
    if (d < .21 && v >= .705 && v < .725) return FRAME;
    if (d < .11 && (v >= .82 || (v >= .78 && Math.hypot(d / .11, (.82 - v) / .04) < 1))) return DARK;   // the door
    return RECESS;
  }
  return BRICK;
}

// ---------- the park ----------
// Where the ground begins in front of the town.
const GTOP = [[0, .525], [.1, .53], [.115, .548], [.215, .548], [.235, .52], [.245, .52], [.41, .52], [.5, .525], [.64, .535], [.66, .555], [.78, .555], [.8, .52], [.9, .505], [1.1, .5]];
// Paths: [kind, width close by, points].  Boardwalks of weathered wood, concrete, trails worn into the meadow.
const PAVE = 1, BOARD = 2, DIRT = 3, SAND = 4, PLAY = 5, EARTH = 6;
const PATHS = [
  [PAVE, .045, [[.64, .535], [.65, .548], [.681, .596], [.707, .65], [.727, .675], [.75, .717], [.777, .767], [.826, .858], [.883, 1], [.93, 1.12]]],   // the promenade
  [PAVE, .022, [[.745, .73], [.77, .742], [.8, .737]]],
  [PAVE, .02, [[.797, .567], [.834, .604], [.859, .6375], [.8875, .667], [.9125, .6875], [.94, .7]]],
  [PAVE, .02, [[.919, .654], [.944, .6875], [.9625, .708], [.98, .72]]],
  [PAVE, .014, [[.734, .5625], [.75, .596], [.78, .625]]],
  [PAVE, .03, [[.33, .528], [.42, .53], [.5, .532], [.64, .54]]],
  [PAVE, .024, [[-.03, .815], [0, .793], [.07, .725], [.1375, .66], [.15, .65]]],
  [PAVE, .014, [[.035, .603], [.1, .592], [.196, .582], [.22, .578]]],
  [PAVE, .012, [[.045, .556], [.12, .548]]],
  [PAVE, .03, [[.347, .922], [.343, 1], [.34, 1.12]]],
  [BOARD, .014, [[-.03, .975], [0, .963], [.18, .903], [.306, .866], [.361, .848], [.43, .825], [.526, .786], [.535, .783]]],
  [BOARD, .013, [[.065, .651], [.134, .66], [.206, .667], [.29, .675], [.364, .681], [.512, .697]]],
  [BOARD, .012, [[.352, .552], [.4, .567], [.47, .583], [.547, .6]]],
  [DIRT, .02, [[.402, .578], [.4125, .637], [.426, .674], [.43, .692], [.423, .715], [.433, .756], [.444, .802], [.457, .834], [.474, .88], [.485, .935], [.492, 1], [.5, 1.12]]],
  [DIRT, .012, [[.361, .582], [.365, .615], [.371, .706], [.368, .738], [.372, .793], [.378, .82]]],
  [DIRT, .01, [[.12, .73], [.16, .705], [.2, .69]]],
];
const PATCHES = [                                              // [kind, outline]: sand at the foot of the trail, the playground, bare earth
  [SAND, [[.33, .853], [.4125, .848], [.402, .922], [.344, .926]]],
  [PLAY, [[.7375, .6], [.769, .6125], [.797, .6417], [.825, .6875], [.83, .708], [.8, .704], [.772, .6875], [.756, .658], [.744, .633]]],
  [EARTH, [[.06, .86], [.13, .845], [.14, .9], [.09, .93], [.05, .91]]],
  [EARTH, [[.115, .772], [.16, .768], [.163, .79], [.12, .795]]],
];
// The trees, on the photo as 2000 by 1500 pixels: [x, y of the crown, how far it spreads, kind].  B broad leaves, D darker
// ones, L light (birches, robinias), E elder (in flower in May), Y young ones just planted, P pines.
const TREES = [
  [0, 640, 50, 'B'], [20, 700, 60, 'D'], [70, 760, 70, 'B'], [150, 800, 40, 'B'], [25, 860, 80, 'P'], [95, 900, 55, 'D'], [130, 960, 45, 'B'], [60, 1000, 70, 'B'],
  [20, 1090, 60, 'D'], [120, 1210, 45, 'B'], [40, 1250, 70, 'B'], [80, 1360, 55, 'D'], [85, 820, 28, 'Y'], [262, 805, 24, 'Y'],
  [430, 790, 65, 'B'], [610, 760, 70, 'B'], [530, 790, 110, 'D'], [570, 870, 85, 'B'], [470, 890, 70, 'D'], [660, 920, 60, 'D'], [720, 880, 45, 'B'], [620, 950, 55, 'B'],
  [720, 800, 25, 'Y'], [780, 805, 25, 'Y'], [870, 800, 25, 'Y'], [960, 800, 35, 'B'], [1020, 820, 30, 'Y'], [1230, 790, 30, 'B'], [1600, 800, 30, 'B'], [1520, 790, 30, 'B'],
  [850, 915, 50, 'P'], [700, 990, 40, 'L'], [800, 1000, 35, 'B'], [650, 1060, 45, 'B'], [200, 950, 35, 'B'], [240, 1060, 55, 'B'], [330, 1030, 40, 'B'], [420, 990, 40, 'B'],
  [400, 1150, 65, 'B'], [580, 1165, 75, 'E'], [150, 1230, 60, 'B'], [310, 1280, 85, 'L'], [720, 1300, 65, 'B'], [490, 1360, 90, 'B'], [620, 1400, 85, 'D'], [860, 1450, 60, 'B'], [780, 1480, 50, 'D'],
  [1060, 880, 45, 'B'], [1130, 850, 35, 'B'], [1200, 830, 35, 'B'], [1280, 870, 30, 'B'], [1075, 940, 70, 'B'], [1010, 1010, 50, 'Y'], [1165, 1020, 85, 'L'], [1370, 950, 60, 'B'],
  [1300, 1100, 55, 'B'], [1215, 1110, 70, 'B'], [1110, 1170, 55, 'B'], [1380, 1050, 45, 'B'], [1240, 1220, 60, 'B'], [1320, 1290, 70, 'E'], [1160, 1360, 55, 'L'],
  [1460, 930, 35, 'B'], [1490, 960, 30, 'L'], [1570, 1000, 38, 'B'], [1430, 1110, 28, 'Y'], [1520, 1180, 28, 'Y'], [1660, 1190, 28, 'Y'], [1600, 860, 40, 'B'],
  [1700, 700, 35, 'B'], [1690, 750, 50, 'B'], [1640, 850, 50, 'B'], [1850, 700, 80, 'B'], [1910, 760, 70, 'B'], [1790, 790, 110, 'D'], [1700, 880, 60, 'B'],
  [1890, 900, 55, 'B'], [1960, 960, 50, 'B'], [1770, 1000, 75, 'L'],
];
const BUSHES = [[330, 1150, 120, 55], [750, 1100, 100, 50], [260, 1330, 100, 50], [560, 1270, 80, 45], [1050, 1280, 80, 45], [1240, 1400, 70, 40],
  [950, 1390, 50, 30], [120, 1100, 70, 45], [1450, 1000, 45, 28], [1050, 900, 50, 28], [700, 1420, 90, 45], [1130, 1450, 80, 40],
  [540, 1010, 90, 35], [200, 1180, 70, 40], [60, 1420, 70, 50], [420, 1430, 70, 40], [1390, 1180, 45, 30],
  [1180, 920, 60, 30], [1330, 1000, 50, 35], [1820, 900, 80, 50], [1920, 1020, 70, 50], [1600, 920, 50, 30], [130, 870, 50, 30]];
// The football cage down on the right: a box of steel bars under a green net, the court in it.
const CAGE = [[.8, .733], [.805, .731], [1.1, .74], [1.1, 1.15], [.939, 1.15]], NET = [[.805, .731], [1.1, .74], [1.1, .871], [.894, .8625]], CORNER = .894;
const CRATE = [.705, .731, .692, .723];                        // the container with the graffiti on it: left, right, top, foot
// Thin things: [x at the top, top, x at the foot, foot, width, kind]: the floodlight, wooden posts, the lamps along the promenade.
const POLES = [[.987, .427, .97, .76, .009, 'flood'], [.4775, .513, .478, .587, .0022, 'post'], [.488, .567, .4885, .627, .0022, 'post'], [.48, .643, .4805, .747, .0026, 'post'],
  [.704, .615, .704, .64, .0018, 'lamp'], [.758, .604, .758, .628, .0018, 'lamp'], [.787, .702, .787, .73, .002, 'lamp'], [.925, .885, .925, .95, .0026, 'lamp'], [.655, .538, .655, .556, .0016, 'lamp']];

// ---------- what stands in front: limbs and poles ----------
let mask = null, mCols = 0, mRows = 0, mCell = 4;
let limbs = [], limbWeight = 0, limbLength = 0, poles = [], poleLength = 0, lamps = [];
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function mark(s, v) {
  const n = Math.ceil(s.len / (mCell * .5)), rad = s.w / 2 + .3 * U, m = Math.ceil(rad / mCell);
  for (let i = 0; i <= n; i++) {
    const x = limbX(s, i / n), y = limbY(s, i / n), ix = Math.floor(x / mCell), iy = Math.floor(y / mCell);
    for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
      const cx = ix + dx, cy = iy + dy;
      if (cx < 0 || cy < 0 || cx >= mCols || cy >= mRows || Math.hypot(dx, dy) * mCell > rad + mCell * .5) continue;
      if (mask[cy * mCols + cx] < v) mask[cy * mCols + cx] = v;
    }
  }
}

// Leaves are a heap of crowns: the bushes are crowns and nothing else, every tree adds a few on its limbs.  They are kept
// in a coarse grid of buckets, so that a point need ask only those near it.
let crowns = [], leafy = [], leafArea = 0, buckets = [], bCols = 0, bRows = 0, folTop = 0, folB = 0, folC = null;
const BUCKET = 24;
function buildTrees() {
  crowns = []; limbs = [];
  const S = (PW / 2000 + PH / 1500) / 2;                       // photo pixels to ours
  let r = rng(5);
  for (const [bx, by, rx, ry] of BUSHES) {
    const n = Math.round(rx * ry / 190) + 4;
    for (let i = 0; i < n; i++) {
      const a = r() * TAU, d = Math.sqrt(r());
      crowns.push({ x: px((bx + Math.cos(a) * d * rx) / 2000), y: py((by + Math.sin(a) * d * ry) / 1500), r: (20 + r() * 22) * S, tree: false, type: 'S', thin: 0 });
    }
  }
  const add = (x1, y1, x2, y2, bend, w, depth) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, len });
  };
  const grow = (x, y, a, len, w, depth, rad, t) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len, last = depth >= 4 || len <= 4 * U;
    add(x, y, x2, y2, (r() - .5) * .3, w, depth);
    crowns.push({ x: x2, y: y2, r: rad * (depth < 3 ? .9 + r() * .5 : .75), tree: true, type: t, thin: t === 'Y' ? .15 : t === 'L' ? .08 : 0 });
    if (last) return;
    const k = r() < .75 ? 2 : 3;
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .62 + (r() - .5) * .45, len * (.58 + r() * .2), Math.max(.6 * U, w * .6), depth + 1, rad, t);
  };
  for (const [tx, ty, tr, t] of TREES) {
    r = rng(31 + tx * 7 + ty);                                 // every tree keeps its shape, whatever else is to be seen
    const rad = tr * S, h = rad * 2, x = px(tx / 2000), foot = py(ty / 1500) + rad * 1.1, w = Math.max(1.2 * U, h * (t === 'Y' ? .014 : .026));
    add(x, foot, x + (r() - .5) * h * .1, foot - h * .32, (r() - .5) * .08, w, 0);
    const trunk = limbs[limbs.length - 1], n = t === 'P' ? 4 : 5;
    for (let i = 0; i < n; i++) {
      const a = -Math.PI / 2 + ((i + .5) / n - .5) * (t === 'P' ? 1.4 : 2.3) + (r() - .5) * .2, k = i === (n >> 1) ? 1 : .65 + r() * .35;
      grow(limbX(trunk, k), limbY(trunk, k), a, h * (.27 - Math.abs(a + Math.PI / 2) * .04) * (.85 + r() * .3), w * .55, 1, h * (t === 'Y' ? .12 : .155), t);
    }
  }
  bCols = Math.ceil(W / BUCKET) + 1; bRows = Math.ceil(H / BUCKET) + 1;
  buckets = new Array(bCols * bRows);
  folTop = H; leafArea = 0; leafy = [];
  for (const c of crowns) {
    folTop = Math.min(folTop, c.y - c.r);
    const pad = c.r + 4 * U;
    for (let iy = Math.max(0, Math.floor((c.y - pad) / BUCKET)); iy <= Math.min(bRows - 1, Math.floor((c.y + pad) / BUCKET)); iy++)
      for (let ix = Math.max(0, Math.floor((c.x - pad) / BUCKET)); ix <= Math.min(bCols - 1, Math.floor((c.x + pad) / BUCKET)); ix++) (buckets[iy * bCols + ix] = buckets[iy * bCols + ix] || []).push(c);
    if (c.y + c.r > 0 && c.y - c.r < H && c.x + c.r > 0 && c.x - c.r < W) { c.at = leafArea; leafArea += c.r * c.r; leafy.push(c); }
  }
  folTop -= 10 * U;
}
const leafless = c => bare && c.tree && c.type !== 'P';      // in winter a tree is its limbs and nothing else, but for the pines
function fol(x, y) {                                           // > 0 inside the leaves
  folC = null;
  if (y < folTop || x < 0 || x >= W || y >= H + 40 * U) return -1;
  const list = buckets[Math.floor(Math.min(y, H) / BUCKET) * bCols + Math.floor(x / BUCKET)];
  if (!list) return -1;
  const sw = wind(time) * 1.6 * U, fall = season === 'autumn' ? .18 : 0;
  let d = -1;
  folB = 0;
  for (const c of list) {
    const dx = x - c.x - (c.tree ? sw : sw * .4); if (dx > c.r || dx < -c.r || leafless(c)) continue;
    const dy = y - c.y, q = 1 - Math.hypot(dx, dy * 1.1) / c.r - c.thin - (c.tree && c.type !== 'P' ? fall : 0);
    if (q > d) { d = q; folB = dy / c.r; folC = c; }
  }
  return d * 1.2 + (fbm(x * .03 / U + time * .02, y * .03 / U) - .47) * .6;
}
function folLum(x, y) {                                        // the light comes in low from the left
  const s = time * .03, k = .022 / U;
  const rel = fbm(x * k + s, y * k * 1.2) - fbm((x - 9 * U) * k + s, (y - 6 * U) * k * 1.2);
  return .47 + rel * 3.2 - folB * .3;
}

function buildPoles() {
  poles = []; lamps = []; poleLength = 0;
  for (const [xt, top, xb, foot, w, kind] of POLES) {
    const s = { x1: px(xt), y1: py(top), x2: px(xb), y2: py(foot), w: Math.max(kind === 'flood' ? 2.5 * U : 1.1 * U, w * PW), kind };
    s.cx = (s.x1 + s.x2) / 2; s.cy = (s.y1 + s.y2) / 2; s.len = Math.hypot(s.x2 - s.x1, s.y2 - s.y1);
    poles.push(s); poleLength += s.len;
    if (kind === 'lamp') lamps.push({ x: s.x1, y: s.y1, r: 1.2 });
    else if (kind === 'flood') lamps.push({ x: s.x1 + U, y: s.y1, r: 2.4, flood: true });
  }
}
function buildMask() {
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows);
  limbWeight = limbLength = 0;
  for (const s of limbs) { s.wt = s.len * (s.w + 2 * U); s.at = limbWeight; limbWeight += s.wt; limbLength += s.len; mark(s, 1); }
  for (const s of poles) mark(s, 2);
}

// The ground is mapped once a size: which path or patch every few pixels belong to, and which way the path runs there.
let gmap = null, gdir = null, gCols = 0, gRows = 0, gtop = null, groundTop = 0;
const GCELL = 2;
const gAt = (x, y) => { const i = (x / GCELL) | 0, j = (y / GCELL) | 0; return i < 0 || j < 0 || i >= gCols || j >= gRows ? 0 : gmap[j * gCols + i]; };
const gDir = (x, y) => { const i = clamp((x / GCELL) | 0, 0, gCols - 1), j = clamp((y / GCELL) | 0, 0, gRows - 1); return gdir[j * gCols + i]; };
function buildGround() {
  gCols = Math.ceil(W / GCELL) + 1; gRows = Math.ceil(H / GCELL) + 1;
  gmap = new Uint8Array(gCols * gRows); gdir = new Float32Array(gCols * gRows);
  for (const [kind, poly] of PATCHES) {
    let x0 = 1e9, x1 = -1e9, y0 = 1e9, y1 = -1e9;
    for (const p of poly) { x0 = Math.min(x0, px(p[0])); x1 = Math.max(x1, px(p[0])); y0 = Math.min(y0, py(p[1])); y1 = Math.max(y1, py(p[1])); }
    for (let j = Math.max(0, Math.floor(y0 / GCELL)); j <= Math.min(gRows - 1, Math.ceil(y1 / GCELL)); j++)
      for (let i = Math.max(0, Math.floor(x0 / GCELL)); i <= Math.min(gCols - 1, Math.ceil(x1 / GCELL)); i++) {
        const xf = fx((i + .5) * GCELL), pf = fy((j + .5) * GCELL), e = (vnoise(xf * 300, pf * 300) - .5) * .004;
        if (inPoly(poly, xf + e, pf - e)) { gmap[j * gCols + i] = kind; gdir[j * gCols + i] = (vnoise(xf * 40, pf * 40) - .5) * 2; }
      }
  }
  for (const [kind, w, pts] of PATHS) for (let k = 0; k < pts.length - 1; k++) {
    const ax = px(pts[k][0]), ay = py(pts[k][1]), bx = px(pts[k + 1][0]), by = py(pts[k + 1][1]);
    const wa = w * PW * near(pts[k][1]) / 2, wb = w * PW * near(pts[k + 1][1]) / 2, m = Math.max(wa, wb) * 1.3 + GCELL, dx = bx - ax, dy = by - ay, L2 = dx * dx + dy * dy || 1, ang = Math.atan2(dy, dx);
    for (let j = Math.max(0, Math.floor((Math.min(ay, by) - m) / GCELL)); j <= Math.min(gRows - 1, Math.ceil((Math.max(ay, by) + m) / GCELL)); j++)
      for (let i = Math.max(0, Math.floor((Math.min(ax, bx) - m) / GCELL)); i <= Math.min(gCols - 1, Math.ceil((Math.max(ax, bx) + m) / GCELL)); i++) {
        const x = (i + .5) * GCELL, y = (j + .5) * GCELL, t = clamp(((x - ax) * dx + (y - ay) * dy) / L2);
        const d = Math.hypot(x - ax - dx * t, y - ay - dy * t), hw = (wa + (wb - wa) * t) * (kind === DIRT ? .75 + vnoise(x * .08 / U, y * .08 / U) * .5 : 1);
        if (d < Math.max(hw, .6 * U)) { gmap[j * gCols + i] = kind; gdir[j * gCols + i] = ang; }
      }
  }
  gtop = new Float32Array(Math.ceil(W) + 1); groundTop = H;
  for (let i = 0; i < gtop.length; i++) { gtop[i] = py(along2(GTOP, fx(i))); groundTop = Math.min(groundTop, gtop[i]); }
}
const groundAt = x => gtop[x < 0 ? 0 : x >= gtop.length ? gtop.length - 1 : x | 0];

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes may start there.
const SKY = 0, FAR = 1, BUILDING = 2, TOWER = 3, GROUND = 4, LEAVES = 5, TREE = 6, POLE = 7, CAGED = 8, BOX = 9;
let zx = 0, zy = 0, zf = -1, zb = null, zp = 0;               // what zone() learnt on the way: x and y on the photo, the leaves there, the building, the part of the tower
function zone(x, y, raw) {                                     // raw: as if the limbs and poles were not there
  const m = raw ? 0 : maskAt(x, y);
  if (m === 2) return POLE;
  const xf = zx = fx(x), pf = zy = fy(y);
  if (pf > .73 && xf > .8 && inPoly(CAGE, xf, pf)) return CAGED;
  if (xf >= CRATE[0] && xf < CRATE[1] && pf >= CRATE[2] && pf < CRATE[3]) return BOX;
  const f = zf = fol(x, y);
  if (m === 1 && (bare || f < .15)) return TREE;
  if (f > 0) return LEAVES;
  if (y >= groundAt(x)) return GROUND;
  if ((zp = wtPart(xf, pf))) return TOWER;
  if ((zb = bldAt(xf, pf))) return BUILDING;
  if (y >= stop[x < 0 ? 0 : x >= stop.length ? stop.length - 1 : x | 0]) return FAR;
  return SKY;
}
const reach = [0, 0];
function run(x, y, ca, sa, l, test) {                          // how far a stroke may go either way along (ca, sa) and stay where test() says
  const step = 2 * U;
  reach[0] = reach[1] = l;
  for (let d = step; d < l; d += step) if (!test(x + ca * d, y + sa * d)) { reach[1] = d - step; break; }
  for (let d = step; d < l; d += step) if (!test(x - ca * d, y - sa * d)) { reach[0] = d - step; break; }
}

// ---------- the town beyond ----------
let stop = null, landTop = 0;
const haze = f => clamp(f * .32 + traits.fog * (.25 + f * .5) + traits.rain * .06);
function farStroke() {
  const R = PP.R, x = R() * W, top = stop[x | 0], y = top + R() * .035 * PH;
  if (!at(x, y) || zone(x, y) !== FAR) return;
  const xf = zx, pf = zy, d = distAt(xf, pf), i = Math.floor(xf * 150), row = Math.floor((pf - HOR) / .0035);
  let col, x0, x1;
  if (d) {
    x0 = px(d[0]); x1 = px(d[1]);
    const fl = Math.floor((pf - d[2]) / .003);
    col = d[3] === 'mosaic' ? pick(pal.mosaic, hash1(fl * 7 + Math.floor((xf - d[0]) / .003)))
      : d[3] === 'glass' ? mix(mix(pal.glass, pal.skyMid, .35), pal.dark, fl & 1 ? .15 : 0)
      : d[3] === 'chimney' ? (fl % 4 < 1 && pf < d[2] + .01 ? pal.cabin : pal.hull)
      : d[3] === 'column' || d[3] === 'mast' ? mix(pal.towerLight, pal.tower, .2) : mix(pal.city[0], pal.dark, fl & 1 ? .2 : 0);
  } else {
    x0 = px(i / 150); x1 = px((i + 1) / 150);
    col = mix(pal.city[[0, 1, 5, 7, 4][(hash1(i * 7 + 1) * 5) | 0]], pal.dark, row & 1 ? .14 : 0);
  }
  col = mix(col, pal.cityFar, clamp(.62 + traits.fog * .3));
  const l = (2 + R() * 4) * U, xa = Math.max(x0, x - l), xb = Math.min(x1, x + l);
  if (xb - xa < .5) return;
  PP.brush(xa, y, xb, y, Math.min(2 * U, .003 * PH), col, { a: .9, jit: 5, dry: .04, bow: 0, sp: U });
  if (night && !d && R() < .05) light(x, y, .5 * U);
}

let bldWeight = 0;
function bldStroke() {
  const R = PP.R; let k = R() * bldWeight, b = BLD[0];
  for (const o of BLD) { b = o; k -= o.wt; if (k <= 0) break; }
  const x = b.X0 + R() * (b.X1 - b.X0), y = py(Math.min(b.t0, b.t1)) + R() * (b.foot - py(Math.min(b.t0, b.t1)));
  if (!at(x, y) || zone(x, y) !== BUILDING || zb !== b) return;
  const xf = zx, pf = zy, u = (xf - b.x0) / (b.x1 - b.x0), top = topOf(b, xf), podium = b.kind === 'checker' && pf >= PODIUM;
  const fh = podium ? (.52 - PODIUM) : b.fh, base = podium ? PODIUM : top, rel = (pf - base) / fh, fl = Math.floor(rel), vv = rel - fl;
  const shift = b.kind === 'checker' && !podium && (fl & 1) ? .5 : 0, uc = u * b.n + shift, ci = Math.floor(uc), uu = uc - ci;
  facade(b, fl, ci, uu, vv, u, pf);
  const cw = (b.x1 - b.x0) / b.n, l = (2 + R() * 6) * U, slope = (b.t1 - b.t0) / (b.x1 - b.x0) * PH / PW * (podium ? 0 : 1);
  let xa = Math.max(b.X0, x - l, px(b.x0 + (ci + fu0 - shift) * cw)), xb = Math.min(b.X1, x + l, px(b.x0 + (ci + fu1 - shift) * cw));
  const bandPx = (fv1 - fv0) * fh * PH, w = Math.max(.8 * U, Math.min(bandPx * .92, 2.6 * U));
  const yc = bandPx < 3.2 * U ? py(base + (fl + (fv0 + fv1) / 2) * fh) : clamp(y, py(base + (fl + fv0) * fh) + w / 2, py(base + (fl + fv1) * fh) - w / 2);
  run(x, yc, 1, slope, Math.max(x - xa, xb - x), (qx, qy) => zone(qx, qy) === BUILDING && zb === b);
  xa = Math.max(xa, x - reach[0]); xb = Math.min(xb, x + reach[1]);
  if (xb - xa < .4) xb = xa + .4;
  const lit = fLit;
  let col = mix(fc, pal.dark, b.shade * .28);
  if (!lit) col = mix(col, pal.cityFar, haze(b.far)); else col = mix(col, pal.cityFar, haze(b.far) * .3);
  const sd = shadow(x, y); if (sd && !lit) col = mix(col, pal.dark, sd * .18);
  PP.brush(xa, yc + (xa - x) * slope, xb, yc + (xb - x) * slope, w, col, { a: .93, jit: 5, dry: .03, bow: 0, sp: Math.min(1.1 * U, w) });
}

// ---------- the water tower ----------
let CH = 2, wtX0 = 0, wtX1 = 0, wtY0 = 0, wtY1 = 0;
function brickCol(row, x) {                                    // brown bricks, a few lighter, a few darker
  const h = hash1(row * 97 + Math.floor(x / (4 * U)) * 13), n = fbm(x * .05 / U, row * .7);
  let c = pal.brick[(h * pal.brick.length) | 0];
  if (n > .58) c = mix(c, pal.ledge, .18 + (n - .58) * 1.2);   // weathered lighter in patches
  return c;
}
function wtStroke() {
  const R = PP.R, x = wtX0 + R() * (wtX1 - wtX0), y = wtY0 + R() * (wtY1 - wtY0);
  if (!at(x, y) || zone(x, y) !== TOWER) return;
  const xf = zx, pf = zy, part = zp, o = { a: .93, jit: 5, dry: .04, bow: 0, sp: Math.min(1.1 * U, CH) };
  const far = haze(.1), shade = part === LEFTWALL || part === LEFTROOF;
  const tone = c => mix(shade ? mix(c, pal.brickShade, .38) : c, pal.cityFar, far);
  if (part === VANE) { PP.brush(x, py(WP[1] - .013), x, py(WP[1]), Math.max(.8 * U, .0009 * PW), pal.ink, o); if (R() < .3) PP.brush(x - 2.4 * U, py(WP[1] - .009), x + 2.4 * U, py(WP[1] - .0095), .9 * U, pal.ink, o); return; }
  if (part === CHIMNEY) {
    const top = pf < CHIM[2] + .0018, yc = Math.round(y / CH) * CH;
    PP.brush(px(CHIM[0]) + .3 * U, yc, px(CHIM[1]) - .3 * U, yc, CH * .9, tone(top ? (bare ? pal.snowy : pal.ledge) : brickCol(yc / CH | 0, x)), o);
    return;
  }
  if (part === LEFTROOF || part === FRONTROOF) {               // slate, laid in courses along the eaves; light on the hips, snow on it in winter
    const a = part === FRONTROOF ? Math.atan2(WR[1] - WF[1], (WR[0] - WF[0]) * PW / PH) : Math.atan2(WF[1] - WL[1], (WF[0] - WL[0]) * PW / PH);
    const ca = Math.cos(a), sa = Math.sin(a), n = fbm(x * .05 / U + 4, y * .2 / U);
    const hip = Math.abs(side(WF, WP, xf, pf)) / Math.hypot(WP[0] - WF[0], WP[1] - WF[1]) * PW;   // the hip between the two faces
    const eave = py(eaveY(xf)) - y;
    let col = part === FRONTROOF ? mix(pal.slate, pal.slateLight, .25 + n * .35) : mix(pal.slate, pal.dark, .2 + n * .2);
    if (eave < 1.6 * U) col = mix(pal.zinc, pal.slate, .3);
    else if (hip < 1.3 * U) col = mix(pal.zinc, pal.slateLight, .4);
    else if (bare && R() < .8) col = part === FRONTROOF ? pal.snowy : mix(pal.snowy, pal.dark, .2);
    const l = (2 + R() * 4) * U;
    run(x, y, ca, sa, l, (qx, qy) => zone(qx, qy) === TOWER && zp === part);
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], 1.5 * U, mix(col, pal.cityFar, far), o);
    return;
  }
  const front = part === FRONTWALL, x0 = front ? WF[0] : WL[0] + .0015, x1 = front ? WR[0] - .0015 : WF[0];
  const u = (xf - x0) / (x1 - x0), e = eaveY(xf), v = (pf - e) / WH, row = Math.floor((pf - e) * PH / CH);
  const f = wtFeature(front, u, v), vc = e + (row + .5) * CH / PH;
  const slope = (front ? (WR[1] - WF[1]) / (WR[0] - WF[0]) : (WF[1] - WL[1]) / (WF[0] - WL[0])) * PH / PW;
  // how far along the course the same thing goes
  const du = 1 / Math.max(1, (x1 - x0) * PW), lim = (2 + R() * 5) * U * du;
  let ua = u, ub = u;
  while (ua - du > 0 && u - ua < lim && wtFeature(front, ua - du, v) === f) ua -= du;
  while (ub + du < 1 && ub - u < lim && wtFeature(front, ub + du, v) === f) ub += du;
  const xa = px(x0 + ua * (x1 - x0)), xb = px(x0 + ub * (x1 - x0)), yc = py(vc);
  let col;
  if (f === BRICK) col = brickCol(row, x);
  else if (f === RECESS) col = mix(brickCol(row, x), pal.brickShade, .62);
  else if (f === STONE) col = bare && v < .5 && R() < .5 ? pal.snowy : mix(pal.ledge, pal.stone, R() * .3);
  else if (f === FRAME) col = pal.frame;
  else if (f === GLASS) col = mix(mix(pal.dark, pal.glass, .3), pal.skyMid, R() < .2 ? .3 : 0);
  else if (f === PIPE) col = pal.ink;
  else col = pal.brickShade;
  const sd = shadow(x, y); if (sd) col = mix(col, pal.brickShade, sd * .25);
  PP.brush(xa, yc + (xa - x) * slope, Math.max(xa + .5, xb), yc + (xb - x) * slope, CH * .9, tone(col), o);
}

// ---------- the ground ----------
function treeShade(x, y, sz) {                                 // the low sun throws the shade of the trees out to the right
  if (traits.shadow < .1 || night) return 0;
  return fol(x - 16 * sz, y - 9 * sz) > .05 ? .6 : 0;
}
function groundStroke() {
  const R = PP.R, x = R() * W, y = groundTop + R() * (H - groundTop);
  if (!at(x, y) || zone(x, y) !== GROUND) return;
  const pf = zy, sz = near(pf) * U, g = gAt(x, y), n = fbm(x * .015 / U + 3, y * .03 / U + 7), sd = shadow(x, y), ts = treeShade(x, y, sz);
  const dark = clamp(sd * .6 + ts), same = (qx, qy) => gAt(qx, qy) === g && zone(qx, qy, true) === GROUND;
  let col;
  if (g === 0) {                                               // the meadow: long grass, dry where the sun gets at it, mown near the town
    const dry = smooth(.5, .68, fbm(x * .006 / U + 11, y * .012 / U)), k = R();
    const m = fbm(x * .004 / U + 5, y * .009 / U + 1);
    col = mix(mix(pal.fieldB, pal.fieldC, smooth(.4, .65, m)), n > .5 ? pal.fieldA : pal.fieldShadow, Math.abs(n - .5) * 1.4);
    if (dry > 0) col = mix(col, pal.earth, dry * .5);
    if (k < .14) col = mix(col, pal.fieldLight, .45); else if (k > .86) col = mix(col, pal.fieldShadow, .45); else if (k > .82 && !bare) col = mix(col, pal.earth, .3);
    if (pf < .56) col = mix(col, pal.fieldA, .25);
    if (dark) col = mix(col, pal.fieldShadow, dark * .75);
    if (night) col = mix(col, pal.fieldShadow, .2);
    const a = -Math.PI / 2 + (R() - .5) * .9 + wind(time) * .06, l = (2 + R() * 5) * sz, ca = Math.cos(a), sa = Math.sin(a);
    run(x, y, ca, sa, l, same);
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (1.2 + R() * 2.2) * sz, col, { a: .85, jit: 9, dry: .2, sp: 1.1 * U, bow: .15 });
    return;
  }
  if (g === SAND || g === PLAY || g === EARTH) {
    col = g === PLAY ? mix(pal.tartan, pal.sand, n * .4) : g === SAND ? mix(pal.sand, pal.dirtDark, n * .35) : mix(pal.earth, pal.dirtDark, n * .5);
    if (dark) col = mix(col, pal.dirtDark, dark * .6);
    const a = R() * Math.PI, l = (1.5 + R() * 3) * sz, ca = Math.cos(a), sa = Math.sin(a);
    run(x, y, ca, sa, l, same);
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (1.2 + R() * 1.8) * sz, col, { a: .85, jit: 7, dry: .15, sp: U, bow: .05 });
    return;
  }
  const a = gDir(x, y), ca = Math.cos(a), sa = Math.sin(a), l = (3 + R() * 7) * sz;
  if (g === BOARD) {                                           // planks across it, dark between them
    col = mix(pal.board, pal.boardDark, n * .5 + (R() < .3 ? .4 : 0));
    if (dark) col = mix(col, pal.boardDark, dark * .5);
    if (R() < .3) {                                            // a joint between two planks, across
      const c2 = Math.cos(a + Math.PI / 2), s2 = Math.sin(a + Math.PI / 2), m = 3 * sz;
      run(x, y, c2, s2, m, same);
      PP.brush(x - c2 * reach[0], y - s2 * reach[0], x + c2 * reach[1], y + s2 * reach[1], .7 * U, pal.boardDark, { a: .8, jit: 4, dry: 0, sp: .7 * U, bow: 0 });
      return;
    }
  } else if (g === PAVE) {
    col = mix(pal.pave, pal.paveDark, n * .5);
    if (dark) col = mix(col, pal.paveDark, dark * .7);
  } else {
    col = mix(pal.dirt, pal.dirtDark, n * .6);
    if (dark) col = mix(col, pal.dirtDark, dark * .6);
  }
  run(x, y, ca, sa, l, same);
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (1.2 + R() * 1.6) * sz, col, { a: .88, jit: 6, dry: .1, sp: U, bow: .03 });
}

// ---------- the leaves ----------
function crownDab() {                                          // somewhere in a crown or a bush
  const R = PP.R; let k = R() * leafArea, c = leafy[0];
  if (!c) return;
  for (let a = 0, b = leafy.length - 1; a <= b;) { const m = (a + b) >> 1; if (leafy[m].at <= k) { c = leafy[m]; a = m + 1; } else b = m - 1; }
  const a = R() * TAU, d = Math.sqrt(R()) * c.r * 1.1;
  leaf(c.x + Math.cos(a) * d, c.y + Math.sin(a) * d);
}
function leaf(x, y) {
  const R = PP.R;
  if (!at(x, y)) return;
  const z = zone(x, y), d = zf, c = folC;
  if (!c || z === TREE || z === POLE || z === CAGED || z === BOX || d < -.08) return;
  const sz = (.45 + near(zy) * .5) * U, lum = clamp(folLum(x, y) + (c.type === 'L' || c.type === 'Y' ? .15 : c.type === 'D' ? -.12 : 0)), k = R();
  const pine = c.type === 'P', far = haze(clamp(.6 - zy) * .4);
  const tone = col => mix(col, pal.cityFar, far);
  if (d < .12) {                                               // the ragged rim: single leaves
    PP.dab(x, y, (2 + R() * 4) * sz, tone(pine ? pal.pineDark : bare && !c.tree ? pal.folDark : lum > .5 ? pal.folMid : pal.folDark), { a: .85, den: .3, s: 2.2 * U });
    return;
  }
  if (bare && !pine) {                                         // the bushes in winter: twigs, and the snow lying on them
    if (lum > .42 && k < .2 + lum * .6) { PP.dab(x, y, (3 + R() * 6) * sz, lum > .6 ? pal.folSun : pal.folLight, { a: .9, den: .6, core: .55, sx: 1.7, sy: .55 }); return; }
    const a = -Math.PI / 2 + (R() - .5) * 2, l = (6 + R() * 12) * sz;
    if (k > .6) PP.dab(x, y, (4 + R() * 5) * sz, mix(pal.folMid, pal.folLight, .25 + lum * .5), { a: .8, den: .5, core: .5, s: 2 * U });
    PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 3 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
      lum < .35 ? pal.folDeep : k < .8 ? pal.folDark : pal.folMid, (.9 + R()) * sz, { pass: 1, a: .8 });
    return;
  }
  let col;
  if (pine) {
    col = lum < .35 ? mix(pal.pineDeep, pal.pineDark, lum / .35) : mix(pal.pineDark, pal.pine, (lum - .35) / .65);
    if (lum > .6 && k < .35) col = bare ? pal.snowy : pal.pineLight;
  } else {
    col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .62 ? mix(pal.folDark, pal.folMid, (lum - .3) / .32) : mix(pal.folMid, pal.folLight, (lum - .62) / .5);
    if (c.type === 'S') col = mix(col, pal.fieldA, .12);
    if (lum > .68 && k < .15) col = mix(pal.folLight, pal.folSun, .5);
    else if (c.type === 'E' && (season === 'spring' || season === 'summer') && lum > .3 && k < .4) {   // the elder in flower: flat white umbels
      PP.dab(x, y, (2.2 + R() * 2.5) * sz, tone(mix(pal.blossom, pal.folLight, lum < .45 ? .3 : 0)), { a: .95, den: .9, core: .7, s: 1.4 * U });
      return;
    } else if (pal.accent && lum > .3 && k > (season === 'spring' ? .8 : .93)) col = pick(pal.accent, R());
  }
  const a = R() * Math.PI, l = (3 + R() * 6) * sz;
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (3.5 + R() * 5) * sz, tone(col), { a: .85, sp: 1.3 * U, dry: .2, jit: 10 });
}
function treeStroke() {                                        // a stretch of bark
  const R = PP.R, k = R() * limbWeight; let s = limbs[0];
  if (!s) return;
  for (let a = 0, b = limbs.length - 1; a <= b;) { const m = (a + b) >> 1; if (limbs[m].at <= k) { s = limbs[m]; a = m + 1; } else b = m - 1; }
  const span = Math.min(1, (6 + R() * 12) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span;
  const mx = limbX(s, tm), my = limbY(s, tm);
  if (!at(mx, my) || zone(mx, my) !== TREE) return;            // not where leaves hide it
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 2 ? .14 : .05), q = R();
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (bare) { if (s.w > 1.3 * U && q < .6) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snowy, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }
    else if (s.w > 2.5 * U && q < .5) PP.brush(ax - s.w * .22, ay, bx - s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .5), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });
  }
}

// ---------- poles, the cage, the container ----------
function poleStroke() {
  const R = PP.R; let k = R() * poleLength, s = poles[0];
  for (const t of poles) { s = t; k -= t.len; if (k <= 0) break; }
  const span = Math.min(1, (6 + R() * 14) * U / s.len), t0 = R() * (1 - span), t1 = t0 + span;
  const ax = s.x1 + (s.x2 - s.x1) * t0, ay = s.y1 + (s.y2 - s.y1) * t0, bx = s.x1 + (s.x2 - s.x1) * t1, by = s.y1 + (s.y2 - s.y1) * t1;
  if (!at((ax + bx) / 2, Math.min(H - 1, (ay + by) / 2))) return;
  const col = s.kind === 'flood' ? pal.concrete : s.kind === 'post' ? pal.post : pal.steelDark, o = { a: .95, jit: 6, dry: .04, sp: Math.min(1.1 * U, s.w), bow: 0 };
  PP.brush(ax, ay, bx, by, s.w, col, o);
  if (s.kind === 'flood' && R() < .6) PP.brush(ax + s.w * .25, ay, bx + s.w * .25, by, s.w * .35, mix(pal.concrete, pal.dark, .3), { a: .8, jit: 5, dry: .04, sp: Math.min(1.1 * U, s.w * .35), bow: 0 });
}
function lampDab() {
  const R = PP.R, l = pick(lamps, R());
  if (!l || !at(l.x, l.y)) return;
  if (!night) { PP.dab(l.x, l.y, l.r * U * .8, l.flood ? pal.concrete : pal.steelDark, { a: 1, den: 2.5, core: .9, s: U, jit: 4 }); return; }
  PP.dab(l.x, l.y + (l.flood ? 0 : 4 * U), (l.flood ? 16 : 8) * U, pal.lights[0], { a: .1, den: .4, s: 3 * U, jit: 3 });
  light(l.x, l.y, l.r * U);
}
let cageX0 = 0, cageY0 = 0;
function cageStroke() {                                        // the net over the court, the bars round it
  const R = PP.R, x = cageX0 + R() * (W - cageX0), y = cageY0 + R() * (H - cageY0);
  if (x < 0 || !at(x, y) || zone(x, y) !== CAGED) return;
  const xf = zx, pf = zy, sz = near(pf) * U, n = fbm(x * .02 / U + 1, y * .04 / U), o = { a: .88, jit: 6, dry: .1, sp: U, bow: 0 };
  const lit = night ? .35 : 0;
  if (inPoly(NET, xf, pf)) {
    const pp = ((xf - CORNER) / .036 % 1 + 1) % 1;
    if (pp < .05 && pf > .745) {                               // a post on the far side, under the net
      const l = (3 + R() * 6) * sz;
      run(x, y, 0, 1, l, (qx, qy) => zone(qx, qy) === CAGED && inPoly(NET, zx, zy));
      PP.brush(x, y - reach[0], x, y + reach[1], 1.1 * U, mix(pal.steel, pal.net, .3), { a: .8, jit: 4, dry: 0, sp: U, bow: 0 });
      return;
    }
    if (R() < .4) {                                            // the mesh, seen from above
      const a = (R() < .5 ? .55 : -.95), l = (4 + R() * 8) * sz, ca = Math.cos(a), sa = Math.sin(a), dk = R() < .5;
      run(x, y, ca, sa, l, (qx, qy) => zone(qx, qy) === CAGED && inPoly(NET, zx, zy));
      PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], .6 * U, mix(dk ? mix(pal.net, pal.dark, .35) : pal.netLight, pal.lights[0], lit), { a: .6, jit: 4, dry: 0, sp: .6 * U, bow: 0 });
      return;
    }
    const col = mix(mix(pal.net, pal.turf, .25 + n * .45), pal.lights[0], lit * .5), l = (3 + R() * 6) * sz;
    run(x, y, 1, .05, l, (qx, qy) => zone(qx, qy) === CAGED && inPoly(NET, zx, zy));
    PP.brush(x - reach[0], y - reach[0] * .05, x + reach[1], y + reach[1] * .05, (1.5 + R() * 2) * sz, bare ? mix(col, pal.snowy, .5) : col, o);
    return;
  }
  const pitch = .0075, ref = xf < CORNER ? xf - (pf - .733) * .089 / .267 : xf, m = ((ref - CORNER) / pitch % 1 + 1) % 1, k = Math.round((ref - CORNER) / pitch);
  const post = Math.abs(xf - CORNER) < .0018 || (Math.abs(m - .5) > .4 && k % 5 === 0);
  if (post || m < .22) {                                       // a bar, upright
    const l = (4 + R() * 8) * sz, w = (post ? 2.2 : 1) * U;
    run(x, y, 0, 1, l, (qx, qy) => zone(qx, qy, true) === CAGED && !inPoly(NET, zx, zy));
    PP.brush(x, y - reach[0], x, y + reach[1], w, mix(post ? pal.steel : mix(pal.steel, pal.court, .25), pal.lights[0], lit * .3), { a: .9, jit: 4, dry: 0, sp: Math.min(U, w), bow: 0 });
    return;
  }
  const col = mix(mix(pal.court, pal.steelDark, n * .2 + (xf < CORNER ? .12 : 0)), pal.lights[0], lit), l = (2 + R() * 4) * sz;
  run(x, y, 1, 0, l, (qx, qy) => zone(qx, qy, true) === CAGED && !inPoly(NET, zx, zy));
  PP.brush(x - reach[0], y, x + reach[1], y, (1.2 + R() * 1.6) * sz, bare ? mix(col, pal.snowy, .6) : col, o);
}
function crateStroke() {
  const R = PP.R, x = px(CRATE[0]) + R() * (CRATE[1] - CRATE[0]) * PW, y = py(CRATE[2]) + R() * (CRATE[3] - CRATE[2]) * PH;
  if (!at(x, y) || zone(x, y) !== BOX) return;
  const xf = zx, pf = zy, top = pf < CRATE[2] + .0045, rib = Math.floor((xf - CRATE[0]) / .0016);
  let col = top ? (bare ? pal.snowy : pal.crateTop) : mix(pal.crate, pal.dark, rib & 1 ? .2 : 0);
  if (!top && pf > CRATE[2] + .009 && pf < CRATE[3] - .006 && vnoise(xf * 900, pf * 700) > .52) col = pick(pal.graffiti, vnoise(xf * 300 + 7, pf * 300));
  const sd = treeShade(x, y, U); if (sd) col = mix(col, pal.dark, .3);
  if (top) PP.brush(x - 2 * U, y, x + 2 * U, y, 1.4 * U, col, { a: .92, jit: 4, dry: 0, bow: 0, sp: U });
  else PP.brush(x, Math.max(py(CRATE[2] + .0045), y - 2 * U), x, Math.min(py(CRATE[3]), y + 2 * U), 1.4 * U, col, { a: .92, jit: 4, dry: 0, bow: 0, sp: U });
}

// ---------- weather ----------
function fogStroke() {                                         // fog lies over the park in drifting banks
  const R = PP.R, x = R() * W, depth = R(), y = py(.5 + depth * .22);
  if (!at(x, y) || !traits.fog) return;
  const z = zone(x, y);
  if (z === POLE || z === TREE || z === BUILDING) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .2 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .45, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y = groundTop + R() * (H - groundTop);
  if (!at(x, y) || !traits.glint || zone(x, y) !== GROUND) return;
  PP.dab(x, y, (.6 + R() * .8) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x, y) {                                    // a broad stroke of a fresh coat
  const R = PP.R, o = { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .03 }, l = (36 + R() * 30) * U;
  const g = groundAt(x);
  if (y < g + 10 * U) {
    for (const b of BLD) if (x > b.X0 - 32 * U && x <= b.X1 + 32 * U && y + 24 * U > py(Math.min(b.t0, b.t1))) {   // every building that stands here: one upright stroke
      const w = Math.min(b.X1 - b.X0, 44 * U), cx = clamp(x, b.X0 + w / 2, b.X1 - w / 2), t = py(topOf(b, fx(cx)));
      const y0 = Math.max(t + Math.min(w, 8 * U) * .3, y - 24 * U), y1 = Math.min(groundAt(cx), y + 24 * U);
      if (y1 > y0) PP.brush(cx, y0, cx, y1, w, mix(mix(pal[b.wall], pal.dark, b.shade * .28 + .08), pal.cityFar, haze(b.far)), { a: .95, jit: 6, dry: 0, full: 1, sp: Math.min(2.2 * U, w), bow: 0 });
    }
    if (x > wtX0 - 30 * U && x < wtX1 + 30 * U && y > wtY0 - 30 * U && y < wtY1 + 30 * U) {
      const P = p => [px(p[0]), py(p[1])], so = { base: .95, hd: 0, wob: .3 * U };
      PP.shape([P([WL[0] + .0015, WL[1]]), P(WF), P([WF[0], WF[1] + WH]), P([WL[0] + .0015, WL[1] + WH])], mix(pal.brick[0], pal.brickShade, .4), so);
      PP.shape([P(WF), P([WR[0] - .0015, WR[1]]), P([WR[0] - .0015, WR[1] + WH]), P([WF[0], WF[1] + WH])], pal.brick[1], so);
      PP.shape([P(WL), P(WF), P(WP)], mix(pal.slate, pal.dark, .25), so);
      PP.shape([P(WF), P(WR), P(WP)], mix(pal.slate, pal.slateLight, .3), so);
    }
    if (y < g) {
      const top = stop[clamp(x | 0, 0, stop.length - 1)];
      if (y > top - 20 * U) PP.brush(x - l, top + .012 * PH, x + l, top + .012 * PH, .025 * PH, mix(pal.city[1], pal.cityFar, .55), o);
    }
  }
  const z = zone(x, Math.min(y, H - 1), true);
  if (y >= g - 6 * U && z !== CAGED && z !== BOX) {           // the ground first, under whatever stands on it
    const k = gAt(x, y);
    if (k === 0) PP.brush(x - l, y, x + l, y, (26 + R() * 10) * U, mix(pal.fieldB, pal.fieldC, .3), o);
    else PP.brush(x - l * .3, y, x + l * .3, y, 8 * U * near(zy), k === PAVE ? pal.pave : k === BOARD ? pal.board : k === PLAY ? pal.tartan : pal.dirt, o);
  }
  if (z === LEAVES) {
    const lum = clamp(folLum(x, y)), a = R() * Math.PI, fl = 14 * U, pine = folC && folC.type === 'P';
    PP.brush(x - Math.cos(a) * fl, y - Math.sin(a) * fl, x + Math.cos(a) * fl, y + Math.sin(a) * fl, (20 + R() * 12) * U,
      pine ? pal.pineDark : bare ? mix(pal.folMid, pal.folLight, lum * .6) : mix(pal.folDeep, lum > .5 ? pal.folMid : pal.folDark, clamp(lum * 1.3)), o);
  } else if (z === CAGED) PP.brush(x - l * .5, y, x + l * .5, y, 20 * U, inPoly(NET, zx, zy) ? pal.net : pal.court, o);
  else if (z === BOX) PP.brush(px(CRATE[0]), y, px(CRATE[1]), y, 6 * U, pal.crate, o);
}

// ---------- what moves ----------
// People walk up and down the promenade, now and then.
const WALK = PATHS[0][2];
function onWalk(u) {                                           // a point along the promenade, 0 at its top
  const n = WALK.length - 1, f = clamp(u) * n, i = Math.min(n - 1, Math.floor(f)), t = f - i;
  return [WALK[i][0] + (WALK[i + 1][0] - WALK[i][0]) * t, WALK[i][1] + (WALK[i + 1][1] - WALK[i][1]) * t];
}
const WALKERS = [[0, 70, 1, .3, 0], [23, 80, -1, -.2, 1], [41, 62, 1, .15, 2], [55, 90, -1, .05, 3]];   // [when, how long the walk takes, which way, which side, coat]
function drawWalkers(P, t) {
  for (const [t0, dur, dir, lane, k] of WALKERS) {
    const cyc = 100, u = ((t - t0) % cyc + cyc) % cyc / dur;
    if (u > 1) continue;
    const [xf, pf] = onWalk(dir > 0 ? u * .82 + .06 : .88 - u * .82), d = near(pf), x = px(xf + lane * PATHS[0][1] * d * .5), y = py(pf);
    if (x < 0 || x > W || y < 0 || y > H || !at(x, y) || zone(x, y) !== GROUND || gAt(x, y) !== PAVE) continue;
    const h = .03 * PH * d, step = Math.sin(t * 7 + k) * h * .16, col = pal.coats[k % pal.coats.length], o = { a: .95, sp: Math.max(.6 * U, h * .06), dry: 0, jit: 4, bow: 0 };
    if (!night && traits.shadow) P.brush(x, y, x + h * .5, y + h * .12, h * .14, pal.fieldShadow, { a: .25, sp: U, dry: 0, jit: 2, bow: 0 });   // the evening sun throws it out long
    P.brush(x, y - h * .45, x - step, y, h * .1, pal.ink, o);
    P.brush(x, y - h * .45, x + step, y, h * .1, pal.ink, o);
    P.brush(x, y - h * .85, x, y - h * .42, h * .2, col, o);
    P.dab(x, y - h * .95, h * .09, pal.skin, { a: 1, den: 2, core: .9, s: Math.max(.5 * U, h * .05), jit: 3 });
  }
}

// ---------- the painting, one frame's worth ----------
const RATE = { far: 9000, bld: 13000, wt: 16000, ground: 11000, fol: 9000, tree: 1.2, pole: 1.6, lamp: 5, cage: 9000, crate: 14000, glint: 90, fog: 1500 };
const place = {
  colors: {
    brick: ['#5a4034', '#4c372d', '#6a4c3c', '#43322a', '#73574a'], brickShade: '#261c18', ledge: '#b9ad99', frame: '#e8e4da', stone: '#8c8579',
    slate: '#4b4f57', slateLight: '#7a7f88', zinc: '#a3a7ab', snowy: '#eef2f4',
    cityWhite: '#efede7', cityCream: '#e4dccb', cityPink: '#e0a492', citySand: '#d8ccb2', cityRed: '#c4553d', cityGray: '#d2d2cc', cityDark: '#55575c',
    cityGreenGlass: '#8db8a0', cityYellow: '#dcc257', curtain: ['#c98e5a', '#7f93b8', '#d9d0b8', '#a8574a'],
    mosaic: ['#c8574a', '#4f86c6', '#e2b84a', '#6aa35c', '#ece6d8'],
    pave: '#d3d1c8', paveDark: '#a9a79f', board: '#7b746c', boardDark: '#4c4743', dirt: '#d0bd98', dirtDark: '#a38f6c', sand: '#dfcca3', tartan: '#c39960', earth: '#c4ad86',
    pine: '#3d6a45', pineDark: '#22402c', pineDeep: '#142a1d', pineLight: '#5f8a52', blossom: '#f4f1df',
    net: '#2b8573', netLight: '#8fd0bb', turf: '#5f9e82', court: '#c4a072', steel: '#d2d5d8', steelDark: '#7d848c',
    coats: ['#b8453a', '#2f3e5c', '#e2d6b8', '#4f7a5a'], skin: '#e0b89a',
    crate: '#2f55a6', crateTop: '#9aabcb', graffiti: ['#ffffff', '#d83a3a', '#7ec8f0', '#f2d03b'], concrete: '#c4c1b9', post: '#6d665d',
  },
  seasons: {
    winter: { pave: '#e8ecef', paveDark: '#c3ccd5', board: '#dde2e6', boardDark: '#959ca4', dirt: '#eef1f3', dirtDark: '#c9d2da', sand: '#eef1f3', tartan: '#e4e8ec', earth: '#e8ecef', crateTop: '#f2f4f5' },
    autumn: { blossom: '#4a2a3a' },
  },
  ground: /^(pave|board|dirt|sand|tartan|earth)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const r = W / H;
    if (r >= 4 / 3) {                                          // all of its width, and as much of its height as fits: the sky above the high tower stays
      PW = W; PH = PW * .75; X0 = 0;
      Y0 = clamp(.06, 0, Math.max(0, 1 - H / PH));            // a very wide one loses the near end of the park
    } else {                                                   // all of its height, a slice of its width, pushed together a bit
      PH = H; PW = PH * 4 / 3 * Math.max(.48, r * .75); Y0 = 0;
      X0 = clamp(.43 - W / PW / 2, 0, Math.max(0, 1 - W / PW));   // from the water tower to the high one
    }
    HZ = place.hz = py(HOR);
    CH = Math.max(1.6 * U, .0024 * PH);
    const n = Math.ceil(W) + 1;
    stop = new Float32Array(n);
    for (let i = 0; i < n; i++) { const xf = fx(i); let t = farTop(xf); for (const d of DIST) if (xf >= d[0] && xf < d[1]) t = Math.min(t, d[2]); stop[i] = py(t); }
    for (const b of BLD) {
      b.X0 = px(b.x0); b.X1 = px(b.x1); b.foot = py(along2(GTOP, (b.x0 + b.x1) / 2) + .02);
      b.wt = clamp((b.X1 - b.X0) * (b.foot - py(Math.min(b.t0, b.t1))) / (90 * U * U), 1, 400);
      for (let i = Math.max(0, Math.floor(b.X0)); i < Math.min(n, Math.ceil(b.X1)); i++) stop[i] = Math.min(stop[i], py(topOf(b, fx(i + .5))));
    }
    bldWeight = 0; for (const b of BLD) bldWeight += b.X1 > 0 && b.X0 < W ? b.wt : (b.wt = 0);
    landTop = H; for (let i = 0; i < n; i++) landTop = Math.min(landTop, stop[i]);
    landTop -= 8 * U;
    wtX0 = px(WL[0]); wtX1 = px(WR[0]); wtY0 = py(WP[1] - .014); wtY1 = py(WF[1] + WH);
    cageX0 = px(.8); cageY0 = py(.73);
    buildTrees(); buildPoles(); buildMask(); buildGround();
  },
  tick(t) { time = t; },
  top(x) { return stop[x < 0 ? 0 : x >= stop.length ? stop.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to the roofs
    const dip = 4 * U - pad, n = stop.length;
    c.moveTo(0, -60); c.lineTo(W, -60);
    let y = stop[n - 1] + dip;
    c.lineTo(W, y);
    for (let i = n - 2; i >= 0; i--) { const e = stop[i] + dip; if (Math.abs(e - y) > .3) { c.lineTo(i + 1, y); c.lineTo(i + 1, e); y = e; } }
    c.lineTo(0, y); c.closePath();
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6, gh = H - groundTop;
    clipBand(landTop, H);
    spend('fm-far', RATE.far * W * .03 * PH / ua * pace, farStroke);
    spend('fm-bld', RATE.bld * bldWeight * 90 * U * U / ua * pace, bldStroke);
    spend('fm-wt', RATE.wt * (wtX1 - wtX0) * (wtY1 - wtY0) / ua * pace, wtStroke);
    spend('fm-ground', RATE.ground * W * gh / ua * pace, groundStroke);
    spend('fm-fol', RATE.fol * leafArea * .75 / ua * pace, crownDab);
    spend('fm-tree', RATE.tree * limbLength / (14 * U) * pace, treeStroke);
    spend('fm-pole', RATE.pole * poleLength / (14 * U) * pace, poleStroke);
    spend('fm-lamp', RATE.lamp * pace, lampDab);
    if (cageX0 < W && cageY0 < H) spend('fm-cage', RATE.cage * (W - cageX0) * (H - cageY0) / ua * pace, cageStroke);
    spend('fm-crate', RATE.crate * (CRATE[1] - CRATE[0]) * PW * (CRATE[3] - CRATE[2]) * PH / ua * pace, crateStroke);
    if (s && s.traits.glint) spend('fm-glint', RATE.glint * W * gh / ua * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('fm-fog', RATE.fog * W * .25 * PH / ua * pace, fogStroke);
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) if (cells[k].y > landTop - 20 * U) coatStroke(cells[k].x, cells[k].y);
    c.restore();
  },
  sprites: [{ draw: drawWalkers, t0: 3, seed: 121 }],
};
return place;
} });
