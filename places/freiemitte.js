/* ------------------------------------------------------------------ *
 *  Vienna, over the Freie Mitte: the wild park where the yard of the
 *  Nordbahnhof used to be, seen from above on a May evening.  The old
 *  water tower stands on the left, the new towers of the
 *  Nordbahnviertel behind the park, the town beyond them.
 *
 *  It is painted after a photograph, measured by hand: the houses one
 *  by one with their roofs, their walls and their rows of windows, the
 *  town behind them made up; where the meadows lie open in the wood,
 *  the paths through it, the trees that stand up in front of the
 *  houses, and where in all that the sun lies and where the shade.
 *  Crown by crown the wood is made up.  The water tower is painted as
 *  it stands without the scaffolding the photograph has it in.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'freiemitte', name: 'Freie Mitte', make(kit) {
'use strict';
const { PP, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0, sway = 0;   // the view (CSS pixels, the stroke unit), the clock and how far the wind pushes the leaves
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
const SPLIT = .56;                                             // down to here the strokes are those of the town, small ones; from here on those of the park
const near = pf => clamp((pf - .3) / .7, .22, 1.3);           // how close the ground is at this height: the size of its strokes
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
function along(pts, x) {                                       // a line through points, from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 1; i < pts.length; i++) if (x <= pts[i][0]) { const a = pts[i - 1], b = pts[i]; return a[1] + (b[1] - a[1]) * (x - a[0]) / (b[0] - a[0]); }
  return pts[pts.length - 1][1];
}
function inPoly(p, x, y) {
  let o = false;
  for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o;
  return o;
}
const SKY = 0, FOL = 1, GRASS = 2, HARD = 3, EARTH = 4, WALL = 5, CREAM = 6, WARM = 7, TEAL = 8, YELLOW = 9, BLOSSOM = 10, TOWER = 11, BOX = 12, NET = 13, BARS = 14, POLE = 15;

// ---------- the town ----------
// A house is a front: [left, right, its roof at the left, at the right, what kind of front, its wall, how light
// that is (0 to 7), how high a floor, how wide a window with the wall beside it, how far off, the wall of its
// balconies if that is another].  From the back to the front: a house hides what stands behind it, down to the
// trees.  The kinds: windows in rows (GRID); balconies all along, the rooms in their shade (BANDS); the white
// tower, a box for every flat, staggered (BOXES); a frame with glass in it (GLAZED); windows, some with yellow
// shutters (SHUTTERS); a wall in the shade and not much to be seen on it (DIM).
const GRID = 0, BANDS = 1, BOXES = 2, GLAZED = 3, SHUTTERS = 4, DIM = 5;
const BLD = [
  [.4285, .431, .371, .371, DIM, WALL, 4, .01, .01, .5], [.511, .52, .368, .368, GRID, WALL, 5, .006, .0045, .5], [.561, .569, .373, .373, DIM, WALL, 3, .006, .007, .5],   // what stands out of the town far off
  [.572, .585, .376, .376, GRID, WALL, 5, .006, .0045, .5], [.7962, .7987, .352, .352, DIM, WALL, 5, .01, .01, .5], [.8183, .8205, .367, .367, DIM, WALL, 5, .01, .01, .5],
  [.852, .868, .366, .366, DIM, WALL, 3, .009, .008, .45],
  [0, .031, .378, .378, BANDS, WALL, 6, .0095, .011, .2], [.031, .087, .381, .381, GRID, WARM, 3, .0095, .0068, .2], [.087, .121, .385, .385, BANDS, WALL, 5, .0095, .011, .2],   // on the left, behind the water tower
  [.121, .248, .381, .381, GRID, CREAM, 6, .0092, .0064, .2], [.165, .24, .393, .393, BANDS, CREAM, 6, .0095, .011, .2, WALL], [.09, .125, .433, .433, GRID, CREAM, 5, .009, .007, .25],
  [.545, .587, .42, .42, DIM, WALL, 3, .009, .012, .3], [.61, .665, .44, .405, DIM, WALL, 1.2, .0095, .01, .15], [.558, .612, .44, .44, BANDS, CREAM, 6, .0095, .009, .12],   // between the towers
  [.52, .558, .44, .44, SHUTTERS, WALL, 7, .0125, .0095, .12], [.496, .52, .462, .462, DIM, WALL, 1.6, .0095, .012, .12],
  [.247, .331, .265, .257, BOXES, WALL, 7, .0099, .0105, .04], [.331, .4095, .257, .273, BOXES, WALL, 7, .0099, .0098, .04],   // the white tower: two sides of it
  [.408, .496, .469, .469, GLAZED, WARM, 6, .0125, .0135, .1],                                                                    // the red house
  [.925, .988, .345, .345, BANDS, WALL, 6, .0095, .012, .2], [.942, .976, .335, .335, BANDS, WALL, 6, .0095, .012, .2], [.925, .99, .412, .412, DIM, WALL, 3.2, .0095, .012, .2],   // on the right
  [.775, .832, .426, .426, DIM, WALL, 1.4, .0095, .012, .2], [.775, .89, .49, .482, DIM, WALL, 1.6, .02, .02, .15],
].map(([x0, x1, t0, t1, kind, wall, lit, fh, bw, far, trim], id) => ({ x0, x1, t0, t1, kind, wall, lit, fh, bw, far, trim, id }));
const topOf = (b, xf) => b.t0 + (b.t1 - b.t0) * (xf - b.x0) / (b.x1 - b.x0);
const PODIUM = .5;                                             // where the white tower stands on its shops
// The high tower leans in the photograph, as high things do from close by.  Its roof; how its left edge runs out as
// it comes down; its right edge; the corner between its side, in the shade, and its front; where the balconies of
// the front end and windows begin; the strip of wall to the right of them; its lobby.
const TALLTOP = [[.6669, .14], [.695, .117], [.769, .1105], [.783, .1235]], LEAN = .033, TALLR = .783, UPPER = .326, STRIP = .766, LOBBY = .533, TALLFOOT = .557;
const tallLeft = pf => TALLTOP[0][0] - LEAN * (pf - TALLTOP[0][1]), tallEdge = pf => .695 - (pf - .117) * .012;
// Behind the houses the town goes on to the horizon, row behind row, house by house, none of them measured: [where
// the roofs of a row are, how wide a house, how high a floor, how far off], from the back to the front.
const ROWS = [[HOR, .019, .0058, .5], [.396, .031, .0068, .3], [.411, .043, .0078, .2], [.431, .051, .0088, .12]];
const houses = new Map();
function house(r, xf) {                                        // the house of a row that stands at x
  const row = ROWS[r], i = Math.floor(xf / row[1] + r * .37), id = 100 + r * 4096 + i;
  let b = houses.get(id);
  if (!b) {
    const h = hash1(id * 13 + 5), t = row[0] + (h - .6) * (r ? .016 : .005), g = hash1(id * 7 + 1);
    houses.set(id, b = { x0: (i - r * .37) * row[1], x1: (i + 1 - r * .37) * row[1], t0: t, t1: t, kind: g < .62 ? GRID : BANDS, wall: hash1(id * 3 + 2) < .72 ? CREAM : WALL, lit: g > .88 && r ? 3 : 6 + h * 1.4,
      fh: row[2], bw: row[2] * (g < .62 ? .72 : 1.2), far: row[3], trim: WALL, id });
  }
  return b;
}
function roofAt(xf) {                                          // the skyline
  let t = Math.min(house(0, xf).t0, house(1, xf).t0);
  for (const b of BLD) if (xf >= b.x0 && xf < b.x1) t = Math.min(t, topOf(b, xf));
  if (xf < TALLR && xf >= tallLeft(TALLFOOT)) t = Math.min(t, xf < TALLTOP[0][0] ? TALLTOP[0][1] + (TALLTOP[0][0] - xf) / LEAN : along(TALLTOP, xf));
  return t;
}
// Where the fronts are not as light as the rest of them: [x, y, half as wide, half as high, how much darker].
const DULL = [[.742, .44, .02, .075, 2.6], [.074, .45, .013, .08, 1], [.853, .44, .01, .05, 2], [.39, .38, .02, .12, .6], [.68, .3, .012, .2, -.5]];
// A front is made of cells, CELL wide and ROW high: a stroke is a cell high and as long as the same thing goes on.
const ROW = .001736, CELL = .001302;
let lev = 0, zfar = 0, zwin = -1, cellJ = 0;                   // what zone() learns of a house on the way: how light (0 to 7), how far off, which window (a number of chance), which row of cells
function shade(l, xf, pf) {                                    // a wall is not the same all over
  for (const s of DULL) { const d = Math.hypot((xf - s[0]) / s[2], (pf - s[1]) / s[3]); if (d < 1.4) l -= s[4] * smooth(1.4, .6, d); }
  return l + (vnoise(xf * 170, pf * 250) - .5) * 1.1;
}
const set = l => { lev = clamp(Math.round(l), 0, 7); };
const chance = (a, b, c) => hash1(Math.imul(a, 73856093) ^ Math.imul(b, 19349663) ^ Math.imul(c, 83492791));   // a number of chance for a window: of which house, on which floor, which one along it
function front(b, xf, pf) {                                    // what there is at a point of a front
  let k = b.wall, l = b.lit;
  if (b.kind === BOXES && pf >= PODIUM) {                      // the shops: glass between piers
    const u = (xf - b.x0) / .011 % 1;
    if (pf < PODIUM + .004 || u < .3) set(5.4 + (pf < PODIUM + .004 ? 1 : 0)); else { zwin = hash1(Math.floor((xf - b.x0) / .011) * 37 + b.id) * .6; set(3); }
    return CREAM;
  }
  const f = (pf - topOf(b, xf)) / b.fh, fl = Math.floor(f), v = f - fl;
  const c = (xf - b.x0) / b.bw + (b.kind === BOXES && fl & 1 ? .5 : 0), ci = Math.floor(c), u = c - ci;
  const h = chance(b.id, fl, ci), g = chance(b.id + 500, fl, ci), win = chance(b.id + 900, fl, ci);
  switch (b.kind) {
    case GRID:
      if (u > .24 && u < .76 && v > .3 && v < .82) { if (h < .8) { zwin = win; set(g * 1.5); } else set(l - 1.6); return k === WARM ? WALL : k; }   // a blind is down in some
      if (v >= .82) l -= .5;
      break;
    case BANDS:
      if (v < .34) { if (b.trim) { k = b.trim; l = 6.4; } else l += .8; break; }
      if (u < .1) l -= 1;
      else { if (h < .6) zwin = win; set(h < .4 ? g * 1.5 : h < .6 ? 2 + g : l - 1.4); return k; }
      break;
    case BOXES:
      if (h > (xf > .386 ? .4 : .93) || v < .2 || v > .88 || u < .08 + g * .2 || u > .62 + win * .3) { if (xf < .2535) l -= 1.6; break; }   // no two boxes are alike; its far end has few; the edge on the left is in the shade
      if (v < .42 || u > .5 + win * .3) { set(3 + g * 1.6); return k; }   // the glass of a balcony
      zwin = win; set(g * 1.2);
      return k;
    case GLAZED:
      if (xf > .471) { l -= .7; break; }                       // its end wall
      if (v > .24 && u > .2) { zwin = win; set(g * 1.6); return WALL; }
      break;
    case SHUTTERS:
      if (u > .22 && u < .78 && v > .25 && v < .8) { if (h < .3) { set(3); return YELLOW; } zwin = win; set(g * 1.3); return k; }
      break;
    default:
      if (v < .22) l += 1.6; else if (h < .2) { zwin = win * .7; set(l - 1); return k; }
  }
  set(shade(l, xf, pf));
  return k;
}
function tallFront(xf, pf) {                                   // the high tower
  const edge = tallEdge(pf), f = (pf - along(TALLTOP, xf)) / .0102, fl = Math.floor(f), v = f - fl;
  if (xf < edge) {                                             // its side: narrow windows, floor on floor
    const c = (xf - tallLeft(pf)) / (edge - tallLeft(pf)) * 4, ci = Math.floor(c), u = c - ci;
    if (u > .3 && u < .7 && v > .25 && v < .8 && chance(1, fl, ci) < .8) { zwin = chance(2, fl, ci); set(1.2); } else set(shade(3.2, xf, pf));
    return WALL;
  }
  const c = (xf - edge) / .0073, ci = Math.floor(c), u = c - ci, h = chance(3, fl, ci), g = chance(4, fl, ci), win = chance(5, fl, ci);
  if (pf >= LOBBY) { if (c % 3 < .7) set(6); else { zwin = win * .5; set(1.2); } return WALL; }
  if (xf < STRIP && pf < UPPER) {                              // balconies all along: the slab, the glass in front, the rooms in the shade
    if (v < .26) { set(6.6); return WALL; }
    if (v < .6) { if (ci % 3 === 1 && h < .45) { set(4); return YELLOW; } if (h > .93) { set(2 + g); return TEAL; } set(4.3 + g * .8); return WALL; }
    if (h < .9) zwin = win;
    set(g * 1.4);
    return WALL;
  }
  if (u > .28 && u < .72 && v > .2 && v < .82 && h < (xf < STRIP ? .78 : .6)) { zwin = win; set(g * 1.2); return WALL; }
  set(shade(7, xf, pf));
  return WALL;
}
function town(xf, pf) {                                        // what there is at a point above the ground
  zwin = -1;
  if (pf < TALLFOOT && xf < TALLR && xf >= tallLeft(pf) && pf >= along(TALLTOP, xf)) { zfar = .04; return tallFront(xf, pf); }
  for (let i = BLD.length - 1; i >= 0; i--) {
    const b = BLD[i];
    if (xf < b.x0 || xf >= b.x1 || pf < topOf(b, xf) || (b.far > .4 && pf >= house(1, xf).t0)) continue;   // what stands far off stands behind the nearer rows
    zfar = b.far;
    return front(b, xf, pf);
  }
  for (let r = ROWS.length - 1; r >= 0; r--) { const b = house(r, xf); if (pf >= b.t0) { zfar = b.far; return front(b, xf, pf); } }
  return SKY;
}

// ---------- the park ----------
// Where the ground begins at the foot of the houses: [x, y].  From there on all is wood, but for what is measured
// here: the meadows, the paths, the bare earth.
const GTOP = [[0, .53], [.04, .527], [.1, .519], [.122, .517], [.21, .523], [.235, .52], [.41, .52], [.5, .525], [.64, .535], [.66, .555], [.78, .555], [.8, .52], [.9, .505], [1.1, .5]];
// The meadows, where the grass is open: [how light, its outline].  The first is the lawn at the foot of the water tower.
const MEADOWS = [
  [.62, [[.034, .529], [.1, .5185], [.21, .521], [.2115, .535], [.172, .5415], [.12, .5495], [.04, .5565], [.032, .547]]],
  [.68, [[.04, .556], [.2, .536], [.206, .556], [.2, .586], [.14, .6], [.08, .61], [.06, .6], [.05, .575]]],
  [.58, [[.065, .606], [.2, .59], [.213, .6], [.19, .625], [.165, .64], [.16, .665], [.14, .69], [.09, .715], [.06, .7], [.06, .65]]],
  [.62, [[.34, .525], [.5, .515], [.5, .53], [.47, .55], [.475, .575], [.5, .59], [.47, .6], [.43, .59], [.41, .6], [.4, .63], [.37, .61], [.345, .6], [.335, .57]]],
  [.52, [[.44, .58], [.5, .6], [.53, .6], [.5, .64], [.5, .69], [.46, .69], [.44, .68], [.42, .64], [.45, .62]]],
  [.5, [[.335, .7], [.5, .705], [.525, .72], [.53, .78], [.51, .8], [.52, .86], [.5, 1.1], [.335, 1.1], [.33, .9], [.345, .86], [.33, .84], [.33, .76]]],
  [.42, [[.5, .86], [.53, .84], [.56, .9], [.6, .93], [.66, .95], [.7, .9], [.72, .78], [.7, .72], [.725, .72], [.78, .86], [.85, 1], [.9, 1.1], [.5, 1.1]]],
  [.52, [[.585, .56], [.655, .555], [.668, .6], [.64, .615], [.6, .6]]],
  [.26, [[.7, .69], [.74, .685], [.8, .7], [.8, .735], [.765, .75], [.73, .735]]],
  [.5, [[.685, .555], [.74, .56], [.75, .6], [.72, .6], [.7, .585]]],
  [.45, [[-.1, .99], [0, .965], [.12, .93], [.17, .915], [.19, .93], [.19, 1.1], [-.1, 1.1]]],
];
// Bare earth and sand: [how light, its outline].
const BARE = [
  [.42, [[.33, .853], [.4125, .848], [.402, .922], [.344, .926]]], [.42, [[.05, .79], [.1, .775], [.13, .8], [.125, .87], [.09, .9], [.05, .88]]], [.42, [[.44, .82], [.5, .815], [.52, .86], [.51, .93], [.46, .93]]], [.36, [[.115, .772], [.16, .768], [.163, .79], [.12, .795]]],
  [.5, [[.735, .575], [.765, .58], [.79, .62], [.8, .655], [.775, .66], [.755, .64], [.74, .61]]],
];
// Paths: [what of, how wide close by, how light, how much of it is to be seen where it runs under the trees, the
// points it runs through].  Paved ones (the promenade first) and boardwalks of weathered wood, light in the sun
// and dark in the shade; trails worn into the meadow.
const PATHS = [
  [HARD, .046, .54, 1, [[.64, .535], [.65, .548], [.681, .596], [.707, .65], [.727, .675], [.75, .717], [.777, .767], [.826, .858], [.883, 1], [.93, 1.12]]],
  [HARD, .018, .42, 1, [[.745, .73], [.77, .742], [.8, .737]]], [HARD, .02, .52, .5, [[.797, .567], [.834, .604], [.859, .6375], [.8875, .667], [.9125, .6875], [.94, .7]]],
  [HARD, .02, .5, .45, [[.919, .654], [.944, .6875], [.9625, .708], [.98, .72]]], [HARD, .012, .5, .6, [[.734, .5625], [.75, .596], [.78, .625]]],
  [HARD, .014, .7, .5, [[.33, .528], [.42, .53], [.47, .531]]], [HARD, .018, .5, .45, [[-.03, .815], [0, .793], [.07, .725], [.1375, .66], [.15, .65]]],
  [HARD, .011, .7, .7, [[.035, .603], [.1, .592], [.196, .582], [.22, .578]]], [HARD, .01, .6, .7, [[.045, .556], [.12, .548]]], [HARD, .024, .3, .6, [[.347, .922], [.343, 1], [.34, 1.12]]],
  [HARD, .012, .4, .55, [[-.03, .975], [0, .963], [.18, .903], [.306, .866], [.361, .848], [.43, .825], [.526, .786], [.535, .783]]],
  [HARD, .01, .5, .4, [[.065, .651], [.134, .66], [.206, .667], [.29, .675], [.364, .681], [.512, .697]]], [HARD, .009, .45, .5, [[.352, .552], [.4, .567], [.47, .583], [.547, .6]]],
  [EARTH, .016, .42, .6, [[.402, .578], [.4125, .637], [.426, .674], [.43, .692], [.423, .715], [.433, .756], [.444, .802], [.457, .834], [.474, .88], [.485, .935], [.492, 1], [.5, 1.12]]],
  [EARTH, .01, .4, .5, [[.361, .582], [.365, .615], [.371, .706], [.368, .738], [.372, .793], [.378, .82]]], [EARTH, .009, .4, .5, [[.12, .73], [.16, .705], [.2, .69]]],
];
// The trees that stand up in front of the houses, crown by crown: [x, y, how big].  And the bushes out on the
// meadows: [x, y, half as wide, half as high].
const TREES = [[.001, .471, .012], [.001, .523, .034], [.119, .504, .012], [.154, .537, .034], [.179, .501, .008], [.187, .51, .012], [.213, .511, .017], [.245, .478, .008], [.258, .518, .034],
  [.295, .543, .034], [.329, .53, .008], [.498, .516, .008], [.518, .551, .024], [.555, .567, .034], [.594, .562, .017], [.613, .527, .008], [.623, .551, .024], [.646, .537, .008],
  [.673, .558, .008], [.679, .563, .008], [.747, .574, .008], [.774, .596, .034], [.792, .551, .008], [.807, .546, .017], [.829, .506, .008], [.844, .543, .034], [.876, .52, .008],
  [.882, .489, .008], [.894, .504, .017], [.906, .463, .012], [.912, .454, .017], [.924, .442, .008], [.936, .489, .034], [.945, .506, .034], [.962, .468, .017], [.973, .454, .008]];
const BUSHES = [[.405, .785, .035, .022], [.465, .775, .02, .02], [.41, .945, .035, .03], [.64, .88, .035, .06], [.585, .92, .02, .035], [.535, .875, .03, .03], [.675, .8, .014, .03],
  [.72, .775, .014, .03], [.755, .805, .014, .03], [.67, .87, .02, .04], [.355, .6, .012, .03], [.43, .615, .022, .028], [.39, .65, .012, .03], [.148, .575, .012, .02], [.123, .628, .012, .025],
  [.095, .59, .012, .015]];
const ELDER = [[.29, .772, .04, .024], [.655, .878, .028, .024], [.39, .672, .02, .012], [.56, .835, .012, .01]];   // those in flower: [x, y, half as wide, half as high]
const EVERGREEN = [[.425, .612, .014], [.5, .59, .012], [.535, .64, .014], [.012, .58, .02]];   // the pines keep their needles: [x, y, how far around]
const evergreen = (xf, pf) => { for (const e of EVERGREEN) if (Math.hypot(xf - e[0], (pf - e[1]) * .75) < e[2]) return true; return false; };
// Thin things that stand in front of it all: [x and y at the top, x and y at the foot, how thick, what of].  The mast
// of the floodlight over the cage, of steel; posts of wood.
const POLES = [[.987, .427, .97, .76, .007, 1], [.4775, .513, .478, .587, .0018, 0], [.488, .567, .4885, .627, .0018, 0], [.48, .643, .4805, .747, .002, 0]];
let zpole = null;
function poleAt(xf, pf) {
  for (const p of POLES) if (pf >= p[1] && pf < p[3] && Math.abs(xf - p[0] - (p[2] - p[0]) * (pf - p[1]) / (p[3] - p[1])) < Math.max(p[4], 1.2 * U / PW) / 2) return (zpole = p);
  return null;
}

// What the ground is, cell by cell, is worked out once a size: what lies there (gG) and how light (gL), how deep in
// the leaves of a tree or a bush that stands by itself a point is (gQ), and where in a crown, for its light (gV).
let gC = 2, gCols = 0, gRows = 0, gG = null, gL = null, gQ = null, gV = null;
const cell = (x, y) => clamp(y / gC | 0, 0, gRows - 1) * gCols + clamp(x / gC | 0, 0, gCols - 1);
function buildPark() {
  gC = Math.max(1.5, U); gCols = Math.ceil(W / gC) + 1; gRows = Math.ceil(H / gC) + 1;
  const n = gCols * gRows, deep = new Float32Array(n).fill(-1);
  gG = new Uint8Array(n); gL = new Uint8Array(n); gQ = new Float32Array(n).fill(-1); gV = new Int8Array(n);
  const box = (x0, y0, x1, y1, f) => {                          // every cell of a part of the photograph
    const i0 = Math.max(0, Math.floor(px(x0) / gC)), i1 = Math.min(gCols - 1, Math.ceil(px(x1) / gC)), j0 = Math.max(0, Math.floor(py(y0) / gC)), j1 = Math.min(gRows - 1, Math.ceil(py(y1) / gC));
    for (let j = j0; j <= j1; j++) for (let i = i0; i <= i1; i++) f(j * gCols + i, fx((i + .5) * gC), fy((j + .5) * gC));
  };
  const patch = (k, l, p, worn) => {                                 // no outline is as straight as it was measured
    let x0 = 9, x1 = -9, y0 = 9, y1 = -9;
    for (const q of p) { x0 = Math.min(x0, q[0]); x1 = Math.max(x1, q[0]); y0 = Math.min(y0, q[1]); y1 = Math.max(y1, q[1]); }
    box(x0 - .02, y0 - .02, x1 + .02, y1 + .02, (m, xf, pf) => {
      if (!gG[m] || (worn && fbm(xf * 70 + 9, pf * 110) < .31)) return;                           // weeds in the bare earth
      if (inPoly(p, xf + (fbm(xf * 40, pf * 55) - .47) * .04, pf + (fbm(xf * 40 + 31.7, pf * 55 + 17.3) - .47) * .03)) { gG[m] = k; gL[m] = l * 100; }
    });
  };
  const crown = (cx, cy, rx, ry, real, tint) => box(cx - rx, cy - ry, cx + rx, cy + ry, (m, xf, pf) => {
    const dx = (xf - cx) / rx, dy = (pf - cy) / ry, q = 1 - Math.hypot(dx, dy);
    if (q > deep[m]) { deep[m] = q; gV[m] = clamp((q - .45) * .9 - dy * .6 - dx * .25 + tint, -1, 1) * 100; }   // light on top and a little from the left, dark at its rim; and a tree is lighter than the next, or darker
    if (real && q > gQ[m]) gQ[m] = q;
  });
  box(-1, .49, 2, 2, (m, xf, pf) => { if (pf > along(GTOP, xf)) gG[m] = FOL; });
  for (const [l, p] of MEADOWS) patch(GRASS, l, p);
  for (const [l, p] of BARE) patch(EARTH, l, p, true);
  // the wood: crown by crown, the nearer the bigger, none of them measured
  for (let row = 0, pf = .5; pf < fy(H) + .06; row++) {
    const r = .004 + .036 * near(pf), step = r * 1.25;
    for (let i = Math.floor((fx(0) - r) / step); i <= (fx(W) + r) / step; i++) {
      const R = rng(row * 7919 + i * 31 + 5), s = r * (.7 + R() * .6);
      crown((i + R()) * step, pf + (R() - .5) * r, s, s * PW / PH * .9, false, (R() - .5) * .7);
    }
    pf += r * PW / PH * .62;
  }
  for (const t of TREES) crown(t[0], t[1], t[2], t[2] * PW / PH, true, .1);
  for (const b of BUSHES) crown(b[0], b[1], b[2], b[3], true, 0);
  const wood = gG.map((g, m) => g === FOL || gQ[m] > .15);
  for (const [k, w, l, open, pts] of PATHS) for (let s = 0; s < pts.length - 1; s++) {
    const a = pts[s], b = pts[s + 1], ax = px(a[0]), ay = py(a[1]), dx = px(b[0]) - ax, dy = py(b[1]) - ay, L2 = dx * dx + dy * dy || 1;
    const wa = w * PW * near(a[1]) / 2, wb = w * PW * near(b[1]) / 2, m = (Math.max(wa, wb) * 1.3 + gC) / PW;
    box(Math.min(a[0], b[0]) - m, Math.min(a[1], b[1]) - m * 1.4, Math.max(a[0], b[0]) + m, Math.max(a[1], b[1]) + m * 1.4, (c, xf, pf) => {
      const x = px(xf), y = py(pf), t = clamp(((x - ax) * dx + (y - ay) * dy) / L2);
      const hw = (wa + (wb - wa) * t) * (k === EARTH ? .7 + vnoise(x * .08 / U, y * .08 / U) * .6 : .9 + vnoise(x * .05 / U, y * .05 / U) * .2);
      if (!gG[c] || Math.hypot(x - ax - dx * t, y - ay - dy * t) >= Math.max(hw, .6 * U)) return;
      if (wood[c] && fbm(xf * 34 + 5, pf * 46) > .2 + open * .56) return;   // the trees hang over it
      gG[c] = k; gL[c] = l * 100; gQ[c] = -1;
    });
  }
}
// How light a thing is, from 0, the dark in among the leaves, to 1, the sun on it.  The trees on the left have the
// evening sun on them, the big ones on the right stand in their own shade; a crown is light on top.  On that lie
// patches of sun and of shade: [x, y, half as wide, half as high, how much lighter it is there].  The rest is
// leaves: clumps of them with the light on their upper left, and no leaf quite as light as the next.
const SPOTS = [[.2, .5, .22, .05, .1], [.09, .58, .08, .05, .12], [.14, .66, .08, .04, .1], [.52, .7, .03, .07, -.2], [.52, .82, .05, .04, -.22], [.6, .91, .03, .04, -.16], [.22, .82, .04, .05, -.12],
  [.33, .82, .06, .06, .1], [.27, .6, .04, .05, -.08], [.66, .74, .06, .08, .06], [.92, .47, .07, .04, .1], [.96, .66, .06, .08, -.08]];
const GLOOM = [[.5, .7, .035, .16, -.26], [.52, .88, .04, .08, -.2], [.72, .8, .03, .1, -.12], [.38, .56, .05, .03, .1]];   // and the same for the ground
const clumps = (xf, pf) => { const a = xf * 46 + 3, b = pf * 58, f = fbm(a, b); return (f - .47) * .6 + (f - fbm(a - .3, b - .34)) * .5; };
function lightAt(k, x, y, xf, pf) {                            // how light a thing (k) is at a point
  const n = cell(x, y);
  if (k === FOL) {
    let t = .32 - .18 * smooth(.42, .8, xf) + gV[n] * .0027;
    for (const p of SPOTS) { const d = Math.hypot((xf - p[0]) / p[2], (pf - p[1]) / p[3]); if (d < 1.5) t += p[4] * smooth(1.5, .5, d); }
    return t + (clumps(xf, pf) * 1.3 + (vnoise(xf * 300, pf * 400 + 9) - .5) * .3) * clamp(.3 + t * 1.4);   // in the shade they are all alike
  }
  let t = gL[n] / 100 + (k === GRASS ? .08 : 0);
  for (const p of GLOOM) { const d = Math.hypot((xf - p[0]) / p[2], (pf - p[1]) / p[3]); if (d < 1.5) t += p[4] * smooth(1.5, .5, d); }
  return t + (k === GRASS ? (fbm(xf * 14, pf * 22) - .47) * .5 + (vnoise(xf * 90, pf * 260) - .5) * .16 : (fbm(xf * 30, pf * 60) - .47) * .3);
}

// ---------- the water tower ----------
// Brown brick under a hipped roof of slate, seen from a little above and almost corner on, its left wall towards
// the evening sun.
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
// The football cage down on the right: walls of steel bars, a green net over the court.  The net; the wall along
// the promenade and the one towards us, which meet at CORNER.
const NETS = [[.805, .733], [.9, .737], [.915, .766], [1.01, .772], [1.01, .868], [.895, .862]], WALLS = [[.8, .733], [.895, .862], [1.01, .868], [1.01, 1.01], [.9, 1.01], [.86, .93], [.8, .8]], CORNER = .895;
const COURT = [[.835, .736], [.905, .745], [.93, .785], [1.01, .83], [1.01, .87], [.92, .86], [.88, .835], [.845, .79]];   // the court under the net, as far as it is to be seen

// ---------- whose a point is ----------
let zx = 0, zy = 0, zp = 0;                                   // what zone() learnt on the way: x and y on the photo, the part of the tower
function zone(x, y) {
  const xf = zx = fx(x), pf = zy = fy(y);
  if ((zp = towerPart(xf, pf))) return TOWER;
  if (xf >= CRATE[0] && xf < CRATE[1] && pf >= CRATE[2] && pf < CRATE[3]) return BOX;
  if (poleAt(xf, pf)) return POLE;
  const n = cell(x, y), q = gQ[n];
  const leafy = q > -1 && q * 1.3 + (vnoise(x * .13 / U, y * .13 / U) - .5) * .5 > 0;   // leaf by leaf at the rim of a crown
  if (xf > .8 && pf > .73 && !leafy) { if (inPoly(NETS, xf, pf)) return NET; if (inPoly(WALLS, xf, pf)) return BARS; }
  const g = gG[n];
  if (leafy || g === FOL) {
    zfar = pf > .5 ? 0 : .1;
    for (const e of ELDER) { const d = Math.hypot((xf - e[0]) / e[2], (pf - e[1]) / e[3]); if (d < 1 && vnoise(xf * 330, pf * 420) > .4 + d * .22) return BLOSSOM; }
    return FOL;
  }
  if (g) { zfar = pf > .5 ? 0 : .1; return g; }
  cellJ = Math.floor(pf / ROW);
  return town((Math.floor(xf / CELL) + .5) * CELL, (cellJ + .5) * ROW);
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

// ---------- the houses ----------
// They are painted in level strokes, a cell high: floor by floor, window by window.
let cw = 1, ch = 1;                                            // how big a cell of a front is in the picture
const hazed = (col, f) => { const h = f * .7 + traits.rain * f * .3; return h > 0 ? mix(col, pal.cityFar, clamp(h, 0, .85)) : col; };
// Fog is not a thing that stands somewhere: it is between us and everything, and the more of it the farther off a
// thing is.  Whoever paints says how far off the thing is (0 at our feet, 1 where the town ends), and the brush
// mixes the fog in.  The park runs away from us up to the houses; how far off those stand each of them says: the
// two towers in front of the rest.
const offAt = (pf, far) => pf > .5 ? .36 * clamp((1 - pf) / .5) : .38 + far * .9;
const TOWER_OFF = .34;                                          // the water tower stands at the near end of the houses
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;   // by night it is no lighter than the sky
const hazeOf = (d, k) => traits.fog ? [fogCol(), clamp(d * 1.4 - .1, 0, .85) * traits.fog * (k || 1)] : null;
const veil = d => { PP.haze = hazeOf(d); };
function wallCol(k, l, r) {
  const t = (l + r) / 8;
  if (k === WARM) return l < 4 ? ramp(pal.pinks, (l + r) / 4) : ramp(pal.reds, (l - 4 + r) / 4);
  return ramp(k === CREAM ? pal.creams : k === YELLOW ? pal.yellows : k === TEAL ? pal.teals : pal.walls, t);
}
function wallStroke(x, y, k) {
  const R = PP.R, pf = zy, l0 = lev, far = zfar, win = zwin, j = cellJ;
  // Stroke by stroke a cell high, in rows; on a big screen, where a cell is more than a stroke, the rows would show as steps
  const rows = ch < 1.7 * U, w = rows ? Math.max(1.1 * U, ch) : 1.5 * U, yc = rows ? py((j + .5) * ROW) : y;
  run(x, yc, 1, 0, (1.5 + R() * 3.5) * Math.max(U, cw * .7), q => q === k && lev === l0, Math.max(1, cw * .5));
  let col = wallCol(k, l0, .3 + R() * .4);
  // a window is dark by day; at night there is a light in some
  if (night && win >= 0 && win < (far < .1 ? .26 : .17)) { PP.haze = hazeOf(offAt(pf, far), .45); col = mix(pal.lights[(win * 977 % 1 * pal.lights.length) | 0], pal.dark, far * .5); }
  else { col = hazed(col, far); const sd = shadow(x, y); if (sd) col = mix(col, pal.dark, sd * .16); }
  PP.brush(x - reach[0], yc, x + reach[1], yc, w, col, { a: .96, jit: 3, dry: 0, bow: 0, sp: Math.min(1.1 * U, w) });
}

// ---------- the green ----------
function leafStroke(x, y, k) {
  const R = PP.R, xf = zx, pf = zy, sz = (.42 + near(pf) * .34) * U, q = R();
  const l0 = lightAt(FOL, x - sway, y, xf - sway / PW, pf);     // the wind moves the light about in them
  const sd = shadow(x, y), far = pf < .5 ? .1 : 0;
  if (bare && !evergreen(xf, pf)) {                            // winter: twigs, and the snow under and on them
    if (q < .3 + l0 * .5) { PP.dab(x, y, (2.5 + R() * 5) * sz, ramp(pal.meadow, l0 + R() * .12), { a: .9, den: .6, core: .5, sx: 1.6, sy: .6 }); return; }
    const a = -Math.PI / 2 + (R() - .5) * 2.6, l = (5 + R() * 10) * sz;
    PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 3 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
      hazed(l0 < .4 ? pal.folDeep : q < .8 ? pal.folDark : pal.folMid, far), (.8 + R() * .9) * sz, { pass: 1, a: .8 });
    return;
  }
  if (k === BLOSSOM && !bare && season !== 'autumn') {         // the elder in flower: flat white umbels
    PP.dab(x, y, (1.6 + R() * 2.2) * sz, mix(pal.blossom, pal.folLight, l0 < .4 ? .35 : q * .15), { a: .95, den: .9, core: .7, s: 1.3 * U });
    return;
  }
  const t = clamp(.5 + (l0 + (R() - .5) * .09 - .44) * 1.3);                                      // a bit harder than the camera saw it: deep shade, bright tops
  let col = t < .3 ? mix(pal.folDeep, pal.folDark, t / .3) : t < .58 ? mix(pal.folDark, pal.folMid, (t - .3) / .28) : t < .86 ? mix(pal.folMid, pal.folLight, (t - .58) / .28) : mix(pal.folLight, pal.folSun, (t - .86) / .14 * (pal.accent ? .5 : .8));
  if (bare) col = l0 > .68 && q < .5 ? pal.snowy : mix(col, pal.pine, .6);                        // a pine, snow on its boughs
  else if (pal.accent && l0 > .3 && q > (season === 'spring' ? .86 : .93)) col = pick(pal.accent, R());
  if (sd) col = mix(col, pal.folDeep, sd * .45);
  const a = R() * Math.PI, ca = Math.cos(a), sa = Math.sin(a), l = (2.5 + R() * 5) * sz;
  run(x, y, ca, sa, l, GREEN, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], room(x, y, -sa, ca, (3 + R() * 4.5) * sz), hazed(col, far), { a: .87, sp: 1.25 * U, dry: .18, jit: 9 });
}
function grassStroke(x, y) {                                   // long grass, every blade upright
  const R = PP.R, pf = zy, sz = (.25 + near(pf) * .6) * U, l0 = lightAt(GRASS, x, y, zx, pf), q = R(), sd = shadow(x, y);
  let col = ramp(pal.meadow, l0 + (R() - .5) * .12);
  if (!bare && l0 > .3) { const dry = smooth(.52, .7, fbm(x * .006 / U + 11, y * .012 / U)); if (dry > 0) col = mix(col, pal.straw, dry * .3); if (q > .92) col = mix(col, pal.straw, .4); else if (q < .1) col = mix(col, pal.meadow[1], .4); }
  if (sd) col = mix(col, pal.meadow[0], sd * .5);
  const a = -Math.PI / 2 + (R() - .5) * .8 + wind(time) * .07, ca = Math.cos(a), sa = Math.sin(a), l = (2 + R() * 4.5) * sz;
  run(x, y, ca, sa, l, GREEN, Math.max(1.5, 2 * sz));
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], room(x, y, -sa, ca, (1.3 + R() * 2.3) * sz), col, { a: .87, jit: 8, dry: .18, sp: 1.1 * U, bow: .12 });
}
const DIRS = []; for (let i = 0; i < 8; i++) DIRS.push([Math.cos(i * Math.PI / 8), Math.sin(i * Math.PI / 8)]);
function groundStroke(x, y, k) {                               // paths, boards, sand: every stroke the way the thing runs
  const R = PP.R, xf = zx, pf = zy, sz = near(pf) * U, l0 = lightAt(k, x, y, xf, pf), same = q => q === k, step = Math.max(1.5, 1.6 * sz), sd = shadow(x, y);
  let best = 0, bl = -1; const o = (R() * 8) | 0, far = (5 + R() * 9) * sz;
  for (let n = 0; n < 8; n++) { const dd = DIRS[(n + o) & 7]; run(x, y, dd[0], dd[1], far, same, step); if (reach[0] + reach[1] > bl) { bl = reach[0] + reach[1]; best = (n + o) & 7; } }
  const d = DIRS[best]; run(x, y, d[0], d[1], (3 + R() * 6) * sz, same, step);
  const list = k === EARTH ? pal.earths : pal.hards;
  let col = ramp(list, l0 + (R() - .5) * .12);
  if (sd) col = mix(col, list[0], sd * .3);
  if (pf < .5) col = hazed(col, .1);
  PP.brush(x - d[0] * reach[0], y - d[1] * reach[0], x + d[0] * reach[1], y + d[1] * reach[1], (1.3 + R() * 1.7) * sz, col, { a: .88, jit: 6, dry: .1, sp: Math.min(1.1 * U, 1.3 * sz + .3), bow: .03 });
}
function paintAt(x, y, k) {
  veil(offAt(zy, zfar));
  if (k === FOL || k === BLOSSOM) leafStroke(x, y, k);
  else if (k === GRASS) grassStroke(x, y);
  else if (k === HARD || k === EARTH) groundStroke(x, y, k);
  else if (k !== SKY) wallStroke(x, y, k);
}
let tops = null, landTop = 0, parkTop = 0;
function townStroke() {                                        // somewhere under the roofs, down to where the park's strokes begin
  const R = PP.R, x = R() * W, y = landTop + R() * (parkTop - landTop);
  if (y < tops[x | 0] - 2 * U || !at(x, y)) return;
  const k = zone(x, y);
  if (k < TOWER) paintAt(x, y, k);
}
function parkStroke() {
  const R = PP.R, x = R() * W, y = parkTop + R() * (H - parkTop);
  if (!at(x, y)) return;
  const k = zone(x, y);
  if (k < TOWER) paintAt(x, y, k);
}
let poleArea = 0;
function poleStroke() {                                        // a stretch of a post, from its top to its foot
  const R = PP.R, p = pick(POLES, R()), f = R(), x = px(p[0] + (p[2] - p[0]) * f), y = py(p[1] + (p[3] - p[1]) * f);
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y) || zone(x, y) !== POLE || zpole !== p) return;
  const w = Math.max(p[4] * PW, 1.2 * U), l = (2 + R() * 5) * U, dx = (p[2] - p[0]) * PW / ((p[3] - p[1]) * PH);
  veil(offAt(zy, .1));
  let col = p[5] ? mix(pal.steel[0], pal.steel[1], .55 + R() * .4) : mix(mix(pal.earths[2], pal.earths[4], R()), pal.hards[3], .4);
  if (night && p[5] && f < .5) col = mix(col, pal.lights[0], .2 * (1 - f * 2));
  PP.brush(x - dx * l, Math.max(py(p[1]), y - l), x + dx * l, Math.min(py(p[3]), y + l), w, col, { a: .93, jit: 3, dry: 0, bow: 0, sp: Math.min(U, w) });
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
  veil(TOWER_OFF);
  const far = pf > .5 ? 0 : .1, tone = c => hazed(lit ? mix(c, pal.brickSun, .3) : mix(c, pal.brickShade, .3), far);
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
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], 1.5 * U, hazed(col, far), o);
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
  if (f === GLASS && night) PP.haze = hazeOf(TOWER_OFF, .45);
  PP.brush(px(a[0]), py(a[1]), px(b[0]), py(b[1]), high / rows * .95, f === GLASS && night ? col : tone(col), o);
}
function crateStroke() {                                       // the container: blue, ribbed, a tag sprayed across it
  const R = PP.R, x = px(CRATE[0]) + R() * (CRATE[1] - CRATE[0]) * PW, y = py(CRATE[2]) + R() * (CRATE[3] - CRATE[2]) * PH;
  if (!at(x, y) || zone(x, y) !== BOX) return;
  veil(offAt(zy, 0));
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
  veil(offAt(zy, 0));
  const xf = zx, pf = zy, sz = near(pf) * U, t = .25 + vnoise(xf * 40, pf * 60) * .4 + R() * .12, k = inPoly(COURT, xf, pf) ? TEAL : 0, glow = night ? .14 : 0, same = q => q === z;
  if (z === NET) {                                             // the net sags between its ropes; the court shows through it
    if (R() < .3) {                                            // its mesh
      const a = R() < .5 ? .5 : -1, ca = Math.cos(a), sa = Math.sin(a);
      run(x, y, ca, sa, (4 + R() * 8) * sz, same, 2 * sz);
      PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], .7 * U, mix(ramp(pal.teals, t + .3), pal.lights[0], glow), { a: .5, jit: 4, dry: 0, sp: .7 * U, bow: 0 });
      return;
    }
    const col = k === TEAL ? ramp(pal.teals, t * .8 + .15) : mix(ramp(pal.earths, t), pal.teals[2], .45);
    run(x, y, 1, .06, (3 + R() * 6) * sz, same, 2 * sz);
    PP.brush(x - reach[0], y - reach[0] * .06, x + reach[1], y + reach[1] * .06, (1.5 + R() * 2) * sz, bare && t > .5 ? mix(col, pal.snowy, .5) : mix(col, pal.lights[0], glow), { a: .88, jit: 6, dry: .1, sp: U, bow: 0 });
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
  PP.haze = null;                                              // a light is seen through any fog
  PP.dab(x, y + (f ? 6 : 3) * U, (f ? 16 : 6) * U * near(l[1]) * 1.4, pal.lights[0], { a: .1, den: .4, s: 3 * U, jit: 3 });
  light(x, y, (f ? 2.2 : 1.1) * U);
}
function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y = parkTop + R() * (H - parkTop);
  if (!at(x, y) || !traits.glint || zone(x, y) !== GRASS || lightAt(GRASS, x, y, zx, zy) < .5) return;
  PP.haze = null;
  PP.dab(x, y, (.6 + R() * .8) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x0, y0) {                                  // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R;
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U) continue;
    const k = zone(clamp(x, 0, W - 1), Math.min(y, H - 1)), xf = zx, pf = zy, l = (14 + R() * 8) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
    if (k === SKY || k === POLE) continue;
    veil(k === TOWER ? TOWER_OFF : offAt(pf, zfar));
    if (k === TOWER) { PP.brush(x, y - 10 * U, x, y + 10 * U, 14 * U, zp >= LEFTROOF ? pal.slate : mix(pal.brick[0], zp === LEFTWALL ? pal.brickSun : pal.brickShade, .25), o); continue; }
    if (k === BOX) { PP.brush(x - 4 * U, y, x + 4 * U, y, 6 * U, pal.crate, o); continue; }
    if (k === NET || k === BARS) { PP.brush(x - l, y, x + l, y, 22 * U, k === NET ? pal.teals[2] : mix(pal.earths[2], pal.steel[0], .5), o); continue; }
    const far = zfar, t = k === GRASS ? .5 : k === HARD || k === EARTH ? Math.max(.42, gL[cell(x, y)] / 100) : 0;
    const col = k === FOL || k === BLOSSOM ? (bare && !evergreen(xf, pf) ? mix(pal.meadow[1], pal.folDark, .3) : mix(pal.folDark, pal.folMid, .45)) : k === GRASS ? ramp(pal.meadow, t)
      : k === HARD ? ramp(pal.hards, t) : k === EARTH ? ramp(pal.earths, t) : wallCol(k, Math.max(lev, 3), .5);
    if (k === FOL || k === BLOSSOM) { const a = (R() - .5) * 1.2; PP.brush(x - Math.cos(a) * l * .9, y - Math.sin(a) * l * .9, x + Math.cos(a) * l * .9, y + Math.sin(a) * l * .9, 26 * U, col, o); }
    else if (k === GRASS || k === HARD || k === EARTH) PP.brush(x - l, y, x + l, y, 22 * U, hazed(col, far), o);
    else PP.brush(x, y - l * .8, x, y + l * .8, 24 * U, hazed(col, far), o);      // a house: an upright stroke
  }
}

// ---------- what moves ----------
// People stroll along the promenade, now and then: come out by the high tower and leave the picture at its lower edge,
// or the other way.  From up here a walker is a coat, a head and a long shadow, and bobs a little.
const WALK = [[.565, .662], [.58, .675], [.6, .689], [.62, .7045], [.64, .7145], [.68, .7285], [.7, .735], [.72, .743], [.74, .757], [.76, .772], [.78, .78], [.8, .784], [.82, .794],
  [.84, .8025], [.86, .81], [.88, .8215], [.92, .8405], [.96, .86], [1, .88], [1.04, .9]];   // the middle of the promenade: how far down the photo, how far across
const WALK0 = WALK[0][0], WALK1 = WALK[WALK.length - 1][0];
function walkX(pf) { for (let i = 1; i < WALK.length; i++) if (pf <= WALK[i][0]) { const a = WALK[i - 1], b = WALK[i]; return a[1] + (b[1] - a[1]) * (pf - a[0]) / (b[0] - a[0]); } return WALK[WALK.length - 1][1]; }
const WALKERS = [[0, 97, 52, 1, -.45, 0], [31, 113, 60, -1, .5, 1], [58, 131, 48, 1, -.3, 2], [12, 89, 56, -1, .35, 3]];   // [when, how often, how long the walk takes, which way, which side, coat]
function drawWalkers(P, t) {
  for (const [t0, every, dur, dir, lane, k] of WALKERS) {
    let u = ((t - t0) % every + every) % every / dur;
    if (u >= 1) continue;
    if (dir < 0) u = 1 - u;
    const s = .5579 * (Math.exp(1.027 * u) - 1);               // far off a step is a small one: they get faster as they get nearer
    const pf = WALK0 + (WALK1 - WALK0) * clamp(s), h = .03 * PH * near(pf), x = px(walkX(pf) + lane * .006) + Math.sin(t * 2.7 + k * 2) * h * .02, y0 = py(pf);
    if (x < -h || x > W + h || y0 < 0 || y0 > H + h || !at(clamp(x, 0, W - 1), Math.min(y0, H - 1))) continue;
    P.haze = hazeOf(offAt(pf, 0));
    const al = smooth(0, .07, s), y = y0 - Math.abs(Math.sin(t * 5.4 + k * 2)) * h * .035;   // they come out of the house at the far end; at the near one they walk out of the picture
    const o = { a: .96 * al, sp: Math.max(.6 * U, h * .06), dry: 0, jit: 3, bow: 0 };
    P.R = rng(7100 + k);                                       // each keeps its own shape, whoever else is about
    if (!night && traits.shadow) P.brush(x + h * .06, y0, x + h * .85, y0 - h * .14, h * .14, pal.dark, { a: .42 * al, sp: U, dry: 0, jit: 2, bow: 0 });   // the evening sun throws it out long
    P.brush(x, y - h * .4, x, y, h * .16, pal.ink, { a: .96 * al, sp: h * .16, dry: 0, jit: 3, bow: 0 });   // no legs to speak of from here: a dark stroke
    for (let n = 0; n < 2; n++) P.brush(x, y - h * .84, x, y - h * .36, h * .25, pal.coats[k % pal.coats.length], o);   // twice over: a coat is not see-through
    P.dab(x, y - h * .96, h * .1, pal.skin, { a: al, den: 2, core: .9 * al, s: Math.max(.5 * U, h * .05), jit: 3 });
  }
  P.haze = null;
}

// ---------- the painting, one frame's worth ----------
const RATE = { town: 19000, park: 13000, tower: 17000, pole: 17000, lamp: 6, crate: 14000, cage: 10000, glint: 90 };
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
    coats: ['#b8453a', '#2c4f7c', '#e6dcc0', '#3f7a58'], skin: '#e0b89a',
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
    cw = PW * CELL; ch = PH * ROW; CH = Math.max(1.5 * U, .0023 * PH);
    const n = Math.ceil(W) + 1;
    tops = new Float32Array(n); landTop = H;
    for (let i = 0; i < n; i++) { tops[i] = py(roofAt(fx(i))); if (tops[i] < landTop) landTop = tops[i]; }
    landTop -= 6 * U;
    parkTop = py(SPLIT);
    poleArea = 0; for (const p of POLES) poleArea += Math.max(p[4] * PW, 1.2 * U) * (p[3] - p[1]) * PH;
    buildPark();
    cageX = Math.max(0, px(.8)); cageY = py(.73);
    wtX0 = px(WL[0]); wtX1 = px(WR[0]); wtY0 = py(WP[1] - .012); wtY1 = py(.536);
  },
  tick(t) { time = t; sway = wind(t) * 1.5 * U; },
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
    spend('fm-town', RATE.town * W * (parkTop - landTop) / ua * pace, townStroke);
    spend('fm-park', RATE.park * W * (H - parkTop) / ua * pace, parkStroke);
    spend('fm-pole', RATE.pole * poleArea / ua * pace, poleStroke);
    spend('fm-tower', RATE.tower * (wtX1 - wtX0) * (wtY1 - wtY0) / ua * pace, towerStroke);
    if (cageX < W && cageY < H) spend('fm-cage', RATE.cage * (W - cageX) * (H - cageY) / ua * pace, cageStroke);
    spend('fm-crate', RATE.crate * (CRATE[1] - CRATE[0]) * PW * (CRATE[3] - CRATE[2]) * PH / ua * pace, crateStroke);
    if ((s && s.night) || (o && o.night)) spend('fm-lamp', RATE.lamp * pace, lampDab);
    if (s && s.traits.glint) spend('fm-glint', RATE.glint * W * (H - parkTop) / ua * pace, glintDab);
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
  sprites: [{ draw: drawWalkers, t0: 3, seed: 121 }],
};
return place;
} });
