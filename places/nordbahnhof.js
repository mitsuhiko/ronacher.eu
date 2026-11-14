/* ------------------------------------------------------------------ *
 *  Vienna, at the Nordbahnhof: the old water tower of the northern
 *  railway on its embankment, ivy on the wall in front of it, and the
 *  gravel path that runs past below through what has grown wild where
 *  the yard used to be.  scene.js paints the sky above it and says what
 *  a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'nordbahnhof', name: 'Nordbahnhof', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, ease, mix, pick, rng, hash1, vnoise, fbm, wind, light, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let HZ = 0, PW = 1, PH = 1, CH = 3;         // horizon; the photo's width and height in pixels; how high a course of bricks is
let pal = null, traits = null, night = false, season = '', snow = false;   // snow: whether any lies.  Winter alone is bare, not white
const use = s => { pal = s.pal; traits = s.traits; night = s.night; season = s.season; snow = traits.snow > 0; return s; };
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph the picture is painted after: x and y as fractions
// of its width and height.  It is an upright one: a narrow screen shows a slice of its width,
// a wide one a slice of its height, and what lies to the left and right of it as well.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along2(pts, x) {                                      // a line through points, eased from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 0; i < pts.length - 1; i++) if (x <= pts[i + 1][0]) { const a = pts[i], b = pts[i + 1]; return a[1] + (b[1] - a[1]) * ease((x - a[0]) / (b[0] - a[0])); }
  return pts[pts.length - 1][1];
}
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;

// The path runs to a point on the horizon, close by on the left.  A line to that point is known
// by how fast it leaves it: the edges of the path, the tracks worn into it.
const HOR = .565, VPX = .078, VPY = .556;
const RIGHT = [[.56, 2.1], [.6, 2], [.65, 1.87], [.71, 1.92], [.79, 1.82], [.86, 1.79], [.94, 1.98], [.975, 2.2], [1.05, 2.4]];
const pathR = pf => along2(RIGHT, pf) + (vnoise(pf * 60, 4.4) - .5) * .08, pathL = pf => -3.5 + (vnoise(pf * 24, 8.1) - .5) * .5;
const farTop = xf => .478 + (vnoise(xf * 7, 3.3) - .5) * .04 + (vnoise(xf * 40, 9.1) - .5) * .014;   // the trees at the far end
// The bushes under the poplars and the ivy on the embankment: where they begin, and where the grass below them does.
const TOPS = [[.07, .56], [.09, .535], [.12, .505], [.2, .492], [.3, .495], [.36, .488], [.4, .475], [.45, .445], [.5, .43], [.53, .4], [.56, .378], [.6, .37],
  [.65, .366], [.7, .372], [.75, .36], [.8, .346], [.85, .335], [.9, .32], [.95, .306], [1, .3], [1.3, .288], [2, .292]];
const FEET = [[.07, .56], [.1, .566], [.2, .6], [.3, .612], [.36, .6], [.42, .615], [.455, .575], [.5, .55], [.6, .52], [.75, .49], [1, .46], [2, .45]];
const bushTop = xf => along2(TOPS, xf), footAt = xf => along2(FEET, xf) + (vnoise(xf * 60, 2.2) - .5) * .02;
const solidTop = xf => Math.min(farTop(xf), bushTop(xf) + .014);
// How far off a thing is: 0 at our feet, 1 where the path ends.  In fog everything pales by that: whoever paints
// says how far off the thing is, and the brush mixes the fog in.  On level ground it is a matter of how close to
// the horizon a point lies.  What is up on the embankment runs to where the path ends as well, but higher above
// the ground and farther to the side, so the same step in the picture is a shorter way.  Its slope is both: level
// ground down at the path (u = 1), the embankment up under the ivy.
const offAt = (xf, pf) => Math.hypot((xf - VPX) * .75, pf - VPY);
const flatOff = pf => .03 / (.03 + Math.max(0, pf - VPY));
const farOff = (xf, pf) => .1 / (.1 + offAt(xf, pf));
const slopeOff = (xf, pf, u) => { const r = offAt(xf, pf), c = .1 + (clamp(.03 * r / (pf - VPY), .03, .1) - .1) * u; return c / (c + r); };
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;   // by night it is no lighter than the sky
const veil = d => { PP.haze = traits.fog ? [fogCol(), clamp(d * 1.4 - .1, 0, .85) * traits.fog] : null; };

// ---------- the tower ----------
// Whatever is level on it runs towards one of two points on the horizon: on its left side to the
// one the path runs to, on its front to one far off on the right.  Such a line is known by the
// height at which it meets the corner between the two.
const VLX = .075, VRX = 3.8, XL = .328, XC = .454, XR = .893;  // the far end of the left side, the corner, the far end of the front
const reachOf = xf => xf < XC ? (xf - VLX) / (XC - VLX) : (VRX - xf) / (VRX - XC);
const levelOf = (xf, pf) => HOR + (pf - HOR) / reachOf(xf);    // which line a point lies on
const levelY = (c, xf) => HOR + (c - HOR) * reachOf(xf);       // how high that line is at x
const towerOff = xf => xf < XC ? .3 + clamp((XC - xf) / (XC - XL)) * .16 : .3 + (xf - XC) * .04;   // its left side runs away from us
// The roof is seen from below: the tip of the eaves on the left, their near corner, the far end of the gutter.
const EAVE = [.299, .259], PEAK = [.429, .071], GUT = [.915, .15], CHIM = [.658, .679, .078];
const roofY = xf => xf < PEAK[0] ? EAVE[1] + (PEAK[1] - EAVE[1]) * (xf - EAVE[0]) / (PEAK[0] - EAVE[0]) : PEAK[1] + (GUT[1] - PEAK[1]) * (xf - PEAK[0]) / (GUT[0] - PEAK[0]);
const WALL = .1, FOOT = .49, TIP = .264, END = .166;           // levels: the top of the walls, their foot; how far down the eaves reach beyond them
const ROOF = 1, LEFT = 2, FRONT = 3, CHIMNEY = 4;
function towerPart(xf, pf) {
  if (xf < EAVE[0] || xf > GUT[0]) return 0;
  if (pf < roofY(xf)) return xf >= CHIM[0] && xf < CHIM[1] && pf >= CHIM[2] ? CHIMNEY : 0;
  if (xf < XL) return pf < TIP ? ROOF : 0;
  if (xf >= XR) return pf < END ? ROOF : 0;
  const c = levelOf(xf, pf);
  return c < WALL ? ROOF : c > FOOT ? 0 : xf < XC ? LEFT : FRONT;
}
function towerPath(c) {
  c.moveTo(px(EAVE[0]), py(EAVE[1])); c.lineTo(px(PEAK[0]), py(PEAK[1]));
  c.lineTo(px(CHIM[0]), py(roofY(CHIM[0]))); c.lineTo(px(CHIM[0]), py(CHIM[2])); c.lineTo(px(CHIM[1]), py(CHIM[2])); c.lineTo(px(CHIM[1]), py(roofY(CHIM[1])));
  c.lineTo(px(GUT[0]), py(GUT[1])); c.lineTo(px(GUT[0]), py(END)); c.lineTo(px(XR), py(END));
  c.lineTo(px(XR), py(levelY(FOOT, XR))); c.lineTo(px(XC), py(FOOT)); c.lineTo(px(XL), py(levelY(FOOT, XL)));
  c.lineTo(px(XL), py(TIP)); c.lineTo(px(EAVE[0]), py(TIP)); c.closePath();
}

// What its walls are made of.  Both sides have the same two floors, the left one a cellar below them.
// The windows of the upper floor: [x, half its width, what is in it: 0 bricks, 1 boards, 2 a shutter over boards].
const NICHES = [[.3453, .0028, 0], [.356, .0031, 0], [.368, .0042, 0], [.395, .0048, 0], [.4117, .0052, 1], [.4275, .0058, 0],
  [.5395, .0185, 0], [.5925, .0185, 2], [.646, .018, 0], [.74, .0175, 0], [.787, .017, 1], [.833, .0165, 0]];
// The lower floor: sunk panels [from, to], in each an arch [x, half its width, half the width of the opening under it]
// with a round stone on either side.
const PANELS = [[.3395, .378], [.385, .44], [.508, .68], [.705, .885]];
const ARCHES = [[.3615, .014, .0065], [.4175, .0213, .01], [.5925, .0655, .0345], [.781, .062, .0325]];
const ROUNDS = [[.3456, .3738], [.3938, .435], [.525, .659], [.73, .84]];
const CELLAR = [[.36, .368], [.41, .4275]], PIERS = [.341, .3875, .4015];
// levels: the heads of the windows and how high their arches are, their sills; the ledge between the floors, the
// corbels under it; the top of the panels, of the arches in them, the bench the arches stand on
const HEAD = .147, ARC = .014, SILL = .2075, LEDGE = .2314, CORBEL = .238, TEETH0 = .247, TEETH1 = .262, PANEL = .2745, RING = .2865, BENCH = .349;
const SHADE = 1, BRICK = 2, RED = 3, STONE = 4, TEETH = 5, EDGE = 6, OPEN = 7, BOARD = 8, SHUTTER = 9, ARCH = 10, RIM = 11, KEY = 12, ROUND = 13, PIPE = 14;
let wx = 0, wc = 0, wr = 0, wlit = false;                      // what wallAt() learnt: the middle of an arch or a round stone, its size; whether the window is the one with a light in it
function wallAt(xf, c) {
  const front = xf >= XC;
  if (front && c > .118 && c < .352 && Math.abs(xf - .697) < .0027) return PIPE;
  if (c < .107) return SHADE;                                  // under the eaves
  if (c < .124) { const q = (c - .107) / .0085; return q - Math.floor(q) > .7 ? SHADE : BRICK; }   // courses stepping out towards them
  if (c < LEDGE) {
    for (const n of NICHES) {
      const hw = n[1], q = Math.abs(xf - n[0]) / hw;
      if (q > 1.5) continue;
      wx = n[0]; wc = HEAD + ARC; wlit = n[2] === 2;
      if (q < 1 && c > HEAD + ARC * (1 - Math.sqrt(1 - q * q))) {
        if (c >= SILL) return c < SILL + .0045 ? STONE : vnoise(xf * 300, c * 200) > .42 ? RED : BRICK;
        if (front && (c < HEAD + ARC * (1 - Math.sqrt(1 - q * q)) + .004 || (q > .8 && xf < n[0]))) return SHADE;   // it is sunk into the wall
        return n[2] === 0 ? RED : n[2] === 2 && c < HEAD + ARC * 1.3 ? SHUTTER : BOARD;
      }
      if (q < .3 && c > HEAD - .019 && c < HEAD + .003) return KEY;
      if (q < 1.42 && c < HEAD + ARC && c > HEAD - .008 + ARC * 1.4 * (1 - Math.sqrt(1 - q * q / 2.02))) return ARCH;   // the arch over it
      if (q < 1.3 && c >= SILL && c < SILL + .0045) return STONE;
      break;
    }
    if (front && c > .129 && c < .1335 && ((xf > .505 && xf < .675) || (xf > .715 && xf < .885))) return SHADE;
    return BRICK;
  }
  if (c < CORBEL) return STONE;
  if (c < TEETH0) return BRICK;
  if (c < TEETH1) return TEETH;
  if (c < TEETH1 + .005) return SHADE;
  if (c < PANEL) return BRICK;
  let i = -1; for (let k = 0; k < 4; k++) if (xf >= PANELS[k][0] && xf < PANELS[k][1]) i = k;
  if (i >= 0 && c < BENCH) {
    const p = PANELS[i], a = ARCHES[i], dx = xf - a[0], dc = BENCH - c;
    wx = a[0]; wc = BENCH; wlit = false;
    if (Math.hypot(dx / a[2], dc / .033) < 1) return !front && dx > 0 && dc < .013 ? BOARD : OPEN;
    if (Math.abs(dx) < a[1] * .11 && c > RING - .005 && c < RING + .016) return KEY;
    const ro = Math.hypot(dx / a[1], dc / .0625);
    if (ro < 1) return ro > .84 ? RIM : ARCH;
    for (const r of ROUNDS[i]) if (Math.hypot((xf - r) / (a[1] * .11), (c - .292) / .0075) < 1) { wx = r; wc = .292; wr = a[1] * .11; return ROUND; }
    if (c < PANEL + .0045 || xf < p[0] + (p[1] - p[0]) * .022) return SHADE;
    return front ? BRICK : RED;
  }
  if (i >= 0 && c < BENCH + .0175) return c < BENCH + .008 || c > BENCH + .0115 ? STONE : SHADE;   // the bench, and a second one right under it
  if (!front) {
    for (const w of CELLAR) if (xf > w[0] && xf < w[1]) {
      const q = (xf - (w[0] + w[1]) / 2) / ((w[1] - w[0]) / 2);
      wlit = false;
      if (c > .394 + .012 * (1 - Math.sqrt(1 - q * q)) && c < .437) return q > -.1 && c > .403 ? BOARD : OPEN;
    }
    for (const p of PIERS) if (Math.abs(xf - p) < .0012 && c > BENCH) return EDGE;
  }
  return BRICK;
}

// ---------- what grows: bushes and ivy, the trees, their limbs ----------
// The trees: [x, foot, top, how wide it spreads, how thin its crown is, whether it stands in front of the tower].
// Three poplars by the path, the tree of heaven beside the tower with its two stems.
const TREES = [[.105, .562, .385, .62, 0, 0], [.185, .566, .318, .55, 0, 0], [.262, .572, .335, .5, 0, 0], [.872, .35, .075, .85, .3, 1], [.91, .34, .03, 1.05, .3, 1]];
function treeList() {                                          // and what stands beyond the photograph, as far as one can see
  const list = TREES.slice();
  for (let i = 1, x = -.14; x > X0 - .25; i++, x -= .27 + hash1(i * 13) * .16) list.push([x, .56, .455 + hash1(i * 7) * .03, .9, .04, 0]);
  for (let i = 1, x = 1.14; x < fx(W) + .25; i++, x += .2 + hash1(i * 29) * .14) list.push([x, .31, .1 + hash1(i * 11) * .09, .95, .06, 1]);
  return list.filter(t => t[0] > X0 - .3 && t[0] < fx(W) + .3);
}
// Leaves are a heap of crowns: those of the bushes and the ivy lie over a solid mass, every tree adds a few of its
// own.  The poplars stand behind the tower, everything else in front of it.
let crowns = [], leafy = [], leafArea = 0, stripsF = [], stripsB = [], topF = [], topB = [], folTop = 0, folB = 0, folTree = false, folOff = 0;
const STRIP = 28;                                              // the crowns are sorted into upright strips, so that a point need not ask all of them
// Thin things other strokes have to keep clear of: a coarse mask says where the limbs are (1 behind the tower, 2 in front of it).
let limbs = [], limbWeight = 0, limbLength = 0, mask = null, mCols = 0, mRows = 0, mCell = 4, frontTop = 0;
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function buildTrees() {
  crowns = []; limbs = [];
  let r = rng(5);
  for (let xf = .075, x1 = fx(W) + .04; xf < x1;) {
    const rad = (.011 + r() * .015) * PH, top = py(bushTop(xf)), k = r(), a = r(), b = r(), c = r(), d = r();
    if (xf > X0 - .04) {
      crowns.push({ x: px(xf), y: top + rad * (.9 + k * .5), r: rad, tree: false, back: false, thin: 0 });
      if (a < .5) crowns.push({ x: px(xf) + (b - .5) * rad, y: top + rad * (2 + c), r: rad * (.8 + d * .5), tree: false, back: false, thin: 0 });
    }
    xf += rad / PW * (.7 + r() * .5);
  }
  let off = 0;                                                 // how far off the tree is that is being grown
  const add = (x1, y1, x2, y2, bend, w, depth, tip, front) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, tip, front, k: r(), len, off });
  };
  const grow = (x, y, a, len, w, depth, rad, t) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len, last = depth >= 5 || len <= 4 * U;
    add(x, y, x2, y2, (r() - .5) * .3, w, depth, last, t[5]);
    crowns.push({ x: x2, y: y2, r: rad * (depth < 3 ? .8 + r() * .5 : depth < 4 ? .7 : .5), tree: true, back: !t[5], thin: t[4], off });
    if (last) return;
    const k = r() < .75 ? 2 : 3;
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .62 + (r() - .5) * .45, len * (.58 + r() * .2), Math.max(.7 * U, w * .6), depth + 1, rad, t);
  };
  for (const t of treeList()) {
    r = rng(31 + Math.round(t[0] * 1000));                     // every tree keeps its shape, whatever else is to be seen
    off = t[5] ? farOff(t[0], t[1]) : clamp(.13 / (t[1] - t[2]));   // those down by the path: the smaller, the farther
    const x = px(t[0]), y = py(t[1]), h = (t[1] - t[2]) * PH, w = Math.max(1.5 * U, h * .024), rad = h * (t[4] > .1 ? .075 : .13);
    add(x, y, x + (r() - .5) * h * .1, y - h * .4, (r() - .5) * .08, w, 0, false, t[5]);
    const trunk = limbs[limbs.length - 1];
    for (let i = 0; i < 5; i++) {
      const a = -Math.PI / 2 + ((i + .5) / 5 - .5) * 2.2 * t[3] + (r() - .5) * .2, at = i === 2 ? 1 : .65 + r() * .35;
      grow(limbX(trunk, at), limbY(trunk, at), a, h * (.27 - Math.abs(a + Math.PI / 2) * .05) * (.85 + r() * .3), w * .55, 1, rad, t);
    }
  }
  folTop = H; leafArea = 0; stripsF = []; stripsB = []; topF = []; topB = []; leafy = crowns.filter(c => c.tree);
  for (const c of crowns) {
    folTop = Math.min(folTop, c.y - c.r);
    if (c.tree) { c.at = leafArea; leafArea += c.r * c.r; }
    const S = c.back ? stripsB : stripsF, T = c.back ? topB : topF;
    for (let i = Math.max(0, Math.floor((c.x - c.r - 4 * U) / STRIP) + 8), n = Math.floor((c.x + c.r + 4 * U) / STRIP) + 8; i <= n; i++) {
      (S[i] = S[i] || []).push(c);
      T[i] = Math.min(T[i] === undefined ? 1e9 : T[i], c.y - c.r - 8 * U);
    }
  }
  folTop -= 10 * U;
  // the mask
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows);
  frontTop = folTop; limbWeight = limbLength = 0;
  for (const s of limbs) {
    s.wt = s.len * (s.w + 2 * U); limbWeight += s.wt; limbLength += s.len;
    const n = Math.ceil(s.len / (mCell * .5)), rad = s.w / 2 + .3 * U, m = Math.ceil(rad / mCell), v = s.front ? 2 : 1;
    for (let i = 0; i <= n; i++) {
      const x = limbX(s, i / n), y = limbY(s, i / n), ix = Math.floor(x / mCell), iy = Math.floor(y / mCell);
      if (!s.front && towerPart(fx(x), fy(y))) continue;       // the tower hides it
      for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
        const cx = ix + dx, cy = iy + dy;
        if (cx < 0 || cy < 0 || cx >= mCols || cy >= mRows || Math.hypot(dx, dy) * mCell > rad + mCell * .5) continue;
        if (mask[cy * mCols + cx] < v) mask[cy * mCols + cx] = v;
      }
      if (y - rad < frontTop) frontTop = y - rad;
    }
  }
}
function fol(x, y, back) {                                     // > 0 inside the leaves: of what stands in front of the tower, or behind it
  if (y < folTop) return -1;
  const si = Math.floor(x / STRIP) + 8, list = (back ? stripsB : stripsF)[si];
  let d = -1, tree = false;
  if (back) { if (!list || y < topB[si]) return -1; }
  else { d = Math.min(.6, (y - py(bushTop(fx(x))) - .012 * PH) / (.02 * PH)); if (d < -1 && (!list || y < topF[si])) return -1; }
  const sw = wind(time) * 2 * U, bare = traits.bare, fall = season === 'autumn' ? .22 : 0;
  folB = 0;
  if (list) for (const c of list) {
    const dx = x - c.x - sw; if (dx > c.r || dx < -c.r || (bare && c.tree)) continue;   // a bare tree is its limbs and nothing else
    const dy = y - c.y, q = 1 - Math.hypot(dx, dy * 1.1) / c.r - (c.tree ? c.thin + fall : 0);   // in autumn half the leaves are down
    if (q > d) { d = q; folB = dy / c.r; tree = c.tree; folOff = c.off; }
  }
  folTree = tree;
  return d * 1.2 + (fbm(x * .03 / U + time * .02, y * .03 / U) - .47) * .6;
}
function folLum(x, y) {                                        // light falls in from the upper right, from behind
  const s = time * .03, k = .02 / U;
  const rel = fbm(x * k + s, y * k * 1.2) - fbm((x + 9 * U) * k + s, (y - 9 * U) * k * 1.2);
  return .45 + rel * 3.2 - folB * .25;
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that
// thing's strokes may start there.
const SKY = 0, FAR = 1, TOWER = 2, LEAVES = 3, TREE = 4, SLOPE = 5, PATH = 6, MEADOW = 7;
let zx = 0, zy = 0, zw = 0, zf = 0, zp = 0;                    // what zone() learnt on the way: the point on the photo, its line to the end of the path, the leaves there, the part of the tower
function zone(x, y, raw) {                                     // raw: as if the limbs were not there
  const xf = zx = fx(x), pf = zy = fy(y);
  if (pf > VPY) {
    const w = zw = (xf - VPX) / (pf - VPY);
    if (w < pathR(pf)) return w > pathL(pf) ? PATH : MEADOW;
  }
  if (xf > .08 && pf > footAt(xf)) return SLOPE;
  const m = raw ? 0 : maskAt(x, y);
  let f = zf = fol(x, y, false);
  if (m === 2 && (traits.bare || f < .15)) return TREE;
  if (f > 0) return LEAVES;
  if ((zp = towerPart(xf, pf))) return TOWER;
  const b = fol(x, y, true);
  if (b > f) f = zf = b;
  if (m === 1 && (traits.bare || f < .15)) return TREE;
  if (f > 0) return LEAVES;
  return pf > farTop(xf) ? FAR : SKY;
}

// ---------- the tower, brick by brick ----------
let TX0 = 0, TX1 = 0, TY0 = 0, TY1 = 0;                        // the box it stands in
function tone(col, front, x, y) {                              // its left side lies in the shade
  if (!front) col = mix(col, pal.brickShade, .4);
  const sd = shadow(x, y);
  return sd ? mix(col, pal.brickShade, sd * .28) : col;
}
function towerStroke() {
  const R = PP.R, x = TX0 + R() * (TX1 - TX0), y = TY0 + R() * (TY1 - TY0);
  if (!at(x, y) || zone(x, y) !== TOWER) return;
  const xf = zx, pf = zy, o = { a: .93, jit: 5, dry: .04, bow: 0, sp: 1.1 * U };
  veil(towerOff(xf));
  if (zp === ROOF) { roofStroke(x, y, xf, pf, o); return; }
  if (zp === CHIMNEY) {                                        // its pots, and the snow on them
    const top = pf < CHIM[2] + .009, yc = Math.round(y / CH) * CH;
    PP.brush(px(CHIM[0]) + U, yc, px(CHIM[1]) - U, yc, CH * .85, tone(top ? (snow ? pal.snow : pal.pot) : pal.brick[(hash1(yc * 7 | 0) * 5) | 0], true, x, y), o);
    return;
  }
  const front = zp === FRONT, c = levelOf(xf, pf), m = wallAt(xf, c), k = reachOf(xf), x0 = front ? XC : XL, x1 = front ? XR : XC;
  const d = front ? VRX - XC : XC - VLX, sg = front ? 1 : -1, a = (xf - XC) * sg / d / (1 - (xf - XC) * sg / d);   // how far along the wall
  const xAt = a => clamp(XC + sg * d * a / (1 + a), x0, x1);
  const fit = (xe, c) => wallAt(xe, c) === m ? xe : wallAt((xe + xf) / 2, c) === m ? (xe + xf) / 2 : xf;   // not beyond what it is made of here
  const dark = mix(pal.brick[4], pal.brickShade, .5);
  if (m === BRICK || m === RED) {                              // every stroke is a brick in its course
    const i = Math.floor(c * PH / CH), cc = (i + .5) * CH / PH, bl = 2.7 * CH / PW / d, j = Math.floor(a / bl + (i & 1) * .5), a0 = (j - (i & 1) * .5) * bl;
    if (wallAt(xf, cc) !== m) return;
    const xa = fit(xAt(Math.max(0, a0)), cc), xb = fit(xAt(a0 + bl), cc), wd = Math.max(1.1 * U, CH * k * .84), list = m === RED ? pal.brickRed : pal.brick;
    let col = mix(list[(hash1(i * 131 + j * 17) * list.length) | 0], m === RED ? pal.brick[1] : pal.brick[7], m === RED ? .25 : .3);
    col = mix(col, pal.brick[4], smooth(.5, .78, fbm(xf * 26 + 3, c * 34)) * .4);        // soot
    if (m === RED && !front) col = mix(col, pal.brick[2], .3); else if (front && xf < XC + .013) col = mix(col, pal.brick[5], .25);
    if (Math.abs(xb - xa) * PW < 1.2 * U) return;
    o.sp = Math.min(1.1 * U, wd);
    PP.brush(px(xa) + sg * .35 * U, py(levelY(cc, xa)), px(xb) - sg * .35 * U, py(levelY(cc, xb)), wd, tone(col, front, x, y), o);
  } else if (m === TEETH) {                                    // the corbels: pale teeth, the dark between them
    const tl = 1.7 * CH / PW / d, j = Math.floor(a / tl), am = (j + .5) * tl, xm = XC + sg * d * am / (1 + am);
    if (xm < x0 || xm > x1) return;
    const wd = Math.max(1.1 * U, tl * d * PW / ((1 + am) * (1 + am)) * .92), ya = py(levelY(TEETH0, xm)), yb = py(levelY(TEETH1, xm));
    o.sp = Math.min(1.1 * U, wd);
    PP.brush(px(xm), ya + .4 * U, px(xm), j & 1 ? ya + (yb - ya) * .72 : yb, wd, tone(j & 1 ? pal.brick[5] : dark, front, x, y), o);
  } else if (m === STONE || m === SHADE || m === OPEN || m === SHUTTER) {   // level strokes
    const l = (m === STONE || m === SHADE ? 3 + R() * 7 : 2 + R() * 3) * U / PW, xa = fit(Math.max(x0, xf - l), c), xb = fit(Math.min(x1, xf + l), c);
    const col = m === STONE ? (snow && R() < .65 ? pal.snow : mix(pal.ledge, pal.stone, R() * .25))
      : m === SHADE ? dark : m === OPEN ? mix(pal.opening, pal.dark, R() * .35) : night ? mix(pal.shutter, pal.lights[1], .6) : mix(pal.shutter, pal.gold, R() * .3);
    PP.brush(px(xa), py(levelY(c, xa)), px(xb), py(levelY(c, xb)), (m === STONE ? 1.8 : 1.5) * U, m === SHUTTER && night ? col : tone(col, front, x, y), o);
  } else if (m === ARCH || m === RIM) {                        // an arch: its bricks stand on end, a moulding runs round it
    const cx = px(wx), cy = py(levelY(wc, wx)); let ux = x - cx, uy = y - cy; const q = 1 / (Math.hypot(ux, uy) || 1); ux *= q; uy *= q;
    if (m === RIM) { const l = (2.5 + R() * 4) * U; PP.brush(x + uy * l, y - ux * l, x - uy * l, y + ux * l, 1.5 * U, tone(mix(pal.ledge, pal.brick[2], .5), front, x, y), o); return; }
    let l = (2 + R() * 2.5) * U;
    if (wallAt(fx(x + ux * l), levelOf(fx(x + ux * l), fy(y + uy * l))) !== ARCH || wallAt(fx(x - ux * l), levelOf(fx(x - ux * l), fy(y - uy * l))) !== ARCH) l *= .45;
    const col = mix(pal.brick[(hash1(Math.floor(Math.atan2(uy, ux) * 60 / (1 + 1 / q / (9 * U)))) * 4) | 0], pal.brick[5], .3);
    PP.brush(x - ux * l, y - uy * l, x + ux * l, y + uy * l, 1.9 * U, tone(col, front, x, y), o);
  } else if (m === ROUND) {                                    // a round stone with a hole in it
    const cx = px(wx), cy = py(levelY(wc, wx)), r = Math.max(1.7 * U, wr * PW);
    PP.dab(cx, cy, r, tone(pal.ledge, front, x, y), { a: 1, den: 2.2, core: .9, s: U, jit: 4 });
    PP.dab(cx, cy, r * .45, tone(dark, front, x, y), { a: 1, den: 2.5, core: 1, s: .8 * U, jit: 3 });
  } else {                                                     // upright strokes: boards, a keystone, the edge of a pier, the downpipe
    const l = (2.5 + R() * 5) * U / PH, fitY = p => wallAt(xf, levelOf(xf, p)) === m ? p : wallAt(xf, levelOf(xf, (p + pf) / 2)) === m ? (p + pf) / 2 : pf;
    const wd = m === PIPE ? Math.max(1.6 * U, .0052 * PW) : m === EDGE ? 1.3 * U : m === KEY ? 2 * U : 1.8 * U;
    const cx = m === BOARD ? (Math.floor(x / (2 * U)) + .5) * 2 * U : m === PIPE ? px(.697) : x;
    let col = m === PIPE ? mix(pal.zinc, pal.gutter, R() * .5) : m === EDGE ? dark : m === KEY ? mix(pal.ledge, pal.stone, R() * .2) : mix(pal.board, pal.stone, hash1(Math.floor(x / (2 * U))) * .45);
    col = tone(col, front, x, y);
    if (m === BOARD && wlit && night) col = R() < .3 ? pal.lights[0] : mix(col, pal.lights[1], .35);   // someone has left a light on in there
    o.sp = Math.min(1.1 * U, wd);
    PP.brush(cx, py(fitY(pf - l)), cx, py(fitY(pf + l)), wd, col, o);
  }
}
function roofStroke(x, y, xf, pf, o) {                         // the underside of the eaves, dark, with the ends of the rafters; the gutter along their edge
  const R = PP.R, left = xf < PEAK[0], a = left ? EAVE : PEAK, b = left ? PEAK : GUT;
  let ca = (b[0] - a[0]) * PW, sa = (b[1] - a[1]) * PH; const q = 1 / Math.hypot(ca, sa); ca *= q; sa *= q;
  const top = py(roofY(xf)), l = (4 + R() * 8) * U;
  let xa = x - ca * l, xb = x + ca * l, ya = y - sa * l, yb = y + sa * l;
  if (y - top < 3.4 * U) {                                     // the gutter (or the snow on the roof)
    const e = clamp(fx(xa), EAVE[0], GUT[0]), f = clamp(fx(xb), EAVE[0], GUT[0]);
    PP.brush(px(e), py(roofY(e)) + 1.3 * U, px(f), py(roofY(f)) + 1.3 * U, 2.2 * U, snow ? pal.snow : mix(pal.gutter, pal.zinc, R() * .5), o);
    return;
  }
  if (towerPart(fx(xa), fy(ya)) !== ROOF) { xa = x - ca * l * .3; ya = y - sa * l * .3; if (towerPart(fx(xa), fy(ya)) !== ROOF) return; }
  if (towerPart(fx(xb), fy(yb)) !== ROOF) { xb = x + ca * l * .3; yb = y + sa * l * .3; if (towerPart(fx(xb), fy(yb)) !== ROOF) return; }
  const rafter = Math.floor((left ? pf * 110 : xf * 62)) % 3 === 0;
  PP.brush(xa, ya, xb, yb, Math.min((1.6 + R() * 1.4) * U, 2 * (y - top - 2.6 * U)), tone(rafter ? mix(pal.soffit, pal.rafter, .7) : mix(pal.soffit, pal.dark, R() * .35), !left, x, y), o);
}
function lampDab() {                                           // the lamp on its bracket, left of the corner
  const x = px(.4255), y = py(.405);
  if (!at(x, y)) return;
  PP.haze = null;                                              // a light is seen through any fog, and all the wider for it
  if (!night) { PP.dab(x, y, 1.2 * U, pal.ink, { a: 1, den: 2.5, core: .9, s: U, jit: 3 }); return; }
  PP.dab(x - U, y + 5 * U, (7 + traits.fog * 5) * U, pal.lights[0], { a: .09, den: .4, s: 2.4 * U, jit: 3 });
  light(x, y, U);
}

// ---------- the ground ----------
function farStroke() {                                         // the trees where the path ends
  const R = PP.R, x = R() * Math.min(W, px(.36)), top = py(farTop(fx(x))), foot = py(VPY) + 3 * U, y = top + R() * (foot - top);
  if (x < 0 || !at(x, y) || zone(x, y) !== FAR) return;
  const f = (foot - y) / (foot - top), n = vnoise(x * .05 / U + 11, y * .09 / U), k = R();
  let col = mix(pal.folMid, n > .5 ? pal.folLight : pal.folDark, Math.abs(n - .5) * 1.5);
  if (f < .2) col = mix(col, pal.folDeep, .4);
  else if (snow && k < .4) col = mix(col, pal.snow, .75);      // it lies on their twigs
  else if (f > .5 && k < .35) col = mix(col, traits.bare ? pal.folLight : pal.folSun, .5); else if (pal.accent && k > .93) col = pick(pal.accent, R());
  veil(.9);
  PP.dab(x, y, (1.5 + R() * 2.6) * U, mix(col, pal.haze, .18), { a: .9, den: .8, core: .6, s: 1.6 * U });
}
function pathStroke() {                                        // gravel: every stroke points to where the path ends
  const R = PP.R, x = R() * W, y = HZ - 8 * U + R() * (H - HZ + 8 * U);
  if (!at(x, y) || zone(x, y) !== PATH) return;
  const xf = zx, pf = zy, w = zw, near = clamp((pf - VPY) / .4), sz = (.4 + near) * U, k = R();
  veil(flatOff(pf));
  let dx = (xf - VPX) * PW, dy = (pf - VPY) * PH; const q = 1 / (Math.hypot(dx, dy) || 1); dx *= q; dy *= q;
  const n = fbm(x * .02 / U + 7, y * .05 / U), sd = shadow(x, y), wr = pathR(pf), wl = pathL(pf);
  let col = mix(pal.gravel, n > .5 ? pal.gravelLight : pal.gravelDark, Math.abs(n - .5) * 1.5);
  if (Math.min(Math.abs(w - wr + 1), Math.abs(w - wr + 2.3)) < .2) col = mix(col, pal.gravelLight, .3);   // where the wheels run
  if (wr - w < .1 || w - wl < .14) col = mix(col, mix(pal.grass, pal.gravelDark, .5), .5);              // grass creeps in from the sides
  if (sd) col = mix(col, pal.gravelDark, sd * .55);
  if (k < .2) { PP.dab(x, y, (1 + R() * 1.5) * sz, k < .09 ? pal.gravelLight : mix(pal.gravelDark, pal.stone, .3), { a: .8, den: 1, core: .5, s: 1.3 * U }); return; }   // stones
  if (season === 'autumn' && k > .975 && vnoise(xf * 14, pf * 30) > .5) { PP.dab(x, y, (1.4 + R() * 1.6) * sz, pick(pal.bits, R()), { a: .9, den: 1.4, core: .8, s: 1.4 * U }); return; }   // leaves
  const l = (4 + R() * 10) * sz;
  PP.brush(x - dx * l, y - dy * l, x + dx * l, y + dy * l, (1.6 + R() * 2.6) * sz, col, { a: .85, jit: 8, dry: .15, sp: 1.1 * U, bow: .02 });
}
function meadowStroke() {                                      // left of the path: grass nobody mows
  const R = PP.R, x = R() * Math.max(0, px(.06)), y = HZ - 8 * U + R() * (H - HZ + 8 * U);
  if (!at(x, y) || zone(x, y) !== MEADOW) return;
  const near = clamp((zy - VPY) / .4), sz = (.45 + near) * U, n = fbm(x * .012 / U + 4, y * .04 / U + 1), sd = shadow(x, y), k = R();
  let col = mix(mix(pal.grass, pal.grassDark, n), pal.straw, smooth(.4, .7, fbm(x * .02 / U + 9, y * .05 / U)) * .7);
  if (k < .2) col = mix(col, pal.grassLight, .45); else if (k > .85) col = mix(col, pal.grassDark, .4);
  if (sd) col = mix(col, pal.grassDark, sd * .6);
  col = mix(col, pal.haze, (1 - near) * .25);
  veil(flatOff(zy));
  const a = -Math.PI / 2 + (R() - .5) * .9 + wind(time) * .08, l = (3 + R() * 7) * sz;
  PP.brush(x, y, x + Math.cos(a) * l * 2, y + Math.sin(a) * l * 2, (1.5 + R() * 2.5) * sz, col, { a: .85, jit: 9, dry: .2, sp: 1.2 * U, bow: .15 });
}
function shrub(x, y, r) {                                      // where the ivy has got down the slope
  const R = PP.R, k = R();
  PP.dab(x, y, r, k < .5 ? pal.ivyDark : k < .85 ? pal.ivyMid : pal.ivyDeep, { a: .9, den: .7, core: .6, s: 2 * U });
  if (snow) { if (k < .6) PP.dab(x, y - r * .4, r * .6, pal.snow, { a: .85, den: .6, core: .4, s: 1.5 * U, sx: 1.4, sy: .6 }); }
  else if (k < .45) PP.dab(x + r * .3, y - r * .4, r * .55, pal.ivyLight, { a: .85, den: .6, core: .4, s: 1.6 * U });
}
let slopeTop = 0;
function slopeStroke() {                                       // the embankment: long grass hanging down it, dry where the sun gets at it
  const R = PP.R, x = R() * W, y = slopeTop + R() * (H - slopeTop);
  if (!at(x, y) || zone(x, y) !== SLOPE) return;
  const xf = zx, pf = zy, foot = footAt(xf), edge = VPY + (xf - VPX) / pathR(pf), u = clamp((pf - foot) / Math.max(.01, edge - foot));   // 0 under the ivy, 1 at the path
  const near = clamp((pf - .5) / .4), sz = (.55 + near * .8) * U, sd = shadow(x, y), k = R();
  veil(slopeOff(xf, pf, u));
  const n = fbm(x * .012 / U + 2, y * .02 / U + 5), dry = smooth(.4, .62, n + (xf - .55) * .35 - u * .3);
  if (fbm(xf * 13 + 1.3, pf * 16) + smooth(.6, 1, xf) * .14 - Math.abs(u - .5) * .35 > .63) { shrub(x, y, (2.2 + R() * 3) * sz); return; }
  let col = u > .8 ? mix(pal.grass, pal.grassLight, n * .6) : mix(mix(pal.grass, pal.grassDark, n), pal.straw, dry * .85);
  if (u < .14) col = mix(col, pal.grassDark, .55);             // under the ivy
  if (k < .18) col = mix(col, pal.grassLight, .5); else if (k > .86) col = mix(col, pal.grassDark, .45);
  if (sd) col = mix(col, pal.grassDark, sd * .6);
  const a = Math.PI * (.6 + dry * .06) + (R() - .5) * .7, l = (4 + R() * 8) * sz * (1 + dry * .5) * (u > .88 ? .6 : 1), ca = Math.cos(a) * l, sa = Math.sin(a) * l;
  PP.brush(x - ca, y - sa, x + ca, y + sa, (1.6 + R() * 2.6) * sz, col, { a: .85, jit: 9, dry: .2, sp: 1.2 * U, bow: .15 });
}

// ---------- leaves and limbs ----------
let bushLeft = 0;
function bushDab() {                                           // somewhere in the bushes or the ivy
  const R = PP.R, x = bushLeft + R() * (W - bushLeft), xf = fx(x), line = bushTop(xf);
  leaf(x, py(line - .03 + R() * (along2(FEET, xf) + .04 - line)));
}
function crownDab() {                                          // somewhere in the crown of a tree
  const R = PP.R; let k = R() * leafArea, c = leafy[0];
  if (!c) return;
  for (let a = 0, b = leafy.length - 1; a <= b;) { const m = (a + b) >> 1; if (leafy[m].at <= k) { c = leafy[m]; a = m + 1; } else b = m - 1; }
  const a = R() * TAU, d = Math.sqrt(R()) * c.r * 1.1;
  leaf(c.x + Math.cos(a) * d, c.y + Math.sin(a) * d);
}
function leaf(x, y) {
  const R = PP.R;
  if (!at(x, y)) return;
  const z = zone(x, y), d = zf;
  if (z === TREE || z === PATH || z === MEADOW || z === SLOPE || d < -.08) return;
  const tree = folTree, xf = zx, line = py(bushTop(xf)), deep = tree ? 0 : clamp((y - line) / (.1 * PH)), k = R();
  const sz = (tree ? .5 : .55 + deep * .3) * U, lum = clamp(folLum(x, y) + (tree ? .1 : -deep * .25));
  veil(tree ? folOff : farOff(xf, zy));
  if (d < .12) {                                               // the ragged rim: single leaves
    PP.dab(x, y, (2.5 + R() * 5) * sz, tree ? (lum > .5 ? pal.folMid : pal.folDark) : lum > .5 ? pal.ivyMid : pal.ivyDark, { a: .85, den: .3, s: 2.4 * U });
    return;
  }
  const a = R() * Math.PI, l = (4 + R() * 8) * sz;
  let col;
  if (tree) {                                                  // the trees go with the season
    col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
    if (lum > .6 && k < .3) col = pal.folSun; else if (pal.accent && lum > .3 && k > (season === 'spring' ? .6 : .92)) col = pick(pal.accent, R());
  } else {                                                     // the ivy, and the bushes it has grown over: green the whole year round, all of it
    if (snow && lum > .5 && k < lum * .7) { PP.dab(x, y, (4 + R() * 8) * sz, lum > .7 ? pal.snowSun : pal.snow, { a: .9, den: .6, core: .55, sx: 1.7, sy: .55 }); return; }   // snow lies on it
    col = lum < .3 ? mix(pal.ivyDeep, pal.ivyDark, lum / .3) : lum < .6 ? mix(pal.ivyDark, pal.ivyMid, (lum - .3) / .3) : mix(pal.ivyMid, pal.ivyLight, (lum - .6) / .4);
    if (!snow && lum > .62 && k < .3) col = pal.ivySun;
  }
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (4 + R() * 6) * sz, col, { a: .85, sp: 1.3 * U, dry: .2, jit: 10 });
}
function treeStroke() {                                        // a stretch of bark
  const R = PP.R; let k = R() * limbWeight, s = limbs[0];
  if (!s) return;
  for (const t of limbs) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span;
  const mx = limbX(s, tm), my = limbY(s, tm);
  if (!at(mx, my) || zone(mx, my) !== TREE) return;            // not where leaves or the tower hide it
  veil(s.off);
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 2 ? .16 : .06), q = R();
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (snow) { if (s.w > 1.3 * U && q < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snow, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax + s.w * .22, ay, bx + s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .5), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });      // the sunny side
  }
}

// ---------- weather ----------
// Fog is not a thing that stands somewhere: it is between us and everything, and the more of it the farther off
// a thing is.  That is veil()'s work.  Where the path ends it lies thickest, in banks that drift.
function fogStroke() {
  const R = PP.R, x1 = Math.min(W, px(.32)), x = R() * x1, y = py(.475 + R() * .15);
  if (x1 <= 0 || !at(x, y) || !traits.fog) return;
  const xf = fx(x), e = Math.hypot((xf - VPX) / .26, (fy(y) - .55) / .075);    // thinning out towards its edges
  if (e > 1 || fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) < .36 + e * .22) return;
  const l = (14 + R() * 30) * U;
  PP.haze = null;
  PP.brush(x - l, y, Math.min(x + l, px(.325)), y + (R() - .5) * 2 * U, (4 + R() * 8) * U, fogCol(), { a: .32, jit: 3, dry: .25, sp: 2 * U, bow: .03 });
}

// ---------- the coat ----------
function coatStroke(x, y) {                                    // a broad stroke of a fresh coat
  const R = PP.R, o = { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .03 }, l = (40 + R() * 40) * U;
  const z = zone(x, Math.min(y, H - 1), true);
  if (z === SKY || z === TOWER) return;
  veil(z === FAR ? .9 : z === LEAVES ? (folTree ? folOff : farOff(zx, zy)) : z === SLOPE ? slopeOff(zx, zy, .5) : flatOff(zy));
  if (z === FAR) PP.brush(x - l * .5, y, x + l * .5, y, .05 * PH, mix(pal.folMid, pal.haze, .2), o);
  else if (z === LEAVES) leafCoat(x, y, folTree, 1, o);
  else {
    let dx = l, dy = 0, col = mix(pal.grass, pal.grassDark, .4);
    if (z === PATH) { dx = (zx - VPX) * PW; dy = (zy - VPY) * PH; const q = l / (Math.hypot(dx, dy) || 1); dx *= q; dy *= q; col = pal.gravel; }
    else if (z === SLOPE) { dx = -l * .45; dy = l * .6; col = mix(pal.grass, zx > .6 ? pal.straw : pal.grassDark, .4); }
    o.jit = 4;
    PP.brush(x - dx, y - dy, x + dx, y + dy, (52 + R() * 12) * U, col, o);
  }
}
function leafCoat(x, y, tree, sz, o) {                        // a patch of leaves, in shade or in the light
  const R = PP.R, lum = clamp(folLum(x, y)), a = R() * Math.PI, fl = 16 * sz * U, k = clamp(lum * 1.3);
  PP.brush(x - Math.cos(a) * fl, y - Math.sin(a) * fl, x + Math.cos(a) * fl, y + Math.sin(a) * fl, (26 + R() * 14) * sz * U,
    tree ? mix(pal.folDeep, lum > .5 ? pal.folMid : pal.folDark, k) : mix(pal.ivyDeep, lum > .5 ? pal.ivyMid : pal.ivyDark, k), o);
}
// The ivy and the bushes are one solid mass from a little below their tops down to the grass: SOLID is how little.
// The tower's coat stops there, and theirs begins.
const SOLID = .014, hedgeTop = x => py(bushTop(fx(x)) + SOLID), hedgeFoot = x => py(footAt(fx(x)));
function hedgePath(c, whole) {                                 // that mass, or (whole) all there is above it
  const a = Math.max(0, px(whole ? EAVE[0] : .07)) - 1, b = (whole ? Math.min(W, px(GUT[0])) : W) + 1, lap = whole ? 0 : 2 * U;
  c.moveTo(a, whole ? -60 : hedgeFoot(a));
  if (whole) c.lineTo(b, -60); else { for (let x = a + 3; x < b; x += 3) c.lineTo(x, hedgeFoot(x)); c.lineTo(b, hedgeFoot(b)); }
  for (let x = b; x > a; x -= 3) c.lineTo(x, hedgeTop(x) - lap);
  c.lineTo(a, hedgeTop(a) - lap); c.closePath();
}
function hedgeCoat(x, y) {                                     // the hedge's share of it: leaves all over, right up to that line
  const top = hedgeTop(x), foot = hedgeFoot(x);
  if (fx(x) < .07 || foot < top || y < top - 30 * U || y > foot + 30 * U) return;
  y = clamp(y, top, foot);
  fol(x, y, false); veil(farOff(fx(x), fy(y)));
  for (let n = -1; n < 2; n++) leafCoat(x + n * 22 * U, y, false, 1.6, { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .03 });
}
const faceY = (c, xf, front) => HOR + (c - HOR) * (front ? (VRX - xf) / (VRX - XC) : (xf - VLX) / (XC - VLX));
function facePath(c, front) {
  const a = front ? XC : XL, b = front ? XR : XC;
  c.moveTo(px(a), py(levelY(WALL, a))); c.lineTo(px(b), py(faceY(WALL, b, front))); c.lineTo(px(b), py(faceY(FOOT, b, front))); c.lineTo(px(a), py(levelY(FOOT, a))); c.closePath();
}
function towerCoat(x, y, part) {                               // the tower's share of it: broad strokes along its eaves (0), its left side (1), its front (2)
  const xf = fx(x), pf = fy(y), front = part === 2, x0 = front ? XC : XL, x1 = front ? XR : XC;
  const R = PP.R, o = { a: .95, jit: 6, dry: 0, full: 1, sp: 2.2 * U, bow: 0 }, l = (50 + R() * 40) * U / PW;
  if (pf < PEAK[1] - .06 || pf > FOOT + .06 || xf < EAVE[0] - .12 || xf > GUT[0] + .12) return;
  veil(towerOff(xf));
  if (part) {
    if (xf < x0 - l || xf > x1 + l) return;
    const c = clamp(levelOf(clamp(xf, x0, x1), pf), WALL, FOOT);
    PP.brush(px(xf - l), py(faceY(c, xf - l, front)), px(xf + l), py(faceY(c, xf + l, front)), 72 * U, tone(mix(pal.brick[1], pal.brick[3], .45), front, x, y), o);
    return;
  }
  const wd = .05 * PH, a = Math.max(EAVE[0], xf - l), b = Math.min(GUT[0], xf + l);   // the eaves hang over the walls
  if (a < PEAK[0]) PP.brush(px(a - .03) + wd * .4, py(roofY(a - .03)), px(Math.min(b, PEAK[0])) + wd * .4, py(roofY(Math.min(b, PEAK[0]))), wd, tone(pal.soffit, false, x, y), o);
  if (b > PEAK[0]) PP.brush(px(Math.max(a, PEAK[0])), py(roofY(Math.max(a, PEAK[0]))) + wd * .4, px(b + .03), py(roofY(b + .03)) + wd * .4, wd, tone(pal.soffit, true, x, y), o);
  if (Math.abs(xf - CHIM[0]) < .1) PP.brush(px(CHIM[0] + .0105), py(CHIM[2]) - 4 * U, px(CHIM[0] + .0105), py(roofY(CHIM[0])) + 6 * U, (CHIM[1] - CHIM[0]) * PW + 8 * U, pal.brick[1], o);
}

// ---------- what moves ----------
const PERCH = [.505, .528, .604, .636];
function drawRoofLife(P, t) {                                  // pigeons on the gutter by day, bats about the roof on a warm night
  const s = Math.max(1.6 * U, .0042 * PW);
  if (!at(px(.56), py(.04))) return;
  if (night) {
    if (traits.rain || traits.snow || traits.fog || season === 'winter') return;
    for (let i = 0; i < 3; i++) {
      const ph = i * 2.1, xf = .56 + Math.sin(t * (.5 + i * .13) + ph) * .3 + Math.sin(t * 2.3 + ph) * .03;
      const x = px(xf), y = py(roofY(clamp(xf, EAVE[0], GUT[0])) - .035 - (.5 + .5 * Math.sin(t * (.8 + i * .2) + ph * 2)) * .06 + Math.sin(t * 3.7 + ph) * .008), fl = Math.sin(t * 22 + ph) * s;
      if (!at(x, y)) continue;
      const col = mix(pal.ink, [6, 8, 14], .6), o = { a: .9, sp: U, dry: 0, jit: 3, bow: .2 };
      P.brush(x - s * 1.5, y - fl, x, y, 1.3 * U, col, o); P.brush(x, y, x + s * 1.5, y - fl, 1.3 * U, col, o);
    }
    return;
  }
  const u = (t % 34) / 5;                                      // now and then one of them takes a turn round the chimney
  for (let i = 0; i < PERCH.length; i++) {
    let x = px(PERCH[i]), y = py(roofY(PERCH[i])) - s * .9; const up = i === 2 && u < 1;
    if (up) { x += Math.sin(u * TAU) * .05 * PW; y -= Math.sin(u * Math.PI) * .055 * PH; }
    if (!at(x, y)) continue;
    const col = mix(pal.ink, pal.towerLight, .3), fl = Math.sin(t * 16) * s * 1.2;
    P.dab(x, y, s, col, { a: .95, den: 2.5, core: .9, s: U, jit: 3 });
    P.dab(x + (i & 1 ? -1 : 1) * s * .8, y - s * .85, s * .55, col, { a: .95, den: 2.5, core: .9, s: .8 * U, jit: 3 });
    if (up) for (const d of [-1, 1]) P.brush(x, y, x + d * s * 2.2, y - fl, 1.6 * U, col, { a: .9, sp: U, dry: 0, jit: 4, bow: .2 });
  }
}

// ---------- the painting, one frame's worth ----------
let tops = null, stop = null, landTop = 0;
function buildSky() {                                          // where the sky ends, pixel by pixel
  const n = Math.ceil(W) + 1;
  tops = new Float32Array(n); stop = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    const xf = fx(i);
    tops[i] = py(solidTop(xf));
    stop[i] = xf > XL && xf < XR ? py(xf >= CHIM[0] && xf < CHIM[1] ? CHIM[2] : roofY(xf)) : tops[i] - U;
  }
}
const OPEN_SKY = [[-.02, .43], [.04, .4], [.1, .34], [.18, .29], [.3, .26]];   // how far down the sky is open on the left
const RATE = { far: 9000, tower: 15000, path: 8000, meadow: 12000, slope: 7000, fol: 8000, tree: 1.2, lamp: 4, fog: 1500 };
const place = {
  colors: {
    brick: ['#a98a5c', '#8b6a47', '#6e5440', '#4c3d34', '#3a302c', '#b79c6c', '#7d5a41', '#96774f'], brickRed: ['#b85c3c', '#c66f45', '#99452f', '#70392c', '#a8523a'],
    brickShade: '#3a3f4a', ledge: '#b7ae9d', soffit: '#3b2c25', rafter: '#74523c', gutter: '#b3babc', zinc: '#8a9296',
    board: '#aaa598', shutter: '#d8b13a', opening: '#353a43', pot: '#8f4a38',
    gravel: '#aba69c', gravelLight: '#c6c1b5', gravelDark: '#807c74', grass: '#6f9b3d', grassLight: '#a8c653', grassDark: '#3f6a31', straw: '#c8ba88',
    ivyDeep: '#14261a', ivyDark: '#1f3d24', ivyMid: '#37652e', ivyLight: '#6f9a3a', ivySun: '#b9cc58',
    snow: '#edf0f1', snowSun: '#ffffff',
  },
  seasons: {
    spring: { ivyMid: '#42782f', ivyLight: '#86b443', grass: '#86b84a', grassLight: '#c3df62', grassDark: '#4f8438', straw: '#b9c27a' },
    autumn: { ivyMid: '#3f6a2c', ivyLight: '#8a9a38', ivySun: '#d8c44c', grass: '#8d9a46', grassLight: '#c4bb5e', grassDark: '#59692f', straw: '#d6bf7c' },
    // Winter by itself is bare and dull, not white: the grass has gone the colour of straw, the twigs are twigs.
    winter: { grass: '#878c5a', grassLight: '#aeab76', grassDark: '#5a6243', straw: '#bcab80', folLight: '#847a70',
      ivyDeep: '#1a2420', ivyDark: '#26392b', ivyMid: '#3a5236', ivyLight: '#5c7a41', ivySun: '#8d9c58' },
  },
  weathers: {                                                  // white is what snow makes it, and frost a little
    snow: { grass: '#eef1f1', grassLight: '#ffffff', grassDark: '#aab7c5', gravel: '#e4e8eb', gravelLight: '#ffffff', gravelDark: '#b9c3ce', straw: '#d9d6c8' },
    frost: { grass: '#9fa893', grassLight: '#dfe6e1', grassDark: '#6a7763', straw: '#c9c5ae', gravel: '#b5b4ae', gravelLight: '#d9d9d3', folLight: '#bfc4c4', ivyLight: '#93aa97', ivySun: '#d6e0da' },
  },
  ground: /^(gravel|straw|grass|snow)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    const wide = clamp(W / H - .75), y1 = 1 - .25 * wide;      // a wide screen loses the path at its feet
    Y0 = .012 * wide; PH = H / (y1 - Y0); PW = PH * .75;
    X0 = .59 - .63 * W / PW;                                   // the tower stands right of the middle
    HZ = place.hz = py(HOR);
    CH = Math.max(2 * U, .0032 * PH);
    TX0 = Math.max(0, px(EAVE[0])); TX1 = Math.min(W, px(GUT[0])); TY0 = Math.max(0, py(PEAK[1])); TY1 = Math.min(H, py(.51));
    slopeTop = py(.43); bushLeft = Math.max(0, px(.07));
    buildTrees(); buildSky();
    landTop = Math.min(frontTop, py(PEAK[1])) - 8 * U;
  },
  tick(t) { time = t; },
  top(x) { return stop[x < 0 ? 0 : x >= W ? W - 1 : x | 0]; },
  sky(c, pad) {                                                // down to the trees and the ivy, and round the tower
    const dip = 4 * U - pad, xr = Math.ceil(Math.max(W, px(GUT[0]) + 1)), xl = Math.floor(Math.min(0, px(EAVE[0]) - 1)), a = px(XR), b = px(XL);
    const g = i => (i >= 0 && i <= W ? tops[i | 0] : py(solidTop(fx(i)))) + dip;
    c.moveTo(xl, -60); c.lineTo(xr, -60); c.lineTo(xr, g(xr));
    for (let i = xr - 3; i > a; i -= 3) c.lineTo(i, g(i));
    c.lineTo(a, g(a)); c.lineTo(a, py(END)); c.lineTo(px(GUT[0]), py(END)); c.lineTo(px(GUT[0]), py(GUT[1]));
    c.lineTo(px(CHIM[1]), py(roofY(CHIM[1]))); c.lineTo(px(CHIM[1]), py(CHIM[2])); c.lineTo(px(CHIM[0]), py(CHIM[2])); c.lineTo(px(CHIM[0]), py(roofY(CHIM[0])));
    c.lineTo(px(PEAK[0]), py(PEAK[1])); c.lineTo(px(EAVE[0]), py(EAVE[1])); c.lineTo(px(EAVE[0]), py(TIP)); c.lineTo(b, py(TIP)); c.lineTo(b, g(b));
    for (let i = b - 3; i > xl; i -= 3) c.lineTo(i, g(i));
    c.lineTo(xl, g(xl)); c.closePath();
  },
  front(s, x, y) { if (y < frontTop) return false; use(s); return maskAt(x, y) > 0 || fol(x, y, false) > -.05 || fol(x, y, true) > -.05; },   // the trees reach up into the sky
  orb(o) {                                                     // the sun and the moon stand on the left, where the sky is open
    o.x = Math.max(o.r * 1.2, Math.min(W - o.x, px(.27) - o.r * 1.4));
    o.y = Math.min(o.y * .75, py(along2(OPEN_SKY, fx(o.x))) - o.r * 1.6);   // and a bit higher, clear of the clouds that lie there
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6;
    clipBand(landTop, H);
    spend('nb-far', RATE.far * Math.max(0, Math.min(W, px(.36))) * .08 * PH / ua * pace, farStroke);
    spend('nb-tower', RATE.tower * (TX1 - TX0) * (TY1 - TY0) * .8 / ua * pace, towerStroke);
    spend('nb-path', RATE.path * W * (H - HZ) * .6 / ua * pace, pathStroke);
    spend('nb-meadow', RATE.meadow * Math.max(0, px(.06)) * (H - HZ) * .5 / ua * pace, meadowStroke);
    spend('nb-slope', RATE.slope * W * (H - slopeTop) * .5 / ua * pace, slopeStroke);
    spend('nb-fol', RATE.fol * (W - bushLeft) * .17 * PH / ua * pace, bushDab);
    if (!s || !s.traits.bare || o) spend('nb-crown', RATE.fol * leafArea * 1.6 / ua * pace, crownDab);
    spend('nb-tree', RATE.tree * limbLength / (14 * U) * pace, treeStroke);
    spend('nb-lamp', RATE.lamp * pace, lampDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('nb-fog', RATE.fog * Math.max(0, Math.min(W, px(.32))) * .15 * PH / ua * pace, fogStroke);
    PP.haze = null;
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    // The tower first, and none of it beyond its walls.  Nor down where the hedge and the slope hide it: its strokes
    // are long, and those of a later frame would lie on leaves and grass that are already there.
    c.save(); c.beginPath(); hedgePath(c, true); c.clip();
    for (let part = 0; part < 3; part++) {
      c.save(); c.beginPath(); if (part) facePath(c, part === 2); else towerPath(c); c.clip();
      for (let k = i; k < j; k++) towerCoat(cells[k].x, cells[k].y, part);
      c.restore();
    }
    c.restore();
    c.save(); c.beginPath(); hedgePath(c, false); c.clip();    // the hedge takes the tower's place there
    for (let k = i; k < j; k++) hedgeCoat(cells[k].x, cells[k].y);
    c.restore();
    for (let k = i; k < j; k++) if (cells[k].y > landTop - 20 * U) for (let n = -1; n < 2; n++) coatStroke(cells[k].x + n * 22 * U, cells[k].y + n * 14 * U);   // thrice over: nothing of the old picture is to show through, however narrow a strip
    PP.haze = null;
    c.restore();
  },
  sprites: [{ draw: drawRoofLife, t0: 4, seed: 121 }],
};
return place;
} });
