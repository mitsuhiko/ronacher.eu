/* ------------------------------------------------------------------ *
 *  Vienna, on the Donauinsel: from a bridge down the length of the
 *  Neue Donau.  A broad sheet of water runs to the horizon between two
 *  green banks: Kaisermühlen on the left, with steps down to the
 *  water, a row of poplars and red roofs behind the trees; the island
 *  on the right, a meadow under a belt of bushes.  Where the banks
 *  meet a low bridge crosses, a tall chimney stands over it, and on
 *  the right the pylon of another bridge holds up its fan of cables.
 *  The wind ruffles the water, the sun and the moon lie on it in a
 *  long glitter, and now and then a fish jumps.
 *
 *  It is painted after a photograph, measured by hand: without the
 *  building cranes that stood behind the island that day, the little
 *  boats out on the water and anybody who was on the banks.  The lamps
 *  along the path on the left are made up, and so is what lies beyond
 *  the photograph on either side.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'donauinsel', name: 'Donauinsel', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, spend, clipBand, skyCol, dens } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let pal = null, traits = null, night = false, season = '', bare = false, wet = false, sun = false, white = 0, ice = false;   // white: how much snow lies; ice: the water has a rim of it
let orbX = 0, orbUp = 0, orbK = 0, orbCol = null;   // the sun or the moon: where it stands, how high over the water, how strongly it lies on it, in which colour
const use = s => {
  pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; wet = traits.rain > 0; sun = !night && traits.shadow > 0;
  white = traits.snow ? 1 : season !== 'winter' ? 0 : s.weather === 'frost' ? .5 : s.weather === 'sunny' ? .75 : .3;
  ice = s.weather === 'frost';
  const o = s.orb;
  orbK = !o ? 0 : night ? .55 : o.y < hzY * .5 ? .3 : 1;
  if (o) { orbX = o.x; orbUp = (hzY - o.y) / PH; orbCol = night ? mix(pal.moonGlow, o.col, .6) : mix(o.col, o.glow, .3); }
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph the picture is painted after: x and y as fractions of its
// width and height.  A wide screen shows what lies to the left and right of it as well, a narrow one
// is pushed together to half its width and still loses the steps on the left and the meadow on the right.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
function along(pts, x) {                                       // a line through points, straight from one to the next
  if (x <= pts[0][0]) return pts[0][1];
  for (let i = 1; i < pts.length; i++) if (x <= pts[i][0]) { const a = pts[i - 1], b = pts[i]; return a[1] + (b[1] - a[1]) * (x - a[0]) / (b[0] - a[0]); }
  return pts[pts.length - 1][1];
}
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}
const HOR = .378, SHORE = .381, VANISH = .495;                 // eye level, the far end of the water, and where the banks run to
// Where the water ends: along the quay and the left bank to the boats moored at its end, across under the bridge,
// round the wooded point of the island and back along its bank.
const WATERLINE = [[-1.6, .739], [0, .4739], [.076, .4613], [.0806, .4566], [.14, .4495], [.2, .4405], [.26, .429], [.32, .4187], [.36, .4129], [.4073, .4035], [.438, .3969],
  [.4633, .3885], [.4787, .3838], [.49, SHORE], [.5679, SHORE], [.5705, .386], [.602, .3884], [.6252, .3907], [.644, .3945], [.68, .3994], [.7143, .4069], [.7543, .4143],
  [.7935, .4227], [.82, .4286], [.855, .437], [.897, .4454], [.925, .4529], [.96, .4594], [1, .469], [2.6, .853]];
// Where the trees end and the open ground begins: the steps, the lawn on the dam, the meadow.
const FOOT = [[-1.6, .59], [0, .4254], [.0151, .4254], [.04, .4226], [.049, .4202], [.07, .417], [.098, .4123], [.119, .407], [.1294, .407], [.149, .4024], [.1595, .4], [.163, .394],
  [.17, .3916], [.36, .3915], [.375, .388], [.39, .388], [.3935, .382], [.417, .382], [.42, .3839], [.4633, .3835], [.4787, .3825], [.49, SHORE], [.5679, SHORE], [.5705, .386],
  [.6035, .3884], [.6041, .385], [.64, .3855], [.656, .3925], [.694, .3938], [.7136, .3882], [.75, .3872], [.8, .3885], [.805, .4], [.82, .4052], [.862, .408], [.9, .411],
  [.925, .4127], [.935, .4095], [1, .4095], [1.5, .43], [2.6, .49]];
// Their tops against the sky, and between the two banks the land beyond the bridge.
const TOP = [[-1.6, .22], [-.6, .29], [-.25, .32], [-.1, .335], [0, .3476], [.049, .348], [.052, .3523], [.0595, .3545], [.0876, .354], [.088, .3504], [.098, .3485], [.14, .352],
  [.143, .356], [.21, .357], [.2239, .3518], [.228, .357], [.235, .3597], [.27, .3597], [.276, .3515], [.286, .3515], [.292, .3625], [.34, .3657], [.347, .364], [.35, .3554],
  [.354, .3554], [.357, .3652], [.368, .3652], [.372, .362], [.388, .361], [.39, .367], [.4146, .3666], [.4325, .3615], [.4493, .3625], [.455, .3644], [.459, .3662], [.4758, .368],
  [.4888, .3702], [.525, .3702], [.532, .3686], [.54, .3702], [.5679, .3702], [.5691, .3557], [.5845, .3545], [.5852, .358], [.595, .36], [.605, .3618], [.623, .359], [.644, .3627],
  [.654, .3664], [.665, .362], [.675, .3552], [.694, .3543], [.6945, .3555], [.725, .359], [.7255, .3555], [.7347, .3555], [.7396, .3503], [.7494, .3503], [.75, .3527],
  [.7634, .3555], [.764, .362], [.7739, .3639], [.7746, .3555], [.8, .358], [.84, .3585], [.915, .357], [.94, .3524], [.975, .354], [1, .3494], [1.25, .335], [1.6, .31], [2.6, .24]];
// The paths: one climbs from the quay to the top of the dam, one runs along the island.  A third keeps to the
// water on the left, a little way up the bank (LOWER of the way down from eye level to the waterline).
const UPPER = [[.042, .4459], [.07, .4426], [.0897, .4384], [.1, .4351], [.119, .4305], [.1294, .4267], [.149, .4187], [.17, .4127], [.1896, .407], [.2093, .4024], [.2296, .3963],
  [.24, .394], [.2529, .3926]];
const ISLE = [[.6041, .386], [.6406, .3907], [.6637, .3926], [.68, .3947], [.7045, .398], [.7248, .4013], [.743, .405], [.7508, .4083], [.764, .4115], [.7837, .4153], [.8047, .4195],
  [.82, .423], [.8326, .4258], [.8943, .4375], [.9195, .4417], [.96, .4482], [1, .4573]];
const LOWER = .85, RAMPS = [[.2564, .3933, .2718, .4031], [.2803, .404, .2915, .411]];   // and two flights of steps down the dam
const MIDL = .4888, MIDR = .5679;                              // between these the far bridge crosses
const QUAY = .0757;                                            // the quay ends here
const FLOAT = [.8522, .8732, .4407, .4454], LANDING = [.866, .897];   // the float of the landing on the island, and the landing

// The poplars: [where it stands, half its width, its top, its foot, where the trees in front of it begin].
const POPLARS = [[-.33, .013, .268, .41, .322], [-.21, .011, .284, .4, .33],                 // made up: the photo begins at the steps
  [.1478, .0048, .3125, .3915, .3915], [.1543, .0046, .3107, .3915, .3915], [.1697, .0086, .3074, .393, .3915], [.1841, .0055, .3256, .385, .362], [.2022, .0063, .321, .385, .362],
  [.2407, .0049, .3298, .38, .3625], [.2498, .0035, .3322, .378, .3625], [.264, .0044, .3284, .38, .3625],
  [.402, .0046, .3479, .378, .3675], [.417, .0025, .3572, .376, .3675], [.4223, .0021, .356, .376, .3675], [.4269, .0021, .36, .376, .3665]]
  .map((p, k) => ({ c: p[0], hw: p[1], top: p[2], base: p[3], cut: p[4], k }));
const POPFOOT = .3915;
function inPoplar(p, xf, pf) {                                 // slim, pointed, ragged at its edge
  const v = (pf - p.top) / (p.base - p.top);
  if (v <= 0 || pf >= p.cut) return false;
  const sh = v < .45 ? Math.pow(v / .45, .6) : v < .85 ? 1 : 1 - (v - .85) * 5;
  return Math.abs(xf - p.c) < p.hw * sh * (.78 + .44 * vnoise(pf * 700 + p.k * 9.1, xf > p.c ? 3.3 : 8.8));
}
// The bushes along the paths: [x, y, half its width, half its height].
const BUSHES = [[.0553, .452, .0048, .005], [.2311, .4228, .0083, .0112], [.2697, .3922, .0049, .0043], [.2732, .4203, .0049, .006], [.2781, .3988, .0049, .0032],
  [.2939, .4143, .0031, .0047], [.3103, .4131, .0064, .0069], [.3245, .411, .0035, .006], [.3426, .4087, .0042, .0047], [.3565, .4075, .0035, .004], [.3796, .4025, .0039, .0047],
  [.3949, .4011, .0031, .0033], [.4023, .3993, .0043, .0042], [.4203, .3947, .0025, .0024], [.4322, .3946, .0018, .0019], [.4381, .393, .0014, .003], [.4497, .3904, .0025, .0028],
  [.7339, .3905, .0049, .0033]];
// What is built, from the back to the front: the houses of Kaisermühlen over the trees, the power station far
// off with its two chimneys, and the pylon.  from, to, top, kind; a roof's top slopes.
const STACK = 1, BLOCK = 2, MAST = 3, ROOF = 4, ATTIC = 5, FLAT = 6, VILLA = 7, FAR = 8;
const B = (x0, x1, y0, kind, o) => Object.assign({ x0, x1, y0, kind, c: (x0 + x1) / 2, top: null }, o);
const BUILT = [B(-1.3, .0883, .3392, ROOF, { top: xf => .3392 + xf * .058 }), B(.0123, .0242, .3362, ATTIC), B(.0249, .0308, .3378, ATTIC), B(.0785, .084, .3408, ATTIC),
  B(.0883, .1445, .3383, FLAT), B(.1896, .1966, .343, VILLA),
  B(.4542, .459, .3644, BLOCK), B(.456, .4574, .3563, STACK, { bands: 1 }), B(.4693, .4742, .3644, BLOCK), B(.4784, .4826, .3648, BLOCK), B(.4758, .4777, .3387, STACK, { bands: 3 }),
  B(.6984, .7002, .3326, MAST)];
const THIN = BUILT.filter(b => b.kind === STACK || b.kind === MAST);
const BRIDGE = B(MIDL, MIDR, .3702, FAR), DECK = .3718, GIRDER = .3768, PIER0 = .4914, PIERS = .01224, PIERW = .0011;
// What is moored at the far end of the left bank: a white tent, a cabin and a boat.  from, to, top, foot
const MOORED_AT = [[.4395, .4451, .3797, .3839], [.455, .47, .3825, .3848], [.462, .4787, .3848, .3893]];

// The land is known column by column of the screen: where the water ends, where the trees end and begin, where
// the sky ends, how near the bank is there (1 at the quay), which way its waterline runs, where a path crosses.
let cols = 1, wl = null, ft = null, tt = null, sc = null, sl = null, pa = null, tops = null, hy = 0, hzY = 0, vx = 0, landTop = 0, landFoot = 0;
const ci = x => x < 0 ? 0 : x >= cols ? cols - 1 : x | 0;   // the column a point is in
function buildLand() {
  cols = Math.ceil(W) + 1; hy = py(HOR); hzY = py(SHORE); vx = px(VANISH);
  wl = new Float32Array(cols); ft = new Float32Array(cols); tt = new Float32Array(cols); sc = new Float32Array(cols); sl = new Float32Array(cols); pa = new Float32Array(cols); tops = new Float32Array(cols);
  for (const b of THIN) { const hw = Math.max(.0009, .8 * U / PW); b.x0 = b.c - hw; b.x1 = b.c + hw; }   // never less than a stroke
  landTop = H; landFoot = 0;
  for (let i = 0; i < cols; i++) {
    const xf = fx(i + .5), w = along(WATERLINE, xf), e = .004, s = sc[i] = clamp((w - HOR) / .085, .1, 2), rough = Math.min(1.5, s);
    wl[i] = py(w); sl[i] = (along(WATERLINE, xf + e) - along(WATERLINE, xf - e)) / (2 * e) * PH / PW;
    ft[i] = Math.min(py(along(FOOT, xf)), wl[i]);
    tt[i] = Math.min(py(along(TOP, xf) + (vnoise(xf * 90, 5.5) - .5) * .014 * rough + (vnoise(xf * 260, 1.5) - .5) * .006 * rough), ft[i] - 1);
    pa[i] = xf > UPPER[0][0] && xf < UPPER[UPPER.length - 1][0] ? py(along(UPPER, xf)) : xf > ISLE[0][0] ? py(xf < 1 ? along(ISLE, xf) : HOR + .871 * (w - HOR)) : 0;
    let top = tt[i];
    for (const b of BUILT) if (xf >= b.x0 && xf < b.x1) top = Math.min(top, py(b.top ? b.top(xf) : b.y0));
    for (const p of POPLARS) if (Math.abs(xf - p.c) < p.hw * 1.25) for (let pf = p.top; pf < p.cut; pf += .0006) if (inPoplar(p, xf, pf)) { top = Math.min(top, py(pf)); break; }
    tops[i] = top;
    if (top < landTop) landTop = top; if (wl[i] > landFoot) landFoot = wl[i];
  }
  landTop = Math.max(0, landTop - 8 * U); landFoot = Math.min(H, landFoot);
}

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes
// may start there.
const SKY = 0, BUILDING = 1, TREE = 2, POPLAR = 3, GRASS = 4, PATH = 5, EDGE = 6, STONE = 7, MOORED = 8, DOCK = 9, WATER = 10;
let zx = 0, zy = 0, zk = 0, zd = 0, zs = 1, zq = 0, zb = null, zt = null;   // what zone() learnt on the way: x and y on the photo, how far down the bank, how deep in the leaves, how near, which part, which building, which poplar
function ground(i, xf, pf, y) {                                // what lies between the trees and the water
  const k = zk = (y - hy) / (wl[i] - hy), s = zs = sc[i];
  for (const b of BUSHES) {
    const dx = (xf - b[0]) / b[2]; if (dx < -1.2 || dx > 1.2) continue;
    const dy = (pf - b[1]) / b[3];
    if (dx * dx + dy * dy < .8 + .5 * vnoise(xf * 600, pf * 600)) { zd = clamp(.5 + dy * .5); return TREE; }
  }
  if (xf < MIDL) {                                             // the left bank
    if (xf < QUAY) {                                           // the quay: its wall, the platform, the steps with a sloping wall at their end
      if (k > .92) { zq = 3; return STONE; }
      if (k > (xf < .04 ? .72 + Math.max(0, xf) * 1.4 : .776 + (xf - .04) * 4.31)) { zq = 2; return STONE; }
      if (xf > -.2) { const e = .0151 + (pf - .4254) * 1.685; if (xf < e) { zq = 0; return STONE; } if (xf < e + .0045) { zq = 1; return STONE; } }
    }
    if (k > .9) { zq = 0; return EDGE; }                       // long grass down to the water
    if (xf > .058 && Math.abs(k - LOWER) < Math.max(.022, .6 * U / (wl[i] - hy))) { zq = 0; return PATH; }
    if (pa[i] && Math.abs(y - pa[i]) < Math.max(.8 * U, .0026 * s * PH)) { zq = 1; return PATH; }
    if (xf > .256 && xf < .292) for (const r of RAMPS) if (xf >= r[0] && xf < r[2] && Math.abs(pf - r[1] - (r[3] - r[1]) * (xf - r[0]) / (r[2] - r[0])) < .0012) { zq = 3; return PATH; }
    return GRASS;
  }
  if (xf > LANDING[0] && xf < LANDING[1] && k > .87) { zq = 1; return DOCK; }
  if (k > .93) { zq = 1; return EDGE; }                        // stones along the island
  if (pa[i] && Math.abs(y - pa[i]) < Math.max(.8 * U, (.001 + .0007 * s) * PH)) { zq = 2; return PATH; }
  return GRASS;
}
function bridgePart(xf, pf) {                                  // 0 the land beyond it, 1 its parapet, 2 the girder, 3 a pier, 4 what one sees under it
  if (pf < DECK) return 0;
  if (pf < DECK + .001) return 1;
  if (pf < GIRDER) return 2;
  const j = Math.round((xf - PIER0) / PIERS);
  return j >= 0 && j < 6 && Math.abs(xf - PIER0 - j * PIERS) < Math.max(PIERW, .7 * U / PW) ? 3 : 4;
}
function zone(x, y) {
  const i = ci(x), xf = zx = fx(x), pf = zy = fy(y);
  if (xf > .439 && xf < .479 && pf > .3795 && pf < .3895) for (let n = 2; n >= 0; n--) {
    const m = MOORED_AT[n];
    if (xf >= m[0] && xf < m[1] && pf >= m[2] + (n ? 0 : Math.abs(xf - .4423) * .9) && pf < m[3]) { zq = n; return MOORED; }
  }
  if (y >= wl[i]) { if (xf > FLOAT[0] && xf < FLOAT[1] && pf > FLOAT[2] && pf < FLOAT[3]) { zq = 0; return DOCK; } return WATER; }
  if (y >= ft[i]) return ground(i, xf, pf, y);
  if (pf < POPFOOT) for (const p of POPLARS) if (xf > p.c - p.hw * 1.25 && xf < p.c + p.hw * 1.25 && inPoplar(p, xf, pf)) { zt = p; return POPLAR; }
  if (y >= tt[i]) {
    if (xf >= MIDL && xf < MIDR) { zb = BRIDGE; zq = bridgePart(xf, pf); return BUILDING; }
    zd = (y - tt[i]) / (ft[i] - tt[i]); zs = sc[i];
    return TREE;
  }
  for (let n = BUILT.length - 1; n >= 0; n--) { const b = BUILT[n]; if (xf >= b.x0 && xf < b.x1 && pf >= (b.top ? b.top(xf) : b.y0)) { zb = b; return BUILDING; } }
  return SKY;
}
function tone(k) {                                             // the colour of a thing, all in all: for a fresh coat, and for the water to mirror
  const snowy = (c, n) => white ? mix(c, pal.snow, white * n) : c;
  switch (k) {
    case BUILDING: {
      const b = zb.kind;
      if (b === FAR) return zq ? mix(pal.bridge, pal.cityFar, .3) : mix(mix(pal.folMid, pal.haze, .55), pal.cityFar, .35);
      return b === ROOF ? snowy(pal.roofRed, .8) : b === FLAT || b === VILLA ? mix(pal.hull, pal.glass, .35) : b === ATTIC ? pal.dark : mix(pal.tower, pal.cityFar, b === MAST ? .2 : .5);
    }
    case TREE: case POPLAR: return bare ? mix(pal.folDark, pal.skyLow, .35) : mix(pal.folDark, pal.folMid, .45);
    case GRASS: return snowy(pal.grass[1], .85);
    case PATH: return snowy(pal.path, .7);
    case EDGE: return zq ? snowy(pal.rock, .5) : pal.reed;
    case STONE: return zq === 3 ? pal.concreteDark : snowy(pal.concrete, .8);
    case MOORED: return zq === 2 ? mix(pal.hull, pal.floatBlue, .4) : pal.hull;
    case DOCK: return zq ? pal.stone : pal.floatBlue;
  }
  return pal.water;
}

// ---------- how far off ----------
// In fog everything pales by how far off it is, in rain a little; the water and the banks are level, there it
// is a matter of how close to the horizon a point lies.  And what is far off is paler anyway.
const flatOff = d => .05 / (.05 + d);
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;
const hazeOf = (d, k) => { const a = clamp(d * 1.4 - .1, 0, .85) * traits.fog * (k || 1) * (night ? .55 : 1) + traits.rain * d * .25; return a > .01 ? [fogCol(), Math.min(.88, a)] : null; };
const veil = d => { PP.haze = hazeOf(d); };
const glow = d => { PP.haze = hazeOf(d, .45); };                // a light carries through the fog
const off = i => flatOff((wl[i] - hy) / PH);                    // how far off the bank is at a column
const away = (c, d) => d > .5 ? mix(c, pal.cityFar, (d - .5) * .8) : c;
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
let dc = 1, ds = 0;
function lie(i, k) { const s = sl[i] * k, q = 1 / Math.hypot(1, s); dc = q; ds = s * q; }   // which way a line along the bank runs, k of the way down to the water

// ---------- trees ----------
function folLum(x, y, sz) {                                    // light falls in from the upper right
  const s = time * .03, k = .02 / sz;
  return .45 + (fbm(x * k + s, y * k * 1.2) - fbm((x + 9 * sz) * k + s, (y - 9 * sz) * k * 1.2)) * 3.2;
}
function leafCol(lum, k, R) {
  let c = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .68 && k < .22) c = pal.folSun; else if (pal.accent && lum > .3 && k > .92) c = pick(pal.accent, R());
  return c;
}
function treeStroke(x, y) {                                    // the trees along both banks, the belt of bushes on the island, a bush by the path
  const R = PP.R, i = ci(x), d = off(i), deep = zd, sz = (.3 + .7 * Math.min(1, zs)) * U, k = R();
  const lum = clamp(folLum(x, y, sz) + .04 - deep * .3 - shadow(x, y) * .3);
  veil(d);
  if (bare) {                                                  // what is behind the twigs, the twigs, and the snow lying on them
    const back = deep < .45 ? skyCol(y) : white ? mix(pal.grass[1], pal.snow, white) : pal.grass[1];
    if (k < .4) PP.dab(x, y, (4 + R() * 6) * sz, away(mix(back, pal.folMid, .25 + deep * .45), d), { a: .85, den: .5, core: .5, s: 2 * U });
    else if (white && lum > .45 && k < .55) PP.dab(x, y, (3 + R() * 6) * sz, mix(pal.folMid, pal.snow, white), { a: .9, den: .6, core: .5, sx: 1.7, sy: .55 });
    else {
      const a = -Math.PI / 2 + (R() - .5) * 1.4, l = (6 + R() * 13) * sz;
      PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 4 * sz, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
        away(lum < .4 ? pal.folDeep : k < .85 ? pal.folDark : pal.folMid, d), Math.max(.7, (.8 + R() * .9) * sz), { pass: 1, a: .8 });
    }
    return;
  }
  if (deep > .94 && zs < 1.2) {                                // under them it is dark
    const l = (3 + R() * 6) * sz;
    PP.brush(x - l, y, x + l, y, (1.4 + R()) * sz, away(mix(pal.folDeep, pal.trunk, .3), d), { a: .85, jit: 6, dry: .1, sp: 1.1 * U, bow: 0 });
    return;
  }
  if (deep < .05) { PP.dab(x, y, (2.5 + R() * 5) * sz, away(lum > .5 ? pal.folMid : pal.folDark, d), { a: .85, den: .3, s: 2.4 * U }); return; }   // the ragged rim: single leaves
  const a = R() * Math.PI, l = (4 + R() * 8) * sz;
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (4 + R() * 6) * sz, away(leafCol(lum, k, R), d), { a: .85, sp: 1.3 * U, dry: .2, jit: 10 });
}
function poplarStroke(x, y) {                                  // every stroke of it goes up
  const R = PP.R, t = zt, i = ci(x), d = off(i), sz = clamp(t.hw / .008, .4, 1.1) * U, cx = px(t.c), side = clamp((x - cx) / (t.hw * PW), -1, 1), k = R();
  const top = py(t.top), v = (zy - t.top) / (t.base - t.top);
  veil(d);
  if (bare) {                                                  // a broom: the stem, and its twigs close to it
    if (Math.abs(side) < .25 && k < .5) { const l = (5 + R() * 9) * sz; PP.brush(cx, Math.max(top + 4 * sz, y - l), cx, y + l, Math.max(1.2 * U, 2.6 * sz * v), away(mix(pal.trunk, pal.folDark, .4), d), { a: .9, jit: 6, dry: .05, bow: .02, sp: 1.1 * U }); }
    else if (k < .3) PP.dab(x, y, (3 + R() * 4) * sz, away(mix(skyCol(y), pal.folMid, .3), d), { a: .8, den: .45, core: .4, s: 2 * U, sx: .7, sy: 1.4 });
    else {
      const l = (8 + R() * 14) * sz, a = -Math.PI / 2 + side * .32 + (R() - .5) * .2;
      PP.line([[x - Math.cos(a) * l * .3, y - Math.sin(a) * l * .3], [x + Math.cos(a) * l * .7, Math.max(top, y + Math.sin(a) * l * .7)]], away(white && k > .88 ? pal.snow : k < .7 ? pal.folDeep : pal.folDark, d), Math.max(.7, (.9 + R()) * sz), { pass: 1, a: .8 });
    }
    return;
  }
  const lum = clamp((folLum(x, y, sz) + side * .22 - (v > .8 ? .15 : 0)) * .72 - shadow(x, y) * .2);   // dark, the sun on its right
  const a = -Math.PI / 2 + side * .22 + (R() - .5) * .36 + wind(time) * .07 * (1 - v), l = (4 + R() * 7) * sz, ca = Math.cos(a) * l, sa = Math.sin(a) * l;
  PP.brush(x - ca * .4, y - sa * .4, x + ca, Math.max(top - sz, y + sa), (1.6 + R() * 1.8) * sz, away(leafCol(lum, k, R), d), { a: .87, sp: 1.2 * U, dry: .16, jit: 9, bow: .1 });
}

// ---------- what is built ----------
const lit = () => ({ a: 1, den: 3, core: 1, s: 1.2 * U, jit: 4 });
function builtStroke(x, y) {
  const R = PP.R, b = zb, kind = b.kind, xf = zx, pf = zy, i = ci(x), xa = px(b.x0), xb = px(b.x1), w = xb - xa, o = { a: .92, jit: 6, dry: .05, bow: 0, sp: 1.1 * U };
  // a level stroke of it, no farther than it goes and not into the trees
  const level = (yc, bw, c, l) => { l = Math.max(.6, Math.min(l, w / 2 - .3)); const cx = clamp(x, xa + l, xb - l); if (kind === FAR || yc + bw / 2 < Math.min(tt[ci(cx - l)], tt[ci(cx + l)]) + U) PP.brush(cx - l, yc, cx + l, yc, bw, c, o); };
  if (kind === FAR) {                                          // the bridge at the far end, and the land behind it
    const part = zq, far = c => mix(c, pal.cityFar, .22), l = (4 + R() * 9) * U;
    veil(.92);
    if (part === 0) level(y, 1.3 * U, mix(mix(pal.folMid, pal.haze, .55), pal.cityFar, .35 + .3 * smooth(.52, .532, xf) * smooth(.545, .532, xf)), l);
    else if (part === 1) level(py(DECK + .0005), Math.max(U, .001 * PH), far(mix(pal.tower, pal.bridge, .3)), l);
    else if (part === 2) { const t = (pf - DECK - .001) / (GIRDER - DECK - .001), h = (GIRDER - DECK - .001) * PH; level(py(DECK + .001) + (t < .55 ? .28 : .78) * h, Math.max(U, h * .5), far(t < .55 ? (white ? mix(pal.bridge, pal.snow, white * .5) : pal.bridge) : mix(pal.bridge, pal.dark, .45)), l); }
    else if (part === 3) { const cx = px(PIER0 + Math.round((xf - PIER0) / PIERS) * PIERS), bw = Math.max(1.3 * U, PIERW * 2 * PW); o.sp = Math.min(1.1 * U, bw); PP.brush(cx, py(GIRDER), cx, hzY, bw, far(mix(pal.bridge, pal.dark, .2)), o); }
    else level(Math.min(y, hzY - .6 * U), 1.2 * U, pf < GIRDER + .0012 ? far(mix(pal.dark, pal.bridge, .3)) : mix(pal.water, pal.cityFar, .5), l * .5);
    return;
  }
  if (kind === STACK || kind === MAST) {                       // a chimney, red and white at its top; the pylon
    const cx = (xa + xb) / 2, bw = Math.max(1.3 * U, w), l = (3 + R() * 7) * U, band = Math.floor((pf - b.y0) / .0021), top = py(b.y0), foot = tt[ci(cx)] + U;
    let c = kind === MAST ? mix(pal.tower, pal.dark, .25) : mix(pal.towerLight, pal.tower, .22), ya = Math.max(top, y - l), yb = Math.min(foot, y + l);
    if (kind === STACK && band < b.bands) { c = band & 1 ? pal.hull : pal.cabin; ya = top + band * .0021 * PH; yb = ya + .0021 * PH; }
    veil(kind === MAST ? .8 : .92);
    o.sp = Math.min(1.1 * U, bw); o.dry = 0;
    if (yb > ya) PP.brush(cx, ya, cx, yb, bw, away(c, kind === MAST ? .65 : .9), o);
    return;
  }
  veil(kind === BLOCK ? .92 : .55);
  const top = b.top ? b.top(xf) : b.y0, l = (2.5 + R() * 5) * U, snowy = (c, n) => white ? mix(c, pal.snow, white * n) : c;
  if (kind === BLOCK) level(y, 1.4 * U, away(mix(pal.tower, pal.towerLight, .15 + .25 * hash1(Math.round(b.x0 * 9973))), .8), l);
  else if (kind === ROOF) {                                    // tiles, a dormer here and there
    const s = .058 * PH / PW, dormer = pf - top > .0035 && hash1(Math.floor(xf * 260) * 7 + 3) < .13;
    const c = dormer ? (night && hash1(Math.floor(xf * 260)) < .5 ? pal.lights[0] : mix(pal.hull, pal.glass, R() * .6)) : snowy(mix(pal.roofRed, pal.roof, R() * .7), pf - top < .003 ? .5 : .85);
    if (y + U < tt[i]) PP.brush(x - l, y - s * l, x + l, y + s * l, 1.5 * U, c, o);
  } else if (kind === ATTIC) {                                 // a box of glass on the roof, a white rim round it
    const rim = pf - top < .0012, on = night && hash1(Math.round(b.x0 * 9973)) < .6;
    level(rim ? py(top + .0006) : y, rim ? Math.max(U, .0012 * PH) : 1.4 * U, rim ? snowy(pal.hull, .8) : on ? mix(pal.lights[0], pal.dark, .25 + R() * .3) : mix(pal.dark, pal.glass, .2 + R() * .3), l);
  } else if (kind === FLAT) {                                  // white floors, a band of glass between each two
    const fh = Math.max(.0033, 2.6 * U / PH), fl = Math.floor((pf - top) / fh), glassy = fl & 1, yc = py(top + (fl + .5) * fh), ll = Math.max(.6, Math.min(l, w / 2 - .3)), cx = clamp(x, xa + ll, xb - ll);
    level(yc, fh * PH * .9, glassy ? mix(pal.glass, pal.dark, .25 + .3 * vnoise(xf * 400, fl)) : snowy(mix(pal.hull, pal.towerLight, xf > .125 ? .35 : 0), fl ? 0 : .8), l);
    if (night && glassy && yc < tt[i]) for (let n = Math.ceil((cx - ll - xa) / (3 * U) - .5); xa + (n + .5) * 3 * U <= cx + ll; n++) if (hash1(n * 131 + fl * 1777) < .32) PP.dab(xa + (n + .5) * 3 * U, yc, .55 * U, pal.lights[(n + fl) % pal.lights.length], lit());
  } else {                                                     // the villa between the poplars: a grey roof, a white wall
    const roof = pf - top < .006, win = !roof && Math.floor(xf / .0018) % 2 === 0 && Math.floor(pf / .0035) % 2 === 0;
    level(y, 1.4 * U, roof ? snowy(mix(pal.stone, pal.tower, .4), .8) : win ? (night && hash1(Math.floor(xf / .0018) * 7 + Math.floor(pf / .0035)) < .4 ? pal.lights[0] : pal.dark) : pal.hull, win ? U : l);
  }
}

// ---------- the banks ----------
function grassStroke(x, y) {
  const R = PP.R, i = ci(x), xf = zx, k = zk, s = zs, d = off(i), sz = (.3 + .9 * Math.min(1.3, s)) * U, r = R(), left = xf < MIDL, under = y - ft[i];
  veil(d);
  const n = fbm(xf * 46 + 3, k * 9 + (left ? 0 : 20));
  lie(i, k);
  if (white && r < white * (.5 + n * .7)) {                    // snow lies on it, and the grass shows through where it is thin
    const l = (4 + R() * 8) * sz;
    PP.brush(x - dc * l, y - ds * l, x + dc * l, y + ds * l, (2 + R() * 2.5) * sz, mix(pal.snow, pal.fieldShadow, under < 5 * sz ? .4 : shadow(x, y) * .5 + (n > .6 ? .12 : 0)), { a: .9, jit: 4, dry: .1, sp: 1.2 * U, bow: .04 });
    return;
  }
  if (season === 'spring' && !left && r > .95 && s > .25) { PP.dab(x, y, (.7 + R() * .8) * sz, pal.blossom[(R() * pal.blossom.length) | 0], { a: .95, den: 2, core: .9, s: U, jit: 4 }); return; }   // the meadow is in flower
  if (season === 'autumn' && under < 16 * sz && r > .82) { PP.dab(x, y, (.9 + R()) * sz, pick(pal.bits, R()), { a: .9, den: 1.6, core: .8, s: U, jit: 6 }); return; }   // leaves lie under the trees
  let lum = .18 + n * .8 + (R() - .5) * .25;
  if (left && k < .32) lum += .18;                             // the lawn on top of the dam is mown
  let c = ramp(pal.grass, lum);
  if (!left) c = mix(c, pal.meadow, .22 + smooth(.4, .68, fbm(xf * 30 + 9, k * 14)) * .6);   // the meadow on the island: long grass going to seed
  if (under < 5 * sz) c = mix(c, pal.folDeep, .45);            // the shade of the trees
  const sd = shadow(x, y); if (sd) c = mix(c, pal.grass[0], sd * .45);
  if (wet) c = mix(c, pal.grass[0], .2);
  let l = (3 + R() * 7) * sz;
  if (zone(x - dc * l, y - ds * l) !== GRASS || zone(x + dc * l, y + ds * l) !== GRASS) l *= .3;
  PP.brush(x - dc * l, y - ds * l, x + dc * l, y + ds * l, (1.5 + R() * 2) * sz, away(c, d), { a: .88, jit: 8, dry: .12, sp: 1.1 * U, bow: .05 });
}
function pathStroke(x, y) {                                    // gravel, pale in the sun
  const R = PP.R, i = ci(x), k = zk, s = zs, part = zq, d = off(i), sz = (.3 + .9 * Math.min(1.3, s)) * U;
  let c = mix(pal.path, pal.pathDark, R() * .35 + shadow(x, y) * .4), l = (4 + R() * 8) * sz, yc = y, bw;
  if (white) c = mix(c, pal.snow, white * .75);
  if (wet) c = mix(mix(c, pal.pathDark, .4), pal.skyLow, .25);
  veil(d);
  if (part === 0) { lie(i, LOWER); yc = hy + LOWER * (wl[i] - hy); bw = Math.max(1.2 * U, .04 * (wl[i] - hy)); if (fx(x - dc * l) < .058) l *= .3; }
  else if (part === 3) { const q = 1 / Math.hypot(PW, .64 * PH); dc = PW * q; ds = .64 * PH * q; bw = 1.2 * U; l = Math.min(l, 3 * U); }
  else {
    const xa = ci(x - 3), xb = ci(x + 3), rise = pa[xa] && pa[xb] && xb > xa ? (pa[xb] - pa[xa]) / (xb - xa) : 0, q = 1 / Math.hypot(1, rise);
    dc = q; ds = rise * q; yc = pa[i]; bw = Math.max(1.2 * U, part === 1 ? .0048 * s * PH : (.002 + .0014 * s) * PH);
    if (!pa[ci(x - dc * l)] || !pa[ci(x + dc * l)] || y - bw < ft[i]) l *= .3;
  }
  PP.brush(x - dc * l, yc - ds * l, x + dc * l, yc + ds * l, bw, away(c, d), { a: .92, jit: 5, dry: .05, bow: .02, sp: Math.min(1.1 * U, bw) });
}
function edgeStroke(x, y) {                                    // where the bank meets the water: long grass on the left, stones along the island
  const R = PP.R, i = ci(x), s = zs, d = off(i), sz = (.3 + .9 * Math.min(1.3, s)) * U, k = R();
  veil(d);
  if (white && k < white * .55) { PP.dab(x, y, (1.5 + R() * 2) * sz, pal.snow, { a: .92, den: 1.2, core: .7, s: 1.2 * U, sx: 1.7, sy: .6, jit: 3 }); return; }
  if (zq && (zk > .975 || k < .3)) {                           // a stone, its top in the light
    const r = (1.1 + R() * 1.5) * sz;
    PP.dab(x, y, r, away(mix(pal.rock, pal.rockDark, k < .12 ? .9 : .15 + R() * .5), d), { a: .93, den: 1.4, core: .85, s: 1.2 * U, sx: 1.5, sy: .7, jit: 6 });
    return;
  }
  const l = (2 + R() * 4) * sz, lean = (R() - .5) * 2.4 * sz + wind(time) * .5 * sz;   // a blade of grass, a reed
  let c = mix(pal.reed, k > .7 ? pal.reedLight : pal.grass[1], R() * .7);
  if (bare) c = mix(c, pal.reedLight, .5);
  PP.brush(x, Math.min(y + l * .4, wl[i] - .5), x + lean, y - l, (1 + R() * .8) * sz, away(c, d), { a: .88, jit: 8, dry: .1, sp: 1.1 * U, bow: .15 });
}
function stoneStroke(x, y) {                                   // the quay: steps down to a platform, its wall in the water
  const R = PP.R, i = ci(x), k = zk, part = zq, d = off(i), sz = (.4 + .7 * Math.min(1.3, zs)) * U, o = { a: .93, jit: 5, dry: .05, bow: 0, sp: 1.1 * U };
  const snowy = (c, n) => white ? mix(c, pal.snow, white * n) : c, damp = c => wet ? mix(mix(c, pal.concreteDark, .3), pal.skyLow, .2) : c;
  let l = (4 + R() * 9) * sz;
  veil(d);
  if (part === 1) {                                            // the wall at the end of the steps runs down with them
    const q = 1 / Math.hypot(1.685 * PW, PH);
    PP.brush(x - 1.685 * PW * q * l, y - PH * q * l, x + 1.685 * PW * q * l, y + PH * q * l, 2.4 * sz, snowy(mix(pal.concrete, pal.hull, .6), .8), o);
    return;
  }
  let kc = k, bw = (1.5 + R()) * sz, c;
  if (part === 3) { const edge = k < .938; kc = edge ? .929 : Math.max(k, .95); bw = edge ? Math.max(U, .018 * (wl[i] - hy)) : 1.5 * sz; c = edge ? snowy(mix(pal.concrete, pal.hull, .5), .8) : mix(mix(pal.concreteDark, pal.dark, .35), pal.waterDark, smooth(.95, 1, k) * .5); }
  else if (part === 0) {                                       // five broad steps one sits on: a tread in the light, a riser in its own shade
    const kt = ft[i] > hy ? (ft[i] - hy) / (wl[i] - hy) : .49, kb = .72, n = (k - kt) / (kb - kt) * 5, row = Math.floor(n), tread = n - row < .62;
    kc = kt + (row + (tread ? .31 : .81)) / 5 * (kb - kt); bw = Math.max(U, (tread ? .62 : .38) / 5 * (kb - kt) * (wl[i] - hy) * .9);
    c = tread ? damp(snowy(mix(pal.concrete, pal.concreteDark, .2 + R() * .12), .9)) : mix(pal.concreteDark, pal.dark, .25);
  } else c = damp(snowy(mix(pal.concrete, R() < .15 ? pal.concreteDark : pal.path, R() * .35), .85));
  lie(i, kc);
  const yc = hy + kc * (wl[i] - hy), before = part;
  if (zone(x - dc * l, yc - ds * l) !== STONE || zq !== before || zone(x + dc * l, yc + ds * l) !== STONE || zq !== before) l *= .25;
  o.sp = Math.min(1.1 * U, bw);
  PP.brush(x - dc * l, yc - ds * l, x + dc * l, yc + ds * l, bw, c, o);
}
function mooredStroke(x, y) {                                  // white things at the far end of the bank: a tent, a cabin, a boat with a dark hull
  const R = PP.R, n = zq, m = MOORED_AT[n], pf = zy, l = Math.min((2 + R() * 4) * U, (m[1] - m[0]) * PW / 2 - .3), cx = clamp(x, px(m[0]) + l, px(m[1]) - l);
  const low = n === 2 && pf > (m[2] + m[3]) / 2;
  veil(.85);
  PP.brush(cx - l, y, cx + l, y, 1.3 * U, away(low ? mix(pal.floatBlue, pal.dark, .5) : white && pf < m[2] + .0012 ? pal.snow : mix(pal.hull, pal.towerLight, R() * .4), .7), { a: .93, jit: 5, dry: .05, bow: 0, sp: 1.1 * U });
}
function dockStroke(x, y) {                                    // the landing on the island, and the blue float moored to it
  const R = PP.R, i = ci(x), pf = zy, o = { a: .93, jit: 5, dry: .05, bow: 0, sp: 1.1 * U };
  veil(off(i));
  if (zq === 0) {
    const l = Math.min((3 + R() * 6) * U, (FLOAT[1] - FLOAT[0]) * PW / 2 - .3), cx = clamp(x, px(FLOAT[0]) + l, px(FLOAT[1]) - l), rim = pf < FLOAT[2] + .0012;
    PP.brush(cx - l, y, cx + l, y, 1.4 * U, rim ? (white ? pal.snow : mix(pal.floatBlue, pal.hull, .6)) : mix(pal.floatBlue, pal.dark, pf > FLOAT[3] - .0015 ? .55 : .1 + R() * .2), o);
    return;
  }
  lie(i, zk);
  const l = (3 + R() * 5) * U, post = R() < .14;
  if (post) PP.brush(x, y, x, y - 3.5 * zs * U, U, mix(pal.trunk, pal.dark, .4), o);   // a post of its railing
  else PP.brush(x - dc * l, y - ds * l, x + dc * l, y + ds * l, 1.4 * U, white ? mix(pal.stone, pal.snow, white * .8) : mix(pal.stone, pal.path, R() * .7), o);
}

// ---------- the water ----------
// Whatever lies on it gets bigger towards us: nearOf() says how big, and lnv() counts the rows of ripples from
// the far end to our feet, so that they keep their shape all the way.
const nearOf = d => .45 + 3.4 * d;
const lnv = d => PH / 3.4 * Math.log(nearOf(d)) / U;
function body(d) { return mix(pal.water, pal.deep, smooth(.02, .4, d) * .85); }   // close by one looks down into it
function waterStroke(x, y) {
  const R = PP.R, i = ci(x), xf = zx, pf = zy, d = Math.max(.003, pf - HOR), s = nearOf(d), sz = s * U, e = (y - wl[i]) / PH;   // e: how far out from the bank
  veil(flatOff(d) * .9);
  if (ice && e < (.004 + .012 * vnoise(xf * 50, 2.2)) * sc[i]) {   // a rim of ice along the bank
    const l = (5 + R() * 12) * sz;
    PP.brush(x - l, y, x + l, y + (R() - .5) * U, (1.4 + R() * 1.6) * sz, mix(pal.ice, R() < .15 ? pal.waterDark : pal.snow, R() * .4), { a: .85, jit: 4, dry: .1, sp: 1.1 * U, bow: 0 });
    return;
  }
  const u = (x - vx) / s, v = lnv(d), gust = traits.gust, k = R();
  const rip = vnoise(u * .05 / U + time * .5, v * .34 + time * .12);
  // What stands over there, upside down and drawn out by the ripples: it is mirrored at the waterline under it.
  // Further out it is the sky over the trees, and the nearer we look the higher up in it.
  const my = wl[i] - e * PH - (rip - .5) * (.01 + e * .12) * PH;
  let col = body(d), up = SKY, mx = x;
  if (my >= landTop) { mx = x + (vnoise(u * .05 / U + 9, v * .2 + time * .3) - .5) * (.006 + e * .05) * PW; up = zone(mx, my); }
  col = mix(col, up === SKY && my > 0 && dens(mx, my) > .15 ? mix(pal.cloud, pal.cloudShade, .4) : skyCol(clamp(my, 0, landTop)), .3 + .34 * flatOff(d));
  if (up !== SKY) col = mix(col, mix(tone(up), pal.waterDark, .25), .7 * smooth(.05, .004, e) * (1 - traits.fog * .5));
  if (rip > .58) col = mix(col, pal.waterLight, Math.min(1, (rip - .58) * 4) * .5);                 // the back of a ripple has the sky on it
  else if (rip < .42) col = mix(col, pal.trough, Math.min(1, (.42 - rip) * 4) * (.6 + gust * .25));   // its front is dark
  // the wind on it: smooth lanes that shine, and gusts that run over it and ruffle it dark
  const lane = vnoise(x * .0016 / U + 3, v * .05 + time * .012), ruffle = vnoise(u * .0035 / U - time * .06, v * .035 + 7);
  if (lane > .6) col = mix(col, pal.waterLight, (lane - .6) * 1.1);
  if (ruffle < .38) col = mix(col, pal.trough, (.38 - ruffle) * (1.4 + gust));
  const sd = shadow(x, y); if (sd) col = mix(col, pal.deep, sd * .22);
  if (e < .0035 * sc[i]) col = mix(col, pal.trough, .45);       // dark right under the bank
  let l = (5 + R() * 13) * sz, w = (1.5 + R() * 2.3) * sz;
  if (k < .3) { col = mix(col, k < .17 ? pal.trough : pal.waterLight, .5); l *= .55; w = (.9 + R() * .6) * sz; }   // a ripple, drawn sharp
  else if (gust && k > .97 && d > .04) { col = mix(pal.waterLight, pal.hull, .6); l *= .35; w = (.9 + R() * .6) * sz; }   // in a storm the waves break
  if (orbK && rip > .44) {                                     // the sun lies on it, or the moon: a long glitter under it, broader towards us
    const q = (d - orbUp) / (orbUp * 1.3 + .08), p = (x - orbX) / ((.014 + d * .22) * PW * (orbUp > .2 ? 1.8 : 1)), g = orbK * Math.exp(-q * q - p * p);
    if (g > .03 && R() < g * 1.3) { col = mix(col, orbCol, .6 + .4 * g); l *= .5; w = (.9 + R() * .8) * sz; }
  }
  if (e * PH < l * Math.abs(sl[i]) + 3 * U && (y < wl[ci(x - l)] + U || y < wl[ci(x + l)] + U)) l *= .2;   // the bank runs aslant
  if (xf > FLOAT[0] - .03 && xf < FLOAT[1] + .03 && pf > FLOAT[2] - .004 && pf < FLOAT[3] + .004) l = Math.min(l, Math.max(U, (xf < FLOAT[0] ? FLOAT[0] - xf : xf - FLOAT[1]) * PW));   // not over the float
  PP.brush(x - l, y, x + l, y + (R() - .5) * U, w, col, { a: .85, jit: 6, dry: .15, sp: 1.1 * U * Math.max(1, s * .7), bow: .25 });
}

// ---------- every stroke where it belongs ----------
const STROKES = [null, builtStroke, treeStroke, poplarStroke, grassStroke, pathStroke, edgeStroke, stoneStroke, mooredStroke, dockStroke, waterStroke];
// The land is a narrow strip between the sky and the water: a stroke of it is looked for in one of a dozen
// upright slabs, each as high as the land is there.
let slabs = [], landArea = 0, bands = [];
function buildSlabs() {
  slabs = []; landArea = 0;
  const n = 12;
  for (let j = 0; j < n; j++) {
    const a = Math.floor(j * (cols - 1) / n), b = Math.floor((j + 1) * (cols - 1) / n);
    let y0 = H, y1 = 0, area = 0;
    for (let i = a; i < b; i++) { if (tops[i] < y0) y0 = tops[i]; if (wl[i] > y1) y1 = wl[i]; area += Math.max(0, Math.min(H, wl[i]) - Math.max(0, tops[i])); }
    y0 = Math.max(0, y0 - 2 * U); y1 = Math.min(H, y1);
    if (b > a && y1 > y0) { landArea += area; slabs.push({ a, b, y0, y1, at: landArea }); }
  }
  // the water in four bands, the far ones with small strokes and many of them
  bands = [];
  const edges = [SHORE, SHORE + .05, SHORE + .15, SHORE + .32, 9];
  for (let j = 0; j < 4; j++) {
    const ya = Math.max(0, py(edges[j])), yb = Math.min(H, py(edges[j + 1])), s = nearOf((Math.min(fy(H), edges[j + 1]) + edges[j]) / 2 - HOR);
    if (yb > ya) bands.push({ key: 'di-water' + j, n: W * (yb - ya) / (s * s), fn: () => { const R = PP.R, x = R() * W, y = ya + R() * (yb - ya); if (y >= wl[ci(x)] && at(x, y)) STROKES[zone(x, y)](x, y); } });
  }
}
function landStroke() {
  const R = PP.R, k = R() * landArea;
  let s = slabs[0]; if (!s) return;
  for (const q of slabs) { s = q; if (q.at >= k) break; }
  for (let n = 0; n < 3; n++) {
    const x = s.a + R() * (s.b - s.a), y = s.y0 + R() * (s.y1 - s.y0), i = ci(x);
    if (y < tops[i] - 2 * U || y >= wl[i]) continue;
    if (at(x, y)) { const z = zone(x, y); if (z && z !== WATER) STROKES[z](x, y); }
    return;
  }
}
function thinStroke() {                                        // what is too thin to be met by chance: the chimneys, the pylon
  const R = PP.R, b = pick(THIN, R()), x = px(b.c), y = py(b.y0) + R() * Math.max(0, tt[ci(x)] - py(b.y0));
  if (x < 0 || x >= W || !at(x, y) || zone(x, y) !== BUILDING || zb !== b) return;
  builtStroke(x, y);
}

// ---------- lights ----------
// The lamps along the path on the left bank, the lights of the boats at its end and the lamps of the far
// bridge: each a dab of light by night, and a long glitter on the water under it.  The island lies dark.
let lights = [];
function buildLights() {
  lights = [];
  const add = (xf, pf, kind, k) => { const x = px(xf); if (x > -10 * U && x < W + 10 * U) lights.push({ x, y: py(pf), kind, k, h: hash1(lights.length * 13 + 5) }); };
  for (let z = 5.6; z < 78; z += 4.6) {                        // evenly along the bank, which is 1 / z below eye level there
    let a = -1.6, b = .4633;
    for (let n = 0; n < 24; n++) { const m = (a + b) / 2; if (along(WATERLINE, m) - HOR > 1 / z) a = m; else b = m; }
    add(a, HOR + .67 / z, 0, clamp(.3 + 8 / z, .3, 1));
  }
  add(.4425, .3822, 0, .4); add(.458, .3832, 0, .35); add(.466, .3838, 1, .4); add(.474, .3843, 0, .35);
  for (let j = 0; j < 8; j++) add(.4935 + j * .0099, .3708, 2, .07);
}
const WHITE_LIGHT = [241, 246, 255];
const lightCol = l => l.kind === 1 ? WHITE_LIGHT : l.kind === 2 ? pal.lights[l.h < .5 ? 1 : 3] : pal.lights[l.h < .4 ? 0 : l.h < .7 ? 4 : 2];
function lampDab() {
  const R = PP.R, l = pick(lights, R());
  if (!l || !at(l.x, l.y) || !night) return;
  PP.haze = null;                                              // a light is seen through any fog
  if (R() < .35) PP.dab(l.x, l.y, (4 + 5 * l.k) * U * (1 + traits.fog * .8), lightCol(l), { a: .1 + traits.fog * .05, den: .4, s: 2.4 * U, jit: 3 });
  PP.dab(l.x, l.y, (.6 + .6 * l.k) * U, lightCol(l), lit());
}
function streak() {                                            // a lamp on the water: drawn out long, broken by the ripples
  const R = PP.R, l = pick(lights, R());
  if (!l || R() > .4 + l.k) return;                            // those far off lie on it but faintly
  const f = Math.pow(R(), 1.6), reach = (.012 + .2 * l.k) * PH, x = l.x + (R() - .5) * (3 * U + f * reach * .14), y = wl[ci(x)] + 2 * U + f * reach;
  if (x < 0 || x >= W || y >= H || !at(x, y) || !night || zone(x, y) !== WATER) return;
  const w = (2 + R() * 4 + f * 5) * U;
  glow(.5);
  PP.brush(x - w, y, x + w, y, (1 + R() * .8) * U, lightCol(l), { a: .6 * (1 - f) * (.5 + .5 * l.k), sp: U, bow: 0, jit: 4, dry: .1 });
}

// ---------- weather ----------
function fogStroke() {                                         // fog lies on the water in drifting banks
  const R = PP.R, x = R() * W, d = R() * .12 - .012, y = py(SHORE + d);
  if (y < wl[ci(x)] || !at(x, y) || !traits.fog) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) < .42 + Math.abs(d - .02) * 3) return;
  let l = (14 + R() * 30) * U;
  if (y < wl[ci(x - l)] || y < wl[ci(x + l)]) l *= .3;
  PP.haze = null;
  PP.brush(x - l, y, x + l, y + (R() - .5) * 2 * U, (3 + R() * 6) * U, fogCol(), { a: night ? .17 : .3, jit: 3, dry: .25, sp: 2 * U, bow: .03 });
}
function glintDab() {                                          // the sun sparkles on the water, and on the snow
  const R = PP.R, x = R() * W, y = hzY + R() * (H - hzY);
  if (!at(x, y) || !sun) return;
  const z = zone(x, y);
  PP.haze = null;
  if (z === WATER) { const sz = nearOf(zy - HOR) * U; if (vnoise(x * .01 / U + time * .1, y * .05 / U) > .56) PP.brush(x - 2.2 * sz, y, x + 2.2 * sz, y, .9 * sz, pal.sun, { a: .8, jit: 2, dry: 0, bow: 0, sp: Math.max(U, sz * .6) }); }
  else if (z === GRASS && traits.glint && R() < .5) PP.dab(x, y, (.7 + R() * .9) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x0, y0) {                                  // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 12 * U) continue;
    const xc = clamp(x, 0, W - 1), yc = Math.min(y, H - 1), i = ci(xc), k = zone(xc, yc), l = (14 + R() * 8) * U;
    if (k === WATER) {                                         // level, and no farther than the bank
      const d = Math.max(.003, zy - HOR), my = 2 * wl[i] - yc, e = l * 1.6, xa = Math.max(x - e, xc - Math.max(0, yc - wl[ci(xc - e)])), xb = Math.min(x + e, xc + Math.max(0, yc - wl[ci(xc + e)]));
      veil(flatOff(d) * .9);
      if (xb - xa > 4 * U) PP.brush(xa, y, xb, y, Math.min(26 * U, Math.max(6 * U, (yc - wl[i]) * 1.6)), mix(body(d), my < landTop ? skyCol(Math.max(0, my)) : pal.waterLight, .32), o);
    } else if (k === TREE || k === POPLAR) {
      const a = k === POPLAR ? Math.PI / 2 : (R() - .5) * 1.2, w = k === POPLAR ? Math.min(14 * U, zt.hw * 1.6 * PW) : Math.min(22 * U, (ft[i] - tt[i]) * .8), ll = Math.min(l, w * 1.5);
      veil(off(i));
      PP.brush(x - Math.cos(a) * ll, y - Math.sin(a) * ll, x + Math.cos(a) * ll, y + Math.sin(a) * ll, w, away(tone(k), off(i)), o);
    } else if (k === GRASS || k === STONE) {
      const w = Math.min(20 * U, (wl[i] - ft[i]) * .5);
      lie(i, zk); veil(off(i));
      PP.brush(x - dc * l, y - ds * l, x + dc * l, y + ds * l, w, away(tone(k), off(i)), o);
    }
  }
  if (y0 < landTop - 30 * U || y0 > landFoot + 30 * U) return;
  for (let n = 0; n < 18; n++) {                               // narrow strokes, close together: the far end of it all is a narrow thing
    const x = x0 + (n % 3 - 1 + (R() - .5) * .5) * 22 * U, y = y0 + ((n / 3 | 0) - 2.5 + (R() - .5) * .5) * 6.6 * U;
    if (x < -20 * U || x > W + 20 * U || y > H + 8 * U || y < 0) continue;
    const xc = clamp(x, 0, W - 1), i = ci(xc), k = zone(xc, Math.min(y, H - 1)), l = 13 * U, small = sc[i] < .5 || k === BUILDING || k === PATH || k === EDGE || k === MOORED || k === DOCK;
    if (!small || k === SKY || k === WATER || (k === BUILDING && zb.kind <= MAST)) continue;
    veil(off(i)); lie(i, k >= GRASS && k <= STONE ? zk : 0);
    PP.brush(x - dc * l, y - ds * l, x + dc * l, y + ds * l, 7 * U, away(tone(k), off(i)), o);
  }
}

// ---------- what moves ----------
// The cables of the pylon are too thin for the brush that paints the land: they are drawn anew every frame, from
// its head down to where the trees hide them.  At night it carries a red light, and so do the chimneys.
const HEAD = [.6993, .3376], BEACONS = [[.6993, .3326, .8], [.4767, .3387, 1], [.4767, .352, .4], [.4567, .3563, .5]];
let cables = [];
function buildCables() {
  cables = [];
  for (const side of [-1, 1]) for (let j = 0; j < 9; j++) {
    const ax = HEAD[0] + side * (.006 + j * (side < 0 ? .0058 : .0062)), ay = .371, x0 = px(HEAD[0]), y0 = py(HEAD[1] + j * .0004);
    let x1 = x0, y1 = y0;
    for (let t = .04; t <= 1; t += .04) { const x = px(HEAD[0] + (ax - HEAD[0]) * t), y = py(HEAD[1] + (ay - HEAD[1]) * t); if (y >= tops[ci(x)]) break; x1 = x; y1 = y; }
    if (y1 > y0 + 2) cables.push([x0, y0, x1, y1]);
  }
}
function drawCables(P, t) {
  const x = px(HEAD[0]), y = py(HEAD[1]);
  if (x > -80 * U && x < W + 80 * U && at(clamp(x, 0, W - 1), y)) {
    const c = night ? mix(pal.tower, pal.skyMid, .35) : mix(pal.tower, pal.skyLow, .3);
    P.haze = hazeOf(.8);
    P.R = rng(8200);
    for (const q of cables) P.line([[q[0], q[1]], [q[2], q[3]]], c, Math.max(.7, .6 * U), { pass: 1, a: .7, wob: .1 * U, step: 999, jit: 3 });
    P.brush(x, py(.3352), x, py(.3396), Math.max(1.8 * U, .0026 * PW), mix(pal.tower, pal.dark, .25), { a: .95, sp: U, dry: 0, jit: 3, bow: 0 });   // its head, where they all meet
    P.haze = null;
  }
  for (let i = 0; i < BEACONS.length; i++) {
    const b = BEACONS[i], bx = px(b[0]), by = py(b[1]);
    if (bx < 0 || bx >= W || !at(bx, by) || !night) continue;
    const fog = traits.fog > 0, a = fog ? .8 + .2 * Math.sin(t * .9 + i * 1.9) : b[2] < .7 ? .85 : Math.pow(Math.max(0, Math.sin(t * 2.1 + i * 2.4)), 2), s = Math.sqrt(b[2]);
    if (a < .05) continue;
    P.R = rng(8300 + i);                                       // each keeps its own shape while it glows
    P.dab(bx, by, (fog ? 11 : 7) * U * s, pal.beacon, { a: a * .34, den: .5, s: 2.4 * U, jit: 3 });
    P.dab(bx, by, 2 * U * s, fog ? mix(pal.beacon, pal.lights[2], .25) : pal.beacon, { a, den: 2.5, core: a, s: 1.1 * U, jit: 3 });
  }
}
// A ring on the water lies flat: wide, and not high.
function ring(P, x, y, r, a, w) {
  const pts = [];
  for (let n = 0; n <= 14; n++) { const an = n / 14 * TAU; pts.push([x + Math.cos(an) * r, y + Math.sin(an) * r * .3]); }
  P.line(pts, mix(pal.waterLight, pal.hull, night ? .25 : .75), w, { pass: 2, a, wob: .25 * U, step: 999, jit: 3 });
}
function drawRings(P, t, s) {                                  // now and then a fish jumps; in the rain every drop leaves its ring
  const onWater = (x, y) => { const xf = fx(x), pf = fy(y); return x > 0 && x < W && y < H && y > wl[ci(x)] + 6 * U && !(xf > FLOAT[0] - .01 && xf < FLOAT[1] + .01 && pf < FLOAT[3] + .01) && at(x, y); };
  if (s.traits.rain) {
    for (let i = 0; i < 26; i++) {
      const life = 1.1 + (i % 5) * .13, n = Math.floor(t / life + i * .37), u = t / life + i * .37 - n, d = .02 + Math.pow(hash1(n * 31 + i * 7), 1.3) * (fy(H) - HOR - .02), sz = nearOf(d) * U;
      const x = hash1(n * 17 + i * 101) * W, y = py(HOR + d);
      if (!onWater(x, y)) continue;
      P.haze = hazeOf(flatOff(d), .6); P.R = rng(8500 + i);
      ring(P, x, y, (2 + u * 14) * sz, (1 - u) * .9, Math.max(1.1, sz));
      if (u < .25) P.dab(x, y - sz, .9 * sz, pal.rain, { a: .9, den: 2, core: .8, s: U, jit: 2 });   // where it struck
    }
    P.haze = null;
    return;
  }
  const every = 19, n = Math.floor(t / every), u = (t - n * every) / 4.5;
  if (u > 1 || s.season === 'winter' || s.traits.gust) return;                                    // in winter they keep to the bottom
  const A = rng(n * 733 + 29), d = .05 + A() * .3, x = (.18 + A() * .64) * W, y = py(HOR + d), sz = nearOf(d) * U;
  if (!onWater(x, y)) return;
  P.haze = hazeOf(flatOff(d), .6); P.R = rng(8400);
  if (u < .07) for (let i = 0; i < 4; i++) P.dab(x + (A() - .5) * 5 * sz, y - A() * 5 * sz * (1 - u * 8), .9 * sz, mix(pal.waterLight, pal.hull, .5), { a: .9, den: 2, core: .8, s: U, jit: 3 });   // the splash
  for (let j = 0; j < 3; j++) { const q = u - j * .16; if (q > 0) ring(P, x, y, (3 + q * 26) * sz, (1 - u) * (1 - j * .2), Math.max(1.4 * U, 1.1 * sz)); }
  P.haze = null;
}

// ---------- the painting, one frame's worth ----------
const RATE = { land: 15000, water: 8000, thin: 8, lamp: 4, streak: 40, glint: 70, fog: 1500 };
const place = {
  colors: {
    deep: '#28507f', trough: '#1c3d66', grass: ['#3a6328', '#588a33', '#79a843', '#a3c659'], meadow: '#b4b162', reed: '#4f6230', reedLight: '#a3a660', blossom: ['#f6d84a', '#ffffff', '#f2e58a'],
    path: '#d2ccbc', pathDark: '#9c968a', concrete: '#d3cebf', concreteDark: '#6f6b63', rock: '#bcb8ac', rockDark: '#625f58',
    roofRed: '#b9573f', bridge: '#8f9196', floatBlue: '#3f63a8', snow: '#eef2f4', ice: '#cfdde6',
  },
  seasons: {
    spring: { deep: '#2c5886', trough: '#20466f', grass: ['#44772c', '#69a43a', '#8dc64c', '#bade68'], meadow: '#a4c956' },
    autumn: { deep: '#3a5067', trough: '#2a3d53', grass: ['#515f29', '#778338', '#9b9646', '#c2b25a'], meadow: '#c0a355', reed: '#6a6532', reedLight: '#c2a85c' },
    winter: { deep: '#39495b', trough: '#293847', grass: ['#474e30', '#676a42', '#878656', '#a8a370'], meadow: '#a3996c', reed: '#6b6440', reedLight: '#b9aa78' },
  },
  ground: /^(snow|ice)/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    // A wide screen loses some of the sky and of the water at our feet, an upright one is pushed together to half its width.
    const a = W / H, wide = clamp((a - 4 / 3) / .25), tall = clamp((4 / 3 - a) / .87), shown = .95 - .15 * wide - .05 * tall;
    Y0 = .02 + .04 * wide - .02 * tall; PH = H / shown; PW = PH * 4 / 3 * (1 - .5 * tall);
    const n = W / PW;                                          // how many photos wide the picture is
    X0 = n >= 1 ? .435 - n / 2 : -.065 + (1 - n) * .48;         // the poplars and the pylon stand as far from the middle on either side
    place.hz = py(SHORE);
    buildLand(); buildSlabs(); buildLights(); buildCables();
  },
  tick(t) { time = t; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // down to the trees, the roofs and the bridge
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
    const ua = U * U * 1e6;
    clipBand(landTop, H);
    spend('di-land', RATE.land * landArea / ua * pace, landStroke);
    for (const b of bands) spend(b.key, RATE.water * b.n / ua * pace, b.fn);
    spend('di-thin', RATE.thin * THIN.length * pace, thinStroke);
    if ((s && s.night) || (o && o.night)) { spend('di-lamp', RATE.lamp * lights.length * pace, lampDab); spend('di-streak', RATE.streak * lights.length * pace, streak); }
    if (s && !s.night && s.traits.shadow) spend('di-glint', RATE.glint * W * (H - place.hz) / ua * pace, glintDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('di-fog', RATE.fog * W * .12 * PH / ua * pace, fogStroke);
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
  sprites: [{ draw: drawRings, t0: 6, seed: 151 }, { draw: drawCables, t0: 3, seed: 152 }],
};
return place;
} });
