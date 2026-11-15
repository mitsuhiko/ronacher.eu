/* ------------------------------------------------------------------ *
 *  Vienna, in an Innenhof: the courtyard of an old house.  Two wings
 *  meet in a corner, an oriel hangs where they do; the low coach house
 *  on the right has kept its round gate.  A wisteria runs along the
 *  ledge and up between the windows, a table stands by the door to the
 *  stairs, and what was a garden once is ivy and ferns.  At night
 *  somebody has left a little lamp on the table.
 *
 *  It is painted after a photograph taken in autumn, measured by hand:
 *  where the two walls run to, how high their floors are, where every
 *  window sits.  The photograph is an upright one and shows no sky:
 *  the top floor, the eaves and the patch of sky over them are made
 *  up, and so is what lies beyond it on either side.  The leaves on
 *  the ground are there in autumn only.
 *  scene.js paints the sky and says what a place has to bring along.
 * ------------------------------------------------------------------ */
(window.viennaPlaces = window.viennaPlaces || []).push({ id: 'innenhof', name: 'Innenhof', make(kit) {
'use strict';
const { PP, TAU, clamp, smooth, mix, pick, rng, hash1, vnoise, fbm, wind, spend, clipBand } = kit;

// ---------- what the brush is looking at ----------
let W = 0, H = 0, U = 1, time = 0;          // the view (CSS pixels, the stroke unit) and the clock
let PW = 1, PH = 1;                         // the photo's width and height in pixels
let cur = null, own = null, pal = null, traits = null, night = false, season = '', bare = false, evening = false, white = 0;   // own: the picture's colours, pal: those at the point being painted
const use = s => {
  cur = s; own = pal = s.pal; traits = s.traits; night = s.night; season = s.season; bare = traits.bare; evening = s.weather === 'golden';
  white = traits.snow ? 1 : s.weather === 'frost' ? .3 : 0;    // how much snow lies: all of it, or a touch of frost
  return s;
};
const at = (x, y) => { const s = kit.at(x, y, place); return s && use(s); };   // the picture at a point, if it is one of this place

// ---------- geometry ----------
// Everything is measured on the photograph: x and y as fractions of its width and height.  Whatever is level on
// the left wall runs to a point far off on the right, on the right wall to one close by on the left; both lie on
// the horizon, which is half way up.  A level is known by how high it is on the left wall at the photograph's
// left edge, and is as high on the right wall where the two meet.
let X0 = 0, Y0 = 0;
const px = f => (f - X0) * PW, py = f => (f - Y0) * PH, fx = x => X0 + x / PW, fy = y => Y0 + y / PH;
const HOR = .5, VLX = 2.07, VRX = -.21, XC = .496;             // the horizon, the two points, the corner
const KR = (VLX - XC) / VLX / (XC - VRX);
const reachL = xf => (VLX - xf) / VLX, reachR = xf => (xf - VRX) * KR;
const levL = (xf, pf) => HOR + (pf - HOR) / reachL(xf), yL = (h, xf) => HOR + (h - HOR) * reachL(xf);
const levR = (xf, pf) => HOR + (pf - HOR) / reachR(xf), yR = (h, xf) => HOR + (h - HOR) * reachR(xf);
const WHITE = [255, 255, 255];
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;
const near = pf => clamp(.24 + (pf - .58) * 2.3, .24, 1.5);   // how close the ground is at this height: the size of its strokes
const inPoly = (p, x, y) => { let o = false; for (let i = 0, j = p.length - 1; i < p.length; j = i++) if ((p[i][1] > y) !== (p[j][1] > y) && x < (p[j][0] - p[i][0]) * (y - p[i][1]) / (p[j][1] - p[i][1]) + p[i][0]) o = !o; return o; };
function ramp(list, t) {                                       // a colour on a scale from its darkest to its lightest
  const f = clamp(t) * (list.length - 1), i = Math.min(list.length - 2, f | 0);
  return mix(list[i], list[i + 1], f - i);
}

// levels: the gutter; the top floor's windows; the string course under them; the first floor's windows, the ledge
// under that floor; the ground floor's windows; the plinth and the foot of the wall
const EAVE = -.17, F2T = -.118, F2B = -.002, STRING = 0, F1T = .1312, F1B = .272, CORN = .329, G0T = .41, G0B = .53, PLINTH = .557, FOOT = .589;
const ARCH0 = .118, ARCH = .0395;                              // the blind arches over the first floor's windows: where they spring from, how high they rise
// The coach house stands in front of the right wall, from LX0 on: its roof, the line its gate's arch springs from, its plinth and foot.
const LX0 = .606, LOWTOP = .2652, SPRING = .4363, LOWPLINTH = .5675, LOWFOOT = .5959, GATE = [.7084, .955], GCX = .8317, GHW = .1233, GRISE = .0734;
const eaveY = xf => xf < XC ? yL(EAVE, xf) : yR(EAVE, xf);
const footY = xf => xf < XC ? yL(FOOT, xf) : xf < LX0 ? yR(.586, xf) : yR(LOWFOOT, xf);

// ---------- the walls ----------
// What a wall is made of at a point.  The stuff says which way its strokes go too: level (0), upright (1) or some
// other way (2: sx, sy).  A window has a number of its own, and says where in it the point lies.
const WALL = 1, NICHE = 2, EDGE = 3, BAND = 4, SHADE = 5, ZINC = 6, SILL = 7, FRAME = 8, GLASS = 9, GRILLE = 10, STUCCO = 11, HOLE = 12,
  DOOR = 13, DPANEL = 14, DGLASS = 15, BRASS = 16, STONE = 17, JOINT = 18, RING = 19, VENT = 20, COVE = 21, FLAT = 22, PIPE = 23;
let dir = 0, sx = 1, sy = 0, wu = 0, wv = 0, wk = 0, bs = 0;   // bs: how steep a level line of the oriel is here
function pane(u, v, bar, cross, k) {                           // a casement window: two wings, a transom, a bar across the lower lights
  wu = u; wv = v; wk = k;
  if (u < .075 || u > .925 || Math.abs(u - .5) < .05) { dir = 1; return FRAME; }
  if (v < .06 || v > .945 || Math.abs(v - bar) < .028 || (cross && v > bar && Math.abs(v - cross) < .012)) return FRAME;
  dir = 1; return GLASS;
}
// The left wall: one bay like the other, evenly spaced along it, which is how 1 / (VLX - x) counts.  Bay 0 is the
// one by the corner, with the door to the stairs; from the fourth on they lie beyond the photograph.
const A0 = .6252, BAYW = .0567, WIN = .0288, RIM = .0035;
function leftDoor(u, v) {                                      // six panes over a panel
  u = 1 - u; wu = u; wv = v; wk = 300; dir = 1;
  if (u > .19 && u < .84 && v > .09 && v < .63) {
    if (Math.abs(u - .515) < .024) return DOOR;
    const b = (v - .09) / .18;
    if (b > .5 && b < 2.5 && Math.abs(b - Math.round(b)) < .07) { dir = 0; return DOOR; }
    return DGLASS;
  }
  if (u > .2 && u < .83 && v > .7 && v < .9) return DPANEL;
  if (Math.abs(u - .135) < .035 && Math.abs(v - .585) < .02) return BRASS;
  return DOOR;
}
function eaves(h) { return h < EAVE + .007 ? ZINC : h < EAVE + .03 ? BAND : h < EAVE + .036 ? SHADE : 0; }
function leftAt(xf, h) {
  dir = 0;
  let m = eaves(h); if (m) return m;
  const u = A0 - 1 / (VLX - xf), n = Math.floor((u + (BAYW - WIN) / 2) / BAYW), t = u - n * BAYW, q = t / WIN;
  const inWin = t >= 0 && t <= WIN, inRim = t >= -RIM && t <= WIN + RIM;
  if (n === 3 && Math.abs(t + .0122) < .0009) { dir = 1; return PIPE; }   // the downpipe, beyond the photograph's edge
  if (h < STRING) {
    if (inWin && h > F2T && h < F2B) return pane(q, (h - F2T) / (F2B - F2T), .37, .7, n * 4 + 2);
    if (inRim && h > F2T - .009) { if (!inWin) dir = 1; return FLAT; }
    return WALL;
  }
  if (h < STRING + .012) return h < STRING + .008 ? BAND : SHADE;
  if (h < CORN) {
    if (inRim) {                                               // a tall niche with a round head, the window in it
      const qa = (t - WIN / 2) / (WIN / 2 + RIM), top = ARCH0 - ARCH * Math.sqrt(Math.max(0, 1 - qa * qa));
      if (h > top) {
        if (inWin) {
          if (h > F1T && h < F1B) return pane(q, (h - F1T) / (F1B - F1T), .385, .7, n * 4 + 1);
          if (h > F1T - .006 && h <= F1T) return ZINC;
          if (h >= F1B && h < F1B + .008) return SILL;
        }
        if (h - top < .0045) return EDGE;
        if (t < -RIM + .0011) { dir = 1; return EDGE; }
        return NICHE;
      }
    }
    return WALL;
  }
  if (h < CORN + .004) return ZINC;
  if (h < CORN + .016) return BAND;
  if (h < CORN + .021) return SHADE;
  if (n === 0) {                                               // the door to the stairs
    if (t > -.0014 && t < .0293 && h > .399) return leftDoor((t + .0014) / .0307, (h - .399) / (FOOT - .399));
    if (t > -.0045 && t < .0325 && h > .39 && h < PLINTH) { dir = h > .399 ? 1 : 0; return FLAT; }
  } else if (h < PLINTH) {
    if (inWin) {
      if (h > G0T && h < G0B) {                                // bars keep the ground floor's windows
        m = pane(q, (h - G0T) / (G0B - G0T), .34, .68, n * 4);
        if (m === GLASS) { const b = q * 11.5; if (Math.abs(b - Math.round(b)) < .1) return GRILLE; }
        return m;
      }
      if (h >= G0B && h < G0B + .007) return SILL;
      if (h > .5445 && t > .0046 && t < .0221) return HOLE;    // and a hole into the cellar
    }
    if (inRim && h > G0T - .01 && h < G0B + .011) { if (!inWin) dir = 1; return FLAT; }
  }
  if (h < PLINTH) return WALL;
  dir = 1; return STUCCO;
}
// The right wall: a window over the oriel and one over the coach house's roof, in a niche like those on the left;
// down in the corner, under the oriel, a window of the ground floor.
const RC = [[.525, .588], [.777, .897]];
function rightAt(xf, h) {
  dir = 0;
  let m = eaves(h); if (m) return m;
  if (h < STRING) {
    for (let i = 0; i < 2; i++) {
      const c = RC[i], q = (xf - c[0]) / (c[1] - c[0]);
      if (q >= 0 && q <= 1 && h > F2T && h < F2B) return pane(q, (h - F2T) / (F2B - F2T), .37, .7, 102 + i * 4);
      if (q > -.1 && q < 1.1 && h > F2T - .009) { if (q < 0 || q > 1) dir = 1; return FLAT; }
    }
    return WALL;
  }
  if (h < STRING + .012) return h < STRING + .008 ? BAND : SHADE;
  if (xf > .762 && xf < .9135 && h < F1B + .012) {
    const qa = (xf - .8377) / .0758, top = .1207 - .05 * Math.sqrt(Math.max(0, 1 - qa * qa));
    if (h > top) {
      if (xf > .777 && xf < .897) {
        if (h > .1228 && h < F1B) return pane((xf - .777) / .12, (h - .1228) / (F1B - .1228), .385, .7, 105);
        if (h > .1165 && h <= .1228) return ZINC;
        if (h >= F1B) return SILL;
      }
      if (h - top < .005) return EDGE;
      if (xf < .7635) { dir = 1; return EDGE; }
      return NICHE;
    }
  }
  if (xf < LX0) {
    if (xf > .523 && xf < .584) {
      if (h > .4087 && h < .525) {
        const q = (xf - .523) / .061;
        m = pane(q, (h - .4087) / .1163, .34, .68, 100);
        if (m === GLASS) { const b = q * 11.5; if (Math.abs(b - Math.round(b)) < .1) return GRILLE; }
        return m;
      }
      if (h >= .525 && h < .532) return SILL;
    }
    if (xf > .517 && xf < .59 && h > .398 && h < .537) { if (xf < .523 || xf > .584) dir = 1; return FLAT; }
    if (xf > .54 && xf < .562 && h > .558 && h < .583) return HOLE;
    if (h > .5568) { dir = 1; return STUCCO; }
  }
  return WALL;
}
// The coach house: its gate is as wide as GATE says and round above SPRING.  Stones stand round the arch.
function gateAt(xf, h, pf, q, e) {
  dir = 0; wk = 400;
  if (e > 0) {                                                 // the fan light: spokes from a small half round
    const r = Math.hypot(q, e), a = Math.atan2(e, q);
    dir = 2;
    if (r > .9 || Math.abs(r - .34) < .04) { sx = -Math.sin(a) * GHW * PW; sy = -Math.cos(a) * GRISE * PH; return DOOR; }
    sx = Math.cos(a) * GHW * PW; sy = -Math.sin(a) * GRISE * PH;
    if (r > .34) { const k = a / (Math.PI / 7); if (Math.abs(k - Math.round(k)) * (Math.PI / 7) * r < .03) return DOOR; }
    else if (Math.abs(q) < .022) return DOOR;
    if (e < .07) { dir = 0; return DOOR; }
    wu = q; wv = e; dir = 1; return DGLASS;
  }
  if (h < SPRING + .011) return DOOR;                          // the transom
  const i = xf < .7735 ? 0 : xf < .8585 ? 1 : 2, g0 = i === 0 ? .7246 : i === 1 ? .7855 : .873, g1 = i === 0 ? .764 : i === 1 ? .848 : .9295;   // three wings
  wk = 401 + i;
  if (xf > g0 && xf < g1) {
    if (h > .452 && h < .5453) {                               // four panes each
      const b = (h - .452) / .0233;
      if (b > .5 && b < 3.5 && Math.abs(b - Math.round(b)) < .09) return DOOR;
      wu = (xf - g0) / (g1 - g0); wv = (h - .452) / .0933; dir = 1; return DGLASS;
    }
    if (h > .5535 && h < .589) { dir = 1; return DPANEL; }
  }
  if (Math.hypot((xf - .8635) * PW, (pf - yR(.515, xf)) * PH) < .0045 * PW) return BRASS;
  dir = 1;
  if (Math.abs(xf - .7735) < .0011 || Math.abs(xf - .8585) < .0011) return SHADE;
  return DOOR;
}
function lowAt(xf, pf) {
  const h = levR(xf, pf);
  dir = 0;
  if (h < LOWTOP + .004) return ZINC;
  if (h < LOWTOP + .016) return BAND;
  if (h < LOWTOP + .021) return SHADE;
  if (xf < .618) {                                             // its end, turned towards us: a narrow strip, a round hole in it
    if (Math.hypot((xf - .6105) * PW, (pf - .457) * PH) < .0062 * PW) return VENT;
    dir = 1; return h > LOWPLINTH ? STUCCO : NICHE;
  }
  const spring = yR(SPRING, xf), q = (xf - GCX) / GHW, e = (spring - pf) / GRISE;
  if (xf > GATE[0] && xf < GATE[1] && (e <= 0 ? h < LOWFOOT : q * q + e * e < 1)) return gateAt(xf, h, pf, q, e);
  if (e > -.03) {
    const r = Math.hypot(q, Math.max(0, e));
    if (r < 1.62) {
      const a = Math.atan2(Math.max(0, e), q);
      dir = 2;
      if (r < 1.1 && e > 0) { sx = -Math.sin(a) * GHW * PW; sy = -Math.cos(a) * GRISE * PH; return RING; }
      if (e > 0) {
        sx = Math.cos(a) * GHW * PW; sy = -Math.sin(a) * GRISE * PH;
        const k = a / (Math.PI / 13);
        if (Math.abs(k - Math.round(k)) * (Math.PI / 13) * r * GHW * PW < .8 * U) return JOINT;
        if (r > 1.6) { sx = -Math.sin(a) * GHW * PW; sy = -Math.cos(a) * GRISE * PH; return JOINT; }
        wk = Math.round(k + .5);
        dir = 0; return STONE;
      }
    }
  }
  dir = 0;
  if (h > .431 && h < .449 && (xf < GATE[0] || xf > GATE[1]) && xf < 1.08) return h < .444 ? BAND : SHADE;   // what the arch stands on
  if (h > LOWPLINTH) { dir = 1; return STUCCO; }
  return WALL;
}
// The oriel hangs on the right wall, where the first floor's window would be: its front runs along that wall, its
// side looks at us.  A tin roof, a moulded foot, two brackets under it.
const bayTop = xf => xf < .568 ? .1865 - (xf - .481) * .5287 : .1405 + (xf - .568) * .141;
const bayRoof = xf => xf < .568 ? bayTop(xf) - .017 : .1235 + (xf - .568) * .09;
const bayFoot = xf => xf < .568 ? .3216 - (xf - .481) * .3103 : .2946 + (xf - .568) * .0615;
const baySlab = xf => xf < .566 ? .365 - (xf - .469) * .169 : .3486 + (xf - .566) * .064;
const FRONT = 1, SIDE = 2, ROOF = 3, BASE = 4, BRACKET = 5;
function bayAt(xf, pf) {
  if (xf < .466 || xf > .618 || pf < .11 || pf > .405) return 0;
  if (pf < bayRoof(xf)) return 0;
  if (pf < bayTop(xf) + .002) return ROOF;
  if (xf >= .481 && xf < .607 && pf < bayFoot(xf)) return xf < .568 ? FRONT : SIDE;
  if (xf < .47 || xf > .61) return 0;
  const slab = baySlab(xf);
  if (pf < slab) return pf > bayFoot(Math.max(xf, .481)) - .001 ? BASE : 0;
  if (xf > .478 && xf < .522 && pf < slab + .004 + .034 * (1 - smooth(.25, 1, (xf - .478) / .044))) return BRACKET;
  if (xf > .552 && xf < .6 && pf < slab + .004 + .036 * smooth(0, .75, (xf - .552) / .048)) return BRACKET;
  return 0;
}
function bayMat(xf, pf) {
  const part = bayAt(xf, pf);
  dir = 0; bs = 0;
  if (part === ROOF) { bs = xf < .568 ? -.5287 : .09; return ZINC; }
  if (part === FRONT) {
    const u = (xf - .481) / .087, top = bayTop(xf), v = (pf - top) / (bayFoot(xf) - top);
    bs = -.5287 + (.5287 - .3103) * v;
    if (v < .035) return SHADE;
    if (u > .15 && u < .795 && v > .145 && v < .94) return pane((u - .15) / .645, (v - .145) / .795, .38, .7, 200);
    if (u > .1 && u < .845 && v > .11 && v < .975) { if (u < .15 || u > .795) dir = 1; return FLAT; }
    return WALL;
  }
  if (part === SIDE) {
    const u = (xf - .568) / .039, top = bayTop(xf), v = (pf - top) / (bayFoot(xf) - top);
    bs = .141 + (.0615 - .141) * v;
    if (u < .06) { dir = 1; return EDGE; }
    if (u > .41 && u < .9 && v > .14 && v < .96) {             // a narrow window, one wing
      const a = (u - .41) / .49, b = (v - .14) / .82;
      wu = a; wv = b; wk = 201;
      if (a < .2 || a > .8) { dir = 1; return FRAME; }
      if (b < .03 || b > .97 || Math.abs(b - .36) < .018) return FRAME;
      dir = 1; return GLASS;
    }
    return NICHE;
  }
  if (part === BASE) {
    const foot = bayFoot(Math.max(xf, .481)), w = (pf - foot) / (baySlab(xf) - foot);
    bs = xf < .567 ? -.3103 + (.3103 - .169) * w : .063;
    return w < .2 ? BAND : w < .3 ? SHADE : w < .86 ? COVE : BAND;
  }
  dir = 1;
  return pf > baySlab(xf) + .027 ? SHADE : COVE;
}

// ---------- what stands about ----------
// The table, a chair and a bench at it, cushions against the wall, two pots, a lid, lanterns: each a patch of the
// photograph with a stuff of its own, those in front last.  slats: it is made of that many, with gaps between.
const WOOD = 1, TEAK = 2, BOARDS = 3, CRATE = 4, TOP = 5, RIMW = 6, CLOTH = 7, PAD = 8, POT = 9, CLAY = 10, SOIL = 11, LID = 12, IRON = 13, LAMPGLASS = 14, ROCK = 15, ENAMEL = 16, RAG = 17, LEAFY = 18;
const box = (a, b, c, d) => [[a, b], [c, b], [c, d], [a, d]];
const THINGS = [
  { k: CLOTH, p: [[.2165, .554], [.22, .5495], [.2445, .5485], [.247, .551], [.2495, .548], [.2715, .547], [.275, .551], [.276, .58], [.218, .582]], up: 1 },
  { k: WOOD, p: box(.2715, .5765, .2755, .597), up: 1 },
  { k: WOOD, p: [[.272, .5455], [.321, .558], [.321, .588], [.272, .5765]], slats: 5 },
  { k: WOOD, p: [[.3225, .581], [.371, .572], [.371, .5765], [.3225, .5855]] },
  { k: PAD, p: [[.322, .5745], [.369, .566], [.3715, .572], [.3245, .581]] },
  { k: WOOD, p: [[.321, .5625], [.369, .5545], [.369, .558], [.321, .566]] },
  { k: WOOD, p: box(.3215, .5855, .3255, .603), up: 1 }, { k: WOOD, p: box(.367, .5765, .371, .594), up: 1 },
  { k: TEAK, p: box(.469, .5745, .473, .6), up: 1 }, { k: TEAK, p: box(.5265, .579, .5305, .598), up: 1 },
  { k: TEAK, p: [[.468, .5357], [.527, .5415], [.527, .572], [.468, .567]], slats: 4 },
  { k: TEAK, p: [[.468, .567], [.527, .572], [.531, .579], [.471, .5745]] },
  { k: TEAK, p: box(.4825, .556, .486, .6), up: 1 }, { k: TEAK, p: [[.4825, .556], [.527, .561], [.527, .564], [.4825, .559]] },
  { k: CRATE, p: [[.335, .547], [.401, .5624], [.401, .599], [.335, .591]], up: 1 },
  { k: BOARDS, p: [[.401, .5624], [.467, .553], [.467, .5956], [.401, .599]], up: 1 },
  { k: RIMW, p: [[.335, .545], [.401, .5604], [.401, .5636], [.335, .5482]] }, { k: RIMW, p: [[.401, .5604], [.467, .551], [.467, .5542], [.401, .5636]] },
  { k: TOP, p: [[.335, .545], [.4, .541], [.467, .551], [.401, .5604]], d: [.066, .0154] },
  { k: POT, p: [[.3265, .604], [.3905, .604], [.3865, .639], [.3315, .639]], up: 1 },
  { k: SOIL, e: [.3585, .604, .032, .0055] },
  { k: LEAFY, e: [.36, .577, .031, .03] },
  { k: CLAY, p: [[.392, .608], [.4535, .608], [.449, .6435], [.397, .6435]], up: 1 },
  { k: SOIL, e: [.4228, .608, .0295, .006] },
  { k: LID, e: [.469, .62, .0165, .0175] },
  { k: IRON, p: box(.3775, .6125, .4005, .65), up: 1 }, { k: LAMPGLASS, p: box(.3815, .62, .3965, .644), up: 1 },
  { k: ENAMEL, p: box(.5505, .563, .558, .58), up: 1 }, { k: IRON, p: box(.56, .558, .584, .585), up: 1 }, { k: IRON, p: box(.586, .559, .609, .587), up: 1 },
  { k: RAG, p: [[.596, .556], [.609, .558], [.607, .572], [.598, .568]] },
  { k: ROCK, p: [[.628, .595], [.652, .594], [.653, .607], [.629, .608]] },
];
for (const t of THINGS) {
  const p = t.e ? [[t.e[0] - t.e[2], t.e[1] - t.e[3]], [t.e[0] + t.e[2], t.e[1] + t.e[3]]] : t.p;
  t.x0 = Math.min(...p.map(q => q[0])); t.x1 = Math.max(...p.map(q => q[0])); t.y0 = Math.min(...p.map(q => q[1])); t.y1 = Math.max(...p.map(q => q[1]));
}
function thingAt(xf, pf) {
  for (let i = THINGS.length - 1; i >= 0; i--) {
    const t = THINGS[i];
    if (xf < t.x0 || xf > t.x1 || pf < t.y0 || pf > t.y1) continue;
    if (t.e) {
      if (Math.hypot((xf - t.e[0]) / t.e[2], (pf - t.e[1]) / t.e[3]) > 1) continue;
      if (t.k === LEAFY && (bare || vnoise(xf * 300, pf * 300) < .5)) continue;
    } else {
      if (!inPoly(t.p, xf, pf)) continue;
      if (t.slats) {
        const p = t.p, f = (xf - p[0][0]) / (p[1][0] - p[0][0]), a = p[0][1] + (p[1][1] - p[0][1]) * f, b = p[3][1] + (p[2][1] - p[3][1]) * f, v = (pf - a) / (b - a) * t.slats;
        if (v - Math.floor(v) > .6 && f > .07 && f < .93) continue;
      }
    }
    return i + 1;
  }
  return 0;
}
const LAMP = [.404, .5488];                                    // where the little lamp stands on the table

// ---------- the wisteria ----------
// It comes up the wall beyond the photograph's left edge, runs along the ledge and climbs on between the windows
// to the eaves, twice.  Leaves are a heap of crowns along it; both are kept in coarse grids: where its limbs are,
// and how deep in the leaves a point is.
const LEADER = [[.262, .362], [.298, .3226], [.324, .307], [.3414, .2836], [.3466, .256], [.3536, .221], [.3605, .182], [.3674, .1457], [.3726, .112], [.378, .078],
  [.3726, .039], [.3605, .0104], [.356, -.018], [.36, -.04]];
const LEADER2 = [[-.342, .35], [-.356, .3], [-.348, .26], [-.33, .225], [-.336, .19], [-.35, .16]];
let limbs = [], limbWeight = 0, limbLength = 0, mask = null, mCols = 0, mRows = 0, mCell = 2;
let crown = null, cCols = 0, cRows = 0, cCell = 4, cX = 0, cY = 0, leafy = [], leafArea = 0;
let vx0 = 0, vx1 = 0, vy0 = 0, vy1 = 0;                        // the box it grows in
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
const maskAt = (x, y) => x < 0 || y < 0 || x >= W || y >= H ? 0 : mask[((y / mCell) | 0) * mCols + ((x / mCell) | 0)];
function buildVine() {
  const r = rng(41), crowns = [], P = q => [px(q[0]), py(q[1])], wd = Math.max(1.8 * U, .0062 * PW), thin = Math.max(.8 * U, wd * .24);
  limbs = [];
  const seg = (a, b, w, bend, depth) => {
    const len = Math.hypot(b[0] - a[0], b[1] - a[1]) || 1, an = Math.atan2(b[1] - a[1], b[0] - a[0]);
    limbs.push({ x1: a[0], y1: a[1], x2: b[0], y2: b[1], cx: (a[0] + b[0]) / 2 - Math.sin(an) * len * bend, cy: (a[1] + b[1]) / 2 + Math.cos(an) * len * bend, w, depth, len });
  };
  const stem = (pts, w0, w1) => { for (let i = 0; i < pts.length - 1; i++) seg(P(pts[i]), P(pts[i + 1]), w0 + (w1 - w0) * i / (pts.length - 1), (r() - .5) * .16, 0); };
  const tuft = (x, y, rad) => crowns.push({ x, y, r: rad * PW });
  const cord = [];
  for (let x = -.31; x < .435; x += .045) cord.push([x, yL(CORN + .011, x) + (vnoise(x * 23, 1.3) - .5) * .009]);
  const ROOT = [[-.388, .618], [-.374, .57], [-.381, .51], [-.362, .45], [-.356, .395], [-.342, .35]];
  stem(ROOT.concat([cord[0]]), wd * 1.4, wd);
  stem(ROOT.map((q, i) => [q[0] + Math.sin(i * 2.4 + 1) * .008 + .004, q[1] + .01]), wd * .5, wd * .35);
  stem(cord, wd, wd * .5);
  stem(LEADER, wd * .9, wd * .4);
  stem(LEADER.map((q, i) => [q[0] + Math.sin(i * 2.1) * .007, q[1] + .012]), wd * .4, wd * .25);   // it winds round itself
  stem(LEADER2, wd * .75, wd * .35);
  const hang = (a, sd, l, k) => {                              // a shoot: out to one side, then down, leaves all along it and a bunch at its end
    const b = [a[0] + sd * l * (.25 + r() * .7), a[1] + l * (r() * .75 - .12)];
    seg(a, b, thin, -sd * (.1 + r() * .45), 2);
    for (let f = .35; f < 1.05; f += .22 + r() * .12) tuft(a[0] + (b[0] - a[0]) * f + (r() - .5) * l * .2, a[1] + (b[1] - a[1]) * f + (r() - .2) * l * .2, (.011 + r() * .014) * k);
    if (r() < .6) tuft(b[0] + sd * .01 * PW, b[1] + .02 * PH * k, (.011 + r() * .012) * k);
  };
  for (let x = -.3; x < .44; x += .022 + r() * .045) {          // what hangs from the ledge, and last year's tangle
    const a = [px(x), py(yL(CORN + .012, x))];
    hang(a, r() < .5 ? -1 : 1, (.014 + r() * r() * .06) * PH, .85);
    if (r() < .5) tuft(a[0] + (r() - .5) * .02 * PW, a[1] + (r() - .5) * .008 * PH, .009 + r() * .011);
    for (let n = r() * 2.4 | 0; n > 0; n--) seg(a, [a[0] + (r() - .5) * .07 * PW, a[1] + (r() - .2) * .04 * PH], .7 * U, (r() - .5) * 1.4, 3);
  }
  const shoots = (pts, k) => {                                 // and what the leaders send out to either side
    for (let i = 1; i < pts.length; i++) for (let f = 0; f < 1; f += .5) {
      const p = pts[i - 1], q = pts[i], a = [px(p[0] + (q[0] - p[0]) * f), py(p[1] + (q[1] - p[1]) * f)];
      for (let n = r() * 2.6 | 0; n > 0; n--) hang(a, r() < .5 ? 1 : -1, (.02 + r() * r() * .1) * PW * k, k);
      if (r() < .7) tuft(a[0] + (r() - .5) * .02 * PW, a[1], .012 + r() * .012);
    }
    const e = P(pts[pts.length - 1]);
    for (let n = 0; n < 4; n++) tuft(e[0] + (r() - .5) * .05 * PW, e[1] + r() * .03 * PH, .016 + r() * .012);
  };
  shoots(LEADER, 1); shoots(LEADER2, .8); shoots(ROOT.slice(2), .6);
  let x0 = 1e9, x1 = -1e9, y0 = 1e9, y1 = -1e9;
  for (const c of crowns) { x0 = Math.min(x0, c.x - c.r); x1 = Math.max(x1, c.x + c.r); y0 = Math.min(y0, c.y - c.r); y1 = Math.max(y1, c.y + c.r); }
  vx0 = x0 - 12 * U; vx1 = x1 + 12 * U; vy0 = y0 - 12 * U; vy1 = Math.max(y1 + 12 * U, py(.63));
  // how deep in the leaves: 1 in the middle of a crown, 0 at its edge
  cCell = 2.5 * U; cX = Math.max(vx0, -20 * U); cY = Math.max(vy0, -20 * U);
  cCols = Math.max(1, Math.ceil((Math.min(vx1, W + 20 * U) - cX) / cCell) + 1); cRows = Math.max(1, Math.ceil((Math.min(vy1, H + 20 * U) - cY) / cCell) + 1);
  crown = new Float32Array(cCols * cRows).fill(-1); leafy = []; leafArea = 0;
  for (const c of crowns) {
    if (c.x + c.r < 0 || c.x - c.r > W || c.y + c.r < 0 || c.y - c.r > H) continue;
    c.at = leafArea; leafArea += c.r * c.r; leafy.push(c);
    const i0 = Math.max(0, Math.floor((c.x - c.r - cX) / cCell)), i1 = Math.min(cCols - 1, Math.ceil((c.x + c.r - cX) / cCell));
    const j0 = Math.max(0, Math.floor((c.y - c.r - cY) / cCell)), j1 = Math.min(cRows - 1, Math.ceil((c.y + c.r - cY) / cCell));
    for (let j = j0; j <= j1; j++) for (let i = i0; i <= i1; i++) {
      const q = 1 - Math.hypot(cX + (i + .5) * cCell - c.x, (cY + (j + .5) * cCell - c.y - c.r * .2) * .72) / c.r;
      if (q > crown[j * cCols + i]) crown[j * cCols + i] = q;
    }
  }
  // where the limbs are
  mCell = 1.5 * U; mCols = Math.ceil(W / mCell) + 1; mRows = Math.ceil(H / mCell) + 1;
  mask = new Uint8Array(mCols * mRows); limbWeight = limbLength = 0;
  for (const s of limbs) {
    if (Math.max(s.x1, s.x2) < -40 * U || Math.min(s.x1, s.x2) > W + 40 * U || Math.max(s.y1, s.y2) < -40 * U || Math.min(s.y1, s.y2) > H + 40 * U) { s.wt = 0; continue; }
    s.wt = s.len * (s.w + 2 * U); limbWeight += s.wt; limbLength += s.len;
    const rad = s.w / 2 + .3 * U, m = Math.ceil(rad / mCell);
    for (let i = 0, n = Math.ceil(s.len / (mCell * .5)); i <= n; i++) {
      const ix = Math.floor(limbX(s, i / n) / mCell), iy = Math.floor(limbY(s, i / n) / mCell);
      for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
        const cx = ix + dx, cy = iy + dy;
        if (cx >= 0 && cy >= 0 && cx < mCols && cy < mRows && Math.hypot(dx, dy) * mCell <= rad + mCell * .5) mask[cy * mCols + cx] = 1;
      }
    }
  }
}
const inVine = (x, y) => x > vx0 && x < vx1 && y > vy0 && y < vy1;
function fol(x, y) {                                           // > 0 inside the leaves
  const i = ((x - wind(time) * 2 * U - cX) / cCell) | 0, j = ((y - cY) / cCell) | 0;
  if (i < 0 || j < 0 || i >= cCols || j >= cRows) return -1;
  const q = crown[j * cCols + i];
  if (q <= -1) return -1;
  return (q - (season === 'autumn' ? .3 : season === 'spring' ? .2 : .06)) * 1.2 + (fbm(x * .04 / U + time * .02, y * .04 / U) - .47) * .7 + (vnoise(x * .23 / U, y * .23 / U) - .5) * .5;   // in autumn half the leaves are down
}
function folLum(x, y) {                                        // light falls in from above
  const s = time * .03, k = .025 / U;
  return .5 + (fbm(x * k + s, y * k * 1.2) - fbm((x + 5 * U) * k + s, (y - 9 * U) * k * 1.2)) * 3.2;
}

// ---------- the ferns ----------
// Each grows from a heart: [x, y, how long its fronds are, how many].  A frond is an arching rib with a comb of
// leaflets on either side.  A coarse grid says which frond a point belongs to: the one nearest to us that covers it.
const HEARTS = [[.75, .672, .1, 7], [.845, .715, .15, 9], [.955, .752, .17, 9], [1.09, .72, .15, 8], [1.21, .78, .17, 8], [.57, .875, .2, 11], [.31, .955, .2, 10],
  [.09, 1.03, .22, 9], [.78, 1, .21, 10], [1, .99, .2, 9], [.45, 1.08, .2, 8], [-.16, .99, .22, 9], [-.42, .8, .13, 8], [-.8, .86, .16, 9], [.2, .84, .15, 9], [-.08, .87, .16, 8], [.42, .8, .1, 7]];
let fronds = [], grid = null, gCols = 0, gRows = 0, gCell = 2, gY = 0, fernWeight = 0;
const frondW = (f, t) => t <= .06 || t >= 1 ? 0 : f.w * Math.pow(Math.sin(Math.PI * Math.pow(t, .72)), .8);
function buildFerns() {
  const list = HEARTS.slice();
  for (let i = 1, x = -1.05; x > X0 - .3; i++, x -= .2 + hash1(i * 13) * .14) list.push([x, .88 + hash1(i * 7) * .16, .17 + hash1(i * 3) * .05, 9]);
  for (let i = 1, x = 1.4; x < fx(W) + .3; i++, x += .2 + hash1(i * 29) * .14) list.push([x, .82 + hash1(i * 11) * .2, .17 + hash1(i * 5) * .05, 9]);
  list.sort((a, b) => a[1] - b[1]);
  fronds = []; fernWeight = 0;
  let top = H;
  for (const h of list) {
    const r = rng(900 + Math.round(h[0] * 977 + h[1] * 131)), bx = px(h[0]), by = py(h[1]), L = h[2] * PW, n = h[3];
    if (bx < -L * 1.3 || bx > W + L * 1.3 || by - L > H) continue;
    for (let i = 0; i < n; i++) {
      const a = ((i + .5) / n - .5) * 2.7 + (r() - .5) * .3, len = L * (.75 + r() * .45) * (1 - Math.abs(a) * .1), sa = Math.sin(a), ca = Math.cos(a);
      const f = { x0: bx, y0: by, x1: bx + sa * len * .5, y1: by - ca * len * .78 - len * .06, x2: bx + sa * len * 1.02, y2: by - ca * len * .6 + Math.abs(sa) * len * .22,
        w: len * (.15 + r() * .04), k: r(), off: clamp((.95 - h[1]) / .36) * .3, px: [], py: [], len: 0, n: 0, pitch: 0 };
      const m = f.n = Math.max(8, Math.ceil(len / (3 * U)));
      for (let j = 0; j <= m; j++) {
        const t = j / m; f.px.push((1 - t) * (1 - t) * f.x0 + 2 * t * (1 - t) * f.x1 + t * t * f.x2); f.py.push((1 - t) * (1 - t) * f.y0 + 2 * t * (1 - t) * f.y1 + t * t * f.y2);
        if (j) f.len += Math.hypot(f.px[j] - f.px[j - 1], f.py[j] - f.py[j - 1]);
        top = Math.min(top, f.py[j] - f.w);
      }
      f.pitch = Math.max(3.6 * U, f.len / 17); f.wt = f.len * f.w; fernWeight += f.wt;
      fronds.push(f);
    }
  }
  gCell = 2 * U; gY = Math.max(0, top - 4 * U); gCols = Math.ceil(W / gCell) + 1; gRows = Math.max(1, Math.ceil((H - gY) / gCell) + 1);
  grid = new Uint16Array(gCols * gRows);
  fronds.forEach((f, id) => {
    for (let j = 0; j <= f.n; j++) {
      const rad = Math.max(frondW(f, j / f.n), 1.2 * U), x = f.px[j], y = f.py[j], m = Math.ceil(rad / gCell);
      const ix = Math.floor(x / gCell), iy = Math.floor((y - gY) / gCell);
      for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
        const cx = ix + dx, cy = iy + dy;
        if (cx >= 0 && cy >= 0 && cx < gCols && cy < gRows && Math.hypot(dx, dy) * gCell <= rad) grid[cy * gCols + cx] = id + 1;
      }
    }
  });
}
const fernAt = (x, y) => x < 0 || y < gY || x >= W || y >= H ? 0 : grid[(((y - gY) / gCell) | 0) * gCols + ((x / gCell) | 0)];

// ---------- whose a point is ----------
// Every point of the picture belongs to one thing, the nearest that covers it, and only that thing's strokes may
// start there.
const SKY = 0, LEFT = 1, RIGHT = 2, LOW = 3, BAY = 4, PAVE = 5, GRASS = 6, IVY = 7, FERN = 8, THING = 9, LEAVES = 10, LIMB = 11;
const IVYTOP = [[-3, .8], [-.6, .735], [0, .708], [.3, .7], [.5, .69], [.6, .664], [.68, .648], [.72, .636]];
function ivyTop(xf) {                                          // where the ivy begins: it leaves a path to the table, and none along the coach house
  let y = IVYTOP[IVYTOP.length - 1][1];
  for (let i = 0; i < IVYTOP.length - 1; i++) if (xf <= IVYTOP[i + 1][0]) { const a = IVYTOP[i], b = IVYTOP[i + 1]; y = a[1] + (b[1] - a[1]) * clamp((xf - a[0]) / (b[0] - a[0])); break; }
  if (xf > .72) { const f = smooth(.72, .82, xf); y += (footY(xf) + .014 - y) * f; }
  return y + (vnoise(xf * 50, 1.7) - .5) * .014;
}
const lawnEdge = xf => .655 - (xf < 0 ? xf * .07 : 0) - .03 * smooth(.2, .34, xf) + (vnoise(xf * 40, 3.1) - .5) * .012;
function groundAt(xf, pf) {
  if (pf > ivyTop(xf)) return IVY;
  if (xf < .33 + (vnoise(pf * 90, 5.5) - .5) * .02 && pf > footY(xf) + .011 * reachL(xf) && pf < lawnEdge(xf)) return GRASS;
  return PAVE;
}
let zx = 0, zy = 0, zi = 0, zf = 0;                            // what zone() learnt on the way: x and y on the photo, which frond or thing, how deep in the leaves
function zone(x, y, raw) {                                     // raw: as if the wisteria and what stands about were not there
  const xf = zx = fx(x), pf = zy = fy(y);
  if ((zi = fernAt(x, y))) return FERN;
  if (!raw) {
    if (pf > .53 && pf < .655 && xf > .21 && xf < .66 && (zi = thingAt(xf, pf))) return THING;
    if (inVine(x, y)) {
      const m = maskAt(x, y), f = zf = bare ? -1 : fol(x, y);
      if (m && f < .15) return LIMB;
      if (f > 0) return LEAVES;
    }
  }
  if (pf > footY(xf)) return groundAt(xf, pf);
  if (xf < XC) return pf < yL(EAVE, xf) ? SKY : LEFT;
  if (bayAt(xf, pf)) return BAY;
  if (xf >= LX0 && pf > yR(LOWTOP, xf)) return LOW;
  return pf < yR(EAVE, xf) ? SKY : RIGHT;
}
const built = (k, xf, pf) => k === LEFT ? leftAt(xf, levL(xf, pf)) : k === RIGHT ? rightAt(xf, levR(xf, pf)) : k === LOW ? lowAt(xf, pf) : bayMat(xf, pf);
const reach = [0, 0];
function run(x, y, ca, sa, l, ok, step) {                      // how far a stroke may go either way along (ca, sa) while ok() says so
  reach[0] = reach[1] = l;
  for (let d = step; d < l + step; d += step) if (!ok(zone(x + ca * d, y + sa * d))) { reach[1] = Math.min(l, d - step * .5); break; }
  for (let d = step; d < l + step; d += step) if (!ok(zone(x - ca * d, y - sa * d))) { reach[0] = Math.min(l, d - step * .5); break; }
}

// ---------- the light ----------
// By day the sun stands over the left wing, when it is out: that wing is in its own shade, the right one has the
// sun on it from some way up, and it gets in among the ferns at our feet.  At night the moon does the same, more
// faintly, and the lamp on the table throws its light on whatever is near it: there things have the colours they
// have by day, only yellow.  The picture's colours are mixed for eight strengths of that light.
const KEEP = /^(sky|cloud|star|moon|beacon|firefly|sun|lights|haze|fog|rain|flake|paper|cityFar)/;
const warm = c => [c[0] * .98, c[1] * .8, c[2] * .52];
function litPal(i) {
  const set = cur.innenhofLit || (cur.innenhofLit = []);
  if (set[i]) return set[i];
  const day = kit.palette(place, cur.season, cur.weather, false), g = i / 7 * .88, out = {};
  for (const key in own) {
    const a = own[key], b = day[key];
    out[key] = !a || !b || KEEP.test(key) ? a : Array.isArray(a[0]) ? a.map((c, n) => mix(c, warm(b[n] || c), g)) : mix(a, warm(b), g);
  }
  return set[i] = out;
}
function glowAt(xf, pf) {
  const d = Math.hypot(xf - LAMP[0], (pf - LAMP[1] + .012) * PH / PW), q = d * d;
  return Math.min(1, .95 * Math.exp(-q / .0075) + .5 * Math.exp(-q / .045));
}
let sun = 0;                                                   // what lightAt() learnt: how much of the sun or the moon gets there
function lightAt(k, x, y, xf, pf, dim) {                       // sets the colours for a point; dim: how much of the lamp's light the thing is hidden from
  pal = own; sun = 0;
  if (night) {
    const g = glowAt(xf, pf) * (dim || 1);
    if (g > .03) { const i = Math.min(7, (g * 7 + PP.R() * .9) | 0); if (i) pal = litPal(i); }
  }
  if (traits.veiled) return;
  if (k === RIGHT || k === LOW || k === BAY) sun = smooth(.03, -.03, levR(xf, pf) + .3 - (xf - XC) * .9 + (evening ? .16 : 0));
  else if (k >= PAVE && k <= FERN) sun = smooth(.72, .84, pf) * smooth(.4, .58, fbm(xf * 5 + 2, pf * 9));
  if (sun && !night) sun *= 1 - clamp(shadow(x, y) * 2.2);
}
const sunlit = col => sun <= 0 ? col : night ? mix(col, pal.moonGlow, sun * .18) : mix(mix(col, WHITE, sun * .3), pal.sunlight, sun * .22);
// Fog is between us and everything, and the more of it the farther off a thing is: the walls stand at the far
// end of the yard, the ferns at our feet.
const fogCol = () => night ? mix(pal.skyLow, pal.fog, .35) : pal.fog;
const veil = d => { const a = d * (traits.fog * 1.25 + traits.rain * .22); PP.haze = a > .01 ? [fogCol(), clamp(a, 0, .8)] : null; };
const offAt = (k, xf, pf) => k === LEFT ? .44 - clamp((XC - xf) * .1, 0, .16) : k === RIGHT || k === BAY ? .44 - (xf - XC) * .18 : k === LOW ? .38 - (xf - LX0) * .25 : clamp((.95 - pf) / .36) * .36;

// ---------- walls, windows, doors ----------
const LIT = { 1: 1, 5: 1, 8: 1, 14: 1, 18: 1, 21: 1, 105: 1, 200: .5 };   // the windows somebody is at home behind: bay times four plus floor on the left
const litUp = k => night && (LIT[k] || (k > 24 && k < 100 && hash1(k * 37 + 5) < .4 ? 1 : 0));
const LEN = [0, 16, 12, 6, 12, 10, 12, 8, 7, 6, 6, 4, 5, 7, 6, 5, 2, 9, 5, 6, 3, 10, 10, 9];   // how long a stroke each stuff gets
const WID = [0, 2.6, 2.4, 1.3, 2, 1.4, 1.7, 1.8, 2.4, 2.6, 1.1, 1.7, 2, 2.2, 1.7, 2.4, 2, 2.4, 1, 1.8, 2, 2.2, 2.2, 3];   // and how broad
function plaster(k, xf, pf, h) {                               // old render: blotches, what the rain has left running down it, the corner in its shade
  let c = k === LEFT ? pal.wallL : k === RIGHT ? mix(pal.wallR, pal.wallL, smooth(.28, .4, h)) : k === LOW ? pal.wallLow : pal.wallBay;   // down in the corner the right wall is as yellow as the left one
  c = mix(c, pal.stain, smooth(.48, .8, fbm(xf * 7 + k * 5.1, h * 11 + 2)) * .22);
  if (k === RIGHT && h > .012 && h < .3) c = mix(c, pal.stain, clamp(vnoise(xf * 120, 7.3) * vnoise(xf * 31, 2.1) * 2.2 - .3) * (1 - smooth(.03, .27, h)) * .7);
  if (k === LOW) c = mix(c, pal.algae, smooth(.5, .75, fbm(xf * 30, h * 60)) * (smooth(LOWPLINTH - .04, LOWFOOT, h) * .5 + smooth(LOWTOP + .08, LOWTOP + .02, h) * .12));
  if (k <= RIGHT) { const dc = Math.abs(xf - XC); if (dc < .07) c = mix(c, pal.shade, (1 - dc / .07) * .16); }
  if (k === LEFT && h > CORN) c = mix(c, pal.stain, smooth(PLINTH - .06, PLINTH, h) * .14);
  return c;
}
function glassCol(k, u, v, R) {                                // a pane: the sky and the house across the yard in it by day, a lamp behind curtains at night
  const fold = vnoise(u * 9 + k * 3.7, k * 1.3 + (v > .4 ? 2 : 0)), l = litUp(k);
  if (l) return mix(mix(mix(pal.lights[0], pal.lights[2], fold), pal.lights[3], smooth(.55, 1, Math.abs(u - .5) * 2) * .5 + R * .15), pal.dark, 1 - l);
  if (night) return mix(pal.pane[0], pal.skyMid, .06 + R * .08);
  const p = hash1(k * 31 + (u > .5 ? 7 : 0) + (v > .38 ? 13 : 0));      // every light of it catches another bit of that
  let t = .08 + p * .3 + smooth(.8, .05, v) * .2 + (R - .5) * .1;
  if (vnoise((u - v * .6) * 3.1 + k, k * .7) > .6) t += .2;
  let c = mix(ramp(pal.pane, t), pal.skyMid, .1 + p * .14);
  if ((k % 4 === 0 && k < 200) || k === 201) c = mix(c, pal.curtain, smooth(.3, .7, vnoise(u * 16 + k, 3)) * .6);   // curtains hang in the ground floor's windows
  return c;
}
function builtStroke(x, y, k) {
  const R = PP.R, xf = zx, pf = zy, m = built(k, xf, pf), d0 = dir, ku = wu, kv = wv, kk = wk, h = k === LEFT ? levL(xf, pf) : levR(xf, pf);
  if (m === BRASS || m === VENT) {
    lightAt(k, x, y, xf, pf); veil(offAt(k, xf, pf));
    if (m === VENT) { const cx = px(.6105), cy = py(.457), r = .006 * PW; PP.dab(cx, cy, r, pal.clay[2], { a: 1, den: 2.2, core: .9, s: U, jit: 4 }); PP.dab(cx, cy, r * .55, pal.dark, { a: 1, den: 2.5, core: 1, s: .8 * U, jit: 3 }); }
    else PP.dab(x, y, 1.3 * U, pal.gold, { a: 1, den: 2.5, core: .9, s: .9 * U, jit: 4 });
    return;
  }
  // which way the stroke goes, and how far it may: no farther than the stuff it began on
  let ca = 1, sa = 0;
  if (d0 === 1) { ca = 0; sa = 1; }
  else if (d0 === 2) { const q = 1 / (Math.hypot(sx, sy) || 1); ca = sx * q; sa = sy * q; }
  else { const s = (k === LEFT ? (HOR - pf) / (VLX - xf) : k === BAY ? bs : (pf - HOR) / (xf - VRX)) * PH / PW, q = 1 / Math.hypot(1, s); ca = q; sa = s * q; }
  const ok = q => q === k && built(k, zx, zy) === m, fine = Math.max(.75, .6 * U);
  run(x, y, ca, sa, LEN[m] * U * (.4 + R()), ok, m === WALL || m === NICHE || m === FLAT ? 2 * U : 1.5 * U);
  const a = reach[0], b = reach[1];
  let wd = WID[m] * U * (m === WALL || m === NICHE ? .7 + R() * .6 : 1);
  run(x, y, -sa, ca, wd * .62, ok, fine);
  if (reach[0] + reach[1] < wd) { const sh = (reach[1] - reach[0]) / 2; x -= sa * sh; y += ca * sh; wd = Math.max(1, reach[0] + reach[1]); }   // a thin thing: right down its middle
  dir = d0;
  lightAt(k, x, y, xf, pf);
  veil(offAt(k, xf, pf));
  let col, base = k === LEFT ? pal.wallL : k === RIGHT ? pal.wallR : k === LOW ? pal.wallLow : pal.wallBay, jit = 4;
  const snowy = white && R() < white;
  if (m === WALL || m === NICHE || m === STONE || m === COVE) {
    col = plaster(k, xf, pf, h);
    if (m === NICHE) col = mix(col, pal.frame, k === BAY ? .22 : .07);
    else if (m === STONE) col = mix(col, kk & 1 ? pal.frame : pal.stain, .06 + hash1(kk * 7) * .08);
    else if (m === COVE) col = mix(mix(col, pal.algae, smooth(.35, .7, fbm(xf * 60, pf * 90)) * .6), pal.shade, .12);
    if (k === BAY && m === WALL && xf >= .568) col = mix(col, pal.frame, .2);
  } else if (m === EDGE) col = mix(base, pal.shade, .4);
  else if (m === BAND) col = mix(mix(base, pal.frame, .3), pal.stain, fbm(xf * 40, h * 30) * .2);
  else if (m === FLAT) col = mix(plaster(k, xf, pf, h), pal.frame, .1);
  else if (m === PIPE) col = mix(pal.zinc, pal.frame, .12 + R() * .2);
  else if (m === SHADE) col = mix(base, pal.shade, .55);
  else if (m === ZINC) col = snowy && (k !== BAY || R() < .7) ? pal.snow : mix(pal.zinc, pal.dark, R() * .35);
  else if (m === SILL) col = snowy ? pal.snow : mix(pal.frame, pal.stain, .15 + R() * .15);
  else if (m === FRAME) col = litUp(kk) ? mix(pal.frame, pal.dark, .45) : mix(pal.frame, pal.stain, R() * .18);
  else if (m === GLASS) { col = glassCol(kk, ku, kv, R()); jit = 5; }
  else if (m === GRILLE) col = litUp(kk) ? mix(pal.dark, pal.lights[1], .2) : mix(pal.dark, pal.pane[1], .3);
  else if (m === STUCCO) {                                     // the plinth: combed render, green where it is damp
    col = mix(pal.plinth, pal.stain, vnoise(xf * 700, 3) * .3);
    col = mix(col, pal.algae, smooth(.45, .75, fbm(xf * 24 + 7, h * 50)) * (k === LOW ? .55 : .2));
  } else if (m === HOLE) col = mix(pal.iron, pal.plinth, .12 + R() * .1);
  else if (m === DOOR) col = mix(pal.gate[1], R() < .5 ? pal.gate[0] : pal.gate[2], R() * .55);
  else if (m === DPANEL) col = mix(pal.gate[0], pal.gate[1], hash1(Math.floor(xf * 420)) * .8);
  else if (m === DGLASS) {
    jit = 5;
    if (kk === 300) col = night ? mix(pal.pane[0], pal.skyMid, .08) : mix(ramp(pal.pane, .2 + R() * .12), pal.curtain, .25 + smooth(.3, .7, vnoise(ku * 9, 4 + (kv * 3 | 0))) * .5);   // net curtains behind the door to the stairs
    else col = mix(mix(pal.gate[0], pal.pane[2], .3 + vnoise(ku * 5 + kk, kv * 4) * .4), night ? pal.dark : pal.fern[2], night ? .3 : .12 + R() * .12);   // the yard, dimly, in the gate
  } else if (m === JOINT) col = mix(base, pal.shade, .34);
  else if (m === RING) col = mix(mix(base, pal.stain, .4), pal.algae, smooth(.6, 1, (yR(SPRING, xf) - pf) / GRISE) * .45);
  else col = base;
  col = sunlit(col);
  const sd = shadow(x, y); if (sd && !sun) col = mix(col, pal.shade, sd * .1);
  PP.brush(x - ca * a, y - sa * a, x + ca * b, y + sa * b, wd, col, { a: .95, jit, dry: m === WALL || m === NICHE ? .08 : 0, bow: 0, sp: Math.min(1.1 * U, wd) });
}

// ---------- what stands about ----------
function fallen(x, y, sz) {                                    // a leaf the wisteria has dropped
  const R = PP.R, a = R() * Math.PI, l = (1.6 + R() * 2.4) * sz, k = R();
  PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l * .7, x + Math.cos(a) * l, y + Math.sin(a) * l * .7, (1.3 + R() * 1.3) * sz, sunlit(k < .12 ? pal.fallen[4] : pal.fallen[(k * 4) | 0]), { a: .93, jit: 7, dry: .05, sp: 1.1 * U, bow: .2 });
}
function thingStroke(x, y) {
  const R = PP.R, id = zi, t = THINGS[id - 1], xf = zx, pf = zy, k = t.k, sz = Math.max(U, .0016 * PW);
  lightAt(THING, x, y, xf, pf, k === CRATE || k === BOARDS ? .45 : k === TOP || k === RIMW ? 1.15 : 1);
  veil(.32);
  if (k === TOP || k === PAD) {                                // what lies on the table and the chair: leaves, or snow
    if (white && R() < white * .9) { PP.dab(x, y, (1.5 + R() * 2) * sz, pal.snow, { a: .95, den: 1.4, core: .8, s: U, sx: 1.6, sy: .7, jit: 3 }); return; }
    if (season === 'autumn' && R() < (k === TOP ? .5 : .25)) { fallen(x, y, sz * .8); return; }
  }
  if (k === LEAFY) { PP.dab(x, y, (1.2 + R() * 1.6) * sz, ramp(pal.vine, .15 + R() * .5), { a: .9, den: 1, core: .7, s: 1.2 * U, jit: 6 }); return; }
  if (k === CLOTH && R() < .2) { PP.dab(x, y, .9 * sz, mix(pal.cushion, pal.dark, .45), { a: .9, den: 1.6, core: .7, s: .8 * U, jit: 3 }); return; }   // its buttons
  let ca = 1, sa = 0, l = 6, wd = 1.8;
  if (t.up) { ca = 0; sa = 1; }
  else if (t.d) { const q = 1 / Math.hypot(t.d[0] * PW, t.d[1] * PH); ca = t.d[0] * PW * q; sa = t.d[1] * PH * q; }
  else if (t.p) { const p = t.p, q = 1 / Math.hypot((p[1][0] - p[0][0]) * PW, (p[1][1] - p[0][1]) * PH); ca = (p[1][0] - p[0][0]) * PW * q; sa = (p[1][1] - p[0][1]) * PH * q; }
  let col;
  if (k === WOOD) col = mix(pal.wood[(hash1((pf * 900 | 0) + id) * 3) | 0], pal.wood[1], .4);
  else if (k === TEAK) col = mix(pal.teak[(hash1((pf * 900 | 0) + id) * 3) | 0], pal.teak[1], .4);
  else if (k === BOARDS) { const i = Math.floor(xf / .0052); col = mix(pal.table[1 + ((hash1(i * 17) * 2.99) | 0)], pal.teak[2], smooth(.4, .9, (pf - t.y0) / (t.y1 - t.y0) + hash1(i) * .5 - .25) * .5); l = 8; wd = Math.min(1.8, .0045 * PW / U); }
  else if (k === CRATE) { const f = xf / .0066; col = f - Math.floor(f) < .26 ? mix(pal.teak[1], pal.table[1], .3) : pal.table[0]; l = 8; wd = 1.2; }
  else if (k === TOP) col = mix(pal.table[2], pal.teak[1], vnoise(xf * 300, pf * 90) * .7);
  else if (k === RIMW) { col = pal.table[0]; wd = 1.2; }
  else if (k === CLOTH) col = Math.abs(xf - .2465) < .0012 ? mix(pal.cushion, pal.dark, .5) : mix(pal.cushion, pal.frame, vnoise(xf * 400, pf * 400) * .25);
  else if (k === PAD) col = mix(pal.cushion, pal.skyMid, .35);
  else if (k === POT) col = mix(pal.pot, pal.frame, smooth(.5, 0, (xf - t.x0) / (t.x1 - t.x0)) * .25);
  else if (k === CLAY) col = mix(pal.clay[0], pal.clay[2], smooth(.6, .05, (xf - t.x0) / (t.x1 - t.x0)) * .7 + R() * .15);
  else if (k === SOIL) col = white && R() < white ? pal.snow : mix(pal.dark, pal.clay[0], .4);
  else if (k === LID) col = mix(pal.clay[0], pal.clay[1], R() * .7);
  else if (k === IRON) col = mix(pal.iron, pal.zinc, R() * .3);
  else if (k === LAMPGLASS) col = mix(pal.iron, pal.skyMid, .25 + R() * .25);
  else if (k === ROCK) col = mix(pal.cobble[1], pal.cobble[2], R());
  else if (k === ENAMEL) col = pal.frame;
  else col = mix(pal.cushion, pal.skyMid, .6);
  run(x, y, ca, sa, l * U * (.4 + R()), q => q === THING && zi === id, 1.2 * U);
  const a = reach[0], b = reach[1];
  wd *= U;
  run(x, y, -sa, ca, wd * .62, q => q === THING && zi === id, Math.max(.75, .6 * U));
  if (reach[0] + reach[1] < wd) { const sh = (reach[1] - reach[0]) / 2; x -= sa * sh; y += ca * sh; wd = Math.max(1, reach[0] + reach[1]); }
  PP.brush(x - ca * a, y - sa * a, x + ca * b, y + sa * b, wd, col, { a: .95, jit: 5, dry: 0, bow: 0, sp: Math.min(1.1 * U, wd) });
}
let tx0 = 0, tx1 = 0, ty0 = 0, ty1 = 0;                        // the box they stand in
function thingDab() {                                          // they are small: each gets strokes of its own
  const R = PP.R, x = tx0 + R() * (tx1 - tx0), y = ty0 + R() * (ty1 - ty0);
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y)) return;
  if (zone(x, y) === THING) thingStroke(x, y);
}

// ---------- the ground ----------
const lying = (xf, pf) => season === 'autumn' ? .14 + smooth(.3, .7, fbm(xf * 9 + 4, pf * 16)) * .42 : 0;   // how thick the leaves lie
function snowDab(x, y, sz) { PP.dab(x, y, (2 + PP.R() * 3) * sz, sun > .3 ? pal.snowSun : pal.snow, { a: .92, den: .9, core: .6, s: 1.6 * U, sx: 1.7, sy: .6, jit: 3 }); }
function paveStroke(x, y) {                                    // cobbles, laid in rows
  const R = PP.R, xf = zx, pf = zy, sz = near(pf) * U * 1.25, k = R();
  lightAt(PAVE, x, y, xf, pf); veil(offAt(PAVE, xf, pf));
  if (white && k < white * .93) { snowDab(x, y, sz); return; }
  if (k < lying(xf, pf) * 1.5) { fallen(x, y, sz); return; }
  if (season === 'spring' && k > .975 && xf < .5) { PP.dab(x, y, (.8 + R()) * sz, pick(pal.bloom, R()), { a: .9, den: 1.5, core: .7, s: U, jit: 4 }); return; }   // what the wisteria sheds in May
  const row = Math.floor(y / (3.2 * sz)), i = Math.floor((x + (row & 1) * 2.6 * sz) / (5.2 * sz)), n = fbm(xf * 26, pf * 50);
  let col = mix(pal.cobble[(hash1(row * 131 + i * 17) * 3) | 0], pal.cobble[1], .35);
  if (n > .56) col = mix(col, pal.algae, (n - .56) * 1.6);
  if (traits.rain) col = mix(col, pal.skyLow, .12 + R() * .14);
  col = sunlit(col);
  const sd = shadow(x, y); if (sd) col = mix(col, pal.cobble[0], sd * .3);
  if (k > .9) { PP.brush(x - 3 * sz, y, x + 3 * sz, y, .8 * sz, mix(pal.cobble[0], pal.dark, .5), { a: .7, jit: 4, dry: .1, sp: U, bow: .05 }); return; }   // the joints
  const l = (1.6 + R() * 1.4) * sz;
  run(x, y, 1, 0, l, q => q === PAVE, 1.5 * U);
  PP.brush(x - reach[0], y, x + reach[1], y, (2 + R() * .9) * sz, col, { a: .9, jit: 7, dry: .08, sp: 1.1 * U, bow: .05 });
}
function grassStroke(x, y) {                                   // the tuft of long grass by the wall, every blade upright
  const R = PP.R, xf = zx, pf = zy, sz = near(pf) * U * 1.3, k = R();
  lightAt(GRASS, x, y, xf, pf); veil(offAt(GRASS, xf, pf));
  if (white && k < white * .8) { snowDab(x, y, sz); return; }
  if (k < lying(xf, pf) * .55) { fallen(x, y, sz); return; }
  let col = ramp(pal.lawn, .15 + fbm(xf * 30, pf * 70) * .75 + (R() - .5) * .3);
  const sd = shadow(x, y); if (sd) col = mix(col, pal.lawn[0], sd * .4);
  const a = -Math.PI / 2 + (R() - .5) * 1.1 + wind(time) * .08, l = (2.5 + R() * 4) * sz;
  run(x, y, Math.cos(a), Math.sin(a), l, q => q === GRASS || q === PAVE, 1.5 * U);
  PP.brush(x - Math.cos(a) * reach[0] * .4, y - Math.sin(a) * reach[0] * .4, x + Math.cos(a) * reach[1], y + Math.sin(a) * reach[1], (1.1 + R() * 1.2) * sz, sunlit(col), { a: .88, jit: 8, dry: .15, sp: 1.1 * U, bow: .2 });
}
function ivyStroke(x, y) {                                     // ivy all over the ground: dark leaves, each with a shine on it
  const R = PP.R, xf = zx, pf = zy, sz = near(pf) * U, k = R();
  lightAt(IVY, x, y, xf, pf); veil(offAt(IVY, xf, pf));
  const n = fbm(xf * 14 + 3, pf * 26), lum = clamp(.24 + (n - .5) * 1.3 + (R() - .5) * .45 + sun * .25);
  if (white && k < white * (.25 + lum * .7)) { snowDab(x, y, sz * 1.2); return; }
  if (k < lying(xf, pf) * .8) { fallen(x, y, sz * 1.6); return; }
  let col = ramp(pal.ivy, lum);
  const sd = shadow(x, y); if (sd) col = mix(col, pal.ivy[0], sd * .5);
  const r = (1.7 + R() * 2.1) * sz;
  PP.dab(x, y, r, sunlit(col), { a: .9, den: .9, core: .75, s: 1.5 * U, sx: 1.25, sy: .8, jit: 7 });
  if (lum > .45 && R() < .3) PP.dab(x - r * .25, y - r * .3, r * .4, sunlit(pal.ivy[4]), { a: .7, den: .8, core: .5, s: U, jit: 5 });
}

// ---------- the ferns ----------
function fernStroke(x, y) {
  const R = PP.R, id = zi, f = fronds[id - 1], xf = zx, pf = zy, n = f.n;
  let bi = 0, bd = 1e9;
  for (let i = 0; i <= n; i++) { const dx = x - f.px[i], dy = y - f.py[i], d = dx * dx + dy * dy; if (d < bd) { bd = d; bi = i; } }
  const i0 = Math.max(0, bi - 1), i1 = Math.min(n, bi + 1);
  let tx = f.px[i1] - f.px[i0], ty = f.py[i1] - f.py[i0]; const q = 1 / (Math.hypot(tx, ty) || 1); tx *= q; ty *= q;
  const dx = x - f.px[bi], dy = y - f.py[bi], al = dx * tx + dy * ty, cr = tx * dy - ty * dx, side = cr > 0 ? 1 : -1, dist = Math.abs(cr);
  const t = clamp(bi / n + al / f.len), s = t * f.len, j = s / f.pitch, fr = j - Math.floor(j), sz = f.pitch / 3.2;
  lightAt(FERN, x, y, xf, pf); veil(f.off);
  const same = z => z === FERN && zi === id;
  if (dist < Math.max(.9 * U, f.pitch * .16)) {                 // the rib
    const l = (3 + R() * 5) * U;
    run(x, y, tx, ty, l, same, 1.5 * U);
    PP.brush(x - tx * reach[0], y - ty * reach[0], x + tx * reach[1], y + ty * reach[1], Math.max(U, f.pitch * .22), sunlit(mix(pal.fern[3], pal.teak[1], bare ? .7 : .35)), { a: .92, jit: 5, dry: 0, bow: .02, sp: U });
    return;
  }
  if (white && R() < white * .5) { snowDab(x, y, sz); return; }
  if (season === 'autumn' && R() < lying(xf, pf) * .3) { fallen(x, y, sz * 1.4); return; }
  // a leaflet: out from the rib and a little towards its tip, and the dark between two of them
  const nx = -ty * side, ny = tx * side;
  let ox = nx * .9 + tx * .44, oy = ny * .9 + ty * .44; const o = 1 / Math.hypot(ox, oy); ox *= o; oy *= o;
  const gap = fr < .2 || fr > .84, lum = clamp(.24 + f.k * .5 + (side > 0 ? .1 : -.06) + (R() - .5) * .3 + sun * .4 - (1 - t) * .12);
  let col = gap ? mix(pal.fern[0], pal.ivy[0], .4) : ramp(pal.fern, lum);
  if (!gap && season === 'autumn' && f.k > .7) col = mix(col, pal.fallen[3], (f.k - .7) * 1.6 + t * .2);   // a few have begun to turn
  const sd = shadow(x, y); if (sd && !gap) col = mix(col, pal.fern[0], sd * .45);
  const l = (2 + R() * 3.5) * sz;
  run(x, y, ox, oy, l, same, 1.5 * U);
  PP.brush(x - ox * Math.min(reach[0], dist), y - oy * Math.min(reach[0], dist), x + ox * reach[1], y + oy * reach[1], f.pitch * (gap ? .26 : .56), gap ? col : sunlit(col), { a: .9, jit: 7, dry: .1, sp: Math.min(1.2 * U, f.pitch * .4), bow: .08 });
}
function fernDab() {                                           // somewhere on a frond
  const R = PP.R; let k = R() * fernWeight, f = fronds[0];
  if (!f) return;
  for (const g of fronds) { f = g; k -= g.wt; if (k <= 0) break; }
  const i = Math.min(f.n, (R() * (f.n + 1)) | 0), w = frondW(f, i / f.n), a = R() * TAU, d = Math.sqrt(R()) * w, x = f.px[i] + Math.cos(a) * d, y = f.py[i] + Math.sin(a) * d;
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y)) return;
  if (zone(x, y) === FERN) fernStroke(x, y);
}

// ---------- leaves and limbs ----------
function leafStroke(x, y) {                                    // the wisteria: a leaf is a row of leaflets, and hangs
  const R = PP.R, d = zf, xf = zx, pf = zy, k = R(), sz = Math.max(1.05 * U, .002 * PW), lum = clamp(folLum(x, y) + (R() - .5) * .25);
  lightAt(LEAVES, x, y, xf, pf); veil(.38);
  if (season === 'spring' && k < .38 && d > .05) {              // in May it flowers: long bunches of blue
    const l = (3 + R() * 5) * sz, a = Math.PI / 2 + (R() - .5) * .3 + wind(time) * .06;
    PP.brush(x, y, x + Math.cos(a) * l, y + Math.sin(a) * l, (1.5 + R()) * sz, pal.bloom[(R() * 3) | 0], { a: .9, sp: 1.2 * U, dry: .12, jit: 9, bow: .1 });
    PP.dab(x + Math.cos(a) * l, y + Math.sin(a) * l, .8 * sz, pal.bloom[3], { a: .85, den: 1.2, core: .6, s: U, jit: 5 });
    return;
  }
  const a = Math.PI / 2 + (R() - .5) * 2.2, ca = Math.cos(a), sa = Math.sin(a);
  let col = ramp(pal.vine, d < .1 ? .3 + lum * .35 : lum * .92 + .04);
  if (lum > .62 && k > .7) col = pal.vine[4];
  const o = { a: .88, sp: 1.1 * U, dry: .12, jit: 9, bow: .2 }, n = d > .1 ? 2 + (R() * 3 | 0) : 1, st = 2.2 * sz, l = (2.2 + R() * 1.6) * sz;
  for (let j = 0; j < n; j++) {                                // a pair of leaflets every little way down its stalk, one at its end
    const cx = x + ca * st * (j - n / 2), cy = y + sa * st * (j - n / 2);
    for (const sd of [-1, 1]) {
      const b = a + sd * (1.05 + R() * .3), cb = Math.cos(b) * l, sb = Math.sin(b) * l;
      PP.brush(cx + cb * .15, cy + sb * .15, cx + cb, cy + sb, (1 + R() * .6) * sz, j & 1 ? col : mix(col, pal.vine[1], .25), o);
    }
  }
  PP.brush(x + ca * st * (n / 2 - .6), y + sa * st * (n / 2 - .6), x + ca * st * (n / 2 + .5), y + sa * st * (n / 2 + .5), (1 + R() * .6) * sz, col, o);
}
function leafDab() {                                           // somewhere in its leaves
  const R = PP.R; let k = R() * leafArea, c = leafy[0];
  if (!c) return;
  for (let a = 0, b = leafy.length - 1; a <= b;) { const m = (a + b) >> 1; if (leafy[m].at <= k) { c = leafy[m]; a = m + 1; } else b = m - 1; }
  const a = R() * TAU, d = Math.sqrt(R()) * c.r * 1.1, x = c.x + Math.cos(a) * d, y = c.y + Math.sin(a) * d;
  if (x < 0 || x >= W || y < 0 || y >= H || !at(x, y) || bare) return;
  const z = zone(x, y);
  if (z === LEAVES) leafStroke(x, y);
  else if (zf > -.1 && z === LEFT && !maskAt(x, y) && R() < .5) { zf = 0; leafStroke(x, y); }   // a leaf or two beyond them
}
function limbStroke() {                                        // a stretch of its wood
  const R = PP.R; let k = R() * limbWeight, s = limbs[0];
  if (!s) return;
  for (const t of limbs) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (8 + R() * 16) * U / s.len), t0 = -.1 + R() * (1.2 - span), tm = t0 + span / 2, t1 = t0 + span;   // a little beyond its ends: it goes on there
  const mx = limbX(s, clamp(tm)), my = limbY(s, clamp(tm));
  if (mx < 0 || mx >= W || my < 0 || my >= H || !at(mx, my) || zone(mx, my) !== LIMB) return;   // not where leaves hide it
  lightAt(LIMB, mx, my, zx, zy); veil(.38);
  const bark = mix(pal.bark, pal.frame, s.depth === 3 ? .3 : s.depth ? .12 : .05), q = R();
  const o = { a: .93, jit: 7, dry: .06, sp: Math.min(1.2 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (white && s.w > 1.3 * U && q < white * .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.snow, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 });   // snow lies on it
  }
}

// ---------- every stroke where it belongs ----------
function paintAt(x, y, k) {
  if (k === SKY || k === LIMB) return;
  if (k <= BAY) builtStroke(x, y, k);
  else if (k === PAVE) paveStroke(x, y); else if (k === GRASS) grassStroke(x, y); else if (k === IVY) ivyStroke(x, y);
  else if (k === FERN) fernStroke(x, y); else if (k === THING) thingStroke(x, y); else leafStroke(x, y);
}
function landStroke(y0, y1) {                                  // somewhere between two heights
  const R = PP.R, x = R() * W, y = y0 + R() * (y1 - y0);
  if (at(x, y)) paintAt(x, y, zone(x, y));
}
const BANDS = [() => landStroke(0, H / 3), () => landStroke(H / 3, H * 2 / 3), () => landStroke(H * 2 / 3, H)];
// Windows and doors are a great many small things: they get strokes of their own, that they need not wait for
// one of the wall's to come by.
let spots = [], spotArea = 0;
function buildSpots() {
  spots = []; spotArea = 0;
  const add = (x0, y0, x1, y1) => {
    x0 = Math.max(0, px(x0)); x1 = Math.min(W, px(x1)); y0 = Math.max(0, py(y0)); y1 = Math.min(H, py(y1));
    if (x1 - x0 < 2 || y1 - y0 < 2) return;
    const s = { x0, y0, x1, y1, at: spotArea }; spotArea += (x1 - x0) * (y1 - y0); spots.push(s);
  };
  for (let n = 0; n < 11; n++) {
    const a1 = A0 - n * BAYW + RIM, a0 = a1 - WIN - 2 * RIM, xa = VLX - 1 / a0, xb = VLX - 1 / a1;
    if (a0 <= 0 || xb < X0 || xa > fx(W)) continue;
    add(xa, yL(F2T - .01, xa), xb, yL(F2B + .012, xb)); add(xa, yL(F1T - .01, xa), xb, yL(F1B + .01, xb)); add(xa, yL(G0T - .012, xa), xb, yL(n ? G0B + .03 : FOOT, n ? xb : xa));
  }
  add(.52, yR(F2T - .01, .592), .592, yR(F2B + .012, .52)); add(.772, yR(F2T - .01, .9), .9, yR(F2B + .012, .772));
  add(.772, yR(.115, .9), .9, yR(LOWTOP, .772)); add(.515, yR(.398, .59), .59, yR(.537, .59));
  add(.466, .12, .618, .405);
  add(.63, yR(.3, 1.04), 1.04, yR(LOWFOOT, 1.04));
}
function spotDab() {
  const R = PP.R; let k = R() * spotArea, s = spots[0];
  if (!s) return;
  for (let a = 0, b = spots.length - 1; a <= b;) { const m = (a + b) >> 1; if (spots[m].at <= k) { s = spots[m]; a = m + 1; } else b = m - 1; }
  const x = s.x0 + R() * (s.x1 - s.x0), y = s.y0 + R() * (s.y1 - s.y0);
  if (!at(x, y)) return;
  const z = zone(x, y);
  if (z >= LEFT && z <= BAY) builtStroke(x, y, z);
}

// ---------- weather ----------
function fogStroke() {                                         // fog hangs in the yard in drifting banks
  const R = PP.R, x = R() * W, depth = R(), y = py(.4 + depth * .32);
  if (y < 0 || y >= H || !at(x, y) || !traits.fog || zone(x, y) === SKY) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .15 < .5) return;
  PP.haze = null;
  PP.dab(x, y, (8 + R() * 16) * U, fogCol(), { a: .3 - depth * .14, den: .2, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}
function glintDab() {                                          // snow glittering in the sun
  const R = PP.R, x = R() * W, y = py(.7) + R() * (H - py(.7));
  if (y < 0 || !at(x, y) || !traits.glint || !white) return;
  const k = zone(x, y);
  lightAt(k, x, y, zx, zy);
  if (sun < .6 || k < PAVE || k > FERN) return;
  PP.haze = null;
  PP.dab(x, y, (.6 + R() * .8) * U, pal.sun, { a: 1, den: 3, core: 1, s: U, jit: 2 });
}

// ---------- the coat ----------
function coatStroke(x0, y0, leaves) {                          // a fresh coat: a few broad strokes of what is there, where the brush comes by
  const R = PP.R;
  for (let n = 0; n < 6; n++) {
    const x = x0 + (n % 3 - 1 + (R() - .5) * .6) * 22 * U, y = y0 + ((n / 3 | 0) - .5 + (R() - .5) * .6) * 20 * U;
    if (x < -20 * U || x > W + 20 * U || y < -12 * U || y > H + 12 * U) continue;
    const xc = clamp(x, 0, W - 1), yc = clamp(y, 0, H - 1), l = (14 + R() * 8) * U, o = { a: .93, jit: 7, dry: 0, full: 1, sp: 2.2 * U, bow: .03 };
    if (leaves) {                                              // the wisteria: it hangs in front of the wall
      if (zone(xc, yc) !== LEAVES) continue;
      lightAt(LEAVES, xc, yc, zx, zy); veil(.38);
      const a = Math.PI / 2 + (R() - .5) * 1.2;
      PP.brush(x - Math.cos(a) * l * .5, y - Math.sin(a) * l * .5, x + Math.cos(a) * l * .5, y + Math.sin(a) * l * .5, 12 * U, ramp(pal.vine, .2 + clamp(folLum(x, y)) * .5), o);
      continue;
    }
    const k = zone(xc, yc, true), xf = zx, pf = zy;
    if (k === SKY) continue;
    lightAt(k, xc, yc, xf, pf); veil(k === FERN ? fronds[zi - 1].off : offAt(k, xf, pf));
    if (k <= BAY) {
      const m = built(k, xf, pf), s = (k === LEFT ? (HOR - pf) / (VLX - xf) : k === BAY ? bs : (pf - HOR) / (xf - VRX)) * PH / PW, q = 1 / Math.hypot(1, s);
      const col = m === DOOR || m === DPANEL || m === DGLASS ? pal.gate[1] : m === STUCCO ? pal.plinth : plaster(k, xf, pf, k === LEFT ? levL(xf, pf) : levR(xf, pf));
      PP.brush(x - q * l, y - s * q * l, x + q * l, y + s * q * l, 24 * U, sunlit(col), o);
    } else if (k === FERN) {
      const a = R() * Math.PI;
      PP.brush(x - Math.cos(a) * l * .7, y - Math.sin(a) * l * .7, x + Math.cos(a) * l * .7, y + Math.sin(a) * l * .7, 16 * U, sunlit(ramp(pal.fern, .25 + R() * .3)), o);
    } else {
      const lie = lying(xf, pf);
      let col = k === PAVE ? pal.cobble[1] : k === GRASS ? pal.lawn[1] : mix(pal.ivy[1], pal.ivy[2], R() * .6);
      if (white) col = mix(col, pal.snow, white * (k === IVY ? .5 : .85)); else if (lie) col = mix(col, pal.fallen[1], lie * (k === GRASS ? .3 : .6));
      PP.brush(x - l, y, x + l, y, 22 * U, sunlit(col), o);
    }
  }
}

// ---------- what moves ----------
// The lamp on the table is too small and too bright for the brush that paints the yard: it is drawn anew every
// frame, with the moths it brings on a warm night.
function drawLamp(P, t) {
  const x = px(LAMP[0]), y = py(LAMP[1]);
  if (x < -40 * U || x > W + 40 * U || y < 0 || y > H + 40 * U || !at(clamp(x, 0, W - 1), clamp(y, 0, H - 1)) || !night) return;
  const s = Math.max(3.2 * U, .0105 * PH), far = 1 + traits.fog * .7 + traits.rain * .15, ink = mix(pal.ink, [8, 8, 12], .5);
  const o = { a: .97, sp: Math.max(.7 * U, s * .12), dry: 0, jit: 2, bow: 0 };
  P.haze = null;
  P.dab(x, y - s * 1.9, s * 6.5 * far, pal.lights[0], { a: .1, den: .22, s: 3 * U, jit: 3 });               // the air round it
  P.dab(x, y - s * 1.9, s * 3 * far, pal.lights[0], { a: .24, den: .6, s: 2 * U, jit: 3 });
  P.dab(x, y + s * .15, s * 1.7, pal.lights[2], { a: .5, den: 1.2, core: .35, s: 1.4 * U, sx: 1.9, sy: .42, jit: 2 });   // its light on the table
  P.dab(x, y - s * .06, s * .42, ink, { a: 1, den: 2.5, core: 1, s: .8 * U, sx: 1.5, sy: .5, jit: 2 });      // its foot
  P.brush(x, y - s * .1, x, y - s * 1.35, s * .16, ink, o);                                                  // its stem
  for (let i = 0; i < 6; i++) {                                // its shade: a small bell, alight
    const f = i / 5, w = s * (1.08 - f * .62), yy = y - s * (1.32 + f * .9);
    for (let n = 0; n < 2; n++) P.brush(x - w, yy, x + w, yy, s * .24, mix(pal.lights[2], pal.lights[1], f * .45 + .1), { a: 1, sp: Math.max(.6 * U, s * .12), dry: 0, jit: 2, bow: 0 });
  }
  P.brush(x - s * 1.1, y - s * 1.26, x + s * 1.1, y - s * 1.26, Math.max(.8, s * .1), pal.lights[3], { a: .9, sp: Math.max(.6 * U, s * .1), dry: 0, jit: 2, bow: 0 });   // its rim
  P.dab(x, y - s * 1.6, s * .5, [255, 250, 232], { a: .95, den: 2.5, core: 1, s: U, sx: 1.3, sy: .7, jit: 1 });
  if (season === 'winter' || traits.rain || traits.snow) return;
  for (let i = 0; i < 3; i++) {                                // moths
    const ph = i * 2.3, w = .9 + i * .37, r = s * (2.2 + Math.sin(t * .31 + ph) * 1.1);
    const mx = x + Math.cos(t * w + ph) * r, my = y - s * 1.9 + Math.sin(t * w * 1.31 + ph * 2) * r * .6, fl = Math.sin(t * 31 + ph) * s * .28;
    P.R = rng(7300 + i);
    P.brush(mx - s * .3, my - fl, mx + s * .3, my + fl, Math.max(U, s * .22), pal.lights[2], { a: .8, sp: U, dry: 0, jit: 4, bow: .3 });
  }
}

// ---------- the painting, one frame's worth ----------
let tops = null, skyL = 0, skyR = 0;
const RATE = { land: 12000, spot: 12000, thing: 40000, fern: 8000, leaf: 3000, limb: 1.3, fog: 900, glint: 60 };
const place = {
  colors: {
    wallL: '#e2d4b4', wallR: '#e1dccb', wallLow: '#e6d9b4', wallBay: '#e6d3a0', plinth: '#cbbf9f', stain: '#9c9886', algae: '#6f7a4c', shade: '#4b4a52',
    frame: '#f1f1ea', pane: ['#1d232d', '#3a4656', '#68788e', '#a9b8cc'], curtain: '#e6e2d6', zinc: '#5b5d60',
    gate: ['#0f251d', '#1c3a2d', '#33594a'], bark: '#4b3b30', iron: '#2b2c2f',
    wood: ['#5a3f2f', '#77543c', '#946c4b'], teak: ['#665c48', '#857a61', '#a69a7c'], table: ['#2b211d', '#4a3228', '#684735', '#7d5a42'],
    cushion: '#6c788b', pot: '#7c836d', clay: ['#4b3024', '#6b4534', '#a4694c'],
    cobble: ['#6a6b68', '#8b8b85', '#a8a69c'],
    lawn: ['#1c3a1b', '#315f25', '#4f862e', '#84b040'],
    ivy: ['#0f2118', '#1b3a26', '#2c5a33', '#4a7a3e', '#8fb07c'],
    fern: ['#17301f', '#2a5631', '#42803f', '#6fa650', '#a9cc6a'],
    vine: ['#102617', '#1f4424', '#37702f', '#62993a', '#9cc254'],
    bloom: ['#8c7ed2', '#a594e2', '#c7b8f0', '#6f62b4'],
    fallen: ['#e9d35a', '#d9b93c', '#f1e596', '#c79f3a', '#b9742c'],
    sunlight: '#ffe6b0', snow: '#eef2f4', snowSun: '#ffffff',
  },
  seasons: {
    spring: { vine: ['#23401f', '#3f6f2c', '#6fa23c', '#a3cc52', '#d2e47a'], fern: ['#1d3a22', '#346a34', '#58993f', '#8cc253', '#c4e078'], lawn: ['#2f5a26', '#52902f', '#80ba3c', '#b3db58'],
      bits: ['#c7b8f0', '#a594e2', '#e4dcf8'] },
    autumn: { vine: ['#4a4a1c', '#7c7a26', '#b5a530', '#dcc640', '#f0e070'], fern: ['#142a1c', '#28502f', '#3f773d', '#6a9a4c', '#a3b860'], lawn: ['#243c1a', '#3d6224', '#5f882d', '#93aa42'],
      bits: ['#e9d35a', '#d9b93c', '#f1e596', '#c79f3a'] },
    winter: { fern: ['#2a2a20', '#4a4530', '#6e6240', '#958257', '#b8a678'], lawn: ['#3b4a2a', '#5c6a3a', '#84894f', '#a9a56a'], ivy: ['#101f18', '#1c3626', '#2b4f32', '#44693c', '#7f9c78'] },
  },
  ground: /^snow/,
  hz: 0,
  layout(v) {
    W = v.W; H = v.H; U = v.U;
    // An upright screen shows the photograph from a patch of sky down to the ferns at our feet, and a slice of its
    // width; a wide one less of its height and a good deal that lies to the left of it.
    const k = clamp((W / H - .6) / 1.15);
    let top = -.27 + .09 * k, foot = .92 - .14 * k;
    PH = H / (foot - top); PW = PH * .75 * clamp(.84 + W / H * .28, .84, 1);
    if (W / PW > 2.65) { PW = W / 2.65; PH = PW / .75; top += (foot - top - H / PH) * .4; }   // a very wide one loses some more of both
    Y0 = top;
    X0 = Math.min(.56 - W / PW / 2, 1.25 - W / PW);
    place.hz = Math.max(8 * U, py(.06));
    const n = Math.ceil(W) + 1;
    tops = new Float32Array(n); skyL = n; skyR = -1;
    for (let i = 0; i < n; i++) { tops[i] = py(eaveY(fx(i))); if (tops[i] > 0) { if (i < skyL) skyL = i; skyR = i; } }
    tx0 = px(.21); tx1 = px(.66); ty0 = py(.53); ty1 = py(.655);
    buildVine(); buildFerns(); buildSpots();
  },
  tick(t) { time = t; },
  top(x) { return tops[x < 0 ? 0 : x >= tops.length ? tops.length - 1 : x | 0] - U; },
  sky(c, pad) {                                                // the patch between the two roofs
    if (skyR < skyL) { c.rect(0, -60, 0, 0); return; }
    const dip = 3 * U - pad, xc = clamp(px(XC), skyL, skyR + 1);
    c.moveTo(skyL, -60); c.lineTo(skyR + 1, -60); c.lineTo(skyR + 1, tops[skyR] + dip);
    if (xc > skyL && xc < skyR) c.lineTo(xc, tops[xc | 0] + dip);
    c.lineTo(skyL, tops[skyL] + dip); c.closePath();
  },
  orb(o) {                                                     // the sun and the moon look in over the left wing
    o.x = px(.37);
    o.y = Math.max(o.r * 1.25, py(eaveY(.37)) - o.r * 2.1);
  },
  paint(pace, s, o) {                                          // s, o: the picture and the one under it, where they show this place
    const ua = U * U * 1e6, leaves = (s && !s.traits.bare) || (o && !o.traits.bare);
    clipBand(0, H);
    for (let n = 0; n < 3; n++) spend('ih-land' + n, RATE.land * W * H / 3 / ua * pace, BANDS[n]);
    spend('ih-spot', RATE.spot * spotArea / ua * pace, spotDab);
    spend('ih-thing', RATE.thing * Math.max(0, Math.min(W, tx1) - Math.max(0, tx0)) * (ty1 - ty0) / ua * pace, thingDab);
    spend('ih-fern', RATE.fern * fernWeight / ua * pace, fernDab);
    spend('ih-limb', RATE.limb * limbLength / (14 * U) * pace, limbStroke);
    if (leaves) spend('ih-leaf', RATE.leaf * leafArea * 3 / ua * pace, leafDab);
    if ((s && s.traits.fog) || (o && o.traits.fog)) spend('ih-fog', RATE.fog * W * .32 * PH / ua * pace, fogStroke);
    if (s && s.traits.glint) spend('ih-glint', RATE.glint * W * (H - py(.7)) / ua * pace, glintDab);
    PP.haze = null;
    PP.c.restore();
  },
  coat(cells, i, j, s) {                                       // the land's share of a fresh coat: everything that is not sky
    const c = PP.c;
    use(s);
    c.save(); c.beginPath(); c.rect(0, 0, W, H); place.sky(c, 8 * U); c.clip('evenodd');
    for (let k = i; k < j; k++) for (let n = -1; n < 2; n++) coatStroke(cells[k].x + n * 22 * U, cells[k].y + n * 14 * U, false);   // thrice over: nothing of the wash under it is to show through
    if (!bare) for (let k = i; k < j; k++) if (inVine(cells[k].x, cells[k].y)) coatStroke(cells[k].x, cells[k].y, true);
    PP.haze = null;
    c.restore();
  },
  sprites: [{ draw: drawLamp, t0: 2, seed: 141 }],
};
return place;
} });
