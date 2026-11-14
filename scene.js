/* ------------------------------------------------------------------ *
 *  Vienna, seen from the Kahlenberg: a painting that never dries.
 *
 *  Sky, city, vineyards and the trees in front share one persistent
 *  canvas that is never cleared.  Every frame a few dozen dry-brush
 *  strokes and chalk dabs are sampled from the scene as it is right now
 *  (drifting clouds, cloud shadows on the fields, wind in the leaves,
 *  the palette of the season) and laid over whatever is already there.
 *  A new season, or day turning into night, is a fresh coat brushed
 *  across the old picture.  Only the few things that really move are
 *  redrawn from scratch each frame, on a second canvas on top.
 * ------------------------------------------------------------------ */
(() => {
'use strict';

const root = document.documentElement;
const qs = new URLSearchParams(location.search);

// ---------- theme: auto / night / day ----------
const THEME_KEY = 'ronacher-theme';
const MODES = ['auto', 'night', 'day'];
const darkQuery = window.matchMedia ? matchMedia('(prefers-color-scheme: dark)') : null;
let mode = 'auto';
try { mode = localStorage.getItem(THEME_KEY) || 'auto'; } catch (e) { /* private mode */ }
if (qs.get('theme')) mode = qs.get('theme');
if (!MODES.includes(mode)) mode = 'auto';
const wantNight = () => mode === 'auto' ? !!(darkQuery && darkQuery.matches) : mode === 'night';

const SEASONS = ['spring', 'summer', 'autumn', 'winter'];
// The weathers each season goes through; the first is the one it is known for.
const WEATHERS = {
  spring: ['fair', 'rain', 'sunny', 'cloudy'],
  summer: ['sunny', 'storm', 'fair', 'golden'],
  autumn: ['golden', 'fog', 'rain', 'cloudy'],
  winter: ['snow', 'frost', 'cloudy', 'fog'],
};
const WEATHER_NAMES = ['sunny', 'fair', 'cloudy', 'rain', 'storm', 'fog', 'golden', 'snow', 'frost'];
const SEASON_SECONDS = parseFloat(qs.get('every') || '60');
let seasonIndex = SEASONS.indexOf(qs.get('season'));
if (seasonIndex < 0) seasonIndex = [3, 3, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3][new Date().getMonth()];
let weatherIndex = 0, weather = WEATHERS[SEASONS[seasonIndex]][0];
if (WEATHER_NAMES.includes(qs.get('weather'))) { weather = qs.get('weather'); weatherIndex = Math.max(0, WEATHERS[SEASONS[seasonIndex]].indexOf(weather)); }

const themeButton = document.getElementById('theme-toggle');
const seasonButton = document.getElementById('season-toggle');
const weatherButton = document.getElementById('weather-toggle');
function announce() {
  const night = wantNight(), season = SEASONS[seasonIndex];
  root.classList.toggle('night', night);
  root.dataset.season = season; root.dataset.weather = weather;
  if (themeButton) {
    themeButton.textContent = mode;
    themeButton.setAttribute('aria-label', 'Color theme: ' + mode);
  }
  if (seasonButton) seasonButton.textContent = season;
  if (weatherButton) weatherButton.textContent = weather === 'golden' ? (night ? 'clear' : 'golden hour') : weather === 'sunny' && night ? 'clear' : weather;
  window.dispatchEvent(new CustomEvent('vienna:change', { detail: { season, weather, night, mode } }));
}
if (themeButton) themeButton.addEventListener('click', () => {
  mode = MODES[(MODES.indexOf(mode) + 1) % MODES.length];
  try { localStorage.setItem(THEME_KEY, mode); } catch (e) { /* ignore */ }
  announce(); retheme();
});
if (darkQuery) {
  const onScheme = () => { announce(); retheme(); };
  if (darkQuery.addEventListener) darkQuery.addEventListener('change', onScheme); else darkQuery.addListener(onScheme);
}
let retheme = () => {};
window.vienna = { get season() { return SEASONS[seasonIndex]; }, get weather() { return weather; }, get night() { return wantNight(); } };
announce();

const paintCv = document.getElementById('scene'), spriteCv = document.getElementById('sprites');
const pctx = paintCv && paintCv.getContext && paintCv.getContext('2d', { alpha: false });
const sctx = spriteCv && spriteCv.getContext && spriteCv.getContext('2d');
if (!pctx || !sctx) return;

const REDUCED = !!(window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches) && !qs.has('motion');
const SPEED = parseFloat(qs.get('speed') || '1');
const HOLD = qs.has('hold');

// ---------- helpers ----------
const E = Object.freeze({}), NODASH = [], TAU = Math.PI * 2;
const clamp = (x, a = 0, b = 1) => x < a ? a : x > b ? b : x;
const smooth = (a, b, x) => { x = clamp((x - a) / (b - a)); return x * x * (3 - 2 * x); };
const ease = x => x * x * (3 - 2 * x);
const hex = h => [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)];
const mix = (a, b, t) => [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t];
const u8 = v => v < 0 ? 0 : v > 255 ? 255 : v | 0;
const css3 = (r, g, b, a) => `rgba(${u8(r)},${u8(g)},${u8(b)},${a})`;
const css = (c, a = 1) => css3(c[0], c[1], c[2], a);
const rect = (x, y, w, h) => [[x, y], [x + w, y], [x + w, y + h], [x, y + h]];
const ell = (cx, cy, rx, ry, n = 12) => {
  const p = []; for (let i = 0; i < n; i++) { const a = i / n * TAU; p.push([cx + Math.cos(a) * rx, cy + Math.sin(a) * ry]); } return p;
};
const pick = (list, r) => list[(r * list.length) | 0];

function rng(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
function hash1(n) {
  n = Math.imul(n ^ (n >>> 15), 0x2c1b3c6d); n = Math.imul(n ^ (n >>> 12), 0x297a2d39);
  return ((n ^ (n >>> 15)) >>> 0) / 4294967296;
}

// value noise
const NP = new Uint8Array(512);
{ const r = rng(7), p = [...Array(256).keys()];
  for (let i = 255; i > 0; i--) { const j = (r() * (i + 1)) | 0; [p[i], p[j]] = [p[j], p[i]]; }
  for (let i = 0; i < 512; i++) NP[i] = p[i & 255]; }
const h2 = (x, y) => NP[(NP[x & 255] + y) & 255] / 255;
function vnoise(x, y) {
  const ix = Math.floor(x), iy = Math.floor(y), fx = x - ix, fy = y - iy;
  const u = fx * fx * (3 - 2 * fx), v = fy * fy * (3 - 2 * fy);
  const a = h2(ix, iy), b = h2(ix + 1, iy), c = h2(ix, iy + 1), d = h2(ix + 1, iy + 1);
  return a + (b - a) * u + (c - a) * v + (a - b - c + d) * u * v;
}
const fbm = (x, y) => vnoise(x, y) * .5 + vnoise(x * 2.03, y * 2.03) * .25 + vnoise(x * 4.1, y * 4.1) * .125 + vnoise(x * 8.2, y * 8.2) * .0625;

// ---------- palettes ----------
// Four seasons by day.  The nights are derived from them (see nightOf).
const DAY = {
  spring: {
    paper: '#e9e3d1',
    skyTop: '#5ca3e2', skyMid: '#92c5ee', skyLow: '#dcefF2', skyDark: '#488fd2', skyLight: '#ebf6f5', skyWarm: '#f6efe2',
    cloud: '#ffffff', cloudShade: '#cdd5e8', cloudDeep: '#b2bcd6',
    haze: '#93b3c8', cityFar: '#b4c6cf',
    city: ['#ece8dd', '#ddd6c8', '#c8705a', '#b65c49', '#aab2b6', '#f6f2e8', '#8d989e', '#d9c7a0'],
    cityGreen: '#86b257',
    water: '#68a2cf', waterLight: '#c2e2f2', waterDark: '#487dac',
    fieldA: '#a9d15a', fieldB: '#8bc04e', fieldC: '#ad9a66', fieldLight: '#d2e86e', fieldShadow: '#5c8c44', row: '#6d9c3f', earth: '#c2a878',
    folDeep: '#1c3a24', folDark: '#2f5c2c', folMid: '#62a23e', folLight: '#a5d250', folSun: '#f6cfdc',
    accent: ['#f8dce7', '#ffffff', '#f2b6ca', '#fbe9a8'],
    bits: ['#f8d3e0', '#ffffff', '#f4bfd0'],
  },
  summer: {
    paper: '#e9e3d1',
    skyTop: '#3f8de4', skyMid: '#6db2f0', skyLow: '#c4e3f6', skyDark: '#2f79d0', skyLight: '#d8edf9', skyWarm: '#eef3ea',
    cloud: '#ffffff', cloudShade: '#c6d3e6', cloudDeep: '#a9b9d2',
    haze: '#7d9fc6', cityFar: '#b4c5d3',
    city: ['#ece8dd', '#ddd6c8', '#c8705a', '#b65c49', '#aab2b6', '#f6f2e8', '#8d989e', '#d9c7a0'],
    cityGreen: '#5b8848',
    water: '#5a94cc', waterLight: '#b0d4f0', waterDark: '#3d6fa3',
    fieldA: '#86bb48', fieldB: '#5f9c3c', fieldC: '#417d35', fieldLight: '#b4d65c', fieldShadow: '#2c5c34', row: '#33662f', earth: '#c2ab72',
    folDeep: '#12261a', folDark: '#1f4023', folMid: '#3d6f2c', folLight: '#74a436', folSun: '#b3cc45',
    accent: null,
    bits: ['#ffffff', '#fbf6dc'],
  },
  autumn: {
    paper: '#e9e1cc',
    skyTop: '#6e92b8', skyMid: '#a8bccc', skyLow: '#f1dab2', skyDark: '#587ea6', skyLight: '#f7e8cb', skyWarm: '#f6cf9a',
    cloud: '#fcf2e1', cloudShade: '#cbb5b1', cloudDeep: '#aa969e',
    haze: '#a9a2ac', cityFar: '#c8bbb0',
    city: ['#efe6d6', '#e0d2bc', '#c56f55', '#b05743', '#b0aca6', '#f8efdf', '#94918f', '#dcc194'],
    cityGreen: '#b08a3a',
    water: '#8099ad', waterLight: '#ecdcb9', waterDark: '#56768e',
    fieldA: '#cdb24a', fieldB: '#bb8830', fieldC: '#9c5c2a', fieldLight: '#ebcc5c', fieldShadow: '#6a4a2c', row: '#7a4a26', earth: '#a98d5f',
    folDeep: '#2a1a15', folDark: '#52301d', folMid: '#b05a20', folLight: '#de902c', folSun: '#f2c640',
    accent: ['#b8352a', '#e9b824', '#8f9a45', '#cf4b2a'],
    bits: ['#d98a2a', '#b8452a', '#e6b83a', '#a8621f'],
  },
  winter: {
    paper: '#e8e6df',
    skyTop: '#5f93cc', skyMid: '#9fc1e2', skyLow: '#e1ebf0', skyDark: '#4a7fba', skyLight: '#eef4f6', skyWarm: '#f3ead9',
    cloud: '#ffffff', cloudShade: '#c2cde0', cloudDeep: '#a6b3cc',
    haze: '#a9bdd2', cityFar: '#ccd6e0',
    city: ['#f4f4f1', '#dde0e1', '#b68d86', '#9aa3aa', '#ffffff', '#7e8790', '#ecebe6', '#c9b8a8'],
    cityGreen: '#959f9e',
    water: '#6c8296', waterLight: '#bcc8d2', waterDark: '#4d6073',
    fieldA: '#f3f5f4', fieldB: '#e3e9ec', fieldC: '#d0d8de', fieldLight: '#ffffff', fieldShadow: '#a6b4c3', row: '#6e6862', earth: '#dcd9d0',
    folDeep: '#25242b', folDark: '#3d3a37', folMid: '#625d58', folLight: '#edf0f1', folSun: '#ffffff',
    accent: null,
    bits: ['#ffffff', '#f2f6fa'],
  },
};
const SHARED = {
  trunk: '#4a3f35', tower: '#46505e', towerLight: '#d5dbe0', glass: '#5f7f9f', dark: '#2b3442', stone: '#69645e',
  wheel: '#565d67', cabin: '#b3463a', gold: '#e2b23a', yellow: '#e2c64c', ink: '#2c3440', roof: '#a8523f', wall: '#ece4cf', hull: '#f4f1e6',
};
const GLOW = {
  star: '#f5f0dd', moon: '#f4efd9', moonShade: '#c8c3ae', beacon: '#ff5140', firefly: '#e6f08a',
  sun: '#fff7da', sunGlow: '#fff0c4', sunLow: '#ffe6a6', sunLowGlow: '#ffc068',
  lights: ['#ffd98a', '#ffbf66', '#fff3cf', '#ffa94d', '#ffe2a8'],
};
const NIGHT = {
  spring: { navy: '#0a1428', k: .76, land: .7, sky: ['#0b1630', '#14294b', '#2b4a69'], cloud: ['#35486a', '#1b2740', '#131c30'], haze: '#1b2c4a', glow: '#5b7399' },
  summer: { navy: '#0a1024', k: .76, land: .72, sky: ['#0a1230', '#131f48', '#2a3a6a'], cloud: ['#33405e', '#1a2238', '#121a2c'], haze: '#1c2848', glow: '#5a6a98' },
  autumn: { navy: '#100d1c', k: .76, land: .72, sky: ['#0d0f24', '#1b1a3a', '#3b2f50'], cloud: ['#3d3a55', '#211e36', '#17152a'], haze: '#272440', glow: '#75618a' },
  winter: { navy: '#18233a', k: .68, land: .46, sky: ['#141a27', '#232b3b', '#3d4758'], cloud: ['#48526a', '#2a3142', '#1f2533'], haze: '#2e3748', glow: '#77839b' },
};
function nightOf(day, n) {
  const navy = hex(n.navy);
  const dim = (c, k) => { const l = c[0] * .3 + c[1] * .55 + c[2] * .15; return mix(mix(c, [l, l, l], .35), navy, k); };
  const out = {};
  for (const key in day) {
    const v = day[key];
    const k = /^field|^earth|^row/.test(key) ? n.land : /^fol|^trunk/.test(key) ? n.k + .03 : /^water/.test(key) ? n.k + .04 : n.k;
    out[key] = v === null ? null : Array.isArray(v[0]) ? v.map(c => dim(c, k)) : dim(v, k);
  }
  [out.skyTop, out.skyMid, out.skyLow] = n.sky.map(hex);
  out.skyDark = mix(out.skyTop, [0, 0, 0], .35); out.skyLight = mix(out.skyMid, out.skyLow, .7); out.skyWarm = mix(out.skyLow, hex('#6a5a6a'), .4);
  [out.cloud, out.cloudShade, out.cloudDeep] = n.cloud.map(hex);
  out.haze = hex(n.haze); out.cityFar = mix(out.haze, out.skyLow, .4);
  out.paper = mix(navy, [0, 0, 0], .2); out.moonGlow = hex(n.glow);
  out.waterLight = mix(out.water, out.moonGlow, .7);
  out.hull = dim(hex(SHARED.hull), .45); out.wall = dim(hex(SHARED.wall), .6);
  return out;
}
const BASE = {};                                               // the four seasons in fair weather, by day
for (const s of SEASONS) {
  const d = BASE[s] = {}, src = Object.assign({}, SHARED, DAY[s], GLOW);
  for (const key in src) { const v = src[key]; d[key] = v === null ? null : Array.isArray(v) ? v.map(hex) : hex(v); }
}

// ---------- weather ----------
// What a weather does to the picture.  clouds: which sky (see CLOUDS); shadow: cloud shadows on
// the fields; cloudy: how busily clouds are painted; dim: how dull and dark the land gets;
// mist: how far the town sinks into the haze; warm: evening light; veiled: no sun, moon or stars.
const WEATHER = {
  sunny: { clouds: 'few', shadow: .5, cloudy: .6, orb: 'high' },
  fair: { clouds: 'cumulus', shadow: 1, cloudy: 1 },
  cloudy: { clouds: 'overcast', shadow: 0, cloudy: 1.3, dim: .12, veiled: true },
  rain: { clouds: 'heavy', shadow: 0, cloudy: 1.4, dim: .24, mist: .3, rain: 1, veiled: true },
  storm: { clouds: 'heavy', shadow: 0, cloudy: 1.4, dim: .38, mist: .25, rain: 1.7, gust: 1, lightning: true, veiled: true },
  fog: { clouds: 'stratus', shadow: 0, cloudy: 1, dim: .08, mist: .6, fog: 1, veiled: true },
  golden: { clouds: 'streaks', shadow: .7, cloudy: .9, warm: 1, orb: 'low' },
  snow: { clouds: 'overcast', shadow: 0, cloudy: 1.3, snow: 1, veiled: true },
  frost: { clouds: 'few', shadow: .6, cloudy: .6, orb: 'high' },
};
// grey skies by day: top, middle, low, then cloud, its shade, its deep shade
const SKIES = {
  cloudy: ['#8f9ba7', '#aeb7be', '#cdd2d2', '#eceeed', '#aab2bb', '#909aa7'],
  rain: ['#69757f', '#8a949c', '#b0b7b9', '#bcc3ca', '#808b97', '#677181'],
  storm: ['#3e4753', '#59626d', '#8f9392', '#8c96a2', '#55606f', '#3c4553'],
  fog: ['#b9bfc2', '#cbd0d0', '#dcdedb', '#e6e8e5', '#c2c7c8', '#b0b6ba'],
  snow: ['#aab6c2', '#c4cdd5', '#e4e7e7', '#f5f6f4', '#b9c1ca', '#a2abb8'],
};
const NIGHT_LIFT = { cloudy: .3, rain: .25, storm: .12, fog: .45, snow: .35 };   // how much the town lights up a covered night sky
const SEASON_TRAITS = {
  spring: { bare: false, shadow: .4, part: 'petal' },
  summer: { bare: false, shadow: .45, part: 'seed' },
  autumn: { bare: false, shadow: .3, part: 'leaf' },
  winter: { bare: true, shadow: .25, part: null },
};
const LAND = /^(city|water|field|row|earth|fol|accent|trunk|tower|glass|dark|stone|wheel|cabin|gold|yellow|roof|wall|hull)/;
const TOWN = /^(city|water|tower|glass|dark|stone|wheel)/;
const WHITE = [255, 255, 255], COOL = hex('#3d4856'), EVENING = hex('#f0a45a');
function recolor(p, test, fn) { for (const k in p) { const v = p[k]; if (v && test.test(k)) p[k] = Array.isArray(v[0]) ? v.map(fn) : fn(v); } }
function weatherDay(d, name) {
  const w = WEATHER[name], p = Object.assign({}, d), sky = SKIES[name];
  if (sky) {
    [p.skyTop, p.skyMid, p.skyLow, p.cloud, p.cloudShade, p.cloudDeep] = sky.map(hex);
    p.skyDark = mix(p.skyTop, [0, 0, 0], .18); p.skyLight = mix(p.skyMid, WHITE, .3); p.skyWarm = mix(p.skyLow, hex('#e9e2d2'), .4);
    p.haze = mix(d.haze, p.skyMid, .6); p.cityFar = mix(d.cityFar, p.skyLow, .5);
  }
  if (w.dim) recolor(p, LAND, c => { const l = c[0] * .3 + c[1] * .55 + c[2] * .15; return mix(mix(c, [l, l, l], w.dim * .9), COOL, w.dim); });
  if (w.mist) {
    p.haze = mix(p.haze, p.skyLow, w.mist); p.cityFar = mix(p.cityFar, p.skyLow, w.mist);
    recolor(p, TOWN, c => mix(c, p.skyLow, w.mist * .55));
  }
  if (w.warm) {                                                // the sun is low: peach sky, long warm light
    p.skyTop = mix(p.skyTop, hex('#6d88b8'), .5); p.skyMid = mix(p.skyMid, hex('#f1c9a2'), .6); p.skyLow = mix(p.skyLow, hex('#f7b46c'), .8);
    p.skyDark = mix(p.skyTop, [0, 0, 0], .15); p.skyLight = mix(p.skyMid, hex('#ffe6c4'), .5); p.skyWarm = hex('#fbcf8c');
    p.cloud = mix(p.cloud, hex('#ffe7cc'), .6); p.cloudShade = mix(p.cloudShade, hex('#d99a8c'), .6); p.cloudDeep = mix(p.cloudDeep, hex('#b07c88'), .6);
    p.haze = mix(p.haze, hex('#e6b08c'), .5); p.cityFar = mix(p.cityFar, hex('#f3c9a0'), .5);
    recolor(p, LAND, c => mix(c, EVENING, .16));
    p.fieldLight = mix(p.fieldLight, hex('#ffd77a'), .35); p.folSun = mix(p.folSun, hex('#ffd060'), .4); p.waterLight = mix(p.waterLight, hex('#ffd9a0'), .6);
  }
  p.fog = mix(p.skyLow, WHITE, .25); p.rain = mix(p.skyLight, WHITE, .5); p.flake = WHITE; p.moonGlow = p.skyLight;
  return p;
}
function weatherNight(n, name) {
  const w = WEATHER[name], lift = NIGHT_LIFT[name];
  if (lift) {
    const g = hex('#525a69');
    n.skyTop = mix(n.skyTop, g, lift * .7); n.skyMid = mix(n.skyMid, g, lift); n.skyLow = mix(n.skyLow, hex('#6c7079'), lift);
    n.skyDark = mix(n.skyTop, [0, 0, 0], .3); n.skyLight = mix(n.skyMid, n.skyLow, .7);
    n.cloud = mix(n.cloud, hex('#667083'), lift); n.cloudShade = mix(n.cloudShade, hex('#454d5c'), lift); n.cloudDeep = mix(n.cloudDeep, hex('#363d4a'), lift);
    n.haze = mix(n.haze, n.skyMid, .5); n.cityFar = mix(n.haze, n.skyLow, .4);
  }
  if (w.mist) { n.haze = mix(n.haze, n.skyLow, w.mist); n.cityFar = mix(n.cityFar, n.skyLow, w.mist); recolor(n, TOWN, c => mix(c, n.skyLow, w.mist * .5)); }
  n.fog = mix(n.skyLow, hex('#8a93a3'), .4); n.rain = mix(n.skyLow, hex('#c3cfe3'), .7); n.flake = mix(n.skyLow, hex('#e6ecf6'), .8);
  return n;
}
const palettes = {};
function paletteOf(season, name, isNight) {
  const key = season + '/' + name + (isNight ? '/night' : '');
  if (palettes[key]) return palettes[key];
  const d = weatherDay(BASE[season], name);
  if (!isNight) return palettes[key] = d;
  const n = nightOf(d, NIGHT[season]);
  for (const k in GLOW) n[k] = BASE[season][k];
  return palettes[key] = weatherNight(n, name);
}
// skies: [x, base (0 top .. 1 horizon), width, height, drift, seed] for every cloud
const OVERCAST = [[.2, .4, .6, .2, 4, 83], [.75, .5, .6, .24, 3.5, 89], [.45, .8, .7, .16, 3, 97], [.05, .85, .4, .12, 3, 101], [.9, .2, .5, .12, 5, 103], [.5, .18, .5, .1, 5, 107]];
const STREAKS = [[.3, .42, .5, .05, 5, 61], [.8, .6, .55, .06, 4, 67], [.1, .75, .4, .04, 3, 71], [.6, .22, .36, .035, 7, 73], [.5, .9, .6, .04, 2.5, 79]];
const CLOUDS = {
  few: [[.76, .2, .075, .035, 5, 11], [.12, .93, .2, .018, 2.5, 23], [.5, .96, .16, .014, 2.5, 31], [.9, .95, .14, .014, 2.5, 37]],
  cumulus: [[.70, .66, .34, .24, 7.5, 11], [.14, .5, .26, .15, 9, 23], [.42, .24, .17, .06, 12, 31], [.30, .82, .2, .05, 5, 37],
    [.95, .88, .26, .07, 4, 41], [.04, .9, .23, .06, 4, 47], [.62, .11, .2, .06, 13, 53]],
  streaks: STREAKS,
  overcast: OVERCAST,
  heavy: OVERCAST.concat([[.35, .62, .7, .22, 6, 109], [.85, .92, .6, .14, 5, 113], [.15, .2, .5, .16, 8, 127]]),
  stratus: STREAKS.concat([[.5, .98, .9, .07, 1.5, 131], [.2, .55, .6, .06, 2, 137]]),
};

// ---------- view state (CSS pixels) ----------
let W = 0, H = 0, SCALE = 1, U = 1;         // U: stroke unit, so a phone gets the same picture as a desktop
let HZ = 0, BAND = 0, HILL = 0, LX = 1;     // horizon, depth of the city plain, depth of the hills, hill wavelength
let time = 0;

// ---------- the brush ----------
class Painter {
  constructor(c, seed) { this.c = c; this.reseed(seed); this.haze = null; }
  reseed(s) { this.R = rng(s); this.n = 0; this.limit = Infinity; }
  col(c) { return this.haze ? mix(c, this.haze[0], this.haze[1]) : c; }
  jit(c, a) {
    const R = this.R, d = (R() - .5) * 2 * a;
    return [c[0] + d + (R() - .5) * a * .7, c[1] + d + (R() - .5) * a * .7, c[2] + d + (R() - .5) * a * .7];
  }

  // dry brush: a bundle of ragged, slightly bowed bristle lines
  brush(x1, y1, x2, y2, w, col, o = E) {
    if (this.n++ >= this.limit) return;
    const c = this.c, R = this.R;
    col = this.col(col);
    const a = o.a ?? .8, jit = o.jit ?? 10, dry = o.dry ?? .18, sp = o.sp ?? 1.5 * U, dash = o.dash || 0;
    const dx = x2 - x1, dy = y2 - y1, len = Math.hypot(dx, dy) || 1, nx = -dy / len, ny = dx / len;
    const n = Math.max(1, Math.round(w / sp));
    const bow = (R() - .5) * len * (o.bow ?? .08);
    const base = this.jit(col, jit);
    for (let i = 0; i < n; i++) {
      const r1 = R(), r2 = R(), r3 = R(), r4 = R();
      if (n > 2 && r1 < dry) continue;
      const e = n > 1 ? i / (n - 1) - .5 : 0;
      const off = e * w + (r2 - .5) * sp;
      const tp = e * e * .5;
      const s0 = tp + r3 * r3 * .22, s1 = 1 - tp - r4 * r4 * .22;
      const ax = x1 + dx * s0 + nx * off, ay = y1 + dy * s0 + ny * off;
      const bx = x1 + dx * s1 + nx * off, by = y1 + dy * s1 + ny * off;
      const k = (r2 - .5) * jit * 1.6;
      c.strokeStyle = css3(base[0] + k, base[1] + k, base[2] + k, a * (.5 + r1 * .5));
      c.lineWidth = sp * (o.full ? 1.25 + r3 * .5 : .7 + r3 * .9);
      if (dash) c.setLineDash([3 + r4 * dash, 1 + r3 * dash * .35]);
      c.beginPath(); c.moveTo(ax, ay);
      if (bow) c.quadraticCurveTo((ax + bx) / 2 + nx * bow, (ay + by) / 2 + ny * bow, bx, by); else c.lineTo(bx, by);
      c.stroke();
    }
    if (dash) c.setLineDash(NODASH);
  }

  // chalk: a ragged blob plus a spray of grain
  dab(x, y, r, col, o = E) {
    if (this.n++ >= this.limit) return;
    const c = this.c, R = this.R, sx = o.sx ?? 1, sy = o.sy ?? 1;
    col = this.jit(this.col(col), o.jit ?? 6);
    if (o.core) {
      c.fillStyle = css(col, o.core); c.beginPath();
      for (let i = 0; i < 7; i++) {
        const an = i / 7 * TAU + R() * .8, q = r * .72 * (.6 + R() * .65);
        const px = x + Math.cos(an) * q * sx, py = y + Math.sin(an) * q * sy;
        i ? c.lineTo(px, py) : c.moveTo(px, py);
      }
      c.closePath(); c.fill();
    }
    c.fillStyle = css(col, o.a ?? .85);
    const ru = r / U, n = Math.round((o.den ?? .5) * ru * ru * .5) + 3, s0 = o.s ?? 2.4 * U;
    c.beginPath();
    for (let i = 0; i < n; i++) {
      const an = R() * TAU, d = r * Math.pow(R(), .6), s = .4 + R() * R() * s0 * .55;
      const px = x + Math.cos(an) * d * sx, py = y + Math.sin(an) * d * sy, rot = R() * 3.14;
      c.moveTo(px + Math.cos(rot) * s, py + Math.sin(rot) * s);
      c.ellipse(px, py, s, s * (.45 + R() * .5), rot, 0, TAU);
    }
    c.fill();
  }

  // pencil: a wobbly line, drawn over itself
  line(pts, col, w = 1.2, o = E) {
    if (this.n++ >= this.limit) return;
    const c = this.c, R = this.R;
    col = this.col(col);
    const wob = o.wob ?? .7 * U, step = o.step ?? 14 * U, a = o.a ?? .88, passes = o.pass ?? 2;
    const q = [];
    for (let i = 0; i < pts.length - 1; i++) {
      const x1 = pts[i][0], y1 = pts[i][1], x2 = pts[i + 1][0], y2 = pts[i + 1][1];
      const n = Math.max(1, Math.ceil(Math.hypot(x2 - x1, y2 - y1) / step));
      for (let k = 0; k < n; k++) { const f = k / n; q.push(x1 + (x2 - x1) * f, y1 + (y2 - y1) * f); }
    }
    q.push(pts[pts.length - 1][0], pts[pts.length - 1][1]);
    const m = q.length / 2;
    for (let p = 0; p < passes; p++) {
      c.strokeStyle = css(this.jit(col, o.jit ?? 8), a * (.6 + R() * .4));
      c.lineWidth = w * (.75 + R() * .5);
      c.beginPath();
      c.moveTo(q[0] + (R() - .5) * wob, q[1] + (R() - .5) * wob);
      for (let i = 1; i < m; i++) {
        const x = q[i * 2] + (R() - .5) * 2 * wob, y = q[i * 2 + 1] + (R() - .5) * 2 * wob;
        if (o.sharp || i === m - 1) c.lineTo(x, y);
        else c.quadraticCurveTo(x, y, (x + q[i * 2 + 2]) / 2, (y + q[i * 2 + 3]) / 2);
      }
      c.stroke();
    }
  }

  path(pts, smoothed, wob = 0) {
    const c = this.c, R = this.R, n = pts.length;
    const q = pts.map(p => [p[0] + (R() - .5) * 2 * wob, p[1] + (R() - .5) * 2 * wob]);
    c.beginPath();
    if (smoothed) {
      c.moveTo((q[n - 1][0] + q[0][0]) / 2, (q[n - 1][1] + q[0][1]) / 2);
      for (let i = 0; i < n; i++) { const b = q[(i + 1) % n]; c.quadraticCurveTo(q[i][0], q[i][1], (q[i][0] + b[0]) / 2, (q[i][1] + b[1]) / 2); }
    } else { c.moveTo(q[0][0], q[0][1]); for (let i = 1; i < n; i++) c.lineTo(q[i][0], q[i][1]); }
    c.closePath();
  }

  // a flat-ish fill that is then hatched over with brush strokes
  shape(pts, col, o = E) {
    if (this.n++ >= this.limit) return;
    const c = this.c, R = this.R;
    c.save();
    this.path(pts, o.smooth, o.wob ?? .6 * U);
    const base = o.base ?? .94;
    if (base > 0) { c.fillStyle = css(this.jit(this.col(col), o.jit ?? 5), base); c.fill(); }
    const hd = o.hd ?? 1;
    if (hd > 0) {
      c.clip();
      let x0 = 1e9, x1 = -1e9, y0 = 1e9, y1 = -1e9;
      for (const p of pts) { if (p[0] < x0) x0 = p[0]; if (p[0] > x1) x1 = p[0]; if (p[1] < y0) y0 = p[1]; if (p[1] > y1) y1 = p[1]; }
      const ang = (o.ang ?? -.4) + (R() - .5) * .12, hw = o.hw ?? 6 * U, hl = o.hl ?? 30 * U;
      const n = Math.min(o.maxN ?? 40, Math.max(2, Math.round(hd * (x1 - x0) * (y1 - y0) / (hw * hl) * 1.2)));
      const ca = Math.cos(ang), sa = Math.sin(ang), cols = o.cols;
      for (let i = 0; i < n; i++) {
        const x = x0 + R() * (x1 - x0), y = y0 + R() * (y1 - y0), l = hl * (.5 + R()) / 2;
        const cc = cols ? cols[(R() * cols.length) | 0] : col;
        this.brush(x - ca * l, y - sa * l, x + ca * l, y + sa * l, hw * (.5 + R()), cc, { a: o.ha ?? .45, jit: o.hj ?? 12, dry: .3 });
      }
    }
    c.restore();
    if (o.edge) this.line(pts.concat([pts[0]]), o.edge, o.ew ?? 1.1 * U, { a: .7, pass: 1, step: o.smooth ? 999 : 14 * U });
  }
}

const PP = new Painter(pctx, 3);                      // the painting
const VP = new Painter(sctx, 1);                      // whatever floats over it

// paper grain, laid over everything by CSS (and reused by the postcard)
{ const n = 256, g = document.createElement('canvas'); g.width = g.height = n;
  const x = g.getContext('2d'), id = x.createImageData(n, n), d = id.data, R = rng(42);
  for (let i = 0; i < d.length; i += 4) {
    const v = 128 + (R() - .5) * (R() < .25 ? 150 : 70);
    d[i] = d[i + 1] = d[i + 2] = v; d[i + 3] = 255;
  }
  x.putImageData(id, 0, 0);
  try { root.style.setProperty('--grain', `url(${g.toDataURL('image/png')})`); } catch (e) { /* ignore */ } }

// ---------- scenes: the picture as it is, and the one it was ----------
let cur = null, old = null, coat = null, boostUntil = 0, coatN = 0;
let pal = null, clouds = [], night = false, traits = SEASON_TRAITS.summer, orb = null;   // what sceneAt() last looked at

function mkScene(season, name, isNight) {
  const w = WEATHER[name], se = SEASON_TRAITS[season];
  const s = {
    season, weather: name, night: isNight, pal: paletteOf(season, name, isNight), orb: null,
    traits: {
      bare: se.bare, part: w.rain ? 'rain' : w.snow ? 'snow' : se.part, shadow: se.shadow * w.shadow, cloudy: w.cloudy,
      rain: w.rain || 0, snow: w.snow || 0, gust: w.gust || 0, fog: w.fog || 0, lightning: !!w.lightning, veiled: !!w.veiled,
    },
    clouds: CLOUDS[w.clouds].map(l => ({ l, fx: l[0] })),
  };
  layoutScene(s);
  return s;
}
function use(s) { pal = s.pal; clouds = s.clouds; night = s.night; traits = s.traits; orb = s.orb; return s; }
// While a coat is being brushed across, everything ahead of the brush still belongs to the old picture.
function sceneAt(x, y) {
  let s = cur;
  if (coat && x * coat.ax + y * coat.ay + coat.J > coat.front) s = old;
  return s ? use(s) : null;
}

// ---------- geometry ----------
const wind = t => Math.sin(t * 1.7) * .5 + Math.sin(t * 2.9 + 1) * .3 + Math.sin(t * .6) * .4;
// Everything below is measured on the photograph the picture is painted after.  Heights
// are fractions of the photo's height: horizon .419, the foot of the city plain .522.
// A wide screen shows a slice of it (Y0..Y1), losing some sky and some of the trees in front.
let Y0 = 0, Y1 = 1;
const py = f => (f - Y0) / (Y1 - Y0) * H;
const TOPS = [[0, .49], [.06, .475], [.12, .5], [.2, .54], [.28, .585], [.34, .66], [.4, .725], [.5, .745], [.6, .74], [.7, .735], [.8, .73], [.9, .735], [1, .745]];
function treeTop(xf) {                                         // where the trees in front begin, as a fraction of the screen height
  xf = clamp(xf);
  let v = TOPS[TOPS.length - 1][1];
  for (let i = 0; i < TOPS.length - 1; i++) if (xf <= TOPS[i + 1][0]) {
    const a = TOPS[i], b = TOPS[i + 1]; v = a[1] + (b[1] - a[1]) * ease((xf - a[0]) / (b[0] - a[0])); break;
  }
  return (v - Y0) / (Y1 - Y0);
}
// where the town ends and the vineyards begin: level on the left, the garden suburbs reach further down on the right
const ridgeY = x => { const xf = x / W; return HZ + BAND * (1 + smooth(.5, .85, xf) * .4 + smooth(.8, 1.05, xf) * .14 + .03 * Math.sin(xf * 9 + 1) + .015 * Math.sin(xf * 23)); };
const HT = [null, { b: .13, a: .05, f: 4.2, p: 2.1 }, { b: .3, a: .07, f: 3.1, p: 4 }, { b: .52, a: .07, f: 2.3, p: .7 }];
function hillTop(i, x) {
  if (!i) return ridgeY(x);
  const h = HT[i], q = x / LX;
  return HZ + BAND + HILL * (h.b + h.a * Math.sin(q * h.f + h.p) + h.a * .3 * Math.sin(q * h.f * 2.3 + h.p * 1.7));
}
const farH = x => (1.5 + 3 * vnoise(x * .004 / U, 3.3) + 1 * vnoise(x * .02 / U, 9.1)) * U;      // the hills beyond the city
const shadow = (x, y) => traits.shadow && !night ? traits.shadow * smooth(.5, .7, fbm((x + time * 14 * U) * .0018 / U, y * .005 / U + 40)) : 0;

// The Danube comes towards us from the far end of the plain and turns left along its foot.
// [x, centre, half width]: x as a fraction of the width, the others of the plain's depth.
const RIVER = [[0, .86, .07], [.1, .85, .075], [.19, .8, .08], [.235, .68, .06], [.27, .58, .05], [.295, .46, .07], [.315, .35, .075], [.33, .26, .04], [.345, .2, .015], [.36, .17, 0]];
const ARM = [[0, .68, .03], [.09, .65, .03], [.145, .55, .03], [.2, .42, .02], [.235, .31, .025], [.3, .29, .022], [.315, .27, 0]];
let chC = 0, chW = 0;
function channel(pts, xf) {
  if (xf >= pts[pts.length - 1][0]) { chW = 0; return false; }
  for (let i = 0; i < pts.length - 1; i++) if (xf <= pts[i + 1][0]) {
    const a = pts[i], b = pts[i + 1], t = (xf - a[0]) / (b[0] - a[0]);
    chC = a[1] + (b[1] - a[1]) * t; chW = a[2] + (b[2] - a[2]) * t; return true;
  }
  return false;
}
function riverAt(x, f) {                                       // 0 land, 1 the river, 2 the New Danube
  const xf = Math.max(0, x / W);
  if (channel(RIVER, xf) && Math.abs(f - chC) < chW) return 1;
  return channel(ARM, xf) && Math.abs(f - chC) < chW ? 2 : 0;
}

// the vineyards: rolling ridges, one behind the other, each cut into fields
const hs = { li: 0, id: 0, type: 0, ang: 0, lum: 0, hedge: false, path: false, h: 0, slope: 0 };
function hillAt(x, y) {
  let li = 0; for (let i = 3; i >= 1; i--) if (y >= hillTop(i, x)) { li = i; break; }
  const d = y - hillTop(li, x), e = 6 * U;
  const slope = (hillTop(li, x + e) - hillTop(li, x - e)) / (2 * e);
  const fw = (70 + li * 45) * U, fh = (34 + li * 30) * U, skew = [.7, -.55, .45, -.4][li];
  const warp = (vnoise(x * .004 / U + li * 7, y * .006 / U) - .5) * 90 * U;
  const id = Math.floor((x + d * skew * 2 + warp) / fw) * 31 + Math.floor((d + warp * .3) / fh) * 17 + li * 101;
  const h = hash1(id * 7 + 3), hb = hash1(id * 13 + 5), hc = hash1(id * 29 + 11);
  hs.li = li; hs.id = id; hs.h = hb; hs.slope = slope;
  hs.type = h < .72 ? 0 : h < .93 ? 1 : 2;                     // vines, meadow, bare earth
  hs.ang = Math.atan(slope) + (hs.type === 0 ? (hc - .5) * 2.4 : 0);
  // light: the lie of the land, sun on the crest, each field its own tone, clouds passing over
  hs.lum = clamp(.52 + slope * 1.8 + (fbm(x * .003 / U + li * 3.7, y * .007 / U) - .47) * 1.2 + .1 - d / (HILL * .22) * .16 + (hb - .5) * .3 - shadow(x, y));
  hs.hedge = d < (7 + li * 6) * U * (.5 + vnoise(x * .03 / U, li * 9.1)) && vnoise(x * .007 / U + li * 13.7, 2.2) > .38;
  if (!hs.hedge) {                                             // the wooded ridge on the right, the trees around the farm
    const xf = x / W, pf = Y0 + y / H * (Y1 - Y0);
    hs.hedge = (xf > .6 && Math.abs(pf - (.548 + (xf - .62) * .25)) < (.008 + (xf - .6) * .09) * (.6 + .8 * vnoise(x * .02 / U, 5.5))) ||
      Math.hypot((xf - .53) / .04, (pf - .578) / .011) < .6 + .8 * vnoise(x * .05 / U, y * .05 / U);
  }
  hs.path = !hs.hedge && Math.abs(d - (30 + li * 26 + Math.sin(x / LX * 9 + li * 2.4) * (9 + li * 4)) * U) < (1.3 + li * .6) * U;
}
function fieldColor() {
  let c = hs.type === 0 ? mix(pal.fieldC, pal.fieldB, .35 + hs.h * .65) : hs.type === 1 ? mix(pal.fieldA, pal.fieldLight, hs.h * .6) : mix(pal.fieldB, pal.earth, .35 + hs.h * .45);
  c = hs.lum > .5 ? mix(c, pal.fieldLight, (hs.lum - .5) * .9) : mix(c, pal.fieldShadow, (.5 - hs.lum) * 1.5);
  return mix(c, pal.haze, (3 - hs.li) * .07);
}

// the trees in front: a heap of crowns over a solid mass
let crowns = [], folB = 0, treeLine = 0;
function buildCrowns() {
  crowns = []; const r = rng(5);
  let x = -30 * U;
  while (x < W + 60 * U) {
    const rad = (26 + r() * 34) * U, top = treeTop(x / W) * H;
    crowns.push({ x, y: top + rad * (.95 + r() * .5), r: rad });
    if (r() < .6) crowns.push({ x: x + (r() - .5) * rad, y: top + rad * (2 + r()), r: rad * (.8 + r() * .5) });
    x += rad * (.7 + r() * .5);
  }
  treeLine = H; for (let i = 0; i <= 40; i++) treeLine = Math.min(treeLine, treeTop(i / 40) * H);
  treeLine -= 14 * U;
}
function fol(x, y) {                                           // > 0 inside the foliage
  if (y < treeLine) return -1;
  const base = treeTop(x / W) * H;
  if (y < base - 14 * U) return -1;
  let d = Math.min(.6, (y - base - 50 * U) / (40 * U));
  folB = 0;
  const sw = wind(time) * 3 * U;
  for (const c of crowns) {
    const dx = x - c.x - sw; if (dx > c.r || dx < -c.r) continue;
    const dy = y - c.y, q = 1 - Math.hypot(dx, dy * 1.1) / c.r;
    if (q > d) { d = q; folB = dy / c.r; }
  }
  return d * 1.2 + (fbm(x * .02 / U + time * .02, y * .02 / U) - .47) * .7;
}
function folLum(x, y) {                                        // light falls in from the upper right
  const s = time * .03, k = .011 / U;
  const rel = fbm(x * k + s, y * k * 1.2) - fbm((x + 13 * U) * k + s, (y - 13 * U) * k * 1.2);
  return clamp(.42 + rel * 3.4 - folB * .25 + (smooth(.1, .55, x / W) - .5) * .3 + (y / H - .8) * .25);
}

// ---------- sky ----------
function skyCol(y) {
  const t = clamp(y / HZ);
  return t < .6 ? mix(pal.skyTop, pal.skyMid, t / .6) : mix(pal.skyMid, pal.skyLow, (t - .6) / .4);
}
// a cumulus is a heap of puffs sitting on a flat base
function layoutScene(s) {
  const w = WEATHER[s.weather], p = s.pal;                     // the moon, or the sun high up or low over the town
  s.orb = w.veiled ? null : s.night ? { x: W * .8, y: HZ * .3, r: 20 * U, moon: true, col: p.moon, glow: p.moonGlow, k: .4, spread: 40 }
    : w.orb === 'high' ? { x: W * .84, y: HZ * .24, r: 17 * U, col: p.sun, glow: p.sunGlow, k: .55, spread: 70 }
    : w.orb === 'low' ? { x: W * .89, y: HZ * .74, r: 22 * U, col: p.sunLow, glow: p.sunLowGlow, k: .8, spread: 150 } : null;
  const short = Math.min(W, HZ * 2.4);
  for (const c of s.clouds) {
    const l = c.l, R = rng(l[5]), w = c.w = l[2] * short, h = c.h = l[3] * short, puffs = c.puffs = [];
    c.reach = w / 2 + 40 * U; c.ns = l[5] % 89; c.base = l[1] * HZ; c.x = c.fx * W;
    const rows = Math.max(1, Math.round(h / (w * .2 + 18 * U)));
    for (let k = 0; k < rows; k++) {
      const f = rows > 1 ? k / (rows - 1) : 0;
      const hwid = w / 2 * (1 - Math.pow(f, 1.25) * .78);
      const pr = (h / rows) * (.7 + R() * .3) + 8 * U;
      const n = Math.max(1, Math.round(hwid * 2 / (pr * 1.05)));
      const lean = f * w * .16;
      for (let i = 0; i < n; i++) puffs.push({
        x: (n > 1 ? (i / (n - 1) - .5) : 0) * 2 * Math.max(0, hwid - pr * .6) + lean + (R() - .5) * pr * .6,
        y: -pr * .75 - f * Math.max(0, h - pr * 1.7) + (R() - .5) * pr * .35,
        r: pr * (.8 + R() * .45),
      });
    }
    for (let i = 0, n = puffs.length; i < n; i++) {             // small billows riding on the big ones
      const p = puffs[i];
      for (let j = 0; j < 3; j++) {
        const a = -Math.PI * (.08 + .84 * R()), r = p.r * (.3 + R() * .28);
        puffs.push({ x: p.x + Math.cos(a) * p.r * .8, y: p.y + Math.sin(a) * p.r * .72, r });
      }
    }
  }
}
function moveClouds(dt) {
  for (const s of [cur, old]) if (s) for (const c of s.clouds) {
    c.fx -= c.l[4] * U * dt / W; c.x = c.fx * W;
    if (c.x + c.reach < -70) { c.fx += (W + 2 * c.reach + 140) / W; c.x = c.fx * W; }
  }
}
let cdV = 0;                                                   // how far down its billow the last sample was
function cloudD(c, x, y) {
  const lx = x - c.x, ly = y - c.base;
  if (lx < -c.reach - 30 * U || lx > c.reach + 60 * U || ly > 30 * U || ly < -c.h - 60 * U) return -1;
  let d = -1;
  for (const p of c.puffs) { const q = 1 - Math.hypot(lx - p.x, (ly - p.y) * 1.12) / p.r; if (q > d) { d = q; cdV = (ly - p.y) / p.r; } }
  d = d * 1.25 + (fbm(lx * .016 / U + c.ns, ly * .016 / U + time * .012) - .47) * .8;
  if (ly > -12 * U) d -= (ly + 12 * U) / (13 * U);
  return d;
}
function dens(x, y) { let b = -1; for (const c of clouds) { const d = cloudD(c, x, y); if (d > b) b = d; } return b; }

const STARS = (() => {
  const r = rng(301), s = [];
  for (let i = 0; i < 520; i++) { const v = Math.pow(r(), 1.25); s.push({ u: r(), v, size: .55 + r() * r() * 1.3, glow: (.45 + r() * .55) * (1 - v * .45) }); }
  return s;
})();

// one streak of sky, wherever there is no cloud (any more)
function skyStreak() {
  const R = PP.R; let x = R() * (W + 160 * U) - 80 * U, y = R() * (HZ + 10 * U) - 15 * U;
  const s = sceneAt(x, y); if (!s) return;
  if (clouds.length && R() < .45) {                            // aim around a billow: that is where a drifting cloud leaves a trail
    const c = pick(clouds, R()), p = pick(c.puffs, R());
    const a = R() * TAU, rr = p.r * (.85 + R() * .75);
    x = c.x + p.x + Math.cos(a) * rr + R() * 14 * U; y = c.base + p.y + Math.sin(a) * rr;
    if (sceneAt(x, y) !== s) return;
  }
  if (y > HZ - farH(x) - U) return;
  let glow = 0;
  if (orb) { const dm = Math.hypot(x - orb.x, y - orb.y) / orb.r; if (dm < 1.35) return; glow = Math.exp(-dm * dm / orb.spread) * orb.k; }
  const d = dens(x, y);
  if (d > .2) return;
  const rim = d > -.07;                                        // inside the ragged rim: only a few thin scratches
  if (rim && R() > (.2 - d) / .27 * .7) return;
  const below = dens(x, y - 36 * U) > .1;                      // in the shadow under a cloud base
  let ang = -.36 + (R() - .5) * .26, l = (40 + R() * 130) / 2 * U, w = (4 + R() * R() * 24) * U;
  if (rim) { l = (5 + R() * 12) * U; w = (2 + R() * 4) * U; }
  if (y > HZ - 40 * U) { ang *= .15; l *= .6; w = Math.min(w, 7 * U); }   // level and thin towards the horizon
  let ca = Math.cos(ang) * l, sa = Math.sin(ang) * l;
  if (!rim && (dens(x + ca, y + sa) > .12 || dens(x - ca, y - sa) > .12)) { l *= .3; w = Math.min(w, 10 * U); ca = Math.cos(ang) * l; sa = Math.sin(ang) * l; }
  let col = skyCol(y), a = .55, dash = 16 * U; const k = R();
  if (below && k < .65) { col = mix(col, k < .35 ? pal.cloudShade : pal.cloudDeep, .7); ca = l; sa = -l * .12; w *= 1.3; }
  else if (k < .24) { col = mix(col, pal.skyDark, .5); w = (2 + R() * 7) * U; a = .5; dash = 9 * U; }
  else if (k < .42) col = mix(col, pal.skyLight, .5);
  else if (y > HZ * .78 && k < .6) { col = mix(col, pal.skyWarm, .7); w = (3 + R() * 6) * U; sa *= .2; }
  if (glow) col = mix(col, orb.glow, glow);
  PP.brush(x - ca, y - sa, x + ca, y + sa, w, col, { a, jit: 7, dry: .32, dash, bow: .06 });
}

// one dab of cloud, wherever a cloud is (now)
function cloudDab() {
  const R = PP.R; if (!cur.clouds.length) return;
  const set = coat && old && R() < .5 ? old.clouds : cur.clouds;
  const c = set[(R() * R() * set.length) | 0], p = pick(c.puffs, R());
  const a = R() * TAU, rr = p.r * Math.sqrt(R()) * 1.15;
  const x = c.x + p.x + Math.cos(a) * rr, y = c.base + p.y + Math.sin(a) * rr;
  const s = sceneAt(x, y); if (!s || s.clouds !== set) return;
  if (y > HZ - farH(x) - 2 * U) return;
  const d = cloudD(c, x, y), v = cdV;
  if (d < -.1) return;
  const lx = x - c.x, ly = y - c.base, big = Math.min(1.5, .6 + c.h / (220 * U));
  if (d < .22) {                                               // the ragged rim is only grain, no body
    PP.dab(x, y, (4 + R() * 9) * U, pal.cloud, { a: .88, den: .2 + d * 1.3, s: 4.2 * U, sx: 1.5, sy: .75 });
    return;
  }
  // tone: the underside of every billow and the flat base sit in shade
  let sh = clamp(v * .6 + .15) * .55 + smooth(-.4 * c.h - 20 * U, -4 * U, ly) * .8;
  sh = clamp(sh * (.35 + 1.5 * fbm(lx * .012 / U + 3, ly * .025 / U + c.ns)) - .12);
  const k = R();
  if (k < .06) { PP.dab(x, y, (5 + R() * 8) * U, skyCol(y), { a: .45, den: .14, s: 3 * U }); return; }   // sky peeping through
  if (sh > .42 && k < .55) {
    const l = (10 + R() * 28) * U;
    PP.brush(x - l, y + l * .14, x + l, y - l * .14, (4 + R() * 9) * U, mix(pal.cloud, R() < .5 ? pal.cloudShade : pal.cloudDeep, sh * .95), { a: .6, dry: .3, dash: 12 * U, jit: 6 });
  } else {
    const col = sh < .12 ? pal.cloud : mix(pal.cloud, pal.cloudShade, sh * .8);
    PP.dab(x, y, (5 + R() * 11 * big) * U, col, { a: .9, den: .55, s: 3.2 * U, core: .3, sx: 1.25, sy: .85, jit: 5 });
  }
}

function starDab() {
  const R = PP.R, n = Math.max(60, Math.min(STARS.length, Math.round(W * HZ / (3600 * U * U))));
  const s = STARS[(R() * n) | 0], x = s.u * W, y = s.v * (HZ - 16 * U);
  if (!sceneAt(x, y) || !night || traits.veiled) return;
  if (dens(x, y) > -.05 || (orb && Math.hypot(x - orb.x, y - orb.y) < orb.r * 1.5)) return;
  PP.dab(x, y, s.size * U, pal.star, { a: s.glow, den: 2.5, s: 1.3 * U, core: s.glow, jit: 3 });
}
function orbDab() {                                            // the moon, or the sun
  const R = PP.R, scene = coat && old && R() < .5 ? old : cur, o = scene.orb;
  if (!o) return;
  const a = R() * TAU, d = Math.sqrt(R()) * o.r * .86, x = o.x + Math.cos(a) * d, y = o.y + Math.sin(a) * d;
  if (sceneAt(x, y) !== scene || dens(x, y) > .25) return;
  const mare = o.moon && fbm(x * .05 / U + 5, y * .05 / U) > .56;
  PP.dab(x, y, (2 + R() * 2.5) * U, mare ? pal.moonShade : o.col, { a: .92, den: .9, core: .7, s: 1.6 * U, jit: 4 });
}
// fog lies over the plain in drifting banks
function fogStroke() {
  const R = PP.R, x = R() * W, depth = R(), y = HZ - 12 * U + depth * BAND * 2.3;
  if (!sceneAt(x, y) || !traits.fog) return;
  if (fol(x, y) > 0 || inTree(x, y)) return;
  if (fbm((x + time * 9 * U) * .003 / U, y * .012 / U + time * .008) + (1 - depth) * .22 < .5) return;
  PP.dab(x, y, (8 + R() * 16) * U, pal.fog, { a: .5, den: .22, s: 4 * U, sx: 2.6, sy: .7, core: .1, jit: 4 });
}

// ---------- the land ----------
function light(x, y, r) { PP.dab(x, y, r, pick(pal.lights, PP.R()), { a: 1, den: 3, core: 1, s: 1.2 * U, jit: 4 }); }

// a bush, a tree seen from far away
function bush(x, y, r) {
  const R = PP.R;
  if (inTree(x, y)) return;
  if (traits.bare) {
    PP.dab(x, y, r, mix(pal.fieldB, pal.folMid, .3), { a: .85, den: .6, core: .7, s: 1.6 * U });
    for (let i = 0; i < 2; i++) { const a = -Math.PI / 2 + (R() - .5) * 1.6; PP.line([[x, y + r * .5], [x + Math.cos(a) * r * 1.3, y + r * .5 + Math.sin(a) * r * 1.5]], pal.folDark, Math.max(.8, r * .16), { pass: 1, a: .75, wob: .4 * U }); }
    if (R() < .45) PP.dab(x, y - r * .4, r * .6, pal.folLight, { a: .85, den: .6, core: .4, s: 1.5 * U, sx: 1.4, sy: .6 });
    return;
  }
  PP.dab(x, y, r, R() < .6 ? pal.folDark : pal.folMid, { a: .9, den: .7, core: .75, s: 2 * U });
  const k = R();
  if (k < .55) PP.dab(x + r * .3, y - r * .35, r * .55, k < .16 && pal.accent ? pick(pal.accent, R()) : pal.folLight, { a: .85, den: .6, core: .4, s: 1.6 * U });
}

// ---------- the old tree ----------
// On the left a tree reaches its bare limbs out of the leaves.  It stands in front of
// everything else, so the other strokes keep clear of it: a coarse mask says where it is.
// [angle, length (in depths of the plain), width, where on the trunk it starts]: wiry limbs, the long ones reaching right
const FAN = [[-2.7, .3, 3, .78], [-2.1, .24, 3, .92], [-1.5, .2, 3, 1], [-1, .26, 3.2, 1], [-.55, .5, 3.8, .9], [-.25, .85, 4.5, .8], [.08, .8, 4, .68]];
let limbs = [], limbWeight = 0, limbLength = 0, treeMask = null, treeCols = 0, treeCell = 4;
const treeBox = [0, 0, 0, 0], reach = [0, 0];
const limbX = (s, t) => (1 - t) * (1 - t) * s.x1 + 2 * t * (1 - t) * s.cx + t * t * s.x2;
const limbY = (s, t) => (1 - t) * (1 - t) * s.y1 + 2 * t * (1 - t) * s.cy + t * t * s.y2;
function buildTree() {
  const r = rng(31); limbs = [];
  const add = (x1, y1, x2, y2, bend, w, depth, tip) => {
    const len = Math.hypot(x2 - x1, y2 - y1), a = Math.atan2(y2 - y1, x2 - x1);
    limbs.push({ x1, y1, x2, y2, cx: (x1 + x2) / 2 - Math.sin(a) * len * bend * 2, cy: (y1 + y2) / 2 + Math.cos(a) * len * bend * 2, w, depth, tip, k: r(), len });
  };
  const grow = (x, y, a, len, w, depth) => {
    const x2 = x + Math.cos(a) * len, y2 = y + Math.sin(a) * len, last = depth >= 5 || len <= 8 * U;
    add(x, y, x2, y2, (r() - .5) * .4, w, depth, last);
    if (last) return;
    const k = (r() < .72 ? 2 : 1) + (r() < .12 ? 1 : 0);
    for (let i = 0; i < k; i++) grow(x2, y2, a + (i - (k - 1) / 2) * .55 + (r() - .5) * .5, len * (.64 + r() * .2), Math.max(U, w * .7), depth + 1);
  };
  const bx = .172 * W, by = H + 12 * U, fx = .187 * W, fy = py(.66);
  add(bx, by, fx, fy, -.05, 8 * U, 0, false);                 // the trunk
  const trunk = limbs[0];
  for (const [a, len, w, at] of FAN) grow(limbX(trunk, at), limbY(trunk, at), a, BAND * len, w * U, 1);
  treeCell = 1.5 * U; treeCols = Math.ceil(W / treeCell) + 1;
  const rows = Math.ceil(H / treeCell) + 1;
  treeMask = new Uint8Array(treeCols * rows);
  let x0 = W, y0 = H, x1 = 0, y1 = 0;
  limbWeight = limbLength = 0;
  for (const s of limbs) {
    s.wt = s.len * (s.w + 2 * U); limbWeight += s.wt; limbLength += s.len;
    const n = Math.ceil(s.len / (treeCell * .5)), rad = s.w / 2 + .3 * U, m = Math.ceil(rad / treeCell);
    for (let i = 0; i <= n; i++) {
      const px = limbX(s, i / n), qy = limbY(s, i / n), ix = Math.floor(px / treeCell), iy = Math.floor(qy / treeCell);
      for (let dy = -m; dy <= m; dy++) for (let dx = -m; dx <= m; dx++) {
        const cx = ix + dx, cy = iy + dy;
        if (cx < 0 || cy < 0 || cx >= treeCols || cy >= rows || Math.hypot(dx, dy) * treeCell > rad + treeCell * .5) continue;
        treeMask[cy * treeCols + cx] = 1;
      }
      if (px - rad < x0) x0 = px - rad; if (px + rad > x1) x1 = px + rad; if (qy - rad < y0) y0 = qy - rad; if (qy + rad > y1) y1 = qy + rad;
    }
  }
  treeBox[0] = Math.max(0, x0); treeBox[1] = Math.max(0, y0); treeBox[2] = Math.min(W, x1); treeBox[3] = Math.min(H, y1);
}
function inTree(x, y) {
  if (x < treeBox[0] || x >= treeBox[2] || y < treeBox[1] || y >= treeBox[3]) return false;
  return treeMask[((y / treeCell) | 0) * treeCols + ((x / treeCell) | 0)] === 1;
}
// How far a stroke through (x, y) may run either way along (ca, sa) before it crosses a limb:
// fills reach[], false if the point itself is on the tree.
function treeReach(x, y, ca, sa, l) {
  reach[0] = reach[1] = l;
  if (x + l < treeBox[0] || x - l > treeBox[2] || y + l < treeBox[1] || y - l > treeBox[3]) return true;
  if (inTree(x, y)) return false;
  for (let d = treeCell; d < l; d += treeCell) if (inTree(x + ca * d, y + sa * d)) { reach[1] = d - treeCell; break; }
  for (let d = treeCell; d < l; d += treeCell) if (inTree(x - ca * d, y - sa * d)) { reach[0] = d - treeCell; break; }
  return true;
}
function treeStroke() {                                        // a stretch of bark
  const R = PP.R; let k = R() * limbWeight, s = limbs[0];
  for (const t of limbs) { s = t; k -= t.wt; if (k <= 0) break; }
  const span = Math.min(1, (10 + R() * 22) * U / s.len), t0 = R() * (1 - span), tm = t0 + span / 2, t1 = t0 + span;
  const scene = sceneAt(limbX(s, tm), Math.min(H - 1, limbY(s, tm))); if (!scene) return;
  const bark = mix(pal.trunk, pal.towerLight, s.depth < 4 ? .34 : .2), q = R();
  const o = { a: .93, jit: 8, dry: .06, sp: Math.min(1.3 * U, s.w), bow: .03 };
  for (const [a, b] of [[t0, tm], [tm, t1]]) {
    const ax = limbX(s, a), ay = limbY(s, a), bx = limbX(s, b), by = limbY(s, b);
    PP.brush(ax, ay, bx, by, s.w, bark, o);
    if (traits.bare) { if (s.w > 1.5 * U && q < .7) PP.brush(ax, ay - s.w * .45, bx, by - s.w * .45, s.w * .45, pal.folLight, { a: .9, jit: 4, dry: .05, sp: Math.min(1.2 * U, s.w * .45), bow: .03 }); }   // snow lies on it
    else if (s.w > 3 * U && q < .5) PP.brush(ax + s.w * .22, ay, bx + s.w * .22, by, s.w * .28, mix(pal.trunk, pal.towerLight, .55), { a: .6, jit: 6, sp: Math.min(1.2 * U, s.w * .28), bow: .03 });   // the sunny side
  }
  if (s.tip && t1 > .8 && !traits.bare) {                      // what the season hangs on the twigs
    const se = scene.season;
    if (se === 'autumn' ? s.k < .4 : s.k < .8)
      PP.dab(s.x2, s.y2, (2.2 + s.k * 2) * U, se === 'spring' ? pick(pal.accent, s.k) : se === 'autumn' ? pick(pal.bits, s.k * 2.5) : s.k < .4 ? pal.folLight : pal.folMid, { a: .9, den: 1.2, core: .7, s: 1.4 * U });
  }
}

// ---------- landmarks ----------
// The few buildings one can tell apart from up here.  They are part of the painting like
// everything else: patches of the town where the same small strokes take another colour.
// Positions and sizes are those of the photograph: x as a fraction of the width, heights
// as fractions of the plain's depth (0 the horizon, 1 its foot), widths in the same unit.
// Nothing rises above the horizon: even the Donauturm only just reaches it.
const BRIDGES = [[0, .237, .725, 1], [.104, .144, .573, 0], [.194, .273, .573, 0], [.235, .302, .514, 0], [.262, .325, .432, 2], [.246, .347, .335, 0]];
const SLABS = [
  [.075, .25, .34, .07, 'towerLight'], [.09, .24, .34, .05, 'towerLight'], [.103, .25, .34, .05, 'tower'], [.132, .25, .35, .08, 'towerLight'],   // Donau City
  [.152, .2, .35, .1, 'glass'], [.167, .19, .35, .09, 'towerLight'], [.185, .26, .35, .12, 'tower'], [.204, .22, .35, .1, 'glass'],
  [.218, .03, .32, .09, 'dark'],                                                                       // DC Tower
  [.33, .17, .44, .065, 'glass'],                                                                      // Millennium Tower
  [.437, .39, .55, .045, 'towerLight'], [.4625, .41, .53, .035, 'dark'], [.595, .42, .53, .1, 'tower'], [.55, .6, .66, .08, 'yellow'],
  [.683, .17, .3, .05, 'tower'], [.705, .19, .3, .06, 'glass'], [.815, .05, .13, .022, 'towerLight'], [.823, .06, .13, .022, 'towerLight'],
];
let town = [], townWeight = 0;
function buildTown() {
  const b = BAND, yb = f => HZ + b * f, lw = Math.max(.7, b / 130);
  town = [];
  const add = (kind, x0, y0, x1, y1, col, f, o) => town.push(Object.assign({ kind, x0, y0, x1, y1, col, f, lw }, o));
  const block = (xf, fTop, fBase, w, col, o) => add('block', xf * W - w * b / 2, yb(fTop), xf * W + w * b / 2, yb(fBase), col, fBase, o);
  const thin = (x, fTop, fBase, w, col, o) => add('block', x - w * lw / 2, yb(fTop), x + w * lw / 2, yb(fBase), col, fBase, o);
  const dot = (x, y, r, col, f, o) => add('dab', x - r, y - r, x + r, y + r, col, f, o);
  for (const [x0, x1, f, kind] of BRIDGES) {                   // bridges across the Danube
    const h = (kind === 1 ? 1.8 : 1.2) * lw;
    add('block', x0 * W, yb(f) - h / 2, x1 * W, yb(f) + h / 2, kind === 2 ? 'cabin' : 'towerLight', f, { piers: kind === 1, lit: .5 });
  }
  block(.011, .34, .58, .12, 'towerLight', { lit: .6 });       // Florido Tower, far left
  block(.017, .565, .61, .36, 'towerLight');
  for (const [xf, top, base, w, col] of SLABS) block(xf, top, base, w, col, { lit: .6 });
  let x = .116 * W;                                            // Donauturm
  thin(x, .19, .385, 2, 'tower'); dot(x, yb(.175), b * .022, 'tower', .3, { lit: .5 }); thin(x, .02, .16, .9, 'tower');
  dot(x, yb(.02), 1.3 * U, 'beacon', 1, { night: true });
  dot(.218 * W, yb(.03), 1.3 * U, 'beacon', 1, { night: true });
  x = .33 * W; thin(x, .1, .17, .8, 'tower'); dot(x, yb(.1), 1.3 * U, 'beacon', 1, { night: true });
  for (const xf of [.494, .505]) thin(xf * W, .04, .18, 1, 'towerLight');        // two chimneys beyond the Prater
  add('ring', .512 * W - b * .034, yb(.29) - b * .034, .512 * W + b * .034, yb(.29) + b * .034, 'towerLight', 1);   // Riesenrad
  x = .446 * W; thin(x, .38, .5, 1.3, 'towerLight'); dot(x, yb(.36), 1.7 * lw, 'gold', 1);   // the golden ball left of it
  x = .773 * W;                                                // Stephansdom: a slender dark spire, a pale roof beside it
  add('block', x - b * .085, yb(.5), x - b * .012, yb(.55), 'dark', .55);
  add('spire', x - b * .018, yb(.32), x + b * .018, yb(.55), 'stone', .7, { lit: .15 });
  add('block', x + b * .035, yb(.515), x + b * .165, yb(.555), 'towerLight', .55);
  for (const dx of [-1, 1]) thin(.822 * W + dx * b * .012, .63, .77, 1, 'sand');   // two churches in the suburbs in front
  x = .596 * W; add('block', x - b * .05, yb(.85), x + b * .045, yb(.9), 'roof', 1); thin(x - b * .04, .79, .86, 1, 'towerLight');
  const s = b / 120, hx = .52 * W, hy = py(.597);              // the farm among the vineyards
  add('block', hx - 9 * s, hy - 7 * s, hx + 9 * s, hy + s, 'wall', 1, { lit: .5 });
  add('block', hx - 10.5 * s, hy - 12 * s, hx + 10.5 * s, hy - 6.5 * s, 'farmRoof', 1);
  thin(hx + 4.3 * s, 0, 0, 2, 'stone'); { const c = town[town.length - 1]; c.y0 = hy - 15 * s; c.y1 = hy - 11 * s; c.f = 1; }
  townWeight = 0;                                              // bigger ones need more strokes to stay covered
  for (const t of town) { t.w = clamp((t.x1 - t.x0) * (t.y1 - t.y0) / (30 * U * U), 1, 7); townWeight += t.w; }
}
function townAt(x, y, padX, padY) {                            // the landmark here (or within reach), if any
  for (const s of town) if (x > s.x0 - padX && x < s.x1 + padX && y > s.y0 - padY && y < s.y1 + padY && !(s.night && !night)) return s;
  return null;
}
function townReach(x, y, l) {                                  // how far a level stroke may run before it paints over a landmark
  for (const s of town) if (y > s.y0 && y < s.y1 && !(s.night && !night)) {
    if (x < s.x0) { if (s.x0 - x < l) l = s.x0 - x; } else if (x - s.x1 < l) l = x - s.x1;
  }
  return l;
}
// one stroke of a landmark, at a point inside it
function landmark(s, x, y) {
  const R = PP.R, w = s.x1 - s.x0, h = s.y1 - s.y0, cx = s.x0 + w / 2, cy = s.y0 + h / 2, lw = s.lw;
  if (fol(x, y) > -.05) return;
  let col = s.col === 'farmRoof' ? (traits.bare ? pal.fieldLight : mix(pal.roof, pal.dark, .55)) : s.col === 'sand' ? pal.city[7] : pal[s.col];
  if (s.col !== 'beacon' && s.col !== 'gold') col = mix(col, pal.haze, .1 + clamp(1 - s.f) * .32);     // blue with distance
  const sz = (.5 + clamp(s.f) * 1.1) * U, o = { a: .9, jit: 9, dry: .05, bow: 0, sp: 1.1 * U };
  if (s.kind === 'dab') PP.dab(cx, cy, w / 2, col, { a: 1, den: 2.5, core: .9, s: U, jit: 4 });
  else if (s.kind === 'ring') {                                // a bit of the rim, now and then a spoke or a leg
    const a = R() * TAU, r = w / 2, px = cx + Math.cos(a) * r, qy = cy + Math.sin(a) * r, l = r * .5, k = R();
    if (night) { PP.dab(px, qy, .6 * U, pal.lights[1], { a: 1, den: 3, core: 1, s: U }); return; }
    if (k > .8) PP.dab(cx, cy, r * .6, mix(pal.folDark, pal.haze, .3), { a: .8, den: 1.5, core: .7, s: U });   // the trees of the Prater show through
    o.sp = lw;
    PP.brush(px - Math.sin(a) * l, qy + Math.cos(a) * l, px + Math.sin(a) * l, qy - Math.cos(a) * l, lw, col, o);
    o.sp = .45 * lw; o.a = .6;
    if (k < .25) PP.brush(cx, cy, px, qy, .45 * lw, col, o);
    else if (k < .4) PP.brush(cx, cy, cx + (k < .32 ? -1 : 1) * r * .5, cy + r * 1.25, .6 * lw, col, o);
  } else if (s.kind === 'spire' || w <= 3 * lw) {              // slender: short upright strokes
    const top = s.kind === 'spire' ? s.y0 + Math.abs(x - cx) / (w / 2) * h * .85 : s.y0, l = (3 + R() * 6) * sz;
    if (y < top) y = top + R() * (s.y1 - top);
    const bw = s.kind === 'spire' ? Math.max(lw, w * .3) : w;
    o.sp = bw;
    PP.brush(s.kind === 'spire' ? x : cx, Math.max(top, y - l), s.kind === 'spire' ? x : cx, Math.min(s.y1, y + l), bw, col, o);
  } else if (h <= 3 * lw) {                                    // a bridge: short level strokes, a pier now and then
    const l = (6 + R() * 12) * sz, xa = Math.max(s.x0, x - l);
    o.sp = h;
    PP.brush(xa, cy, Math.min(s.x1, x + l), cy, h, col, o);
    if (s.piers && R() < .5) { o.sp = lw; PP.brush(xa, cy, xa, cy + 3.4 * lw, lw, col, o); }
  } else {                                                     // a building: the same little dashes the town is made of
    const l = Math.min((2 + R() * 5) * sz, w / 2), t = Math.min((1.6 + R() * 2.4) * sz, h);
    const px = clamp(x, s.x0 + l, s.x1 - l), qy = clamp(y, s.y0 + t / 2, s.y1 - t / 2);
    PP.brush(px - l, qy, px + l, qy, t, mix(col, R() < .5 ? pal.towerLight : pal.dark, R() * .16), o);
  }
  if (night && s.lit && R() < s.lit * .3) light(s.x0 + R() * w, s.y0 + R() * h, (.5 + R() * .4) * U);
}
function townStroke() {
  const R = PP.R; let k = R() * townWeight, s = town[0];
  for (const t of town) { s = t; k -= t.w; if (k <= 0) break; }
  const x = s.x0 + R() * (s.x1 - s.x0), y = s.y0 + R() * (s.y1 - s.y0);
  if (sceneAt(x, y) && !(s.night && !night)) landmark(s, x, y);
}

function farStroke() {
  const R = PP.R, x = R() * W, fh = farH(x), y = HZ - R() * fh;
  if (!sceneAt(x, y)) return;
  const l = (14 + R() * 40) * U;
  PP.brush(x - l, y, x + l, y + (R() - .5) * U, (1.2 + R() * 1.6) * U, mix(pal.haze, pal.skyLow, R() * .3 + (HZ - y) / fh * .2), { a: .8, jit: 5, dry: .15, sp: 1.2 * U, bow: .02 });
}

function cityStroke() {
  const R = PP.R, x = R() * W, ry = ridgeY(x), y = HZ + R() * (ry - HZ);
  if (!sceneAt(x, y)) return;
  if (fol(x, y) > -.1 || inTree(x, y)) return;
  const f = clamp((y - HZ) / (ry - HZ)), sz = (.5 + f * 1.1) * U;
  const mark = townAt(x, y, 0, 0);
  if (mark) { landmark(mark, x, y); return; }
  if (riverAt(x, f)) {
    const sh = vnoise(x * .03 / U + time * .5, y * .4 / U), l = townReach(x, y, (8 + R() * 26) * sz);
    PP.brush(x - l, y, x + l, y + (R() - .5) * 1.5 * U, (1.5 + R() * 2.5) * sz, mix(pal.water, sh > .5 ? pal.waterLight : pal.waterDark, Math.abs(sh - .5) * 1.5),
      { a: .85, jit: 6, dry: .15, sp: 1.1 * U, bow: 0 });
    if (night && R() < .1) PP.brush(x, y - 2 * sz, x, y + 3 * sz, 1.4 * U, pick(pal.lights, R()), { a: .5, sp: U, bow: 0 });
    return;
  }
  // beyond the river lie woods and gardens with pale houses in them, all of it blue with distance
  const xf = x / W, haze = clamp(1 - f) * .5, beyond = channel(RIVER, xf) && f < chC;
  if (f < .16) {                                               // the far end of the plain dissolves in the haze
    const l = townReach(x, y, (6 + R() * 18) * U), woods = beyond || R() < smooth(.62, .5, xf), k = R();   // dark woods behind the river and the Prater
    PP.brush(x - l, y, x + l, y, (1.4 + R() * 2) * U, woods ? mix(mix(pal.cityGreen, pal.haze, .7), pal.cityFar, R() * .35) : mix(pal.cityFar, k < .4 ? pal.city[0] : k < .7 ? pal.haze : pal.cityGreen, R() * (k < .7 ? .6 : .3)), { a: .8, jit: 8, dry: .1, sp: 1.1 * U, bow: 0 });
    if (R() < (woods ? .15 : .6) && !townAt(x, y, 2 * U, 2 * U)) PP.dab(x, y, (1 + R()) * U, R() < .7 ? pal.city[0] : pal.cityFar, { a: .6, den: 1.2 });
    if (night && R() < (woods ? .15 : .5)) light(x + (R() - .5) * l, y, (.5 + R() * .5) * U);
    return;
  }
  let g = fbm(x * .008 / U + 3, y * .03 / U + 7) + smooth(.62, 1, f) * .28 + (beyond ? .2 : 0) + (R() - .5) * .22;   // garden suburbs in front
  if (xf > .43 && xf < .55 && f > .17 && f < .31) g += .5;     // the Prater
  const green = g > .56;
  if (green) {                                                 // parks, gardens, the woods along the river
    const r = (1.6 + R() * 2.4) * sz;
    if (townAt(x, y, r * .7, r * .7)) return;
    PP.haze = [pal.haze, haze * (beyond ? 1.3 : 1)];
    bush(x, y, r);
    PP.haze = null;
  } else {
    let c = pick(pal.city, R());
    if ((c === pal.city[2] || c === pal.city[3]) && (beyond || R() > f * .75)) c = pal.city[R() < .6 ? 0 : 5];   // red roofs only show close by
    const l = townReach(x, y, (2 + R() * 5) * sz);
    PP.brush(x - l, y, x + l, y, (1.6 + R() * 2.4) * sz, mix(c, pal.cityFar, haze * .9), { a: .9, jit: 9, dry: .05, sp: 1.1 * U, bow: 0 });
    if (!beyond && R() < f * f * .3) PP.brush(x - l, y - 1.6 * sz, x + l * .6, y - 1.6 * sz, 1.3 * sz, pal.city[2 + (R() * 2 | 0)], { a: .8, sp: U, bow: 0 });   // a roof
  }
  if (night && R() < (green ? .12 : beyond ? .3 : .55)) light(x + (R() - .5) * 6 * sz, y + (R() - .5) * 3 * sz, (.55 + R() * .7) * U);
}

function hillStroke() {
  const R = PP.R, x = R() * W, y0 = HZ + BAND * .8, y = y0 + R() * (H - y0);
  if (!sceneAt(x, y)) return;
  const ry = ridgeY(x);
  if (y < ry || fol(x, y) > -.05 || townAt(x, y, 10 * U, 4 * U) || inTree(x, y)) return;
  hillAt(x, y);
  const li = hs.li, id = hs.id, sz = (.55 + li * .3) * U;
  if (hs.hedge) { bush(x, y, (2.5 + R() * 3.5) * sz); return; }
  if (hs.path) {                                               // a track along the slope
    const a = Math.atan(hs.slope), l = (5 + R() * 9) * sz;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (1.6 + li * .7) * U, mix(pal.earth, pal.fieldLight, .25 + R() * .3), { a: .8, jit: 6, dry: .1, sp: 1.1 * U, bow: .03 });
    return;
  }
  const col = fieldColor();
  if (hs.type === 0) {                                         // vines: every stroke sits on a row
    const sp = (3.6 + li * 2) * U, ca = Math.cos(hs.ang), sa = Math.sin(hs.ang);
    const n = -x * sa + y * ca, k = Math.round(n / sp), off = k * sp - n;
    const px = x - sa * off, py = y + ca * off; let l = (12 + R() * 28) * sz;
    hillAt(px + ca * l, py + sa * l); if (hs.id !== id) l *= .4;
    else { hillAt(px - ca * l, py - sa * l); if (hs.id !== id) l *= .4; }
    if (py - Math.abs(sa) * l < ry + 2 * U) l *= .4;
    if (!treeReach(px, py, ca, sa, l)) return;
    PP.brush(px - ca * reach[0], py - sa * reach[0], px + ca * reach[1], py + sa * reach[1], (k & 1) ? sp * .42 : sp * .8, (k & 1) ? mix(col, pal.row, .78) : mix(col, pal.fieldLight, .18), { a: .84, jit: 8, dry: .08, sp: 1.2 * U, bow: .02, dash: 12 * U });
  } else {
    const a = hs.ang + (R() - .5) * .25; let l = (14 + R() * 34) * sz;
    const ca = Math.cos(a), sa = Math.sin(a);
    hillAt(x + ca * l, y + sa * l); if (hs.id !== id) l *= .4;
    if (y - Math.abs(sa) * l < ry + 2 * U) l *= .4;
    if (!treeReach(x, y, ca, sa, l)) return;
    PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (5 + R() * 8) * sz, mix(col, R() < .5 ? pal.fieldLight : pal.fieldShadow, R() * .22), { a: .62, jit: 9, dry: .3, dash: 16 * U });
  }
}

function foliageDab() {
  const R = PP.R, x = R() * W, y = treeLine + R() * (H - treeLine);
  if (!sceneAt(x, y)) return;
  const d = fol(x, y); if (d < -.08 || inTree(x, y)) return;
  const lum = folLum(x, y), sz = (.8 + (y / H - .5) * 1.5) * U, k = R();
  if (d < .12) {                                               // the ragged rim: single leaves
    PP.dab(x, y, (3 + R() * 6) * sz, traits.bare ? pal.folDark : lum > .5 ? pal.folMid : pal.folDark, { a: .85, den: .3, s: 3 * U });
    return;
  }
  if (traits.bare) {                                           // twigs, and the snow lying on them
    if (lum > .42 && k < .2 + lum * .6) { PP.dab(x, y, (4 + R() * 8) * sz, lum > .6 ? pal.folSun : pal.folLight, { a: .9, den: .6, core: .55, sx: 1.7, sy: .55 }); return; }
    const a = -Math.PI / 2 + (R() - .5) * 2, l = (8 + R() * 20) * sz;
    if (inTree(x + Math.cos(a) * l * .5, y + Math.sin(a) * l * .5) || inTree(x + Math.cos(a) * l, y + Math.sin(a) * l)) return;
    if (k > .6) PP.dab(x, y, (5 + R() * 6) * sz, mix(pal.folMid, pal.folLight, .25 + lum * .5), { a: .8, den: .5, core: .5, s: 2 * U });
    PP.line([[x, y], [x + Math.cos(a) * l * .5 + (R() - .5) * 5 * U, y + Math.sin(a) * l * .5], [x + Math.cos(a) * l, y + Math.sin(a) * l]],
      lum < .35 ? pal.folDeep : k < .8 ? pal.folDark : pal.folMid, (1 + R() * 1.5) * sz, { pass: 1, a: .8 });
    return;
  }
  let col = lum < .3 ? mix(pal.folDeep, pal.folDark, lum / .3) : lum < .6 ? mix(pal.folDark, pal.folMid, (lum - .3) / .3) : mix(pal.folMid, pal.folLight, (lum - .6) / .4);
  if (lum > .6 && k < .3) col = pal.folSun; else if (k > .92 && pal.accent && lum > .3) col = pick(pal.accent, R());
  const a = R() * Math.PI, l = (4 + R() * 9) * sz, ca = Math.cos(a), sa = Math.sin(a);
  if (!treeReach(x, y, ca, sa, l)) return;
  PP.brush(x - ca * reach[0], y - sa * reach[0], x + ca * reach[1], y + sa * reach[1], (4 + R() * 7) * sz, col, { a: .85, sp: 1.5 * U, dry: .2, jit: 10 });
}

// ---------- the coat: broad strokes swept across the whole picture ----------
const DIRS = [[-1, .3], [1, .3], [-1, -.3], [.3, 1], [1, -.3]];
function startCoat(dur) {
  const r = rng(17 + coatN), [ax, ay] = DIRS[coatN++ % DIRS.length];
  const cw = 62 * U, ch = 38 * U, cells = [];
  let lo = 1e9, hi = -1e9;
  for (let y = -ch / 2; y < H + ch; y += ch) for (let x = -cw; x < W + cw; x += cw) {
    const c = { x: x + r() * cw * .65, y: y + r() * ch * .8 }; c.k = c.x * ax + c.y * ay;
    if (c.k < lo) lo = c.k; if (c.k > hi) hi = c.k;
    cells.push(c);
  }
  const J = (hi - lo) * .2;
  for (const c of cells) c.k += r() * J;
  cells.sort((a, b) => a.k - b.k);
  coat = { cells, i: 0, t0: time, dur, ax, ay, lo, hi: hi + J, J, front: lo };
  boostUntil = time + dur + (REDUCED ? 6 : 2.5);
}
function coatStroke(x, y) {
  const R = PP.R;
  if (fol(x, y) > 0) {
    const lum = folLum(x, y), a = R() * Math.PI, l = 26 * U;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (40 + R() * 20) * U,
      traits.bare ? mix(pal.folMid, pal.folLight, clamp(lum * 1.1) * .6) : mix(pal.folDeep, lum > .5 ? pal.folMid : pal.folDark, clamp(lum * 1.3)), { a: .93, jit: 9, dry: 0, full: 1, sp: 2.4 * U });
  } else if (y < HZ) {
    const ang = -.36 + (R() - .5) * .2, l = (150 + R() * 90) / 2 * U;
    PP.brush(x - Math.cos(ang) * l, y - Math.sin(ang) * l, x + Math.cos(ang) * l, y + Math.sin(ang) * l, (44 + R() * 26) * U, skyCol(y), { a: .92, jit: 9, dry: 0, full: 1, sp: 2.4 * U, bow: .05 });
  } else if (y < ridgeY(x)) {
    const f = (y - HZ) / (ridgeY(x) - HZ), l = (40 + R() * 40) * U;
    const col = riverAt(x, f) ? pal.water : mix(mix(pal.city[1], pal.cityGreen, .4), pal.cityFar, clamp(1 - f) * .6);
    PP.brush(x - l, y, x + l, y, (36 + R() * 12) * U, col, { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .03 });
  } else {
    hillAt(x, y);
    const a = Math.atan(hs.slope), l = (40 + R() * 40) * U;
    PP.brush(x - Math.cos(a) * l, y - Math.sin(a) * l, x + Math.cos(a) * l, y + Math.sin(a) * l, (42 + R() * 14) * U, hs.hedge ? pal.folDark : fieldColor(), { a: .92, jit: 8, dry: 0, full: 1, sp: 2.2 * U, bow: .04 });
  }
}
function clipBand(y0, y1) { pctx.save(); pctx.beginPath(); pctx.rect(0, y0, W, Math.max(0, y1 - y0)); pctx.clip(); }
function runCoat() {
  if (!coat) return;
  const p = coat.dur > 0 ? (time - coat.t0) / coat.dur : 1;
  coat.front = coat.lo + (coat.hi - coat.lo) * p;
  let j = coat.i; const cells = coat.cells;
  while (j < cells.length && cells[j].k <= coat.front) j++;
  use(cur);
  clipBand(0, HZ);
  for (let i = coat.i; i < j; i++) if (cells[i].y < HZ + 30 * U) coatStroke(cells[i].x, Math.min(cells[i].y, HZ - 1));
  pctx.restore();
  clipBand(HZ, H);
  for (let i = coat.i; i < j; i++) if (cells[i].y >= HZ - 30 * U) coatStroke(cells[i].x, Math.max(cells[i].y, HZ));
  pctx.restore();
  coat.i = j;
  if (p >= 1 && j >= cells.length) { coat = null; old = null; }
}

// ---------- the painting, one frame's worth ----------
const RATE = { sky: 900, cloud: 1100, star: 260, moon: 70, fog: 1500, far: 260, city: 9000, town: 1.3, hill: 3600, fol: 3200, tree: 1.2 };
const budget = { sky: 0, cloud: 0, star: 0, moon: 0, fog: 0, far: 0, city: 0, town: 0, hill: 0, fol: 0, tree: 0 };
let quality = 1;
function spend(key, amount, fn) {
  budget[key] = Math.min(500, budget[key] + amount);
  while (budget[key] >= 1) { budget[key]--; fn(); }
}
function paint(dt) {
  runCoat();
  const ua = U * U * 1e6, anyNight = cur.night || (old && old.night);
  let pace = dt * quality * (REDUCED && time > boostUntil ? .3 : 1);
  if (time < boostUntil) pace *= 3;
  const skyA = W * HZ / ua;
  clipBand(0, HZ - 3 * U);
  spend('sky', RATE.sky * skyA * (cur.night ? .45 : 1) * pace, skyStreak);
  spend('cloud', RATE.cloud * skyA * cur.traits.cloudy * pace, cloudDab);
  if (anyNight) spend('star', RATE.star * skyA * pace, starDab);
  if (cur.orb || (old && old.orb)) spend('moon', RATE.moon * pace, orbDab);
  pctx.restore();
  clipBand(HZ - 8 * U, H);
  spend('far', RATE.far * W / (1000 * U) * pace, farStroke);
  spend('city', RATE.city * W * BAND / ua * pace, cityStroke);
  spend('town', RATE.town * townWeight * pace, townStroke);
  spend('hill', RATE.hill * W * HILL * .6 / ua * pace, hillStroke);
  spend('fol', RATE.fol * W * (H - treeLine) * .7 / ua * pace, foliageDab);
  spend('tree', RATE.tree * limbLength / (14 * U) * pace, treeStroke);
  if (cur.traits.fog || (old && old.traits.fog)) spend('fog', RATE.fog * W * BAND * 2.3 / ua * pace, fogStroke);
  pctx.restore();
}

// ---------- things that float over the picture ----------
// Birds, rain, snow, whatever is in the air: they move too fast to be brushed into the
// picture, so they have a canvas of their own that is wiped every frame.  They are painted
// with the same brushes and chalk as everything below them.
function drawSmoke(P, t) {                                     // the farm has its stove going when it is cold
  if (cur.season !== 'winter' && cur.season !== 'autumn') return;
  const s = BAND / 120, x0 = .52 * W + 4.3 * s, y0 = py(.597) - 15 * s, g = wind(t);
  if (!sceneAt(x0, y0) || fol(x0, y0) > -.1) return;
  for (let j = 1; j <= 8; j++) {
    const x = x0 + (Math.sin(t * 1.6 + j * .9) * (1 + j) - j * j * .5 - g * j * 1.6) * s, y = y0 - j * 5.5 * s;
    P.dab(x, y, (2 + j * 1.2) * s, pal.cloud, { a: .55 * (1 - j / 9), den: .6, core: .22 * (1 - j / 9), s: 2.2 * s, jit: 4 });
  }
}

function drawSkyLife(P, t) {
  use(cur);
  if (!night) {                                                // a flock of birds, now and then, if the weather is fit for flying
    const u = (t % 38) / 30; if (u > 1 || traits.rain || traits.snow || traits.fog) return;
    const bx = W + 80 * U - u * (W + 340 * U), by = HZ * (.42 + Math.sin(u * 5) * .1);
    for (let i = 0; i < 8; i++) {
      const x = bx + (i * 37 % 110) * 1.6 * U, y = by + ((i * 53 % 60) - 30) * U, fl = Math.sin(t * 9 + i * 1.7) * 3.5 * U, s = (6 + (i % 3) * 1.5) * U;
      if (!sceneAt(x, y)) continue;
      const col = mix(pal.ink, pal.haze, .25), o = { a: .9, sp: 1.1 * U, dry: 0, jit: 6, bow: .25 };
      P.brush(x - s, y - 2.5 * U + fl, x, y, 2 * U, col, o);
      P.brush(x, y, x + s, y - 2.5 * U + fl, 2 * U, col, o);
      P.dab(x, y + .3 * U, 1.3 * U, col, { a: .95, den: 2, core: .9, s: U, jit: 4 });
    }
    return;
  }
  const cyc = 13, n = Math.floor(t / cyc), u = (t - n * cyc) / .9;   // a shooting star, if the sky is open
  if (u > 1 || traits.veiled) return;
  const A = rng(n * 7919 + 13), x0 = (.15 + A() * .8) * W, y0 = (.05 + A() * .4) * HZ, len = (140 + A() * 160) * U, a = 2.5 + A() * .5;
  const ca = Math.cos(a), sa = Math.sin(a), hx = x0 + ca * len * u, hy = y0 + sa * len * u, tail = Math.min(u, .35) * len, fade = Math.sin(u * Math.PI);
  for (let i = 0; i < 6; i++) {
    const f0 = i / 6, f1 = (i + 1) / 6;
    P.brush(hx - ca * tail * f0, hy - sa * tail * f0, hx - ca * tail * f1, hy - sa * tail * f1, (2.6 - i * .36) * U, pal.star, { a: (1 - f0) * fade * .9, sp: 1.1 * U, dry: .1, jit: 3, bow: 0 });
    P.dab(hx - ca * tail * f1 + (A() - .5) * 6 * U, hy - sa * tail * f1 + (A() - .5) * 6 * U, 1.2 * U, pal.star, { a: (1 - f1) * fade * .7, den: 1.5, s: U });
  }
  P.dab(hx, hy, 2.4 * U, pal.star, { a: fade, den: 2.5, core: fade, s: 1.2 * U });
}

function drawStorm(P, t) {                                     // lightning: the sky flickers, a bolt stands for a moment
  if (!cur.traits.lightning) return;
  use(cur);
  const cyc = 8.5, n = Math.floor(t / cyc), u = t - n * cyc; if (u > .5) return;
  const A = rng(n * 131 + 17), flick = u < .07 ? 1 : u < .13 ? .25 : u < .26 ? .9 : Math.max(0, 1 - (u - .26) / .24), c = P.c;
  if (!REDUCED) { c.fillStyle = css([255, 251, 232], +(flick * .2).toFixed(3)); c.fillRect(0, 0, W, H); }   // the whole sky lights up
  let x = (.12 + A() * .76) * W, y = HZ * (.15 + A() * .25);
  const steps = 7 + (A() * 3 | 0), dy = (HZ - 6 * U - y) / steps, bolt = [[x, y]];
  for (let i = 0; i < steps; i++) {
    const nx = x + (A() - .5) * 46 * U, fork = A() < .3, fx = (A() - .5) * 90 * U, fy = dy * (1 + A());
    if (fork) P.line([[x, y], [x + fx * .5 + (A() - .5) * 14 * U, y + fy * .5], [x + fx, y + fy]], pal.star, 1.3 * U, { a: flick * .7, pass: 1, wob: .6 * U, sharp: 1, step: 999 });
    x = nx; y += dy; bolt.push([x, y]);
  }
  P.line(bolt, pal.star, 5 * U, { a: flick * .25, pass: 1, wob: U, sharp: 1, step: 999 });      // its glare
  P.line(bolt, pal.star, 2.2 * U, { a: flick, pass: 2, wob: .7 * U, sharp: 1, step: 999 });
}

// whatever is in the air: rain, snow, petals, seeds and fireflies, leaves
const PARTS = (() => { const A = rng(5), l = []; for (let i = 0; i < 320; i++) l.push({ x: A(), y: A(), z: .35 + A() * .65, ph: A() * TAU, k: A(), sp: 2 + A() * 4 }); return l; })();
let partScene = null;
function drawAir(P, t) {
  let alpha = 1;
  if (coat && old) { const p = clamp((time - coat.t0) / Math.max(coat.dur, .01)); alpha = Math.abs(1 - 2 * p); if (p > .5) partScene = cur; } else partScene = cur;
  if (!partScene) return;
  use(partScene);
  const kind = traits.part, c = P.c, g = wind(t);
  if (!kind) return;
  c.globalAlpha = alpha * clamp((time - 3.5) / 1.5);
  if (kind === 'rain') {                                       // thin dry strokes, slanting with the wind
    const n = Math.min(PARTS.length, Math.round(180 * traits.rain)), slant = -.16 - traits.gust * .22 - g * .04;
    for (let i = 0; i < n; i++) {
      const p = PARTS[i], len = (26 + 44 * p.z) * U, fall = p.y + t * (1.5 + p.z * .9 + traits.gust * .5);
      const y = (fall % 1) * (H + len * 2) - len, x = ((p.x + p.k * .37 * Math.floor(fall)) % 1) * (W + H * .5) + slant * y, w = (1 + p.z) * U;
      P.brush(x, y, x + slant * len, y + len, w, pal.rain, { a: .5 + .35 * p.z, sp: w, jit: 5, bow: 0, dash: 14 * U });
    }
  } else if (kind === 'snow') {                                // chalk dabs, the near ones bigger and faster
    const n = Math.min(PARTS.length, Math.round(170 * traits.snow));
    for (let i = 0; i < n; i++) {
      const p = PARTS[i], y = ((p.y + t * (.03 + .04 * p.z)) % 1) * (H + 20) - 10;
      const x = (((p.x + t * .004 * p.z) % 1) * (W + 40)) - 20 + Math.sin(t * .8 + p.ph) * 14 * U * p.z - g * 8 * U;
      P.dab(x, y, (1.2 + p.z * 2.4) * U, pal.flake, { a: .95, den: 1.6, core: .9, s: (.8 + p.z) * U, jit: 3 });
    }
  } else if (kind === 'seed' && night) {                       // fireflies
    for (let i = 0; i < 26; i++) {
      const p = PARTS[i], a = Math.pow(Math.max(0, Math.sin(t * (.7 + p.k) + p.ph)), 2);
      if (a < .05) continue;
      const x = p.x * W + Math.sin(t * .23 + p.ph) * 40 * U, y = (.62 + p.y * .36) * H + Math.cos(t * .31 + p.ph * 2) * 20 * U;
      P.R = rng(9000 + i);                                     // each keeps its own shape while it glows
      P.dab(x, y, 6 * U, pal.firefly, { a: a * .3, den: .5, s: 2.4 * U, jit: 3 });
      P.dab(x, y, 1.7 * U, pal.firefly, { a, den: 2.5, core: a, s: U, jit: 3 });
    }
  } else {
    const n = kind === 'leaf' ? 22 : kind === 'petal' ? 34 : 16;
    for (let i = 0; i < n; i++) {
      const p = PARTS[i], near = i % 3 === 0, z = near ? 1 : .55, span = W + 60 * U;
      const vx = (kind === 'seed' ? 14 : 40 + p.k * 70) * U, x = W + 30 * U - ((p.x * span + vx * z * (t + .6 * Math.sin(t * .7 + p.ph))) % span);
      const yb = near ? (.6 + p.y * .4) * H : (.08 + p.y * .8) * H;
      const y = kind === 'seed' ? yb - ((t * 6 * U * (1 + p.k)) % (H * .3)) + Math.sin(t * .8 + p.ph) * 12 * U : yb + Math.sin(t * .9 + p.ph) * 40 * U * z + Math.sin(t * 2.3 + p.ph) * 10 * U;
      if (kind === 'seed') { P.dab(x, y, (1.2 + p.k) * U, pal.bits[0], { a: .85, den: 2, core: .8, s: U }); continue; }
      const a = t * p.sp + p.ph, s = ((kind === 'leaf' ? 6 : 3.4) * z + 1.5) * U, ca = Math.cos(a) * s, sa = Math.sin(a) * s * .6;
      P.brush(x - ca, y - sa, x + ca, y + sa, s * .7 * Math.abs(Math.cos(a * .7)) + 1.6 * U, pick(pal.bits, p.k), { a: .92, sp: 1.2 * U, dry: .08, jit: 8, bow: .3 });
    }
  }
  c.globalAlpha = 1;
}

// Each is painted with a steady hand: the same strokes every frame, only somewhere else.
const SPRITES = [
  { draw: drawStorm, t0: 4, seed: 102 },
  { draw: drawSmoke, t0: 4, seed: 103 },
  { draw: drawSkyLife, t0: 5, seed: 104 },
  { draw: drawAir, t0: 3.5, seed: 106 },
];
function drawSprites() {
  sctx.setTransform(1, 0, 0, 1, 0, 0);
  sctx.clearRect(0, 0, spriteCv.width, spriteCv.height);
  sctx.setTransform(SCALE, 0, 0, SCALE, 0, 0); sctx.lineCap = sctx.lineJoin = 'round';
  for (const sp of SPRITES) if (time > sp.t0) { VP.reseed(sp.seed * 977); sp.draw(VP, time); }
}

// ---------- canvas & layout ----------
const scratch = document.createElement('canvas');
function resize() {
  const w = paintCv.clientWidth, h = paintCv.clientHeight;
  if (!w || !h) return;
  const scale = Math.min(window.devicePixelRatio || 1, 2, Math.sqrt(3.4e6 / (w * h)));
  const pw = Math.max(1, Math.round(w * scale)), ph = Math.max(1, Math.round(h * scale)), had = W > 0;
  if (pw !== paintCv.width || ph !== paintCv.height) {
    // Resizing a canvas wipes it: stretch the old painting onto the new one and let fresh strokes heal it.
    if (had) { scratch.width = paintCv.width; scratch.height = paintCv.height; scratch.getContext('2d').drawImage(paintCv, 0, 0); }
    paintCv.width = spriteCv.width = pw; paintCv.height = spriteCv.height = ph;
    if (had) { pctx.setTransform(1, 0, 0, 1, 0, 0); pctx.drawImage(scratch, 0, 0, pw, ph); scratch.width = scratch.height = 1; }
  }
  W = w; H = h; SCALE = scale;
  U = clamp(Math.min(w, h) / 800, .55, 1.5);
  const wide = clamp((W / H - 1) / .6); Y0 = .1 * wide; Y1 = 1 - .15 * wide;
  HZ = py(.419); BAND = py(.522) - HZ; HILL = H - HZ - BAND; LX = Math.max(W, H * 1.2);
  pctx.setTransform(scale, 0, 0, scale, 0, 0); pctx.lineCap = pctx.lineJoin = 'round';
  buildCrowns(); buildTown(); buildTree();
  if (!had) {
    cur = mkScene(SEASONS[seasonIndex], weather, wantNight());
    pctx.fillStyle = css(cur.pal.paper); pctx.fillRect(0, 0, W, H);
    startCoat(REDUCED ? 0 : 1.9);
  } else {
    for (const s of [cur, old]) if (s) layoutScene(s);
    if (coat) { old = null; startCoat(0); }
    boostUntil = Math.max(boostUntil, time + 1.5);
  }
}

// ---------- seasons, day and night ----------
// Every minute the picture is painted over with another one: first the weather turns, the
// next time the season does.  Over sixteen steps every season has seen all of its weathers.
let seasonClock = 0, steps = 0;
function show(dur) {
  const season = SEASONS[seasonIndex], isNight = wantNight();
  seasonClock = 0;
  announce();
  if (cur.season === season && cur.weather === weather && cur.night === isNight) return;
  old = cur; cur = mkScene(season, weather, isNight);
  startCoat(REDUCED ? 0 : dur);
}
function nextWeather() { weatherIndex = (weatherIndex + 1) % 4; weather = WEATHERS[SEASONS[seasonIndex]][weatherIndex]; show(3.5); }
function nextSeason() { seasonIndex = (seasonIndex + 1) % SEASONS.length; weather = WEATHERS[SEASONS[seasonIndex]][weatherIndex]; show(4.5); }
function nextScene() {
  if (++steps % 2) { nextWeather(); return; }
  weatherIndex = (weatherIndex + (steps % 8 ? 1 : 3)) % 4;
  nextSeason();
}
retheme = () => show(1.8);
paintCv.addEventListener('click', nextScene);
if (seasonButton) seasonButton.addEventListener('click', nextSeason);
if (weatherButton) weatherButton.addEventListener('click', nextWeather);

// ---------- the loop ----------
function step(dt, draw) {
  if (!cur) return;
  time += dt;
  if (!HOLD) { seasonClock += dt; if (seasonClock >= SEASON_SECONDS) nextScene(); }
  moveClouds(dt);
  paint(dt);
  if (draw) drawSprites();
}
let last = performance.now(), cost = 8;
const GAP = REDUCED ? 120 : 31;
function frame(now) {
  requestAnimationFrame(frame);
  if (now - last < GAP) return;
  const dt = Math.min(.1, (now - last) / 1000) * SPEED; last = now;
  const t0 = performance.now();
  step(dt, true);
  cost += (performance.now() - t0 - cost) * .05;               // slow machines get fewer fresh strokes
  if (cost > 16) quality = Math.max(.3, quality * .97); else if (cost < 9) quality = Math.min(1, quality * 1.01);
}
window.addEventListener('resize', resize);
resize();
if (!W) { requestAnimationFrame(function wait() { resize(); if (!W) requestAnimationFrame(wait); }); }
window.vienna.ff = sec => { for (let i = 0, n = sec * 30; i < n; i++) step(1 / 30, false); drawSprites(); return time; };
window.vienna.next = nextScene; window.vienna.nextSeason = nextSeason; window.vienna.nextWeather = nextWeather;
if (qs.get('t')) window.vienna.ff(parseFloat(qs.get('t')));
paintCv.classList.add('ready');
requestAnimationFrame(frame);
})();
