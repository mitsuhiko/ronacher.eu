# /// script
# dependencies = ["pillow", "numpy", "scipy", "scikit-image"]
# ///
# Reads the photograph the Thurngasse is painted after and writes places/thurngasse-map.js: what there is at every
# point (a wall, a window, a step ...), how light it is there without the sun, and where the sun falls.  The rails
# of the stairs are too thin for such a map: they are taken out of it and written down as lines.  The two people
# coming down the stairs are taken out too, and so are the trees, the lamp and its wires: those are drawn by hand.
#   uv run tools/thurngasse-map.py photo.jpg places/thurngasse-map.js [map width] [prefix of pictures to check it by]
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageOps
from scipy import ndimage as ndi
from skimage.morphology import thin as thinned

SRC, OUT = sys.argv[1], sys.argv[2]
MW = int(sys.argv[3]) if len(sys.argv) > 3 else 448          # how many cells wide and high the map is
SW = 128                                                     # and the map of the sun
N = 1772                                                     # the size all of this is worked out at: the photo is square
im = ImageOps.exif_transpose(Image.open(SRC)).convert('RGB').resize((N, N), Image.LANCZOS)
rgb = np.asarray(im).astype(np.float32) / 255
yy, xx = np.mgrid[0:N, 0:N]; XF = (xx + .5) / N; YF = (yy + .5) / N
P = lambda f: max(1, int(round(f * N)))                      # a length of the photo in pixels
blur = lambda a, s: ndi.gaussian_filter(a.astype(np.float32), s)
lum = lambda c: .3 * c[..., 0] + .55 * c[..., 1] + .15 * c[..., 2]
smooth = lambda a, b, x: (lambda t: t * t * (3 - 2 * t))(np.clip((x - a) / (b - a), 0, 1))
box = lambda f: np.ones((P(f), P(f)), bool)

def poly(pts):
    m = Image.new('L', (N, N), 0); ImageDraw.Draw(m).polygon([(x * N, y * N) for x, y in pts], fill=1); return np.asarray(m).astype(bool)
def along(pts, x): return np.interp(x, [p[0] for p in pts], [p[1] for p in pts])

# ---- the lie of the land ----
# Where the sky ends: down the left house, over the roofs at the end of the street, up the eaves on the right.
SKYLINE = [(.2545, 0), (.273, .095), (.272, .238), (.332, .262), (.34, .30), (.345, .353), (.469, .355), (.466, .30), (.477, .25), (.481, .22),
           (.489, .19), (.492, .181), (.503, .173), (.52, .16), (.541, .12), (.551, .08), (.56, .04), (.564, 0)]
sky = poly([(SKYLINE[0][0], -.01)] + SKYLINE + [(SKYLINE[-1][0], -.01)])
LEFT_EDGE = [(0, .2545), (.095, .273), (.238, .272), (.49, .271)]          # the corner of the house on the left: y, x
# where the houses stand on the stairs, where the stairs stand on the pavement, the kerb
STAIR_TOP = [(0, .68), (.118, .68), (.122, .6), (.15, .56), (.18, .52), (.205, .505), (.255, .49), (.272, .485), (.33, .482), (.5, .482), (.56, .49), (.6, .5),
             (.62, .52), (.66, .55), (.7, .575), (.74, .6), (.78, .63), (.82, .66), (.85, .7), (.9, .7), (.96, .712), (1, .712)]
STAIR_FOOT = [(0, .80), (.15, .80), (.17, .808), (1, .806)]
KERB = .853
below = YF > along(STAIR_TOP, XF)
foot = along(STAIR_FOOT, XF)
steps = below & (YF < foot); pave = (YF >= foot) & (YF < KERB); road = YF >= KERB
house = ~sky & ~below
lefthouse = house & (XF < along(LEFT_EDGE, YF))
fence = poly([(0, .6), (.122, .6), (.122, .682), (0, .682)])           # the slats beside the stairs on the left
farhouse = poly([(.345, .35), (.47, .35), (.47, .49), (.345, .49)])

# ---- what is not to be painted ----
# The two people: behind them the steps go on, row by row as they are just right of them.
PEOPLE = [(.15, .533), (.186, .533), (.197, .56), (.2075, .6), (.207, .682), (.134, .682), (.13, .6), (.14, .575)]
people = ndi.binary_dilation(poly(PEOPLE), iterations=P(.002))
rows = (np.arange(N) + .5) / N
strip = np.where((rows < .596)[:, None], rgb[:, P(.222):P(.232)].mean(1), rgb[:, P(.2075):P(.214)].mean(1))
gap = (rows > .596) & (rows < .628)                                   # a rail ends there
for k in range(3): strip[gap, k] = np.interp(rows[gap], rows[~gap], strip[~gap, k])
grain = (np.random.RandomState(1).rand(N, N, 1) - .5) * .03
rgb[people] = (strip[yy[people]] + grain[people]).clip(0, 1)
# The tree at the head of the stairs: its trunk first, then its twigs and those of the one on the left.
x0, x1, y0, y1 = P(.402), P(.427), P(.33), P(.492)
f = np.linspace(0, 1, x1 - x0)[None, :, None]
rgb[y0:y1, x0:x1] = rgb[y0:y1, x0 - 2:x0 - 1] * (1 - f) + rgb[y0:y1, x1 + 1:x1 + 2] * f
x0, x1, y0, y1 = P(.268), P(.51), P(.14), P(.495)
med = ndi.median_filter(rgb[y0:y1, x0:x1], size=(P(.011), P(.011), 1))
twigs = (house & (XF > .272))[y0:y1, x0:x1]
rgb[y0:y1, x0:x1][twigs] = med[twigs]

# ---- the rails ----
# Rails and their posts: thin and lighter than the steps behind them, or thin and darker than the wall behind
# them where they run up along a house.  They become lines, each with its width and how light it is, and what was
# behind them takes their place in the picture.  The three bollards on the pavement are of the same stuff.
L = lum(rgb)
thin = (P(.008), P(.008))
light = L - ndi.grey_opening(L, size=thin); darkl = ndi.grey_closing(L, size=thin) - L
ALONG = [[(.62, .49), (.7, .5), (.8, .55), (.86, .62), (1, .62), (1, .68), (.83, .68), (.62, .57)],
         [(.118, .6), (.17, .5), (.272, .455), (.272, .5), (.2, .545), (.14, .625)]]
alongwall = np.zeros((N, N), bool)
for p in ALONG: alongwall |= poly(p)
def solid(m):                                                             # without the specks
    lab, n = ndi.label(m, structure=np.ones((3, 3)))
    return m & (ndi.sum(m, lab, np.arange(n + 1))[lab] > P(.004) ** 2 * 3)
lightrail = solid(ndi.binary_dilation(steps, structure=np.ones((P(.02), 1), bool)) & ~people & (light > np.where(YF > .685, .16, .07)))
darkrail = solid(alongwall & (darkl > .09))
rail = lightrail | darkrail
# A rail is long and thin.  The rocks beside the stairs and what is sprayed on the walls are neither: only what
# lies on a straight line of some length stays, and nothing of what is broad.
def lines(m, length):
    y0, y1 = P(.45), P(.84); part = m[y0:y1]; keep = np.zeros_like(part); h = length // 2
    for a in np.arange(0, np.pi, np.pi / 24):
        e = np.zeros((2 * h + 1, 2 * h + 1), bool); t = np.arange(-h, h + 1)
        e[np.round(h + t * np.sin(a)).astype(int), np.round(h + t * np.cos(a)).astype(int)] = True
        keep |= ndi.binary_opening(part, structure=e)
    out = np.zeros_like(m); out[y0:y1] = keep; return out
broad = ndi.binary_dilation(ndi.binary_opening(rail, structure=box(.0065)), iterations=P(.003))
rail = lines(rail & ~broad, P(.022))
thick = ndi.distance_transform_edt(rail)
# The lines: the middle of every rail, cut apart where two of them meet, each piece followed from one end to the
# other and straightened.
skel = thinned(ndi.binary_closing(rail, structure=np.ones((3, 3), bool)))
ring = [np.roll(skel, (-dy, -dx), (0, 1)) for dy, dx in ((-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1))]
arms = sum((~a & b).astype(int) for a, b in zip(ring, ring[1:] + ring[:1]))      # how many lines leave a point
skel &= ~ndi.binary_dilation(skel & (arms > 2), structure=np.ones((3, 3), bool))
lab, n = ndi.label(skel, structure=np.ones((3, 3)))
def straighten(pts, eps):                                                 # Douglas and Peucker's way
    if len(pts) < 3: return [pts[0], pts[-1]]
    a, b = np.array(pts[0], float), np.array(pts[-1], float); d = b - a; q = np.array(pts, float) - a
    off = np.abs(q[:, 0] * d[1] - q[:, 1] * d[0]) / max(np.hypot(*d), 1e-6) if d.any() else np.hypot(q[:, 0], q[:, 1])
    i = int(off.argmax())
    return [pts[0], pts[-1]] if off[i] < eps else straighten(pts[:i + 1], eps)[:-1] + straighten(pts[i:], eps)
RAILS = []                                                                # x, y, x, y, how wide, how light
for i, sl in enumerate(ndi.find_objects(lab), 1):
    ys, xs = np.nonzero(lab[sl] == i)
    if len(ys) < P(.012): continue
    left = set(zip(ys.tolist(), xs.tolist()))
    around = lambda p: [(p[0] + dy, p[1] + dx) for dy, dx in ((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)) if (p[0] + dy, p[1] + dx) in left]
    while left:
        p = next((q for q in left if len(around(q)) == 1), next(iter(left)))
        chain = []
        while p is not None:
            chain.append((p[1] + sl[1].start, p[0] + sl[0].start)); left.discard(p); nxt = around(p); p = nxt[0] if nxt else None
        if len(chain) < P(.008): continue
        line = straighten(chain, 1.6)
        for (xa, ya), (xb, yb) in zip(line, line[1:]):
            m = int(max(abs(xb - xa), abs(yb - ya))) + 1
            px_ = np.linspace(xa, xb, m).round().astype(int); py_ = np.linspace(ya, yb, m).round().astype(int)
            if abs(yb - ya) < .025 * abs(xb - xa) and min(ya, yb) > .69 * N and steps[py_, px_].mean() > .5: continue   # level, low down: the edge of a step
            RAILS.append((xa / N, ya / N, xb / N, yb / N, float(np.median(thick[py_, px_])) * 2 / N, float(np.clip(L[py_, px_].mean() / .8, 0, 1))))
near = ndi.binary_dilation(rail, iterations=P(.0028))                       # right beside a rail the camera has drawn a dark line
far, (iy, ix) = ndi.distance_transform_edt(near, return_indices=True)
near &= far < P(.0085)                                                    # not where they stand so close that nothing shows between them
rgb[near] = rgb[iy[near], ix[near]]                                       # behind it is what is next to it
BOLLARDS = [(.3775, .3925, .716, .832), (.692, .7065, .715, .832), (.9635, .981, .713, .832)]
for a, b, c, e in BOLLARDS:
    x0, x1, y0, y1 = P(a) - 3, P(b) + 3, P(c) - 3, P(e)
    RAILS.append(((a + b) / 2, c + (b - a) / 2, (a + b) / 2, e, b - a, float(np.clip(L[y0:y1, P(a) + 3:P(b) - 3].mean() / .8, 0, 1))))
    f = np.linspace(0, 1, x1 - x0)[None, :, None]
    rgb[y0:y1 + 6, x0:x1] = rgb[y0:y1 + 6, x0 - 2:x0 - 1] * (1 - f) + rgb[y0:y1 + 6, x1 + 1:x1 + 2] * f

R, G, B = rgb[..., 0], rgb[..., 1], rgb[..., 2]
mx, mn = rgb.max(-1), rgb.min(-1)
V = mx; S = np.where(mx > 0, (mx - mn) / np.maximum(mx, 1e-6), 0)
d = np.maximum(mx - mn, 1e-6)
Hh = np.where(mx == R, ((G - B) / d) % 6, np.where(mx == G, (B - R) / d + 2, (R - G) / d + 4)) * 60
L = lum(rgb)

# ---- where the sun falls ----
# On the upper part of the big house on the right, on the yellow one beside it and the one at the end of the
# street; in patches on the pavement and the road, and on the slats.  The stairs lie in the shade.
GAIN = 1 / .6                                                  # how much lighter a thing is in the sun: places/thurngasse.js knows it too
wallL = ndi.grey_closing(L, size=(P(.04), P(.04)))             # the wall without its windows
fine = ndi.grey_closing(L, size=(P(.006), P(.006)))
sun = np.zeros((N, N), np.float32)
m = house & ~lefthouse & (XF > .345); sun[m] = smooth(.6, .7, wallL)[m]
m = house & farhouse; sun[m] = smooth(.42, .52, wallL)[m]
sun[pave] = smooth(.4, .5, fine)[pave]; sun[road] = smooth(.22, .3, fine)[road]
m = fence & ~below; sun[m] = smooth(.42, .55, fine)[m]
sun = blur(sun, P(.0025))
Lf = L / (1 + (GAIN - 1) * sun)                                # how light it would be without

# ---- what things are ----
NONE, WALL, CREAM, YELLOW, WARM, GLASS, LIT, STEP, PAVE, ROAD = range(10)
cls = np.full((N, N), WALL, np.uint8)
cls[house & (Hh > 16) & (Hh < 64) & (S > .12) & (V > .25)] = CREAM
cls[house & (Hh > 28) & (Hh < 58) & (S > .34) & (V > .35)] = YELLOW
cls[house & farhouse & ((Hh < 22) | (Hh > 340)) & (S > .25) & (V > .22)] = WARM
cls[lefthouse] = WALL
cls[fence & ~below] = CREAM
cls[steps] = STEP; cls[pave] = PAVE; cls[road] = ROAD

# Windows: what is a good deal darker than the wall round it, and of the size and shape of one.  Some have a
# light on at night.
around = ndi.grey_closing(Lf, size=(P(.05), P(.05)))
dark = house & ~fence & (YF < .6) & (Lf < around * .62) & (around - Lf > .09)
dark = ndi.binary_opening(dark, structure=box(.003))
lab, n = ndi.label(dark)
glass = np.zeros((N, N), bool); lit = np.zeros((N, N), bool)
rnd = np.random.RandomState(3)
for i, sl in enumerate(ndi.find_objects(lab), 1):
    h, w = sl[0].stop - sl[0].start, sl[1].stop - sl[1].start
    part = lab[sl] == i; area = part.sum()
    if area < P(.005) ** 2 or h > P(.11) or w > P(.05) or h < w * .6 or h > w * 8 or area < .4 * h * w: continue
    glass[sl] |= part
    if rnd.rand() < .4: lit[sl] |= part
cls[glass] = GLASS; cls[lit] = LIT

cls[sky] = NONE

# ---- how light: every kind of thing has its own scale, from its darkest to its lightest ----
Lo = np.clip(Lf + (Lf - blur(Lf, 3.5)) * .7, 0, 1)                       # a little crisper than the camera had it
lev = np.zeros((N, N), np.float32)
SPAN = {}
for c in range(1, 10):
    m = cls == c
    if not m.any(): continue
    lo, hi = np.percentile(Lo[m], [1.5, 98.5])
    if c in (WALL, CREAM, YELLOW, WARM): lo, hi = .05, .97 / GAIN        # what is white in the sun is as light as anything gets
    SPAN[c] = (lo, hi)
    lev[m] = np.clip((Lo[m] - lo) / max(hi - lo, 1e-3), 0, 1)

def reduce(w):
    """the map, w cells wide: the kind most of a cell is, and how light that is"""
    idx = np.minimum((np.arange(N) * w / N).astype(int), w - 1); idx = idx[:, None] * w + idx[None, :]
    cnt = np.zeros((10, w * w)); sm = np.zeros((10, w * w))
    for k in range(10):
        m = cls == k
        cnt[k] = np.bincount(idx[m], minlength=w * w); sm[k] = np.bincount(idx[m], weights=lev[m], minlength=w * w)
    wt = np.ones((10, 1)); wt[GLASS] = wt[LIT] = 1.3; wt[NONE] = 1.2
    best = (cnt * wt).argmax(0)
    level = sm[best, np.arange(w * w)] / np.maximum(cnt[best, np.arange(w * w)], 1)
    return best.reshape(w, w).astype(np.uint8), np.clip((level * 8).astype(int), 0, 7).reshape(w, w).astype(np.uint8)

A = ''.join(chr(c) for c in range(35, 127) if c != 92)                    # 91 characters that need no escaping
mc, ml = reduce(MW)
for x in range(MW):                                                       # the sky is all there is above the roofs, and nothing of it below
    col = mc[:, x]; nz = np.nonzero(col)[0]; t = nz[0] if len(nz) else MW
    for i in range(t + 1, MW):
        if col[i] == NONE: col[i] = col[i - 1]
cells = ''.join(A[a * 8 + b] for a, b in zip(mc.ravel(), ml.ravel()))
q = N // SW; sg = sun[:q * SW, :q * SW].reshape(SW, q, SW, q).mean((1, 3))
suns = ''.join(str(int(v)) for v in np.clip((sg * 8).astype(int), 0, 7).ravel())
with open(OUT, 'w') as f:
    f.write('/* Generated by tools/thurngasse-map.py from the photograph the Thurngasse is painted after.  Not to be edited:\n'
            ' * what there is at every point, how light it is there without the sun, one character each; the rails of the\n'
            ' * stairs, six numbers each; and where the sun falls, one digit each (see places/thurngasse.js). */\n')
    f.write('(window.viennaMaps = window.viennaMaps || {}).thurngasse = {\n')
    f.write(f'  w: {MW}, h: {MW}, gain: {GAIN:.4f},\n  cells: "{cells}",\n')
    f.write('  rails: [' + ','.join(','.join(str(int(round(v * 10000))) for v in r[:5]) + ',' + str(int(min(7, r[5] * 8))) for r in RAILS) + '],\n')
    f.write(f'  sun: {{ w: {SW}, h: {SW},\n    cells: "{suns}" }},\n}};\n')
print('map', mc.shape, len(cells), 'rails', len(RAILS), 'sun', len(suns), {k: (round(float(a), 2), round(float(b), 2)) for k, (a, b) in SPAN.items()})

if len(sys.argv) > 4:                                                     # pictures to check it by
    COL = np.array([[120, 170, 230], [235, 235, 235], [225, 205, 170], [240, 205, 70], [205, 95, 75], [50, 70, 110], [255, 150, 0], [150, 130, 105], [130, 135, 145], [70, 72, 80]], np.float32)
    Image.fromarray(COL[cls].astype(np.uint8)).save(sys.argv[4] + '-cls.png')
    pic = Image.fromarray((COL[mc] * (.3 + .7 * ml[..., None] / 7)).astype(np.uint8)).resize((1344, 1344), Image.NEAREST); dr = ImageDraw.Draw(pic)
    for r in RAILS: dr.line([(r[0] * 1344, r[1] * 1344), (r[2] * 1344, r[3] * 1344)], fill=(int(255 * r[5]), 0, 255), width=max(1, int(round(r[4] * 1344))))
    pic.save(sys.argv[4] + '-map.png')
    Image.fromarray((np.clip(rgb * (.35 + .65 * sun[..., None]), 0, 1) * 255).astype(np.uint8)).save(sys.argv[4] + '-sun.png')
    Image.fromarray((np.clip(rgb / (1 + (GAIN - 1) * sun[..., None]) * 1.3, 0, 1) * 255).astype(np.uint8)).save(sys.argv[4] + '-flat.png')
    pic = np.zeros((N, N, 3), np.uint8); pic[rail] = (90, 90, 90); pic[skel] = (255, 60, 60)      # what was taken for a rail, its middle, the lines it became
    pic = Image.fromarray(pic); dr = ImageDraw.Draw(pic)
    for r in RAILS: dr.line([(r[0] * N, r[1] * N), (r[2] * N, r[3] * N)], fill=(60, 255, 60), width=1)
    pic.crop((0, P(.45), N, P(.84))).save(sys.argv[4] + '-rails.png')
