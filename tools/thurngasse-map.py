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
from skimage.transform import probabilistic_hough_line

SRC, OUT = sys.argv[1], sys.argv[2]
MW = int(sys.argv[3]) if len(sys.argv) > 3 else 448          # how many cells wide the map is
MH = MW * 3 // 2                                             # and how many high: a step of the stairs is long, but not high
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
STAIR_TOP = [(0, .68), (.118, .68), (.122, .612), (.15, .573), (.18, .536), (.205, .519), (.255, .501), (.272, .492), (.33, .482), (.5, .482), (.56, .49), (.6, .5),
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
# A twig is thin and dark, a window is dark too but broader: what is light on both sides of something thin takes
# its place.  Smoothing all of it would take the windows along.
x0, x1, y0, y1 = P(.268), P(.51), P(.14), P(.495)
twigs = (house & (XF > .272))[y0:y1, x0:x1]
for k in range(3):
    c = rgb[y0:y1, x0:x1, k]; c[twigs] = ndi.grey_closing(c, size=(P(.0048), P(.0048)))[twigs]

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
# The lines: whatever of their middles lies on a straight line of some length, which is how a rail differs from
# what else is thin up there.  The posts are shorter, but they are upright.
skel = thinned(ndi.binary_closing(rail, structure=np.ones((3, 3), bool)))
slant = np.linspace(-np.pi / 2, np.pi / 2, 180, endpoint=False)
found = probabilistic_hough_line(skel, threshold=25, line_length=P(.035), line_gap=P(.006), theta=slant[np.abs(slant) > .14], rng=1)
found += probabilistic_hough_line(skel, threshold=12, line_length=P(.014), line_gap=P(.003), theta=np.linspace(-.12, .12, 9), rng=1)
RAILS = []                                                                # x, y, x, y, how wide, how light
for (xa, ya), (xb, yb) in found:
    m = int(max(abs(xb - xa), abs(yb - ya))) + 1
    xs = np.linspace(xa, xb, m).round().astype(int); ys = np.linspace(ya, yb, m).round().astype(int)
    if abs(yb - ya) < .025 * abs(xb - xa) and min(ya, yb) > .69 * N and steps[ys, xs].mean() > .5: continue   # level, low down: the edge of a step
    on = thick[ys, xs] > 0
    RAILS.append((xa / N, ya / N, xb / N, yb / N, float(np.median(thick[ys, xs][on])) * 2 / N, float(np.clip(L[ys, xs][on].mean() / .8, 0, 1))))
near = ndi.binary_dilation(rail, iterations=P(.0028))                       # right beside a rail the camera has drawn a dark line
far, (iy, ix) = ndi.distance_transform_edt(near, return_indices=True)
single = near & (far < P(.0085))
rgb[single] = rgb[iy[single], ix[single]]                                 # behind it is what is next to it
crowd = near & ~single                                                    # where they stand so close that little shows between them: only the rails themselves go
for k in range(3):
    c = rgb[..., k]; c[crowd & ~alongwall] = ndi.grey_opening(c, size=(P(.005), P(.005)))[crowd & ~alongwall]
BOLLARDS = [(.3775, .3925, .716, .832), (.692, .7065, .715, .832), (.9635, .981, .713, .832)]
for a, b, c, e in BOLLARDS:
    x0, x1, y0, y1 = P(a) - 3, P(b) + 3, P(c) - 3, P(e)
    RAILS.append(((a + b) / 2, c + (b - a) / 2, (a + b) / 2, e, b - a, float(np.clip(L[y0:y1, P(a) + 3:P(b) - 3].mean() / .8, 0, 1))))
    f = np.linspace(0, 1, x1 - x0)[None, :, None]
    rgb[y0:y1 + 6, x0:x1] = rgb[y0:y1 + 6, x0 - 2:x0 - 1] * (1 - f) + rgb[y0:y1 + 6, x1 + 1:x1 + 2] * f
# A step is the same from one end to the other: the grain of the stone and what is left of the rails go, the edges
# of the steps and the ends of the flights stay.
y0, y1 = P(.46), P(.83)
for k in range(3):
    c = rgb[y0:y1, :, k]; c[steps[y0:y1]] = ndi.median_filter(c, size=(1, P(.012)))[steps[y0:y1]]

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
NONE, WALL, CREAM, YELLOW, WARM, GLASS, STEP, PAVE, ROAD = range(9)
cls = np.full((N, N), WALL, np.uint8)
cls[house & (Hh > 16) & (Hh < 64) & (S > .12) & (V > .25)] = CREAM
cls[house & (Hh > 28) & (Hh < 58) & (S > .34) & (V > .35)] = YELLOW
cls[house & farhouse & ((Hh < 22) | (Hh > 340)) & (S > .25) & (V > .22)] = WARM
cls[lefthouse] = WALL
cls[fence & ~below] = CREAM
cls[steps] = STEP; cls[pave] = PAVE; cls[road] = ROAD

# Windows: what is a good deal darker than the wall round it, and of the size and shape of one.  By day that is
# all there is to them.  At night some have a light on, and a lit window has four corners, which cells are bad at:
# those of the house on the left are written down as shapes too, each with its upright sides and the lean of its
# sill.  Those of the big house on the right the sun is on, and they are not dark at all, and those of the house
# at the end of the street are small and behind a tree: they were measured by hand, clockwise from the upper left
# corner.
BIG = [[.822, .060, .8485, .020, .855, .1365, .826, .158], [.761, .1525, .779, .130, .783, .2175, .764, .239], [.717, .038, .7345, .005, .7375, .0875, .720, .118],
       [.721, .205, .740, .180, .743, .267, .7245, .285], [.6875, .098, .7025, .068, .705, .145, .6895, .171], [.686, 0, .700, -.01, .7015, .040, .687, .062],
       [.6907, .2475, .7056, .228, .7077, .310, .6939, .324], [.6623, .032, .6736, .019, .6757, .0896, .6636, .113], [.6644, .1387, .6757, .1216, .6779, .193, .6657, .2133],
       [.633, .1003, .6422, .0832, .6437, .1515, .6341, .1717], [.6346, .1973, .6442, .1813, .6452, .2432, .6356, .2603], [.7547, -.005, .7739, -.03, .776, .028, .7568, .0576],
       [.8315, .2835, .858, .2685, .863, .388, .8357, .4072], [.7675, .3453, .7873, .3336, .792, .437, .7717, .4488], [.7295, .3784, .7468, .3688, .7493, .4627, .7327, .4712],
       [.6977, .4093, .7114, .4008, .7148, .4861, .7003, .4936], [.6715, .4317, .6828, .4243, .6858, .5032, .6736, .5085], [.6388, .4563, .6459, .452, .6486, .5171, .6405, .5192],
       [.6666, .2856, .6785, .2717, .68, .34, .6678, .3539], [.6341, .324, .6437, .3133, .6448, .3827, .6348, .3933], [.7306, .5235, .7461, .5192, .7472, .566, .7312, .572]]
BIG += [[a, c, b, c, b, e, a, e] for a, b, c, e in ((.3513, .362, .402, .4195), (.3919, .4025, .402, .4195), (.4372, .4468, .403, .417), (.3508, .362, .4387, .4637), (.3913, .4031, .4387, .4637), (.4367, .4479, .4429, .4632))]
rnd = np.random.RandomState(7)
WINDOWS = [q + [int(rnd.rand() < .42)] for q in BIG]                      # corner by corner, and whether it is lit
around = ndi.grey_closing(Lf, size=(P(.05), P(.05)))
dark = house & ~fence & (YF < .6) & ((XF < .272) | (XF > .345)) & (Lf < around * .62) & (around - Lf > .09)   # not what is left of the tree in front of the yellow house
dark = ndi.binary_opening(dark, structure=box(.003))
lab, n = ndi.label(dark)
glass = np.zeros((N, N), bool)
for i, sl in enumerate(ndi.find_objects(lab), 1):
    h, w = sl[0].stop - sl[0].start, sl[1].stop - sl[1].start
    part = lab[sl] == i; area = part.sum()
    if area < P(.005) ** 2 or h > P(.11) or w > P(.05) or h < w * .6 or h > w * 8 or area < .4 * h * w: continue
    glass[sl] |= part
    on = rnd.rand() < .4
    if sl[1].start < P(.14) or sl[1].stop > P(.272): continue             # a balcony; one of those measured by hand, or small and far up the street
    # Its sides are upright, its upper and lower edge run the same way: the way its middle runs, column by column.
    cols = np.nonzero(part.any(0))[0]; top = part.argmax(0)[cols]; bot = h - part[::-1].argmax(0)[cols]
    lean = np.polyfit(cols, (top + bot) / 2, 1)[0] if len(cols) >= 4 else 0
    high = np.percentile(bot - top, 70); mid = np.median((top + bot) / 2 - lean * cols)
    x0, x1 = sl[1].start, sl[1].stop; y0 = sl[0].start + mid - high / 2
    if w > P(.02) or abs(lean) > 1.5: continue                            # not a window after all
    WINDOWS.append([v / N for v in (x0, y0, x1, y0 + lean * w, x1, y0 + lean * w + high, x0, y0 + high)] + [int(on)])
cls[glass] = GLASS
cls[sky] = NONE

# ---- how light: every kind of thing has its own scale, from its darkest to its lightest ----
Lo = np.clip(Lf + (Lf - blur(Lf, 3.5)) * .7, 0, 1)                       # a little crisper than the camera had it
lev = np.zeros((N, N), np.float32)
SPAN = {}
for c in range(1, 9):
    m = cls == c
    if not m.any(): continue
    lo, hi = np.percentile(Lo[m], [1.5, 98.5])
    if c in (WALL, CREAM, YELLOW, WARM): lo, hi = .05, .97 / GAIN        # what is white in the sun is as light as anything gets
    SPAN[c] = (lo, hi)
    lev[m] = np.clip((Lo[m] - lo) / max(hi - lo, 1e-3), 0, 1)

def reduce(w, h):
    """the map, w cells wide and h high: the kind most of a cell is, and how light that is"""
    idx = np.minimum((np.arange(N) * h / N).astype(int), h - 1)[:, None] * w + np.minimum((np.arange(N) * w / N).astype(int), w - 1)[None, :]
    cnt = np.zeros((9, w * h)); sm = np.zeros((9, w * h))
    for k in range(9):
        m = cls == k
        cnt[k] = np.bincount(idx[m], minlength=w * h); sm[k] = np.bincount(idx[m], weights=lev[m], minlength=w * h)
    wt = np.ones((9, 1)); wt[GLASS] = 1.3; wt[NONE] = 1.2
    best = (cnt * wt).argmax(0)
    level = sm[best, np.arange(w * h)] / np.maximum(cnt[best, np.arange(w * h)], 1)
    return best.reshape(h, w).astype(np.uint8), np.clip((level * 8).astype(int), 0, 7).reshape(h, w).astype(np.uint8)

A = ''.join(chr(c) for c in range(35, 127) if c != 92)                    # 91 characters that need no escaping
mc, ml = reduce(MW, MH)
for x in range(MW):                                                       # the sky is all there is above the roofs, and nothing of it below
    col = mc[:, x]; nz = np.nonzero(col)[0]; t = nz[0] if len(nz) else MH
    for i in range(t + 1, MH):
        if col[i] == NONE: col[i] = col[i - 1]
cells = ''.join(A[a * 8 + b] for a, b in zip(mc.ravel(), ml.ravel()))
q = N // SW; sg = sun[:q * SW, :q * SW].reshape(SW, q, SW, q).mean((1, 3))
suns = ''.join(str(int(v)) for v in np.clip((sg * 8).astype(int), 0, 7).ravel())
with open(OUT, 'w') as f:
    f.write('/* Generated by tools/thurngasse-map.py from the photograph the Thurngasse is painted after.  Not to be edited:\n'
            ' * what there is at every point, how light it is there without the sun, one character each; the rails of the\n'
            ' * stairs, six numbers each; the windows, nine numbers each; and where the sun falls, one digit each (see\n'
            ' * places/thurngasse.js). */\n')
    f.write('(window.viennaMaps = window.viennaMaps || {}).thurngasse = {\n')
    f.write(f'  w: {MW}, h: {MH}, gain: {GAIN:.4f},\n  cells: "{cells}",\n')
    f.write('  rails: [' + ','.join(','.join(str(int(round(v * 10000))) for v in r[:5]) + ',' + str(int(min(7, r[5] * 8))) for r in RAILS) + '],\n')
    f.write('  windows: [' + ','.join(','.join(str(int(round(v * 10000))) for v in w[:8]) + f',{w[8]}' for w in WINDOWS) + '],\n')
    f.write(f'  sun: {{ w: {SW}, h: {SW},\n    cells: "{suns}" }},\n}};\n')
print('map', mc.shape, len(cells), 'rails', len(RAILS), 'windows', len(WINDOWS), 'sun', len(suns), {k: (round(float(a), 2), round(float(b), 2)) for k, (a, b) in SPAN.items()})

if len(sys.argv) > 4:                                                     # pictures to check it by
    COL = np.array([[120, 170, 230], [235, 235, 235], [225, 205, 170], [240, 205, 70], [205, 95, 75], [50, 70, 110], [150, 130, 105], [130, 135, 145], [70, 72, 80]], np.float32)
    Image.fromarray(COL[cls].astype(np.uint8)).save(sys.argv[4] + '-cls.png')
    pic = Image.fromarray((COL[mc] * (.3 + .7 * ml[..., None] / 7)).astype(np.uint8)).resize((1344, 1344), Image.NEAREST); dr = ImageDraw.Draw(pic)
    for r in RAILS: dr.line([(r[0] * 1344, r[1] * 1344), (r[2] * 1344, r[3] * 1344)], fill=(int(255 * r[5]), 0, 255), width=max(1, int(round(r[4] * 1344))))
    for w in WINDOWS: dr.polygon([(w[i] * 1344, w[i + 1] * 1344) for i in range(0, 8, 2)], outline=(255, 150, 0) if w[8] else (0, 0, 0))
    pic.save(sys.argv[4] + '-map.png')
    Image.fromarray((np.clip(rgb * (.35 + .65 * sun[..., None]), 0, 1) * 255).astype(np.uint8)).save(sys.argv[4] + '-sun.png')
    Image.fromarray((np.clip(rgb / (1 + (GAIN - 1) * sun[..., None]) * 1.3, 0, 1) * 255).astype(np.uint8)).save(sys.argv[4] + '-flat.png')
    pic = np.zeros((N, N, 3), np.uint8); pic[rail] = (90, 90, 90); pic[skel] = (255, 60, 60)      # what was taken for a rail, its middle, the lines it became
    pic = Image.fromarray(pic); dr = ImageDraw.Draw(pic)
    for r in RAILS: dr.line([(r[0] * N, r[1] * N), (r[2] * N, r[3] * N)], fill=(60, 255, 60), width=1)
    pic.crop((0, P(.45), N, P(.84))).save(sys.argv[4] + '-rails.png')
