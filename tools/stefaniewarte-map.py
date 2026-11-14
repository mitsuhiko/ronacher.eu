# /// script
# dependencies = ["pillow", "numpy", "scipy"]
# ///
# Reads the photograph the Stefaniewarte is painted after and writes places/stefaniewarte-map.js: what there is at
# every point of it (sky, leaves, grass, gravel) and how light it is there.  The tower with its stairs and the table
# in front of it is taken out: it is painted by hand.  So are the people who were about, and are not painted at all.
#   uv run tools/stefaniewarte-map.py photo.jpg places/stefaniewarte-map.js [width] [prefix of pictures to check it by]
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageOps
from scipy import ndimage as ndi

SRC, OUT = sys.argv[1], sys.argv[2]
MW = int(sys.argv[3]) if len(sys.argv) > 3 else 288          # how many cells wide the map is
MH = MW * 4 // 3
WW, WH = 768, 1024                                           # the size all of this is worked out at
im = ImageOps.exif_transpose(Image.open(SRC)).convert('RGB').resize((WW, WH), Image.LANCZOS)
rgb = np.asarray(im).astype(np.float32) / 255
R, G, B = rgb[..., 0], rgb[..., 1], rgb[..., 2]
mx, mn = rgb.max(-1), rgb.min(-1)
L = .3 * R + .55 * G + .15 * B
yy, xx = np.mgrid[0:WH, 0:WW]; XF = (xx + .5) / WW; YF = (yy + .5) / WH
blur = lambda a, s: ndi.gaussian_filter(a.astype(np.float32), s)
rnd = lambda seed, s: (lambda n: (n - n.mean()) / n.std())(blur(np.random.RandomState(seed).rand(WH, WW), s))

def poly(pts):
    m = Image.new('L', (WW, WH), 0); ImageDraw.Draw(m).polygon([(x * WW, y * WH) for x, y in pts], fill=1); return np.asarray(m).astype(bool)
def along(pts, x): return np.interp(x, [p[0] for p in pts], [p[1] for p in pts])

SKY, LEAF, WISP, GRASS, GRAVEL = range(5)
GROUND = .703                                                # where the trees end and the clearing begins
sky = (B - R > .1) & (B - G > .02) & (mx > .4)
ground = YF > GROUND + rnd(23, 3) * .0025
Lo = L.copy()
def mend(mask, grain=.05, seed=7):                           # what is taken out is as light as what is around it
    keep = ~ndi.binary_dilation(mask, iterations=3)
    Lo[mask] = (blur(Lo * keep, 14) / np.maximum(blur(keep, 14), 1e-3))[mask] + rnd(seed, 2.5)[mask] * grain + rnd(seed + 1, 7)[mask] * grain

# ---- what is not to be painted ----
# The tower with its dishes and aerials: behind it the sky, and the trees that stand to its left and right.
tower = poly([(.553, .195), (.6, .195), (.625, .29), (.665, .31), (.715, .345), (.74, .4), (.755, .44), (.755, .5), (.765, .6), (.785, .63), (.8, .66), (.8, GROUND + .01),
              (.465, GROUND + .01), (.465, .63), (.498, .6), (.492, .47), (.468, .43), (.468, .365), (.51, .345), (.54, .32), (.558, .28)])
behind = YF > .594 + (XF - .63) * .02 + rnd(3, 5) * .012 + rnd(5, 14) * .012
sky = np.where(tower, ~behind, sky)
Lo[tower] = .26 + rnd(7, 2.5)[tower] * .07 + rnd(8, 7)[tower] * .06 + np.clip(.66 - YF[tower], 0, .08) * 1.5

# ---- the clearing ----
green = ((G - B) / np.maximum(mx, .05) > .3) & (G > R * .97)
grass = blur(green, 2.5) > .5
grass &= ~((XF < .47) & (YF > .738 + rnd(21, 6) * .004))    # on the left the gravel reaches into the shade: what looks green there is not grass
# the foot of the tower, its stairs, the table in front of it and who stood there: gravel, the meadow behind it, weeds along the wall
foot = (XF > .455) & (XF < .812 + rnd(31, 5) * .006) & (YF > GROUND - .012) & (YF < .772 + rnd(33, 6) * .005)
weeds = foot & (YF < .731 + rnd(11, 6) * .004) & (((XF > .55) & (XF < .625)) | ((XF > .685) & (XF < .8)))
lawn = foot & (YF < .711 + rnd(12, 8) * .004)
grass = np.where(foot, weeds | lawn, grass)
mend(foot, .04, 13)
Lo[foot] = np.where(weeds[foot] & ~lawn[foot], .3, np.where(lawn[foot], .52, np.maximum(Lo[foot], .8))) + rnd(15, 2.5)[foot] * .05
grass |= ~foot & (YF < GROUND + .013 + rnd(39, 7) * .004)        # along the foot of the wood the grass is long: what is pale in it is straw, not gravel
kids = poly([(.32, .7), (.44, .7), (.44, .75), (.32, .75)])  # two boys playing in the gravel
grass[kids] = YF[kids] < .713 + rnd(17, 5)[kids] * .004; Lo[kids] = np.where(grass[kids], .5, .84) + rnd(19, 2.5)[kids] * .05
bench = (XF > .885 + rnd(35, 5) * .006) & (YF > .68) & (YF < .73 + rnd(37, 6) * .004)   # and somebody on a bench on the right: there the meadow goes on
sky[bench] = False; grass[bench] = True; mend(bench, .05, 29)

cls = np.where(ground, np.where(grass, GRASS, GRAVEL), np.where(sky, SKY, LEAF)).astype(np.uint8)

# ---- how light: every kind of thing has its own scale, from its darkest to its lightest ----
lev = np.zeros((WH, WW), np.float32)
SPAN = {}
for c in (LEAF, GRASS, GRAVEL):
    m = cls == c
    lo, hi = np.percentile(Lo[m], [1.5, 98.5])
    SPAN[c] = (round(float(lo), 2), round(float(hi), 2))
    lin = np.clip((Lo[m] - lo) / max(hi - lo, 1e-3), 0, 1)
    rank = np.searchsorted(np.sort(Lo[m]), Lo[m]) / m.sum()  # half by how light it is, half by how many are lighter: no step of the scale goes unused
    lev[m] = lin * .5 + rank * .5

# ---- the map: the kind most of a cell is, and how light that is.  A cell that is leaves and sky is a wisp of leaves:
# there the scale says how much of it is leaves.
ys = np.minimum((np.arange(WH) * MH / WH).astype(int), MH - 1); xs = np.minimum((np.arange(WW) * MW / WW).astype(int), MW - 1)
idx = (ys[:, None] * MW + xs[None, :])
cnt = np.zeros((5, MH * MW)); sm = np.zeros((5, MH * MW))
for k in range(5):
    m = cls == k
    cnt[k] = np.bincount(idx[m], minlength=MH * MW); sm[k] = np.bincount(idx[m], weights=lev[m], minlength=MH * MW)
tot = cnt.sum(0)
best = cnt[1:].argmax(0) + 1
level = sm[best, np.arange(MH * MW)] / np.maximum(cnt[best, np.arange(MH * MW)], 1)
cover = 1 - cnt[SKY] / tot
kind = np.where(cover < .1, SKY, np.where((cover < .8) & (best == LEAF), WISP, best))
level = np.where(kind == WISP, (cover - .1) / .7, level)
kind = kind.reshape(MH, MW).astype(np.uint8); level = np.clip((level * 8).astype(int), 0, 7).reshape(MH, MW).astype(np.uint8)
level[kind == SKY] = 0

A = ''.join(chr(c) for c in range(35, 127) if c != 92)       # 91 characters that need no escaping
cells = ''.join(A[a * 8 + b] for a, b in zip(kind.ravel(), level.ravel()))
with open(OUT, 'w') as f:
    f.write('/* Generated by tools/stefaniewarte-map.py from the photograph the Stefaniewarte is painted after.  Not to be edited:\n'
            ' * what there is at every point around the tower and how light it is there, one character each (see places/stefaniewarte.js). */\n')
    f.write('(window.viennaMaps = window.viennaMaps || {}).stefaniewarte = {\n')
    f.write(f'  w: {MW}, h: {MH}, ground: {GROUND},\n  cells: "{cells}",\n}};\n')
print('map', kind.shape, len(cells), SPAN)

if len(sys.argv) > 4:                                        # a picture to check it by
    COL = np.array([[120, 170, 230], [30, 110, 40], [190, 220, 90], [150, 200, 70], [200, 190, 170]], np.float32)
    pic = COL[kind] * np.where(kind == SKY, 1, .3 + .7 * level / 7)[..., None]
    Image.fromarray(pic.astype(np.uint8)).resize((MW * 3, MH * 3), Image.NEAREST).save(sys.argv[4] + '-map.png')
