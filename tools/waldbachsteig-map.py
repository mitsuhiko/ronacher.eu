# /// script
# dependencies = ["pillow", "numpy", "scipy"]
# ///
# Reads the photograph the Waldbachsteig is painted after and writes places/waldbachsteig-map.js: what grows at every
# point of it (leaves, the wooded hill, the bed, grass) or whether it is sky there, and how light it is.  The saint,
# the fence, the wall and the bench are taken out: they are painted by hand.  Where the wall and the bench were, the
# map has leaves, as light and dark as they are further up: the picture goes on beyond the photograph as its mirror
# image, and there bushes stand where here the wall does.
#   uv run tools/waldbachsteig-map.py photo.jpg places/waldbachsteig-map.js [width] [prefix of pictures to check it by]
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

SKY, LEAF, WISP, HILL, BED, GRASS = range(6)

# ---- the lie of the land, as places/waldbachsteig.js has it ----
RIDGE = [(0, .405), (.3, .4), (.44, .39), (.47, .379), (.5, .369), (.55, .361), (.6, .355), (.7, .343), (.8, .33), (.9, .325), (1, .328)]
HEDGE = [(0, .6), (.3, .6), (.36, .5), (.42, .46), (.5, .45), (.53, .44), (.57, .43), (.62, .405), (.67, .385), (.75, .375), (.8, .36), (.84, .343), (.87, .35), (.9, .365), (1, .365)]
BEDTOP = [(0, .6), (.1, .605), (.2, .59), (.3, .575), (.36, .555), (.42, .565), (.447, .6), (.456, .648), (.54, .652), (.56, .64), (.6, .632), (.7, .625), (.8, .63), (.9, .635), (1, .64)]
CROWN = [(0, .5), (.33, .5), (.36, .455), (.4, .435), (.46, .43), (.5, .4), (.56, .36), (.6, .3), (1, .3)]   # how far to the right the walnut reaches at a height: y, x
WEND = .895                                                  # where the wall ends
coping = .7 + .1 * (2.09 - XF) / 2.09                        # the back of its coping
ridge, hedge, bedtop, crown = along(RIDGE, XF), along(HEDGE, XF), along(BEDTOP, XF), along(CROWN, YF)

blue = ((B - R > .1) & (B >= G) & (mx > .5)) | (mn > .82)    # the sky, and the sky between leaves, which is white
tree = XF < crown
sky = np.where(tree, blur(blue, 1.2) > .5, YF < ridge)
sky &= YF < bedtop - .03
green = blur(((G - B) / np.maximum(mx, .05) > .22) & (G > R * 1.04), 3) > .5
low = np.where(XF < WEND, YF > coping, YF > .775 + rnd(3, 6) * .004)   # below the bed: the wall with the bench in front of it, and the grass
stone = low & ~((XF > WEND) | (green & (YF > .86)))
grass = low & ~stone
bed = ~low & (YF > bedtop)
hill = ~low & ~bed & ~tree & ~sky & (YF < hedge)
leaf = ~low & ~bed & ~sky & ~hill

# ---- what is not to be painted from it ----
Lo = L.copy()
def mend(mask, among, s=10, grain=.05, seed=7):              # what is taken out is as light as what is around it, of its own kind
    keep = among & ~ndi.binary_dilation(mask, iterations=3)
    Lo[mask] = (blur(Lo * keep, s) / np.maximum(blur(keep, s), 1e-3))[mask] + rnd(seed, 2.5)[mask] * grain + rnd(seed + 1, 7)[mask] * grain
saint = poly([(.415, .44), (.44, .415), (.46, .385), (.5, .378), (.525, .395), (.555, .43), (.558, .51), (.552, .59), (.556, .68), (.435, .68), (.438, .6), (.445, .53), (.41, .5)])
iron = ndi.binary_dilation((mx < .2) & (G - (R + B) / 2 < .035) & (YF > .585) & (YF < .86) & ~stone, iterations=2)   # the fence, and the gate
bench = (XF > .455) & (YF > .805 + (1 - XF) * .09) & grass
mend(saint, leaf | hill | bed, 18, .07, 11)
mend(iron & ~saint, bed | leaf | grass, 6, .04, 17)
mend(bench, grass & ~bench, 16, .05, 23)

cls = np.where(sky, SKY, np.where(hill, HILL, np.where(bed, BED, np.where(grass, GRASS, LEAF)))).astype(np.uint8)

# ---- how light: every kind of thing has its own scale, from its darkest to its lightest ----
lev = np.zeros((WH, WW), np.float32)
SPAN = {}
for c in (LEAF, HILL, BED, GRASS):
    m = (cls == c) & ~stone
    lo, hi = np.percentile(Lo[m], [1.5, 98.5])
    SPAN[c] = (round(float(lo), 2), round(float(hi), 2))
    lin = np.clip((Lo[m] - lo) / max(hi - lo, 1e-3), 0, 1)
    rank = np.searchsorted(np.sort(Lo[m]), Lo[m]) / m.sum()  # half by how light it is, half by how many are lighter: no step of the scale goes unused
    lev[m] = lin * .5 + rank * .5
# where the wall and the bench are: leaves, as light as those a good way further up
UP = int(.25 * WH)
src = np.roll(lev, UP, axis=0); ok = np.roll((cls == LEAF) | (cls == BED), UP, axis=0) & ~np.roll(stone, UP, axis=0)
fill = blur(lev * ((cls != SKY) & ~stone), 20) / np.maximum(blur((cls != SKY) & ~stone, 20), 1e-3)
lev[stone] = np.where(ok, src, fill)[stone]

# ---- the map: the kind most of a cell is, and how light that is.  A cell that is leaves and sky is a wisp of leaves:
# there the scale says how much of it is leaves.
ys = np.minimum((np.arange(WH) * MH / WH).astype(int), MH - 1); xs = np.minimum((np.arange(WW) * MW / WW).astype(int), MW - 1)
idx = (ys[:, None] * MW + xs[None, :])
K = 6
cnt = np.zeros((K, MH * MW)); sm = np.zeros((K, MH * MW))
for k in range(K):
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
    f.write('/* Generated by tools/waldbachsteig-map.py from the photograph the Waldbachsteig is painted after.  Not to be edited:\n'
            ' * what grows at every point of it and how light it is there, one character each (see places/waldbachsteig.js). */\n')
    f.write('(window.viennaMaps = window.viennaMaps || {}).waldbachsteig = {\n')
    f.write(f'  w: {MW}, h: {MH},\n  cells: "{cells}",\n}};\n')
print('map', kind.shape, len(cells), SPAN)

if len(sys.argv) > 4:                                        # a picture to check it by
    COL = np.array([[120, 170, 230], [30, 110, 40], [190, 220, 90], [110, 150, 120], [150, 210, 60], [120, 190, 90]], np.float32)
    pic = COL[kind] * np.where(kind == SKY, 1, .25 + .75 * level / 7)[..., None]
    Image.fromarray(pic.astype(np.uint8)).resize((MW * 3, MH * 3), Image.NEAREST).save(sys.argv[4] + '-map.png')
