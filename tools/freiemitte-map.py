# /// script
# dependencies = ["pillow", "numpy", "scipy"]
# ///
# Reads the photograph the Freie Mitte is painted after and writes places/freiemitte-map.js: what there is at every
# point under the sky (leaves, grass, a path, a wall ...) and how light it is there.  The cranes, the scaffolding
# round the water tower and the building site at its foot are taken out.
#   uv run tools/freiemitte-map.py photo.jpg places/freiemitte-map.js [town width] [park width] [prefix of pictures to check it by]
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageOps
from scipy import ndimage as ndi

SRC, OUT = sys.argv[1], sys.argv[2]
TW = int(sys.argv[3]) if len(sys.argv) > 3 else 768          # how many cells wide the map of the town is
PKW = int(sys.argv[4]) if len(sys.argv) > 4 else 448         # and that of the park
TOP, SPLIT = .10, .56                                        # the town map begins and ends here, the park map goes on from there
WW, WH = 2048, 1536                                          # the size all of this is worked out at
im = ImageOps.exif_transpose(Image.open(SRC)).convert('RGB').resize((WW, WH), Image.LANCZOS)
rgb = np.asarray(im).astype(np.float32) / 255
R, G, B = rgb[..., 0], rgb[..., 1], rgb[..., 2]
mx, mn = rgb.max(-1), rgb.min(-1)
V = mx; S = np.where(mx > 0, (mx - mn) / np.maximum(mx, 1e-6), 0)
d = np.maximum(mx - mn, 1e-6)
Hh = np.where(mx == R, ((G - B) / d) % 6, np.where(mx == G, (B - R) / d + 2, (R - G) / d + 4)) * 60
L = .3 * R + .55 * G + .15 * B
yy, xx = np.mgrid[0:WH, 0:WW]; XF = (xx + .5) / WW; YF = (yy + .5) / WH
blur = lambda a, s: ndi.gaussian_filter(a.astype(np.float32), s)

def poly(pts):
    m = Image.new('L', (WW, WH), 0); ImageDraw.Draw(m).polygon([(x * WW, y * WH) for x, y in pts], fill=1); return np.asarray(m).astype(bool)
def ell(cx, cy, rx, ry): return ((XF - cx) / rx) ** 2 + ((YF - cy) / ry) ** 2 < 1
def along(pts, x): return np.interp(x, [p[0] for p in pts], [p[1] for p in pts])

# ---- the sky ----
# Above the horizon stand the two towers, a house on the right and what sticks out of the town far off.  Each row of
# the sky has its own colour; whatever is unlike it, where one of those may stand, is not sky.  The cranes are sky.
HORIZON = .3845
towers = poly([(.249, .268), (.331, .2575), (.4095, .2755), (.4095, .5), (.25, .5)]) | poly([(.669, .139), (.698, .117), (.782, .1125), (.7765, .33), (.7765, .5), (.658, .5), (.662, .33)])
near = ndi.binary_dilation(towers, iterations=24) | ((XF > .925) & (XF < .988) & (YF > .325))
far = (YF > .345) & ~near
crane = ((XF < .14) & (YF < .3765)) | ((XF > .165) & (XF < .246) & (YF < .3825))
skyrow = np.stack([np.ma.median(np.ma.array(rgb[..., k], mask=near), axis=1).filled(0) for k in range(3)], -1)   # the colour of the sky, row by row
unlike = np.abs(rgb - skyrow[:, None, :]).sum(-1)
solid = ((near & (unlike > .22)) | (far & (unlike > .16))) & ~crane
solid = ndi.binary_closing(solid, structure=np.ones((7, 7)))
solid = ndi.binary_opening(solid, structure=np.ones((1, 3)))            # antennas and other thin things
solid |= ndi.binary_erosion(towers, iterations=6)
solid |= YF > HORIZON
lab, n = ndi.label(solid)
solid = ndi.binary_fill_holes(lab == lab[WH - 1, 0])                     # only what stands on the ground
top = np.where(solid.any(0), solid.argmax(0), WH)
sky = yy < top[None, :]

# ---- what things are ----
NONE, FOL, GRASS, HARD, EARTH, WALL, CREAM, WARM, TEAL, YELLOW, BLOSSOM = range(11)
GTOP = [(0, .53), (.04, .527), (.10, .519), (.122, .517), (.21, .523), (.235, .52), (.41, .52), (.5, .525), (.64, .535), (.66, .555), (.78, .555), (.8, .52), (.9, .505), (1, .5)]
ground = YF > along(GTOP, XF)
# where the meadow is open.  Everything else that is green is a tree or a bush.
MEADOW = [
    [(.04, .556), (.2, .536), (.206, .556), (.2, .586), (.14, .6), (.08, .61), (.06, .6), (.05, .575)],
    [(.065, .606), (.2, .59), (.213, .6), (.19, .625), (.165, .64), (.16, .665), (.14, .69), (.09, .715), (.06, .7), (.06, .65)],
    [(.34, .525), (.5, .515), (.5, .53), (.47, .55), (.475, .575), (.5, .59), (.47, .6), (.43, .59), (.41, .6), (.4, .63), (.37, .61), (.345, .6), (.335, .57)],
    [(.44, .58), (.5, .6), (.53, .6), (.5, .64), (.5, .69), (.46, .69), (.44, .68), (.42, .64), (.45, .62)],
    [(.335, .70), (.5, .705), (.525, .72), (.53, .78), (.51, .80), (.52, .86), (.5, 1), (.335, 1), (.33, .9), (.345, .86), (.33, .84), (.33, .76)],
    [(.5, .86), (.53, .84), (.56, .9), (.6, .93), (.66, .95), (.7, .9), (.72, .78), (.7, .72), (.725, .72), (.78, .86), (.85, 1), (.5, 1)],
    [(.585, .56), (.655, .555), (.668, .6), (.64, .615), (.6, .6)],
    [(.7, .69), (.74, .685), (.8, .7), (.8, .735), (.765, .75), (.73, .735)],
    [(.685, .555), (.74, .56), (.75, .6), (.72, .6), (.7, .585)],
    [(0, .965), (.12, .93), (.17, .915), (.19, .93), (.19, 1), (0, 1)],
]
BUSHES = [(.405, .785, .035, .022), (.465, .775, .02, .02), (.41, .945, .035, .03), (.64, .88, .035, .06), (.585, .92, .02, .035), (.535, .875, .03, .03),
          (.675, .8, .014, .03), (.72, .775, .014, .03), (.755, .805, .014, .03), (.67, .87, .02, .04), (.355, .6, .012, .03), (.43, .615, .022, .028), (.39, .65, .012, .03),
          (.148, .575, .012, .02), (.123, .628, .012, .025), (.095, .59, .012, .015)]
meadow = np.zeros((WH, WW), bool)
for p in MEADOW: meadow |= poly(p)
for b in BUSHES: meadow &= ~ell(*b)
edge = blur(np.random.RandomState(2).rand(WH, WW), 6); edge = (edge - edge.mean()) / edge.std()
meadow = blur(meadow, 5) + edge * .12 > .5                               # a ragged edge

veg = (Hh > 48) & (Hh < 170) & (S > .2) & (G > B)
veg &= ground | ((blur(veg, 6) > .45) & ~(towers & (YF < np.where(XF < .5, .47, .53))))   # up on a house only where there is a lot of it, and none on the towers
teal = (Hh > 150) & (Hh < 205) & (S > .3) & (V > .2) & (B > R + .04)
veg &= ~teal
vegN = blur(veg, 8)
cls = np.where(ground, HARD, WALL).astype(np.uint8)
cls[~ground & (Hh > 20) & (Hh < 62) & (S > .1) & (V > .5)] = CREAM
# the pink house on the left and the red one in the middle; elsewhere what is reddish is a wall in the shade
rosy = ((XF < .115) & (YF > .37)) | ((XF > .4) & (XF < .5) & (YF > .455) & (YF < .535))
warm = (((Hh < 32) | (Hh > 335)) & (S > .18) | ((Hh < 30) | (Hh > 340)) & (S > .13) & (V > .6)) & (V > .35) & ~ground & rosy
cls[warm] = WARM
cls[(Hh >= 18) & (Hh <= 50) & (S > .16) & (S < .6) & (V > .42) & ground] = EARTH
cls[(cls == CREAM) & (towers & ((XF < .5) | (YF > .325)))] = WALL                # the two towers are white, whatever the evening does to them
cls[(Hh > 33) & (Hh < 60) & (S > .36) & (V > .55) & ~ground] = YELLOW
cls[teal] = TEAL
cls[veg] = FOL
cls[veg & meadow & ground] = GRASS
# Boardwalks and paths that run through the shade of the trees: there, what is grey or dark is the path
PATHS = [(.016, [(0, .966), (.04, .953), (.1, .932), (.14, .92), (.18, .906), (.24, .888), (.3, .868), (.34, .853), (.4, .832), (.45, .815), (.5, .796), (.525, .786)]),
         (.010, [(.36, .682), (.42, .686), (.47, .692), (.515, .698)]), (.007, [(.385, .563), (.45, .575), (.5, .586), (.548, .601)]),
         (.008, [(.035, .6), (.1, .593), (.2, .582)]), (.008, [(.065, .651), (.135, .662), (.2, .667)]),
         (.011, [(.14, .656), (.115, .685), (.095, .705), (.07, .73), (.04, .76), (0, .79)]), (.006, [(.335, .553), (.37, .556), (.39, .561)])]
m = Image.new('L', (WW, WH), 0); dr = ImageDraw.Draw(m)
for wd, pts in PATHS: dr.line([(x * WW, y * WH) for x, y in pts], fill=1, width=max(2, int(wd * WH)))
onpath = np.asarray(m).astype(bool)
cls[onpath & ((S < .28) | (V < .3)) & ground & (cls != EARTH)] = HARD
hard = (cls == HARD) | (cls == EARTH)
speck = ~ndi.binary_opening(hard, structure=np.ones((4, 4)))               # a path is broader than a gap between two leaves
cls[(cls != FOL) & (cls != GRASS) & (cls != TEAL) & (V < .4) & (vegN > .3) & (speck | (V < .2)) & ~onpath] = FOL          # dark among the leaves is their shade
cls[(cls == FOL) & meadow & ground & (vegN > .4)] = GRASS
shady = poly([(.745, .5), (1, .48), (1, .75), (.84, .73), (.8, .7), (.76, .62)])   # under the big trees on the right the ground is dark with their shade, not paved
cls[shady & ground & (V < .32) & (cls != TEAL)] = FOL
# the elder is in flower: white specks among the leaves, but not the paths that run through them
white = (S < .32) & (V > .7) & ground & (vegN > .45)
white &= ~ndi.binary_opening(white, structure=np.ones((5, 5))) & ~meadow
cls[white] = BLOSSOM

# ---- what is not to be painted ----
Lo = L.copy()
noise = blur(np.random.RandomState(4).rand(WH, WW), 10); noise = (noise - noise.min()) / (noise.max() - noise.min())
# the scaffolding: behind it is the house behind it, copied from beside it; the tower itself is painted by hand
scaf = poly([(.116, .463), (.16, .4405), (.2105, .4505), (.2105, .536), (.12, .536)])
ys, xs = np.nonzero(scaf)
sx = ((.2125 + ((xs / WW - .116) % .03)) * WW).astype(int)
cls[ys, xs] = cls[ys, sx]; Lo[ys, xs] = Lo[ys, sx]
# the building site at its foot becomes lawn
site = poly([(.034, .529), (.10, .5185), (.123, .5165), (.123, .535), (.205, .535), (.209, .521), (.2115, .535), (.172, .5415), (.171, .547), (.12, .5495), (.085, .5525), (.062, .5535), (.04, .5565), (.032, .547)]) | (scaf & (YF > along([(.116, .526), (.136, .531), (.2105, .524)], XF)))
cls[site] = GRASS; Lo[site] = .40 + noise[site] * .2 - (YF[site] - .52) * .4
cls[sky] = NONE

# ---- how light: every kind of thing has its own scale, from its darkest to its lightest ----
Lo = np.where(YF < SPLIT, np.clip(Lo + (Lo - blur(Lo, 3.5)) * .9, 0, 1), Lo)       # the town a little crisper than the camera had it
Lo = np.where(YF >= SPLIT, np.clip(Lo + (Lo - blur(Lo, 7)) * .7, 0, 1), Lo)        # and the crowns of the trees rounder
lev = np.zeros((WH, WW), np.float32)
SPAN = {}
for c in range(1, 11):
    m = cls == c
    if not m.any(): continue
    lo, hi = np.percentile(Lo[m], [1.5, 98.5])
    if c in (WALL, CREAM): lo, hi = .1, .96
    SPAN[c] = (lo, hi)
    lev[m] = np.clip((Lo[m] - lo) / max(hi - lo, 1e-3), 0, 1)
lev[cls == HARD] **= .8                                                    # the paths lie in the shade: lift them a little
# what is red and what only pink: the upper half of the scale is for the red
red = (cls == WARM) & (S > .42)
lev[cls == WARM] *= .499; lev[red] += .5

def reduce(y0, y1, w, sharp=False):
    """the map of a band of the photo, w cells wide: the kind most of a cell is, and how light that is"""
    h = int(round((y1 - y0) * w * .75))
    r0, r1 = int(round(y0 * WH)), int(round(y1 * WH))
    ys = np.minimum(((np.arange(r0, r1) - r0) * h / (r1 - r0)).astype(int), h - 1); xs = np.minimum((np.arange(WW) * w / WW).astype(int), w - 1)
    idx = ys[:, None] * w + xs[None, :]
    c = cls[r0:r1]; l = lev[r0:r1]
    cnt = np.zeros((11, h * w)); sm = np.zeros((11, h * w))
    for k in range(11):
        m = c == k
        cnt[k] = np.bincount(idx[m], minlength=h * w); sm[k] = np.bincount(idx[m], weights=l[m], minlength=h * w)
    green = (cnt[FOL] + cnt[GRASS]) > .25 * cnt.sum(0)                   # and a narrow path through the green is still a path
    wt = np.ones((11, h * w))
    if not sharp: wt[HARD] = np.where(green, 2.4, 1); wt[EARTH] = np.where(green, 1.7, 1)
    wt[BLOSSOM] = 2.2; cnt = cnt * wt
    best = cnt.argmax(0)
    n = np.maximum(cnt[best, np.arange(h * w)] / wt[best, np.arange(h * w)], 1)
    level = sm[best, np.arange(h * w)] / n
    if sharp:                                                             # a window is not to drown in the wall round it: where a good part of a cell is dark, the cell is
        own = c == best[idx]; dark = own & (l < level[idx] - .12)
        nd = np.bincount(idx[dark], minlength=h * w); sd = np.bincount(idx[dark], weights=l[dark], minlength=h * w)
        level = np.where(nd >= .42 * n, sd / np.maximum(nd, 1), (sm[best, np.arange(h * w)] - sd) / np.maximum(n - nd, 1))
    return best.reshape(h, w).astype(np.uint8), np.clip((level * 8).astype(int), 0, 7).reshape(h, w).astype(np.uint8)

A = ''.join(chr(c) for c in range(35, 127) if c != 92)                    # 91 characters that need no escaping
tc, tl = reduce(TOP, SPLIT, TW, True)
pc, pl = reduce(SPLIT, 1, PKW)
tops = []; cols = []
for x in range(TW):
    col = tc[:, x]; nz = np.nonzero(col)[0]; t = nz[0] if len(nz) else len(col)
    c = col[t:].copy(); l = tl[t:, x]
    for i in range(len(c)):                                               # no holes under the roofs
        if c[i] == NONE: c[i] = c[i - 1] if i else WALL
    tops.append(t); cols.append(''.join(A[a * 8 + b] for a, b in zip(c, l)))
pc[pc == NONE] = HARD
town = ''.join(cols); park = ''.join(A[a * 8 + b] for a, b in zip(pc.ravel(), pl.ravel()))
with open(OUT, 'w') as f:
    f.write('/* Generated by tools/freiemitte-map.py from the photograph the Freie Mitte is painted after.  Not to be edited:\n'
            ' * what there is at every point under the sky and how light it is there, one character each (see places/freiemitte.js). */\n')
    f.write('(window.viennaMaps = window.viennaMaps || {}).freiemitte = {\n')
    f.write(f'  top: {TOP}, split: {SPLIT},\n')
    f.write(f'  town: {{ w: {TW}, h: {tc.shape[0]}, tops: "{"".join(A[t // 91] + A[t % 91] for t in tops)}",\n    cells: "{town}" }},\n')
    f.write(f'  park: {{ w: {PKW}, h: {pc.shape[0]},\n    cells: "{park}" }},\n}};\n')
print('town', tc.shape, len(town), 'park', pc.shape, len(park), {k: (round(float(a), 2), round(float(b), 2)) for k, (a, b) in SPAN.items()})

if len(sys.argv) > 5:                                                     # pictures to check it by
    COL = np.array([[120, 170, 230], [30, 90, 40], [150, 200, 70], [150, 150, 150], [200, 170, 110], [240, 240, 240], [230, 215, 180], [220, 110, 90], [40, 160, 150], [240, 210, 60], [255, 0, 255]], np.float32)
    Image.fromarray(COL[cls].astype(np.uint8)).save(sys.argv[5] + '-cls.png')
    full = np.zeros((int(round(TW * .75)), TW, 3), np.float32) + COL[0]
    r0 = int(round(TOP * TW * .75)); full[r0:r0 + tc.shape[0]] = COL[tc] * (.35 + .65 * tl[..., None] / 7)
    pk = COL[pc] * (.35 + .65 * pl[..., None] / 7)
    Image.fromarray(full.astype(np.uint8)).resize((1600, 1200), Image.NEAREST).save(sys.argv[5] + '-town.png')
    Image.fromarray(pk.astype(np.uint8)).resize((1600, int(1600 * pk.shape[0] / PKW)), Image.NEAREST).save(sys.argv[5] + '-park.png')
