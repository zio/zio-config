"""TEMP design: the Z of ZIO drawn as the stair. usage: build_embed.py <Sora ttf> <outdir>"""
import sys, os
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
SORA, OUT = sys.argv[1], sys.argv[2]; os.makedirs(OUT, exist_ok=True)
def inst(w): return instantiateVariableFont(TTFont(SORA), {"wght": w})
bold, reg = inst(700), inst(400)
capH = bold["OS/2"].sCapHeight / bold["head"].unitsPerEm
def tp(font, s, size, x, y, tr=0):
    gs = font.getGlyphSet(); cm = font.getBestCmap(); u = font["head"].unitsPerEm; sc = size / u
    pen = SVGPathPen(gs); cx = x
    for ch in s:
        g = cm[ord(ch)]; gs[g].draw(TransformPen(pen, (sc, 0, 0, -sc, cx, y))); cx += gs[g].width * sc + tr
    return pen.getCommands(), cx
def z_stair(x, top, cap, cols, xs, uid, mono_knob=True):
    """stair sized to cap height. mark bbox is 58 x 62 units; xs squeezes the bar lengths."""
    s = cap / 62.0; ink, acc = cols
    rows = [(3, 61, "R", ink), (22, 61, "R", acc), (3, 42, "L", acc), (3, 61, "L", ink)]
    bars = holes = ""
    for i, (a, b, side, c) in enumerate(rows):
        a2, b2 = 3 + (a - 3) * xs, 3 + (b - 3) * xs
        y = 1 + i * 16
        X = lambda v: x + (v - 3) * s; Y = lambda v: top + (v - 1) * s
        bars += f'<rect x="{X(a2):.2f}" y="{Y(y):.2f}" width="{(b2-a2)*s:.2f}" height="{14*s:.2f}" rx="{7*s:.2f}" fill="{c}"/>'
        kx = b2 - 7 if side == "R" else a2 + 7
        holes += f'<circle cx="{X(kx):.2f}" cy="{Y(y+7):.2f}" r="{5.2*s:.2f}" fill="#000"/>'
    w = (61 - 3) * xs * s
    return (f'<mask id="{uid}" maskUnits="userSpaceOnUse" x="-50" y="-50" width="2000" height="400"><rect x="-50" y="-50" width="2000" height="400" fill="#fff"/>{holes}</mask>'
            f'<g mask="url(#{uid})">{bars}</g>'), w
def lockup(cols, xs, uid):
    size = 30 / capH; base = 47; top = base - 30
    zb, zw = z_stair(0, top, 30, cols, xs, uid)
    gap = size * 0.09
    d1, x1 = tp(bold, "IO", size, zw + gap, base, size * 0.01)
    d2, x2 = tp(reg, "Config", size, x1 + size * 0.26, base, size * 0.005)
    ink = cols[0]
    body = zb + f'<path d="{d1}" fill="{ink}"/><path d="{d2}" fill="{ink}"/>'
    return f'<svg xmlns="http://www.w3.org/2000/svg" width="{round(x2)}" height="64" viewBox="0 0 {x2:.1f} 64">{body}</svg>\n'
L = ("#101521", "#F2501F"); D = ("#F4F5F8", "#FF6B3D"); LM = ("#101521", "#101521"); DM = ("#F4F5F8", "#F4F5F8")
V = {"v1-square": 1.0, "v2-narrow": 0.78}
for n, xs in V.items():
    open(f"{OUT}/lockup-{n}-light.svg", "w").write(lockup(L, xs, "a"))
    open(f"{OUT}/lockup-{n}-dark.svg", "w").write(lockup(D, xs, "a"))
open(f"{OUT}/lockup-v3-mono-light.svg", "w").write(lockup(LM, 0.78, "a"))
open(f"{OUT}/lockup-v3-mono-dark.svg", "w").write(lockup(DM, 0.78, "a"))
print("ok")
