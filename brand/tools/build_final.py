"""Build final ZIO Config mark, lockups, favicon SVGs from the chosen Stair (round 6, version 1).
Run with a python that has fontTools: python build_final.py <Sora variable ttf> <outdir>"""
import sys, os
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen

SORA, OUT = sys.argv[1], sys.argv[2]
os.makedirs(OUT, exist_ok=True)
INK, ACC = "#101521", "#F2501F"          # light
INK_D, ACC_D = "#F4F5F8", "#FF6B3D"      # dark
NIGHT = "#101521"

# --- mark: 64-unit box, four bars + knobs (knobs are real holes via mask) ---
def mark(small=False, uid="m"):
    h = 14.5 if small else 14
    gap = (62 - 4 * h) / 3
    rows = [(3, 61, "R", "ink"), (22, 61, "R", "acc"), (3, 42, "L", "acc"), (3, 61, "L", "ink")]
    bars, holes = [], []
    for i, (a, b, side, c) in enumerate(rows):
        y = 1 + i * (h + gap)
        bars.append((a, y, b - a, h, c))
        kx = b - h / 2 if side == "R" else a + h / 2
        holes.append((kx, y + h / 2, h / 2 - 1.8))
    return bars, holes

def mark_svg_body(cols, small=False, uid="m", tx="", ):
    ink, acc = cols
    bars, holes = mark(small)
    m = "".join(f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{r:.2f}" fill="#000"/>' for x, y, r in holes)
    b = "".join(f'<rect x="{x}" y="{y:.2f}" width="{w}" height="{h}" rx="{h/2}" fill="{ink if c=="ink" else acc}"/>' for x, y, w, h, c in bars)
    return f'<mask id="{uid}"><rect width="64" height="64" fill="#fff"/>{m}</mask><g mask="url(#{uid})"{tx}>{b}</g>'

def svg(w, h, vb, body, bg=None):
    r = f'<rect width="100%" height="100%" fill="{bg}"/>' if bg else ""
    return f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" viewBox="{vb}">{r}{body}</svg>\n'

# --- text outlines ---
def instance(w):
    f = TTFont(SORA); return instantiateVariableFont(f, {"wght": w})
def text_path(font, s, size, x, y, tracking=0):
    gs = font.getGlyphSet(); cmap = font.getBestCmap(); upm = font["head"].unitsPerEm
    sc = size / upm; pen = SVGPathPen(gs); cx = x
    for ch in s:
        g = cmap[ord(ch)]
        gs[g].draw(TransformPen(pen, (sc, 0, 0, -sc, cx, y)))
        cx += gs[g].width * sc + tracking
    return pen.getCommands(), cx
bold, reg = instance(700), instance(400)
capH = bold["OS/2"].sCapHeight / bold["head"].unitsPerEm

def wordmark(ink, size, x, baseline):
    d1, x1 = text_path(bold, "ZIO", size, x, baseline, tracking=size * 0.01)
    d2, x2 = text_path(reg, "Config", size, x1 + size * 0.26, baseline, tracking=size * 0.005)
    return f'<path d="{d1}" fill="{ink}"/><path d="{d2}" fill="{ink}"/>', x2

# --- logo: the Z of ZIO is the stair (chosen design, round 10 version 1) ---
# Z letter = the symbol at cap height ("1 Square Z", round 9): the same four bars and knobs, full width,
# bar thickness equals the Sora Bold I stem (6.77 vs 6.74 units at cap 30).
_zid = [0]
def z_letter(x, top, cap, cols):
    s_ = cap / 62.0; ink, acc = cols; _zid[0] += 1; uid = f"z{_zid[0]}"
    bars, holes = mark(False)
    body = ""
    for (bx, by, bw, bh, c) in bars:
        body += f'<rect x="{x + (bx - 3) * s_:.2f}" y="{top + (by - 1) * s_:.2f}" width="{bw * s_:.2f}" height="{bh * s_:.2f}" rx="{bh * s_ / 2:.2f}" fill="{ink if c == "ink" else acc}"/>'
    hs = "".join(f'<circle cx="{x + (hx - 3) * s_:.2f}" cy="{top + (hy - 1) * s_:.2f}" r="{hr * s_:.2f}" fill="#000"/>' for hx, hy, hr in holes)
    return (f'<mask id="{uid}"><rect x="-5000" y="-5000" width="10000" height="10000" fill="#fff"/>{hs}</mask>'
            f'<g mask="url(#{uid})">{body}</g>'), 58 * s_
def logo_body(cols, cap, x, baseline):
    """returns svg body and right edge. Z + IO (bold) + Config (regular), outlined."""
    size = cap / capH
    zb, zw = z_letter(x, baseline - cap, cap, cols)
    d1, x1 = text_path(bold, "IO", size, x + zw + size * 0.10, baseline, size * 0.01)
    d2, x2 = text_path(reg, "Config", size, x1 + size * 0.26, baseline, size * 0.005)
    return zb + f'<path d="{d1}" fill="{cols[0]}"/><path d="{d2}" fill="{cols[0]}"/>', x2

files = {}
for name, cols, bg in [("light", (INK, ACC), None), ("dark", (INK_D, ACC_D), None)]:
    files[f"mark-{name}.svg"] = svg(256, 256, "0 0 64 64", mark_svg_body(cols, uid="a"))
    files[f"mark-small-{name}.svg"] = svg(64, 64, "0 0 64 64", mark_svg_body(cols, True, uid="a"))
    # logo: cap height 30, tight viewBox (cap top 17 to descender 58)
    lb, lw = logo_body(cols, 30, 0, 47)
    files[f"logo-{name}.svg"] = svg(round(lw * 2), 82, f"0 16 {lw:.1f} 42", lb)
# favicon: dark rounded tile, light ink + dark accent (works on any browser chrome)
tile = lambda rx: f'<rect width="64" height="64" rx="{rx}" fill="{NIGHT}"/>'
sc = 'transform="translate(6.4 6.4) scale(0.8)"'
files["favicon-tile.svg"] = svg(64, 64, "0 0 64 64", tile(14) + mark_svg_body((INK_D, ACC_D), True, uid="f", tx=f" {sc}"))
files["favicon-square.svg"] = svg(64, 64, "0 0 64 64", tile(0) + mark_svg_body((INK_D, ACC_D), True, uid="f", tx=f" {sc}"))  # apple-touch / maskable
# tiny favicon (16/32 px): tile with small radius, mark fills more of it
sc2 = 'transform="translate(2.2 2.2) scale(0.931)"'
files["favicon-tiny.svg"] = svg(64, 64, "0 0 64 64", tile(10) + mark_svg_body((INK_D, ACC_D), True, uid="f", tx=f" {sc2}"))
# tab favicon: no tile, 3 pill bars fill the 16 px box, 2x2 knob holes, colours switch with the tab theme
_tb = [(0,16,0,"i",13,1),(2,14,6,"a",4,7),(0,16,12,"i",1,13)]
tbars = "".join(f'<rect x="{a}" y="{y}" width="{b-a}" height="4" rx="2" class="{c}"/>' for a,b,y,c,kx,ky in _tb)
tholes = "".join(f'<rect x="{kx}" y="{ky}" width="2" height="2" rx="1" fill="#000"/>' for a,b,y,c,kx,ky in _tb)
tab_css = f'<style>.i{{fill:{INK}}}.a{{fill:{ACC}}}@media (prefers-color-scheme:dark){{.i{{fill:{INK_D}}}.a{{fill:{ACC_D}}}}}</style>'
files["favicon.svg"] = svg(16, 16, "0 0 16 16", f'{tab_css}<mask id="t"><rect width="16" height="16" fill="#fff"/>{tholes}</mask><g mask="url(#t)">{tbars}</g>')
# 16 px pixel-grid favicon: bars 3 px, gap 1 px, 1 px knob dot at one end of each bar (tile colour)
px = [(1,15,"R",INK_D),(5,15,"R",ACC_D),(1,11,"L",ACC_D),(1,15,"L",INK_D)]
bars16 = "".join(f'<rect x="{a}" y="{4*i}" width="{b-a}" height="3" rx="1" fill="{c}"/><rect x="{(b-2) if side=="R" else (a+1)}" y="{4*i+1}" width="1" height="1" fill="{NIGHT}"/>' for i,(a,b,side,c) in enumerate(px))
files["favicon-16px.svg"] = svg(16, 16, "0 0 16 16", f'<rect width="16" height="16" rx="3" fill="{NIGHT}"/>' + bars16)
# maskable icon: mark inside the central 80% safe circle (radius 0.4 of the side)
files["icon-maskable.svg"] = svg(64, 64, "0 0 64 64", tile(0) + mark_svg_body((INK_D, ACC_D), True, uid="f", tx=' transform="translate(13.4 13.4) scale(0.58)"'))
# social cards
semi = instance(600)
CARD_L1 = "Type-safe configuration for ZIO applications"
CARD_L2 = "Automatic derivation, validation and generated docs"
CARD_CAP = "Reads configuration from many sources"
CARD_LIST = "HOCON  \u00b7  JSON  \u00b7  YAML  \u00b7  TOML  \u00b7  XML  \u00b7  Maps"
CARD_URL = "zio.dev/zio-config"
def card(W, H, cols, bg, sub, name):
    """Social card modelled on the zio-blocks one: logo, bright tagline, supporting line, source list, address at the right."""
    ink = cols[0]; acc = cols[1]; dim = sub
    B = round(H * 0.455)                                  # logo baseline
    lb, lw = logo_body(cols, 90, 96, B)
    l1, _ = text_path(semi, CARD_L1, 38, 96, B + 68)
    l2, _ = text_path(reg, CARD_L2, 29, 96, B + 114)
    cp, _ = text_path(reg, CARD_CAP, 25, 96, H - 150)
    ls, _ = text_path(reg, CARD_LIST, 25, 96, H - 84)
    _, uw = text_path(reg, CARD_URL, 25, 0, 0)
    ur, _ = text_path(reg, CARD_URL, 25, W - 96 - uw, H - 84)
    ghost = f'<g opacity="{0.14 if bg == NIGHT else 0.09}">' + mark_svg_body(cols, uid="g", tx=f' transform="translate({W - 340} {(H - 460) / 2:.1f}) scale({460 / 64:.4f})"') + '</g>'
    body = (ghost + lb + f'<path d="{l1}" fill="{ink}"/><path d="{l2}" fill="{dim}"/><path d="{cp}" fill="{dim}"/>'
            f'<rect x="96" y="{H - 128}" width="64" height="3" rx="1.5" fill="{acc}"/>'
            f'<path d="{ls}" fill="{dim}"/><path d="{ur}" fill="{dim}"/>')
    files[name] = svg(W, H, f"0 0 {W} {H}", body, bg)
card(1200, 630, (INK, ACC), "#FFFFFF", "#566074", "social-card-light.svg")
card(1200, 630, (INK_D, ACC_D), NIGHT, "#A7AEBD", "social-card-dark.svg")
card(1280, 640, (INK_D, ACC_D), NIGHT, "#A7AEBD", "github-social-preview.svg")
for n, s in files.items():
    open(os.path.join(OUT, n), "w").write(s)
print(len(files), "files", round(capH, 3))
