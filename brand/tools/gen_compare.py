"""Comparison board: 4-bar symbol vs two 3-bar options (symbol, small sizes, lockup).
Run: python gen_compare.py <Sora ttf> <outdir>   -> Round13.dc.html + prev-compare.html"""
import sys, os
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen

SORA, OUT = sys.argv[1], sys.argv[2]
os.makedirs(OUT, exist_ok=True)
INK, ACC, INK_D, ACC_D, NIGHT, SLATE, SNOW, MIST = "#101521", "#F2501F", "#F4F5F8", "#FF6B3D", "#101521", "#566074", "#F4F5F8", "#A7AEBD"

def inst(w): return instantiateVariableFont(TTFont(SORA), {"wght": w})
bold, reg = inst(700), inst(400)
capH = bold["OS/2"].sCapHeight / bold["head"].unitsPerEm

def text_path(font, s, size, x, y, tr=0):
    gs = font.getGlyphSet(); cm = font.getBestCmap(); upm = font["head"].unitsPerEm
    sc = size / upm; pen = SVGPathPen(gs); cx = x
    for ch in s:
        g = cm[ord(ch)]; gs[g].draw(TransformPen(pen, (sc, 0, 0, -sc, cx, y))); cx += gs[g].width * sc + tr
    return pen.getCommands(), cx

# variants: (name, bar height, rows [(x0,x1,side,colour)])
VARS = [
    ("A", "Four bars (current)", 14, [(3, 61, "R", "i"), (22, 61, "R", "a"), (3, 42, "L", "a"), (3, 61, "L", "i")],
     "Right-then-left stagger draws a zigzag. Most detail, thinnest bars."),
    ("B", "Three bars, centred middle", 18, [(3, 61, "R", "i"), (13, 51, "C", "a"), (3, 61, "L", "i")],
     "Literal Z: top, diagonal, bottom. The centred middle risks reading as a menu icon."),
    ("C", "Three bars, middle shifted left", 18, [(3, 61, "R", "i"), (3, 43, "L", "a"), (3, 61, "L", "i")],
     "Middle bar steps off the right edge, so the stair direction stays visible."),
]
# C alt: middle shifted so it hangs from the right (diagonal runs top-right to bottom-left) -> use right-aligned
VARS[2] = ("C", "Three bars, middle shifted right", 18, [(3, 61, "R", "i"), (23, 61, "C", "a"), (3, 61, "L", "i")],
           "Middle bar hangs from the right edge, a clear step. Closest to the current stair.")

_u = [0]
def mark(rows, h, size, ink, acc):
    _u[0] += 1; uid = f"m{_u[0]}"
    n = len(rows); gap = (62 - n * h) / (n - 1)
    bars = holes = ""
    for i, (a, b, side, c) in enumerate(rows):
        y = 1 + i * (h + gap); col = ink if c == "i" else acc
        bars += f'<rect x="{a}" y="{y:.2f}" width="{b-a}" height="{h}" rx="{h/2}" fill="{col}"/>'
        kx = (a + b) / 2 if side == "C" else (b - h / 2 if side == "R" else a + h / 2)
        holes += f'<circle cx="{kx:.2f}" cy="{y+h/2:.2f}" r="{h/2-0.129*h:.2f}" fill="#000"/>'
    return (f'<svg width="{size}" height="{size}" viewBox="0 0 64 64" role="img" aria-label="symbol"><mask id="{uid}"><rect width="64" height="64" fill="#fff"/>{holes}</mask>'
            f'<g mask="url(#{uid})">{bars}</g></svg>')

def logo(rows, h, cap, ink, acc, height):
    """lockup: the symbol as the Z at cap height, then IO + Config, outlined. returns svg scaled to `height` px tall."""
    _u[0] += 1; uid = f"l{_u[0]}"
    n = len(rows); gap = (62 - n * h) / (n - 1); s = cap / 62.0; base = 47; top = base - cap
    bars = holes = ""
    for i, (a, b, side, c) in enumerate(rows):
        y = 1 + i * (h + gap); col = ink if c == "i" else acc
        bars += f'<rect x="{(a-3)*s:.2f}" y="{top+(y-1)*s:.2f}" width="{(b-a)*s:.2f}" height="{h*s:.2f}" rx="{h*s/2:.2f}" fill="{col}"/>'
        kx = (a + b) / 2 if side == "C" else (b - h / 2 if side == "R" else a + h / 2)
        holes += f'<circle cx="{(kx-3)*s:.2f}" cy="{top+(y+h/2-1)*s:.2f}" r="{(h/2-0.129*h)*s:.2f}" fill="#000"/>'
    size = cap / capH; zw = 58 * s
    d1, x1 = text_path(bold, "IO", size, zw + size * 0.10, base, size * 0.01)
    d2, x2 = text_path(reg, "Config", size, x1 + size * 0.26, base, size * 0.005)
    w = (x2) * height / 42
    return (f'<svg width="{w:.0f}" height="{height}" viewBox="0 16 {x2:.1f} 42" role="img" aria-label="ZIO Config">'
            f'<mask id="{uid}"><rect x="-50" y="-50" width="{x2+100:.0f}" height="200" fill="#fff"/>{holes}</mask><g mask="url(#{uid})">{bars}</g>'
            f'<path d="{d1}" fill="{ink}"/><path d="{d2}" fill="{ink}"/></svg>')

MONO = "font-family:'JetBrains Mono',monospace"
SORAF = "font-family:Sora,system-ui,sans-serif"
def panel(content, dark, pad=24, extra=""):
    bg, bd = (NIGHT, NIGHT) if dark else ("#FFFFFF", "#DCDFE6")
    return f'<div style="background:{bg};border:1px solid {bd};border-radius:14px;padding:{pad}px;display:flex;align-items:center;justify-content:center;gap:22px;{extra}">{content}</div>'
def lab(t, dark=False): return f'<div style="{MONO};font-size:11px;color:{MIST if dark else SLATE};margin-top:6px;text-align:center">{t}</div>'

cols = []
for key, name, h, rows, note in VARS:
    def m(size, dark): return mark(rows, h, size, INK_D if dark else INK, ACC_D if dark else ACC)
    def lg(height, dark): return logo(rows, h, 30, INK_D if dark else INK, ACC_D if dark else ACC, height)
    big = panel(m(190, False), False, 28, "height:250px;") + panel(m(190, True), True, 28, "height:250px;")
    sizes = ""
    for dark in (False, True):
        cells = "".join(f'<div style="display:flex;flex-direction:column;align-items:center">{m(s, dark)}{lab(f"{s}px", dark)}</div>' for s in (16, 24, 32, 48, 64))
        sizes += panel(cells, dark, 20, "align-items:flex-end;")
    locks = panel(lg(36, False), False, 24, "height:96px;") + panel(lg(36, True), True, 24, "height:96px;")
    locks_s = panel(lg(20, False), False, 18, "height:60px;") + panel(lg(20, True), True, 18, "height:60px;")
    cols.append(f'''<div style="display:flex;flex-direction:column;gap:14px;min-width:0">
<div><div style="{MONO};font-size:12px;color:{SLATE};letter-spacing:.08em">OPTION {key}</div><div style="{SORAF};font-weight:700;font-size:22px;color:{INK};margin-top:4px">{name}</div>
<div style="font-size:14px;color:{SLATE};line-height:1.5;margin-top:6px;min-height:42px">{note}</div></div>
{big}{sizes}{locks}{locks_s}</div>''')

lead = ("Same symbol idea at three bar counts. Each column shows the symbol large, at 16 to 64 px, and as the Z of the wordmark "
        "(36 px and 20 px tall), on light and dark. Sizes are plain scaling with no pixel hinting, so what you see is the worst case.")
BODY = (f'<div style="{SORAF};background:#F4F5F8;color:{INK};padding:44px 40px 48px;box-sizing:border-box;width:1480px">'
        f'<div style="{MONO};font-size:12px;letter-spacing:.08em;color:{SLATE}">COMPARISON</div>'
        f'<h1 style="font-size:40px;line-height:1.1;margin:8px 0 10px;letter-spacing:-.02em">Four bars or three?</h1>'
        f'<p style="font-size:16px;line-height:1.55;color:{SLATE};max-width:820px;margin:0 0 28px">{lead}</p>'
        f'<div style="display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:28px">{"".join(cols)}</div></div>')

HEAD = ('<link rel="preconnect" href="https://fonts.googleapis.com"><link href="https://fonts.googleapis.com/css2?family=Sora:wght@400;600;700'
        '&amp;family=JetBrains+Mono:wght@400;500&amp;display=swap" rel="stylesheet">')
open(f"{OUT}/prev-compare.html", "w").write(f'<!doctype html><meta charset="utf-8">{HEAD}<body style="margin:0">{BODY}')
H = int(sys.argv[3]) if len(sys.argv) > 3 else 1500
dc = (f'<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n<title>ZIO Config bars comparison</title>\n<script src="./support.js"></script>\n</head>\n<body>\n<x-dc>\n<helmet>\n{HEAD}\n<style>\nbody{{margin:0}}\n</style>\n</helmet>\n{BODY}\n</x-dc>\n'
      f'<script type="text/x-dc" data-dc-script data-props=\'{{"$preview":{{"width":1480,"height":{H}}}}}\'>\nclass Component extends DCLogic {{\nrenderVals() {{\nreturn {{}};\n}}\n}}\n</script>\n</body>\n</html>\n')
open(f"{OUT}/Round13.dc.html", "w").write(dc)
print("ok")
