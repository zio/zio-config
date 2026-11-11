#!/usr/bin/env python3
"""Write round-1 concept SVGs and a size-test page; chromium screenshots it."""
import os, sys
sys.path.insert(0, os.path.dirname(__file__))
from marks import PAL, CONCEPTS
out = sys.argv[1] if len(sys.argv) > 1 else 'round-1'
os.makedirs(out, exist_ok=True)
for mode in PAL:
    p = PAL[mode]
    for k, name, fn in CONCEPTS:
        slug = name.lower().replace(' = ', '-').replace(' ', '-')
        open(f'{out}/{k}-{slug}-{mode}.svg', 'w').write(fn(p['brand'], p['accent']))
        open(f'{out}/{k}-{slug}-small-{mode}.svg', 'w').write(fn(p['brand'], p['accent'], small=True))
rows = ''
for mode in PAL:
    p = PAL[mode]
    rows += f'<section style="background:{p["page"]};color:{p["text"]}"><h2>{mode}</h2>'
    for k, name, fn in CONCEPTS:
        rows += f'<div class="r"><b>{k} {name}</b>'
        for s in (16, 24, 32, 96):
            svg = fn(p['brand'], p['accent'], small=(s < 24)).replace('<svg ', f'<svg width="{s}" height="{s}" ', 1)
            # unique mask ids per instance
            svg = svg.replace('id="m1"', f'id="m1{mode}{s}"').replace('id="m2"', f'id="m2{mode}{s}"').replace('url(#m1)', f'url(#m1{mode}{s})').replace('url(#m2)', f'url(#m2{mode}{s})')
            rows += f'<span>{svg}<i>{s}</i></span>'
        rows += '</div>'
    rows += '</section>'
html = f'<!doctype html><meta charset=utf-8><style>body{{margin:0;font:11px monospace;display:flex}}section{{padding:16px;width:380px}}.r{{display:flex;gap:18px;align-items:flex-end;margin:14px 0;flex-wrap:wrap}}.r b{{width:100%}}span{{display:flex;flex-direction:column;align-items:center;gap:4px}}i{{font-style:normal;opacity:.6}}</style>{rows}'
open(f'{out}/sizes.html', 'w').write(html)
