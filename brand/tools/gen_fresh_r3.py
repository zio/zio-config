syms = {
"N1": '<rect x="4" y="10" width="56" height="44" rx="9" fill="var(--ink)"/><g fill="var(--bg)"><rect x="9" y="16" width="10" height="32" rx="5"/><rect x="27" y="16" width="10" height="32" rx="5"/><rect x="45" y="16" width="10" height="32" rx="5"/></g><g fill="var(--fg)"><circle cx="14" cy="21" r="5"/><circle cx="32" cy="32" r="5"/><circle cx="50" cy="43" r="5"/></g>',
"N1s": '<rect x="2" y="8" width="60" height="48" rx="10" fill="var(--ink)"/><g fill="var(--bg)"><rect x="7" y="14" width="12" height="36" rx="6"/><rect x="26" y="14" width="12" height="36" rx="6"/><rect x="45" y="14" width="12" height="36" rx="6"/></g><g fill="var(--fg)"><circle cx="13" cy="21" r="6"/><circle cx="32" cy="32" r="6"/><circle cx="51" cy="43" r="6"/></g>',
"N2": '<rect x="3" y="4" width="58" height="15" rx="7.5" fill="var(--fg)"/><circle cx="53.5" cy="11.5" r="5.5" fill="var(--bg)"/><rect x="3" y="24.5" width="58" height="15" rx="7.5" fill="var(--ink)"/><circle cx="10.5" cy="32" r="5.5" fill="var(--bg)"/><rect x="3" y="45" width="58" height="15" rx="7.5" fill="var(--fg)"/><circle cx="53.5" cy="52.5" r="5.5" fill="var(--bg)"/>',
"N3": '<g stroke-linecap="round" fill="none" stroke-width="11"><path d="M10 10 H38" stroke="var(--ink)"/><path d="M22 24 H48" stroke="var(--ink)"/><path d="M34 38 H54" stroke="var(--fg)"/><path d="M22 52 H48" stroke="var(--ink)"/></g>',
"N4": '<rect x="3" y="12" width="40" height="16" rx="8" fill="var(--ink)"/><rect x="21" y="36" width="40" height="16" rx="8" fill="var(--fg)"/>',
"N5": '<g fill="var(--ink)"><rect x="3" y="5" width="18" height="18" rx="5.5"/><rect x="3" y="23" width="0" height="0"/><rect x="3" y="26" width="18" height="18" rx="5.5" fill-opacity="0"/></g><rect x="3" y="5" width="18" height="18" rx="5.5" fill="var(--ink)"/><rect x="3" y="23.5" width="18" height="18" rx="5.5" fill="var(--ink)"/><rect x="3" y="42" width="18" height="18" rx="5.5" fill="var(--fg)"/><g fill="none" stroke="var(--bg)" stroke-width="3.2" stroke-linecap="round" stroke-linejoin="round"><path d="M7.5 14.5 L11 18 L16.5 10.5"/><path d="M7.5 33 L11 36.5 L16.5 29"/><path d="M8 47 L16 55 M16 47 L8 55"/></g><g stroke-linecap="round" stroke-width="8" fill="none"><path d="M32 14 H58" stroke="var(--ink)"/><path d="M32 32.5 H52" stroke="var(--ink)"/><path d="M32 51 H56" stroke="var(--fg)"/></g>',
}
cards = [
("N1","N1s","1 DIP switch","a bank of DIP switches, the hardware way of setting a machine up before software had config files. Knobs step down in a diagonal, a Z in motion.","reads as a retro or hardware icon, and three slots can look like a volume mixer."),
("N2","N2","2 Toggles","three on/off settings switches stacked, the shape of every settings screen. Knobs go right, left, right: a Z drawn by the switch positions.","the most UI-icon of the five; the Z is only implied."),
("N3","N3","3 Nested","indented key rows, the way HOCON, YAML and TOML nest settings. The coloured row is the value you asked for, deep in the tree.","reads as a plain list or a chart; no obvious ZIO link."),
("N4","N4","4 Equals","key = value: the sign every setting is written with, offset so it steps like a Z. Top bar is the key, coloured bar is the value.","very simple, so it can look like a menu or a placeholder; the Z is faint."),
("N5","N5","5 Validated","rows of settings with a check box each. Two values accepted, the coloured one rejected: ZIO Config reports errors, it does not guess.","busy at small sizes, and a red cross can read as an error badge for the brand."),
]
tpl_sym = "".join(f'<symbol id="{k}" viewBox="0 0 64 64">{v}</symbol>\n' for k,v in syms.items())
def use(i,s): return f'<svg width="{s}" height="{s}"><use href="#{i}" width="{s}" height="{s}"/></svg>'
B='border: 1px solid #E3E6EC'
out=[]
for sym,sm,title,says,risk in cards:
    out.append(f'''
    <div style="display: flex; flex-direction: column; gap: 12px; min-width: 0">
      <div style="display: grid; grid-template-columns: 1fr 1fr">
        <div class="i1 tile" style="{B}">{use(sym,84)}</div>
        <div class="i2 tile">{use(sym,84)}</div>
        <div class="c1 tile" style="{B}">{use(sym,84)}</div>
        <div class="c2 tile">{use(sym,84)}</div>
        <div class="c1 strip" style="{B}">{use(sm,16)}{use(sm,24)}{use(sym,32)}</div>
        <div class="c2 strip">{use(sm,16)}{use(sm,24)}{use(sym,32)}</div>
      </div>
      <h2 style="margin: 0; font-size: 20px; font-weight: 600">{title}</h2>
      <p style="margin: 0; font-size: 14px; line-height: 1.55"><b style="font-weight: 600">Says configuration:</b> {says}</p>
      <p style="margin: 0; font-size: 14px; line-height: 1.55; color: #566074"><b style="color: #101521; font-weight: 600">Risk:</b> {risk}</p>
    </div>
''')
src=open('canvas2/project/Round2.dc.html').read()
head=src[:src.index('<svg width="0"')]
html=head+'<svg width="0" height="0" style="position:absolute" aria-hidden="true">\n'+tpl_sym+'</svg>\n'
html+='''<div style="width: 1440px; height: 1040px; box-sizing: border-box; padding: 40px 32px; background: #FFFFFF; color: #101521; font-family: 'Sora', system-ui, sans-serif; display: flex; flex-direction: column; gap: 28px">
  <div style="display: flex; flex-direction: column; gap: 8px">
    <div style="font-family: 'JetBrains Mono', monospace; font-size: 12px; font-weight: 500; letter-spacing: 0.08em; text-transform: uppercase; color: #566074">ZIO Config logo, fresh start, round 3</div>
    <h1 style="margin: 0; font-size: 38px; font-weight: 600; letter-spacing: -0.02em">Five settings you can point at</h1>
    <p style="margin: 0; font-size: 16px; line-height: 1.55; color: #566074; max-width: 80ch">Round 2 stayed with abstract ideas (merge, override). This round uses things people actually flip, nest or tick when they configure: switches, toggles, indented keys, key = value, checked rows. Chunkier and rounder, no gear. Same ink plus one orange-red.</p>
  </div>
  <div style="display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 20px">
'''+"".join(out)+'''  </div>
  <p style="margin: 0; font-size: 14px; line-height: 1.55; color: #566074; max-width: 100ch">You have rejected rounds 1 and 2 without saying why. Tell me one thing you dislike (too cold, too UI-icon, wrong colour, no Z, too plain) and I will aim the next round at it.</p>
</div>
</x-dc>
<script type="text/x-dc" data-dc-script data-props='{"$preview":{"width":1440,"height":1040}}'>
class Component extends DCLogic {
  renderVals() { return {}; }
}
</script>
</body>
</html>
'''
open('canvas2/project/Round3.dc.html','w').write(html)
# sizes test page
t='<style>body{margin:0;font-family:sans-serif}.i1{--bg:#fff;--fg:#101521;--ink:#101521}.c1{--bg:#fff;--fg:#F2501F;--ink:#101521}.c2{--bg:#101521;--fg:#FF6B3D;--ink:#F4F5F8}.r{display:flex;gap:24px;align-items:center;padding:12px;background:var(--bg)}</style><svg width="0" height="0" style="position:absolute">'+tpl_sym+'</svg>'
for cls in ('c1','c2'):
    for sym,sm,*_ in cards:
        t+=f'<div class="r {cls}">'+use(sm,16)+use(sm,24)+use(sym,32)+use(sym,64)+use(sym,96)+'</div>'
open('r3sizes.html','w').write(t)
