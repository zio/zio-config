def z(pill,diag,knob,s=False):
    h,dw,kr = (19,14,7) if s else (16,12,5.5)
    ty=2 if s else 4; by=64-ty-h
    x0,w=(2,60) if s else (3,58)
    ky_t=ty+h/2; ky_b=by+h/2
    kx_r=x0+w-h/2; kx_l=x0+h/2
    return (f'<path d="M{kx_r-4} {ty+h/2} L{kx_l+4} {by+h/2}" fill="none" stroke="var(--{diag})" stroke-width="{dw}" stroke-linecap="round"/>'
      f'<rect x="{x0}" y="{ty}" width="{w}" height="{h}" rx="{h/2}" fill="var(--{pill})"/>'
      f'<rect x="{x0}" y="{by}" width="{w}" height="{h}" rx="{h/2}" fill="var(--{pill})"/>'
      f'<circle cx="{kx_r}" cy="{ky_t}" r="{kr}" fill="var(--{knob})"/><circle cx="{kx_l}" cy="{ky_b}" r="{kr}" fill="var(--{knob})"/>'
      f'<circle cx="32" cy="32" r="{kr-1}" fill="var(--{knob})"/>')
syms={}
for k,(p,d,kn) in {"Z1":("ink","fg","bg"),"Z2":("ink","ink","fg"),"Z3":("fg","ink","bg")}.items():
    syms[k]=z(p,d,kn); syms[k+"s"]=z(p,d,kn,True)
cards=[("Z1","Z1s","1 Ink pills, colour diagonal","Two toggles at the top and bottom, a coloured diagonal between them. Z first, switch second.","the diagonal has no knob of its own, so it reads more as a Z than as three switches."),
("Z2","Z2s","2 All ink, colour knobs","Three switches in one ink, with coloured knobs at the two corners and the middle. The knobs are the only colour.","busy: three knobs on one Z; the middle knob is small at 16 px."),
("Z3","Z3s","3 Colour pills, ink diagonal","Top and bottom toggles in colour, ink diagonal joining them. Strongest colour, most playful.","the colour sits on the horizontals, so the diagonal looks like a connector rather than part of the Z.")]
src=open('canvas2/project/Round3.dc.html').read()
head=src[:src.index('<svg width="0"')]
def use(i,s): return f'<svg width="{s}" height="{s}"><use href="#{i}" width="{s}" height="{s}"/></svg>'
B='border: 1px solid #E3E6EC'
out=''
for sym,sm,title,says,risk in cards:
    out+=f'''
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
'''
sy="".join(f'<symbol id="{k}" viewBox="0 0 64 64">{v}</symbol>\n' for k,v in syms.items())
html=head+'<svg width="0" height="0" style="position:absolute" aria-hidden="true">\n'+sy+'</svg>\n'+'''<div style="width: 1440px; height: 640px; box-sizing: border-box; padding: 40px 32px; background: #FFFFFF; color: #101521; font-family: 'Sora', system-ui, sans-serif; display: flex; flex-direction: column; gap: 28px">
  <div style="display: flex; flex-direction: column; gap: 8px">
    <div style="font-family: 'JetBrains Mono', monospace; font-size: 12px; font-weight: 500; letter-spacing: 0.08em; text-transform: uppercase; color: #566074">ZIO Config logo, fresh start, round 4</div>
    <h1 style="margin: 0; font-size: 38px; font-weight: 600; letter-spacing: -0.02em">Toggles, drawn as a Z</h1>
    <p style="margin: 0; font-size: 16px; line-height: 1.55; color: #566074; max-width: 80ch">From round 3 concept 2. Top and bottom switches are the Z's bars, a diagonal joins them, knobs sit where the Z turns. Three colour treatments of the same shape.</p>
  </div>
  <div style="display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 24px; max-width: 1000px">
'''+out+'''  </div>
</div>
</x-dc>
<script type="text/x-dc" data-dc-script data-props='{"$preview":{"width":1440,"height":640}}'>
class Component extends DCLogic {
  renderVals() { return {}; }
}
</script>
</body>
</html>
'''
open('canvas2/project/Round4.dc.html','w').write(html)
t='<style>body{margin:0}.c1{--bg:#fff;--fg:#F2501F;--ink:#101521}.c2{--bg:#101521;--fg:#FF6B3D;--ink:#F4F5F8}.r{display:flex;gap:24px;align-items:center;padding:12px;background:var(--bg)}</style><svg width="0" height="0" style="position:absolute">'+sy+'</svg>'
for c in('c1','c2'):
    for sym,sm,*_ in cards: t+=f'<div class="r {c}">'+use(sm,16)+use(sm,24)+use(sym,32)+use(sym,64)+use(sym,96)+'</div>'
open('r4sizes.html','w').write(t)
import json
p='canvas2/project/canvas.json';d=json.load(open(p))
d['boards']['Round4.dc.html']={"x":0,"y":3360,"w":1440,"h":640,"title":"Round 4: toggles drawn as a Z"}
if 'Round4.dc.html' not in d['order']: d['order'].append('Round4.dc.html')
json.dump(d,open(p,'w'),separators=(',',':'))
