import re,json
def rows(spec,h,pill_mid,pill_end,knob):
    # spec: list of (x0,x1,knobside,kind) ; kind 'end' or 'mid'
    n=len(spec); gap=(62-n*h)/(n-1); o=''
    for i,(a,b,side,kind) in enumerate(spec):
        y=1+i*(h+gap); c=pill_end if kind=='end' else pill_mid
        o+=f'<rect x="{a}" y="{y:.2f}" width="{b-a}" height="{h}" rx="{h/2}" fill="var(--{c})"/>'
        kx=(b-h/2) if side=='R' else (a+h/2) if side=='L' else (a+b)/2
        o+=f'<circle cx="{kx}" cy="{y+h/2:.2f}" r="{h/2-1.6:.2f}" fill="var(--{knob})"/>'
    return o
def mk(spec,h,pm,pe,kn): return rows(spec,h,pm,pe,kn)
A=[(3,61,'R','end'),(25,61,'R','mid'),(3,39,'L','mid'),(3,61,'L','end')]
B=[(3,61,'R','end'),(17,47,'C','mid'),(3,61,'L','end')]
C=[(3,61,'R','end'),(30,61,'R','mid'),(17,47,'C','mid'),(3,34,'L','mid'),(3,61,'L','end')]
syms={}
cfg={"H1":(A,13,15,"fg","ink","bg"),"H2":(B,17,20,"fg","ink","bg"),"H3":(C,9.5,11.5,"fg","ink","bg")}
for k,(sp,h,hs,pm,pe,kn) in cfg.items():
    syms[k]=mk(sp,h,pm,pe,kn); syms[k+"s"]=mk(sp,hs,pm,pe,kn)
cards=[("H1","H1s","1 Stair","Four switches, all horizontal. The two middle ones step from right to left, so the stack draws the Z's diagonal with nothing but switches.","the stair is coarse, and four bars can read as a menu."),
("H2","H2s","2 Three switches","Three switches, all horizontal: full top, short coloured middle, full bottom. Knobs step right, centre, left and trace the diagonal.","the cleanest and the plainest; the Z comes mostly from the knobs and is weaker at 16 px."),
("H3","H3s","3 Fine stair","Five switches. Top and bottom full, three short ones stepping between them for a smoother Z.","thin bars: busy at 24 px and below.")]
src=open('canvas2/project/Round4.dc.html').read()
head=src[:src.index('<svg width="0"')]
def use(i,s): return f'<svg width="{s}" height="{s}"><use href="#{i}" width="{s}" height="{s}"/></svg>'
B_='border: 1px solid #E3E6EC'
out=''
for sym,sm,title,says,risk in cards:
    out+=f'''
    <div style="display: flex; flex-direction: column; gap: 12px; min-width: 0">
      <div style="display: grid; grid-template-columns: 1fr 1fr">
        <div class="i1 tile" style="{B_}">{use(sym,84)}</div>
        <div class="i2 tile">{use(sym,84)}</div>
        <div class="c1 tile" style="{B_}">{use(sym,84)}</div>
        <div class="c2 tile">{use(sym,84)}</div>
        <div class="c1 strip" style="{B_}">{use(sm,16)}{use(sm,24)}{use(sym,32)}</div>
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
    <div style="font-family: 'JetBrains Mono', monospace; font-size: 12px; font-weight: 500; letter-spacing: 0.08em; text-transform: uppercase; color: #566074">ZIO Config logo, fresh start, round 5</div>
    <h1 style="margin: 0; font-size: 38px; font-weight: 600; letter-spacing: -0.02em">Toggles, all horizontal, still a Z</h1>
    <p style="margin: 0; font-size: 16px; line-height: 1.55; color: #566074; max-width: 80ch">No diagonal line at all. Every part is a horizontal switch with a round knob, like a settings screen; the Z comes from a full top, a full bottom, and shorter switches stepping between them.</p>
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
open('canvas2/project/Round5.dc.html','w').write(html)
t='<style>body{margin:0}.c1{--bg:#fff;--fg:#F2501F;--ink:#101521}.c2{--bg:#101521;--fg:#FF6B3D;--ink:#F4F5F8}.r{display:flex;gap:24px;align-items:center;padding:12px;background:var(--bg)}</style><svg width="0" height="0" style="position:absolute">'+sy+'</svg>'
for c in('c1','c2'):
    for sym,sm,*_ in cards: t+=f'<div class="r {c}">'+use(sm,16)+use(sm,24)+use(sym,32)+use(sym,64)+use(sym,96)+'</div>'
open('r5sizes.html','w').write(t)
