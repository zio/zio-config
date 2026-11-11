"""Eight brand-sheet boards for ZIO Config (modelled on the ZIO Prelude sheet structure).
usage: gen_sheet_boards.py <Sora variable ttf> <outdir> [heights.json]
Outputs BrandN*.dc.html for the canvas plus plain preview html files (prev-*.html) for self-review."""
import sys, os, json
sys.path.insert(0, os.path.dirname(__file__))
from wcag import ratio
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen

SORA, OUT = sys.argv[1], sys.argv[2]
HEIGHTS = json.load(open(sys.argv[3])) if len(sys.argv) > 3 else {}
os.makedirs(OUT, exist_ok=True)

INK='#101521'; ACC='#F2501F'; PAPER='#FFFFFF'; SNOW='#F4F5F8'; NIGHT='#101521'; ACCD='#FF6B3D'
SLATE='#566074'; MIST='#A7AEBD'; TILE='#F1F2F6'; LINE='#E3E6EC'; SURF2='#1A2133'; LINED='#2A3142'
def mix(a, b, t):
    A=[int(a[i:i+2],16) for i in (1,3,5)]; B=[int(b[i:i+2],16) for i in (1,3,5)]
    return '#%02X%02X%02X' % tuple(round(x+(y-x)*t) for x,y in zip(A,B))
ACC700 = mix(ACC, '#000000', 0.30)          # accent text on light
def best_text(bg): return INK if ratio(INK,bg) >= ratio('#FFFFFF',bg) else '#FFFFFF'

# ---------- shapes: symbol + outlined logo ----------
def inst(w): return instantiateVariableFont(TTFont(SORA), {"wght": w})
bold, reg = inst(700), inst(400)
capH = bold["OS/2"].sCapHeight / bold["head"].unitsPerEm
def tp(font, s, size, x, y, tr=0):
    gs=font.getGlyphSet(); cm=font.getBestCmap(); u=font["head"].unitsPerEm; sc=size/u
    pen=SVGPathPen(gs); cx=x
    for ch in s:
        g=cm[ord(ch)]; gs[g].draw(TransformPen(pen,(sc,0,0,-sc,cx,y))); cx+=gs[g].width*sc+tr
    return pen.getCommands(), cx
def mark_parts(h=14.0):
    gap=(62-4*h)/3; rows=[(3,61,'R','i'),(22,61,'R','a'),(3,42,'L','a'),(3,61,'L','i')]; out=[]
    for i,(a,b,side,c) in enumerate(rows):
        y=1+i*(h+gap); kx=b-h/2 if side=='R' else a+h/2
        out.append((a,y,b-a,h,c,kx,y+h/2,h/2-0.129*h))
    return out
def fillv(c): return 'var(--ink)' if c=='i' else 'var(--acc)'
def mark_symbol(sid, h=14.0, knobs=True):
    parts=mark_parts(h); mid=sid+'k'
    bars=''.join(f'<rect x="{a}" y="{y:.2f}" width="{w}" height="{hh}" rx="{hh/2}" fill="{fillv(c)}"/>' for a,y,w,hh,c,kx,ky,r in parts)
    holes=''.join(f'<circle cx="{kx:.2f}" cy="{ky:.2f}" r="{r:.2f}" fill="#000"/>' for a,y,w,hh,c,kx,ky,r in parts)
    if knobs:
        return f'<symbol id="{sid}" viewBox="0 0 64 64"><mask id="{mid}"><rect width="64" height="64" fill="#fff"/>{holes}</mask><g mask="url(#{mid})">{bars}</g></symbol>'
    return f'<symbol id="{sid}" viewBox="0 0 64 64">{bars}</symbol>'
# logo: cap 30, baseline 47, Z = mark at cap height
s_=30/62.0; parts=mark_parts(14.0); size=30/capH
zb=''.join(f'<rect x="{(a-3)*s_:.2f}" y="{17+(y-1)*s_:.2f}" width="{w*s_:.2f}" height="{hh*s_:.2f}" rx="{hh*s_/2:.2f}" fill="{fillv(c)}"/>' for a,y,w,hh,c,kx,ky,r in parts)
zh=''.join(f'<circle cx="{(kx-3)*s_:.2f}" cy="{17+(ky-1)*s_:.2f}" r="{r*s_:.2f}" fill="#000"/>' for a,y,w,hh,c,kx,ky,r in parts)
zw=58*s_
d1,x1=tp(bold,"IO",size,zw+size*0.10,47,size*0.01); d2,LW=tp(reg,"Config",size,x1+size*0.26,47,size*0.005)
LOGO=f'<symbol id="zl" viewBox="0 16 {LW:.1f} 42"><mask id="zlk"><rect x="-50" y="-50" width="{LW+100:.0f}" height="200" fill="#fff"/>{zh}</mask><g mask="url(#zlk)">{zb}</g><path d="{d1}" fill="var(--ink)"/><path d="{d2}" fill="var(--ink)"/></symbol>'
# 16 px pixel-grid favicon (tile + 3 px bars + 1 px knob dots), colours fixed for the dark tile
px=[(1,15,'R',SNOW),(5,15,'R',ACCD),(1,11,'L',ACCD),(1,15,'L',SNOW)]
F16='<symbol id="zf" viewBox="0 0 16 16"><rect width="16" height="16" rx="3" fill="'+NIGHT+'"/>'+''.join(f'<rect x="{a}" y="{4*i}" width="{b-a}" height="3" rx="1" fill="{c}"/><rect x="{(b-2) if sd=="R" else (a+1)}" y="{4*i+1}" width="1" height="1" fill="{NIGHT}"/>' for i,(a,b,sd,c) in enumerate(px))+'</symbol>'
DEFS='<svg width="0" height="0" style="position:absolute" aria-hidden="true"><defs>'+mark_symbol('zm')+mark_symbol('zs',14.5)+mark_symbol('zn',14.0,False)+LOGO+F16+'</defs></svg>'

def mk(size, ink, acc, sym='zm', extra=''):
    return f'<svg width="{size}" height="{size}" viewBox="0 0 64 64" role="img" aria-label="ZIO Config symbol" style="--ink:{ink};--acc:{acc};{extra}"><use href="#{sym}"/></svg>'
def lg(h, ink, acc, extra=''):
    w=h*LW/42
    return f'<svg width="{w:.1f}" height="{h}" viewBox="0 0 {LW:.1f} 42" role="img" aria-label="ZIO Config" style="--ink:{ink};--acc:{acc};{extra}"><use href="#zl"/></svg>'
def tile_icon(size, dark=True, radius=0.225):
    bg = NIGHT if dark else PAPER; ink = SNOW if dark else INK; acc = ACCD if dark else ACC
    bd = '' if dark else f'stroke="{LINE}" stroke-width="0.8"'
    return (f'<svg width="{size}" height="{size}" viewBox="0 0 64 64" role="img" aria-label="ZIO Config app icon" style="--ink:{ink};--acc:{acc}">'
            f'<rect x="0.4" y="0.4" width="63.2" height="63.2" rx="{64*radius:.1f}" fill="{bg}" {bd}/><g transform="translate(9.6 9.6) scale(0.7)"><use href="#zm" width="64" height="64"/></g></svg>')

# ---------- layout helpers ----------
MONO="font-family:'JetBrains Mono',monospace"
def kicker(t, c=SLATE): return f'<div style="{MONO};font-size:13px;color:{c};letter-spacing:0.08em;text-transform:uppercase">{t}</div>'
def header(num, name, h1, intro):
    return (f'<div style="display:flex;flex-direction:column;gap:10px">{kicker(f"Brand sheet · {num} · {name}")}'
            f'<h1 style="margin:0;font-size:64px;line-height:1.05;font-weight:700;letter-spacing:-0.02em">{h1}</h1>'
            f'<div style="font-size:20px;color:{SLATE};max-width:960px;line-height:1.45">{intro}</div></div>')
def sec(n, title, body, note=None):
    nt = f'<div style="font-size:16px;color:{SLATE};line-height:1.5;max-width:900px">{note}</div>' if note else ''
    return (f'<div style="display:flex;flex-direction:column;gap:20px"><div style="display:flex;align-items:baseline;gap:16px;border-top:2px solid {INK};padding-top:14px">'
            f'<div style="{MONO};font-size:13px;color:{SLATE}">{n}</div><div style="font-size:28px;font-weight:700">{title}</div></div>{nt}{body}</div>')
def tile(bg, inner, h, extra=''):
    return f'<div style="height:{h}px;background:{bg};border-radius:16px;display:flex;align-items:center;justify-content:center;gap:24px;position:relative;overflow:hidden;{extra}">{inner}</div>'
def grid(cols, items, gap=32): return f'<div style="display:grid;grid-template-columns:repeat({cols},minmax(0,1fr));gap:{gap}px">{"".join(items)}</div>'
def cap(t, c=SLATE): return f'<div style="font-size:15px;color:{c};line-height:1.45">{t}</div>'
def code(t): return f'<code style="{MONO};font-size:0.92em">{t}</code>'
def light(inner,h,extra=''): return tile(TILE,inner,h,extra)
def dark(inner,h,extra=''): return tile(NIGHT,inner,h,extra)
def card(inner, label, h=150, bg=TILE):
    return (f'<div style="border:1px solid {LINE};border-radius:16px;overflow:hidden"><div style="height:{h}px;background:{bg};display:flex;align-items:center;justify-content:center;overflow:hidden">{inner}</div>'
            f'<div style="padding:12px 16px;color:{SLATE};font-size:15px">{label}</div></div>')

boards = {}
LN=lambda: (INK,ACC); DK=lambda: (SNOW,ACCD)

# ===== 1 Logo =====
b=[]
b.append(header('01','Logo','The Z is a stair of switches',
  'Four horizontal switches step across to draw a Z, each with a round knob. Ink on the top and bottom, orange-red in the middle. The Z of “ZIO” is the symbol, so the symbol and the logo are one shape.'))
b.append(sec('1.1','Symbol',
  grid(2,[light(mk(220,*LN()),380),dark(mk(220,*DK()),380)])+
  grid(2,[cap(f'Light mode. Bars in ink {code(INK)}, middle bars in orange-red {code(ACC)}. Knobs are real holes, so the colour behind shows through.'),
          cap(f'Dark mode. Bars in snow {code(SNOW)}, middle bars in light orange-red {code(ACCD)}.')])))
b.append(sec('1.2','Logo with the full name',
  grid(2,[light(lg(96,*LN()),240),dark(lg(96,*DK()),240),light(lg(56,*LN()),150),dark(lg(56,*DK()),150)])+
  cap('Wherever the full name appears, the Z of “ZIO” is the symbol. It stands at cap height on the baseline, then “IO” in Sora Bold and “Config” in Sora Regular, on one line. The symbol never sits beside a full “ZIO Config”, because that would say Z twice. For square spaces use the symbol alone or the icon tile.')))
b.append(sec('1.3','Clear space and minimum size',
  grid(2,[
    light(f'<div style="padding:48px;border:2px dashed {ACC700};position:relative">{mk(220,*LN())}</div>',420)+'',
    f'<div style="height:420px;background:{TILE};border-radius:16px;padding:32px;box-sizing:border-box;display:flex;flex-direction:column;justify-content:center;gap:40px">'
    f'<div style="display:flex;align-items:flex-end;gap:36px">'+''.join(f'<div style="display:flex;flex-direction:column;align-items:center;gap:10px">{mk(s,INK,ACC,"zs" if s<32 else "zm")}<div style="{MONO};font-size:12px;color:{SLATE}">{s}px</div></div>' for s in (16,24,32,48))+'</div>'
    f'<div style="display:flex;align-items:flex-end;gap:44px">'+''.join(f'<div style="display:flex;flex-direction:column;align-items:flex-start;gap:10px">{lg(h,INK,ACC)}<div style="{MONO};font-size:12px;color:{SLATE}">{h}px high</div></div>' for h in (32,48))+'</div></div>']) +
  grid(2,[cap('Keep one bar height (the dashed margin, 14 of 64 units of the symbol) free on every side of the symbol. Around the full logo keep the same distance, measured from the Z. No text, edges or other logos enter it.'),
          cap('The symbol alone goes down to 16 px, and uses the small cut (slightly thicker bars) below 32 px. The full logo goes down to 32 px high, so the knobs stay visible. Below that, use the symbol alone.')])))
mis=[card(mk(90,INK,ACC,extra='transform:scaleX(1.5)'),'Don’t stretch or squash.'),
     card(mk(90,INK,ACC,extra='transform:rotate(-14deg)'),'Don’t rotate or tilt.'),
     card(mk(90,INK,'#2F6FED'),'Don’t recolour the bars.'),
     card(mk(90,INK,ACC,'zn'),'Don’t remove the knobs.'),
     card(mk(90,INK,ACC),'Don’t place on a busy image.',bg='repeating-linear-gradient(45deg,#F9C4B2 0 14px,#F2501F 14px 28px)')]
b.append(sec('1.4','Misuse',grid(5,mis,24)))
boards['Brand1Logo']=('1 · Logo','Logo',b)

# ===== 2 Icon and favicon =====
b=[]
b.append(header('02','Icon and favicon','One mark, every small place',
  'The icon is the symbol on a rounded tile. The tile is what lets one file work on a light tab, a dark tab, a phone home screen and a GitHub avatar. At 16 px the symbol is redrawn on a pixel grid.'))
b.append(sec('2.1','App icon',
  grid(2,[light(tile_icon(240,True),380),dark(tile_icon(240,False),380)])+
  grid(4,[light(tile_icon(s,True),200) for s in (160,120,76,40)],24)+
  cap('Dark tile is the default. The light tile is for dark backgrounds that would swallow the dark one. Corner radius is 22.5% of the side; platforms that apply their own mask (iOS, Android adaptive) use the full-bleed square, with the symbol inside the central 80% safe circle.')))
b.append(sec('2.2','Favicon',
  grid(2,[light(f'<div style="display:flex;align-items:flex-end;gap:40px"><div style="display:flex;flex-direction:column;align-items:center;gap:10px"><svg width="16" height="16" viewBox="0 0 16 16" aria-hidden="true"><use href="#zf" width="16" height="16"/></svg><div style="{MONO};font-size:12px;color:{SLATE}">16 px</div></div><div style="display:flex;flex-direction:column;align-items:center;gap:10px">{tile_icon(32,True,0.2)}<div style="{MONO};font-size:12px;color:{SLATE}">32 px</div></div><div style="display:flex;flex-direction:column;align-items:center;gap:10px">{tile_icon(48,True,0.2)}<div style="{MONO};font-size:12px;color:{SLATE}">48 px</div></div></div>',260),
          light(f'<svg width="256" height="256" viewBox="0 0 16 16" shape-rendering="crispEdges" role="img" aria-label="16 pixel favicon enlarged"><use href="#zf" width="16" height="16"/></svg>',260)])+
  grid(2,[cap('The favicon keeps the dark tile so it is legible on light and dark browser chrome. The ICO holds 16, 32 and 48 px.'),
          cap('The 16 px version is drawn by hand on the pixel grid: 3 px bars, 1 px gaps and a single dot for each knob. Plain scaling blurs the bars together, so it is not scaled down from the larger file.')])))
def tab(bg,fg,bar,title,sub):
    return (f'<div style="background:{bar};border-radius:16px;padding:14px 14px 0;box-sizing:border-box;height:200px;overflow:hidden"><div style="display:flex;align-items:center;gap:10px;background:{bg};color:{fg};border-radius:10px 10px 0 0;padding:10px 16px;width:300px;font-size:14px"><svg width="16" height="16" viewBox="0 0 16 16" aria-hidden="true"><use href="#zf" width="16" height="16"/></svg><span>{title}</span></div>'
            f'<div style="background:{bg};height:140px;border-radius:0 8px 0 0;padding:20px;color:{fg};font-size:14px;{MONO}">{sub}</div></div>')
b.append(sec('2.3','In a browser tab',
  grid(2,[tab(PAPER,INK,'#DCDFE6','ZIO Config','zio.dev/zio-config'),tab(NIGHT,SNOW,'#0B0F18','ZIO Config','zio.dev/zio-config')])))
b.append(sec('2.4','Profile avatar',
  grid(4,[light(f'<div style="width:200px;height:200px;border-radius:50%;overflow:hidden">{tile_icon(200,True,0)}</div>',260),
          light(tile_icon(200,True),260),
          light(f'<div style="width:80px;height:80px;border-radius:50%;overflow:hidden">{tile_icon(80,True,0)}</div>',260),
          light(f'<div style="width:40px;height:40px;border-radius:50%;overflow:hidden">{tile_icon(40,True,0)}</div>',260)],24)+
  cap('Avatars are cropped to a circle or a rounded square. The symbol sits inside the central 80%, so neither crop touches the bars.')))
b.append(sec('2.5','One-colour versions',
  grid(4,[light(mk(120,INK,INK),220),dark(mk(120,SNOW,SNOW),220),light(mk(120,ACC,ACC),220),light(lg(52,INK,INK),220)],24)+
  cap('For stamps, engraving, fax and single-ink print. All bars take one colour and the knobs stay as holes. The one-colour orange-red is for the symbol only, never for text: it is 3.53:1 on white.')))
boards['Brand2Icons']=('2 · Icon and favicon','Icon and favicon',b)

# ===== 3 Colour =====
b=[]
b.append(header('03','Colour','Ink and one orange-red',
  'Almost everything is ink and paper. Orange-red appears once per layout, on the two middle bars of the Z, where the stair turns. Everything else stays cool and neutral, never warm grey.'))
def sw(name,hexv,role,w=1):
    fg=best_text(hexv); bd=f'border:1px solid {LINE};' if hexv.upper() in (PAPER,SNOW) else ''
    return (f'<div style="display:flex;flex-direction:column;border-radius:16px;overflow:hidden;{bd}"><div style="background:{hexv};height:150px;padding:16px;box-sizing:border-box;display:flex;align-items:flex-end;color:{fg};{MONO};font-size:14px">{hexv}</div>'
            f'<div style="padding:14px 16px;background:#fff;border-top:1px solid {LINE}"><div style="font-size:18px;font-weight:700">{name}</div><div style="font-size:14px;color:{SLATE};line-height:1.4;margin-top:4px">{role}</div></div></div>')
core=[('Ink',INK,'Symbol, text and dark surfaces'),('Orange-red',ACC,'Middle bars on light'),('Light orange-red',ACCD,'Middle bars on dark'),('Paper',PAPER,'Light background'),('Snow',SNOW,'Text on dark, panels on light'),('Slate',SLATE,'Secondary text on light'),('Mist',MIST,'Secondary text on dark'),('Orange-red 700',ACC700,'Orange-red text on light')]
b.append(sec('3.1','Core palette',grid(4,[sw(*c) for c in core],24)))
def scale(name,core_hex,steps):
    cells=''
    for lab,hx in steps:
        cells+=f'<div style="flex:1;background:{hx};height:92px;padding:10px;box-sizing:border-box;display:flex;flex-direction:column;justify-content:flex-end;color:{best_text(hx)};{MONO};font-size:12px"><div>{lab}</div><div>{hx}</div></div>'
    return f'<div><div style="font-size:16px;font-weight:700;margin-bottom:10px">{name}</div><div style="display:flex;border-radius:12px;overflow:hidden;border:1px solid {LINE}">{cells}</div></div>'
acc_steps=[('50',mix(ACC,'#FFFFFF',.92)),('100',mix(ACC,'#FFFFFF',.84)),('200',mix(ACC,'#FFFFFF',.68)),('300',mix(ACC,'#FFFFFF',.5)),('400',mix(ACC,'#FFFFFF',.25)),('500',ACC),('600',mix(ACC,'#000000',.15)),('700',ACC700),('800',mix(ACC,'#000000',.5)),('900',mix(ACC,'#000000',.68))]
ink_t=[.95,.88,.76,.6,.42,.28,.16,.08,.03,0]
ink_steps=[(str(l),mix(INK,'#FFFFFF',t)) for l,t in zip((50,100,200,300,400,500,600,700,800,900),ink_t)]
b.append(sec('3.2','Tints and shades',scale('Orange-red',ACC,acc_steps)+scale('Ink',INK,ink_steps),
  f'Each scale runs from a light wash to a deep shade. The core orange-red is step 500. Use 50 to 200 for fills and highlights. Step 700 ({ACC700}) is the orange-red for text on a light surface: it passes 4.5:1 where the core colour does not. Ink step 900 is the core ink.'))
def mode(bg,fg,sub,acc,surf,line,label):
    return (f'<div style="background:{bg};border-radius:16px;padding:32px;display:flex;flex-direction:column;gap:20px;border:1px solid {line}">{kicker(label,sub)}'
            f'<div style="display:flex;align-items:center;gap:20px">{mk(72,fg,acc)}<div style="color:{fg};font-size:28px;font-weight:700;line-height:1.1">Configuration, set once</div></div>'
            f'<div style="color:{sub};font-size:16px;line-height:1.5">Body copy sits on the page colour in {("ink" if bg==PAPER else "snow")}; secondary text is {("slate" if bg==PAPER else "mist")}.</div>'
            f'<div style="background:{surf};border-radius:12px;padding:18px 20px;color:{fg};{MONO};font-size:15px;line-height:1.6">port = 8080<br>host = &quot;localhost&quot;</div>'
            f'<div style="display:flex;gap:12px"><div style="background:{fg};color:{bg};border-radius:12px;padding:14px 24px;font-size:16px;font-weight:700">Get started</div><div style="border:2px solid {fg};color:{fg};border-radius:12px;padding:12px 22px;font-size:16px;font-weight:700">GitHub</div></div></div>')
b.append(sec('3.3','Light and dark mode',grid(2,[mode(PAPER,INK,SLATE,ACC,SNOW,LINE,'Light mode'),mode(NIGHT,SNOW,MIST,ACCD,SURF2,LINED,'Dark mode')]),
  'The primary button flips between ink and snow, so its label always has full contrast. The orange-red never fills a button.'))
pairs=[('Ink on paper',INK,PAPER,4.5),('Slate on paper',SLATE,PAPER,4.5),('Orange-red 700 on paper',ACC700,PAPER,4.5),('Orange-red on paper (graphics only)',ACC,PAPER,3.0),('Ink on orange-red',INK,ACC,4.5),('White on orange-red',PAPER,ACC,4.5),
       ('Snow on night',SNOW,NIGHT,4.5),('Mist on night',MIST,NIGHT,4.5),('Light orange-red on night',ACCD,NIGHT,4.5)]
rows=''
for n,f,bg,need in pairs:
    r=ratio(f,bg); ok='Pass' if r>=need else 'Fail'; col='#1B7A3E' if ok=='Pass' else '#B3261E'
    rows+=(f'<div style="display:grid;grid-template-columns:44px 1.6fr 1fr 0.7fr 0.7fr 0.6fr;gap:16px;align-items:center;padding:12px 0;border-bottom:1px solid {LINE};font-size:16px">'
           f'<div style="width:36px;height:36px;border-radius:8px;background:{bg};border:1px solid {LINE};display:flex;align-items:center;justify-content:center;color:{f};font-weight:700">Aa</div>'
           f'<div style="font-weight:600">{n}</div><div style="{MONO};font-size:14px;color:{SLATE}">{f} on {bg}</div><div>{r:.2f}:1</div><div style="color:{SLATE}">needs {need}:1</div><div style="color:{col};font-weight:700">{ok}</div></div>')
b.append(sec('3.4','Contrast pairs',f'<div style="display:flex;flex-direction:column">{rows}</div>',
  f'Ratios are computed from the hex values (WCAG 2.x). Orange-red on white is {ratio(ACC,PAPER):.2f}:1, under the 4.5:1 text bar: use it for the middle bars of the symbol, rules and large graphics, and use step 700 for any orange-red text on light. White text on orange-red also fails, so labels on orange-red are ink.'))
def seg(pct,col,lab,tc):
    return f'<div style="flex:{pct};background:{col};color:{tc};padding:14px;{MONO};font-size:13px;display:flex;align-items:flex-end;min-height:120px">{lab}</div>'
b.append(sec('3.5','Proportion',f'<div style="display:flex;border-radius:16px;overflow:hidden;border:1px solid {LINE}">{seg(70,PAPER,"Paper and snow 70%",INK)}{seg(25,INK,"Ink 25%",SNOW)}{seg(5,ACC,"5%",INK)}</div>',
  'Mostly paper and ink. Orange-red is about 5%: the two middle bars of the symbol, one rule, one link. If it fills a section it has stopped being the turn in the stair and become the background.'))
boards['Brand3Colour']=('3 · Colour','Colour',b)

# ===== 4 Typography =====
b=[]
b.append(header('04','Typography','Two typefaces, one scale',
  'Sora carries headings and body. JetBrains Mono carries code, type names and small labels. Both are free and open source, and both load from Google Fonts. The wordmark is Sora with its letters converted to outlines.'))
def spec(fam,name,weights,sample,desc,mono=False):
    ff=f"font-family:{fam}"
    return (f'<div style="border:1px solid {LINE};border-radius:16px;padding:28px;display:flex;flex-direction:column;gap:16px"><div style="{ff};font-size:120px;line-height:1;font-weight:700;letter-spacing:-0.02em">Aa</div>'
            f'<div style="font-size:28px;font-weight:700">{name}</div><div style="color:{SLATE};font-size:15px;line-height:1.5">{desc}</div>'
            f'<div style="{ff};font-size:20px;line-height:1.5">{sample}</div><div style="{MONO};font-size:13px;color:{SLATE}">{weights}</div></div>')
b.append(sec('4.1','Typefaces',grid(2,[spec("'Sora',sans-serif",'Sora','400 · 600 · 700','ABCDEFGHIJKLMNOPQRSTUVWXYZ<br>abcdefghijklmnopqrstuvwxyz 0123456789','Headings and body. A geometric sans with open shapes; the wordmark uses Bold for IO and Regular for Config.'),
  spec("'JetBrains Mono',monospace",'JetBrains Mono','400 · 500','ABCDEFGHIJKLMNOPQRSTUVWXYZ<br>abcdefghijklmnopqrstuvwxyz 0123456789 {}[]=&gt;','Code, type names, file names and small labels.',True)],32)))
scale_rows=[('Display','64 / 1.05','700','-2%','Configuration for ZIO',64,700,-0.02),('Heading 1','44 / 1.1','700','-1.5%','Describe your settings once',44,700,-0.015),('Heading 2','28 / 1.2','700','-1%','Sources and providers',28,700,-0.01),('Heading 3','20 / 1.3','600','0','Load from the environment',20,600,0),('Body','16 / 1.6','400','0','Body copy keeps normal tracking and a line length of 60 to 72 characters.',16,400,0),('Small','14 / 1.5','400','0','Captions, notes and table text.',14,400,0)]
rows=''
for n,sz,w,tr,sample,fs,fw,ls in scale_rows:
    rows+=(f'<div style="display:grid;grid-template-columns:200px 1fr;gap:24px;align-items:baseline;padding:16px 0;border-bottom:1px solid {LINE}"><div><div style="font-size:16px;font-weight:700">{n}</div><div style="{MONO};font-size:12px;color:{SLATE};margin-top:4px">{sz} · {w} · {tr}</div></div><div style="font-size:{fs}px;font-weight:{fw};letter-spacing:{ls}em;line-height:1.15">{sample}</div></div>')
rows+=(f'<div style="display:grid;grid-template-columns:200px 1fr;gap:24px;align-items:baseline;padding:16px 0"><div><div style="font-size:16px;font-weight:700">Label</div><div style="{MONO};font-size:12px;color:{SLATE};margin-top:4px">12 · mono 500 · +8%</div></div><div style="{MONO};font-size:12px;font-weight:500;letter-spacing:0.08em;text-transform:uppercase;color:{SLATE}">Brand sheet · 04 · Typography</div></div>')
b.append(sec('4.2','Type scale',f'<div style="display:flex;flex-direction:column">{rows}</div>','Heading sizes tighten their tracking as they grow. Body copy keeps normal tracking. Sentence case everywhere, never all caps except mono labels.'))
KW=lambda t,c:f'<span style="color:{c}">{t}</span>'
def codeblock(bg,fg,kw,ty,dim,border):
    return (f'<div style="background:{bg};color:{fg};border-radius:16px;padding:28px 32px;border:1px solid {border};{MONO};font-size:16px;line-height:1.7;white-space:pre">'
            f'{KW("final case class",kw)} {KW("AppConfig",ty)}(host: {KW("String",ty)}, port: {KW("Int",ty)})\n\n'
            f'{KW("val",kw)} appConfig: {KW("Config",ty)}[{KW("AppConfig",ty)}] =\n  ({KW("Config",ty)}.string({KW(chr(34)+"host"+chr(34),dim)}) {KW("zip",kw)} {KW("Config",ty)}.int({KW(chr(34)+"port"+chr(34),dim)}))\n    .map {{ {KW("case",kw)} (h, p) => {KW("AppConfig",ty)}(h, p) }}</div>')
b.append(sec('4.3','Code',grid(2,[codeblock(SNOW,INK,ACC700,'#1D3F72','#5B6478',LINE),codeblock(NIGHT,SNOW,ACCD,'#8FB4F0',MIST,LINED)],32),
  f'Code uses JetBrains Mono at 15 to 16 px with a 1.7 line height. Keywords take the deep orange-red ({ACC700}) on light and the light orange-red on dark. Type names take a blue, the one cool colour, used only inside code. The snippet is illustrative.'))
b.append(sec('4.4','Rules',grid(3,[cap('<b style="color:'+INK+'">Sentence case.</b> Headings and buttons are sentence case: “Get started”, not “Get Started”.'),cap('<b style="color:'+INK+'">Names in mono.</b> Library, type and file names are set in JetBrains Mono when they appear inside running text.'),cap('<b style="color:'+INK+'">One weight jump.</b> A layout uses regular, then bold. Semibold is for small headings only.')],32)))
boards['Brand4Type']=('4 · Typography','Typography',b)

# ===== 5 Voice =====
b=[]
b.append(header('05','Voice and tone','Exact, plain and calm',
  'ZIO Config helps developers describe, load and check settings. The voice says exactly what a thing does, in the plainest words that stay accurate, and stays calm when something is misconfigured.'))
trait=[('Exact','Says which setting, which source and what was expected. “port must be an integer, found “eight”.”'),('Plain','Short words, short sentences. “Load” rather than “leverage”, “set” rather than “provision”.'),('Calm','Mistakes in configuration are normal. Errors state the fix, never blame.'),('Practical','Shows the smallest working example first, then the detail.')]
b.append(sec('5.1','Voice',grid(4,[f'<div style="border:1px solid {LINE};border-radius:16px;padding:24px;display:flex;flex-direction:column;gap:10px"><div style="font-size:28px;font-weight:700">{t}</div><div style="color:{SLATE};font-size:16px;line-height:1.5">{d}</div></div>' for t,d in trait],24),
  'The voice follows how the library behaves: typed, explicit and predictable.'))
tone=[('Documentation','Direct and patient','Step by step, one idea per paragraph, code before explanation.',20),('Error messages','Calm and specific','Name the setting and the source, say what was expected, suggest the fix.',40),('Release notes','Factual','What changed, who it affects, how to migrate. No adjectives.',60),('Social','Friendly and brief','One idea, one link. A small joke is fine, a boast is not.',85)]
rowsT=''.join(f'<div style="display:grid;grid-template-columns:200px 220px 1fr 260px;gap:24px;align-items:center;padding:16px 0;border-bottom:1px solid {LINE}"><div style="font-size:18px;font-weight:700">{c}</div><div style="font-size:16px">{t}</div><div style="font-size:15px;color:{SLATE};line-height:1.5">{d}</div><div style="position:relative;height:8px;background:{TILE};border-radius:4px"><div style="position:absolute;left:{p}%;top:-6px;width:20px;height:20px;border-radius:50%;background:{INK};transform:translateX(-50%)"></div></div></div>' for c,t,d,p in tone)
b.append(sec('5.2','Tone by context',f'<div style="display:flex;flex-direction:column">{rowsT}<div style="display:grid;grid-template-columns:200px 220px 1fr 260px;gap:24px;padding-top:10px;{MONO};font-size:12px;color:{SLATE}"><div></div><div></div><div></div><div style="display:flex;justify-content:space-between"><span>Careful</span><span>Playful</span></div></div></div>','The voice stays the same. The tone moves along one slider as the reader’s situation changes.'))
ex=[('Docs','Start with a single value. Config.int(“port”) reads one integer called port from your sources.'),('Error','Could not load “port”: expected an integer, found “eight” in the environment variable APP_PORT.'),('Release note','Fixed: a missing optional setting no longer reports an error when a default is set.'),('Social','Your settings, written down once. Typed, checked, in one place.')]
b.append(sec('5.3','In practice',grid(2,[f'<div style="border:1px solid {LINE};border-radius:16px;padding:24px;display:flex;flex-direction:column;gap:10px"><div style="{MONO};font-size:12px;letter-spacing:0.08em;text-transform:uppercase;color:{SLATE}">{l}</div><div style="font-size:20px;line-height:1.4">{t}</div></div>' for l,t in ex],24),'Sample lines for tone only. They are not release claims.'))
tags=[('Configuration for ZIO applications','Placeholder, plain description. Used on the banner and cards in this sheet.',True),('Settings you can reason about','Leans on the typed, explicit side.',False),('Describe it once. Load it anywhere.','Leans on the sources idea.',False)]
b.append(sec('5.4','Taglines',f'<div style="display:flex;flex-direction:column;gap:12px">'+''.join(f'<div style="display:grid;grid-template-columns:48px 1fr 1fr 120px;gap:20px;align-items:center;padding:16px 20px;border:{"2px solid "+INK if sel else "1px solid "+LINE};border-radius:16px"><div style="{MONO};font-size:13px;color:{SLATE}">{i+1}</div><div style="font-size:24px;font-weight:700;letter-spacing:-0.01em">{t}</div><div style="font-size:15px;color:{SLATE}">{d}</div><div style="{MONO};font-size:12px;text-transform:uppercase;letter-spacing:0.08em;color:{INK}">{"In use" if sel else "Option"}</div></div>' for i,(t,d,sel) in enumerate(tags))+'</div>',
  'Proposals to choose from; the wording is for the maintainers to confirm. A tagline used as a headline carries no end punctuation.'))
b.append(sec('5.5','Words',grid(2,[f'<div style="border:1px solid {LINE};border-radius:16px;padding:24px;display:flex;flex-direction:column;gap:12px"><div style="font-size:18px;font-weight:700;color:#1B7A3E">Use</div><div style="font-size:18px;line-height:1.7">load · set · source · setting · value · check · default · override · missing · expected</div></div>',
  f'<div style="border:1px solid {LINE};border-radius:16px;padding:24px;display:flex;flex-direction:column;gap:12px"><div style="font-size:18px;font-weight:700;color:#B3261E">Avoid</div><div style="font-size:18px;line-height:1.7">magic · seamless · simply · just · blazing · leverage · robust · effortless</div></div>'],24)))
boards['Brand5Voice']=('5 · Voice and tone','Voice and tone',b)

# ===== 6 Imagery =====
b=[]
b.append(header('06','Visual and imagery style','Switches, flows and code',
  'The imagery comes from the symbol and from what the library does: switches, sources flowing into one value, and code. It is flat, cool and structured. No people at laptops, no gradients, no 3D.'))
def bars_big(h):  # large cropped stair
    return f'<div style="position:absolute;left:-40px;top:-30px">{mk(h,INK,ACC)}</div>'
gl=[light(f'<div style="position:absolute;right:-50px;bottom:-60px">{mk(380,INK,ACC)}</div>',340),
    light(f'<svg width="260" height="100" viewBox="0 0 52 20" role="img" aria-label="Single switch" style="--ink:{INK};--acc:{ACC}"><rect x="2" y="2" width="48" height="16" rx="8" fill="{INK}"/><circle cx="42" cy="10" r="5.4" fill="{TILE}"/><circle cx="42" cy="10" r="5.4" fill="{TILE}"/></svg>',340),
    light('<div style="display:flex;flex-direction:column;gap:10px;align-items:flex-start">'+''.join(f'<div style="width:{w}px;height:30px;border-radius:15px;background:{c}"></div>' for w,c in ((220,INK),(160,ACC),(260,INK),(120,INK),(190,ACC)))+'</div>',340)]
b.append(sec('6.1','Graphic language',grid(3,gl,24)+grid(3,[cap('<b style="color:'+INK+'">Stair.</b> The symbol, large and cropped by the edge of the layout.'),cap('<b style="color:'+INK+'">Switch.</b> One pill with a knob, on its own, as a divider or a button shape.'),cap('<b style="color:'+INK+'">Rows.</b> Pills of different lengths, like a settings list. Orange-red on one or two rows only.')],24),
  'Three constructions taken from the symbol. Use one per layout, big and cropped, with the orange-red on the first element only. Never tile them across a page.'))
def pill(x,y,w,t,fill,fc,stroke=''):
    return f'<rect x="{x}" y="{y}" width="{w}" height="44" rx="22" fill="{fill}" {stroke}/><text x="{x+w/2}" y="{y+28}" text-anchor="middle" font-family="JetBrains Mono, monospace" font-size="16" fill="{fc}">{t}</text>'
def diag(dk):
    pf,pt,line,boxf,boxt,sub,ac = (SNOW,NIGHT,SNOW,NIGHT,SNOW,MIST,ACCD) if dk else (INK,SNOW,INK,PAPER,INK,SLATE,ACC)
    return (f'<svg viewBox="0 0 900 320" width="100%" role="img" aria-label="Three sources merge into one typed config for the app" style="max-width:900px">'
      +pill(20,40,200,'environment',pf,pt)+pill(20,138,200,'config file',pf,pt)+pill(20,236,200,'command line',pf,pt)
      +f'<path d="M222 62 H300 Q330 62 330 92 V138 M222 160 H330 M222 258 H300 Q330 258 330 228 V182" fill="none" stroke="{line}" stroke-width="3" stroke-linecap="round"/>'
      +f'<rect x="330" y="116" width="240" height="88" rx="18" fill="{boxf}" stroke="{line}" stroke-width="3"/><text x="450" y="154" text-anchor="middle" font-family="Sora, sans-serif" font-size="22" font-weight="700" fill="{boxt}">Config[AppConfig]</text><text x="450" y="182" text-anchor="middle" font-family="Sora, sans-serif" font-size="15" fill="{sub}">typed, checked</text>'
      +f'<path d="M572 160 H650" fill="none" stroke="{ac}" stroke-width="4" stroke-linecap="round"/><path d="M640 148 L654 160 L640 172" fill="none" stroke="{ac}" stroke-width="4" stroke-linecap="round" stroke-linejoin="round"/>'
      +pill(660,138,200,'your app',pf,pt)+'</svg>')
b.append(sec('6.2','Diagrams',tile(TILE,diag(False),380,'padding:24px;box-sizing:border-box')+tile(NIGHT,diag(True),380,'padding:24px;box-sizing:border-box'),
  'Diagrams carry most of the imagery. Sources are solid pills, the typed Config is an outlined box, and the single orange-red stroke marks the value arriving in the app.'))
b.append(sec('6.3','Code screenshots',grid(2,[codeblock(SNOW,INK,ACC700,'#1D3F72','#5B6478',LINE),codeblock(NIGHT,SNOW,ACCD,'#8FB4F0',MIST,LINED)],32),
  'Code is shown as text in a rounded card with a 32 px inner margin, never as a window with traffic-light buttons. Light and dark cards match the page.'))
IC='stroke-width="3" stroke-linecap="round" stroke-linejoin="round" fill="none"'
def icon(inner,label): return f'<div style="display:flex;flex-direction:column;align-items:center;gap:12px"><div style="width:120px;height:120px;border-radius:16px;background:{TILE};display:flex;align-items:center;justify-content:center"><svg width="72" height="72" viewBox="0 0 48 48" aria-hidden="true">{inner}</svg></div><div style="font-size:14px;color:{SLATE}">{label}</div></div>'
icons=[icon(f'<rect x="5" y="15" width="38" height="18" rx="9" stroke="{INK}" {IC}/><circle cx="34" cy="24" r="4" stroke="{ACC}" {IC}/>','Setting'),
       icon(f'<path d="M12 6 H28 L38 16 V42 H12 Z" stroke="{INK}" {IC}/><path d="M18 28 H32" stroke="{ACC}" {IC}/><path d="M18 35 H28" stroke="{INK}" {IC}/>','File'),
       icon(f'<path d="M10 14 L22 24 L10 34" stroke="{INK}" {IC}/><path d="M26 36 H38" stroke="{ACC}" {IC}/>','Environment'),
       icon(f'<path d="M8 10 H18 L28 24 M8 38 H18 L28 24 M8 24 H28" stroke="{INK}" {IC}/><path d="M28 24 H40 M34 18 L40 24 L34 30" stroke="{ACC}" {IC}/>','Merge'),
       icon(f'<circle cx="24" cy="24" r="17" stroke="{INK}" {IC}/><path d="M16 25 L22 31 L33 18" stroke="{ACC}" {IC}/>','Valid'),
       icon(f'<circle cx="16" cy="24" r="8" stroke="{INK}" {IC}/><path d="M24 24 H42 M36 24 V31" stroke="{ACC}" {IC}/>','Key')]
b.append(sec('6.4','Icons',f'<div style="display:grid;grid-template-columns:repeat(6,1fr);gap:24px">{"".join(icons)}</div>','Line icons on a 48 px grid, a 3 px stroke with round caps. Ink strokes with one orange-red stroke per icon, on the part that changes or arrives.'))
b.append(sec('6.5','Rules',grid(3,[cap('<b style="color:'+INK+'">One accent per picture.</b> Orange-red marks one thing: the turn of the stair, the arriving value, the key part of an icon.'),cap('<b style="color:'+INK+'">Round ends.</b> Strokes and bars end in a full radius, like the pills in the symbol. Square ends only in code and tables.'),cap('<b style="color:'+INK+'">Real content.</b> Show real setting names and values, not lorem ipsum, and keep example values obviously fake (port 8080, localhost).')],32)))
boards['Brand6Imagery']=('6 · Visual and imagery style','Visual and imagery style',b)

# ===== 7 Hero =====
TAGL='Configuration for ZIO applications'
SUBL='Describe your settings once, then load them from files, the environment or the command line.'
def hero(mode_dark, w=1352, h=440, scale=1.0):
    bg=NIGHT if mode_dark else SNOW; fg=SNOW if mode_dark else INK; sub=MIST if mode_dark else SLATE; acc=ACCD if mode_dark else ACC; bd=LINED if mode_dark else LINE
    tilebg=SURF2 if mode_dark else PAPER
    btn1=f'background:{fg};color:{bg}'; btn2=f'border:2px solid {fg};color:{fg}'
    inner=(f'<div style="width:{w}px;height:{h}px;background:{bg};border-radius:20px;border:1px solid {bd};box-sizing:border-box;padding:56px 64px;display:flex;align-items:center;justify-content:space-between;position:relative;overflow:hidden;color:{fg}">'
           f'<div style="position:absolute;right:-60px;bottom:-80px;opacity:{0.10 if mode_dark else 0.07}">{mk(520,fg,acc)}</div>'
           f'<div style="display:flex;flex-direction:column;gap:22px;max-width:760px;position:relative"><div>{lg(56,fg,acc)}</div>'
           f'<div style="font-size:68px;font-weight:700;line-height:1.04;letter-spacing:-0.025em">{TAGL}</div>'
           f'<div style="font-size:22px;line-height:1.45;color:{sub};max-width:640px">{SUBL}</div>'
           f'<div style="display:flex;gap:14px;margin-top:6px"><div style="{btn1};border-radius:12px;padding:16px 28px;font-size:18px;font-weight:700">Get started</div><div style="{btn2};border-radius:12px;padding:14px 26px;font-size:18px;font-weight:700">View on GitHub</div></div></div>'
           f'<div style="position:relative;width:300px;height:300px;border-radius:40px;background:{tilebg};border:1px solid {bd};display:flex;align-items:center;justify-content:center">{mk(190,fg,acc)}</div></div>')
    return inner
def readme(dark_):
    bg=NIGHT if dark_ else PAPER; fg=SNOW if dark_ else INK; sub=MIST if dark_ else SLATE; acc=ACCD if dark_ else ACC; bd=LINED if dark_ else LINE
    inner=(f'<div style="width:1200px;height:300px;background:{bg};border:1px solid {bd};box-sizing:border-box;padding:0 64px;display:flex;align-items:center;gap:48px;position:relative;overflow:hidden;color:{fg}">'
           f'<div style="display:flex;flex-direction:column;gap:20px"><div>{lg(96,fg,acc)}</div><div style="font-size:26px;color:{sub}">{TAGL}</div></div>'
           f'<div style="position:absolute;right:-30px;top:-20px;opacity:{0.16 if dark_ else 0.10}">{mk(340,fg,acc)}</div></div>')
    return f'<div style="width:660px;height:165px;overflow:hidden;position:relative;flex:none"><div style="width:1200px;height:300px;transform:scale(0.55);transform-origin:0 0">{inner}</div></div>'
b=[]
b.append(header('07','Hero banner','The first thing on the page',
  'The hero leads with the logo and the tagline and keeps orange-red to the two middle bars of the symbol. The symbol appears twice: once large and cropped as a ghost, once as the sharp focal point on a tile.'))
b.append(sec('7.1','Website hero · 1352 × 440',
  f'<div style="display:flex;flex-direction:column;gap:32px">{kicker("Light mode")}{hero(False)}{kicker("Dark mode")}{hero(True)}</div>',
  'Light and dark share one layout. The primary button flips between ink and snow, so the label always has full contrast. Orange-red never fills a button. Headline and subline are placeholder copy.'))
b.append(sec('7.2','README banner · 1200 × 300',
  grid(2,[f'<div style="display:flex;flex-direction:column;gap:12px">{kicker("Light mode")}{readme(False)}</div>',f'<div style="display:flex;flex-direction:column;gap:12px">{kicker("Dark mode")}{readme(True)}</div>'],32),
  'GitHub shows a README in light or dark depending on the viewer, so ship both and switch with a picture element. Previews here are drawn at 55%.'))
boards['Brand7Hero']=('7 · Hero banner','Hero banner',b)

# ===== 8 Social =====
def social_card(dark_, w, h, scale):
    bg=NIGHT if dark_ else PAPER; fg=SNOW if dark_ else INK; sub=MIST if dark_ else SLATE; acc=ACCD if dark_ else ACC; bd=LINED if dark_ else LINE
    inner=(f'<div style="width:{w}px;height:{h}px;background:{bg};border:1px solid {bd};box-sizing:border-box;padding:0 96px;display:flex;flex-direction:column;justify-content:center;gap:28px;position:relative;overflow:hidden;color:{fg}">'
                      f'<div style="position:relative">{lg(150,fg,acc)}</div><div style="font-size:40px;color:{sub};position:relative">{TAGL}</div>'
           f'<div style="position:absolute;right:96px;bottom:52px;{MONO};font-size:26px;color:{sub}">zio.dev/zio-config</div></div>')
    ow=640; oh=round(h*ow/w)
    return f'<div style="width:{ow}px;height:{oh}px;overflow:hidden;position:relative;flex:none;border-radius:8px"><div style="width:{w}px;height:{h}px;transform:scale({ow/w:.4f});transform-origin:0 0">{inner}</div></div>'
plat=[('Open Graph','Link previews in chat, forums and most sites',1200,630),('X','Large summary card',1200,600),('LinkedIn','Shared link image',1200,627),('GitHub','Repository social preview',1280,640)]
b=[]
b.append(header('08','Social cards','One card, four platforms',
  'One template, resized per platform, in light and dark. Previews are drawn at 640 px wide. Platforms crop and round corners differently, so the logo and tagline sit well inside every edge.'))
for i,(n,d,w,h) in enumerate(plat):
    b.append(sec(f'8.{i+1}',f'{n} · {w} × {h}',grid(2,[f'<div style="display:flex;flex-direction:column;gap:12px">{kicker("Light mode")}{social_card(False,w,h,1)}</div>',f'<div style="display:flex;flex-direction:column;gap:12px">{kicker("Dark mode")}{social_card(True,w,h,1)}</div>'],32),d+'. Tagline and address are placeholders until the maintainers confirm them.'))
boards['Brand8Social']=('8 · Social cards','Social cards',b)

# ---------- emit ----------
HEAD=('<link rel="preconnect" href="https://fonts.googleapis.com">'
      '<link href="https://fonts.googleapis.com/css2?family=Sora:wght@400;600;700&amp;family=JetBrains+Mono:wght@400;500&amp;display=swap" rel="stylesheet">')
meta={}
for key,(title,name,secs) in boards.items():
    H=HEIGHTS.get(key,2400)
    body=DEFS+f'<div style="width:1480px;height:{H}px;box-sizing:border-box;padding:64px;background:#FFFFFF;color:{INK};font-family:\'Sora\',Inter,sans-serif;display:flex;flex-direction:column;gap:56px;overflow:hidden">'+''.join(secs)+'</div>'
    dc=(f'<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n<title>ZIO Config brand {name}</title>\n<script src="./support.js"></script>\n</head>\n<body>\n<x-dc>\n<helmet>\n{HEAD}\n<style>\nbody{{margin:0}}\n</style>\n</helmet>\n{body}\n</x-dc>\n'
        f'<script type="text/x-dc" data-dc-script data-props=\'{{"$preview":{{"width":1480,"height":{H}}}}}\'>\nclass Component extends DCLogic {{\nrenderVals() {{\nreturn {{}};\n}}\n}}\n</script>\n</body>\n</html>\n')
    open(f'{OUT}/{key}.dc.html','w').write(dc)
    prev=f'<!doctype html><html><head><meta charset="utf-8">{HEAD.replace("&amp;","&")}<style>body{{margin:0}}</style></head><body>{DEFS}<div style="width:1480px;box-sizing:border-box;padding:64px;background:#FFFFFF;color:{INK};font-family:\'Sora\',Inter,sans-serif;display:flex;flex-direction:column;gap:56px">'+''.join(secs)+'</div></body></html>'
    open(f'{OUT}/prev-{key}.html','w').write(prev)
    meta[key]=title
json.dump(meta,open(f'{OUT}/titles.json','w'))
print('ok',len(boards),'logo width',round(LW,1))
