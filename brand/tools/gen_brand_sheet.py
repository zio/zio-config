"""Brand sheet. usage: gen_brand_sheet.py <brand/final dir> <Round dc template> <out board .dc.html> <out standalone .html>"""
import base64, sys
sys.path.insert(0, __file__.rsplit('/', 1)[0])
from wcag import ratio
F, TPL, OUT_DC, OUT_HTML = sys.argv[1:5]
def uri(n):
    mt = 'image/png' if n.endswith('.png') else 'image/svg+xml'
    return f'data:{mt};base64,' + base64.b64encode(open(f'{F}/{n}', 'rb').read()).decode()
def img(n, h=None, w=None, extra=''):
    a = (f' height="{h}"' if h else '') + (f' width="{w}"' if w else '')
    return f'<img src="{uri(n)}"{a} style="display:block;{extra}" alt="">'
INK='#101521'; ACC='#F2501F'; NIGHT='#101521'; SNOW='#F4F5F8'; ACCD='#FF6B3D'; G1='#566074'; G2='#A7AEBD'
def cr(a,b): return f'{ratio(a,b):.2f}'
LBL = "font-family:'JetBrains Mono',monospace;font-size:12px;font-weight:500;letter-spacing:.08em;text-transform:uppercase;color:#566074"
def h2(t): return f'<h2 style="margin:0;font-size:22px;font-weight:600;letter-spacing:-0.01em">{t}</h2>'
def p(t,c='#101521'): return f'<p style="margin:0;font-size:14px;line-height:1.6;color:{c}">{t}</p>'
def sec(label,title,body): return f'<section style="display:flex;flex-direction:column;gap:16px;border-top:1px solid #E3E6EC;padding-top:24px"><div style="{LBL}">{label}</div>{h2(title)}{body}</section>'
def panel(bg,body,border=True,extra=''):
    b='border:1px solid #E3E6EC;' if border else ''
    return f'<div style="background:{bg};{b}display:flex;align-items:center;justify-content:center;gap:32px;padding:28px;{extra}">{body}</div>'
def sw(name,hexv,fg,note):
    return (f'<div style="display:flex;flex-direction:column;border:1px solid #E3E6EC"><div style="background:{hexv};height:84px;display:flex;align-items:flex-end;padding:8px 10px;color:{fg};font-family:\'JetBrains Mono\',monospace;font-size:12px">{hexv}</div>'
            f'<div style="padding:10px;font-size:13px;line-height:1.5"><b style="font-weight:600">{name}</b><br><span style="color:#566074">{note}</span></div></div>')
rows=[("Ink on paper",INK,'#FFFFFF',4.5),("Accent on paper (graphic only)",ACC,'#FFFFFF',3),("Ink on accent",INK,ACC,4.5),("White on accent",'#FFFFFF',ACC,4.5),
      ("Snow on night",SNOW,NIGHT,4.5),("Dark accent on night",ACCD,NIGHT,3),("Grey text on paper",G1,'#FFFFFF',4.5),("Grey text on night",G2,NIGHT,4.5)]
def tr(n,a,b,need):
    r=ratio(a,b); ok='Pass' if r>=need else 'Fail'
    col='#1B7A3E' if ok=='Pass' else '#B3261E'
    return f'<tr><td style="padding:6px 12px 6px 0">{n}</td><td style="padding:6px 12px 6px 0;font-family:\'JetBrains Mono\',monospace">{a} on {b}</td><td style="padding:6px 12px 6px 0">{r:.2f}:1</td><td style="padding:6px 12px 6px 0">{need}:1</td><td style="padding:6px 0;color:{col};font-weight:600">{ok}</td></tr>'
table='<table style="border-collapse:collapse;font-size:13px;text-align:left"><thead><tr style="color:#566074"><th style="padding:6px 12px 6px 0;font-weight:500">Pair</th><th style="padding:6px 12px 6px 0;font-weight:500">Colours</th><th style="padding:6px 12px 6px 0;font-weight:500">Ratio</th><th style="padding:6px 12px 6px 0;font-weight:500">Need</th><th style="padding:6px 0;font-weight:500">Result</th></tr></thead><tbody>'+''.join(tr(*r) for r in rows)+'</tbody></table>'
H=88; pad=round(H*14/64,1)
clear=(f'<div style="outline:1px dashed #F2501F;outline-offset:0;padding:{pad}px;display:inline-block;background:#fff;position:relative">{img("lockup-horizontal-light.svg",h=H)}</div>')
dont=lambda t,body: f'<div style="display:flex;flex-direction:column;gap:8px">{panel("#F7F8FA",body,False,"height:110px")}<div style="font-size:13px;color:#566074;line-height:1.5"><b style="color:#B3261E;font-weight:600">Don\'t</b> {t}</div></div>'
sheet=f'''
  <div style="display:flex;flex-direction:column;gap:8px">
    <div style="{LBL}">ZIO Config brand sheet, draft 1</div>
    <h1 style="margin:0;font-size:44px;font-weight:600;letter-spacing:-0.02em">Four switches, one Z</h1>
    <p style="margin:0;font-size:16px;line-height:1.6;color:#566074;max-width:90ch">The ZIO Config mark is a stack of four settings switches. The two coloured ones step across so the stack reads as a Z. It says configuration (switches you set) and ZIO (the Z) with no gear and no letter. This is a design draft for Milad to review, not an adopted brand.</p>
  </div>
  {sec('1 Mark','The mark, light and dark',f"""<div style="display:grid;grid-template-columns:1fr 1fr;gap:16px">{panel('#FFFFFF',img('mark-light.svg',h=200),True,'height:280px')}{panel(NIGHT,img('mark-dark.svg',h=200),False,'height:280px')}</div>
  {p('Four pill-shaped bars, 14 units tall with a 2 unit gap, in a 64 unit box. Top and bottom bars are ink; the two middle bars are the accent and step from right to left. Each bar has a round knob that is a real hole, so the mark works on any background. The small cut (16 to 23 px) has bars 14.5 units tall.')}""")}
  {sec('2 Colour','Ink plus one accent',f"""<div style="display:grid;grid-template-columns:repeat(6,minmax(0,1fr));gap:12px">
    {sw('Ink',INK,'#fff','Mark and text on light')}{sw('Accent',ACC,'#fff','Middle bars, light')}{sw('Paper','#FFFFFF','#101521','Light background')}
    {sw('Snow',SNOW,'#101521','Mark and text on dark')}{sw('Accent, dark',ACCD,'#101521','Middle bars, dark')}{sw('Night',NIGHT,'#fff','Dark background')}</div>
  {table}
  {p(f'<b style="font-weight:600">Rule that follows:</b> the accent on white is {cr(ACC,"#FFFFFF")}:1. That is fine for the mark (a graphic needs 3:1) but too low for text, and white text on the accent fails too. Use ink on the accent ({cr(INK,ACC)}:1) and never set body text in the accent on white.')}""")}
  {sec('3 Type','Sora for the name, outlines in the files',f"""<div style="display:flex;gap:48px;align-items:baseline;flex-wrap:wrap"><div style="font-size:56px;letter-spacing:-0.01em"><b style="font-weight:700">ZIO</b> <span style="font-weight:400">Config</span></div><div style="font-size:20px;color:#566074">Configuration for ZIO applications</div></div>
  {p('The wordmark is Sora, ZIO in bold (700) and Config in regular (400), converted to outlines in every logo file so no font is needed. Docs and slides can use Sora for headings and JetBrains Mono for code and small labels.')}""")}
  {sec('4 Lockups','Horizontal and stacked',f"""<div style="display:grid;grid-template-columns:1fr 1fr;gap:16px">{panel('#FFFFFF',img('lockup-horizontal-light.svg',h=72),True,'height:190px')}{panel(NIGHT,img('lockup-horizontal-dark.svg',h=72),False,'height:190px')}{panel('#FFFFFF',img('lockup-stacked-light.svg',h=120),True,'height:230px')}{panel(NIGHT,img('lockup-stacked-dark.svg',h=120),False,'height:230px')}</div>
  <div style="display:flex;gap:32px;align-items:center;flex-wrap:wrap"><div>{clear}</div><div style="max-width:60ch;display:flex;flex-direction:column;gap:8px">{p('<b style="font-weight:600">Clear space</b> is one bar height (14 of 64 units, about a fifth of the mark) on every side, shown by the dashed box.')}{p('<b style="font-weight:600">Minimum size:</b> the mark alone down to 16 px (use the small cut below 24 px); the horizontal lockup down to 24 px high. The stacked lockup is for square spaces; use the horizontal one in a header.')}</div></div>""")}
  {sec('5 Icons','Favicon and app icons',f"""<div style="display:flex;gap:28px;align-items:flex-end;flex-wrap:wrap;background:#F1F2F6;padding:24px">{img('favicon-16.png')}{img('favicon-32.png')}{img('favicon-48.png')}{img('apple-touch-icon.png',w=90)}{img('icon-maskable-512.png',w=90)}{img('favicon.svg',w=90)}</div>
  {p('Dark tile so it works on light and dark browser chrome. 16 px is a hand-built pixel-grid version without knobs (2 px bars, 1 px gaps); 32 px and up keep the knobs. Files: favicon.svg, favicon.ico (16, 32, 48), apple-touch-icon.png (180), icon-192.png, icon-512.png, and icon-maskable-512.png, which keeps the mark inside the central 80% safe circle for Android adaptive icons.')}""")}
  {sec('6 Social','Link preview and GitHub card',f"""<div style="display:grid;grid-template-columns:1fr 1fr;gap:16px">{img('social-card-light.png',w=None,extra='width:100%;border:1px solid #E3E6EC')}{img('social-card-dark.png',extra='width:100%')}</div>
  {p('Open Graph and Twitter cards are 1200 by 630; the GitHub social preview is 1280 by 640 (dark, same layout). Text is outlined. The tagline "Configuration for ZIO applications" and the zio.dev/zio-config address are placeholders until Milad confirms the wording.')}""")}
  {sec('7 Usage','Do not',f"""<div style="display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:16px">
  {dont('recolour the bars. Only the ink and accent pairs above.',"<svg width='120' height='80' viewBox='0 0 64 64'><rect x='3' y='1' width='58' height='14' rx='7' fill='#2E9E5B'/><rect x='22' y='17' width='39' height='14' rx='7' fill='#7A3FD1'/><rect x='3' y='33' width='39' height='14' rx='7' fill='#D1A33F'/><rect x='3' y='49' width='58' height='14' rx='7' fill='#2E9E5B'/></svg>")}
  {dont('rotate, skew or flip the mark. The stair only reads as a Z one way.',f"<div style='transform:scaleX(-1)'>{img('mark-light.svg',h=80)}</div>")}
  {dont('set text in the accent on white. It is 3.53:1.',"<div style='font-size:30px;font-weight:700;color:#F2501F;font-family:Sora,sans-serif'>ZIO Config</div>")}
  {dont('add shadows, gradients or outlines.',f"<div style='filter:drop-shadow(4px 6px 3px rgba(0,0,0,.45))'>{img('mark-light.svg',h=80)}</div>")}</div>""")}
'''
standalone=f'''<!doctype html><html lang="en"><head><meta charset="utf-8"><title>ZIO Config brand sheet</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Sora:wght@400;600;700&family=JetBrains+Mono:wght@400;500&display=swap">
<style>body{{margin:0}}</style></head><body><div style="width:1440px;box-sizing:border-box;padding:40px 32px 56px;background:#FFFFFF;color:#101521;font-family:'Sora',system-ui,sans-serif;display:flex;flex-direction:column;gap:32px">{sheet}</div></body></html>'''
open(OUT_HTML,'w').write(standalone)
src=open(TPL).read(); head=src[:src.index('<div style="width')]
H_=3760
board=head+f'''<div style="width: 1440px; height: {H_}px; box-sizing: border-box; padding: 40px 32px 56px; background: #FFFFFF; color: #101521; font-family: 'Sora', system-ui, sans-serif; display: flex; flex-direction: column; gap: 32px">{sheet}</div>
</x-dc>
<script type="text/x-dc" data-dc-script data-props='{{"$preview":{{"width":1440,"height":{H_}}}}}'>
class Component extends DCLogic {{
  renderVals() {{ return {{}}; }}
}}
</script>
</body>
</html>
'''
open(OUT_DC,'w').write(board)
