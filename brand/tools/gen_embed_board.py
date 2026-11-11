import base64,sys
F,FIN,TPL,OUT=sys.argv[1:5]
def uri(d,n): return 'data:image/svg+xml;base64,'+base64.b64encode(open(f'{d}/{n}','rb').read()).decode()
def img(d,n,h): return f'<img src="{uri(d,n)}" height="{h}" style="display:block" alt="">'
LBL="font-family:'JetBrains Mono',monospace;font-size:12px;font-weight:500;letter-spacing:.08em;text-transform:uppercase;color:#566074"
def panel(bg,body,b=True,extra=''): return f'<div style="background:{bg};{"border:1px solid #E3E6EC;" if b else ""}display:flex;align-items:center;justify-content:center;gap:40px;padding:28px;{extra}">{body}</div>'
cards=[("v1-square","1 Square Z","The stair at full cap height replaces the Z. Same proportions as the mark.","reads wide next to the narrow I; the Z reads a little like an equals sign with a tail."),
("v2-narrow","2 Narrow Z","Bars shortened to about 78% so the stair is letter-width. Closest to a real Z next to I and O.","knobs get tight; still lighter than the bold stems of I and O."),
("v3-mono","3 One colour","Narrow Z with all bars in ink, for one-colour use. Knobs still cut out.","loses the orange-red that makes the stair look like a Z; reads as three lines.")]
body=''
for k,t,says,risk in cards:
    body+=f'''<div style="display:flex;flex-direction:column;gap:12px;min-width:0">
    {panel('#FFFFFF',img(F,f'lockup-{k}-light.svg',72),True,'height:150px')}
    {panel('#101521',img(F,f'lockup-{k}-dark.svg',72),False,'height:150px')}
    {panel('#F7F8FA',img(F,f'lockup-{k}-light.svg',32)+img(F,f'lockup-{k}-light.svg',24),False,'height:72px;gap:28px')}
    <h2 style="margin:0;font-size:20px;font-weight:600">{t}</h2>
    <p style="margin:0;font-size:14px;line-height:1.55">{says}</p>
    <p style="margin:0;font-size:14px;line-height:1.55;color:#566074"><b style="color:#101521;font-weight:600">Risk:</b> {risk}</p></div>'''
src=open(TPL).read(); head=src[:src.index('<div style="width')]
H=860
html=head+f'''<div style="width: 1440px; height: {H}px; box-sizing: border-box; padding: 40px 32px; background: #FFFFFF; color: #101521; font-family: 'Sora', system-ui, sans-serif; display: flex; flex-direction: column; gap: 24px">
  <div style="display:flex;flex-direction:column;gap:8px">
    <div style="{LBL}">ZIO Config logo, temporary design, round 9</div>
    <h1 style="margin:0;font-size:38px;font-weight:600;letter-spacing:-0.02em">The Z of ZIO, drawn as the stair</h1>
    <p style="margin:0;font-size:16px;line-height:1.55;color:#566074;max-width:90ch">Temporary. The stair mark sits in the wordmark as the Z, so the logo is one piece: stair + IO + Config. Nothing in brand/final/ changes unless you accept this. Compare with the standalone mark and lockup on the earlier boards.</p>
  </div>
  <div style="display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:24px">{body}</div>
  <p style="margin:0;font-size:14px;line-height:1.55;color:#566074;max-width:100ch">My read: it works as a wordmark idea, but the stair is lighter than the bold I and O, and at cap height it reads as a Z mostly because you already know the word. Version 2 is the one to judge. If the Z is not clear enough for you, the mark beside "ZIO Config" (the current lockup) stays the safer choice.</p>
</div>
</x-dc>
<script type="text/x-dc" data-dc-script data-props='{{"$preview":{{"width":1440,"height":{H}}}}}'>
class Component extends DCLogic {{
  renderVals() {{ return {{}}; }}
}}
</script>
</body>
</html>
'''
open(OUT,'w').write(html)
