import base64,sys
F,FIN,TPL,OUT=sys.argv[1:5]
def uri(d,n): return 'data:image/svg+xml;base64,'+base64.b64encode(open(f'{d}/{n}','rb').read()).decode()
def img(d,n,h): return f'<img src="{uri(d,n)}" height="{h}" style="display:block" alt="">'
LBL="font-family:'JetBrains Mono',monospace;font-size:12px;font-weight:500;letter-spacing:.08em;text-transform:uppercase;color:#566074"
def panel(bg,body,b=True,extra=''): return f'<div style="background:{bg};{"border:1px solid #E3E6EC;" if b else ""}display:flex;align-items:center;justify-content:center;gap:40px;padding:28px;{extra}">{body}</div>'
cards=[("w4-steep-square","1 Steep, square ends","No knobs. Bars the same weight as the I stem, middle bars step far apart so the diagonal shows, ends only slightly rounded. Clearest Z in the word.","the toggle knobs are gone from the wordmark, so the switch idea is carried only by the pill-like bars and the standalone mark."),
("w5-steep-round","2 Steep, round ends","Same steps, fully round pill ends, so it still looks like switches without knobs.","round ends make the Z a little softer and less letter-like than version 1."),
("w3-square-ends","3 Shallow, square ends","The previous step distance with square ends, for comparison.","middle bars overlap more, so the diagonal is weaker and it reads as lines again.")]
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
    <div style="{LBL}">ZIO Config logo, temporary design, round 10</div>
    <h1 style="margin:0;font-size:38px;font-weight:600;letter-spacing:-0.02em">Z of ZIO, simpler and steeper</h1>
    <p style="margin:0;font-size:16px;line-height:1.55;color:#566074;max-width:90ch">You said the Z details were not distinguishable inside the full logo. Cause: knobs and thin steps are too small at letter size. Fixes here: no knobs in the wordmark, bar thickness matched to the I stem (6.74 vs 6.77 units), and a larger step between the two orange bars.</p>
  </div>
  <div style="display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:24px">{body}</div>
  <p style="margin:0;font-size:14px;line-height:1.55;color:#566074;max-width:100ch">My read: version 1 is the clearest Z. Trade-off: the knobs, which make the standalone mark feel like toggles, only live in the mark. If you accept this I would use the full stair (with knobs) as the icon and favicon, and this simpler Z only in the wordmark.</p>
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
