import base64,sys
F=sys.argv[1]; src=open(sys.argv[2]).read()
head=src[:src.index('<svg width="0"')]
def uri(n):
    mt='image/png' if n.endswith('.png') else 'image/svg+xml'
    return f'data:{mt};base64,'+base64.b64encode(open(f'{F}/{n}','rb').read()).decode()
def img(n,h=None,w=None,extra=''):
    a=(f' height="{h}"' if h else '')+(f' width="{w}"' if w else '')
    return f'<img src="{uri(n)}"{a} style="display:block;{extra}" alt="">'
LBL='font-family:\'JetBrains Mono\',monospace;font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:#566074'
def panel(bg,body,border=False,extra=''):
    b='border:1px solid #E3E6EC;' if border else ''
    return f'<div style="background:{bg};{b}display:flex;align-items:center;justify-content:center;gap:48px;padding:32px;{extra}">{body}</div>'
def cap(t): return f'<div style="font-size:13px;line-height:1.5;color:#566074">{t}</div>'
html=head+f'''<div style="width: 1440px; height: 1180px; box-sizing: border-box; padding: 40px 32px; background: #FFFFFF; color: #101521; font-family: 'Sora', system-ui, sans-serif; display: flex; flex-direction: column; gap: 24px">
  <div style="display:flex;flex-direction:column;gap:8px">
    <div style="{LBL}">ZIO Config logo, fresh start, round 7</div>
    <h1 style="margin:0;font-size:38px;font-weight:600;letter-spacing:-0.02em">Lockup and favicon</h1>
    <p style="margin:0;font-size:16px;line-height:1.55;color:#566074;max-width:90ch">Bold stair from round 6. Wordmark is Sora (ZIO bold, Config regular), converted to outlines so it needs no font. Files are in brand/final/ in the repo.</p>
  </div>
  <div style="display:grid;grid-template-columns:1fr 1fr;gap:16px">
    {panel('#FFFFFF',img('lockup-horizontal-light.svg',h=88),True,'height:200px')}
    {panel('#101521',img('lockup-horizontal-dark.svg',h=88),False,'height:200px')}
    {panel('#FFFFFF',img('lockup-stacked-light.svg',h=140),True,'height:240px')}
    {panel('#101521',img('lockup-stacked-dark.svg',h=140),False,'height:240px')}
  </div>
  <div style="display:grid;grid-template-columns:1.1fr 1fr 1fr;gap:16px">
    {panel('#FFFFFF',img('lockup-horizontal-light.svg',h=24)+img('lockup-horizontal-light.svg',h=32)+img('mark-small-light.svg',w=16)+img('mark-small-light.svg',w=24),True,'height:96px;gap:28px')}
    {panel('#101521',img('lockup-horizontal-dark.svg',h=24)+img('lockup-horizontal-dark.svg',h=32)+img('mark-small-dark.svg',w=16)+img('mark-small-dark.svg',w=24),False,'height:96px;gap:28px')}
    {panel('#F1F2F6',img('favicon-16.png')+img('favicon-32.png')+img('favicon-48.png')+img('apple-touch-icon.png',w=72)+img('favicon.svg',w=72),False,'height:96px;gap:24px')}
  </div>
  <div style="display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:24px">
    {cap('<b style="color:#101521">Lockup.</b> Mark 64 high, wordmark cap height 30, gap 20 (units of the 64 box). Stacked version centres the mark over the wordmark.')}
    {cap('<b style="color:#101521">Clear space.</b> One bar height (14 of 64 units, about a fifth of the mark) on all sides.')}
    {cap('<b style="color:#101521">Small sizes.</b> Mark alone from 16 px (small cut, bars slightly thicker); horizontal lockup from 24 px high.')}
    {cap('<b style="color:#101521">Favicon.</b> Dark tile so it works on light and dark browser chrome. 16 px is a pixel-grid version without knobs; 32 px and up keep the knobs. ICO holds 16, 32, 48; 180, 192 and 512 px PNGs are full square.')}
  </div>
</div>
</x-dc>
<script type="text/x-dc" data-dc-script data-props='{{"$preview":{{"width":1440,"height":1180}}}}'>
class Component extends DCLogic {{
  renderVals() {{ return {{}}; }}
}}
</script>
</body>
</html>
'''
open(sys.argv[3],'w').write(html)
