#!/usr/bin/env bash
# rasterize favicon SVGs to PNG + ICO with headless chromium. usage: rasterize.sh <brand dir> <tmp dir>
D=$(cd "$1" && pwd); T=$2; mkdir -p "$T"
while read -r src size out; do
  echo "<body style='margin:0;background:transparent'><img src='$D/$src' width=$size height=$size style='display:block'>" > "$T/p.html"
  chromium --headless --no-sandbox --disable-gpu --hide-scrollbars --default-background-color=00000000 --window-size=$size,$size --screenshot="$D/$out" "file://$T/p.html" >/dev/null 2>&1
done <<LIST
favicon-16px.svg 16 favicon-16.png
favicon-tiny.svg 32 favicon-32.png
favicon-tile.svg 48 favicon-48.png
favicon-square.svg 180 apple-touch-icon.png
favicon-square.svg 192 icon-192.png
favicon-square.svg 512 icon-512.png
icon-maskable.svg 512 icon-maskable-512.png
LIST
python3 - "$D" <<'PY'
import struct,sys
d=sys.argv[1]
png=[open(f'{d}/favicon-{n}.png','rb').read() for n in (16,32,48)]
off=6+16*3; ents=b''; data=b''
for n,p in zip((16,32,48),png):
    ents+=struct.pack('<BBBBHHII',n,n,0,0,1,32,len(p),off+len(data)); data+=p
open(f'{d}/favicon.ico','wb').write(struct.pack('<HHH',0,1,3)+ents+data)
PY

# social cards (non-square)
while read -r src w h out; do
  echo "<body style='margin:0'><img src='$D/$src' width=$w height=$h style='display:block'>" > "$T/c.html"
  chromium --headless --no-sandbox --disable-gpu --hide-scrollbars --window-size=$w,$h --screenshot="$D/$out" "file://$T/c.html" >/dev/null 2>&1
done <<LIST
social-card-light.svg 1200 630 social-card-light.png
social-card-dark.svg 1200 630 social-card-dark.png
github-social-preview.svg 1280 640 github-social-preview.png
LIST
