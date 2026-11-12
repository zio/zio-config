# ZIO Config brand

Final identity: the Z of ZIO is a stack of three pill switches, each with a round knob (ink top and bottom, orange-red middle). Sora wordmark, JetBrains Mono for code.

## Files
- `logo-light.svg`, `logo-dark.svg`: full logo, text outlined.
- `mark-*.svg`, `mark-small-*.svg`: symbol (small cut for sizes under 32 px).
- `favicon.svg`: tab icon, no tile, follows the browser theme. `favicon-tile.svg`, `favicon-tiny.svg`, `favicon-16px.svg`, `favicon-square.svg`, `favicon.ico`, `favicon-16/32/48.png`: tiled versions.
- `apple-touch-icon.png`, `icon-192.png`, `icon-512.png`, `icon-maskable.svg`, `icon-maskable-512.png`: app icons.
- `social-card-light|dark.svg/.png` (1200x630), `github-social-preview.svg/.png` (1280x640).
- `brand-sheet.html/.png`: one-page overview. `sheet/`: the eight brand-sheet boards (logo, icons, colour, type, voice, imagery, hero, social).

## Palette
Ink/Night #101521, orange-red #F2501F (light) and #FF6B3D (dark), Paper #FFFFFF, Snow #F4F5F8, Slate #566074, Mist #A7AEBD. Orange-red on white is 3.53:1: graphics only, text uses #A93816.

## Rebuild
Needs Python with `fonttools` and `pillow`, the Sora variable font, and headless Chromium.
1. `python tools/build_final.py <Sora.ttf> .` builds the SVGs.
2. `bash tools/rasterize.sh . <tmp dir>` builds the PNGs and `favicon.ico`.
3. `python tools/gen_sheet_boards.py <Sora.ttf> <outdir> [heights.json]` builds the eight boards.
4. `python tools/gen_brand_sheet.py . <overview template .dc.html> <out board> <out html>` builds the overview (needs the design-canvas template board).

Tagline, web address and sample copy are placeholders for the maintainers to confirm.
