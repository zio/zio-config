# ZIO Config logo: working notes

Branch `zio-config-logo`, base `cff7f00` (origin/series/4.x). Draft only, no PR. Brief from session brand-family-rules-7a.

## Round 1 (2026-11-11): five concepts

Palette draft (hue chosen to be unused in the family): Plum (brand colour, carries the mark) + Lime (accent). Type: Sora + DM Mono (no other family uses them).
Family hues in use: terracotta (Prelude), mint (HTTP), ultramarine (Blocks), amber (Logging).

| | Concept | Idea | Accent element | Risk |
| --- | --- | --- | --- | --- |
| A | Switch | pill track and knob: a setting turned on | knob | generic UI toggle; pill is only 10 px tall at 16 px |
| B | Key = value | dot (key) and two bars (value) | the key dot | two horizontal bars, nearest to Logging lines; wide, short, low presence |
| C | Cascade | default, environment, override layers; front layer overrides | front layer | layers merge below 24 px; nearest rival is Blocks |
| D | Branch | root key with three child keys (config tree) | one leaf | thin strokes at 16 px; reads as molecule or network |
| E | Fold Z | Z ribbon with slanted ends; diagonal is the override path | diagonal | nearest to Prelude (Z with accent) and Blocks; only Z option |

Contrast: `round-1/contrast.md` (tools/wcag.py). All text pairs pass 4.5:1 in both modes.
Deviation: accent against brand inside the mark is below 3:1 (light 2.55, dark 2.79). Both are saturated mid tones, so the pieces separate by hue. Marks have no WCAG graphics requirement for logos, but flag for Milad.
Size test: `round-1/sizes.png` (16, 24, 32, 96 px; light and dark; small cut under 24 px).
Maskable arithmetic: safe circle 80% of 512 = 409.6 diameter. Square mark max side = 409.6 / sqrt(2) = 289.6 (56.6%). Pill A (24 x 14 units): half-diagonal 13.89 units, scale 204.8 / 13.89 = 14.74, width 353.8 = 69.1% of 512.

Regenerate: `python3 tools/render.py round-1` then chromium headless screenshot of `round-1/sizes.html`.

Artifact (private, Design canvas): https://claude.ai/artifact/PAj3a7pVj4Hx21ABgVTBE1 . Sources in `canvas/`.
Note: the artifact draws C with polygon layers; `round-1/` PNG draws it with masks. Same idea, not pixel-identical, and the artifact was not rendered by me (the type forbids verifying).

## Round 1b: colour (Milad: "the colouring is awful")

Plum and lime rejected. Four replacement palettes on the `Palettes` board (see `canvas/Palettes.dc.html`): 1 violet + sky, 2 raspberry + ink, 3 graphite + cyan, 4 petrol + pink. Text contrast of brand and accent on page passes 4.5:1 in all (5.77 to 17.85 light, 7.64 to 16.36 dark values in the board). Accent vs brand inside the mark is under 3:1 in all four (hue separation only). Awaiting pick.

## Round 2 (Milad: shapes too generic, too cold, want a Z; colour: bright and playful)

Rejected: plum/lime and the four round-1b palettes; round-1 shapes (switch, key=value, cascade, branch, fold Z).
New: four Z concepts, soft and chunky: 1 Bubble Z (single stroke, no accent), 2 Two-tone Z (top brand, bottom accent), 3 Node Z (path through four dots, accent last node), 4 Chunky Z (solid, softened corners, no accent). Three bright palettes: Bubblegum (pink #E6177F / #FF5BA8, cyan accent), Grape (violet #6D2FEA / #A98BFF, pink accent), Cyan pop (cyan #0098CC / #3DD6FA, violet accent). Mark colours are graphics (3:1 on page, all pass: 3.30 to 10.96); link/button text uses a deeper step (pink #B3125F 6.63, violet #5B21C9 8.38, cyan #00718F 5.59 on white). Accent vs brand 1.24 to 1.70 (hue only).
Size test: `round-2/sizes.png` (16, 24, 32, 96; the 96 px Node and Chunky columns are squeezed by the test row's flex layout, a test artefact). Findings: Bubble and Chunky hold at 16 px; Two-tone holds, both colours visible; Node is cluttered below 32 px.
Artifact board `Round2.dc.html`.

## Round 3 (Milad: "run a new round with an empty new page")

Read as: a fresh board with none of the earlier concepts. Board `Round3.dc.html`.
Concepts (all Z): 1 Plug Z (cable ending in a plug, accent plug), 2 Barred Z (crossbar accent), 3 Stitch Z (stitched diagonal, accent middle stitch), 4 Italic Z (single colour). An earlier Swash Z was dropped after my render showed it reads as a "2".
Palettes: Poppy (red #E5242F / #FF6070, accent ink #1A1523 / #F3EFF8; link text #B3141F) and Orchid (fuchsia #C026D3 / #E879F9, accent teal #00606A / #2DD4E0; link text #9A1AAB). Mark vs page: 4.54, 6.43, 4.71, 7.67. Accent vs brand: Poppy 3.93 / 2.59, Orchid 1.55 / 1.36.
Size render: `round-3/sizes.png` (16, 24, 32, 64). Plug and Italic hold at 16 px; Barred and Stitch blur their accent at 16 px.

## Fresh start (Milad: earlier context "was not good", all designs awful; "start from zero without any predefined context")

Dropped: the family brand rules, the Logging precedent, the palette and font choices of rounds 1 to 3. New artifact (private): https://claude.ai/artifact/1SUC4aKVttnyZUbGLeTfvm , sources in `canvas-fresh/`.
Five directions from what the library does (describes config as values, reads many sources, merges them): 1 Dial, 2 Braces (Z between curly braces), 3 Layers, 4 z. (lowercase z with a full stop, from zio.config), 5 Switch board (5x5 dots, the set ones draw a Z). Neutral colour only to judge shape: ink #101521 and orange-red #F2501F (3.53:1 on white) / #FF6B3D (6.45:1 on ink). Not rendered by me before publishing (design canvas type forbids it).

## Fresh start, round 2 (request relayed from Milad: concepts much closer to settings and configuration)

Board `canvas-fresh/Round2.dc.html` in https://claude.ai/artifact/1SUC4aKVttnyZUbGLeTfvm . Five concepts, each tied to what ZIO Config does: 1 Merge (three sources join into one value), 2 Override (hollow default ring, solid overriding knob), 3 Slider Z (two sliders form a Z), 4 Gear Z (settings gear with a Z cut out), 5 Config file (page of key = value rows). Validation is not shown yet.
Render check (`fresh-r2/sizes.png`, 16/24/32/64, ink, colour, light, dark): Merge, Gear Z and Config file read at 16 px; Override reads but the track is faint; Slider Z is messy at 16 px.

## Fresh start, round 3 (all of round 2 rejected, no reason given)
Five concrete "settings you can point at" concepts, rounder and chunkier, no gear: DIP switch, Toggles, Nested keys, Equals (key = value), Validated rows (checks plus one rejected value, covers validation). Same ink + orange-red colourways.
Render check (brand/fresh-r3/sizes.png, 16/24/32/64/96): Equals and Validated read best; Toggles at 16 px collapse to stacked bars (Z lost); DIP slots tiny at 16 px; Nested reads as plain bars.
Artifact page not rendered (design canvas rule). Asked Milad for one dislike to aim next round.

## Fresh start, round 4: Toggles drawn as a Z
Milad picked round-3 concept 2 (Toggles) and asked for a Z shape. Top and bottom switches are the Z bars, a diagonal joins them, knobs at both turns plus middle. Three colourings (Z1 ink pills + colour diagonal, Z2 all ink + colour knobs, Z3 colour pills + ink diagonal).
Render (brand/fresh-r4/sizes.png, 16-96 px): all three read as Z at every size; Z2 (colour knobs) cleanest at 16 px; Z1 diagonal knob fiddly at 16 px.

## Fresh start, round 5: toggles, all horizontal
Milad: no diagonal line, every line horizontal, must still feel like toggles. Three options: 1 Stair (4 switches, two short ones step right to left), 2 Three switches (short coloured middle, knobs trace diagonal), 3 Fine stair (5 switches). Round white knobs on every pill; ends ink, middles colour.
Render (brand/fresh-r5/sizes.png): all read as toggles at 32+; stair gives the best Z; Three switches is plainest, Z weak at 16 px; Fine stair busy at 24 px and below.

## Fresh start, round 6: the Stair, bolder and simpler
Milad chose Stair from round 5; message cut off at "make the bars", read as "thicker". Three versions: S1 bold stair (4 knobs), S2 knobs on end bars only, S3 one colour with cut-out knobs. Bars h14, gap 2 (small cut 14.5).
Render (brand/fresh-r6/sizes.png): all read as Z + toggles at 24+; S3 simplest/most favicon-ready; S2 fewer parts but middle bars stop looking like switches.

## Fresh start, round 7: chosen mark, lockup, favicon
Milad chose Bold stair (round 6 version 1). Built in brand/final/ by brand/tools/build_final.py (needs fontTools + Sora variable TTF) and brand/tools/rasterize.sh (chromium).
- Mark: 4 pills h14 gap 2 in a 64 box, knobs are real mask holes (work on any background). Light: ink #101521 + #F2501F. Dark: #F4F5F8 + #FF6B3D. Small cut (16-23 px): bars h14.5.
- Wordmark: Sora, ZIO 700 + Config 400, outlined. Horizontal lockup: mark 64, cap height 30, gap 20. Stacked: centred.
- Clear space: one bar height (14/64 of the mark). Min: mark 16 px, horizontal lockup 24 px high.
- Favicon: dark tile; favicon.svg (tile r14, mark scale .8), favicon-square.svg (full bleed for apple-touch 180, 192, 512). 16 px is a hand pixel-grid version without knobs (bars 2 px, gap 1 px) because the knobs and 4 bars blur at 16 px with plain scaling. ICO = 16/32/48 PNGs.
- Not done yet: social cards, brand sheet, maskable safe-zone check, final contrast review of dark tile accent.

## Fresh start, round 8: social cards, maskable icon, brand sheet
Added to brand/final/: social-card-light/dark (1200x630, svg+png), github-social-preview (1280x640, dark), icon-maskable (mark scaled .58 inside the 80% safe circle). Brand sheet: brand/final/brand-sheet.html + .png, also board Round8 in the artifact (brand/tools/gen_brand_sheet.py).
Contrast (brand/tools/wcag.py): ink on white 18.24; accent #F2501F on white 3.53 (graphic only, fails text); white on accent 3.53 FAIL for text; ink on accent 5.16; snow on night 16.73; dark accent on night 6.45; grey #566074 on white 6.32; grey #A7AEBD on night 8.19. Rule: never set text in the accent on white.
Placeholders needing Milad: tagline "Configuration for ZIO applications" and URL zio.dev/zio-config.
Verified by rendering the standalone sheet and cards (not the artifact page). Still open: sheet is a draft, no PR.

## Fresh start, round 9: Z of ZIO as the stair (TEMPORARY, not accepted)
Milad asked to embed the Z shape in ZIO as a temporary design; if accepted, switch to it. Files in brand/embed/ (brand/tools/build_embed.py), board Round9. brand/final/ is untouched.
V1 square stair as Z, V2 narrow (78% bar length, letter width), V3 one colour. Rendered at 200/80/32/24 px high. Finding: reads as ZIO Config in context, but stair is lighter than bold I/O stems and the Z identity depends on the word; V2 best. Safer fallback = current lockup (mark beside wordmark).

## Fresh start, round 11: rebuild around the stair Z (accepted by Milad)
Milad accepted round-10 version 1 (steep stair, square ends, no knobs) and asked to rebuild everything.
- Logo: logo-light/dark.svg, Z = 4 bars (thickness = Sora Bold I stem), IO bold, Config regular, outlined. Replaces lockup-horizontal/stacked (removed; stacked dropped because two stairs would repeat). Tight viewBox, cap 30.
- Symbol (icons): same steep stair, 4 pills with knobs (mark-*.svg, favicon set, maskable). Mark geometry changed from shallow (22/42) to steep (30/34) to match the wordmark. 16 px favicon rows updated to match.
- Social cards + GitHub preview rebuilt with the logo (cap 96). Brand sheet rebuilt (Round11 board, brand/final/brand-sheet.html/.png). brand/embed temp files removed.
- Clear space = one bar height (about 1/4 cap height). Min logo 20 px high (checked in render at 48/32/24/20); symbol from 16 px.
- Contrast table unchanged: accent on white 3.53 (graphic only), ink on accent 5.16.
- Open: tagline and URL still placeholders; no PR.

## Round 12: knobs on every shape (Milad: discrepancies, all logo shapes must have circles)
Wordmark Z now has a round knob (mask hole) at one end of each bar, same sides as the symbol (rows 1-2 right, rows 3-4 left; ring = 12% of bar height, same ratio as symbol). 16 px favicon rebuilt: 3 px bars, 1 px gaps, 1 px dot per bar. Social cards and brand sheet regenerated.
Render: knobs clear from about 48 px logo height; at 32 px still visible; at 24 px and below muddy. Min logo size raised from 20 to 32 px high (symbol below that). Earlier finding stands: knobs are small at letter size, accepted trade-off.

## Round 13: Final brand sheet, "1 Square Z" only
Milad asked for a new page "Final brand sheet" using only "1 Square Z" as the logo. His quoted name matches round 9 card "1 Square Z" (full-width stair with knobs, same as the symbol), NOT the round-10 narrow steep Z I had used in rounds 11-12 (that explains his "discrepancies"). Rebuilt everything on it:
- Z letter = the symbol at cap height (shallow steps 22/42, full width 58 units, round ends, knobs). Same geometry as mark-*.svg, favicon, icons, so one shape everywhere. Bar thickness still matches the Sora Bold I stem (6.77 vs 6.74).
- 16 px favicon rows back to shallow steps (5..15, 1..11). Logo/social cards/sheet regenerated. brand/final/ overwritten (old narrow-Z version is in git history, commits 87989e0 and e914eaf).
- New board Round12 "Final brand sheet" (only this logo). Older boards (including Round11 with the narrow Z) left as history.
- Render-checked logo at 130/64/48/32 px, 16 px favicon at 6x, full sheet. Min logo 32 px high kept. Tagline and URL still placeholders. No PR.

Board order change: Final brand sheet (Round12) moved to the top-left of the canvas (y=0), all other boards shifted down 4080 px and keep their order below it. Reason: at y=16820 Milad could not find it.

Pages: Milad added pages in the canvas UI (page-1, page-32ozgn "Final Brand Sheet" with an empty handmade Artboard-vpum). He then asked for the final sheet on page-1b076f29e50b, an id not in the saved file, so I added that page and set Round12 "page" to it; other boards back to their original y (undid my 4080 shift), kept his Artboard-vpum entry and pages untouched.

## Round 14: complete brand sheet (8 boards), modelled on the ZIO Prelude sheet
Milad pointed to a ZIO Prelude brand sheet (page "Brand sheet": 1 Logo, 2 Icon and favicon, 3 Colour, 4 Typography, 5 Voice and tone, 6 Visual and imagery style, 7 Hero banner, 8 Social cards; 1480 wide, mono kicker, 64 px H1, numbered 2 px-ruled sections, light/dark tile pairs). Built the same eight boards for ZIO Config on page-1b076f29e50b below the one-page overview (Round12, retitled "Overview").
Generator: brand/tools/gen_sheet_boards.py (needs fontTools; outputs BrandN*.dc.html + plain preview html used for self-review and height measurement). Sources in brand/sheet/, previews rendered per board and checked section by section; fixed: use/viewBox double scaling, logo overflow in tiles, tab mock overflow, dark diagram labels, social ghost mark overlapping text.
Content notes: palette ink #101521, orange-red #F2501F / light #FF6B3D, paper, snow, slate #566074, mist #A7AEBD, orange-red 700 #A93816 for text on light (6.43:1); Sora + JetBrains Mono; accent not for text on white (3.53:1). Voice copy, taglines, hero headline/subline and the code snippet are placeholders/illustrative for maintainers to confirm. Not verified: the artifact boards themselves (design-canvas rule); verified via equivalent plain-HTML renders.

Social cards: project URL moved to the bottom-right (right edge 96 px, same margin as the logo on the left). Changed in the Brand8Social board, brand/final social-card-*.svg/png and github-social-preview (build_final.py), and the overview board Round12 that embeds them.

Social cards v2 (modelled on zio-blocks-social-github.svg: logo, bright tagline, supporting line, caption + short accent rule + source list, address bottom-right, big faint cropped symbol at the right). Lines grounded in the repo README: "Type-safe configuration for ZIO applications" (tagline, still a placeholder wording), "Automatic derivation, validation and generated docs" (README key features), "Reads configuration from many sources" + "HOCON · JSON · YAML · TOML · XML · Maps" (README source list), zio.dev/zio-config (README link). Text outlined in the SVGs (Sora SemiBold/Regular). Updated build_final.py card(), gen_sheet_boards.py social_card, Brand8Social board (h 2619), overview board Round12 (embeds PNGs). Ghost symbol overlaps the end of the tagline slightly at 6-9% opacity, as in the reference.

## Tab favicon redo
- Problem: dark tile vanished on dark tabs, 4 bars with 1 px gaps and 1 px knob dots read as noise at 16 px.
- `favicon.svg` is now tab-only: no tile, 3 pill bars (4 px thick, 2 px gaps) filling the 16 px box, 2 px round knob holes (R, L, R), ink/orange-red on light tabs and snow/light orange-red on dark tabs via `prefers-color-scheme` inside the SVG. Safari ignores SVG favicons and uses apple-touch-icon / ICO.
- Old tile version renamed `favicon-tile.svg` (feeds favicon-48.png). ICO, apple-touch-icon and maskable keep the tile.
- Headless Chromium does not honour a forced dark scheme for `<img>` SVGs, so dark colours were checked with a copy having the dark values substituted, not through the media query.

## Round 13: bar count comparison
- `tools/gen_compare.py` -> board Round13 (page-1b076f29e50b, right of the overview): A four bars (current), B three bars centred middle, C three bars middle hangs from right. Symbol large, 16-64 px, lockup at 36 and 20 px, light and dark.
- Per Milad: the knob of the middle bar sits at the centre of the bar in B and C.
- No decision yet; logo, icons, favicon and sheet are unchanged.
