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
