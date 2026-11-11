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
