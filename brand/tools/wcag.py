#!/usr/bin/env python3
"""WCAG 2.x contrast. Usage: wcag.py  (prints the ZIO Config palette table)."""
def lum(h):
    h = h.lstrip('#'); r, g, b = (int(h[i:i+2], 16) / 255 for i in (0, 2, 4))
    f = lambda c: c / 12.92 if c <= 0.03928 else ((c + 0.055) / 1.055) ** 2.4
    return 0.2126 * f(r) + 0.7152 * f(g) + 0.0722 * f(b)
def ratio(a, b):
    la, lb = lum(a), lum(b)
    if la < lb: la, lb = lb, la
    return (la + 0.05) / (lb + 0.05)
if __name__ == '__main__':
    import sys; sys.path.insert(0, __file__.rsplit('/', 1)[0])
    from marks import PAL, PAIRS
    for mode in ('light', 'dark'):
        p = PAL[mode]
        print(f'## {mode}')
        for fg, bg, need, note in PAIRS:
            r = ratio(p[fg], p[bg]); ok = 'Pass' if r >= need else 'FAIL'
            print(f'| {fg} {p[fg]} on {bg} {p[bg]} | {r:.2f} | need {need} | {ok} | {note} |')
