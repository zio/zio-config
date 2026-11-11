#!/usr/bin/env python3
"""ZIO Config round 1: five concept marks (24-unit grid) and the draft palette."""
PAL = {
 'light': dict(page='#FFFFFF', panel='#F7F3F8', text='#1D1423', text2='#5E5266',
               brand='#4E0A4A', accent='#587000', ghost='#F3ECF4'),
 'dark':  dict(page='#15101A', panel='#1F1826', text='#F1EBF4', text2='#AFA3B8',
               brand='#BD66B1', accent='#C4F03A', ghost='#241B2C'),
}
# (fg, bg, need, note)  text pairs 4.5, graphics 3.0
PAIRS = [
 ('text','page',4.5,'body text'), ('text','panel',4.5,'text on panel'),
 ('text2','page',4.5,'secondary text'), ('text2','panel',4.5,'secondary on panel'),
 ('brand','page',4.5,'brand colour as text/links'), ('brand','panel',4.5,'brand on panel'),
 ('accent','page',4.5,'accent as text'), ('accent','panel',4.5,'accent on panel'),
 ('page','brand',4.5,'button text on brand'), ('page','accent',4.5,'button text on accent'),
 ('accent','brand',3.0,'accent against brand inside mark (graphics)'),
]

def svg(body, vb=24, c=None, defs=''):
    return f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {vb} {vb}" role="img" aria-label="ZIO Config">{defs}{body}</svg>'

def A_switch(b, a, small=False):
    # pill 24x12 with knob; small cut on 16 grid: pill 16x10, knob r3
    if small:
        return svg(f'<rect x="0" y="3" width="16" height="10" rx="5" fill="{b}"/><circle cx="11" cy="8" r="3" fill="{a}"/>', 16)
    return svg(f'<rect x="0" y="5" width="24" height="14" rx="7" fill="{b}"/><circle cx="16.5" cy="12" r="4.5" fill="{a}"/>')

def B_keyvalue(b, a, small=False):
    if small:
        return svg(f'<circle cx="4" cy="8" r="4" fill="{a}"/><rect x="9" y="3.5" width="7" height="4" fill="{b}"/><rect x="9" y="8.5" width="7" height="4" fill="{b}"/>', 16)
    return svg(f'<circle cx="6" cy="12" r="6" fill="{a}"/><rect x="14" y="5" width="10" height="6" fill="{b}"/><rect x="14" y="13" width="10" height="6" fill="{b}"/>')

def C_cascade(b, a, small=False):
    # three layers: default, environment, override (front, accent); 1.5-unit knockout via mask
    if small:
        defs = '<defs><mask id="m1"><rect width="16" height="16" fill="#fff"/><rect x="5" y="-1" width="12" height="12" fill="#000"/></mask><mask id="m2"><rect width="16" height="16" fill="#fff"/><rect x="5" y="-1" width="12" height="12" fill="#000"/></mask></defs>'
        return svg(f'<rect x="0" y="6" width="10" height="10" fill="{b}" mask="url(#m1)"/><rect x="3" y="3" width="10" height="10" fill="{b}" mask="url(#m2)"/><rect x="6" y="0" width="10" height="10" fill="{a}"/>', 16, defs=defs)
    defs = ('<defs><mask id="m1"><rect width="24" height="24" fill="#fff"/><polygon points="4.5,2.5 24,2.5 24,24 4.5,24" fill="#000"/></mask>'
            '<mask id="m2"><rect width="24" height="24" fill="#fff"/><rect x="6.5" y="-2" width="20" height="20" fill="#000"/></mask></defs>')
    return svg(f'<rect x="0" y="8" width="16" height="16" fill="{b}" mask="url(#m1)"/>'
               f'<rect x="4" y="4" width="16" height="16" fill="{b}" mask="url(#m2)"/>'
               f'<rect x="8" y="0" width="16" height="16" fill="{a}"/>')

def D_branch(b, a, small=False):
    if small:
        return svg(f'<g stroke="{b}" stroke-width="2.4" fill="none" stroke-linecap="butt"><path d="M8 3.5 L2.5 12.5"/><path d="M8 3.5 V12.5"/><path d="M8 3.5 L13.5 12.5"/></g>'
                   f'<circle cx="8" cy="3.5" r="3.2" fill="{b}"/><circle cx="2.5" cy="12.5" r="2.5" fill="{b}"/><circle cx="8" cy="12.5" r="2.5" fill="{b}"/><circle cx="13.5" cy="12.5" r="2.5" fill="{a}"/>', 16)
    return svg(f'<g stroke="{b}" stroke-width="2.6" fill="none"><path d="M12 5 L3.5 19"/><path d="M12 5 V19"/><path d="M12 5 L20.5 19"/></g>'
               f'<circle cx="12" cy="5" r="4" fill="{b}"/><circle cx="3.5" cy="19" r="3.5" fill="{b}"/><circle cx="12" cy="19" r="3.5" fill="{b}"/><circle cx="20.5" cy="19" r="3.5" fill="{a}"/>')

def E_foldz(b, a, small=False):
    if small:
        return svg(f'<polygon points="4,0 16,0 16,4 8,12 16,12 12,16 0,16 0,12 8,4 0,4" fill="{b}"/><polygon points="8,4 16,4 8,12 0,12" fill="{a}"/>'.replace('points="4,0 16,0 16,4 8,12 16,12 12,16 0,16 0,12 8,4 0,4"','points="4,0 16,0 16,4 8,12 16,12 12,16 0,16 0,12 8,4 0,4"'), 16)
    return svg(f'<polygon points="6,0 24,0 24,6 12,18 24,18 18,24 0,24 0,18 12,6 0,6" fill="{b}"/><polygon points="12,6 24,6 12,18 0,18" fill="{a}"/>')

CONCEPTS = [
 ('A', 'Switch',   A_switch),
 ('B', 'Key = value', B_keyvalue),
 ('C', 'Cascade',  C_cascade),
 ('D', 'Branch',   D_branch),
 ('E', 'Fold Z',   E_foldz),
]
