-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0)
let b = mce [1..8] * 99
    o = blip AR (b / 2 + lfSaw KR ((-8) / b) 1 * 99) (b / 4 + (lfSaw KR (1 / b) 1 * 99))
    c = combN (o * sinOsc AR (8 / b) (lfSaw AR (99 / b) 0)) 0.2 0.2 1
in sin (splay c 1 1 0 True) * 0.1
