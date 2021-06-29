-- http://www.fredrikolofsson.com/f0blog/?q=node/617 (f0)
let b = mce [1..8] * 99
    o = blip ar (b / 2 + lfSaw kr ((-8) / b) 1 * 99) (b / 4 + (lfSaw kr (1 / b) 1 * 99))
    c = combN (o * sinOsc ar (8 / b) (lfSaw ar (99 / b) 0)) 0.2 0.2 1
in sin (splay c 1 1 0 True) * 0.1
