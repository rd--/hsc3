-- tw 0011 (f0) ; this graph has 48 UGens ; c.f. scd graph which has 1950
let s o i = sinOsc AR ((mce2 (constant i) (constant i + 0.0001)) ** 2 * f o (i - 1)) (f o (i - 1) * 0.0001) * f o (i - 1)
    f o i = if i > 0 then s o i else o
in f 60 5 / 60
