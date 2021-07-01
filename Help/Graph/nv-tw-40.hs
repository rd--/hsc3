-- http://sccode.org/1-V (nv) [Line 40]
let nd i =
        let t = (0.6 ** i) * 40 * impulse ar ((2 ** i) / 32) (1/2)
            f = (4 ** lfNoise0 kr (1/16)) * 300
        in sin (rlpf t f 5e-3)
    x = splay (mceFill 8 nd) 1 1 0 True
    r u = let (p,q) = mce2c u in freeVerb2 p q 0.1 1 1
in r (r x)

-- http://sccode.org/1-V (nv) [Line 40] ; id
let nd z =
        let i = constant z
            t = (0.6 ** i) * 40 * impulse ar ((2 ** i) / 32) (1/2)
            f = (4 ** lfNoise0Id z kr (1/16)) * 300
        in sin (rlpf t f 5e-3)
    x = splay (mce (map nd [0::Int .. 7])) 1 1 0 True
    r u = let (p,q) = mce2c u in freeVerb2 p q 0.1 1 1
in r (r x)
