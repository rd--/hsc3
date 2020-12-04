-- http://sccode.org/1-V (nv) [Line 41]
let nd z =
        let i = constant z
            t = (0.6 ** i) * 40 * impulse AR ((2 ** i) / 32) (1/2)
            f = (4 ** lfNoise0 z KR (1/16)) * 300
        in sin (rlpf t f 5e-3)
    x = splay (mce (map nd [0::Int .. 7])) 1 1 0 True
    r u = let (p,q) = mce2c u in freeVerb2 p q 0.1 1 1
in r (r x)
