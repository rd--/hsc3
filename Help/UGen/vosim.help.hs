-- vosim
let trg = impulse ar 100 0
    frq = mouseX kr 440 880 Exponential 0.2
    n_cycles = 3
    dcy = 0.1
in X.vosim ar trg frq n_cycles dcy * 0.25

-- vosim
let p = tRand 'α' 0 1 (impulse ar 6 0)
    f0 = linLin (lfNoise2 'β' kr 4) (-1) 1 3 9
    t = impulse ar (f0 * (1 + (p `greater_than` 0.95))) 0
    x = mouseX kr 0.25 2 Linear 0.2
    y = mouseY kr 0.25 0.75 Linear 0.2
    z = 9
    rng i = linLin i (-1) 1
    mk_n e = rng (lfNoise2 e kr z) 0.25 2
    tR e ll rl = tRand e (mce ll) (mce rl)
    f = tR 'γ' [40,120,220] [440,990,880] t
    n = tR 'δ' [4] [8,16,32] t
    d = tR 'ε' [0.2,0.4,0.6] [0.6,0.8,1] t
    a = tR 'ζ' [0] [0.05,0.15,0.25] t
    l = tR 'η' [-1] [1] t
    xn = mk_n 'θ'
    yn = mk_n 'ι'
    v = X.vosim ar t (f * x * xn) n (d * y * yn) * a
in pan2 (mix v) l 1

-- vosim ; event control
let f _ (g,_,y,z,o,rx,_,p,_,_) =
      let trg = impulse ar (midiCPS p) 0
          frq = linExp y 0 1 440 880
      in pan2 (X.vosim ar trg frq 1 rx) (o * 2 - 1) (g * z)
in mix (eventVoicer 16 f) * control kr "gain" 1
