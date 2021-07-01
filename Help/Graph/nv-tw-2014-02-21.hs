-- nv ; https://twitter.com/headcube/status/437094206767513600
let x = impulse ar 0.05 0
    f i =
      let n = lfNoise2 kr
          a = allpassL (leakDC i 0.995) 4 ((8 ** n 0.1) / 2) 8 * 1.2
      in tanh (lpf a ((8 ** n (mce2 (rand 0 0.1) (rand 0 0.1))) * 2500))
in ugen_optimise_ir_rand (mixFill 4 (\_ -> iter 20 f x * 5))

-- nv ; https://twitter.com/headcube/status/437094206767513600 ; id
let x = impulse ar 0.05 0
    f i =
      let n e = lfNoise2Id e kr
          a = allpassL (leakDC i 0.995) 4 ((8 ** n 'α' 0.1) / 2) 8 * 1.2
      in tanh (lpf a ((8 ** n 'β' (mce2 (randId 'γ' 0 0.1) (randId 'δ' 0 0.1))) * 2500))
    y z = Protect.useq_all z 20 f x * 5
in ugen_optimise_ir_rand (mix (mceFillInt 4 y))
