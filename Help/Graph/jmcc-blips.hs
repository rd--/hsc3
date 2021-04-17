-- blips 001 (jmcc) #SC3d1.5 ; texture=overlap,2,1,12,inf
let mk z =
      let f = xLine KR (expRand (z,'α') 0.25 400) (expRand (z,'β') 0.25 400) 4 DoNothing
          nh = xLine KR (expRand (z,'γ') 2 100) (expRand (z,'δ') 2 100) 4 DoNothing
      in blip AR f nh
    s =
      let c = rand 'ε' 0 1 <** 0.8
          o = mk 'ζ' * mk 'η'
      in c * pan2 o (line KR (rand2 'θ' 1) (rand2 'ι' 1) 4 DoNothing) 0.3
    pp =
      let f z x = allpassN x 0.05 (mce2 (rand (z,'κ') 0 0.05) (rand (z,'λ') 0 0.05)) 4
      in Sound.SC3.Common.Base.compose_l (map f (id_seq 6 'μ')) . distort
in pp s
