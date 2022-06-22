-- blips 001 (jmcc) #Sc3d1.5 ; texture=overlap,2,1,12,inf
let mk _ =
      let f = xLine kr (expRand 0.25 400) (expRand 0.25 400) 4 DoNothing
          nh = xLine kr (expRand 2 100) (expRand 2 100) 4 DoNothing
      in blip ar f nh
    pp = iter 6 (\x -> allpassN x 0.05 (mce2 (rand 0 0.05) (rand 0 0.05)) 4) . distort
in pp (rand 0 1 <** 0.8 * pan2 (mk () * mk ()) (line kr (rand2 1) (rand2 1) 4 DoNothing) 0.3)

-- blips 001 (jmcc) #Sc3d1.5 ; texture=overlap,2,1,12,inf ; id
let mk z =
      let f = xLine kr (expRandId (z,'α') 0.25 400) (expRandId (z,'β') 0.25 400) 4 DoNothing
          nh = xLine kr (expRandId (z,'γ') 2 100) (expRandId (z,'δ') 2 100) 4 DoNothing
      in blip ar f nh
    s =
      let c = randId 'ε' 0 1 <** 0.8
          o = mk 'ζ' * mk 'η'
      in c * pan2 o (line kr (rand2Id 'θ' 1) (rand2Id 'ι' 1) 4 DoNothing) 0.3
    pp =
      let f z x = allpassN x 0.05 (mce2 (randId (z,'κ') 0 0.05) (randId (z,'λ') 0 0.05)) 4
      in Sound.Sc3.Common.Base.compose_l (map f (id_seq 6 'μ')) . distort
in pp s
