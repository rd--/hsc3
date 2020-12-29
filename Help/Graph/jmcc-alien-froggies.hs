-- alien froggies (jmcc) #1 ; texture=overlap,0.25,0.5,5,inf
let alien_froggies_n r =
      let r' = fold (r * exp (linRand 'α' (-0.2) 0.2 0)) 1 30
          o = formant AR r' (expRand 'β' 200 3000) (rand 'γ' 0 9 * r' + r')
      in o * 0.05
in alien_froggies_n 11

-- alien froggies (jmcc) #1 ; event control
let f _ (g,x,y,z,o,rx,_,_) =
      let r = fold (11 * exp (x * 0.4 - 0.2)) 1 30
          s = formant AR r (linExp y 0 1 200 3000) (rx * 9 * r + r)
      in pan2 s (o * 2 - 1) (0.5 * z * g)
in mix (rEventVoicer 16 f) * control KR "gain" 1
