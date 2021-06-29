-- alien froggies (jmcc) #1 ; texture=overlap,0.25,0.5,5,inf
let alien_froggies_n r =
      let r' = fold (r * exp (linRand (-0.2) 0.2 0)) 1 30
          o = formant ar r' (expRand 200 3000) (rand 0 9 * r' + r')
      in o * 0.05
in alien_froggies_n 11

-- alien froggies (jmcc) #1 ; texture=overlap,0.25,0.5,5,inf ; id
let alien_froggies_n r =
      let r' = fold (r * exp (linRandId 'α' (-0.2) 0.2 0)) 1 30
          o = formant ar r' (expRandId 'β' 200 3000) (randId 'γ' 0 9 * r' + r')
      in o * 0.05
in alien_froggies_n 11

-- alien froggies (jmcc) #1 ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let r = fold (11 * exp (x * 0.4 - 0.2)) 1 30
          s = formant ar r (linExp y 0 1 200 3000) (rx * 9 * r + r)
      in pan2 s (o * 2 - 1) (0.5 * z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1
