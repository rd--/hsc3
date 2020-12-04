-- alien froggies (jmcc) #1 ; texture=overlap,0.25,0.5,5,inf
let alien_froggies_n r =
      let r' = fold (r * exp (linRand 'α' (-0.2) 0.2 0)) 1 30
          o = formant AR r' (expRand 'β' 200 3000) (rand 'γ' 0 9 * r' + r')
      in o * 0.05
in alien_froggies_n 11
