-- zizle (jmcc) #Sc3d1.5 ; texture=overlap,4,4,12,inf
let a f = let fm = mce2 (rand 0.7 1.3) 1
              ph = mce2 (rand 0 two_pi) (rand 0 two_pi)
          in mix (sinOsc ar (f * fm) ph * 0.1)
    a1 = max (a (expRand 0.3 8)) 0
    a2 = abs (a (expRand 6 24))
    o = sinOsc ar (midiCps (rand 24 108)) (rand 0 two_pi)
in pan2 (o * a1 * a2) (rand2 1) 1

-- zizle (jmcc) #Sc3d1.5 ; texture=overlap,4,4,12,inf ; id
let a z f = let fm = mce2 (randId (z,'α') 0.7 1.3) 1
                ph = mce2 (randId (z,'β') 0 two_pi) (randId (z,'γ') 0 two_pi)
            in mix (sinOsc ar (f * fm) ph * 0.1)
    a1 = max (a 'δ' (expRandId 'ε' 0.3 8)) 0
    a2 = abs (a 'ζ' (expRandId 'η' 6 24))
    o = sinOsc ar (midiCps (randId 'θ' 24 108)) (randId 'ι' 0 two_pi)
in pan2 (o * a1 * a2) (rand2Id 'κ' 1) 1

-- zizle (jmcc) #Sc3d1.5 ; event control
let f (_,g,x,y,z,o,_,_,_,_,_) =
      let a f0 = let fm = mce2 (tRand 0.7 1.3 g) 1
                     ph = mce2 (tRand 0 two_pi g) (tRand 0 two_pi g)
                 in mix (sinOsc ar (f0 * fm) ph)
          a1 = max (a (tExpRand 0.3 8 g)) 0
          a2 = abs (a (tExpRand 6 24 g))
          sig = sinOsc ar (midiCps (x * 84 + 24)) (tRand 0 two_pi g)
      in pan2 (sig * a1 * a2) (o * 2 - 1) (z * lagUD g (y * 0.01) (y * 2))
in mix (voicer 16 f) * control kr "gain" 1

-- zizle (jmcc) #Sc3d1.5 ; event control ; id
let f (c,g,x,y,z,o,_,_,_,_,_) =
      let a e f0 = let fm = mce2 (tRandId (e,'α') 0.7 1.3 g) 1
                       ph = mce2 (tRandId (e,'β') 0 two_pi g) (tRandId (e,'γ') 0 two_pi g)
                   in mix (sinOsc ar (f0 * fm) ph)
          a1 = max (a (c,'δ') (tExpRandId (c,'ε') 0.3 8 g)) 0
          a2 = abs (a (c,'ζ') (tExpRandId (c,'η') 6 24 g))
          sig = sinOsc ar (midiCps (x * 84 + 24)) (tRandId 'ι' 0 two_pi g)
      in pan2 (sig * a1 * a2) (o * 2 - 1) (z * lagUD g (y * 0.01) (y * 2))
in mix (voicer 16 f) * control kr "gain" 1
