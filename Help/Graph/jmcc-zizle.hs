-- zizle (jmcc) #SC3d1.5 ; texture=overlap,4,4,12,inf
let a e f = let fm = mce2 (rand 'α' 0.7 1.3) 1
                ph = mce2 (rand 'β' 0 two_pi) (rand 'γ' 0 two_pi)
            in Protect.uprotect_all e (mix (sinOsc AR (f * fm) ph * 0.1))
    a1 = max (a 'δ' (expRand 'ε' 0.3 8)) 0
    a2 = abs (a 'ζ' (expRand 'η' 6 24))
    o = sinOsc AR (midiCPS (rand 'θ' 24 108)) (rand 'ι' 0 two_pi)
in pan2 (o * a1 * a2) (rand2 'κ' 1) 1

-- zizle (jmcc) #SC3d1.5 ; event control
let f c (g,x,y,z,o,_,_,_,_,_) =
      let a e f0 = let fm = mce2 (tRand (e,'α') 0.7 1.3 g) 1
                       ph = mce2 (tRand (e,'β') 0 two_pi g) (tRand (e,'γ') 0 two_pi g)
                   in mix (sinOsc AR (f0 * fm) ph)
          a1 = max (a (c,'δ') (tExpRand (c,'ε') 0.3 8 g)) 0
          a2 = abs (a (c,'ζ') (tExpRand (c,'η') 6 24 g))
          sig = sinOsc AR (midiCPS (x * 84 + 24)) (tRand 'ι' 0 two_pi g)
      in pan2 (sig * a1 * a2) (o * 2 - 1) (z * lagUD g (y * 0.01) (y * 2))
in mix (rEventVoicer 16 f) * control KR "gain" 1
