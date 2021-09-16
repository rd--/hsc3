-- synthetic piano (jmcc) #3 ; texture=overlap,6,0,6,inf
let n = iRand 36 90
    s = impulse ar (rand 0.1 0.5) (rand 0 1) * 0.1
    e = decay2 s 0.008 0.04
    c o = let n0 = lfNoise2 ar 3000
              dt = 1 / midiCps (n + o)
          in combL (n0 * e) dt dt 6
    l = ((n - 36) / 27) - 1
    c_ = sum_opt (map c [-0.05,0,0.04])
in pan2 c_ l 1

-- synthetic piano (jmcc) #3 ; texture=overlap,6,0,6,inf ; id
let n = iRandId 'α' 36 90
    f = randId 'β' 0.1 0.5
    ph = randId 'γ' 0 1
    s = impulse ar f ph * 0.1
    e = decay2 s 0.008 0.04
    c z o = let n0 = lfNoise2Id z ar 3000
                dt = 1 / midiCps (n + o)
            in combL (n0 * e) dt dt 6
    l = ((n - 36) / 27) - 1
    c_ = sum_opt (zipWith c ['δ'..] [-0.05,0,0.04])
in pan2 c_ l 1

-- synthetic piano (jmcc) #3 ; event-control
let f _ (g,_,y,z,_,_,_,p,_,_) =
      let e = decay2 (trig g controlDur) 0.008 (linLin y 0 1 0.02 0.06) * z * 2
          cmb o = let n0 = lfNoise2 ar 3000
                      dt = 1 / midiCps (p + o)
                  in combL (n0 * e) dt dt 6
          l = ((p - 36) / 27) - 1
          cmb_ = sum_opt (map cmb [-0.05,0,0.04])
      in pan2 cmb_ l (1 + z)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- synthetic piano (jmcc) #3 ; event-control ; id
let f c (g,_,y,z,_,_,_,p,_,_) =
      let e = decay2 (trig g controlDur) 0.008 (linLin y 0 1 0.02 0.06) * z * 2
          cmb k o = let n0 = lfNoise2Id (c,k) ar 3000
                        dt = 1 / midiCps (p + o)
                    in combL (n0 * e) dt dt 6
          l = ((p - 36) / 27) - 1
          cmb_ = sum_opt (zipWith cmb ['δ'..] [-0.05,0,0.04])
      in pan2 cmb_ l (1 + z)
in mix (eventVoicer 16 f) * control kr "gain" 1
