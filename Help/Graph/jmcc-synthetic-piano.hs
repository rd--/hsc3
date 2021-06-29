-- synthetic piano (jmcc) #3 ; texture=overlap,6,0,6,inf
let n = iRand 'α' 36 90
    f = rand 'β' 0.1 0.5
    ph = rand 'γ' 0 1
    s = impulse ar f ph * 0.1
    e = decay2 s 0.008 0.04
    c z o = let n0 = lfNoise2 z ar 3000
                dt = 1 / midiCPS (n + o)
            in combL (n0 * e) dt dt 6
    l = ((n - 36) / 27) - 1
    c_ = sum_opt (zipWith c ['δ'..] [-0.05,0,0.04])
in pan2 c_ l 1

-- synthetic piano (jmcc) #3 ; event-control
let f c (g,_,y,z,_,_,_,p,_,_) =
      let e = decay2 (trig g controlDur) 0.008 (linLin y 0 1 0.02 0.06) * z * 2
          cmb k o = let n0 = lfNoise2 (c,k) ar 3000
                        dt = 1 / midiCPS (p + o)
                    in combL (n0 * e) dt dt 6
          l = ((p - 36) / 27) - 1
          cmb_ = sum_opt (zipWith cmb ['δ'..] [-0.05,0,0.04])
      in pan2 cmb_ l (1 + z)
in mix (eventVoicer 16 f) * control kr "gain" 1
