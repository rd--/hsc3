-- pluck ; excitation signal is white noise, triggered twice a second with varying OnePole coef
let n = whiteNoiseId 'α' ar
    t = impulse kr 9 0
    x = mouseX kr (-0.999) 0.999 Linear 0.1
    y = mouseY kr 0.1 1 Linear 0.1
    dl = 1 / 440
in pluck (n * 0.25) t dl (dl * y) 10 x

-- pluck
let n = 50
    f = X.rRandNId n 'α' 0.05 0.2
    p = X.rRandNId n 'β' 0 1
    w = mceFill_z 'γ' n (\z _ -> whiteNoiseId z ar)
    fi = X.rRandNId n 'δ' 10 12
    coef = randId 'ε' 0.01 0.2
    l = X.rRandNId n 'ζ' (-1) 1
    x = mouseX kr 60 1000 Exponential 0.1
    o = linLin (sinOsc kr f p) (-1) 1 x 3000
    i = impulse kr fi 0
    ks = pluck (w * 0.1) i 0.01 (1 / o) 2 coef
in leakDC (mix (pan2 ks l 1)) 0.995

-- pluck ; event control
let f c (g,x,y,z,o,_,_,_,_,_) =
      let n = whiteNoiseId (c,'α') ar * z
          dl_max = 1 / 220
          dl = dl_max * (1 - x * 0.9)
          sig = pluck n g dl_max dl 10 (y / 3)
      in pan2 sig (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 2

-- pluck ; event control ; gateReset
let f _ (g,x,y,z,o,_,_,p,_,_) =
      let n = whiteNoiseId 'α' ar * z
          dl_max = 1 / 8
          (gt,tr) = eventGateReset g p
          dx = x - latch x tr
          dl = 1 / midiCPS (p + dx * 8)
      in pan2 (pluck n gt dl_max dl 10 (y / 3)) (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 2

-- pluck ; event control ; gateReset ; p+px
let f _ (g,_,y,z,o,_,_,p,px,_) =
      let n = whiteNoiseId 'α' ar * z
          dl_max = 1 / 8
          (gt,tr) = eventGateReset g p
          dl = 1 / midiCPS (p + px * 1)
      in pan2 (pluck n gt dl_max dl 10 (y / 3)) (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 2
