-- stringVoice
let pan = 0
    freq = 100
    trig = dust2 'α' kr 7
    infsustain = 0
    accent = tRand 'β' 0 1 trig
    structure = tRand 'γ' 0 1 trig
    brightness = tRand 'δ' 0 0.5 trig
    damping = tRand 'ε' 0.1 0.5 trig
    sig = X.stringVoice ar trig infsustain freq accent structure brightness damping
in pan2 sig pan 1

-- stringVoice ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 25 + 36)
          tr = trig g controlDur
          sig = X.stringVoice ar tr 0 freq z y rx (ry * 2)
      in pan2 sig (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 1
