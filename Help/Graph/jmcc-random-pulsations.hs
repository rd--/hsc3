-- random pulsations (jmcc) #1 ; texture=spawn,1.25,inf
let e = envLinen 2 5 2 0.02
    o1 = fSinOsc AR (rand 'α' 0 2000) 0 * envGen KR 1 1 0 1 DoNothing e
    o2 = sinOsc AR (linRand 'β' 8 88 0) 0
    o3 = sinOsc KR (rand 'γ' 0.3 0.8) (rand 'δ' 0 (2 * pi)) * 0.7
in pan2 (o1 `amClip` o2) o3 1

-- random pulsations (jmcc) #1 ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let e = envLinen 2 5 2 0.02
          o1 = fSinOsc AR (x * 2000) 0 * envGen KR g 1 0 1 DoNothing e
          o2 = sinOsc AR (y * 80 + 8) 0
          o3 = sinOsc KR (o * 0.5 + 0.3) (rx * 2 * pi) * 0.7
      in pan2 (o1 `amClip` o2) o3 (z * 5 * lagUD g 0 12)
in mix (rEventVoicer 16 f) * control KR "gain" 1
