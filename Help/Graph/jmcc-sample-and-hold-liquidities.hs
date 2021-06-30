-- sample and hold liquidities (jmcc) #4
let r = mouseX kr 1 200 Exponential 0.1
    t = recip r
    c = impulse kr r 0 * 0.4
    cf = mouseY kr 100 8000 Exponential 0.1
    f = latch (whiteNoise kr * cf * 0.5 + cf) c
    p = latch (whiteNoise kr) c
    i = pan2 (sinOsc ar f 0 * decay2 c (t * 0.1) (t * 0.9)) p 1
in combN i 0.3 0.3 2

-- sample and hold liquidities (jmcc) #4 ; id
let r = mouseX kr 1 200 Exponential 0.1
    t = recip r
    c = impulse kr r 0 * 0.4
    cf = mouseY kr 100 8000 Exponential 0.1
    f = latch (whiteNoiseId 'α' kr * cf * 0.5 + cf) c
    p = latch (whiteNoiseId 'β' kr) c
    i = pan2 (sinOsc ar f 0 * decay2 c (t * 0.1) (t * 0.9)) p 1
in combN i 0.3 0.3 2

-- sample and hold liquidities (jmcc) #4 ; event control
let f _ (g,_,y,z,o,_,_,p,px,_) =
      let env = decay2 (trig g controlDur * z * 2) (0.01 + y * 0.1) (0.5 + y * 0.4)
          sig = pan2 (sinOsc ar (midiCPS (p + px)) 0 * env) (o * 2 - 1) 1
      in sig + combN sig 0.3 0.3 2
in mix (eventVoicer 16 f) * control kr "gain" 1
