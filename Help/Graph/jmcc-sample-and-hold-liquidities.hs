-- sample and hold liquidities (jmcc) #4
let r = mouseX KR 1 200 Exponential 0.1
    t = recip r
    c = impulse KR r 0 * 0.4
    cf = mouseY KR 100 8000 Exponential 0.1
    f = latch (whiteNoise 'α' KR * cf * 0.5 + cf) c
    p = latch (whiteNoise 'β' KR) c
    i = pan2 (sinOsc AR f 0 * decay2 c (t * 0.1) (t * 0.9)) p 1
in combN i 0.3 0.3 2

-- sample and hold liquidities (jmcc) #4 ; event control
let f _ (g,_,y,z,o,_,_,p,px,_) =
      let env = decay2 (trig g controlDur * z * 2) (0.01 + y * 0.1) (0.5 + y * 0.4)
          sig = pan2 (sinOsc AR (midiCPS (p + px)) 0 * env) (o * 2 - 1) 1
      in sig + combN sig 0.3 0.3 2
in mix (rEventVoicer 16 f) * control KR "gain" 1
