-- spring ; mouse control ; button=gate ; x=spring-constant ; y=damping
let inforce = k2a (mouseButton KR 0 1 0) `greater_than` 0
    k = mouseX KR 0.1 20 Exponential 0.2
    d = mouseY KR 0.00001 0.1 Exponential 0.2
    outforce = spring AR inforce k d
    freq = outforce * 400 + 500 -- modulate frequency with the force
in sinOsc AR freq 0 * 0.1

-- spring ;several springs in series
let inforce = k2a (mouseButton KR 0 1 0) `greater_than` 0
    k = mouseX KR 0.1 20 Exponential 0.2
    d = mouseY KR 0.00001 0.1 Exponential 0.2
    m0 = spring AR inforce k 0.01
    m1 = spring AR m0 (0.5 * k) d
    m2 = spring AR m0 (0.6 * k + 0.2) d
    m3 = spring AR (m1 - m2) 0.4 d
in sinOsc AR (m3 * 200 + 500) 0 * 0.1 -- modulate frequency with the force

-- spring ; modulating a resonating string
let k = mouseX KR 0.5 100 Exponential 0.2
    d = mouseY KR 0.0001 0.1 Exponential 0.2
    t = dust 'α' AR 2
    m0 = spring AR (toggleFF t) k 0.01
    m1 = spring AR m0 (0.5 * k) d
    m2 = spring AR m0 (0.6 * k) d
    m3 = spring AR (mce2 m1 m2) (0.4 * k) d
    m4 = spring AR (m3 - m1 + m2) (0.1 * k) d
in combL t 0.1 (linLin m4 (-10) 10 (1/8000) (1/100)) 12

-- spring ; event control
let f _ (w,x,y,z,o,rx,_,_,_,_) =
      let k = linExp x 0 1 0.1 20
          d = linExp y 0 1 0.00001 0.065
          s = spring AR (k2a w) k d
          freq = midiCPS (x * 24 + 48) + (s * 800 * rx) -- modulate frequency with the force
      in pan2 (sinOsc AR freq 0) (o * 2 - 1) (w * z * z)
in mix (rEventVoicer 16 f) * control KR "gain" 2

-- spring ; event control
let f _ (w,x,y,z,_,_,_,_,_,_) =
      let k = linExp x 0 1 0.5 100
          d = linExp y 0 1 0.0001 0.1
          t = trig (k2a w) 0
          m0 = spring AR (k2a w) k 0.01
          m1 = spring AR m0 (0.5 * k) d
          m2 = spring AR m0 (0.6 * k) d
          m3 = spring AR (mce2 m1 m2) (0.4 * k) d
          m4 = spring AR (m3 - m1 + m2) (0.1 * k) d
          dt = linLin m4 (-10) 10 (x/8000) (x/100)
      in combL (trig (k2a w) (z * 0.05)) 0.1 dt ((latch y w + 1) * 9)
in mix (rEventVoicer 16 f) * control KR "gain" 2
