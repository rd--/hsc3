-- friction ; filters a sine wave ; chaotic result
let o = sinOsc ar 660 0
    s = X.friction ar o 0.0000541322 0.414 0.313 8.05501 1.0
in pan2 s 0 0.1

-- friction ; mouse control
let o = lag (sinOsc ar ((lfPulse kr 0.5 0 0.5 + lfPulse kr 0.33 0 0.5) * 440 + 220) 0) 0.1
    x = mouseX kr 0.00001 0.03 Exponential 0.2
    y = mouseY kr 0.2 10 Exponential 0.2
    s = X.friction ar o x 0.414 0.313 y 1.0
in (mce2 o s) * 0.5

-- friction ; control rate
let s = lfPar kr 33 0 + mouseX kr 0.01 10 Exponential 0.2
    y = mouseY kr 0 1 Linear 0.2
    o = X.friction kr s 0.0000541322 y 0.318 8.05501 1.0
in pan2 (sinOsc ar (o * 350 + 150) 0) 0 0.1

-- friction ; event control
let f _ (w,x,y,z,o,_,_,_,_,_) =
      let im = trig1 (k2a w) sampleDur * 0.1
          kl = ringz im (x * mce [800, 1071, 1153, 1723]) 1
          ru = X.friction ar kl (y * 0.000175584) 0.414 0.313 2.69789 1.0
      in pan2 (mix ru) (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 1

-- mutantsounds 05 https://swiki.hfbk-hamburg.de/MusicTechnology/899
let x = mceFillInt 5 (\i -> clip (lfNoise2Id i kr 3) 0 1 * 0.03 + 0.00001)
    s = X.friction ar (lfTri ar 50 0) x 0.414 0.313 (x * 30000) 1
in splay s 1 1 0 True
