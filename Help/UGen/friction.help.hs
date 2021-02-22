-- friction ; filters a sine wave ; chaotic result
let o = sinOsc AR 660 0
    s = X.friction AR o 0.0000541322 0.414 0.313 8.05501 1.0
in pan2 s 0 0.1

-- friction ; mouse control
let o = lag (sinOsc AR ((lfPulse KR 0.5 0 0.5 + lfPulse KR 0.33 0 0.5) * 440 + 220) 0) 0.1
    x = mouseX KR 0.00001 0.03 Exponential 0.2
    y = mouseY KR 0.2 10 Exponential 0.2
    s = X.friction AR o x 0.414 0.313 y 1.0
in (mce2 o s) * 0.5

-- friction ; control rate
let s = lfPar KR 33 0 + mouseX KR 0.01 10 Exponential 0.2
    y = mouseY KR 0 1 Linear 0.2
    o = X.friction KR s 0.0000541322 y 0.318 8.05501 1.0
in pan2 (sinOsc AR (o * 350 + 150) 0) 0 0.1

-- friction ; event control
let f _ (w,x,y,z,o,_,_,_,_,_) =
      let im = trig1 (k2a w) sampleDur * 0.1
          kl = ringz im (x * mce [800, 1071, 1153, 1723]) 1
          ru = X.friction AR kl (y * 0.000175584) 0.414 0.313 2.69789 1.0
      in pan2 (mix ru) (o * 2 - 1) 1
in mix (rEventVoicer 16 f) * control KR "gain" 1

