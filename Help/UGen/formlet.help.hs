-- formlet
formlet (impulse AR 20 0.5) 1000 0.01 0.1

-- formlet
let f = xLine KR 10 400 8 RemoveSynth
in formlet (blip AR f 1000 * 0.1) 1000 0.01 0.1

-- formlet ; modulating formant frequency
let s = blip AR (sinOsc KR 5 0 * 20 + 300) 1000 * 0.1
    ff = xLine KR 1500 700 8 RemoveSynth
in formlet s ff 0.005 0.04

-- formlet ; mouse control of frequency and decay time
let s = blip AR (sinOsc KR 5 0 * 20 + 300) 1000 * 0.1
    x = mouseX KR 0.01 0.2 Exponential 0.2
    y = mouseY KR 700 2000 Exponential 0.2
in formlet s y 0.005 x

-- formlet ; and again (control-rate)
let s = dust 'α' KR (mce2 10 11)
    x = mouseX KR 0.1 2 Exponential 0.2
    y = mouseY KR 7 200 Exponential 0.2
    f = formlet s y 0.005 x
in k2a f + sinOsc AR (f * 200 + mce2 500 600 - 100) 0 * 0.1

-- formlet ; mouse control of frequency and decay time
let s = blip AR (sinOsc KR 5 0 * 20 + 300) 1000 * 0.1
    x = mouseX KR 0.01 0.2 Exponential 0.2
    y = mouseY KR 700 2000 Exponential 0.2
in formlet s y 0.005 x

-- formlet ; event control
let f c (g,x,y,z,o,rx,_,_) =
      let s = blip AR (sinOsc KR 5 0 * 20 * constant (c + 1) + 300) (2000 * rx)
      in pan2 (formlet s (linExp y 0 1 700 2000) 0.005 (linExp x 0 1 0.01 0.2)) (o * 2 - 1) (g * z)
in mix (rEventVoicer 10 f) * control KR "gain" 1
