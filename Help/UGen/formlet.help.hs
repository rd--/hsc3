-- formlet
formlet (impulse ar 20 0.5) 1000 0.01 0.1

-- formlet
let f = xLine kr 10 400 8 RemoveSynth
in formlet (blip ar f 1000 * 0.1) 1000 0.01 0.1

-- formlet ; modulating formant frequency
let s = blip ar (sinOsc kr 5 0 * 20 + 300) 1000 * 0.1
    ff = xLine kr 1500 700 8 RemoveSynth
in formlet s ff 0.005 0.04

-- formlet ; mouse control of frequency and decay time
let s = blip ar (sinOsc kr 5 0 * 20 + 300) 1000 * 0.1
    x = mouseX kr 0.01 0.2 Exponential 0.2
    y = mouseY kr 700 2000 Exponential 0.2
in formlet s y 0.005 x

-- formlet ; and again (control-rate)
let s = dust 'α' kr (mce2 10 11)
    x = mouseX kr 0.1 2 Exponential 0.2
    y = mouseY kr 7 200 Exponential 0.2
    f = formlet s y 0.005 x
in k2a f + sinOsc ar (f * 200 + mce2 500 600 - 100) 0 * 0.1

-- formlet ; event control
let f c (g,x,y,z,o,rx,_,_,_,_) =
      let s = blip ar (sinOsc kr 5 0 * 20 * constant (c + 1) + 300) (2000 * rx)
      in pan2 (formlet s (linExp y 0 1 700 2000) 0.005 (linExp x 0 1 0.01 0.2)) (o * 2 - 1) (g * z)
in mix (eventVoicer 16 f) * control kr "gain" 1

