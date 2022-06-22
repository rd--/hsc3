-- pulse ; modulate frequency
let f = xLine kr 40 4000 6 RemoveSynth
in pulse ar f 0.1 * 0.1

-- pulse ; modulate width, 0.5 = square wave
let w = line kr 0.01 0.99 8 RemoveSynth
in pulse ar 200 w * 0.1

-- pulse ; two band limited square waves through a resonant low pass filter
let p = pulse ar (mce2 100 250) 0.5 * 0.1
    f = xLine kr 8000 400 5 DoNothing
in rlpf p f 0.05

