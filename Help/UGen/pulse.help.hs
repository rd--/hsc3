-- pulse ; modulate frequency
let f = xLine KR 40 4000 6 RemoveSynth
in pulse AR f 0.1 * 0.1

-- pulse ; modulate width, 0.5 = square wave
let w = line KR 0.01 0.99 8 RemoveSynth
in pulse AR 200 w * 0.1

-- pulse ; two band limited square waves through a resonant low pass filter
let p = pulse AR (mce2 100 250) 0.5 * 0.1
    f = xLine KR 8000 400 5 DoNothing
in rlpf p f 0.05

