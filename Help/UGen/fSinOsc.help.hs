-- fSinOsc ; note SC2 did not have the initial phase argument
fSinOsc AR (mce2 440 550) 0 * 0.05

-- fSinOsc ; modulate frequency
fSinOsc AR (xLine KR 200 4000 1 RemoveSynth) 0 * 0.1

-- fSinOsc ; loses amplitude towards the end
let f = fSinOsc AR (xLine KR 4 401 8 RemoveSynth)
in fSinOsc AR (f 0 * 200 + 800) 0 * 0.1
