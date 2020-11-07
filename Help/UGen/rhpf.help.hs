-- rhpf
let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
in rhpf (saw AR 200 * 0.1) f 0.2

-- rhpf
let c = rhpf (lfSaw KR 2 0) (sinOsc KR (xLine KR 0.7 30 20 RemoveSynth) 0 * 35 + 40) 0.05
in sinOsc AR (c * 200 + 500) 0 * 0.25
