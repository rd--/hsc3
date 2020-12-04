-- down sample (adc)
let f = lfNoise2 'α' KR 8
    nh = lfNoise2 'β' KR 3
    src = blip AR (f * 200 + 300) (nh * 10 + 20)
    sr = mouseX KR 1000 (sampleRate * 0.1) Exponential 0.2
in latch src (impulse AR sr 0) * 0.1
