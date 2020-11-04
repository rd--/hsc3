-- moogFF
let n = whiteNoise 'α' AR * 0.05
    y = mouseY KR 100 10000 Exponential 0.1
    x = mouseX KR 0 4 Linear 0.1
in moogFF n y x 0

-- moogFF ; note distortion at high gain
let x = mouseX KR 100 20000 Exponential 0.1
    y = mouseY KR 0.1 4.0 Linear 0.1
    i = mix (saw AR (mce [0.99, 1, 1.01] * 440)) * 0.2
in moogFF i x y 0

-- moogFF
let n = lfNoise0 'α' KR 0.43
    p = pulse AR (mce [40, 121]) (mce [0.3, 0.7])
    f0 = linLin n 0 1 0.001 2.2
    f = linLin (sinOsc KR f0 0) (-1) 1 30 4200
    y = mouseY KR 1 4 Linear 0.1
in moogFF p f (0.83 * y) 0
