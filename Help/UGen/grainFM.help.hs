-- grainFM
let d = 15
    lin a b = line KR a b d RemoveSynth
    l = lin (-0.5) 0.5
    f = lin 200 800
    t = impulse KR (lin 7.5 15) 0
    i = lin (-1) 1
in grainFM 2 t 0.1 f 200 i l (-1) 512 * 0.1

-- grainFM
let n1 = whiteNoise 'α' KR
    n2 = lfNoise1 'β' KR 500
    x = mouseX KR (-0.5) 0.5 Linear 0.1
    y = mouseY KR 0 400 Linear 0.1
    f = n1 * y + 440
    t = impulse KR 12.5 0
    i = linLin n2 (-1) 1 1 10
in grainFM 2 t 0.1 f 200 i x (-1) 512 * 0.1
