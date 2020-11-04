-- linLin ; linLin is a function for writing a MulAdd UGen.
let f = linLin (mouseX KR 0 1 Linear 0.2) 0 1 440 660
in sinOsc AR f 0 * 0.1

-- linLin ; the destination range may be k-rate
let x = mouseX KR 0 1 Linear 0.2
    y = mouseY KR 220 440 Linear 0.2
    f = linLin x 0 1 y 660
in sinOsc AR f 0 * 0.1

-- linLin ; modulating source and destination values
let n = lfNoise2 'α' AR 80
    x = mouseX KR 200 8000 Linear 0.2
    y = mouseY KR 200 8000 Linear 0.2
    f = linLin n (sinOsc KR 0.2 0) (sinOsc KR 0.2543 0) x y
in sinOsc AR f 0 * 0.1
