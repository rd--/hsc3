-- linLin ; linLin is a function for writing a MulAdd UGen.
let f = linLin (mouseX kr 0 1 Linear 0.2) 0 1 440 660
in sinOsc ar f 0 * 0.1

-- linLin ; the destination range may be k-rate
let x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 220 440 Linear 0.2
    f = linLin x 0 1 y 660
in sinOsc ar f 0 * 0.1

-- linLin ; modulating source and destination values
let n = lfNoise2 'α' ar 80
    x = mouseX kr 200 8000 Linear 0.2
    y = mouseY kr 200 8000 Linear 0.2
    f = linLin n (sinOsc kr 0.2 0) (sinOsc kr 0.2543 0) x y
in sinOsc ar f 0 * 0.1
