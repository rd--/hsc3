-- linExp
let mod = sinOsc KR (line KR 1 10 10 DoNothing) 0
in sinOsc AR (mce2 (mod * 400 + 500) (linExp mod (-1) 1 100 900)) 0 * 0.05

-- linExp
let f = linExp (mouseX KR 0 1 Linear 0.2) 0 1 440 660
in sinOsc AR f 0 * 0.1

-- linExp ; the destination range may be k-rate
let x = mouseX KR 0 1 Linear 0.2
    y = mouseY KR 220 440 Linear 0.2
    f = linExp x 0 1 y 660
in sinOsc AR f 0 * 0.1

-- linExp ; i-rate
sinOsc AR (linExp (rand 'α' 0 1) 0 1 220 440) 0 * 0.1
