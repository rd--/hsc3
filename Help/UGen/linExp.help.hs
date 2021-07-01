-- linExp
let mod = sinOsc kr (line kr 1 10 10 DoNothing) 0
in sinOsc ar (mce2 (mod * 400 + 500) (linExp mod (-1) 1 100 900)) 0 * 0.05

-- linExp
let f = linExp (mouseX kr 0 1 Linear 0.2) 0 1 440 660
in sinOsc ar f 0 * 0.1

-- linExp ; the destination range may be k-rate
let x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 220 440 Linear 0.2
    f = linExp x 0 1 y 660
in sinOsc ar f 0 * 0.1

-- linExp ; i-rate
sinOsc ar (linExp (randId 'α' 0 1) 0 1 220 440) 0 * 0.1
