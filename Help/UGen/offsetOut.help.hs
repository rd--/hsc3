-- offsetOut
let a = offsetOut 0 (impulse AR 5 0)
    b = out 0 (sinOsc AR 60 0 * 0.1)
in mrg2 a b

-- offsetOut ; c.f. out
let a = out 0 (impulse AR 5 0)
    b = out 0 (sinOsc AR 60 0 * 0.1)
in mrg [a,b]
