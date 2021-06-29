-- sumSqr
let o1 = fSinOsc ar 800 0
    o2 = fSinOsc ar (xLine kr 200 500 5 DoNothing) 0
in sumSqr o1 o2 * 0.125

-- sumSqr ; written out
let o1 = fSinOsc ar 800 0
    o2 = fSinOsc ar (xLine kr 200 500 5 DoNothing) 0
in (o1 * o1 + o2 * o2) * 0.125
