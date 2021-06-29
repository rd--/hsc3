-- iirFilter
let x = mouseX kr 20 12000 Exponential 0.2
    y = mouseY kr 0.01 1 Linear 0.2
    o = lfSaw ar (mce [x * 0.99,x * 1.01]) 0 * 0.1
    freq = sinOsc kr (sinOsc kr 0.1 0) (1.5 * pi) * 1550 + 1800
    s = X.iirFilter o freq y
in combN s 0.5 (mce2 0.4 0.35) 2 * 0.4 + s * 0.5
