-- blitB3Square
let x = mouseX kr 20 400 Exponential 0.2
in X.blitB3Square ar x 0.99 * 0.1

-- blitB3Square
let f = xLine kr 1000 20 10 DoNothing
in X.blitB3Square ar f 0.99 * 0.1

-- blitB3Square ; aliasing at high frequencies
let f = mouseX kr 20 10000 Exponential 0.2
    c = mouseY kr 0.001 0.999 Linear 0.2
in X.blitB3Square ar f c * 0.1

-- blitB3Square ; consider difference in CPU usage (excessive wire use,-w 1024)
let sqr_osc rt freq = X.blitB3Square rt freq 0.99 -- pulse rt f 0.5
    f z = midiCPS (range 36 72 (lfNoise0 z kr (rand z 2 3)))
    l z = rand z (-1) 1
    o z = pan2 (sqr_osc ar (f z) * 0.1) (l z) 0.1
in sum (map o [0::Int .. 99])
