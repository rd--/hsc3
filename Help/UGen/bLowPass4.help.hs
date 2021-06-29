-- bLowPass4
let i = mix (saw ar (mce [0.99, 1, 1.01] * 440) * 0.3)
    f = mouseX kr 100 20000 Exponential 0.2
    rq = mouseY kr 0.1 1 Linear 0.2
in bLowPass4 i f rq
