-- tgrn (rd)
let b = control kr "buf" 0
    trate = mouseY kr 2 120 Exponential 0.1
    dur = 1.2 / trate
    clk = impulse ar trate 0
    pos = mouseX kr 0 (bufDur kr b) Linear 0.1
    pan = whiteNoise kr * 0.6
    n = roundTo (whiteNoise kr * 3) 1
    rate = shiftLeft 1.2 n
in tGrains 2 clk b rate pos dur pan 0.5 2

-- tgrn (rd) ; id
let b = control kr "buf" 0
    trate = mouseY kr 2 120 Exponential 0.1
    dur = 1.2 / trate
    clk = impulse ar trate 0
    pos = mouseX kr 0 (bufDur kr b) Linear 0.1
    pan = whiteNoiseId 'α' kr * 0.6
    n = roundTo (whiteNoiseId 'β' kr * 3) 1
    rate = shiftLeft 1.2 n
in tGrains 2 clk b rate pos dur pan 0.5 2

---- ; load sndfile
withSc3 (async (b_allocRead 0 "/home/rohan/sw/hsc3-graphs/snd/2006-10-05.snd" 0 0))
