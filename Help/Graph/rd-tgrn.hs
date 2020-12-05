-- tgrn (rd)
let b = control KR "buf" 0
    trate = mouseY KR 2 120 Exponential 0.1
    dur = 1.2 / trate
    clk = impulse AR trate 0
    pos = mouseX KR 0 (bufDur KR b) Linear 0.1
    pan = whiteNoise 'α' KR * 0.6
    n = roundTo (whiteNoise 'β' KR * 3) 1
    rate = shiftLeft 1.2 n
in tGrains 2 clk b rate pos dur pan 0.5 2

---- ; load sndfile
withSC3 (async (b_allocRead 0 "/home/rohan/sw/hsc3-graphs/snd/2006-10-05.snd" 0 0))
