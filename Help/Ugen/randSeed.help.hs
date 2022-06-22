-- randSeed ; start a sound that includes noise ugens
let n = mceFillId 'α' 2 (\z _ -> whiteNoiseId z ar * 0.05 + dust2Id z ar 70)
    f = lfNoise1Id 'β' kr 3 * 5500 + 6000
in resonz (n * 5) f 0.5 + n * 0.5

---- ; CRASH
-----  WidthFirstUGen : LocalBuf SetBuf ClearBuf IFFT PV_ChainUGen RandSeed RandID

-- randSeed ; reset the seed at a variable rate (CRASH)
let s = control kr "seed" 1956
    i = impulse kr (mouseX kr 0.1 100 Linear 0.2) 0
in wrapOut Nothing $ randSeed kr i s

-- randSeed ; always the same (for a given seed)... (CRASH)
let sd = 1957
    n = tiRandId 'α' 4 12 (dustId 'β' kr 1)
    f = n * 150 + (mce [0,1])
    r = randSeed ir 1 sd
in mrg2 (sinOsc ar f 0 * 0.1) r

