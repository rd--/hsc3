-- randSeed ; start a sound that includes noise ugens
let n = Protect.uclone_all 'α' 2 (whiteNoise 'β' AR * 0.05 + dust2 'γ' AR 70)
    f = lfNoise1 'δ' KR 3 * 5500 + 6000
in resonz (n * 5) f 0.5 + n * 0.5

---- ; CRASH
-----  WidthFirstUGen : LocalBuf SetBuf ClearBuf IFFT PV_ChainUGen RandSeed RandID

-- randSeed ; reset the seed at a variable rate (CRASH)
let s = control KR "seed" 1956
    i = impulse KR (mouseX KR 0.1 100 Linear 0.2) 0
in wrapOut Nothing $ randSeed KR i s

-- randSeed ; always the same (for a given seed)... (CRASH)
let sd = 1957
    n = tiRand 'α' 4 12 (dust 'β' KR 1)
    f = n * 150 + (mce [0,1])
    r = randSeed IR 1 sd
in mrg2 (sinOsc AR f 0 * 0.1) r

