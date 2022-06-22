-- tartini ; comparison of input frequency (x) and tracked oscillator frequency (f).
let x = mouseX kr 440 880 Exponential 0.1
    o = lfSaw ar x 0 * 0.05 {- sinOsc ar x 0 * 0.1 -}
    [f,_e] = mceChannels (X.tartini kr o 0.2 2048 0 1024 0.5)
    r = sinOsc ar f 0 * 0.1
    t = impulse kr 4 0
    pf = poll t f 0 (label "f")
    px = poll t x 0 (label "x")
in mrg [out 0 (mce2 o r),pf,px]

-- tartini ; test live pitch tracking, not careful with amplitude of input
let z = hpf (soundIn 0) 90
    [f,e] = mceChannels (X.tartini kr z 0.2 2048 0 1024 0.5)
in mce2 (z * 0.1) (lfTri ar f 0 * 0.2 * lag e 0.2 * lag (f >** 90 * f <** 500) 0.2)

-- tartini ; printer for pitch tracker
let i = soundIn 0
    [f,_e] = mceChannels (X.tartini kr i 0.2 2048 0 1024 0.5)
    r = sinOsc ar f 0 * 0.1
    t = impulse kr 4 0
    pf = poll t f 0 (label "f")
in mrg [out 0 (mce2 i r),pf]

---- ; ZELL = C4 - C5 = 247 259 277 295 309 331 345 369 387 415 441 462 493 (hz)
