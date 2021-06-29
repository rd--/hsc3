-- record scratcher (jp) ; requires=bus
let b = control kr "bus" 0
    dup a = mce2 a a
    d = Envelope [0, 1, 0] [0.1, 0.1] [EnvSin] (Just 1) (Just 0) 0
    e = envGen kr 1 0.5 0 1 RemoveSynth d
    x = mouseX kr (-10) 10 Linear 0.2
    dx = x - delayN x 0.1 0.1
    bdx = mouseButton kr 1 0 0.3 + dx
    bdxr = bdx * bufRateScale kr b
    scr = playBuf 1 ar b bdxr 0 0 Loop DoNothing
in dup (scr * e)
