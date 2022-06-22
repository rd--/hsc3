-- record scratcher (jp) ; requires=buf
let (buf, nc) = (control kr "buf" 0, 2)
    dup a = mce2 a a
    d = Envelope [0, 1, 0] [0.1, 0.1] [EnvSin] (Just 1) (Just 0) 0
    e = envGen kr 1 0.5 0 1 RemoveSynth d
    x = mouseX kr (-10) 10 Linear 0.2
    dx = x - delayN x 0.1 0.1
    bdx = mouseButton kr 1 0 0.3 + dx
    bdxr = bdx * bufRateScale kr buf
    scr = playBuf nc ar buf bdxr 0 0 Loop DoNothing
in dup (scr * e)

---- ; buffer setup
withSc3 (async (b_allocRead 0 (sfResolve "pf-c5.aif") 0 0))
