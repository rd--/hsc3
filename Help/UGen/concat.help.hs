-- concat ; requires=buf ; granulator
let b = control KR "buf" 0
    y0 = mouseY KR 0.01 1 Linear 0.2
    y1 = mouseY KR b 100 Linear 0.2
    n = lfNoise0 'α' KR y0 * 3 + 4.5
    k = saw AR (sinOsc KR n 0 * 10 + y1)
    i = playBuf 1 AR b (bufRateScale KR b) 0 0 Loop DoNothing
    x0 = mouseX KR 0.01 0.1 Linear 0.2
    y2 = mouseY KR 0 0.1 Linear 0.2
    c :: UGen
    c = X.concat AR k i 2 2 2 x0 0 y2 1 0.5 0 0
in pan2 c 0 1
