-- concat ; requires=buf ; granulator
let b = control kr "buf" 0
    y0 = mouseY kr 0.01 1 Linear 0.2
    y1 = mouseY kr b 100 Linear 0.2
    n = lfNoise0Id 'α' kr y0 * 3 + 4.5
    k = saw ar (sinOsc kr n 0 * 10 + y1)
    i = playBuf 1 ar b (bufRateScale kr b) 0 0 Loop DoNothing
    x0 = mouseX kr 0.01 0.1 Linear 0.2
    y2 = mouseY kr 0 0.1 Linear 0.2
    c :: UGen
    c = X.concat ar k i 2 2 2 x0 0 y2 1 0.5 0 0
in pan2 c 0 1
