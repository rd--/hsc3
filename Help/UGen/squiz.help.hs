-- squiz ; of sin oscillator
let zmax = 10
    x = mouseX KR 1 10 Exponential 0.2
    y = mouseY KR 1 zmax Linear 0.2
in X.squiz (sinOsc AR 440 0) x y 0.1 * 0.1

-- squiz ; of soundin
let zmax = 100
    s = sinOsc AR 440 0
    x = mouseX KR 1 10 Exponential 0.2
    y = mouseY KR 1 zmax Linear 0.2
in X.squiz (soundIn 0) x y 0.1 * 0.1

-- squiz ; of buffer
let b = control KR "buf" 0
    zmax = 100
    s = sinOsc AR 440 0
    x = mouseX KR 1 10 Exponential 0.2
    y = mouseY KR 1 zmax Linear 0.2
    r = bufRateScale KR b
    p = playBuf 1 AR b (r * 0.5) 1 0 Loop DoNothing
in X.squiz p x y 0.1 * 0.1
