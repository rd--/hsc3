-- squiz ; of sin oscillator
let zmax = 10
    x = mouseX kr 1 10 Exponential 0.2
    y = mouseY kr 1 zmax Linear 0.2
in X.squiz (sinOsc ar 440 0) x y 0.1 * 0.1

-- squiz ; of soundin
let zmax = 100
    s = sinOsc ar 440 0
    x = mouseX kr 1 10 Exponential 0.2
    y = mouseY kr 1 zmax Linear 0.2
in X.squiz (soundIn 0) x y 0.1 * 0.1

-- squiz ; of buffer ; requires=buf
let b = control kr "buf" 0
    zmax = 100
    s = sinOsc ar 440 0
    x = mouseX kr 1 10 Exponential 0.2
    y = mouseY kr 1 zmax Linear 0.2
    r = bufRateScale kr b
    p = playBuf 1 ar b (r * 0.5) 1 0 Loop DoNothing
in X.squiz p x y 0.1 * 0.1
