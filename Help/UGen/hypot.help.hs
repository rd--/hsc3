-- hypot
let x = mouseX KR 0 0.1 Linear 0.1
    y = mouseY KR 0 0.1 Linear 0.1
in sinOsc AR 440 0 * hypot x y

-- hypot ; object travels 200 meters in 6 secs (=120kph) passing 10 meters from the listener
let x = 10
    y = lfSaw KR (1 / 6) 0 * 100
    d = hypot x y
    a = 10 / (squared d)
    v = slope d
    r = (344 - v) / 344 -- the speed of sound is 344 meters/sec
in fSinOsc AR (1000 * r) 0 * a

-- hypot
let x = 10
    y = lfSaw KR (1 / 6) 0 * 100
    d = hypot x y
    a = 40 / (squared d)
    s = rlpf (fSinOsc AR 200 0 * lfPulse AR 31.3 0 0.4) 400 0.3
in delayL s (110 / 344) (d / 344) * a
