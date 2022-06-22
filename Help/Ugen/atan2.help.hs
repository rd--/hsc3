-- atan2 ; pan hypot doppler example, atan2 finds direction of source, speakers at +/- 45 degrees
let x = 10
    y = lfSaw kr (1 / 6) 0 * 100
    d = hypot x y
    a = 40 / (squared d)
    s = rlpf (fSinOsc ar 200 0 * lfPulse ar 31.3 0 0.4) 400 0.3
    z = atan2E y x
    l = clip2 (z / (pi / 2)) 1
in pan2 (delayL s (110 / 344) (d / 344)) l a
