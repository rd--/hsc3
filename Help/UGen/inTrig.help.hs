-- inTrig ; oscillator with the trigger at bus ; dust & mouse button both set bus
let b = control KR "bus" 10
    t = inTrig 1 b
    e = envGen KR t t 0 1 DoNothing (envPerc 0.01 1)
in mrg2 (sinOsc AR 440 0 * e) (out b (mouseButton KR 0 1 0 + dust 'α' KR 0.5))
