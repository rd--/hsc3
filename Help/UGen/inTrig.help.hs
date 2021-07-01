-- inTrig ; oscillator with the trigger at bus ; dust & mouse button both set bus
let b = control kr "bus" 10
    t = inTrig 1 b
    e = envGen kr t t 0 1 DoNothing (envPerc 0.01 1)
in mrg2 (sinOsc ar 440 0 * e) (out b (mouseButton kr 0 1 0 + dustId 'α' kr 0.5))
