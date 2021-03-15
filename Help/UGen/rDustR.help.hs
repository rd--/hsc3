-- rDustR ; inter-offset times generated randomly in range (seconds) ; uniform distribution
--       ; audio rate impulse train of varying amplitude (min = max)
mce2 (X.rDustR 'α' AR 0.1 0.1 * 0.1) (impulse AR 10 0 * 0.1)

-- rDustR ; mean iot=0.0005 is equivalent to density=2000 at dust, rDustR spacing is more uniform
mce2 (X.rDustR 'β' AR 0.0001 0.001 * 0.1) (dust 'γ' AR 2000 * 0.05)

-- rDustR ; velvet noise (approx.)
let iot = 20 / sampleRate
    x = mouseX KR 1 16 Exponential 0.1
    d = X.rDustR 'α' AR (iot / x) (iot * x)
    s = tRand 'β' (-1) 1 d
in trig d sampleDur * signum s * 0.1

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen_nrt (48000,64) 1.0 (X.rDustR 'δ' AR 0.1 0.1)
plot_ugen_nrt (48000,64) 1.0 (X.rDustR 'ε' AR 0.1 0.1 + impulse AR 10 0 * 0.5)
plot_ugen_nrt (48000,64) 0.1 (X.rDustR 'ζ' AR 0.0001 0.001)
plot_ugen_nrt (48000,64) 0.1 (dust 'ζ' AR 2000)
plot_ugen_nrt (48000,64) 0.1 (let tr = X.rDustR 'β' AR 0.001 0.005 in trig tr sampleDur * signum (tRand 'α' (-1) 1 tr))
