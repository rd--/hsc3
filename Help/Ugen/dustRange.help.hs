-- dustRange ; inter-offset times generated randomly in range (seconds) ; uniform distribution
--       ; audio rate impulse train of varying amplitude (min = max)
mce2 (X.dustRangeId 'α' ar 0.1 0.1 * 0.1) (impulse ar 10 0 * 0.1)

-- dustRange ; mean iot=0.0005 is equivalent to density=2000 at dust, dustRange spacing is more uniform
mce2 (X.dustRangeId 'β' ar 0.0001 0.001 * 0.1) (dustId 'γ' ar 2000 * 0.05)

-- dustRange ; velvet noise (approx.)
let iot = 20 / sampleRate
    x = mouseX kr 1 16 Exponential 0.1
    d = X.dustRangeId 'α' ar (iot / x) (iot * x)
    s = tRandId 'β' (-1) 1 d
in trig d sampleDur * signum s * 0.1

---- ; drawings
import Sound.Sc3.Plot {- hsc3-plot -}
plot_ugen_nrt (48000,64) 1.0 (X.dustRangeId 'δ' ar 0.1 0.1)
plot_ugen_nrt (48000,64) 1.0 (X.dustRangeId 'ε' ar 0.1 0.1 + impulse ar 10 0 * 0.5)
plot_ugen_nrt (48000,64) 0.1 (X.dustRangeId 'ζ' ar 0.0001 0.001)
plot_ugen_nrt (48000,64) 0.1 (dustId 'ζ' ar 2000)
plot_ugen_nrt (48000,64) 0.1 (let tr = X.dustRangeId 'β' ar 0.001 0.005 in trig tr sampleDur * signum (tRandId 'α' (-1) 1 tr))
