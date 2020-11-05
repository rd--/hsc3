-- dustR ; inter-offset times generated randomly in range (seconds) ; uniform distribution
--       ; audio rate impulse train of varying amplitude (min = max)
mce2 (X.dustR 'α' AR 0.1 0.1 * 0.1) (impulse AR 10 0 * 0.1)

-- dustR ; mean iot=0.0005 is equivalent to density=2000 at dust, dustR spacing is more uniform
mce2 (X.dustR 'β' AR 0.0001 0.001 * 0.1) (dust 'γ' AR 2000 * 0.05)

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen_nrt (48000,64) 1.0 (X.dustR 'δ' AR 0.1 0.1)
plot_ugen_nrt (48000,64) 1.0 (X.dustR 'ε' AR 0.1 0.1 + impulse AR 10 0 * 0.5)
plot_ugen_nrt (48000,64) 0.1 (X.dustR 'ζ' AR 0.0001 0.001)
plot_ugen_nrt (48000,64) 0.1 (dust 'ζ' AR 2000)
