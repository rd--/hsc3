-- hasher ; noise
hasher (line ar 0 1 1 RemoveSynth) * 0.2

-- hasher ; remap x
let x = mouseX kr 0 10 Linear 0.2
    f = hasher (roundTo x 1) * 300 + 500
in sinOsc ar f 0 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (400,1) 1.0 (hasher (line ar 0 1 1 RemoveSynth))

