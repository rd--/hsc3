-- impulse ; note SC2 had no phase input
impulse ar 800 0 * 0.1

-- impulse
let f = xLine kr 800 10 5 RemoveSynth
in impulse ar f 0.0 * 0.1

-- impulse
let f = mouseY kr 4 8 Linear 0.1
    x = mouseX kr 0 1 Linear 0.1
in impulse ar f (mce [0,x]) * 0.1

-- impulse ; frequency 0 returns a single impulse
decay (impulse ar 0 0) 1 * brownNoiseId 'α' ar * 0.1

-- impulse ; non-bandlimited
let f = sinOsc ar 0.25 0.0 * 2500.0 + 2505.0
in impulse ar f 0.0 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 1.0 (impulse ar 10 0)
