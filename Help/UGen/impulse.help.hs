-- impulse ; note SC2 had no phase input
impulse AR 800 0 * 0.1

-- impulse
let f = xLine KR 800 10 5 RemoveSynth
in impulse AR f 0.0 * 0.1

-- impulse
let f = mouseY KR 4 8 Linear 0.1
    x = mouseX KR 0 1 Linear 0.1
in impulse AR f (mce [0,x]) * 0.1

-- impulse ; frequency 0 returns a single impulse
decay (impulse AR 0 0) 1 * brownNoise 'α' AR * 0.1

-- impulse ; non-bandlimited
let f = sinOsc AR 0.25 0.0 * 2500.0 + 2505.0
in impulse AR f 0.0 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 1.0 (impulse AR 10 0)
