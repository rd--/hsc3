-- lfGauss ; modulating duration
let d = xLine KR 0.1 0.001 10 DoNothing
in lfGauss AR d 0.03 0 Loop DoNothing * 0.2

-- lfGauss ; modulating width, freq=60Hz
let w = xLine KR 0.1 0.001 10 DoNothing
in lfGauss AR (1 / 60) w 0 Loop DoNothing * 0.2

-- lfGauss ; x=frequency, y=width ; note alisasing at high frequencies
let d = mouseX KR (1/8000) 0.1 Exponential 0.2
    w = mouseY KR 0.001 0.1 Exponential 0.2
in lfGauss AR d w 0 Loop DoNothing * 0.1

-- lfGauss ; amplitude modulator
let d = mouseX KR 1 0.001 Exponential 0.2
    g = lfGauss AR d 0.1 0 Loop DoNothing
    o = sinOsc AR 1000 0
in g * o * 0.1

-- lfGauss ; modulate iphase
let ph = mouseX KR (-1) 1 Linear 0.2
    g = lfGauss AR 0.001 0.2 (mce2 0 ph) Loop DoNothing
in mix g * 0.2

-- lfGauss ; very small width approach dirac function
let w = sampleDur * mouseX KR 10 3000 Exponential 0.2
in lfGauss AR 0.01 w 0 Loop DoNothing * 0.2

-- lfGauss ; dur and width can be modulated at audio rate
let x = mouseX KR 2 1000 Exponential 0.2
    d = range 0.0006 0.01 (sinOsc AR x 0 * mce2 1 1.1)
    w = range 0.01 0.3 (sinOsc AR (0.5 * (mce2 1 1.1)) 0)
in lfGauss AR d w 0 Loop DoNothing * 0.2

-- lfGauss ; several frequencies and widths combined
let x = mouseX KR 1 0.07 Exponential 0.2
    y = mouseY KR 1 3 Linear 0.2
    g = lfGauss AR x (y ** mce [-1,-2 .. -6]) 0 Loop DoNothing
    o = sinOsc AR (200 * (1.3 ** mce [0..5])) 0
in mix (g * o) * 0.1

---- ; drawings
let plot_f tm du ph = Sound.SC3.Plot.plot_ugen1 tm (lfGauss AR du 0.12 ph Loop DoNothing)
plot_f 0.1 0.1 0 -- centred
plot_f 0.1 0.1 (-1) -- shifting left
plot_f 0.1 0.1 2 -- moving further away from the center
plot_f 0.3 0.065 0 -- several grains
