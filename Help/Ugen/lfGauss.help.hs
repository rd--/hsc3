-- lfGauss ; modulating duration
let d = xLine kr 0.1 0.001 10 DoNothing
in lfGauss ar d 0.03 0 Loop DoNothing * 0.2

-- lfGauss ; modulating width, freq=60Hz
let w = xLine kr 0.1 0.001 10 DoNothing
in lfGauss ar (1 / 60) w 0 Loop DoNothing * 0.2

-- lfGauss ; x=frequency, y=width ; note alisasing at high frequencies
let d = mouseX kr (1/8000) 0.1 Exponential 0.2
    w = mouseY kr 0.001 0.1 Exponential 0.2
in lfGauss ar d w 0 Loop DoNothing * 0.1

-- lfGauss ; amplitude modulator
let d = mouseX kr 1 0.001 Exponential 0.2
    g = lfGauss ar d 0.1 0 Loop DoNothing
    o = sinOsc ar 1000 0
in g * o * 0.1

-- lfGauss ; modulate iphase
let ph = mouseX kr (-1) 1 Linear 0.2
    g = lfGauss ar 0.001 0.2 (mce2 0 ph) Loop DoNothing
in mix g * 0.2

-- lfGauss ; very small width approach dirac function
let w = sampleDur * mouseX kr 10 3000 Exponential 0.2
in lfGauss ar 0.01 w 0 Loop DoNothing * 0.2

-- lfGauss ; dur and width can be modulated at audio rate
let x = mouseX kr 2 1000 Exponential 0.2
    d = range 0.0006 0.01 (sinOsc ar x 0 * mce2 1 1.1)
    w = range 0.01 0.3 (sinOsc ar (0.5 * (mce2 1 1.1)) 0)
in lfGauss ar d w 0 Loop DoNothing * 0.2

-- lfGauss ; several frequencies and widths combined
let x = mouseX kr 1 0.07 Exponential 0.2
    y = mouseY kr 1 3 Linear 0.2
    g = lfGauss ar x (y ** mce [-1,-2 .. -6]) 0 Loop DoNothing
    o = sinOsc ar (200 * (1.3 ** mce [0..5])) 0
in mix (g * o) * 0.1

---- ; drawings
let plot_f tm du ph = Sound.Sc3.Plot.plot_ugen1 tm (lfGauss ar du 0.12 ph Loop DoNothing)
plot_f 0.1 0.1 0 -- centred
plot_f 0.1 0.1 (-1) -- shifting left
plot_f 0.1 0.1 2 -- moving further away from the center
plot_f 0.3 0.065 0 -- several grains
