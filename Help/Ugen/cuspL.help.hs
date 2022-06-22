-- cuspL ; default values
cuspL ar 22050 1.0 1.9 0.0 * 0.1

-- cuspL ; vary frequency
let x = mouseX kr 20 (sampleRate / 2) Linear 0.1
in cuspL ar x 1.0 1.99 0 * 0.1

-- cuspL ; mouse-controlled parameters
let x = mouseX kr 0.9 1.1 Linear 0.1
    y = mouseY kr 1.8 2.0 Linear 0.1
in cuspL ar (sampleRate / 4) x y 0 * 0.1

-- cuspL ; frequency control
let x = mouseX kr 0.9 1.1 Linear 0.1
    y = mouseY kr 1.8 2.0 Linear 0.1
    n = cuspL ar 40 x y 0 * 0.3
in sinOsc ar (n * 800 + 900) 0 * 0.1

---- ; drawings
Sound.Sc3.Plot.plot_ugen_nrt (600,1) 1.0 (cuspL ar 600 1.0 1.9 0)

---- ; haskell implementation of equation
cusp_hs a b = iterate (Sound.Sc3.Common.Math.Noise.cusp_f a b) 0
Sound.Sc3.Plot.plot_p1_ln [take 600 (cusp_hs 1.0 1.9)]
