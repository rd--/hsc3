-- cuspL ; default values
cuspL AR 22050 1.0 1.9 0.0 * 0.1

-- cuspL ; vary frequency
let x = mouseX KR 20 (sampleRate / 2) Linear 0.1
in cuspL AR x 1.0 1.99 0 * 0.1

-- cuspL ; mouse-controlled parameters
let x = mouseX KR 0.9 1.1 Linear 0.1
    y = mouseY KR 1.8 2.0 Linear 0.1
in cuspL AR (sampleRate / 4) x y 0 * 0.1

-- cuspL ; frequency control
let x = mouseX KR 0.9 1.1 Linear 0.1
    y = mouseY KR 1.8 2.0 Linear 0.1
    n = cuspL AR 40 x y 0 * 0.3
in sinOsc AR (n * 800 + 900) 0 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (600,1) 1.0 (cuspL AR 600 1.0 1.9 0)

---- ; haskell implementation of equation
cusp_hs a b = iterate (Sound.SC3.Common.Math.Noise.cusp_f a b) 0
Sound.SC3.Plot.plot_p1_ln [take 600 (cusp_hs 1.0 1.9)]
