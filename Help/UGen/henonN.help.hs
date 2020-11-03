-- henonN ; with SC3 default initial parameters
let x = mouseX KR 20 sampleRate Linear 0.1
in henonN AR x 1.4 0.3 0 0 * 0.1

-- henonN ; with mouse-control of parameters
let x = mouseX KR 1 1.4 Linear 0.1
    y = mouseY KR 0 0.3 Linear 0.1
in henonN AR (sampleRate / 4) x y 0 0 * 0.1

-- henonN ; with randomly modulated parameters
let n0 = lfNoise2 'α' KR 1 * 0.20 + 1.20
    n1 = lfNoise2 'β' KR 1 * 0.15 + 0.15
in henonN AR (sampleRate / 8) n0 n1 0 0 * 0.1

-- henonN ; as a frequency control
let x = mouseX KR 1 1.4 Linear 0.1
    y = mouseY KR 0 0.3 Linear 0.1
    f0 = 40
    f = henonN AR f0 x y 0 0 * 800 + 900
in sinOsc AR f 0 * 0.4

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (henonN AR 2500 1.4 0.3 0 0 * 0.1)

---- ; haskell
> import qualified Sound.SC3.Common.Math.Noise as Math {- hsc3 -}
> henon_hs a b = map snd (iterate (Math.henon_f a b) (0.0,0.0))
Sound.SC3.Plot.plot_p1_ln [take 600 (henon_hs 1.4 0.3)]
Sound.SC3.Plot.plot_ugen_nrt (600,1) 1.0 (henonN AR 600 1.4 0.3 0 0)
