-- linCongC ; default SC3 initial parameters
linCongC AR 22050 1.1 0.13 1 0 * 0.1

-- linCongC ; mouse
let x = mouseX KR 20 sampleRate Linear 0.1
in linCongC AR x 1.1 0.13 1 0 * 0.1

-- linCongC ; randomly modulate parameters
let fr = [1,0.1,0.1,0.1]
    [n0,n1,n2,m] = map (\(i,j) -> lfNoise2 i KR j) (zip ['α'..] fr)
    f = n0 * 1e4 + 1e4
    a = n1 * 0.5 + 1.4
    c = n2 * 0.1 + 0.1
in linCongC AR f a c m 0 * 0.1

---- ; drawings ; haskell implementation of equation
import Sound.SC3.Common.Math.Noise {- hsc3 -}
import Sound.SC3.Plot {- hsc3-plot -}
linCong_hs a c m = iterate (linCong_f a c m) 0
plot_p1_ln [take 600 (linCong_hs 1.1 0.13 1.0)]
plot_ugen_nrt (600,1) 1.0 (linCongC AR 600 1.1 0.13 1.0 0.0)
