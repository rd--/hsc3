-- latoocarfianC ; SC3 default initial parameters
let x = mouseX kr 20 sampleRate Linear 0.1
in latoocarfianC ar x 1 3 0.5 0.5 0.5 0.5 * 0.1

-- latoocarfianC ; randomly modulate all parameters
let [n0,n1,n2,n3] = map (\e -> lfNoise2Id e kr 5) ['α','β','γ','δ']
    f = sampleRate / 4
    a = n0 * 1.5 + 1.5
    b = n1 * 1.5 + 1.5
    c = n2 * 0.5 + 1.5
    d = n3 * 0.5 + 1.5
in latoocarfianC ar f a b c d 0.5 0.5 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (600,1) 1.0 (latoocarfianC ar 600 1.0 3.0 0.5 0.5 0.5 0.5)

---- ; haskell implementation of equation
latoocarfian_hs a b c d = map fst (iterate (Sound.SC3.Common.Math.Noise.latoocarfian_f a b c d) (0.5,0.5))
Sound.SC3.Plot.plot_p1_ln [take 600 (latoocarfian_hs 1.0 3.0 0.5 0.5)]
