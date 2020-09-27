import Sound.SC3 {- hsc3 -}

-- Sound.SC3.Plot.plot_fn_r1_ln (wrap_hs (-1,1)) (-2,2)
-- Sound.SC3.Plot.plot_fn_r1_ln (\x -> wrap2 x 1) (-2,2)
g_01 = wrap2 (fSinOsc AR 1000 0) (line KR 0 1.01 8 DoNothing) * 0.1
