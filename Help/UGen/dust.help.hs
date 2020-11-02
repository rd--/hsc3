-- dust
dust 'α' AR 2 * 0.25

-- dust
let d = xLine KR 20000 2 10 RemoveSynth
in dust 'β' AR d * 0.15

-- dust ; illustrate monadic constructor
uid_st_eval (fmap (* 0.25) (dustM AR 200))

-- dust ; illustrate monadic constructor
uid_st_eval (fmap (* 0.15) (dustM AR (xLine KR 20000 2 10 RemoveSynth)))

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 0.1 (dust 'γ' AR 300)
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 0.1 (dust 'δ' AR (xLine KR 5000 100 0.1 RemoveSynth))

