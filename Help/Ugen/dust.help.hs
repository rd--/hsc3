-- dust
dustId 'α' ar 2 * 0.25

-- dust
let d = xLine kr 20000 2 10 RemoveSynth
in dustId 'β' ar d * 0.15

-- dust ; illustrate monadic constructor
uid_st_eval (fmap (* 0.25) (dustM ar 200))

-- dust ; illustrate monadic constructor
uid_st_eval (fmap (* 0.15) (dustM ar (xLine kr 20000 2 10 RemoveSynth)))

---- ; drawings
Sound.Sc3.Plot.plot_ugen_nrt (48000,64) 0.1 (dustId 'γ' ar 300)
Sound.Sc3.Plot.plot_ugen_nrt (48000,64) 0.1 (dustId 'δ' ar (xLine kr 5000 100 0.1 RemoveSynth))

