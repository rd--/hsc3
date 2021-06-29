-- pinkNoise ; plain
pinkNoise 'α' ar * 0.1

-- pinkNoise ; monadic constructor
uid_st_eval (fmap (* 0.05) (pinkNoiseM ar))

-- pinkNoise ; c.f. whiteNoise
uid_st_eval (fmap (* 0.05) (whiteNoiseM ar))

-- pinkNoise ; c.f. whiteNoise
uid_st_eval (fmap (* 0.05) (brownNoiseM ar))

-- pinkNoise ; speaker balance
let n = pinkNoise 'γ' ar * 0.05 in mce2 n n

-- pinkNoise ; speaker balance ; mouse control
let x = mouseX kr 0 1 Linear 0.2
    x' = 1 - x
    n = pinkNoise 'δ' ar * 0.05
in mce2 (n * x') (n * x)

-- pinkNoise ; identifiers & referential transparency ; L = silence, R = pink-noise
mce2 (pinkNoise 'α' ar - pinkNoise 'α' ar) (pinkNoise 'α' ar - pinkNoise 'β' ar) * 0.1

-- pinkNoise ; silence
let n = pinkNoise 'α' ar in (n - n) * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (pinkNoise 'ε' ar)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (pinkNoise 'ζ' ar)
UI.ui_sc3_scope_freq (600,400) 0
