-- pinkNoise ; monadic constructor
uid_st_eval (fmap (* 0.05) (pinkNoiseM AR))

-- pinkNoise ; c.f. whiteNoise
uid_st_eval (fmap (* 0.05) (whiteNoiseM AR))

-- pinkNoise ; c.f. whiteNoise
uid_st_eval (fmap (* 0.05) (brownNoiseM AR))

-- pinkNoise ; speaker balance
let n = pinkNoise 'γ' AR * 0.05 in mce2 n n

-- pinkNoise ; speaker balance ; mouse control
let x = mouseX KR 0 1 Linear 0.2
    x' = 1 - x
    n = pinkNoise 'δ' AR * 0.05
in mce2 (n * x') (n * x)

-- pinkNoise ; identifiers & referential transparency ; L = silence, R = pink-noise
mce2 (pinkNoise 'α' AR - pinkNoise 'α' AR) (pinkNoise 'α' AR - pinkNoise 'β' AR) * 0.1

-- pinkNoise ; silence
let n = pinkNoise 'α' AR in (n - n) * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (pinkNoise 'ε' AR)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (pinkNoise 'ζ' AR)
