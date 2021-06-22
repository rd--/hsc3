-- whiteNoise
let n = whiteNoise 'α' AR * 0.05 in mce2 n n

-- whiteNoise ; monad constructor
uid_st_eval (fmap (* 0.05) (whiteNoiseM AR))

-- whiteNoise ; random filtered noise bursts
let n = whiteNoise 'α' AR
    t = dust 'β' AR (mce [3, 7])
    f = tExpRand 'γ' 20 1800 t
    bw = tExpRand 'δ' 0.001 1 t
    e = decay2 t 0.01 0.2
in resonz (n * e) f bw

-- whiteNoise ; monadic form
uid_st_eval (do
  n <- whiteNoiseM AR
  t <- dustM AR (mce [3, 7])
  f <- tExpRandM 20 1800 t
  bw <- tExpRandM 0.001 1 t
  let e = decay2 t 0.01 0.2
  return (resonz (n * e) f bw))

-- whiteNoise ; monadic form, without using do notation
uid_st_eval (
  whiteNoiseM AR >>= \n ->
  dustM AR (mce [3, 7]) >>= \t ->
  tExpRandM 20 1800 t >>= \f ->
  tExpRandM 0.001 1 t >>= \bw ->
  let e = decay2 t 0.01 0.2
  in return (resonz (n * e) f bw))

-- whiteNoise ; speaker balance
let n = whiteNoise 'α' AR * 0.1 in mce2 n n

-- whiteNoise ; speaker balance
let x = mouseX KR 0.1 2 Linear 0.2
    y = mouseY KR (-90) (-30) Linear 0.2
    l = sinOsc KR x 0
    n = whiteNoise 'α' AR
in pan2 n l (dbAmp y)

-- whiteNoise ; hpz1
hpz1 (whiteNoise 'α' AR * 0.01)

-- whiteNoise ; frequency control
sinOsc AR (whiteNoise 'α' KR * 300 + 500) 0 * 0.1

-- whiteNoise ; monad constructor ; clone
uid_st_eval (fmap (* mce [0.02,0.04]) (clone 2 (whiteNoiseM AR)))

-- whiteNoiseM ; Control.Monad.ap
uid_st_eval
(do x <- return (-) `ap` whiteNoiseM AR `ap` whiteNoiseM AR
    return (x * 0.05))

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.01 (whiteNoise 'γ' AR)
Sound.SC3.Plot.plot_ugen1 0.05 (lpf (whiteNoise 'γ' AR) 500)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (whiteNoise 'α' AR)
UI.ui_sc3_scope_freq (600,400) 0
