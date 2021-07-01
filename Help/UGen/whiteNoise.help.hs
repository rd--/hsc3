-- whiteNoise
let n = whiteNoiseId 'α' ar * 0.05 in mce2 n n

-- whiteNoise ; monad constructor
uid_st_eval (fmap (* 0.05) (whiteNoiseM ar))

-- whiteNoise ; random filtered noise bursts
let n = whiteNoiseId 'α' ar
    t = dustId 'β' ar (mce [3, 7])
    f = tExpRandId 'γ' 20 1800 t
    bw = tExpRandId 'δ' 0.001 1 t
    e = decay2 t 0.01 0.2
in resonz (n * e) f bw

-- whiteNoise ; monadic form
uid_st_eval (do
  n <- whiteNoiseM ar
  t <- dustM ar (mce [3, 7])
  f <- tExpRandM 20 1800 t
  bw <- tExpRandM 0.001 1 t
  let e = decay2 t 0.01 0.2
  return (resonz (n * e) f bw))

-- whiteNoise ; monadic form, without using do notation
uid_st_eval (
  whiteNoiseM ar >>= \n ->
  dustM ar (mce [3, 7]) >>= \t ->
  tExpRandM 20 1800 t >>= \f ->
  tExpRandM 0.001 1 t >>= \bw ->
  let e = decay2 t 0.01 0.2
  in return (resonz (n * e) f bw))

-- whiteNoise ; speaker balance
let n = whiteNoiseId 'α' ar * 0.1 in mce2 n n

-- whiteNoise ; speaker balance
let x = mouseX kr 0.1 2 Linear 0.2
    y = mouseY kr (-90) (-30) Linear 0.2
    l = sinOsc kr x 0
    n = whiteNoiseId 'α' ar
in pan2 n l (dbAmp y)

-- whiteNoise ; hpz1
hpz1 (whiteNoiseId 'α' ar * 0.01)

-- whiteNoise ; frequency control
sinOsc ar (whiteNoiseId 'α' kr * 300 + 500) 0 * 0.1

-- whiteNoise ; monad constructor ; clone
uid_st_eval (fmap ((* mce [0.02,0.04]) . mce) (replicateM 2 (whiteNoiseM ar)))

-- whiteNoiseM ; Control.Monad.ap
uid_st_eval
(do x <- return (-) `ap` whiteNoiseM ar `ap` whiteNoiseM ar
    return (x * 0.05))

-- white noise ; noise
(whiteNoise ar - whiteNoise ar) * 0.05

-- whiteNoise ; silence
let n = whiteNoise ar in (n - n) * 0.05

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.01 (whiteNoiseId 'γ' ar)
Sound.SC3.Plot.plot_ugen1 0.05 (lpf (whiteNoiseId 'γ' ar) 500)
Sound.SC3.Plot.FFT.plot_ugen_fft1 0.1 (whiteNoiseId 'α' ar)
UI.ui_sc3_scope_freq (600,400) 0
