import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

g_01 = X.sinGrain AR (impulse KR 10 0) 0.1 (range 440 880 (whiteNoise 'α' KR)) * 0.1

g_02 =
  let x = mouseX KR 0.001 0.2 Linear 0.1
      y = mouseX KR 90 600 Linear 0.1
  in X.sinGrain AR (dust 'β' KR 25) x y * 0.1
