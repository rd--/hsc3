import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

f_01 = "/home/rohan/data/audio/metal.wav"

-- > withSC3 (async m_01)
m_01 :: Message
m_01 = b_allocRead 10 f_01 0 0

-- > Sound.SC3.Plot.plotEnvelope [e_01]
e_01 :: Num n => Envelope n
e_01 = envelope [0,1,0] [3,2] [EnvSin,EnvSin]

g_01 =
  let x = mouseX KR 0.5 8 Linear 0.2
      y = mouseY KR 0.05 0.2 Linear 0.2
      e = envGen KR 1 1 0 1 RemoveSynth e_01
  in X.bufGrain AR (impulse KR 10 0) y 10 x e 2

