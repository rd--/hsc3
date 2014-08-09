module Sound.SC3.UGen.Transform where

import Sound.SC3.UGen.Bindings
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Envelope
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Generate an 'envGen' UGen with @fadeTime@ and @gate@ controls.
--
-- > import Sound.SC3
-- > audition (out 0 (makeFadeEnv 1 * sinOsc AR 440 0 * 0.1))
-- > withSC3 (send (n_set1 (-1) "gate" 0))
makeFadeEnv :: Real n => n -> UGen
makeFadeEnv fadeTime =
    let dt = control KR "fadeTime" (realToFrac fadeTime)
        gate_ = control KR "gate" 1
        startVal = dt <=* 0
        env = Envelope [startVal,1,0] [1,1] [EnvLin,EnvLin] (Just 1) Nothing
    in envGen KR gate_ 1 0 dt RemoveSynth env

-- | If @z@ isn't a sink node, multiply by 'makeFadeEnv' and route to
-- an @out@ node writing to @bus@.
--
-- > import Sound.SC3
-- > audition (wrapOut (sinOsc AR 440 0 * 0.1) 1)
-- > withSC3 (send (n_set1 (-1) "gate" 0))
wrapOut :: Real n => n -> UGen -> UGen
wrapOut fadeTime z =
    let bus = control KR "out" 0
    in if isSink z
       then z
       else out bus (z * makeFadeEnv fadeTime)
