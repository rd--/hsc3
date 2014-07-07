-- | Non-deterministic external 'UGen's.
module Sound.SC3.UGen.External.ID where

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- * SC3plugins/BhobUGens

-- | random walk step
lfBrownNoise0 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise0 z r freq dev dist = mkOscIdR [AR,KR] z r "LFBrownNoise0" [freq,dev,dist] 1

-- | random walk linear interp
lfBrownNoise1 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise1 z r freq dev dist = mkOscIdR [AR,KR] z r "LFBrownNoise1" [freq,dev,dist] 1

-- | random walk cubic interp
lfBrownNoise2 :: ID a => a -> Rate -> UGen -> UGen -> UGen -> UGen
lfBrownNoise2 z r freq dev dist = mkOscIdR [AR,KR] z r "LFBrownNoise2" [freq,dev,dist] 1
