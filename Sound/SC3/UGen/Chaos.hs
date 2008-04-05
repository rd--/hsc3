module Sound.SC3.UGen.Chaos where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct

-- | Chaotic noise.
crackle :: Rate -> UGen -> UGen
crackle r chaosParam = mkOsc r "Crackle" [chaosParam] 1

-- | Cusp map chaotic generator (linear interpolation).
cuspL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspL r freq a b xi = mkOsc r "CuspL" [freq, a, b, xi] 1

-- | Cusp map chaotic generator (no interpolation).
cuspN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
cuspN r freq a b xi = mkOsc r "CuspN" [freq, a, b, xi] 1

-- | Feedback sine with chaotic phase indexing (cubic interpolation).
fbSineC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineC r freq im fb a c xi yi = mkOsc r "FBSineC" [freq, im, fb, a, c, xi, yi] 1

-- | Feedback sine with chaotic phase indexing (linear interpolation).
fbSineL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineL r freq im fb a c xi yi = mkOsc r "FBSineL" [freq, im, fb, a, c, xi, yi] 1

-- | Feedback sine with chaotic phase indexing (no interpolation).
fbSineN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fbSineN r freq im fb a c xi yi = mkOsc r "FBSineN" [freq, im, fb, a, c, xi, yi] 1

-- | Henon map chaotic generator (cubic interpolation).
henonC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonC r freq a b x0 x1 = mkOsc r "HenonC" [freq, a, b, x0, x1] 1

-- | Henon map chaotic generator (linear interpolation).
henonL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonL r freq a b x0 x1 = mkOsc r "HenonL" [freq, a, b, x0, x1] 1

-- | Henon map chaotic generator (no interpolation).
henonN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
henonN r freq a b x0 x1 = mkOsc r "HenonN" [freq, a, b, x0, x1] 1

-- | Latoocarfian chaotic function (cubic interpolation).
latoocarfianC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianC r f a b c d xi yi = mkOsc r "LatoocarfianC" [f, a, b, c, d, xi, yi] 1

-- | Latoocarfian chaotic function (linear interpolation).
latoocarfianL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianL r f a b c d xi yi = mkOsc r "LatoocarfianL" [f, a, b, c, d, xi, yi] 1

-- | Latoocarfian chaotic function (no interpolation).
latoocarfianN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
latoocarfianN r f a b c d xi yi = mkOsc r "LatoocarfianN" [f, a, b, c, d, xi, yi] 1

-- | Linear congruential chaotic generator (cubic interpolation).
linCongC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongC r f a c m xi = mkOsc r "LinCongC" [f, a, c, m, xi] 1

-- | Linear congruential chaotic generator (linear interpolation).
linCongL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongL r f a c m xi = mkOsc r "LinCongL" [f, a, c, m, xi] 1

-- | Linear congruential chaotic generator (no interpolation).
linCongN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linCongN r f a c m xi = mkOsc r "LinCongN" [f, a, c, m, xi] 1

-- | Lorenz chaotic generator (linear interpolation).
lorenzL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lorenzL rate freq s r b h xi yi zi = mkOsc rate "LorenzL" [freq, s, r, b, h, xi, yi, zi] 1

-- | General quadratic map chaotic generator (cubic interpolation).
quadC :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadC r freq a b c xi = mkOsc r "QuadC" [freq, a, b, c, xi] 1

-- | General quadratic map chaotic generator (linear interpolation).
quadL :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadL r freq a b c xi = mkOsc r "QuadL" [freq, a, b, c, xi] 1

-- | General quadratic map chaotic generator (no interpolation).
quadN :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadN r freq a b c xi = mkOsc r "QuadN" [freq, a, b, c, xi] 1

-- Local Variables:
-- truncate-lines:t
-- End:
