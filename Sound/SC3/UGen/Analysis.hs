-- | Signal analysis unit generators.
module Sound.SC3.UGen.Analysis where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Amplitude follower.
amplitude :: Rate -> UGen -> UGen -> UGen -> UGen
amplitude r i at rt = mkOsc r "Amplitude" [i, at, rt] 1

-- | Autocorrelation pitch follower.
pitch :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitch i initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar = mkOsc KR "Pitch" [i, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample, clar] 2

-- | Slope of signal.
slope :: UGen -> UGen
slope i = mkFilter "Slope" [i] 1

-- | Zero crossing frequency follower.
zeroCrossing :: UGen -> UGen
zeroCrossing i = mkFilter "ZeroCrossing" [i] 1

-- Local Variables:
-- truncate-lines:t
-- End:
