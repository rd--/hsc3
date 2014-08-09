-- | Machine listening & feature extraction analysis unit generators.
module Sound.SC3.UGen.MachineListening where

import Data.List
import Data.Maybe
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Autocorrelation beat tracker.
beatTrack :: UGen -> UGen -> UGen
beatTrack fft lock = mkOsc KR "BeatTrack" [fft, lock] 4

-- | Template matching beat tracker.
beatTrack2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
beatTrack2 b mf ws pa lk w = mkOsc KR "BeatTrack2" [b, mf, ws, pa, lk, w] 6

-- | Extraction of instantaneous loudness in sones.
loudness :: UGen -> UGen -> UGen -> UGen
loudness fft smask tmask = mkOsc KR "Loudness" [fft, smask, tmask] 1

-- | Onset detector.
onsets :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
onsets c t o r f mg ms wt rw = mkOsc KR "Onsets" [c, t, o, r, f, mg, ms, wt, rw] 1

-- | Key tracker.
keyTrack :: UGen -> UGen -> UGen -> UGen -> UGen
keyTrack fft kd cl _ = mkOsc KR "KeyTrack" [fft, kd, cl] 1

-- | Mel frequency cepstral coefficients.
mfcc :: Int -> UGen -> UGen
mfcc nc b = mkOsc KR "MFCC" [b, constant nc] nc

-- | Spectral Flatness measure.
specFlatness :: UGen -> UGen
specFlatness b = mkOsc KR "SpecFlatness" [b] 1

-- | Find a percentile of FFT magnitude spectrum.
specPcile :: UGen -> UGen -> UGen -> UGen
specPcile b f i = mkOsc KR "SpecPcile" [b, f, i] 1

-- | Spectral centroid.
specCentroid :: UGen -> UGen
specCentroid b = mkOsc KR "SpecCentroid" [b] 1

-- Local Variables:
-- truncate-lines:t
-- End:
