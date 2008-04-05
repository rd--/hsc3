module Sound.SC3.UGen.MachineListening where

import Data.List
import Data.Maybe
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Math ()
import Sound.SC3.UGen.UGen.Construct

-- | Autocorrelation beat tracker.
beatTrack :: UGen -> UGen -> UGen
beatTrack fft lock = mkOsc KR "BeatTrack" [fft, lock] 4

-- | Template matching beat tracker.
beatTrack2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
beatTrack2 b mf ws pa lk w = mkOsc KR "BeatTrack2" [b, mf, ws, pa, lk, w] 6

-- | Extraction of instantaneous loudness in sones.
loudness :: UGen -> UGen -> UGen -> UGen
loudness fft smask tmask = mkOsc KR "Compander" [fft, smask, tmask] 1

-- | Translate onset type string to constant UGen value.
onsetType :: String -> UGen
onsetType s = fromIntegral (maybe 3 id (findIndex (== s) t))
    where t = ["power", "magsum", "complex", "rcomplex", "phase", "wphase", "mkl"]

-- | Onset detector.
onsets :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
onsets c t o r f mg ms wt rw = mkOsc KR "Onsets" [c, t, o, r, f, mg, ms, wt, rw] 1

-- | Onset detector with default values for minor parameters.
onsets' :: UGen -> UGen -> UGen -> UGen
onsets' c t o = onsets c t o 1 0.1 10 11 1 0

-- | Key tracker.
keyTrack :: UGen -> UGen -> UGen -> UGen -> UGen
keyTrack fft kd cl _ = mkOsc KR "KeyTrack" [fft, kd, cl] 1

-- | Mel frequency cepstral coefficients.
mfcc :: Int -> UGen -> UGen
mfcc nc b = mkOsc KR "MFCC" [b, constant nc] nc

specFlatness :: UGen -> UGen
specFlatness b = mkOsc KR "SpecFlatness" [b] 1

specPcile :: UGen -> UGen -> UGen -> UGen
specPcile b f i = mkOsc KR "SpecPcile" [b, f, i] 1

specCentroid :: UGen -> UGen
specCentroid b = mkOsc KR "SpecFlatness" [b] 1

-- Local Variables:
-- truncate-lines:t
-- End:
