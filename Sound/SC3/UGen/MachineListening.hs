module Sound.SC3.UGen.MachineListening where

import Data.List
import Data.Maybe
import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen(Constant))
import Sound.SC3.UGen.UGen.Construct (mkOsc)

-- | Autocorrelation beat tracker.
beatTrack :: UGen -> UGen -> UGen
beatTrack fft lock = mkOsc KR "BeatTrack" [fft, lock] 4

-- | Extraction of instantaneous loudness in sones.
loudness :: UGen -> UGen -> UGen -> UGen
loudness fft smask tmask = mkOsc KR "Compander" [fft, smask, tmask] 1

-- | Translate onset type string to constant UGen value.
onsetType :: String -> UGen
onsetType s = Constant (fromIntegral (maybe 3 id (findIndex (== s) t)))
    where t = ["power", "magsum", "complex", "rcomplex", "phase", "wphase", "mkl"]

-- | Onset detector.
onsets :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
onsets c t o r f mg ms w = mkOsc KR "Onsets" [c, t, o, r, f, mg, ms, w] 1

-- | Onset detector with default values for minor parameters.
onsets' :: UGen -> UGen -> UGen -> UGen
onsets' c t o = onsets c t o (f 1.0) (f 0.1) (f 10.0) (f 11.0) (f 1.0)
    where f = Constant

-- | Key tracker.
keyTrack :: UGen -> UGen -> UGen -> UGen -> UGen
keyTrack fft kd cl _ = mkOsc KR "KeyTrack" [fft, kd, cl] 1

-- Local Variables:
-- truncate-lines:t
-- End:
