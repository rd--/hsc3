module Sound.SC3.UGen.Record.Pitch (
    Args(..),
    defaults,
    pitch
) where

import Sound.SC3 (UGen)
import qualified Sound.SC3 as SC3

data Args = Args {
    initFreq :: UGen,
    minFreq :: UGen,
    maxFreq :: UGen,
    execFreq :: UGen,
    maxBinsPerOctave :: UGen,
    median :: UGen,
    ampThreshold :: UGen,
    peakThreshold :: UGen,
    downSample :: UGen
}

defaults :: Args
defaults = Args {
    initFreq = 440,
    minFreq = 60,
    maxFreq = 4000,
    execFreq = 100,
    maxBinsPerOctave = 16,
    median = 1,
    ampThreshold = 0.01,
    peakThreshold = 0.5,
    downSample = 1.0
}

pitch :: Args -> UGen -> UGen
pitch args input = SC3.pitch
                    input
                    (initFreq args)
                    (minFreq args)
                    (maxFreq args)
                    (execFreq args)
                    (maxBinsPerOctave args)
                    (median args)
                    (ampThreshold args)
                    (peakThreshold args)
                    (downSample args)
