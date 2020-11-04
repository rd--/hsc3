-- sampleRate ; the current nominal sample rate of the server
let sr = 48000 {- 44100 -}
    f = mce2 sampleRate sr * 0.01
in sinOsc AR f 0 * 0.1

---- ; server status ; extract nominal and actual sample rates from the synthesis server
import Control.Monad {- base -}
withSC3 (liftM2 (,) serverSampleRateNominal serverSampleRateActual)
