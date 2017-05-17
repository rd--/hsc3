-- | Envelope / UGen.
module Sound.SC3.UGen.Envelope where

import Data.Maybe {- base -}

import qualified Sound.SC3.Common.Envelope as E {- hsc3 -}
import qualified Sound.SC3.UGen.Type as U {- hsc3 -}

envelope_to_ugen :: E.Envelope U.UGen -> U.UGen
envelope_to_ugen =
    let err = error "envGen: bad Envelope"
    in U.mce . fromMaybe err . E.envelope_sc3_array
