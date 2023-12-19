-- | Bindings to unit generators in sc3-plugins.
module Sound.Sc3.Ugen.Bindings.Hw.External.Sc3_Plugins where

import Sound.Sc3.Common.Rate

import qualified Sound.Sc3.Ugen.Bindings.Db.External as X
import qualified Sound.Sc3.Ugen.Bindings.Hw.Construct as C
import Sound.Sc3.Ugen.Types
import qualified Sound.Sc3.Ugen.Util as Util

-- | Convert frequency value to value appropriate for Ay tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- | Ladspa plugins inside SuperCollider.
ladspa :: Int -> Rate -> Ugen -> [Ugen] -> Ugen
ladspa nc rt k z = C.mkOsc rt "LADSPA" (constant nc : k : z) nc

-- | Lookup index of Stk instrument by name.
stkAt :: (Num t, Enum t) => String -> t
stkAt nm =
  let nm_seq =
        [ "Clarinet"
        , "BlowHole"
        , "Saxofony"
        , "Flute"
        , "Brass"
        , "BlowBotl"
        , "Bowed"
        , "Plucked"
        , "StifKarp"
        , "Sitar"
        , "Mandolin"
        , "Rhodey"
        , "Wurley"
        , "TubeBell"
        , "HevyMetl"
        , "PercFlut"
        , "BeeThree"
        , "FMVoices"
        , "VoicForm"
        , "Moog"
        , "Simple"
        , "Drummer"
        , "BandedWG"
        , "Shakers"
        , "ModalBar"
        , "Mesh2D"
        , "Resonate"
        , "Whistle"
        ]
      tbl = zip nm_seq [0 ..]
  in case lookup nm tbl of
      Just ix -> ix
      Nothing -> error "stkAt: unknown instr"

-- | freq=220, gate=1, onamp=1, offamp=1, bowpressure=64, bowposition=64, vibfreq=64, vibgain=64, loudness=64
stkBowedI :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
stkBowedI rt freq gate_ onamp offamp bowpressure bowposition vibfreq vibgain loudness =
  let args = mce [2, bowpressure, 4, bowposition, 11, vibfreq, 1, vibgain, 128, loudness]
  in X.stkInst rt (stkAt "Bowed") freq gate_ onamp offamp args

{- | Wrapping Synthesis toolkit.

 StkGlobals [ar] showWarnings=0.0 printErrors=0.0 rawfilepath=0.0
-}
stkGlobals :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
stkGlobals rate showWarnings printErrors rawfilepath =
  mkUgen Nothing [ar] (Left rate) "StkGlobals" ([showWarnings, printErrors] ++ Util.unpackLabel False rawfilepath) Nothing 1 (Special 0) NoId
