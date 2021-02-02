-- | Bindings to unit generators in sc3-plugins.
module Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins where

import Sound.SC3.Common.Rate
import qualified Sound.SC3.UGen.Bindings.HW.Construct as C
import Sound.SC3.UGen.Type
import qualified Sound.SC3.UGen.UGen as U
import qualified Sound.SC3.UGen.Bindings.DB.External as X

-- | Convert frequency value to value appropriate for AY tone inputs.
ayFreqToTone :: Fractional a => a -> a
ayFreqToTone f = 110300 / (f - 0.5)

-- | LADSPA plugins inside SuperCollider.
ladspa :: Int -> Rate -> UGen -> [UGen] -> UGen
ladspa nc rt k z = C.mkOsc rt "LADSPA" (constant nc : k : z) nc

-- | Lookup index of STK instrument by name.
stkAt :: (Num t,Enum t) => String -> t
stkAt nm =
  let nm_seq = ["Clarinet", "BlowHole", "Saxofony", "Flute", "Brass"
               ,"BlowBotl", "Bowed", "Plucked", "StifKarp", "Sitar", "Mandolin"
               ,"Rhodey", "Wurley", "TubeBell", "HevyMetl", "PercFlut"
               ,"BeeThree", "FMVoices", "VoicForm", "Moog", "Simple", "Drummer"
               ,"BandedWG", "Shakers", "ModalBar", "Mesh2D", "Resonate", "Whistle"]
      tbl = zip nm_seq [0..]
  in case lookup nm tbl of
       Just ix -> ix
       Nothing -> error "stkAt: unknown instr"

-- | freq=220, gate=1, onamp=1, offamp=1, bowpressure=64, bowposition=64, vibfreq=64, vibgain=64, loudness=64
stkBowedI :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stkBowedI rt freq gate_ onamp offamp bowpressure bowposition vibfreq vibgain loudness =
  let args = mce [2,bowpressure,4,bowposition,11,vibfreq,1,vibgain,128,loudness]
  in X.stkInst rt (stkAt "Bowed") freq gate_ onamp offamp args

-- | Wrapping Synthesis toolkit.
--
--  StkGlobals [AR] showWarnings=0.0 printErrors=0.0 rawfilepath=0.0
stkGlobals :: Rate -> UGen -> UGen -> UGen -> UGen
stkGlobals rate showWarnings printErrors rawfilepath =
  mkUGen Nothing [AR] (Left rate) "StkGlobals" ([showWarnings,printErrors] ++ U.unpackLabel False rawfilepath) Nothing 1 (Special 0) NoId
