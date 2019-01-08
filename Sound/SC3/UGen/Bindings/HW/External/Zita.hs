-- | Zita UGen definitions.
--
-- See hsc3/ext/faust to build the SC3 plugin.
module Sound.SC3.UGen.Bindings.HW.External.Zita where

import Sound.SC3.Common.Rate
import Sound.SC3.UGen.Bindings.HW.Construct
import Sound.SC3.UGen.Type

-- | Parameter (name,value) pairs.
--
-- > unwords $ map fst zitaRev_param
zitaRev_param :: [(String, Double)]
zitaRev_param =
  [("in1",0.0)
  ,("in2",0.0)
  ,("in_delay",60.0)
  ,("lf_x",200) -- log, 50, 1000
  ,("low_rt60",3) -- log, 1, 8
  ,("mid_rt60",2) -- log, 1, 8
  ,("hf_damping",6000) -- log, 1500, 24000
  ,("eq1_freq",315) -- log, 40, 2500
  ,("eq1_level",0) -- lin, -15, 15
  ,("eq2_freq",1500) -- log, 160, 10000
  ,("eq2_level",0) -- lin, -15, 15
  ,("dry_wet_mix",0) -- lin, 0, 1
  ,("level",-20) -- lin, -9, 9
  ]

-- | ZitaRev binding.
zitaRev :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
zitaRev in1 in2 in_delay lf_x low_rt60 mid_rt60 hf_damping eq1_freq eq1_level eq2_freq eq2_level dry_wet_mix level = mkFilterR [AR] "FaustZitaRev" [in1,in2,in_delay,lf_x,low_rt60,mid_rt60,hf_damping,eq1_freq,eq1_level,eq2_freq,eq2_level,dry_wet_mix,level] 2
