-- | Zita UGen definitions.
--
-- See hsc3/ext/faust to build the SC3 plugin.
module Sound.SC3.UGen.Bindings.HW.External.Zita where

import Sound.SC3.Common.Rate
import Sound.SC3.UGen.Bindings.HW.Construct
import Sound.SC3.UGen.Type

-- | Parameter (name,value) pairs.
zitaRev_param :: Num n => [(String,n,(n,n,String))]
zitaRev_param =
  [("in_delay",60,(20,100,"lin")) -- ms
  ,("lf_x",200,(50,1000,"exp"))
  ,("low_rt60",3,(1,8,"exp"))
  ,("mid_rt60",2,(1,8,"exp"))
  ,("hf_damping",6000,(1500,24000,"exp"))
  ,("eq1_freq",315,(40,2500,"exp"))
  ,("eq1_level",0,(-15,15,"lin"))
  ,("eq2_freq",1500,(160,10000,"exp"))
  ,("eq2_level",0,(-15,15,"lin"))
  ,("dry_wet_mix",0,(0,1,"lin"))
  ,("level",-20,(-9,9,"lin"))
  ]

-- | ZitaRev binding.
zitaRev :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
zitaRev in1 in2 in_delay lf_x low_rt60 mid_rt60 hf_damping eq1_freq eq1_level eq2_freq eq2_level dry_wet_mix level = mkFilterR [ar] "FaustZitaRev" [in1,in2,in_delay,lf_x,low_rt60,mid_rt60,hf_damping,eq1_freq,eq1_level,eq2_freq,eq2_level,dry_wet_mix,level] 2
