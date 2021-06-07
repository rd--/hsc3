-- | Event and Ctl systems for external control interfaces.
module Sound.SC3.UGen.Event where

import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Bindings.Composite {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}

-- * Event

-- | (w/gate,x,y,z/force,orientation,radius-x,radius-y,pitch,pitch-x,pitch-y)
type REvent t = (t,t,t,t,t,t,t,t,t,t)

-- | Translate list to REvent.
rEvent_from_list :: Num t => [t] -> REvent t
rEvent_from_list l =
  case l of
    [w,x,y,z,o,rx,ry,p,px,py] -> (w,x,y,z,o,rx,ry,p,px,py)
    _ -> error "rEvent_from_list?"

{- | k0 = index of control bus zero for event system,
    stp = voice index incremennt,
      c = event channel or voice (zero indexed)
-}
rEventAddr :: UGen -> UGen -> UGen -> REvent UGen
rEventAddr k0 stp c =
  let u = in' 10 KR (k0 + (c * stp))
  in rEvent_from_list (mceChannels u)

-- | c0 = index of voice (channel) zero for event set, n = number of voices (channels)
rEventVoicerAddr :: UGen -> UGen -> UGen -> Int -> (Int -> REvent UGen -> UGen) -> UGen
rEventVoicerAddr k0 stp c0 n f = mce (map (\c -> f c (rEventAddr k0 stp (c0 + constant c))) [0 .. n - 1])

-- | 'rEventAddr' with 'control' inputs for /eventAddr/, /eventIncr/ and /eventZero/.
rEvent :: REvent UGen
rEvent = rEventAddr (control KR "eventAddr" 13000) (control KR "eventIncr" 10) (control KR "eventZero" 0)

-- | 'rEventVoicerAddr' with 'control' inputs for /eventAddr/, /eventIncr/ and /eventZero/.
rEventVoicer :: Int -> (Int -> REvent UGen -> UGen) -> UGen
rEventVoicer = rEventVoicerAddr (control KR "eventAddr" 13000) (control KR "eventIncr" 10) (control KR "eventZero" 0)

{- | Given /g/ and /p/ fields of an 'REvent' derive a 'gateReset' from g
and a trigger derived from monitoring /g/ and /p/ for changed values.
-}
rEventGateReset :: UGen -> UGen -> (UGen, UGen)
rEventGateReset g p = let tr = changed p 0.01 + changed g 0.01 in (gateReset g tr,tr)

-- * Ctl

-- | Sequence of 16 continous controller inputs in range (0-1).
type RCtl = (UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen)

-- | k0 = index of control bus zero for ctl system, c = ctl channel or voice (zero indexed)
rCtlAddr :: UGen -> UGen -> RCtl
rCtlAddr k0 c =
  let u = in' 16 KR (k0 + (c * 16))
  in case mceChannels u of
       [cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7,cc8,cc9,cc10,cc11,cc12,cc13,cc14,cc15] ->
         (cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7,cc8,cc9,cc10,cc11,cc12,cc13,cc14,cc15)
       _ -> error "rCtlAddr?"

-- | c0 = index of voice (channel) zero for ctl set, n = number of voices (channels)
rCtlVoicerAddr :: UGen -> UGen -> Int -> (Int -> RCtl -> UGen) -> UGen
rCtlVoicerAddr k0 c0 n f = mce (map (\c -> f c (rCtlAddr k0 (c0 + constant c))) [0 .. n - 1])

-- | 'rCtlAddr' with 'control' inputs for /CtlAddr/ and /CtlZero/.
rCtl :: RCtl
rCtl = rCtlAddr (control KR "CtlAddr" 11000) (control KR "CtlZero" 0)

-- | 'rCtlVoicerAddr' with 'control' inputs for /CtlAddr/ and /CtlZero/.
rCtlVoicer :: Int -> (Int -> RCtl -> UGen) -> UGen
rCtlVoicer = rCtlVoicerAddr (control KR "CtlAddr" 11000) (control KR "CtlZero" 0)

-- | First eight elements of RCtl.
type RCtl8 = (UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen)

-- | Select first eight elements of RCtl.
rCtl_to_rCtl8 :: RCtl -> RCtl8
rCtl_to_rCtl8 (cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7,_,_,_,_,_,_,_,_) = (cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7)

-- | 'rCtlVoicer' of 'rCtl_to_rCtl8'
rCtl8Voicer :: Int -> (Int -> RCtl8 -> UGen) -> UGen
rCtl8Voicer k0 f = rCtlVoicer k0 (\n c -> f n (rCtl_to_rCtl8 c))

-- * Names

{- | See SCClassLibrary/Common/Control/Spec:ControlSpec.initClass

"ControlSpec defines the range and curve of a control"
-}
sc3_control_spec :: Fractional t => [(String,(t,t,String))]
sc3_control_spec =
  [("amp",(0,1,"amp"))
  ,("beats",(0,20,"lin"))
  ,("bipolar",(-1,1,"lin"))
  ,("boostcut",(-20,20,"lin"))
  ,("db",(-180,0,"db"))
  ,("delay",(0.0001,1,"exp"))
  ,("detune",(-20,20,"lin"))
  ,("freq",(20,20000,"exp"))
  ,("lofreq",(0.1,100,"exp"))
  ,("midfreq",(25,4200,"exp"))
  ,("midi",(0,127,"lin"))
  ,("midinote",(0,127,"lin"))
  ,("midivelocity",(1,127,"lin"))
  ,("pan",(-1,1,"lin"))
  ,("phase",(0,6.28318,"lin"))
  ,("rate",(0.125,8,"exp"))
  ,("rq",(0.001,2,"exp"))
  ,("unipolar",(0,1,"lin"))
  ,("widefreq",(0.1,20000,"exp"))]

{- | See Kyma X Revealed, p.403

"The following EventValue names are associated with initial ranges
other than (0,1). EventValue names are not case-sensitive."

This list adds curve specifiers as strings.

> let x = Data.List.intersect (map fst sc3_control_spec) (map fst kyma_event_value_ranges)
> x == ["beats","boostcut","freq","rate"]
> let c z = let (p,q) = unzip z in let f i = filter (flip elem i . fst) in zip (f p sc3_control_spec) (f q kyma_event_value_ranges)
> c (zip x x)

> c [("lofreq","freqlow"),("midfreq","freqmid")]
> lookup "freqhigh" kyma_event_value_ranges
-}
kyma_event_value_ranges :: Fractional t => [(String,(t,t,String))]
kyma_event_value_ranges =
  [("angle",(-0.5,1.5,"lin"))
  ,("beats",(1,16,"lin"))
  ,("boostcut",(-12,12,"lin"))
  ,("bpm",(0,2000,"lin"))
  ,("centervalue",(-1,1,"lin"))
  ,("coef",(-1,1,"lin"))
  ,("cutoff",(0,10000,"exp"))
  ,("cycles",(0,100,"lin"))
  ,("dcoffset",(-1,1,"lin"))
  ,("direction",(-1,1,"lin"))
  ,("distance",(-2,2,"lin"))
  ,("fmntshift",(0.75,1.25,"lin"))
  ,("freq",(0,10000,"exp"))
  ,("freqhigh",(8000,24000,"exp")) -- sampleRate / 2
  ,("freqjitter",(0,1,"lin"))
  ,("freqlow",(0,1000,"exp"))
  ,("freqmid",(1000,8000,"exp"))
  ,("gain",(0,10,"amp"))
  ,("gaindb",(-128,128,"lin"))
  ,("interval",(-24,24,"lin"))
  ,("keynumber",(0,127,"lin"))
  ,("logfreq",(0,127,"lin"))
  ,("looplength",(-1,1,"lin"))
  ,("offset",(-1,1,"lin"))
  ,("onduration",(0,30,"lin"))
  ,("panner",(-0.5,1.5,"lin"))
  ,("pitch",(0,127,"lin"))
  ,("q",(0,10,"lin"))
  ,("radius",(-2,2,"lin"))
  ,("rate",(0,2,"lin"))
  ,("ratio",(0,100,"lin"))
  ,("scale",(-2,2,"lin"))
  ,("smallInterval",(0,12,"lin"))
  ,("steps",(1,128,"lin"))
  ,("swing",(0,0.5,"lin"))
  ,("threshdb",(-60,0,"lin"))
  ,("timeconstant",(0.0001,5,"lin"))
  ,("timeindex",(-1,1,"lin"))
  ,("tune",(-1,1,"lin"))
  ,("upinterval",(0,24,"lin"))]
