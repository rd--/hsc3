-- | Event and Ctl systems for external control interfaces.
module Sound.SC3.UGen.Event where

import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Bindings.Composite {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}

-- * Event

-- | (gate,x,y,z/force,orientation,radius-x,radius-y,pitch,pitch-x-distance,pitch-y-distance)
type REvent = (UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen)

{- | k0 = index of control bus zero for event system,
    stp = voice index incremennt,
      c = event channel or voice (zero indexed)
-}
rEventAddr :: UGen -> UGen -> UGen -> REvent
rEventAddr k0 stp c =
  let u = in' 10 KR (k0 + (c * stp))
  in case mceChannels u of
       [g,x,y,z,o,rx,ry,p,px,py] -> (g,x,y,z,o,rx,ry,p,px,py)
       _ -> error "rEventAddr?"

-- | c0 = index of voice (channel) zero for event set, n = number of voices (channels)
rEventVoicerAddr :: UGen -> UGen -> UGen -> Int -> (Int -> REvent -> UGen) -> UGen
rEventVoicerAddr k0 stp c0 n f = mce (map (\c -> f c (rEventAddr k0 stp (c0 + constant c))) [0 .. n - 1])

-- | 'rEventAddr' with 'control' inputs for /EventAddr/ and /EventZero/.
rEvent :: REvent
rEvent = rEventAddr (control KR "EventAddr" 13000) (control KR "EventIncr" 10) (control KR "EventZero" 0)

-- | 'rEventVoicerAddr' with 'control' inputs for /EventAddr/ and /EventZero/.
rEventVoicer :: Int -> (Int -> REvent -> UGen) -> UGen
rEventVoicer = rEventVoicerAddr (control KR "EventAddr" 13000) (control KR "EventIncr" 10) (control KR "EventZero" 0)

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

> let x = Data.List.intersect (map fst sc3_control_spec) (map fst kyma_event_value_ranges)
> x == ["beats","boostcut","freq","rate"]
> let f = filter (flip elem x . fst) in zip (f sc3_control_spec) (f kyma_event_value_ranges)
-}
kyma_event_value_ranges :: Fractional t => [(String,(t,t))]
kyma_event_value_ranges =
  [("angle",(-0.5,1.5))
  ,("beats",(1,16))
  ,("boostcut",(-12,12))
  ,("bpm",(0,2000))
  ,("centervalue",(-1,1))
  ,("coef",(-1,1))
  ,("cutoff",(0,10000))
  ,("cycles",(0,100))
  ,("dcoffset",(-1,1))
  ,("direction",(-1,1))
  ,("distance",(-2,2))
  ,("fmntshift",(0.75,1.25))
  ,("freq",(0,10000))
  ,("freqhigh",(8000,24000))
  ,("freqjitter",(0,1))
  ,("freqlow",(0,1000))
  ,("freqmid",(1000,8000))
  ,("gain",(0,10))
  ,("gaindb",(-128,128))
  ,("interval",(-24,24))
  ,("keynumber",(0,127))
  ,("logfreq",(0,127))
  ,("looplength",(-1,1))
  ,("offset",(-1,1))
  ,("onduration",(0,30))
  ,("panner",(-0.5,1.5))
  ,("pitch",(0,127))
  ,("q",(0,10))
  ,("radius",(-2,2))
  ,("rate",(0,2))
  ,("ratio",(0,100))
  ,("scale",(-2,2))
  ,("smallInterval",(0,12))
  ,("steps",(1,128))
  ,("swing",(0,0.5))
  ,("threshdb",(-60,0))
  ,("timeconstant",(0.0001,5))
  ,("timeindex",(-1,1))
  ,("tune",(-1,1))
  ,("upinterval",(0,24))]
