-- | Event and Ctl systems for external control interfaces.
module Sound.SC3.UGen.Event where

import Data.List {- base -}

import Data.List.Split {- split -}

import Sound.SC3.Common.Math {- hsc3 -}
import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Bindings.Composite {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}

-- * Event

-- | (w/gate,x,y,z/force,orientation,radius-x,radius-y,pitch,pitch-x,pitch-y)
type Event t = (t,t,t,t,t,t,t,t,t,t)

-- | Translate list to Event.
event_from_list :: Num t => [t] -> Event t
event_from_list l =
  case l of
    [w,x,y,z,o,rx,ry,p,px,py] -> (w,x,y,z,o,rx,ry,p,px,py)
    _ -> error "event_from_list?"

{- | k0 = index of control bus zero for event system,
    stp = voice index incremennt,
      c = event channel or voice (zero indexed)
-}
eventAddr :: UGen -> UGen -> UGen -> Event UGen
eventAddr k0 stp c =
  let u = in' 10 ControlRate (k0 + (c * stp))
  in event_from_list (mceChannels u)

-- | c0 = index of voice (channel) zero for event set, n = number of voices (channels)
eventVoicerAddr :: UGen -> UGen -> UGen -> Int -> (Int -> Event UGen -> UGen) -> UGen
eventVoicerAddr k0 stp c0 n f = mce (map (\c -> f c (eventAddr k0 stp (c0 + constant c))) [0 .. n - 1])

-- | 'eventAddr' with 'control' inputs for /eventAddr/, /eventIncr/ and /eventZero/.
event :: Event UGen
event = eventAddr (control ControlRate "eventAddr" 13000) (control ControlRate "eventIncr" 10) (control ControlRate "eventZero" 0)

-- | 'eventVoicerAddr' with 'control' inputs for /eventAddr/, /eventIncr/ and /eventZero/.
eventVoicer :: Int -> (Int -> Event UGen -> UGen) -> UGen
eventVoicer = eventVoicerAddr (control ControlRate "eventAddr" 13000) (control ControlRate "eventIncr" 10) (control ControlRate "eventZero" 0)

{- | Given /g/ and /p/ fields of an 'Event' derive a 'gateReset' from g
and a trigger derived from monitoring /g/ and /p/ for changed values.
-}
eventGateReset :: UGen -> UGen -> (UGen, UGen)
eventGateReset g p = let tr = changed p 0.01 + changed g 0.01 in (gateReset g tr,tr)

-- * Ctl

-- | Sequence of 16 continous controller inputs in range (0-1).
type Ctl16 = (UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen)

-- | k0 = index of control bus zero for ctl system, c = ctl channel or voice (zero indexed)
ctl16Addr :: UGen -> UGen -> Ctl16
ctl16Addr k0 c =
  let u = in' 16 ControlRate (k0 + (c * 16))
  in case mceChannels u of
       [cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7,cc8,cc9,cc10,cc11,cc12,cc13,cc14,cc15] ->
         (cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7,cc8,cc9,cc10,cc11,cc12,cc13,cc14,cc15)
       _ -> error "ctl16Addr?"

-- | c0 = index of voice (channel) zero for ctl set, n = number of voices (channels)
ctl16VoicerAddr :: UGen -> UGen -> Int -> (Int -> Ctl16 -> UGen) -> UGen
ctl16VoicerAddr k0 c0 n f = mce (map (\c -> f c (ctl16Addr k0 (c0 + constant c))) [0 .. n - 1])

-- | 'ctl16Addr' with 'control' inputs for /Ctl16Addr/ and /Ctl16Zero/.
ctl16 :: Ctl16
ctl16 = ctl16Addr (control ControlRate "Ctl16Addr" 11000) (control ControlRate "Ctl16Zero" 0)

-- | 'ctlVoicerAddr' with 'control' inputs for /CtlAddr/ and /CtlZero/.
ctl16Voicer :: Int -> (Int -> Ctl16 -> UGen) -> UGen
ctl16Voicer = ctl16VoicerAddr (control ControlRate "Ctl16Addr" 11000) (control ControlRate "Ctl16Zero" 0)

-- | First eight elements of Ctl16.
type Ctl8 = (UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen)

-- | Select first eight elements of Ctl.
ctl16_to_ctl8 :: Ctl16 -> Ctl8
ctl16_to_ctl8 (cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7,_,_,_,_,_,_,_,_) = (cc0,cc1,cc2,cc3,cc4,cc5,cc6,cc7)

-- | 'ctl16Voicer' of 'ctl_to_ctl8'
ctl8Voicer :: Int -> (Int -> Ctl8 -> UGen) -> UGen
ctl8Voicer k0 f = ctl16Voicer k0 (\n c -> f n (ctl16_to_ctl8 c))

-- * Names

-- | Control Specificier.  (name,default,(minValue,maxValue,warpName))
type ControlSpec t = (String,t,(t,t,String))

-- | Comma separated, no spaces.
control_spec_parse :: String -> ControlSpec Double
control_spec_parse str =
  case splitOn "," str of
    [cnmdef,lhs,rhs,wrp] -> case splitOn ":" cnmdef of
                              [cnm,def] -> (cnm,read def,(read lhs,read rhs,wrp))
                              _ -> error ("control_spec_parse: " ++ cnmdef)
    _ -> error ("control_spec_parse: " ++ str)

-- | Semicolon separated, no spaces.
--
-- > control_spec_seq_parse "freq:220,110,440,exp;amp:0.1,0,1,amp;pan:0,-1,1,lin"
control_spec_seq_parse :: String -> [ControlSpec Double]
control_spec_seq_parse = map control_spec_parse . splitOn ";"

-- | Comma separated, 6 decimal places, no spaces.
control_spec_print :: ControlSpec Double -> String
control_spec_print (cnm,def,(lhs,rhs,wrp)) = intercalate "," [concat [cnm,":",double_pp 6 def],double_pp 6 lhs,double_pp 6 rhs,wrp]

-- | Semicolon separated, no spaces.
--
-- > control_spec_seq_print (control_spec_seq_parse "freq:220,220,440,exp;amp:0.1,0,1,amp;pan:0,-1,1,lin")
control_spec_seq_print :: [ControlSpec Double] -> String
control_spec_seq_print = intercalate ";" . map control_spec_print

control_spec_to_control :: ControlSpec Double -> Control
control_spec_to_control (cnm,def,(lhs,rhs,wrp)) =
  let grp = if last cnm `elem` "[]" then Just Control_Range else Nothing
  in Control ControlRate Nothing cnm def False (Just (Control_Meta lhs rhs wrp 0 "" grp))

{- | See SCClassLibrary/Common/Control/Spec:ControlSpec.initClass

"ControlSpec defines the range and curve of a control"

This list adds default values.
-}
sc3_control_spec :: Fractional t => [ControlSpec t]
sc3_control_spec =
  [("amp",0.1,(0,1,"amp"))
  ,("beats",1,(0,20,"lin"))
  ,("bipolar",0,(-1,1,"lin"))
  ,("boostcut",0,(-20,20,"lin"))
  ,("db",-12,(-180,0,"db"))
  ,("delay",0.01,(0.0001,1,"exp"))
  ,("detune",0,(-20,20,"lin"))
  ,("freq",440,(20,20000,"exp"))
  ,("lofreq",20,(0.1,100,"exp"))
  ,("midfreq",440,(25,4200,"exp"))
  ,("midi",64,(0,127,"lin"))
  ,("midinote",64,(0,127,"lin"))
  ,("midivelocity",64,(1,127,"lin"))
  ,("pan",0,(-1,1,"lin"))
  ,("phase",0,(0,6.28318,"lin"))
  ,("rate",1,(0.125,8,"exp"))
  ,("rq",0.1,(0.001,2,"exp"))
  ,("unipolar",0,(0,1,"lin"))
  ,("widefreq",440,(0.1,20000,"exp"))]

{- | See Kyma X Revealed, p.403

"The following EventValue names are associated with initial ranges
other than (0,1). EventValue names are not case-sensitive."

This list adds curve specifiers as strings and default values.

> let x = Data.List.intersect (map fst sc3_control_spec) (map fst kyma_event_value_ranges)
> x == ["beats","boostcut","freq","rate"]
> let c z = let (p,q) = unzip z in let f i = filter (flip elem i . fst) in zip (f p sc3_control_spec) (f q kyma_event_value_ranges)
> c (zip x x)

> c [("lofreq","freqlow"),("midfreq","freqmid")]
> lookup "freqhigh" kyma_event_value_ranges
-}
kyma_event_value_ranges :: Fractional t => [ControlSpec t]
kyma_event_value_ranges =
  [("angle",0,(-0.5,1.5,"lin"))
  ,("beats",1,(1,16,"lin"))
  ,("boostcut",0,(-12,12,"lin"))
  ,("bpm",60,(0,2000,"lin"))
  ,("centervalue",0,(-1,1,"lin"))
  ,("coef",0,(-1,1,"lin"))
  ,("cutoff",440,(0,10000,"exp"))
  ,("cycles",1,(0,100,"lin"))
  ,("dcoffset",0,(-1,1,"lin"))
  ,("direction",0,(-1,1,"lin"))
  ,("distance",0,(-2,2,"lin"))
  ,("fmntshift",1,(0.75,1.25,"lin"))
  ,("freq",440,(0,10000,"exp"))
  ,("freqhigh",12000,(8000,24000,"exp")) -- sampleRate / 2
  ,("freqjitter",0,(0,1,"lin"))
  ,("freqlow",120,(0,1000,"exp"))
  ,("freqmid",1200,(1000,8000,"exp"))
  ,("gain",0.1,(0,10,"amp"))
  ,("gaindb",-12,(-128,128,"lin"))
  ,("interval",0,(-24,24,"lin"))
  ,("keynumber",64,(0,127,"lin"))
  ,("logfreq",20,(0,127,"lin"))
  ,("looplength",0,(-1,1,"lin"))
  ,("offset",0,(-1,1,"lin"))
  ,("onduration",0.1,(0,30,"lin"))
  ,("panner",0,(-0.5,1.5,"lin"))
  ,("pitch",64,(0,127,"lin"))
  ,("q",0.1,(0,10,"lin"))
  ,("radius",1,(-2,2,"lin"))
  ,("rate",1,(0,2,"lin"))
  ,("ratio",1,(0,100,"lin"))
  ,("scale",0,(-2,2,"lin"))
  ,("smallInterval",0,(0,12,"lin"))
  ,("steps",1,(1,128,"lin"))
  ,("swing",0,(0,0.5,"lin"))
  ,("threshdb",-12,(-60,0,"lin"))
  ,("timeconstant",1,(0.0001,5,"lin"))
  ,("timeindex",0,(-1,1,"lin"))
  ,("tune",0,(-1,1,"lin"))
  ,("upinterval",0,(0,24,"lin"))]
