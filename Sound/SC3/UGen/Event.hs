-- | Event and Ctl systems for external control interfaces.
module Sound.SC3.UGen.Event where

import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}

-- * Event

-- | (gate,x,y,z/force,orientation,radius-x,radius-y,pitch)
type REvent = (UGen,UGen,UGen,UGen,UGen,UGen,UGen,UGen)

-- | k0 = index of control bus zero for event system, c = event channel or voice (zero indexed)
rEventAddr :: UGen -> UGen -> REvent
rEventAddr k0 c =
  let u = in' 8 KR (k0 + (c * 10))
  in case mceChannels u of
       [g,x,y,z,o,rx,ry,p] -> (g,x,y,z,o,rx,ry,p)
       _ -> error "rEventAddr?"

-- | c0 = index of voice (channel) zero for event set, n = number of voices (channels)
rEventVoicerAddr :: UGen -> UGen -> Int -> (Int -> REvent -> UGen) -> UGen
rEventVoicerAddr k0 c0 n f = mce (map (\c -> f c (rEventAddr k0 (c0 + constant c))) [0 .. n - 1])

-- | 'rEventAddr' with 'control' inputs for /EventAddr/ and /EventZero/.
rEvent :: REvent
rEvent = rEventAddr (control KR "EventAddr" 13000) (control KR "EventZero" 0)

-- | 'rEventVoicerAddr' with 'control' inputs for /EventAddr/ and /EventZero/.
rEventVoicer :: Int -> (Int -> REvent -> UGen) -> UGen
rEventVoicer = rEventVoicerAddr (control KR "EventAddr" 13000) (control KR "EventZero" 0)

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
