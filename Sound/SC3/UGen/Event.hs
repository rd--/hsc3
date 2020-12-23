module Sound.SC3.UGen.Event where

import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}

-- | (gate,x,y,z/force,orientation,radius-x,radius-y)
type REvent = (UGen,UGen,UGen,UGen,UGen,UGen,UGen)

rEventAddr :: UGen -> UGen -> REvent
rEventAddr k0 c =
  let u = in' 7 KR (k0 + (c * 10))
  in case mceChannels u of
       [g,x,y,z,o,rx,ry] -> (g,x,y,z,o,rx,ry)
       _ -> error "rEventAddr?"

rEventVoicerAddr :: UGen -> UGen -> Int -> (Int -> REvent -> UGen) -> UGen
rEventVoicerAddr k0 c0 n f = mce (map (\c -> f c (rEventAddr k0 (c0 + constant c))) [0 .. n - 1])

rEvent :: REvent
rEvent = rEventAddr (control KR "EventAddr" 13000) (control KR "EventZero" 0)

rEventVoicer :: Int -> (Int -> REvent -> UGen) -> UGen
rEventVoicer = rEventVoicerAddr (control KR "EventAddr" 13000) (control KR "EventZero" 0)
