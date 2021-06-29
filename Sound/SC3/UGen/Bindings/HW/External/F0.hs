-- | F0 UGens (f0plugins)
module Sound.SC3.UGen.Bindings.HW.External.F0 where

import Sound.SC3.Common.Rate
import qualified Sound.SC3.UGen.Bindings.HW.Construct as C
import Sound.SC3.UGen.Type

-- | Emulation of the sound generation hardware of the Atari TIA chip.
atari2600 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
atari2600 audc0 audc1 audf0 audf1 audv0 audv1 rate = C.mkOsc ar "Atari2600" [audc0,audc1,audf0,audf1,audv0,audv1,rate] 1

-- | POKEY Chip Sound Simulator
mzPokey :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
mzPokey f1 c1 f2 c2 f3 c3 f4 c4 ctl = C.mkOsc ar "MZPokey" [f1,c1,f2,c2,f3,c3,f4,c4,ctl] 1

-- | A phasor that can loop.
redPhasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
redPhasor rate trig rate_ start end loop loopstart loopend = C.mkOsc rate "RedPhasor" [trig,rate_,start,end,loop,loopstart,loopend] 1

-- | A phasor that can loop.
redPhasor2 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
redPhasor2 rate trig rate_ start end loop loopstart loopend = C.mkOsc rate "RedPhasor2" [trig,rate_,start,end,loop,loopstart,loopend] 1

-- Local Variables:
-- truncate-lines:t
-- End:
