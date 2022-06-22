-- | F0 Ugens (f0plugins)
module Sound.Sc3.Ugen.Bindings.Hw.External.F0 where

import Sound.Sc3.Common.Rate
import qualified Sound.Sc3.Ugen.Bindings.Hw.Construct as C
import Sound.Sc3.Ugen.Type

-- | Emulation of the sound generation hardware of the Atari TIA chip.
atari2600 :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
atari2600 audc0 audc1 audf0 audf1 audv0 audv1 rate = C.mkOsc ar "Atari2600" [audc0,audc1,audf0,audf1,audv0,audv1,rate] 1

-- | POKEY Chip Sound Simulator
mzPokey :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
mzPokey f1 c1 f2 c2 f3 c3 f4 c4 ctl = C.mkOsc ar "MZPokey" [f1,c1,f2,c2,f3,c3,f4,c4,ctl] 1

-- | A phasor that can loop.
redPhasor :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
redPhasor rate trig rate_ start end loop loopstart loopend = C.mkOsc rate "RedPhasor" [trig,rate_,start,end,loop,loopstart,loopend] 1

-- | A phasor that can loop.
redPhasor2 :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
redPhasor2 rate trig rate_ start end loop loopstart loopend = C.mkOsc rate "RedPhasor2" [trig,rate_,start,end,loop,loopstart,loopend] 1

-- Local Variables:
-- truncate-lines:t
-- End:
