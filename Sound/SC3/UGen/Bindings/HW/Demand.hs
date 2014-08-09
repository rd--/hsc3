-- | Demand rate unit generators.
module Sound.SC3.UGen.Demand where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Demand results from demand rate ugens.
demand :: UGen -> UGen -> UGen -> UGen
demand t r d =
    let d' = mceChannels d
    in mkFilterKeyed "Demand" 0 (t : r : d') (length d')

-- | Demand envelope generator.
demandEnvGen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
demandEnvGen r l d s c g rst ls lb ts a = mkOsc r "DemandEnvGen" [l, d, s, c, g, rst, ls, lb, ts, from_done_action a] 1

-- | Demand results from demand rate ugens.
duty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
duty rate d r act l = mkOsc rate "Duty" [d, r, from_done_action act, l] 1

-- | Demand results as trigger from demand rate ugens.
tDuty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen -> UGen
tDuty r d rst act l gap = mkOsc r "TDuty" [d, rst, from_done_action act, l, gap] 1

-- * NONDET

-- | Buffer demand ugen.
dbufrd :: ID i => i -> UGen -> UGen -> Loop -> UGen
dbufrd z b p l = mkOscId z DR "Dbufrd" [b, p, from_loop l] 1

-- | Buffer write on demand unit generator.
dbufwr :: ID i => i -> UGen -> UGen -> UGen -> Loop -> UGen
dbufwr z b p i l = mkOscId z DR "Dbufwr" [b, p, i, from_loop l] 1

-- | Demand rate white noise.
dwhite :: ID i => i -> UGen -> UGen -> UGen -> UGen
dwhite z l lo hi = mkOscId z DR "Dwhite" [l, lo, hi] 1

-- | Demand rate integer white noise.
diwhite :: ID i => i -> UGen -> UGen -> UGen -> UGen
diwhite z l lo hi = mkOscId z DR "Diwhite" [l, lo, hi] 1

-- | Demand rate brown noise.
dbrown :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown z l lo hi step = mkOscId z DR "Dbrown" [l, lo, hi, step] 1

-- | Demand rate integer brown noise.
dibrown :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown z l lo hi step = mkOscId z DR "Dibrown" [l, lo, hi, step] 1

-- | Demand rate random selection.
drand :: ID i => i -> UGen -> UGen -> UGen
drand z l array = mkOscMCEId z DR "Drand" [l] array 1

-- | Demand rate random selection with no immediate repetition.
dxrand :: ID i => i -> UGen -> UGen -> UGen
dxrand z l array = mkOscMCEId z DR "Dxrand" [l] array 1

-- | Demand rate arithmetic series.
dseries :: ID i => i -> UGen -> UGen -> UGen -> UGen
dseries z l i n = mkOscId z DR "Dseries" [l, i, n] 1

-- | Demand rate geometric series.
dgeom :: ID i => i -> UGen -> UGen -> UGen -> UGen
dgeom z l i n = mkOscId z DR "Dgeom" [l, i, n] 1

-- | Demand rate sequence generator.
dseq :: ID i => i -> UGen -> UGen -> UGen
dseq z l array = mkOscMCEId z DR "Dseq" [l] array 1

-- | Demand rate series generator.
dser :: ID i => i -> UGen -> UGen -> UGen
dser z l array = mkOscMCEId z DR "Dser" [l] array 1

-- | Demand rate sequence shuffler.
dshuf :: ID i => i -> UGen -> UGen -> UGen
dshuf z l array = mkOscMCEId z DR "Dshuf" [l] array 1

-- | Demand input replication
dstutter :: ID i => i -> UGen -> UGen -> UGen
dstutter z n i = mkOscId z DR "Dstutter" [n,i] 1

-- | Demand rate input switching.
dswitch1 :: ID i => i -> UGen -> UGen -> UGen
dswitch1 z l array = mkOscMCEId z DR "Dswitch1" [l] array 1

-- | Demand rate input switching.
dswitch :: ID i => i -> UGen -> UGen -> UGen
dswitch z l array = mkOscMCEId z DR "Dswitch" [l] array 1

-- Local Variables:
-- truncate-lines:t
-- End:
