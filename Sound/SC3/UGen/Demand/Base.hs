module Sound.SC3.UGen.Demand.Base where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Utilities

-- | Buffer demand ugen.
dbufrd :: UGenId -> UGen -> UGen -> Loop -> UGen
dbufrd z b p l = mkOscId z DR "Dbufrd" [b, p, from_loop l] 1

-- | Buffer write on demand unit generator.
dbufwr :: UGenId -> UGen -> UGen -> UGen -> Loop -> UGen
dbufwr z b p i l = mkOscId z DR "Dbufwr" [b, p, i, from_loop l] 1

-- | Demand rate white noise.
dwhite :: UGenId -> UGen -> UGen -> UGen -> UGen
dwhite z l lo hi = mkOscId z DR "Dwhite" [l, lo, hi] 1

-- | Demand rate integer white noise.
diwhite :: UGenId -> UGen -> UGen -> UGen -> UGen
diwhite z l lo hi = mkOscId z DR "Diwhite" [l, lo, hi] 1

-- | Demand rate brown noise.
dbrown :: UGenId -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown z l lo hi step = mkOscId z DR "Dbrown" [l, lo, hi, step] 1

-- | Demand rate integer brown noise.
dibrown :: UGenId -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown z l lo hi step = mkOscId z DR "Dibrown" [l, lo, hi, step] 1

-- | Demand rate random selection.
drand :: UGenId -> UGen -> UGen -> UGen
drand z l array = mkOscMCEId z DR "Drand" [l] array 1

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGenId -> UGen -> UGen -> UGen
dxrand z l array = mkOscMCEId z DR "Dxrand" [l] array 1

-- | Demand rate arithmetic series.
dseries :: UGenId -> UGen -> UGen -> UGen -> UGen
dseries z l i n = mkOscId z DR "Dseries" [l, i, n] 1

-- | Demand rate geometric series.
dgeom :: UGenId -> UGen -> UGen -> UGen -> UGen
dgeom z l i n = mkOscId z DR "Dgeom" [l, i, n] 1

-- | Demand rate sequence generator.
dseq :: UGenId -> UGen -> UGen -> UGen
dseq z l array = mkOscMCEId z DR "Dseq" [l] array 1

-- | Demand rate series generator.
dser :: UGenId -> UGen -> UGen -> UGen
dser z l array = mkOscMCEId z DR "Dser" [l] array 1

-- | Demand input replication
dstutter :: UGenId -> UGen -> UGen -> UGen
dstutter z n i = mkOscId z DR "Dstutter" [n,i] 1

-- | Demand rate input switching.
dswitch1 :: UGenId -> UGen -> UGen -> UGen
dswitch1 z l array = mkOscMCEId z DR "Dswitch1" [l] array 1

-- | Demand rate input switching.
dswitch :: UGenId -> UGen -> UGen -> UGen
dswitch z l array = mkOscMCEId z DR "Dswitch" [l] array 1
