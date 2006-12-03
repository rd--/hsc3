module Sound.SC3.UGen.Demand where

import Sound.SC3.UGen.Rate (Rate(DR))
import Sound.SC3.UGen.UGen (UGen, UId,
                            mkOsc, mkOscMCE, mkOscUId, mkOscUIdMCE, 
                            mkFilterKeyed,
                            uniquify, zeroUId)
import Sound.SC3.UGen.Envelope (DoneAction, fromAction)

-- | Demand results from demand rate ugens.
demand :: UGen -> UGen -> UGen -> UGen
demand t r d = mkFilterKeyed "Demand" 0 [t, r, d] 1 0

-- | Demand results from demand rate ugens.
duty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
duty rate d r act l = mkOsc rate "Duty" [d, r, fromAction act, l] 1 0

-- | Buffer demand ugen.
dbufrd :: UGen -> UGen -> UGen -> IO UGen
dbufrd b p l = uniquify (dbufrdUId zeroUId b p l)

dbufrdUId :: UId -> UGen -> UGen -> UGen -> UGen
dbufrdUId uid b p l = mkOscUId uid DR "Dbufrd" [b, p, l] 1 0

-- | Demand rate white noise.
dwhite :: UGen -> UGen -> UGen -> IO UGen
dwhite l lo hi = uniquify (dwhiteUId zeroUId l lo hi)

dwhiteUId :: UId -> UGen -> UGen -> UGen -> UGen
dwhiteUId uid l lo hi = mkOscUId uid DR "Dwhite" [l, lo, hi] 1 0

-- | Demand rate integer white noise.
diwhite :: UGen -> UGen -> UGen -> IO UGen
diwhite l lo hi = uniquify (diwhiteUId zeroUId l lo hi)

diwhiteUId :: UId -> UGen -> UGen -> UGen -> UGen
diwhiteUId uid l lo hi = mkOscUId uid DR "Diwhite" [l, lo, hi] 1 0

-- | Demand rate brown noise.
dbrown :: UGen -> UGen -> UGen -> UGen -> IO UGen
dbrown l lo hi step = uniquify (dbrownUId zeroUId l lo hi step)

dbrownUId :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
dbrownUId uid l lo hi step = mkOscUId uid DR "Dbrown" [l, lo, hi, step] 1 0

-- | Demand rate integer brown noise.
dibrown :: UGen -> UGen -> UGen -> UGen -> IO UGen
dibrown l lo hi step = uniquify (dibrownUId zeroUId l lo hi step)

dibrownUId :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
dibrownUId uid l lo hi step = mkOscUId uid DR "Dibrown" [l, lo, hi, step] 1 0

-- | Demand rate random selection.
drand :: UGen -> UGen -> IO UGen
drand l array = uniquify (drandUId zeroUId l array)

drandUId :: UId -> UGen -> UGen -> UGen
drandUId uid l array = mkOscUIdMCE uid DR "Drand" [l] array 1 0

-- | Demand rate random selection with no immediate repetition.
dxrand :: UGen -> UGen -> IO UGen
dxrand l array = uniquify (dxrandUId zeroUId l array)

dxrandUId :: UId -> UGen -> UGen -> UGen
dxrandUId uid l array = mkOscUIdMCE uid DR "Dxrand" [l] array 1 0

-- | Demand rate arithmetic series.
dseries :: UGen -> UGen -> UGen -> IO UGen
dseries l i n = uniquify (dseriesUId zeroUId l i n)

dseriesUId :: UId -> UGen -> UGen -> UGen -> UGen
dseriesUId uid l i n = mkOscUId uid DR "Dseries" [l, i, n] 1 0

-- | Demand rate geometric series.
dgeom :: UGen -> UGen -> UGen -> IO UGen
dgeom l i n = uniquify (dgeomUId zeroUId l i n)

dgeomUId :: UId -> UGen -> UGen -> UGen -> UGen
dgeomUId uid l i n = mkOscUId uid DR "Dgeom" [l, i, n] 1 0

-- | Demand rate sequence generator.
dseq :: UGen -> UGen -> IO UGen
dseq l array = uniquify (dseqUId zeroUId l array)

dseqUId :: UId -> UGen -> UGen -> UGen
dseqUId uid l array = mkOscUIdMCE uid DR "Dseq" [l] array 1 0

-- | Demand rate series generator.
dser :: UGen -> UGen -> IO UGen
dser l array = uniquify (dserUId zeroUId l array)

dserUId :: UId -> UGen -> UGen -> UGen
dserUId uid l array = mkOscUIdMCE uid DR "Dser" [l] array 1 0

-- | Demand rate input switching.
dswitch1 :: UGen -> UGen -> IO UGen
dswitch1 l array = uniquify (dswitch1UId zeroUId l array)

dswitch1UId :: UId -> UGen -> UGen -> UGen
dswitch1UId uid l array = mkOscUIdMCE uid DR "Dswitch1" [l] array 1 0

tDuty :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
tDuty r d rst a l = mkOscMCE r "TDuty" [d,rst,a] l 1 0
