module Sound.SC3.UGen.Demand.Pure where

import Sound.SC3.UGen.Rate (Rate(DR))
import Sound.SC3.UGen.UGen (UGen, UId,
                            mkOsc, mkOscMCE, mkOscUId, mkOscUIdMCE, 
                            mkFilterKeyed,
                            uniquify, zeroUId)
import Sound.SC3.UGen.Envelope (DoneAction, fromAction)

-- | Buffer demand ugen.
dbufrd :: UId -> UGen -> UGen -> UGen -> UGen
dbufrd uid b p l = mkOscUId uid DR "Dbufrd" [b, p, l] 1 0

-- | Demand rate white noise.
dwhite :: UId -> UGen -> UGen -> UGen -> UGen
dwhite uid l lo hi = mkOscUId uid DR "Dwhite" [l, lo, hi] 1 0

-- | Demand rate integer white noise.
diwhite :: UId -> UGen -> UGen -> UGen -> UGen
diwhite uid l lo hi = mkOscUId uid DR "Diwhite" [l, lo, hi] 1 0

-- | Demand rate brown noise.
dbrown :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
dbrown uid l lo hi step = mkOscUId uid DR "Dbrown" [l, lo, hi, step] 1 0

-- | Demand rate integer brown noise.
dibrown :: UId -> UGen -> UGen -> UGen -> UGen -> UGen
dibrown uid l lo hi step = mkOscUId uid DR "Dibrown" [l, lo, hi, step] 1 0

-- | Demand rate random selection.
drand :: UId -> UGen -> UGen -> UGen
drand uid l array = mkOscUIdMCE uid DR "Drand" [l] array 1 0

-- | Demand rate random selection with no immediate repetition.
dxrand :: UId -> UGen -> UGen -> UGen
dxrand uid l array = mkOscUIdMCE uid DR "Dxrand" [l] array 1 0

-- | Demand rate arithmetic series.
dseries :: UId -> UGen -> UGen -> UGen -> UGen
dseries uid l i n = mkOscUId uid DR "Dseries" [l, i, n] 1 0

-- | Demand rate geometric series.
dgeom :: UId -> UGen -> UGen -> UGen -> UGen
dgeom uid l i n = mkOscUId uid DR "Dgeom" [l, i, n] 1 0

-- | Demand rate sequence generator.
dseq :: UId -> UGen -> UGen -> UGen
dseq uid l array = mkOscUIdMCE uid DR "Dseq" [l] array 1 0

-- | Demand rate series generator.
dser :: UId -> UGen -> UGen -> UGen
dser uid l array = mkOscUIdMCE uid DR "Dser" [l] array 1 0

-- | Demand rate input switching.
dswitch1 :: UId -> UGen -> UGen -> UGen
dswitch1 uid l array = mkOscUIdMCE uid DR "Dswitch1" [l] array 1 0
