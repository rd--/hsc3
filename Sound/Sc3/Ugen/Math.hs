-- | Ugen math
module Sound.Sc3.Ugen.Math where

import qualified Sound.Sc3.Common.Math as Math
import qualified Sound.Sc3.Common.Math.Operator as Operator
import qualified Sound.Sc3.Common.Uid as Uid

import qualified Sound.Sc3.Ugen.Bindings.Db as Bindings
import qualified Sound.Sc3.Ugen.Ugen as Ugen

-- | Pseudo-infinite constant Ugen.
dinf :: Ugen.Ugen
dinf = Ugen.constant (9e8 :: Ugen.Sample)

-- | 'Ugen' form of 'ceilingE'.
ceil :: Ugen.Ugen -> Ugen.Ugen
ceil = Operator.ceilingE

-- | Midi note number and velocity data is in (0, 127).  This maps (0,1) to (0,127), i.e. is it (* 127).
unitMidi :: Num t  => t -> t
unitMidi = (*) 127

{- | midiCps of (0,1) scaled to (0,127).
     To make control signal data uniform, all control signals are in (0, 1).
-}
unitCps :: Operator.UnaryOp t  => t -> t
unitCps = Operator.midiCps . (* 127)

-- | Optimised Ugen sum function.
sum_opt :: [Ugen.Ugen] -> Ugen.Ugen
sum_opt = Math.sum_opt_f Bindings.sum3 Bindings.sum4

-- | Apply the Ugen processor /f/ /k/ times in sequence to /i/, ie. for k=4 /f (f (f (f i)))/.
useqId :: (Uid.ID z, Enum z) => z -> Int -> (z -> Ugen.Ugen -> Ugen.Ugen) -> Ugen.Ugen -> Ugen.Ugen
useqId z k f i = if k <= 0 then i else useqId (succ z) (k - 1) f (f z i)
