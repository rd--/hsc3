-- | Ugen math
module Sound.SC3.UGen.Math where

import qualified Sound.SC3.Common.Math as Math
import qualified Sound.SC3.Common.Math.Operator as Operator
import qualified Sound.SC3.Common.UId as UId

import qualified Sound.SC3.UGen.Bindings.DB as Bindings
import qualified Sound.SC3.UGen.Type as Type

-- | Pseudo-infinite constant UGen.
dinf :: Type.UGen
dinf = Type.constant (9e8 :: Type.Sample)

-- | 'UGen' form of 'ceilingE'.
ceil :: Type.UGen -> Type.UGen
ceil = Operator.ceilingE

-- | Midi note number and velocity data is in (0, 127).  This maps (0,1) to (0,127), i.e. is it (* 127).
unitMidi :: Num t  => t -> t
unitMidi = (*) 127

{- | midiCps of (0,1) scaled to (0,127).
     To make control signal data uniform, all control signals are in (0, 1).
-}
unitCps :: Operator.UnaryOp t  => t -> t
unitCps = Operator.midiCps . (* 127)

-- | Optimised UGen sum function.
sum_opt :: [Type.UGen] -> Type.UGen
sum_opt = Math.sum_opt_f Bindings.sum3 Bindings.sum4

-- | Apply the UGen processor /f/ /k/ times in sequence to /i/, ie. for k=4 /f (f (f (f i)))/.
useqId :: (UId.ID z, Enum z) => z -> Int -> (z -> Type.UGen -> Type.UGen) -> Type.UGen -> Type.UGen
useqId z k f i = if k <= 0 then i else useqId (succ z) (k - 1) f (f z i)
