-- | UGen math
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

-- | Optimised UGen sum function.
sum_opt :: [Type.UGen] -> Type.UGen
sum_opt = Math.sum_opt_f Bindings.sum3 Bindings.sum4

-- | Apply the UGen processor /f/ /k/ times in sequence to /i/, ie. for k=4 /f (f (f (f i)))/.
useqId :: (UId.ID z, Enum z) => z -> Int -> (z -> Type.UGen -> Type.UGen) -> Type.UGen -> Type.UGen
useqId z k f i = if k <= 0 then i else useqId (succ z) (k - 1) f (f z i)
