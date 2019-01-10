-- | Non-standard mathematical classes and class instances.
module Sound.SC3.UGen.Math where

import qualified Sound.SC3.Common.Math.Operator as Operator
import qualified Sound.SC3.UGen.Type as Type

-- | Pseudo-infinite constant UGen.
dinf :: Type.UGen
dinf = Type.constant (9e8 :: Type.Sample)

-- | 'UGen' form of 'ceilingE'.
ceil :: Type.UGen -> Type.UGen
ceil = Operator.ceilingE
