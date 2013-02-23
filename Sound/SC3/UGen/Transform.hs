module Sound.SC3.UGen.Transform where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

-- | Lift a 'Constant' to a named 'KR' 'Control'.
lift_constant :: String -> Constant -> Control
lift_constant nm (Constant n) = Control KR nm n False

-- | If 'UGen' /u/ is a 'Constant' apply 'lift_constant', else 'id'.
tag_ugen :: String -> UGen -> UGen
tag_ugen nm u =
    case u of
      Constant_U c -> Control_U (lift_constant nm c)
      _ -> u

-- | Typeclass for values that can be 'Tagged'.  The default 'tag'
-- operation is 'id'.
class Num n => Tagged n where
    tag :: String -> n -> n
    tag = flip const

instance Tagged Int
instance Tagged Integer
instance Tagged Double
instance Tagged UGen where tag = tag_ugen
