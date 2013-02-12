module Sound.SC3.UGen.Transform where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

lift_constant :: String -> Constant -> Control
lift_constant nm (Constant n) = Control KR nm n False

tag_ugen :: String -> UGen -> UGen
tag_ugen nm u =
    case u of
      Constant_U c -> Control_U (lift_constant nm c)
      _ -> u

class Num n => Tagged n where
    tag :: String -> n -> n
    tag = flip const

instance Tagged Int
instance Tagged Integer
instance Tagged Double
instance Tagged UGen where tag = tag_ugen

