-- | Constant
module Sound.SC3.UGen.Constant where

import Sound.SC3.UGen.Brackets {- hsc3 -}

{- | Constants.
Constants may have brackets.
This allows for buffer allocation and deallocation to be associated with a buffer identifier.

> Constant 3 == Constant 3
> (Constant 3 > Constant 1) == True
-}
data Constant =
  Constant
  {constantValue :: Double
  ,constantBrackets :: Brackets}
  deriving (Ord, Eq, Read, Show)

