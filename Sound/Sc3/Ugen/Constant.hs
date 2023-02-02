-- | Constant
module Sound.Sc3.Ugen.Constant where

import Sound.Sc3.Ugen.Brackets {- hsc3 -}

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

-- > constantIsInteger (Constant 1 emptyBrackets) == True
constantIsInteger :: Constant -> Bool
constantIsInteger (Constant n _) = snd (properFraction n) == 0
