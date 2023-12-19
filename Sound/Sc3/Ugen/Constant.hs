-- | Constant
module Sound.Sc3.Ugen.Constant where

import Sound.Sc3.Ugen.Brackets {- hsc3 -}

{- | Constants.
Constants may have brackets.
This allows for buffer allocation and deallocation to be associated with a buffer identifier.

>>> Constant 3 emptyBrackets == Constant 3 emptyBrackets
True

>>> Constant 3 emptyBrackets > Constant 1 emptyBrackets
True
-}
data Constant = Constant
  { constantValue :: Double
  , constantBrackets :: Brackets
  }
  deriving (Ord, Eq, Read, Show)

-- | Get fractional part of a double.
fractionPart :: Double -> Double
fractionPart = snd . (properFraction :: Double -> (Integer, Double))

{- | Is integer?

>>> constantIsInteger (Constant 1 emptyBrackets)
True
-}
constantIsInteger :: Constant -> Bool
constantIsInteger (Constant n _) = fractionPart n == 0
