-- | Sc Ugen primitive.
module Sound.Sc3.Ugen.Primitive where

import Sound.Sc3.Common.Uid {- hsc3 -}
import Sound.Sc3.Common.Rate {- hsc3 -}

import Sound.Sc3.Ugen.Brackets {- hsc3 -}

-- | Identifier used to distinguish otherwise equal non-deterministic nodes.
data UgenId = NoId | Uid Id deriving (Ord, Eq, Read, Show)

-- | Alias of 'NoId', the 'UgenId' used for deterministic Ugens.
no_id :: UgenId
no_id = NoId

-- | Unit generator output descriptor.
type Output = Rate

-- | Selector for unary and binary operators.
newtype Special = Special Int
    deriving (Ord, Eq, Read, Show)

-- | Sc Ugen primitive.
data Primitive t =
  Primitive
  {ugenRate :: Rate
  ,ugenName :: String
  ,ugenInputs :: [t]
  ,ugenOutputs :: [Output]
  ,ugenSpecial :: Special
  ,ugenId :: UgenId
  ,primitiveBrackets :: Brackets}
  deriving (Ord, Eq, Read, Show)
