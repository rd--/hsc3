-- | Sc Ugen primitive.
module Sound.SC3.UGen.Primitive where

import Sound.SC3.Common.UId {- hsc3 -}
import Sound.SC3.Common.Rate {- hsc3 -}

import Sound.SC3.UGen.Brackets {- hsc3 -}

-- | Identifier used to distinguish otherwise equal non-deterministic nodes.
data UGenId = NoId | UId Id deriving (Ord, Eq, Read, Show)

-- | Alias of 'NoId', the 'UGenId' used for deterministic UGens.
no_id :: UGenId
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
  ,ugenId :: UGenId
  ,primitiveBrackets :: Brackets}
  deriving (Ord, Eq, Read, Show)
