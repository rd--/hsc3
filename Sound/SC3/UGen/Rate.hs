-- | Operating rate definitions and utilities.
module Sound.SC3.UGen.Rate where

import Data.Function

-- | Operating rate of unit generator.
data Rate = IR | KR | AR | DR
            deriving (Eq, Show, Enum, Bounded)

instance Ord Rate where
    compare = compare `on` rate_ord

-- | Integer rate identifier, as required for scsynth bytecode.
rateId :: Rate -> Int
rateId = fromEnum

-- | Rates as ordered for filter rate selection.
rate_ord :: Rate -> Int
rate_ord r =
    case r of
      IR -> 0
      KR -> 1
      AR -> 2
      DR -> 3

-- | Color identifiers for each 'Rate'.
rate_color :: Rate -> String
rate_color r =
    case r of
      AR -> "black"
      KR -> "blue"
      IR -> "yellow"
      DR -> "red"

