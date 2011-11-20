-- | Operating rate definitions and utilities.
module Sound.SC3.UGen.Rate (Rate(..)
                           ,rateId
                           ,ar,kr,ir,dr) where

import Data.Function

-- | Operating rate of unit generator.
data Rate = IR | KR | AR | DR
            deriving (Eq, Show, Enum, Bounded)

instance Ord Rate where
    compare = compare `on` rate_ord

{-# DEPRECATED ar,kr,ir,dr "Aliases to be removed" #-}
-- | Rate constructors alias.
ar :: Rate
ar = AR

-- | Rate constructors alias.
kr :: Rate
kr = KR

-- | Rate constructors alias.
ir :: Rate
ir = IR

-- | Rate constructors alias.
dr :: Rate
dr = DR

-- | Integer rate identifier, as required for scsynth bytecode.
rateId :: Rate -> Int
rateId = fromEnum

-- Rates as ordered for filter rate selection.
rate_ord :: Rate -> Int
rate_ord r =
    case r of
      IR -> 0
      KR -> 1
      AR -> 2
      DR -> 3
