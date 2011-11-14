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
-- | Rate constructors (lower case aliases of upper case data
--   constructors).
ar, kr, ir, dr :: Rate
ar = AR
kr = KR
ir = IR
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
