module Sound.SC3.UGen.Rate ( Rate(..)
                           , rateId
                           , ar, kr, ir, dr ) where

-- | Operating rate of UGen.
data Rate = IR | KR | AR | DR deriving (Eq, Show, Enum)

instance Ord Rate where
    compare a b = compare (rateOrd a) (rateOrd b)

ar, kr, ir, dr :: Rate
ar = AR
kr = KR
ir = IR
dr = DR

rateOrd :: Rate -> Int
rateOrd IR = 0
rateOrd KR = 1
rateOrd AR = 2
rateOrd DR = 3

-- | Integer rate identifier, as required for scsynth bytecode.
rateId :: Rate -> Int
rateId = fromEnum
