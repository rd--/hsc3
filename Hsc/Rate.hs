module Hsc.Rate where

data Rate = IR | KR | AR | DR deriving (Eq, Show, Enum)

instance Ord Rate where
    compare a b = compare (rateOrd a) (rateOrd b)

rateOrd :: Rate -> Int
rateOrd IR = 0
rateOrd DR = 1
rateOrd KR = 2
rateOrd AR = 3

rateId :: Rate -> Int
rateId = fromEnum
