-- | Multiple root graph (Mrg)
module Sound.SC3.UGen.Mrg where

data Mrg t =
  Mrg
  {mrgLeft :: t
  ,mrgRight :: t}
  deriving (Ord, Eq, Read, Show)
