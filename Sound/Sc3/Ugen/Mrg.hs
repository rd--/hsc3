-- | Multiple root graph (Mrg)
module Sound.Sc3.Ugen.Mrg where

data Mrg t =
  Mrg
  {mrgLeft :: t
  ,mrgRight :: t}
  deriving (Ord, Eq, Read, Show)
