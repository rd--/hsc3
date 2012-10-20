--  | Multiple channel expansion.
module Sound.SC3.UGen.MCE where

-- | Multiple channel expansion.
data MCE n = MCE_Unit n | MCE_Vector [n]
             deriving (Eq,Show)

mce_elem :: MCE t -> [t]
mce_elem m =
    case m of
      MCE_Unit e -> [e]
      MCE_Vector e -> e

-- | Extend UGen to specified degree.
mce_extend :: Int -> MCE n -> MCE n
mce_extend n m =
    case m of
      MCE_Unit e -> MCE_Vector (replicate n e)
      MCE_Vector e -> MCE_Vector (take n (cycle e))

