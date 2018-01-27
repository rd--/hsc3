-- | Typeclass and functions to manage UGen identifiers.
module Sound.SC3.UGen.Identifier where

import qualified Data.Hashable as H {- hashable -}

-- | Typeclass to constrain UGen identifiers.
--
-- > map resolveID [0::Int,1] == [0::Int,1]
-- > map resolveID ['α','β'] == [945,946]
-- > map resolveID [('α','β'),('β','α')] == [15854849041,15871627911]
-- > map resolveID [('α',('α','β')),('β',('α','β'))] == [266006616529191908,266288126000523575]
class H.Hashable a => ID a where
    resolveID :: a -> Int
    resolveID = H.hash

instance ID Char where resolveID = fromEnum
instance ID Int where resolveID = id
instance (ID p,ID q) => ID (p,q) where

{-
instance ID Int where
instance ID Integer where
instance ID Float where
instance ID Double where

-- | Hash 'ID's /p/ and /q/ and sum to form an 'Int'.
--
-- > 'a' `joinID` (1::Int) == 1627429042
joinID :: (ID a,ID b) => a -> b -> Int
joinID p q = H.hash p `H.hashWithSalt` H.hash q
-}
