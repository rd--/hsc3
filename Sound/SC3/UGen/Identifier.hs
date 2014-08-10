-- | Typeclass and functions to manage UGen identifiers.
module Sound.SC3.UGen.Identifier where

import qualified Data.Hashable as H {- hashable -}

-- | Typeclass to constrain UGen identifiers.
class H.Hashable a => ID a where
    resolveID :: a -> Int
    resolveID = H.hash

instance ID Int where
instance ID Integer where
instance ID Char where
instance ID Float where
instance ID Double where

-- | Hash 'ID's /p/ and /q/ and sum to form an 'Int'.
--
-- > 'a' `joinID` (1::Int) == 1627429042
joinID :: (ID a,ID b) => a -> b -> Int
joinID p q = H.hash p `H.hashWithSalt` H.hash q
