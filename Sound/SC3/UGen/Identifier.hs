-- | Typeclass and functions to manage UGen identifiers.
module Sound.SC3.UGen.Identifier where

import Data.Char
import qualified Data.Digest.Murmur32 as H {- murmur-hash -}

-- | Typeclass to constrain UGen identifiers.
class ID a where
    resolveID :: a -> Int

instance ID Int where
    resolveID = id

instance ID Integer where
    resolveID = fromInteger

instance ID Char where
    resolveID = ord

-- | Hash value to 'Int'.
hash :: H.Hashable32 a => a -> Int
hash = fromIntegral . H.asWord32 . H.hash32

-- | Hash 'ID' to 'Int'.
idHash :: ID a => a -> Int
idHash = hash . resolveID

-- | Hash 'ID's /p/ and /q/ and sum to form an 'Int'.
--
-- > 'a' `joinID` (1::Int) == 149929881
joinID :: (ID a,ID b) => a -> b -> Int
joinID p q = idHash p + idHash q
