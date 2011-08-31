module Sound.SC3.Identifier where

import Data.Char
import qualified Data.HashTable as H

-- | Typeclass to constrain UGen identifiers.
class ID a where
    resolveID :: a -> Int

instance ID Int where
    resolveID = id

instance ID Char where
    resolveID = ord

idHash :: ID a => a -> Int
idHash e = fromIntegral (H.hashInt (resolveID e))

-- | Resolve the ID at 'a' and add the resolved enumeration of 'j'.
editID :: (ID a, Enum b) => a -> b -> Int
editID i j = resolveID i + fromEnum j

-- | Infix alias for editID
(//) :: (ID a, Enum b) => a -> b -> Int
(//) = editID
