{-# LANGUAGE FlexibleInstances #-}
module Sound.SC3.Identifier where

import Data.Char
import Data.HashTable

-- | Typeclass to constrain UGen identifiers.
class ID a where
    resolveID :: a -> Int

instance ID Int where
    resolveID = id

instance ID Char where
    resolveID = ord

instance ID [Char] where
    resolveID = fromIntegral . hashString
