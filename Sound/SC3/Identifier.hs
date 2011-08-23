module Sound.SC3.Identifier where

import Data.Char

-- | Typeclass to constrain UGen identifiers.
class Enum a => ID a where
    resolveID :: a -> Int

instance ID Int where
    resolveID = id

instance ID Char where
    resolveID = ord
