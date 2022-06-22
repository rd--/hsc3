-- | Label.
module Sound.SC3.UGen.Label where

newtype Label = Label {ugenLabel :: String} deriving (Ord, Eq, Read, Show)
