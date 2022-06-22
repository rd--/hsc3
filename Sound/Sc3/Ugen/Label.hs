-- | Label.
module Sound.Sc3.Ugen.Label where

newtype Label = Label {ugenLabel :: String} deriving (Ord, Eq, Read, Show)
