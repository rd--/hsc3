module Sound.SC3.UGen.Record where

import Sound.SC3.UGen.UGen

class Make a where
    make :: a -> UGen
