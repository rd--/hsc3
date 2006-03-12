module Hsc.Score where

import Hsc.OpenSoundControl
import Hsc.U8v

data Score = Score [Osc]
             deriving (Eq, Show)

oscWithSize o = l ++ b
    where b = osc o
          l = i32_u8v (length b)

writeScore fn (Score s) = u8vWrite fn b
    where b = concatMap oscWithSize s

