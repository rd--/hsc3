module Hsc.Score where

import Hsc.OpenSoundControl
import Hsc.U8v
import Hsc.Schedule (utc, pauseUntil)
import Hsc.Udp (send')

data Score = Score [Osc]
             deriving (Eq, Show)

oscWithSize o = l ++ b
    where b = osc o
          l = i32_u8v (length b)

writeScore fn (Score s) = u8vWrite fn b
    where b = concatMap oscWithSize s

playBundle _  (OscM _ _) = error "playBundle: received Message"
playBundle fd (OscB t l) = do pauseUntil t
                              mapM (send' fd) l

playScore fd (Score l) = do now <- utc
                            mapM (playBundle fd) (map (f now) l)
    where f now (OscB t l) = OscB (now + t) l
          f _   (OscM _ _) = error "playScore: received Message"
