module Hsc.Score where

import Hsc.OpenSoundControl
import Hsc.U8v
import Hsc.Schedule (utc, pauseUntil)
import Hsc.Udp (send')
import Network.Socket (Socket)

data Score = Score [Osc]
             deriving (Eq, Show)

oscWithSize :: Osc -> [U8]
oscWithSize o = l ++ b
    where b = osc o
          l = i32_u8v (length b)

writeScore :: FilePath -> Score -> IO ()
writeScore fn (Score s) = u8vWrite fn b
    where b = concatMap oscWithSize s

playBundle :: Socket -> Osc -> IO [Int]
playBundle _  (OscM _ _) = error "playBundle: received Message"
playBundle fd (OscB t l) = do pauseUntil t
                              mapM (send' fd) l

playScore :: Socket -> Score -> IO [[Int]]
playScore fd (Score sc) = do now <- utc
                             mapM (playBundle fd) (map (f now) sc)
    where f now (OscB t l) = OscB (now + t) l
          f _   (OscM _ _) = error "playScore: received Message"
