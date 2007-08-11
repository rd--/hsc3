module Sound.SC3.Server.NRT ( encodeNRT 
                            , writeNRT ) where

import Sound.OpenSoundControl
import qualified Data.ByteString.Lazy as B

oscWithSize :: OSC -> B.ByteString
oscWithSize o = B.append l b
    where b = encodeOSC o
          l = encode_i32 (fromIntegral (B.length b))

-- (ntp->utc. 0) ==> -2208988800
ntpZeroed :: [OSC] -> [OSC]
ntpZeroed l = map f l
    where f (Bundle t c) = Bundle (t - 2208988800.0) c
          f (Message _ _) = undefined

-- Encode a list of OSC bundles as an NRT score.
encodeNRT :: [OSC] -> B.ByteString
encodeNRT s = B.concat (map oscWithSize (ntpZeroed s))

-- Write an list of OSC bundles as an NRT score.
writeNRT :: FilePath -> [OSC] -> IO ()
writeNRT fn s = B.writeFile fn (encodeNRT s)
