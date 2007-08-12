module Sound.SC3.Server.NRT ( encodeNRT 
                            , writeNRT ) where

import Sound.OpenSoundControl
import qualified Data.ByteString.Lazy as B

oscWithSize :: OSC -> B.ByteString
oscWithSize o = B.append l b
    where b = encodeOSC_NTP o
          l = encode_i32 (fromIntegral (B.length b))

-- Encode a list of OSC bundles as an NRT score.
encodeNRT :: [OSC] -> B.ByteString
encodeNRT s = B.concat (map oscWithSize s)

-- Write an list of OSC bundles as an NRT score.
writeNRT :: FilePath -> [OSC] -> IO ()
writeNRT fn s = B.writeFile fn (encodeNRT s)
