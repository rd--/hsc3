-- | Non-realtime score generation.
module Sound.SC3.Server.NRT (encodeNRT
                            ,writeNRT
                            ,putNRT ) where

import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl
import Sound.OpenSoundControl.Coding.Byte
import System.IO

-- | Encode and prefix with encoded length.
oscWithSize :: OSC -> B.ByteString
oscWithSize o = B.append l b
    where b = encodeOSC o
          l = encode_i32 (fromIntegral (B.length b))

-- | Encode a list of OSC bundles as an NRT score.
encodeNRT :: [OSC] -> B.ByteString
encodeNRT = B.concat . map oscWithSize

-- | Write a list of OSC bundles as an NRT score.
writeNRT :: FilePath -> [OSC] -> IO ()
writeNRT fn = B.writeFile fn . encodeNRT

-- | Write a list of OSC bundles as an NRT score to a file handle.
putNRT :: Handle -> [OSC] -> IO ()
putNRT h = B.hPut h . encodeNRT
