-- | Non-realtime score generation.
module Sound.SC3.Server.NRT where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Sound.OSC.Core {- hosc -}
import Sound.OSC.Coding.Byte {- hosc -}
import System.IO {- base -}

-- | Encode and prefix with encoded length.
oscWithSize :: Bundle -> B.ByteString
oscWithSize o =
    let b = encodeBundle o
        l = encode_i32 (fromIntegral (B.length b))
    in B.append l b

-- | An 'NRT' score is a sequence of 'Bundle's.
data NRT = NRT {nrt_bundles :: [Bundle]} deriving (Show)

-- | 'span' of 'f' of 'bundleTime'.  Can be used to separate the
-- /initialisation/ and /remainder/ parts of a score.
nrt_span :: (Time -> Bool) -> NRT -> ([Bundle],[Bundle])
nrt_span f = span (f . bundleTime) . nrt_bundles

-- | Encode an 'NRT' score.
encodeNRT :: NRT -> B.ByteString
encodeNRT = B.concat . map oscWithSize . nrt_bundles

-- | Write an 'NRT' score.
writeNRT :: FilePath -> NRT -> IO ()
writeNRT fn = B.writeFile fn . encodeNRT

-- | Write an 'NRT' score to a file handle.
putNRT :: Handle -> NRT -> IO ()
putNRT h = B.hPut h . encodeNRT

-- | Decode an 'NRT' 'B.ByteString' to a list of 'Bundle's.
decode_nrt_bundles :: B.ByteString -> [Bundle]
decode_nrt_bundles s =
    let (p,q) = B.splitAt 4 s
        n = fromIntegral (decode_i32 p)
        (r,s') = B.splitAt n q
        r' = decodeBundle r
    in if B.null s'
       then [r']
       else r' : decode_nrt_bundles s'

-- | Decode an 'NRT' 'B.ByteString'.
decodeNRT :: B.ByteString -> NRT
decodeNRT = NRT . decode_nrt_bundles

-- | 'decodeNRT' of 'B.readFile'.
readNRT :: FilePath -> IO NRT
readNRT = fmap decodeNRT . B.readFile
