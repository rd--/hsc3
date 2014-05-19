-- | Non-realtime score generation.
module Sound.SC3.Server.NRT where

import Data.Maybe {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import System.FilePath {- filepath -}
import System.IO {- base -}
import System.Process {- process -}

import Sound.OSC.Core {- hosc -}
import Sound.OSC.Coding.Byte {- hosc -}

import Sound.SC3.Server.Enum

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

-- * Render

-- | Minimal NRT rendering options.  The sound file type is inferred
-- from the file name extension.  Structure is: OSC file name, output
-- audio file name, output number of channels, sample rate, sample
-- format.
type NRT_Render_Plain = (FilePath,FilePath,Int,Int,SampleFormat)

-- | Minimal NRT rendering, for more control see Stefan Kersten's
-- /hsc3-process/ package at:
-- <https://github.com/kaoskorobase/hsc3-process>.
nrt_render_plain :: NRT_Render_Plain -> NRT -> IO ()
nrt_render_plain (osc_nm,sf_nm,nc,sr,sf) sc = do
  let sf_ty = case takeExtension sf_nm of
                '.':ext -> let fmt = soundFileFormat_from_extension ext
                           in fromMaybe (error "nrt_render_plain: unknown sf extension") fmt
                _ -> error "nrt_render_plain: invalid sf extension"
      sys = unwords ["scsynth"
                    ,"-i","0"
                    ,"-o",show nc
                    ,"-N"
                    ,osc_nm,"_"
                    ,sf_nm,show sr,soundFileFormatString sf_ty,sampleFormatString sf]
  writeNRT osc_nm sc
  _ <- system sys
  return ()
