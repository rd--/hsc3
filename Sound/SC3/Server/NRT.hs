-- | Non-realtime score generation.
module Sound.SC3.Server.NRT where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Maybe
import Sound.OpenSoundControl {- hosc -}
import Sound.OpenSoundControl.Coding.Byte
import Sound.SC3.Server.Enum
import System.Exit
import System.IO
import System.Process {- process -}

-- | Encode and prefix with encoded length.
oscWithSize :: Bundle -> B.ByteString
oscWithSize o =
    let b = encodeBundle o
        l = encode_i32 (fromIntegral (B.length b))
    in B.append l b

-- | An 'NRT' score is a sequence of 'Bundle's.
data NRT = NRT {nrt_bundles :: [Bundle]} deriving (Show)

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

-- | Data required to render an 'NRT' score using @scsynth@.  The
-- input file is optional.
data NRT_Render = NRT_Render {nrt_score :: FilePath
                             ,nrt_input_file ::Maybe FilePath
                             ,nrt_output_file :: FilePath
                             ,nrt_channels :: Int
                             ,nrt_sample_rate :: Double
                             ,nrt_file_format :: SoundFileFormat
                             ,nrt_sample_format :: SampleFormat}

-- | Format 'NRT_Render' as list of arguments to @scsynth@.
--
-- > let {r = NRT_Render "x.osc" Nothing "x.aif" 2 44100 AIFF I16
-- >     ;a = ["-o","2","-N","x.osc","_","x.aif","44100","AIFF","int16"]}
-- > in renderNRT_opt r == a
renderNRT_opt :: NRT_Render -> [String]
renderNRT_opt (NRT_Render c_fn i_fn o_fn nc sr hdr fmt) =
    let i_fn' = fromMaybe "_" i_fn
        nc' = show nc
        sr' = show (round sr :: Integer)
        hdr' = soundFileFormatString hdr
        fmt' = sampleFormatString fmt
    in ["-o",nc',"-N",c_fn,i_fn',o_fn,sr',hdr',fmt']

-- | 'renderNRT' command as 'String', with shell protected arguments.
--
-- > renderNRT_cmd [] (NRT_Render "s.osc" Nothing "s.flac" 2 44100 FLAC I24)
renderNRT_cmd :: [String] -> NRT_Render -> String
renderNRT_cmd x =
    let protect s = '\'' : s ++ "\'"
    in unwords . ("scsynth" :) . map protect . (x ++) . renderNRT_opt

-- | Run @scsynth@ to render 'NRT_Render' with given @scsynth@ options.
renderNRT :: [String] -> NRT_Render -> IO ExitCode
renderNRT o = rawSystem "scsynth" . (o ++ ) . renderNRT_opt
