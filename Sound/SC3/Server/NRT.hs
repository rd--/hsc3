-- | Non-realtime score generation.
module Sound.SC3.Server.NRT where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Maybe
import Sound.OpenSoundControl {- hosc -}
import Sound.OpenSoundControl.Coding.Byte
import System.Exit
import System.IO
import System.Process {- process -}

-- | Encode and prefix with encoded length.
oscWithSize :: OSC -> B.ByteString
oscWithSize o =
    let b = encodeOSC o
        l = encode_i32 (fromIntegral (B.length b))
    in B.append l b

-- | Encode a list of OSC bundles as an NRT score.
encodeNRT :: [OSC] -> B.ByteString
encodeNRT = B.concat . map oscWithSize

-- | Write a list of OSC bundles as an NRT score.
writeNRT :: FilePath -> [OSC] -> IO ()
writeNRT fn = B.writeFile fn . encodeNRT

-- | Write a list of OSC bundles as an NRT score to a file handle.
putNRT :: Handle -> [OSC] -> IO ()
putNRT h = B.hPut h . encodeNRT

-- | File formats @scsynth@ renders to.
data NRT_File_Format = AIFF | FLAC | NeXT | WAVE deriving (Eq,Show)

-- | Sample formats @scsynth@ renders to.
data NRT_Sample_Format = I16 | I24 | I32 | F32 | F64 deriving (Eq,Show)

-- | Data required to render 'OSC' score using @scsynth@.  The input
-- file is optional.
data NRT_Render = NRT_Render {nrt_score :: FilePath
                             ,nrt_input_file ::Maybe FilePath
                             ,nrt_output_file :: FilePath
                             ,nrt_channels :: Int
                             ,nrt_sample_rate :: Double
                             ,nrt_file_format :: NRT_File_Format
                             ,nrt_sample_format :: NRT_Sample_Format}

-- | Format 'NRT_Sample_Format' for @scsynth@.
nrt_sf_pp :: NRT_Sample_Format -> String
nrt_sf_pp f =
    case f of
         I16 -> "int16"
         I24 -> "int24"
         I32 -> "int32"
         F32 -> "float"
         F64 -> "double"

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
        hdr' = show hdr
        fmt' = nrt_sf_pp fmt
    in ["-o",nc',"-N",c_fn,i_fn',o_fn,sr',hdr',fmt']

-- | 'renderNRT' command as 'String', with shell protected arguments.
--
-- > renderNRT_cmd [] (NRT_Render "s.osc" Nothing "s.flac" 2 44100 FLAC I24)
renderNRT_cmd :: [String] -> NRT_Render -> String
renderNRT_cmd x o =
    let protect s = '\'' : s ++ ['\'']
    in unwords ("scsynth" : map protect (x ++ renderNRT_opt o))

-- | Run @scsynth@ to render 'NRT_Render' with given @scsynth@ options.
renderNRT :: [String] -> NRT_Render -> IO ExitCode
renderNRT o = rawSystem "scsynth" . (o ++ ) . renderNRT_opt
