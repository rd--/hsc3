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
data NRT_File_Format = AIFF | WAVE | NeXT deriving (Eq,Show)

-- | Sample formats @scsynth@ renders to.
data NRT_Sample_Format = I16 | I24 | I32 | F32 | F64 deriving (Eq,Show)

-- | Data required to render 'OSC' score using @scsynth@.  The input
-- file is optional.
type NRT_Render = (FilePath,Maybe FilePath,FilePath
                  ,Double
                  ,NRT_File_Format,NRT_Sample_Format)

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
-- > let a = ["-N","x.osc","_","x.aif","44100","AIFF","int16"]
-- > in renderNRT_opt ("x.osc",Nothing,"x.aif",44100,AIFF,I16) == a
renderNRT_opt :: NRT_Render -> [String]
renderNRT_opt (c_fn,i_fn,o_fn,sr,hdr,fmt) =
    let i_fn' = fromMaybe "_" i_fn
        sr' = show (round sr :: Integer)
        hdr' = show hdr
        fmt' = nrt_sf_pp fmt
    in ["-N",c_fn,i_fn',o_fn,sr',hdr',fmt']

-- | Run @scsynth@ to render 'NRT_Render'.
renderNRT :: NRT_Render -> IO ExitCode
renderNRT = rawSystem "scsynth" . renderNRT_opt
