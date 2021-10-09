-- | Non-realtime score generation.
module Sound.SC3.Server.Nrt where

import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}
import System.Process {- process -}

import qualified Data.ByteString.Lazy as B {- bytestring -}

import Sound.OSC.Core {- hosc -}
import qualified Sound.OSC.Coding.Byte as Byte {- hosc -}

import Sound.SC3.Common.Base
import Sound.SC3.Server.Enum

-- | Encode and prefix with encoded length.
oscWithSize :: Bundle -> B.ByteString
oscWithSize o =
    let b = encodeBundle o
        l = Byte.encode_i32 (fromIntegral (B.length b))
    in B.append l b

-- | An 'Nrt' score is a sequence of 'Bundle's.
newtype Nrt = Nrt {nrt_bundles :: [Bundle]} deriving (Show)

-- | Trivial Nrt statistics.
type Nrt_Stat =
    ((String, Time)
    ,(String, Int)
    ,(String, Int)
    ,(String, [(String,Int)]))

-- | Nrt_Stat names.
nrt_stat_param :: (String, String, String, String)
nrt_stat_param = ("duration","# bundles","# messages","command set")

-- | Trivial Nrt statistics.
nrt_stat :: Nrt -> Nrt_Stat
nrt_stat (Nrt b_seq) =
    let b_msg = map bundleMessages b_seq
    in p4_zip
       nrt_stat_param
       (bundleTime (last b_seq)
       ,length b_seq
       ,sum (map length b_msg)
       ,histogram (concatMap (map messageAddress) b_msg))

-- | 'span' of 'f' of 'bundleTime'.  Can be used to separate the
-- /initialisation/ and /remainder/ parts of a score.
nrt_span :: (Time -> Bool) -> Nrt -> ([Bundle],[Bundle])
nrt_span f = span (f . bundleTime) . nrt_bundles

-- | Encode an 'Nrt' score.
encodeNrt :: Nrt -> B.ByteString
encodeNrt = B.concat . map oscWithSize . nrt_bundles

{- | Write an 'Nrt' score.

import Sound.OSC
import Sound.SC3
m1 = g_new [(1, AddToTail, 0)]
m2 = d_recv (synthdef "sin" (out 0 (sinOsc AR 660 0 * 0.15)))
m3 = s_new "sin" 100 AddToTail 1 []
m4 = n_free [100]
m5 = nrt_end
sc = Nrt [bundle 0 [m1,m2],bundle 1 [m3],bundle 10 [m4],bundle 15 [m5]]
writeNrt "/tmp/t.osc" sc

-}
writeNrt :: FilePath -> Nrt -> IO ()
writeNrt fn = B.writeFile fn . encodeNrt

-- | Write an 'Nrt' score to a file handle.
putNrt :: Handle -> Nrt -> IO ()
putNrt h = B.hPut h . encodeNrt

-- | Decode an 'Nrt' 'B.ByteString' to a list of 'Bundle's.
decode_nrt_bundles :: B.ByteString -> [Bundle]
decode_nrt_bundles s =
    let (p,q) = B.splitAt 4 s
        n = fromIntegral (Byte.decode_i32 p)
        (r,s') = B.splitAt n q
        r' = decodeBundle r
    in if B.null s'
       then [r']
       else r' : decode_nrt_bundles s'

-- | Decode an 'Nrt' 'B.ByteString'.
decodeNrt :: B.ByteString -> Nrt
decodeNrt = Nrt . decode_nrt_bundles

-- | 'decodeNrt' of 'B.readFile'.
readNrt :: FilePath -> IO Nrt
readNrt = fmap decodeNrt . B.readFile

-- * Render

{- | Minimal Nrt rendering parameters.

The sound file type is inferred from the file name extension.
Structure is:
OSC file name,
input audio file name and input number of channels (use ("_",0) for no input file),
output audio file name and output number of channels,
sample rate (int),
sample format,
further parameters (ie. ["-m","32768"]) to be inserted before the Nrt -N option.

-}
type Nrt_Param_Plain = (FilePath,(FilePath,Int),(FilePath,Int),Int,SampleFormat,[String])

{- | Compile argument list from Nrt_Param_Plain.

> let opt = ("/tmp/t.osc",("_",0),("/tmp/t.wav",1),48000,PcmInt16,[])
> let r = ["-i","0","-o","1","-N","/tmp/t.osc","_","/tmp/t.wav","48000","wav","int16"]
> nrt_param_plain_to_arg opt == r

-}
nrt_param_plain_to_arg :: Nrt_Param_Plain -> [String]
nrt_param_plain_to_arg (osc_nm,(in_sf,in_nc),(out_sf,out_nc),sr,sf,param) =
  let sf_ty = case takeExtension out_sf of
                '.':ext -> soundFileFormat_from_extension_err ext
                _ -> error "nrt_exec_plain: invalid sf extension"
  in concat [["-i",show in_nc
             ,"-o",show out_nc]
            ,param
            ,["-N"
             ,osc_nm,in_sf,out_sf
             ,show sr,soundFileFormatString sf_ty,sampleFormatString sf]]

{- | Compile argument list from Nrt_Param_Plain and run scynth.

> nrt_exec_plain opt

-}
nrt_exec_plain :: Nrt_Param_Plain -> IO ()
nrt_exec_plain opt = callProcess "scsynth" (nrt_param_plain_to_arg opt)

-- | Minimal Nrt rendering, for more control see Stefan Kersten's
-- /hsc3-process/ package at:
-- <https://github.com/kaoskorobase/hsc3-process>.
nrt_proc_plain :: Nrt_Param_Plain -> Nrt -> IO ()
nrt_proc_plain opt sc = do
  let (osc_nm,_,_,_,_,_) = opt
  writeNrt osc_nm sc
  nrt_exec_plain opt

-- | Variant for no input case.
--
-- (osc-file-name,audio-file-name,number-of-channels,sample-rate,sample-format,param)
type Nrt_Render_Plain = (FilePath,FilePath,Int,Int,SampleFormat,[String])

{- | Add ("-",0) as input parameters and run 'nrt_proc_plain'.

> nrt_render_plain opt sc

-}
nrt_render_plain :: Nrt_Render_Plain -> Nrt -> IO ()
nrt_render_plain (osc_nm,sf_nm,nc,sr,sf,param) sc =
  let opt = (osc_nm,("_",0),(sf_nm,nc),sr,sf,param)
  in nrt_proc_plain opt sc

-- * QUERY

-- | Find any non-ascending sequences.
nrt_non_ascending :: Nrt -> [(Bundle, Bundle)]
nrt_non_ascending (Nrt b) =
  let p = zip b (tail b)
      f (i,j) = if bundleTime i > bundleTime j then Just (i,j) else Nothing
  in mapMaybe f p
