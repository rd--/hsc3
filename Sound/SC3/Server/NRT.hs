-- | Non-realtime score generation.
module Sound.SC3.Server.NRT where

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

-- | An 'NRT' score is a sequence of 'Bundle's.
data NRT = NRT {nrt_bundles :: [Bundle]} deriving (Show)

-- | Trivial NRT statistics.
type NRT_STAT =
    ((String, Time)
    ,(String, Int)
    ,(String, Int)
    ,(String, [(String,Int)]))

-- | NRT_STAT names.
nrt_stat_param :: (String, String, String, String)
nrt_stat_param = ("duration","# bundles","# messages","command set")

-- | Trivial NRT statistics.
nrt_stat :: NRT -> NRT_STAT
nrt_stat (NRT b_seq) =
    let b_msg = map bundleMessages b_seq
    in p4_zip
       nrt_stat_param
       (bundleTime (last b_seq)
       ,length b_seq
       ,sum (map length b_msg)
       ,histogram (concatMap (map messageAddress) b_msg))

-- | 'span' of 'f' of 'bundleTime'.  Can be used to separate the
-- /initialisation/ and /remainder/ parts of a score.
nrt_span :: (Time -> Bool) -> NRT -> ([Bundle],[Bundle])
nrt_span f = span (f . bundleTime) . nrt_bundles

-- | Encode an 'NRT' score.
encodeNRT :: NRT -> B.ByteString
encodeNRT = B.concat . map oscWithSize . nrt_bundles

{- | Write an 'NRT' score.

import Sound.OSC
import Sound.SC3
m1 = g_new [(1, AddToTail, 0)]
m2 = d_recv (synthdef "sin" (out 0 (sinOsc AR 660 0 * 0.15)))
m3 = s_new "sin" 100 AddToTail 1 []
m4 = n_free [100]
m5 = nrt_end
sc = NRT [bundle 0 [m1,m2],bundle 1 [m3],bundle 10 [m4],bundle 15 [m5]]
writeNRT "/tmp/t.osc" sc

-}
writeNRT :: FilePath -> NRT -> IO ()
writeNRT fn = B.writeFile fn . encodeNRT

-- | Write an 'NRT' score to a file handle.
putNRT :: Handle -> NRT -> IO ()
putNRT h = B.hPut h . encodeNRT

-- | Decode an 'NRT' 'B.ByteString' to a list of 'Bundle's.
decode_nrt_bundles :: B.ByteString -> [Bundle]
decode_nrt_bundles s =
    let (p,q) = B.splitAt 4 s
        n = fromIntegral (Byte.decode_i32 p)
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

{- | Minimal NRT rendering parameters.

The sound file type is inferred from the file name extension.
Structure is:
OSC file name,
input audio file name and input number of channels,
output audio file name and output number of channels,
sample rate,
sample format,
further parameters (ie. ["-m","32768"]) to be inserted before the NRT -N option.

-}
type NRT_Param_Plain = (FilePath,(FilePath,Int),(FilePath,Int),Int,SampleFormat,[String])

{- | Compile argument list from NRT_Param_Plain.

> let opt = ("/tmp/t.osc",("_",0),("/tmp/t.wav",1),48000,PcmInt16,[])
> let r = ["-i","0","-o","1","-N","/tmp/t.osc","_","/tmp/t.wav","48000","wav","int16"]
> nrt_param_plain_to_arg opt == r

-}
nrt_param_plain_to_arg :: NRT_Param_Plain -> [String]
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

{- | Compile argument list from NRT_Param_Plain and run scynth.

> nrt_exec_plain opt

-}
nrt_exec_plain :: NRT_Param_Plain -> IO ()
nrt_exec_plain opt = callProcess "scsynth" (nrt_param_plain_to_arg opt)

-- | Minimal NRT rendering, for more control see Stefan Kersten's
-- /hsc3-process/ package at:
-- <https://github.com/kaoskorobase/hsc3-process>.
nrt_proc_plain :: NRT_Param_Plain -> NRT -> IO ()
nrt_proc_plain opt sc = do
  let (osc_nm,_,_,_,_,_) = opt
  writeNRT osc_nm sc
  nrt_exec_plain opt

-- | Variant for no input case.
--
-- (osc-file-name,audio-file-name,number-of-channels,sample-rate,sample-format,param)
type NRT_Render_Plain = (FilePath,FilePath,Int,Int,SampleFormat,[String])

{- | Add ("-",0) as input parameters and run 'nrt_proc_plain'.

> nrt_render_plain opt sc

-}
nrt_render_plain :: NRT_Render_Plain -> NRT -> IO ()
nrt_render_plain (osc_nm,sf_nm,nc,sr,sf,param) sc =
  let opt = (osc_nm,("_",0),(sf_nm,nc),sr,sf,param)
  in nrt_proc_plain opt sc

-- * QUERY

-- | Find any non-ascending sequences.
nrt_non_ascending :: NRT -> [(Bundle, Bundle)]
nrt_non_ascending (NRT b) =
  let p = zip b (tail b)
      f (i,j) = if bundleTime i > bundleTime j then Just (i,j) else Nothing
  in mapMaybe f p
