-- | Non-realtime score rendering.
module Sound.Sc3.Server.Nrt.Render where

import System.FilePath {- filepath -}
import System.Process {- process -}

import Sound.Sc3.Server.Enum
import Sound.Sc3.Server.Nrt

{- | Minimal Nrt rendering parameters.

The sound file type is inferred from the file name extension.
Structure is:
Osc file name,
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

