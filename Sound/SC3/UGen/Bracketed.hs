{- | Bracketed UGens.

'Brackets' are a pair of OpenSoundControl message sequences that can be attached to UGen values.
One sequence (called /pre/) is to be sent before the graph the UGen belongs to is started,
the other (called /post/) is to be sent afterwards.
The functions defined here return UGens with Brackets.
They're designed so that they can be used to write ordinary graphs with ordinary control parameters.
The brackets contain instructions that would otherwise be given and executed outside of the graph context.
-}
module Sound.SC3.UGen.Bracketed where

import Sound.SC3.Common.Enum {- hsc3 -}
import Sound.SC3.Common.Rate {- hsc3 -}
import Sound.SC3.Common.SoundFile {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.UGen.UGen {- hsc3 -}
import Sound.SC3.Server.Command.Plain {- hsc3 -}
import Sound.SC3.Server.Enum {- hsc3 -}

{- | Number of channels, either sfNc or the length of readChan.
     Checks requested channels are in range.
-}
readChanToNc :: Int -> [Int] -> Int
readChanToNc sfNc readChan =
  if null readChan
  then sfNc
  else if maximum readChan < sfNc
       then length readChan
       else error "readChanToNc: channel error"

{- | diskIn or vDiskIn with brackets to allocate and read and then close and free buffer.
     Ignoring the brackets, this is equivalent to writing a diskIn UGen with
     the number of channels derived from the named file.
     If readChan is empty all channels are read.
-}
sndfileDiskIn :: (Buffer_Id, String, [Int]) -> FilePath -> Maybe UGen -> Loop UGen -> UGen
sndfileDiskIn (bufId, ctlName, readChan) sndFileName maybeRate loop =
  let fileName = sfResolve sndFileName
      (sfNc,_sr,_nf) = sfInfo fileName
      bufSize = 65536
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = readChanToNc sfNc readChan
  in bracketUGen
     (maybe (diskIn bufNc buf loop) (\rate -> vDiskIn bufNc buf rate loop 0) maybeRate)
     ([b_alloc bufId bufSize bufNc, b_readChannel bufId fileName 0 (-1) 0 True readChan]
     ,[b_close bufId, b_free bufId])

-- | diskIn form of sndfileDiskIn
sndfileIn :: (Buffer_Id, String, [Int]) -> FilePath -> Loop UGen -> UGen
sndfileIn opt sndFileName loop = sndfileDiskIn opt sndFileName Nothing loop

-- | vDiskIn form of sndfileDiskIn
sndfileVarIn :: (Buffer_Id, String, [Int]) -> FilePath -> UGen -> Loop UGen -> UGen
sndfileVarIn opt sndFileName rate loop = sndfileDiskIn opt sndFileName (Just rate) loop

{- | Returns Buffer_Id as a bracketed buffer identifier UGen
     along with basic sound file information: numberOfChannels, sampleRate, numberOfFrames.
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
     The brackets will allocate and read and then free the buffer.
     Ignoring the brackets, and the sample rate and frame count,
     this can be used to write the synthdefs that function the same as if just having a buffer control input.
     If readChan is empty all channels are read.
-}
sndfileRead :: (Buffer_Id, String, [Int]) -> FilePath -> (UGen, Int, UGen, UGen)
sndfileRead (bufId, ctlName, readChan) sndFileName =
  let fileName = sfResolve sndFileName
      (sfNc, sfSr, sfNf) = sfInfo fileName
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = readChanToNc sfNc readChan
  in (bracketUGen buf ([b_allocReadChannel bufId fileName 0 0 readChan], [b_free bufId]), bufNc, constant sfSr, constant sfNf)

-- | Bracketed b_gen sine1
bGenSine1 :: (Buffer_Id, String, Int) -> [B_Gen] -> [Double] -> UGen
bGenSine1 (bufId, ctlName, numFrames) flags param =
  let buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = 1
  in bracketUGen buf ([b_alloc bufId numFrames bufNc, b_gen_sine1 bufId flags param], [b_free bufId])

-- | bGenSine1 with standard wavetable flags.
bGenSine1Tbl :: (Buffer_Id, String, Int) -> [Double] -> UGen
bGenSine1Tbl opt = bGenSine1 opt [Normalise, Wavetable, Clear]

-- | Bracketed b_gen sine1
bGenCheby :: (Buffer_Id, String, Int) -> [B_Gen] -> [Double] -> UGen
bGenCheby (bufId, ctlName, numFrames) flags param =
  let buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = 1
  in bracketUGen buf ([b_alloc bufId numFrames bufNc, b_gen_cheby bufId flags param], [b_free bufId])

-- | bGenCheby with standard wavetable flags.
bGenChebyTbl :: (Buffer_Id, String, Int) -> [Double] -> UGen
bGenChebyTbl opt = bGenCheby opt [Normalise, Wavetable, Clear]

