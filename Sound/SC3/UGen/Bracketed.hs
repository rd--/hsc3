{- | Bracketed UGens.

UGen brackets are a pair of OpenSoundControl message sequences that are attached to UGen values.
The first sequence is to be sent before the graph the UGen belongs to is started, the other after it has ended.
The brackets contain instructions that would otherwise be written outside of the graph context.
The functions defined here return UGen values with brackets attached to them.
It is possible to use these functions to write the same graphs as would be written without the brackets,
i.e. with control and channel count inputs.
scsynthPlayAt will read and send UGen bracket messages.
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

{- | sfNc is the number of channels at a sound file.
     readChan is a list of channels indexed to read.
     Returns a channel count, either sfNc or the length of readChan.
     If readChan in empty returns sfNc, else returns the length of readChan.
     This function checks that requested channels are in range.
-}
readChanToNc :: Int -> [Int] -> Int
readChanToNc sfNc readChan =
  if null readChan
  then sfNc
  else if maximum readChan < sfNc
       then length readChan
       else error "readChanToNc: channel error"

{- | diskIn or vDiskIn with brackets to 1. allocate and read and then 2. close and free buffer.
     Ignoring the brackets, this is equivalent to writing a diskIn UGen,
     with the number of channels given by readChan or derived from the named file.
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

{- | Returns Buffer_Id as a bracketed buffer identifier UGen,
     along with basic sound file information: numberOfChannels, sampleRate, numberOfFrames.
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
     The brackets will 1. allocate and read and then 2. free the buffer.
     Ignoring the brackets, and the sample rate and frame count,
     this can be used to write the synthdefs that are the same as if just having a buffer control input.
     If readChan is empty all channels are read.
-}
sndfileRead :: (Buffer_Id, String, [Int]) -> FilePath -> (UGen, Int, UGen, UGen)
sndfileRead (bufId, ctlName, readChan) sndFileName =
  let fileName = sfResolve sndFileName
      (sfNc, sfSr, sfNf) = sfInfo fileName
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = readChanToNc sfNc readChan
  in (bracketUGen buf ([b_allocReadChannel bufId fileName 0 0 readChan], [b_free bufId]), bufNc, constant sfSr, constant sfNf)

{- | Bracketed b_gen sine1
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
-}
bGenSine1 :: (Buffer_Id, String, Int) -> [B_Gen] -> [Double] -> UGen
bGenSine1 (bufId, ctlName, numFrames) flags param =
  let buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = 1
  in bracketUGen buf ([b_alloc bufId numFrames bufNc, b_gen_sine1 bufId flags param], [b_free bufId])

-- | bGenSine1 with standard wavetable flags (normalise and wavetable and clear).
bGenSine1Tbl :: (Buffer_Id, String, Int) -> [Double] -> UGen
bGenSine1Tbl opt = bGenSine1 opt [Normalise, Wavetable, Clear]

{- | Bracketed b_gen sine1
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
-}
bGenCheby :: (Buffer_Id, String, Int) -> [B_Gen] -> [Double] -> UGen
bGenCheby (bufId, ctlName, numFrames) flags param =
  let buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = 1
  in bracketUGen buf ([b_alloc bufId numFrames bufNc, b_gen_cheby bufId flags param], [b_free bufId])

-- | bGenCheby with standard wavetable flags (normalise and wavetable and clear).
bGenChebyTbl :: (Buffer_Id, String, Int) -> [Double] -> UGen
bGenChebyTbl opt = bGenCheby opt [Normalise, Wavetable, Clear]

