{- | Bracketed Ugens.

ScSynth is controlled by sending instructions in the form of Open Sound Control (Osc) messages.
One family of messages allocate, set and free Buffers.
Ugen graphs that utilise Buffers don't contain the messages to manage them.
These messages are ordinarily written and sent outside of the graph context.

The bracketUgen function attaches a pair of Osc message sequences to a Ugen value.
The first sequence is to be sent before the graph the Ugen belongs to is started, the other after it has ended.
The messages are stored in the Ugen type, but are not written to the SynthDef file representing the Ugen graph.
The scsynthPlayAt function reads and sends Ugen bracket messages, in addition to the Ugen graph itself.

The functions defined here return Ugen values with brackets attached to them.
-}
module Sound.Sc3.Ugen.Bracketed where

import Sound.Sc3.Common.Enum {- hsc3 -}
import Sound.Sc3.Common.Rate {- hsc3 -}
import Sound.Sc3.Common.SoundFile {- hsc3 -}

import Sound.Sc3.Ugen.Bindings.Db {- hsc3 -}
import Sound.Sc3.Ugen.Ugen {- hsc3 -}
import Sound.Sc3.Ugen.Util {- hsc3 -}

import Sound.Sc3.Server.Command.Plain {- hsc3 -}
import Sound.Sc3.Server.Enum {- hsc3 -}

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
    else
      if maximum readChan < sfNc
        then length readChan
        else error "readChanToNc: channel error"

{- | diskIn or vDiskIn with brackets to 1. allocate and read and then 2. close and free buffer.
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
     Ignoring the brackets, this is equivalent to writing a diskIn or vDiskIn Ugen,
     with the number of channels given by readChan or derived from the named file.
     If readChan is empty all channels are read.
-}
sndfileDiskIn :: (String, Buffer_Id, [Int]) -> FilePath -> Maybe Ugen -> Loop Ugen -> Ugen
sndfileDiskIn (ctlName, bufId, readChan) sndFileName maybeRate loop =
  let fileName = sfResolve sndFileName
      (sfNc, _sr, _nf) = sfInfo fileName
      bufSize = 65536
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = readChanToNc sfNc readChan
  in bracketUgen
      (maybe (diskIn bufNc buf loop) (\rate -> vDiskIn bufNc buf rate loop 0) maybeRate)
      ( [b_alloc bufId bufSize bufNc, b_readChannel bufId fileName 0 (-1) 0 True readChan]
      , [b_close bufId, b_free bufId]
      )

-- | diskIn form of sndfileDiskIn
sndfileIn :: (String, Buffer_Id, [Int]) -> FilePath -> Loop Ugen -> Ugen
sndfileIn opt sndFileName loop = sndfileDiskIn opt sndFileName Nothing loop

-- | vDiskIn form of sndfileDiskIn
sndfileVarIn :: (String, Buffer_Id, [Int]) -> FilePath -> Ugen -> Loop Ugen -> Ugen
sndfileVarIn opt sndFileName rate loop = sndfileDiskIn opt sndFileName (Just rate) loop

{- | Returns Buffer_Id as a bracketed buffer identifier Ugen,
     along with basic sound file information: numberOfChannels, sampleRate, numberOfFrames.
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
     The brackets will 1. allocate and read and then 2. free the buffer.
     Ignoring the brackets, and the sample rate and frame count, this is equivalent to declaring a buffer identifier.
     If readChan is empty all channels are read.
-}
sndfileRead :: (String, Buffer_Id, [Int]) -> FilePath -> (Ugen, Int, Ugen, Ugen)
sndfileRead (ctlName, bufId, readChan) sndFileName =
  let fileName = sfResolve sndFileName
      (sfNc, sfSr, sfNf) = sfInfo fileName
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = readChanToNc sfNc readChan
  in (bracketUgen buf ([b_allocReadChannel bufId fileName 0 0 readChan], [b_free bufId]), bufNc, constant sfSr, constant sfNf)

{- | Bracketed b_gen sine1
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
-}
bGenSine1 :: (String, Buffer_Id, Int) -> [B_Gen] -> [Double] -> Ugen
bGenSine1 (ctlName, bufId, numFrames) flags param =
  let buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = 1
  in bracketUgen buf ([b_alloc bufId numFrames bufNc, b_gen_sine1 bufId flags param], [b_free bufId])

-- | bGenSine1 with standard wavetable flags (normalise and wavetable and clear).
bGenSine1Tbl :: (String, Buffer_Id, Int) -> [Double] -> Ugen
bGenSine1Tbl opt = bGenSine1 opt [Normalise, Wavetable, Clear]

{- | Bracketed b_gen sine1
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
-}
bGenCheby :: (String, Buffer_Id, Int) -> [B_Gen] -> [Double] -> Ugen
bGenCheby (ctlName, bufId, numFrames) flags param =
  let buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
      bufNc = 1
  in bracketUgen buf ([b_alloc bufId numFrames bufNc, b_gen_cheby bufId flags param], [b_free bufId])

-- | bGenCheby with standard wavetable flags (normalise and wavetable and clear).
bGenChebyTbl :: (String, Buffer_Id, Int) -> [Double] -> Ugen
bGenChebyTbl opt = bGenCheby opt [Normalise, Wavetable, Clear]
