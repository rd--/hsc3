{- | Bracketed UGens.

'Brackets' are a pair of sequences of OpenSoundControl messages that can be attached to UGen values.
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

{- | diskIn with brackets to allocate and read and then close and free buffer.
     Ignoring the brackets, this is equivalent to writing a diskIn UGen with
     the number of channels derived from the named file.
-}
sndfileIn :: (Buffer_Id, String) -> FilePath -> Loop UGen -> UGen
sndfileIn (bufId, ctlName) sndFileName loop =
  let fileName = sfResolve sndFileName
      (nc,_sr,_nf) = sfInfo fileName
      bufSize = 65536
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
  in bracketUGen
     (diskIn nc buf loop)
     ([b_alloc bufId bufSize nc, b_read bufId fileName 0 (-1) 0 True]
     ,[b_close bufId, b_free bufId])

{- | Returns Buffer_Id as a bracketed buffer identifier UGen
     along with basic sound file information: numberOfChannels, sampleRate, numberOfFrames.
     If ctlName is empty the buffer is returned as a constant, else as a control with the given name.
     The brackets will allocate and read and then free the buffer.
     Ignoring the brackets, and the sample rate and frame count,
     this can be used to write the synthdefs that function the same as if just having a buffer control input.
-}
sndfileRead :: (Buffer_Id, String) -> FilePath -> (UGen, Int, UGen, UGen)
sndfileRead (bufId, ctlName) sndFileName =
  let fileName = sfResolve sndFileName
      (nc,sr,nf) = sfInfo fileName
      buf = if null ctlName then constant bufId else control kr ctlName (fromIntegral bufId)
  in (bracketUGen buf ([b_allocRead bufId fileName 0 0], [b_free bufId]), nc, constant sr, constant nf)
