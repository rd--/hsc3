-- | Bracketed UGens.
module Sound.SC3.UGen.Bracketed where

import Sound.SC3.Common.Enum {- hsc3 -}
import Sound.SC3.Common.SoundFile {- hsc3 -}
import Sound.SC3.UGen.Bindings.DB {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}
import Sound.SC3.Server.Command.Plain {- hsc3 -}

{- | diskIn with brackets to allocate and read and then close and free buffer.

sinOsc ar 440 0 + sndfileIn 0 "20.2-LW+RD.flac" Loop
-}
sndfileIn :: Buffer_Id -> FilePath -> Loop UGen -> UGen
sndfileIn bufId sndFileName loop =
  let fileName = sfResolve sndFileName
      (nc,_sr,_nf) = sfInfo fileName
      bufSize = 65536
  in bracketUGen
     (diskIn nc (constant bufId) loop)
     ([b_alloc bufId bufSize nc, b_read bufId fileName 0 (-1) 0 True]
     ,[b_close bufId, b_free bufId])

{- | b_allocRead with brackets to allocate and read and then free a buffer.
     Returns bufId as a bracketed constant, and basic sound file information,
     (numberOfChannels, sampleRate, numberOfFrames).

let (buf, nc, sr, nf) = sndfileRead 0 "metal.wav"
    tr = impulse ar (nf / sr) 0
    ph = phasor ar tr (sr / sampleRate) 0 nf 0
in bufRdL nc ar buf ph NoLoop

-}
sndfileRead :: Buffer_Id -> FilePath -> (UGen, Int, UGen, UGen)
sndfileRead bufId sndFileName =
  let fileName = sfResolve sndFileName
      (nc,sr,nf) = sfInfo fileName
  in (bracketUGen (constant bufId) ([b_allocRead bufId fileName 0 0], [b_free bufId]), nc, constant sr, constant nf)
