import qualified Sound.OSC as Osc {- hosc -}

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
