-- | Collection of modules for communicating with the SuperCollider
-- synthesis server, see also "Sound.Sc3.Server.FD" and
-- "Sound.Sc3.Server.Monad".
module Sound.Sc3.Server (module S) where

import Sound.Sc3.Server.Command as S
import Sound.Sc3.Server.Enum as S
import Sound.Sc3.Server.Synthdef as S
import Sound.Sc3.Server.Status as S
import Sound.Sc3.Server.Nrt as S
import Sound.Sc3.Server.Nrt.Edit as S
import Sound.Sc3.Server.Nrt.Render as S
import Sound.Sc3.Server.Param as S
import Sound.Sc3.Server.Recorder as S
import Sound.Sc3.Server.Scsynth as S
