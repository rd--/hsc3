-- | Collection of modules for communicating with the SuperCollider
-- synthesis server, see also "Sound.SC3.Server.FD" and
-- "Sound.SC3.Server.Monad".
module Sound.SC3.Server (module S) where

import Sound.SC3.Server.Command as S
import Sound.SC3.Server.Enum as S
import Sound.SC3.Server.Synthdef as S
import Sound.SC3.Server.Status as S
import Sound.SC3.Server.NRT as S
import Sound.SC3.Server.NRT.Edit as S
import Sound.SC3.Server.Param as S
import Sound.SC3.Server.Recorder as S
