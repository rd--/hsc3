-- | Collection of modules for communicating with the SuperCollider
--   synthesis server.
module Sound.SC3.Server (module S) where

import Sound.SC3.Server.Buffer as S
import Sound.SC3.Server.Command as S
import Sound.SC3.Server.Synthdef as S
import Sound.SC3.Server.Play as S
import Sound.SC3.Server.Status as S
import Sound.SC3.Server.NRT as S
