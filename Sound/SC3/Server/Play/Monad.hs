module Sound.SC3.Server.Play.Monad where

import Sound.OpenSoundControl.Transport.Monad
import Sound.OpenSoundControl.Transport.UDP

-- | 'withTransport' at standard SC3 UDP port.
--
-- > import Sound.SC3.Server.Command
--
-- > withSC3 (sendOSC status >> recvPacket)
withSC3 :: Connection UDP a -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)
