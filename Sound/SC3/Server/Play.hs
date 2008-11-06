-- | Basic user interaction with the scsynth server.
module Sound.SC3.Server.Play ( play, stop, reset, send, async
                             , withSC3, audition ) where

import Sound.OpenSoundControl
import Sound.SC3.Server.Command
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.UGen

-- | Construct an instrument definition, send /d_recv and /s_new
-- | messages to scsynth.
play :: Transport t => t -> UGen -> IO OSC
play fd u = do let d = synthdef "Anonymous" u
               send fd (d_recv d) 
               r <- wait fd "/done"
               send fd (s_new "Anonymous" (-1) AddToTail 1 [])
               return r

-- | Free all nodes at the group with node id 1.
stop :: Transport t => t -> IO ()
stop fd = send fd (g_freeAll [1])

-- | Send an osc message and wait for a reply.
async :: Transport t => t -> OSC -> IO OSC
async fd m = send fd m >> wait fd "/done"

-- | Free all nodes and re-create group node with id 1.
reset :: Transport t => t -> IO ()
reset fd = do send fd (g_freeAll [0])
              send fd (g_new [(1, AddToTail, 0)])

-- | Bracket SC3 communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

-- | withSC3 . play
audition :: UGen -> IO ()
audition u = withSC3 (\fd -> play fd u) >> return ()
