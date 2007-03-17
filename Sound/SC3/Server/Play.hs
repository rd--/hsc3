module Sound.SC3.Server.Play (play, stop, reset, withSC3, audition) where

import Sound.OpenSoundControl
import Sound.SC3.UGen.UGen (UGen(..))
import Sound.SC3.UGen.IO (addOut)
import Sound.SC3.UGen.Graph (graph)
import Sound.SC3.Server.Graphdef (graphdef)
import Sound.SC3.Server.Command (AddAction(AddToTail), s_new, d_recv, g_new, g_freeAll)

-- | Construct an instrument definition, send /d_recv and /s_new messages to scsynth.
play :: Transport -> UGen -> IO OSC
play fd u = do let g = graphdef "Anonymous" (graph (addOut u))
               send fd (d_recv g) 
               r <- wait fd "/done"
               send fd (s_new "Anonymous" (-1) AddToTail 1 [])
               return r

-- | Free all nodes at the group with node id 1.
stop :: Transport -> IO ()
stop fd = send fd (g_freeAll [1])

-- | Free all nodes and re-create group node with id 1.
reset :: Transport -> IO ()
reset fd = do send fd (g_freeAll [0])
              send fd (g_new [(1, AddToTail, 0)])

-- | Bracket SC3 communication.
withSC3 :: (Transport -> IO a) -> IO a
withSC3 = withTransport (udp "127.0.0.1" 57110)

-- | withSC3 . play
audition :: UGen -> IO OSC
audition = withSC3 . (flip play)
