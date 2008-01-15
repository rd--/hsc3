module Sound.SC3.Server.Play ( play, stop, reset
                             , withSC3, audition, auditionG ) where

import Sound.OpenSoundControl
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Graph
import Sound.SC3.Server.Graphdef
import Sound.SC3.Server.Command

playG :: Transport t => t -> Graph -> IO OSC
playG fd g = do let d = graphdef "Anonymous" g
                send fd (d_recv d) 
                r <- wait fd "/done"
                send fd (s_new "Anonymous" (-1) AddToTail 1 [])
                return r

-- | Construct an instrument definition, send /d_recv and /s_new
-- | messages to scsynth.
play :: Transport t => t -> UGen -> IO OSC
play fd = playG fd . graph

-- | Free all nodes at the group with node id 1.
stop :: Transport t => t -> IO ()
stop fd = send fd (g_freeAll [1])

-- | Free all nodes and re-create group node with id 1.
reset :: Transport t => t -> IO ()
reset fd = do send fd (g_freeAll [0])
              send fd (g_new [(1, AddToTail, 0)])

-- | Bracket SC3 communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

auditionG :: Graph -> IO ()
auditionG g = withSC3 (\fd -> playG fd g) >> return ()

-- | withSC3 . play
audition :: UGen -> IO ()
audition = auditionG . graph
