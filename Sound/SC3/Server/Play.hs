-- | Basic user interaction with the scsynth server.
module Sound.SC3.Server.Play (stop,reset,send,async
                             ,withSC3
                             ,Audible(..)) where

import Sound.OpenSoundControl
import Sound.SC3.Server.Command
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.UGen

-- | Free all nodes at the group with node id 1.
stop :: Transport t => t -> IO ()
stop fd = send fd (g_freeAll [1])

-- | Send an osc message and wait for a reply.
async :: Transport t => t -> OSC -> IO OSC
async fd m = send fd m >> wait fd "/done"

-- | Free all nodes and re-create group nodes with ids 1 & 2.
reset :: Transport t => t -> IO ()
reset fd = do
  send fd (g_freeAll [0])
  send fd (g_new [(1,AddToTail,0),(2,AddToTail,0)])

-- | Bracket SC3 communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

-- | Send /d_recv and /s_new messages to scsynth.
playSynthdef :: Transport t => t -> Synthdef -> IO ()
playSynthdef fd s = do
  _ <- async fd (d_recv s)
  send fd (s_new (synthdefName s) (-1) AddToTail 1 [])

-- | Construct an instrument definition, send /d_recv and /s_new
--   messages to scsynth.
playUGen :: Transport t => t -> UGen -> IO ()
playUGen fd = playSynthdef fd . synthdef "Anonymous"

class Audible e where
    play :: Transport t => t -> e -> IO ()
    audition :: e -> IO ()
    audition e = withSC3 (\fd -> play fd e)

instance Audible Synthdef where
    play = playSynthdef

instance Audible UGen where
    play = playUGen
