-- | Basic user interaction with the scsynth server.
module Sound.SC3.Server.Play (stop,reset,send,async
                             ,withSC3
                             ,Audible(..)
                             ,perform) where

import Sound.OpenSoundControl
import Sound.SC3.Server.Command
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.UGen

-- | Free all nodes ('g_freeAll') at group @1@.
stop :: Transport t => t -> IO ()
stop fd = send fd (g_freeAll [1])

-- | Send an 'OSC' message and wait for a @\/done@ reply.
async :: Transport t => t -> OSC -> IO OSC
async fd m = send fd m >> wait fd "/done"

-- | Free all nodes ('g_freeAll') at group @0@ and re-create groups
-- @1@ and @2@.
reset :: Transport t => t -> IO ()
reset fd = do
  send fd (g_freeAll [0])
  send fd (g_new [(1,AddToTail,0),(2,AddToTail,0)])

-- | Bracket @SC3@ communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: Transport t => t -> Synthdef -> IO ()
playSynthdef fd s = do
  _ <- async fd (d_recv s)
  send fd (s_new (synthdefName s) (-1) AddToTail 1 [])

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
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

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to initial 'UTCr' time, then send each message, asynchronously if
-- required.
run_bundle :: Transport t => t -> Double -> OSC -> IO ()
run_bundle fd i o =
    let wr m = if isAsync m
               then async fd m >> return ()
               else send fd m
    in case o of
         Bundle (NTPr t) x' -> do
             pauseThreadUntil (i + t)
             mapM_ wr x'
         _ -> error "run_bundle: non bundle or non-NTPr bundle"

-- | Perform an 'OSC' score (as would be rendered by 'writeNRT').  In
-- particular note that: (1) all 'OSC' must be 'Bundle's and (2)
-- timestamps /must/ be in 'NTPr' form.
perform :: [OSC] -> IO ()
perform s = do
  let f i fd x = run_bundle fd i x
  withSC3 (\fd -> utcr >>= \i -> mapM_ (f i fd) s)
