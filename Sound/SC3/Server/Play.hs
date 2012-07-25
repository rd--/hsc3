-- | Basic user interaction with the scsynth server.
module Sound.SC3.Server.Play (stop,reset,send,async
                             ,withSC3
                             ,Audible(..)
                             ,performNRT) where

import Sound.OpenSoundControl
import Sound.SC3.Server.Command
import Sound.SC3.Server.NRT
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.UGen

-- | Synonym for 'sendMessage'.
send :: (Transport t) => t -> Message -> IO ()
send = sendMessage

-- | Free all nodes ('g_freeAll') at group @1@.
stop :: Transport t => t -> IO ()
stop fd = sendMessage fd (g_freeAll [1])

-- | Send a 'Message' and wait for a @\/done@ reply.
async :: Transport t => t -> Message -> IO Message
async fd m = sendMessage fd m >> waitAddressMessage fd "/done"

-- | Free all nodes ('g_freeAll') at and re-create groups @1@ and @2@.
reset :: Transport t => t -> IO ()
reset fd = do
  sendMessage fd (g_freeAll [1,2])
  sendMessage fd (g_new [(1,AddToTail,0),(2,AddToTail,0)])

-- | Bracket @SC3@ communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: Transport t => t -> Synthdef -> IO ()
playSynthdef fd s = do
  _ <- async fd (d_recv s)
  sendMessage fd (s_new (synthdefName s) (-1) AddToTail 1 [])

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: Transport t => t -> UGen -> IO ()
playUGen fd = playSynthdef fd . synthdef "Anonymous"

-- | Class for values that can be encoded and send to @scsynth@ for
-- audition.
class Audible e where
    play :: Transport t => t -> e -> IO ()
    audition :: e -> IO ()
    audition e = withSC3 (`play` e)

instance Audible Graph where
    play fd g = playSynthdef fd (Synthdef "Anonymous" g)

instance Audible Synthdef where
    play = playSynthdef

instance Audible UGen where
    play = playUGen

instance Audible NRT where
    play = performNRT

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to initial 'UTCr' time, then send each message, asynchronously if
-- required.
run_bundle :: Transport t => t -> Double -> Bundle -> IO ()
run_bundle fd i (Bundle t x) =
    let wr m = if isAsync m
               then async fd m >> return ()
               else sendMessage fd m
    in case t of
          NTPr n -> do
                pauseThreadUntil (i + n)
                mapM_ wr x
          _ -> error "run_bundle: non-NTPr bundle"

-- | Perform an 'NRT' score (as would be rendered by 'writeNRT').  In
-- particular note that all timestamps /must/ be in 'NTPr' form.
performNRT :: Transport t => t -> NRT -> IO ()
performNRT fd s = utcr >>= \i -> mapM_ (run_bundle fd i) (nrt_bundles s)
