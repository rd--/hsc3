-- | /FD/ variant of interaction with the scsynth server.
module Sound.SC3.Server.Transport.FD where

import Control.Monad
import Sound.OpenSoundControl.Transport.FD
import Sound.OpenSoundControl.Transport.UDP
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait.FD
import Sound.SC3.Server.Command
import Sound.SC3.Server.NRT
import Sound.SC3.Server.Status
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.UGen

-- * hosc variants.

-- | Synonym for 'sendMessage'.
send :: (Transport t) => t -> Message -> IO ()
send = sendMessage

-- | Synonym for 'waitReply'.
wait :: Transport t => t -> String -> IO Message
wait fd = waitReply fd

-- | Send a 'Message' and 'wait' for a @\/done@ reply.
async :: Transport t => t -> Message -> IO Message
async fd m = sendMessage fd m >> wait fd "/done"

-- | Bracket @SC3@ communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

-- * Server control

-- | Free all nodes ('g_freeAll') at group @1@.
stop :: Transport t => t -> IO ()
stop fd = sendMessage fd (g_freeAll [1])

-- | Free all nodes ('g_freeAll') at and re-create groups @1@ and @2@.
reset :: Transport t => t -> IO ()
reset fd = do
  sendMessage fd (g_freeAll [1,2])
  sendMessage fd (g_new [(1,AddToTail,0),(2,AddToTail,0)])

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: Transport t => t -> Synthdef -> IO ()
playSynthdef fd s = do
  _ <- async fd (d_recv s)
  sendMessage fd (s_new (synthdefName s) (-1) AddToTail 1 [])

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: Transport t => t -> UGen -> IO ()
playUGen fd = playSynthdef fd . synthdef "Anonymous"

-- * Non-real time

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

-- * Audible

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

-- * Buffer

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3 (\fd -> b_getn1_data fd 0 (0,5))
b_getn1_data :: Transport t => t -> Int -> (Int,Int) -> IO [Double]
b_getn1_data fd b s = do
  let f d = case d of
              Int _:Int _:Int _:x -> map datum_real_err x
              _ -> error "b_getn1_data"
  sendMessage fd (b_getn1 b s)
  fmap f (waitDatum fd "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
--
-- > withSC3 (\fd -> b_getn1_data_segment fd 1 0 (0,5))
b_getn1_data_segment :: Transport t => t -> Int -> Int -> (Int,Int) -> IO [Double]
b_getn1_data_segment fd n b (i,j) = do
  let ix = b_indices n j i
  d <- mapM (b_getn1_data fd b) ix
  return (concat d)

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: Transport t => t -> Int -> Int -> IO [Double]
b_fetch fd n b = do
  let f d = case d of
              [Int _,Int nf,Int nc,Float _] ->
                  let ix = (0,nf * nc)
                  in b_getn1_data_segment fd n b ix
              _ -> error "b_fetch"
  sendMessage fd (b_query1 b)
  waitDatum fd "/b_info" >>= f


-- * Status

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus = liftM statusFormat . serverStatusData

-- | Read nominal sample rate of server.
serverSampleRateNominal :: (Transport t) => t -> IO Double
serverSampleRateNominal = liftM (extractStatusField 7) . serverStatusData

-- | Read actual sample rate of server.
serverSampleRateActual :: (Transport t) => t -> IO Double
serverSampleRateActual = liftM (extractStatusField 8) . serverStatusData

-- | Retrieve status data from server.
serverStatusData :: Transport t => t -> IO [Datum]
serverStatusData fd = do
  sendMessage fd status
  waitDatum fd "/status.reply"
