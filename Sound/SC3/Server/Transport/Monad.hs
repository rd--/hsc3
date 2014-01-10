-- | /Monad/ variant of interaction with the scsynth server.
module Sound.SC3.Server.Transport.Monad where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import Sound.OSC {- hosc -}

import Sound.SC3.Server.Command.Core
import Sound.SC3.Server.Command.Int
import Sound.SC3.Server.Enum
import Sound.SC3.Server.NRT
import Sound.SC3.Server.Status
import Sound.SC3.Server.Synthdef
import Sound.SC3.Server.Synthdef.Type
import Sound.SC3.UGen.Transform
import Sound.SC3.UGen.Type

-- * hosc variants

-- | Synonym for 'sendMessage'.
send :: SendOSC m => Message -> m ()
send = sendMessage

-- | Send a 'Message' and 'waitReply' for a @\/done@ reply.
async :: DuplexOSC m => Message -> m Message
async m = send m >> waitReply "/done"

-- | Bracket @SC3@ communication. 'withTransport' at standard SC3 UDP
-- port.
--
-- > import Sound.SC3.Server.Command
--
-- > withSC3 (send status >> waitReply "/status.reply")
withSC3 :: Connection UDP a -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" 57110)

-- * Server control

-- | Free all nodes ('g_freeAll') at group @1@.
stop :: SendOSC m => m ()
stop = send (g_freeAll [1])

-- * Composite

-- | 'clearSched', free all nodes ('g_freeAll') at, and then
-- re-create, groups @1@ and @2@.
reset :: SendOSC m => m ()
reset =
    let m = [clearSched
            ,g_freeAll [1,2]
            ,g_new [(1,AddToTail,0),(2,AddToTail,0)]]
    in sendBundle (bundle immediately m)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: DuplexOSC m => Int -> Synthdef -> m ()
playSynthdef nid s = do
  _ <- async (d_recv s)
  send (s_new0 (synthdefName s) nid AddToTail 1)

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: DuplexOSC m => Int -> UGen -> m ()
playUGen nid =
    playSynthdef nid .
    synthdef "Anonymous" .
    wrapOut (0.1::Double)

-- * NRT

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to the initial 'Time', then send each message, asynchronously if
-- required.
run_bundle :: Transport m => Time -> Bundle -> m ()
run_bundle st b = do
  let t = bundleTime b
      latency = 0.1
      wr m = if isAsync m
             then async m >> return ()
             else sendBundle (bundle (st + t) [m])
  liftIO (pauseThreadUntil (st + t - latency))
  mapM_ wr (bundleMessages b)

-- | Perform an 'NRT' score (as would be rendered by 'writeNRT').
--
-- > let sc = NRT [bundle 1 [s_new0 "default" (-1) AddToTail 1]
-- >              ,bundle 2 [n_set1 (-1) "gate" 0]]
-- > in withSC3 (performNRT sc)
performNRT :: Transport m => NRT -> m ()
performNRT s = do
  let (i,r) = nrt_span (<= 0) s
  t0 <- liftIO time
  mapM_ (run_bundle t0) i
  t1 <- liftIO time
  mapM_ (run_bundle t1) r

-- * Audible

-- | Class for values that can be encoded and send to @scsynth@ for
-- audition.
class Audible e where
    play :: Transport m => e -> m ()

instance Audible Graph where
    play g = playSynthdef (-1) (Synthdef "Anonymous" g)

instance Audible Synthdef where
    play = playSynthdef (-1)

instance Audible UGen where
    play = playUGen (-1)

instance Audible NRT where
    play = performNRT

audition :: Audible e => e -> IO ()
audition e = withSC3 (play e)

-- * Notifications

-- | Turn on notifications, run /f/, turn off notifications, return
-- result.
withNotifications :: DuplexOSC m => m a -> m a
withNotifications f = do
  _ <- async (notify True)
  r <- f
  _ <- async (notify False)
  return r

-- * Buffer

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3 (b_getn1_data 0 (0,5))
b_getn1_data :: DuplexOSC m => Int -> (Int,Int) -> m [Double]
b_getn1_data b s = do
  let f d = case d of
              Int32 _:Int32 _:Int32 _:x -> mapMaybe datum_floating x
              _ -> error "b_getn1_data"
  sendMessage (b_getn1 b s)
  liftM f (waitDatum "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
--
-- > withSC3 (b_getn1_data_segment 1 0 (0,5))
b_getn1_data_segment :: DuplexOSC m =>
                        Int -> Int -> (Int,Int) -> m [Double]
b_getn1_data_segment n b (i,j) = do
  let ix = b_indices n j i
  d <- mapM (b_getn1_data b) ix
  return (concat d)

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: DuplexOSC m => Int -> Int -> m [Double]
b_fetch n b = do
  let f d = case d of
              [Int32 _,Int32 nf,Int32 nc,Float _] ->
                  let ix = (0,fromIntegral (nf * nc))
                  in b_getn1_data_segment n b ix
              _ -> error "b_fetch"
  sendMessage (b_query1 b)
  waitDatum "/b_info" >>= f

-- * Status

-- | Collect server status information.
serverStatus :: DuplexOSC m => m [String]
serverStatus = liftM statusFormat serverStatusData

-- | Read nominal sample rate of server.
--
-- > withSC3 serverSampleRateNominal
serverSampleRateNominal :: DuplexOSC m => m Double
serverSampleRateNominal = liftM (extractStatusField 7) serverStatusData

-- | Read actual sample rate of server.
--
-- > withSC3 serverSampleRateActual
serverSampleRateActual :: DuplexOSC m => m Double
serverSampleRateActual = liftM (extractStatusField 8) serverStatusData

-- | Retrieve status data from server.
serverStatusData :: DuplexOSC m => m [Datum]
serverStatusData = do
  sendMessage status
  waitDatum "/status.reply"
