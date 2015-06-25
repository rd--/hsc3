-- | /Monad/ variant of interaction with the scsynth server.
module Sound.SC3.Server.Transport.Monad where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Sound.OSC {- hosc -}

import Sound.SC3.Server.Command
import Sound.SC3.Server.Enum
import qualified Sound.SC3.Server.Graphdef as G
import Sound.SC3.Server.NRT
import Sound.SC3.Server.Status
import Sound.SC3.Server.Synthdef

import Sound.SC3.UGen.Bindings.Composite (wrapOut)
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
            ,g_new [(1,AddToHead,0),(2,AddToTail,0)]]
    in sendBundle (bundle immediately m)

type Play_Opt = (Int,AddAction,Int,[(String,Double)])

-- | Send 'd_recv' and 's_new' messages to scsynth.
playGraphdef :: DuplexOSC m => Play_Opt -> G.Graphdef -> m ()
playGraphdef (nid,act,gid,param) g = do
  _ <- async (d_recv' g)
  send (s_new (ascii_to_string (G.graphdef_name g)) nid act gid param)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: DuplexOSC m => Play_Opt -> Synthdef -> m ()
playSynthdef opt = playGraphdef opt . synthdef_to_graphdef

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: DuplexOSC m => Play_Opt -> UGen -> m ()
playUGen loc =
    playSynthdef loc .
    synthdef "Anonymous" .
    wrapOut Nothing

-- * NRT

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to the initial 'Time', then send each message, asynchronously if
-- required.
run_bundle :: Transport m => Time -> Bundle -> m ()
run_bundle st b = do
  let t = bundleTime b
      latency = 0.1
      wr m = if isAsync m
             then void (async m)
             else sendBundle (bundle (st + t) [m])
  liftIO (pauseThreadUntil (st + t - latency))
  mapM_ wr (bundleMessages b)

-- | Perform an 'NRT' score (as would be rendered by 'writeNRT').
-- Asynchronous commands at time @0@ are separated out and run before
-- the initial time-stamp is taken.  This re-orders synchronous
-- commands in relation to asynchronous at time @0@.
--
-- > let sc = NRT [bundle 1 [s_new0 "default" (-1) AddToHead 1]
-- >              ,bundle 2 [n_set1 (-1) "gate" 0]]
-- > in withSC3 (performNRT sc)
performNRT :: Transport m => NRT -> m ()
performNRT s = do
  let (i,r) = nrt_span (<= 0) s
      i' = concatMap bundleMessages i
      (a,b) = partition_async i'
  mapM_ async a
  t <- liftIO time
  mapM_ (run_bundle t) (Bundle 0 b : r)

-- * Audible

-- | Class for values that can be encoded and send to @scsynth@ for
-- audition.
class Audible e where
    play_at :: Transport m => Play_Opt -> e -> m ()
    -- | Variant where /id/ is @-1@.
    play :: Transport m => e -> m ()
    play = play_at (-1,AddToHead,1,[])

instance Audible G.Graphdef where
    play_at = playGraphdef

instance Audible Synthdef where
    play_at = playSynthdef

instance Audible UGen where
    play_at = playUGen

instance Audible NRT where
    play_at _ = performNRT

-- | 'withSC3' of 'play_at'.
audition_at :: Audible e => Play_Opt -> e -> IO ()
audition_at k = withSC3 . play_at k

-- | Variant where /id/ is @-1@.
audition :: Audible e => e -> IO ()
audition = audition_at (-1,AddToHead,1,[])

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
b_fetch :: DuplexOSC m => Int -> Int -> m [[Double]]
b_fetch n b = do
  let f d = case d of
              [Int32 _,Int32 nf,Int32 nc,Float _] ->
                  let ix = (0,fromIntegral (nf * nc))
                      deinterleave = transpose . chunksOf (fromIntegral nc)
                  in liftM deinterleave (b_getn1_data_segment n b ix)
              _ -> error "b_fetch"
  sendMessage (b_query1 b)
  waitDatum "/b_info" >>= f

-- | First channel of 'b_fetch'.
b_fetch1 :: DuplexOSC m => Int -> Int -> m [Double]
b_fetch1 n b = liftM head (b_fetch n b)

c_getn1_data :: DuplexOSC m => (Int,Int) -> m [Double]
c_getn1_data s = do
  let f d = case d of
              Int32 _:Int32 _:x -> mapMaybe datum_floating x
              _ -> error "c_getn1_data"
  sendMessage (c_getn1 s)
  liftM f (waitDatum "/c_setn")

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
