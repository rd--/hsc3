-- | /Monad/ variant of interaction with the scsynth server.
module Sound.SC3.Server.Transport.Monad where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import Data.List {- base -}
import qualified Data.List.Split as Split {- split -}
import Data.Maybe {- base -}
import qualified Data.Tree as Tree {- containers -}
import qualified Safe {- safe -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Sound.OSC {- hosc -}

import Sound.SC3.Server.Command
import qualified Sound.SC3.Server.Command.Generic as Generic
import qualified Sound.SC3.Server.Enum as Enum
import qualified Sound.SC3.Server.Graphdef as Graphdef
import qualified Sound.SC3.Server.NRT as NRT
import qualified Sound.SC3.Server.Status as Status
import qualified Sound.SC3.Server.Synthdef as Synthdef

import Sound.SC3.UGen.Bindings.Composite (wrapOut)
import Sound.SC3.UGen.Type (UGen)

-- * hosc variants

-- | 'sendMessage' and 'waitReply' for a @\/done@ reply.
async :: DuplexOSC m => Message -> m Message
async m = sendMessage m >> waitReply "/done"

-- | 'void' of 'async'.
async_ :: DuplexOSC m => Message -> m ()
async_ = void . async

-- | If 'isAsync' then 'async_' else 'sendMessage'.
maybe_async :: DuplexOSC m => Message -> m ()
maybe_async m = if isAsync m then async_ m else sendMessage m

-- | Variant that timestamps synchronous messages.
maybe_async_at :: DuplexOSC m => Time -> Message -> m ()
maybe_async_at t m =
    if isAsync m
    then async_ m
    else sendBundle (bundle t [m])

-- | Local host (ie. @127.0.0.1@) at port @57110@.
sc3_default_udp :: IO UDP
sc3_default_udp = openUDP "127.0.0.1" 57110

-- | Maximum packet size, in bytes, that can be sent over UDP.
sc3_udp_limit :: Num n => n
sc3_udp_limit = 65507

-- | Bracket @SC3@ communication, ie. 'withTransport' 'sc3_default_udp'.
--
-- > import Sound.SC3.Server.Command
--
-- > withSC3 (sendMessage status >> waitReply "/status.reply")
withSC3 :: Connection UDP a -> IO a
withSC3 = withTransport sc3_default_udp

-- | 'void' of 'withSC3'.
withSC3_ :: Connection UDP a -> IO ()
withSC3_ = void . withSC3

-- | 'timeout_r' of 'withSC3'
withSC3_tm :: Double -> Connection UDP a -> IO (Maybe a)
withSC3_tm tm = timeout_r tm . withSC3

-- * Server control

-- | Free all nodes ('g_freeAll') at group @1@.
stop :: SendOSC m => m ()
stop = sendMessage (g_freeAll [1])

-- * Composite

-- | Runs 'clearSched' and then frees and re-creates groups @1@ and @2@.
reset :: SendOSC m => m ()
reset =
    let m = [clearSched
            ,n_free [1,2]
            ,g_new [(1,Enum.AddToHead,0),(2,Enum.AddToTail,0)]]
    in sendBundle (bundle immediately m)

-- | (node-id,add-action,group-id,parameters)
type Play_Opt = (Node_Id,Enum.AddAction,Group_Id,[(String,Double)])

-- | Make 's_new' message to play 'Graphdef.Graphdef'.
play_graphdef_msg :: Play_Opt -> Graphdef.Graphdef -> Message
play_graphdef_msg (nid,act,gid,param) g =
    let nm = ascii_to_string (Graphdef.graphdef_name g)
    in s_new nm nid act gid param

-- | If the graph size is less than 'sc3_udp_limit' encode and send
-- using 'd_recv_bytes', else write to temporary directory and read
-- using 'd_load'.
recv_or_load_graphdef :: Transport m => Graphdef.Graphdef -> m Message
recv_or_load_graphdef g = do
  tmp <- liftIO getTemporaryDirectory
  let nm = ascii_to_string (Graphdef.graphdef_name g)
      fn = tmp </> nm <.> "scsyndef"
      by = Graphdef.encode_graphdef g
      sz = L.length by
  if sz < sc3_udp_limit
    then async (d_recv_bytes by)
    else liftIO (Graphdef.graphdefWrite fn g) >> async (d_load fn)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playGraphdef :: Transport m => Play_Opt -> Graphdef.Graphdef -> m ()
playGraphdef opt g = recv_or_load_graphdef g >> sendMessage (play_graphdef_msg opt g)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: Transport m => Play_Opt -> Synthdef.Synthdef -> m ()
playSynthdef opt = playGraphdef opt . Synthdef.synthdef_to_graphdef

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: Transport m => Play_Opt -> UGen -> m ()
playUGen loc =
    playSynthdef loc .
    Synthdef.synthdef "Anonymous" .
    wrapOut Nothing

-- * NRT

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to the initial 'Time', then send each message, asynchronously if
-- required.
run_bundle :: Transport m => Time -> Bundle -> m ()
run_bundle t0 b = do
  let t = t0 + bundleTime b
      latency = 0.1
  liftIO (pauseThreadUntil (t - latency))
  mapM_ (maybe_async_at t) (bundleMessages b)

{- | Play an 'NRT' score (as would be rendered by 'writeNRT').

> let sc = NRT [bundle 1 [s_new0 "default" (-1) AddToHead 1]
>              ,bundle 2 [n_set1 (-1) "gate" 0]]
> in withSC3 (nrt_play sc)

-}
nrt_play :: Transport m => NRT.NRT -> m ()
nrt_play sc = do
  t0 <- liftIO time
  mapM_ (run_bundle t0) (NRT.nrt_bundles sc)

-- | Variant where asynchronous commands at time @0@ are separated out and run before
-- the initial time-stamp is taken.  This re-orders synchronous
-- commands in relation to asynchronous at time @0@.
nrt_play_reorder :: Transport m => NRT.NRT -> m ()
nrt_play_reorder s = do
  let (i,r) = NRT.nrt_span (<= 0) s
      i' = concatMap bundleMessages i
      (a,b) = partition_async i'
  mapM_ async a
  t <- liftIO time
  mapM_ (run_bundle t) (Bundle 0 b : r)

-- | 'withSC3' of 'nrt_play'.
nrt_audition :: NRT.NRT -> IO ()
nrt_audition = withSC3 . nrt_play

-- * Audible

-- | Class for values that can be encoded and send to @scsynth@ for audition.
class Audible e where
    play_at :: Transport m => Play_Opt -> e -> m ()
    -- | Variant where /id/ is @-1@.
    play :: Transport m => e -> m ()
    play = play_at (-1,Enum.AddToHead,1,[])

instance Audible Graphdef.Graphdef where
    play_at = playGraphdef

instance Audible Synthdef.Synthdef where
    play_at = playSynthdef

instance Audible UGen where
    play_at = playUGen

-- | 'withSC3' of 'play_at'.
audition_at :: Audible e => Play_Opt -> e -> IO ()
audition_at k = withSC3 . play_at k

-- | Variant where /id/ is @-1@.
audition :: Audible e => e -> IO ()
audition = audition_at (-1,Enum.AddToHead,1,[])

-- * Notifications

-- | Turn on notifications, run /f/, turn off notifications, return result.
withNotifications :: DuplexOSC m => m a -> m a
withNotifications f = do
  async_ (notify True)
  r <- f
  async_ (notify False)
  return r

-- * Buffer & control & node variants.

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3_tm 1.0 (b_getn1_data 0 (0,5))
b_getn1_data :: DuplexOSC m => Int -> (Int,Int) -> m [Double]
b_getn1_data b s = do
  let f m = let (_,_,_,r) = unpack_b_setn_err m in r
  sendMessage (b_getn1 b s)
  liftM f (waitReply "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
--
-- > withSC3_tm 1.0 (b_getn1_data_segment 1 0 (0,5))
b_getn1_data_segment :: DuplexOSC m =>
                        Int -> Int -> (Int,Int) -> m [Double]
b_getn1_data_segment n b (i,j) = do
  let ix = b_indices n j i
  d <- mapM (b_getn1_data b) ix
  return (concat d)

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: DuplexOSC m => Int -> Int -> m [[Double]]
b_fetch n b = do
  let f m = let (_,nf,nc,_) = unpack_b_info_err m
                ix = (0,nf * nc)
                deinterleave = transpose . Split.chunksOf nc
            in liftM deinterleave (b_getn1_data_segment n b ix)
  sendMessage (b_query1 b)
  waitReply "/b_info" >>= f

-- | First channel of 'b_fetch', errors if there is no data.
--
-- > withSC3 (b_fetch1 512 123456789)
b_fetch1 :: DuplexOSC m => Int -> Int -> m [Double]
b_fetch1 n b = liftM (Safe.headNote "b_fetch1: no data") (b_fetch n b)

-- | Combination of 'b_query1_unpack' and 'b_fetch'.
b_fetch_hdr :: Transport m => Int -> Int -> m ((Int,Int,Int,Double),[[Double]])
b_fetch_hdr k b = do
  q <- b_query1_unpack b
  d <- b_fetch k b
  return (q,d)

-- | 'b_info_unpack_err' of 'b_query1'.
b_query1_unpack_generic :: (DuplexOSC m,Num n,Fractional r) => Int -> m (n,n,n,r)
b_query1_unpack_generic n = do
  sendMessage (b_query1 n)
  q <- waitReply "/b_info"
  return (Generic.unpack_b_info_err q)

-- | Type specialised 'b_query1_unpack_generic'.
--
-- > withSC3 (b_query1_unpack 0)
b_query1_unpack :: DuplexOSC m => Buffer_Id -> m (Int,Int,Int,Double)
b_query1_unpack = b_query1_unpack_generic

-- | Variant of 'c_getn1' that waits for the reply and unpacks the data.
c_getn1_data :: DuplexOSC m => (Int,Int) -> m [Double]
c_getn1_data s = do
  let f d = case d of
              Int32 _:Int32 _:x -> mapMaybe datum_floating x
              _ -> error "c_getn1_data"
  sendMessage (c_getn1 s)
  liftM f (waitDatum "/c_setn")

-- | Apply /f/ to result of 'n_query'.
n_query1_unpack_f :: Transport m => (Message -> t) -> Node_Id -> m t
n_query1_unpack_f f n = do
  sendMessage (n_query [n])
  r <- waitReply "/n_info"
  return (f r)

-- | Variant of 'n_query' that waits for and unpacks the reply.
n_query1_unpack :: Transport m => Node_Id -> m (Maybe (Int,Int,Int,Int,Int,Maybe (Int,Int)))
n_query1_unpack = n_query1_unpack_f unpack_n_info

-- | Variant of 'n_query1_unpack' that returns plain (un-lifted) result.
n_query1_unpack_plain :: Transport m => Node_Id -> m [Int]
n_query1_unpack_plain = n_query1_unpack_f unpack_n_info_plain

-- | Variant of 'g_queryTree' that waits for and unpacks the reply.
g_queryTree1_unpack :: Transport m => Group_Id -> m Status.Query_Node
g_queryTree1_unpack n = do
  sendMessage (g_queryTree [(n,True)])
  r <- waitReply "/g_queryTree.reply"
  return (Status.queryTree (messageDatum r))

-- * Status

-- | Collect server status information.
--
-- > withSC3 serverStatus >>= mapM putStrLn
serverStatus :: DuplexOSC m => m [String]
serverStatus = liftM Status.statusFormat serverStatusData

-- | Read nominal sample rate of server.
--
-- > withSC3 serverSampleRateNominal
serverSampleRateNominal :: DuplexOSC m => m Double
serverSampleRateNominal = liftM (Status.extractStatusField 7) serverStatusData

-- | Read actual sample rate of server.
--
-- > withSC3 serverSampleRateActual
serverSampleRateActual :: DuplexOSC m => m Double
serverSampleRateActual = liftM (Status.extractStatusField 8) serverStatusData

-- | Retrieve status data from server.
serverStatusData :: DuplexOSC m => m [Datum]
serverStatusData = do
  sendMessage status
  waitDatum "/status.reply"

-- * Tree

-- | Collect server node tree information.
--
-- > withSC3 serverTree >>= mapM_ putStrLn
serverTree :: Transport m => m [String]
serverTree = do
  qt <- g_queryTree1_unpack 0
  let tr = Status.queryTree_rt qt
  return (["***** SuperCollider Server Tree *****",Tree.drawTree (fmap Status.query_node_pp tr)])
