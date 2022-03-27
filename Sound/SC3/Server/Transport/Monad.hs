-- | /Monad/ variant of interaction with the scsynth server.
module Sound.SC3.Server.Transport.Monad where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.List.Split as Split {- split -}
import qualified Data.Tree as Tree {- containers -}
import qualified Safe {- safe -}

import Sound.Osc {- hosc -}

import qualified Sound.SC3.Server.Command as Command
import qualified Sound.SC3.Server.Command.Generic as Generic
import qualified Sound.SC3.Server.Enum as Enum
import qualified Sound.SC3.Server.Graphdef as Graphdef
import qualified Sound.SC3.Server.Graphdef.Binary as Graphdef
import qualified Sound.SC3.Server.Nrt as Nrt
import qualified Sound.SC3.Server.Options as Options
import qualified Sound.SC3.Server.Status as Status
import qualified Sound.SC3.Server.Synthdef as Synthdef
import qualified Sound.SC3.UGen.Bindings.Composite as Composite
import qualified Sound.SC3.UGen.Type as UGen

{-
import qualified Control.Monad.IO.Class as M {- transformers -}
import qualified Control.Monad.Trans.Reader as R {- transformers -}
import qualified Sound.SC3.Server.Transport.FD as FD
-}

-- * hosc variants

-- | 'sendMessage' and 'waitReply' for a @\/done@ reply.
async :: DuplexOsc m => Message -> m Message
async m = sendMessage m >> waitReply "/done"

-- | 'void' of 'async'.
async_ :: DuplexOsc m => Message -> m ()
async_ = void . async

-- | If 'isAsync' then 'async_' else 'sendMessage'.
maybe_async :: DuplexOsc m => Message -> m ()
maybe_async m = if Command.isAsync m then async_ m else sendMessage m

-- | Variant that timestamps synchronous messages.
maybe_async_at :: DuplexOsc m => Time -> Message -> m ()
maybe_async_at t m =
    if Command.isAsync m
    then async_ m
    else sendBundle (bundle t [m])

-- | Hostname and port number.
type SC3_Address = (String, Int)

-- | Local host (ie. @127.0.0.1@) at port 'sc3_port_def'
sc3_default_udp :: SC3_Address
sc3_default_udp = (Options.sc3_addr_def,Options.sc3_port_def)

-- | Maximum packet size, in bytes, that can be sent over Udp.
--   However, see also <https://tools.ietf.org/html/rfc2675>
sc3_udp_limit :: Num n => n
sc3_udp_limit = 65507

-- | Bracket @SC3@ communication at indicated host and port.
withSC3At :: SC3_Address -> Connection Udp a -> IO a
withSC3At (h,p) = withTransport (openUdp h p)

-- | Bracket @SC3@ communication, ie. 'withSC3At' 'sc3_default_udp'.
--
-- > import Sound.SC3.Server.Command
--
-- > withSC3 (sendMessage status >> waitReply "/status.reply")
withSC3 :: Connection Udp a -> IO a
withSC3 = withSC3At sc3_default_udp

-- | 'void' of 'withSC3'.
withSC3_ :: Connection Udp a -> IO ()
withSC3_ = void . withSC3

-- | 'timeout_r' of 'withSC3'
withSC3_tm :: Double -> Connection Udp a -> IO (Maybe a)
withSC3_tm tm = timeout_r tm . withSC3

-- | Run /f/ at /k/ scsynth servers with sequential port numbers starting at 'Options.sc3_port_def'.
--
-- > withSC3AtSeq sc3_default_udp 2 (sendMessage status >> waitReply "/status.reply")
withSC3AtSeq :: SC3_Address -> Int -> Connection Udp a -> IO [a]
withSC3AtSeq (h,p) k f = do
  let mk_udp i = openUdp h (p + i)
  mapM (\i -> withTransport (mk_udp i) f) [0 .. k - 1]

-- | 'void' of 'withSC3AtSeq'.
withSC3AtSeq_ :: SC3_Address -> Int -> Connection Udp a -> IO ()
withSC3AtSeq_ loc k = void . withSC3AtSeq loc k

-- * Server control

-- | Free all nodes ('g_freeAll') at group @1@.
stop :: SendOsc m => m ()
stop = sendMessage (Command.g_freeAll [1])

-- * Composite

-- | Runs 'clearSched' and then frees and re-creates groups @1@ and @2@.
reset :: SendOsc m => m ()
reset =
    let m = [Command.clearSched
            ,Command.n_free [1,2]
            ,Command.g_new [(1,Enum.AddToHead,0),(2,Enum.AddToTail,0)]]
    in sendBundle (bundle immediately m)

-- | (node-id,add-action,group-id,parameters)
type Play_Opt = (Command.Node_Id,Enum.AddAction,Command.Group_Id,[(String,Double)])

-- | Make 's_new' message to play 'Graphdef.Graphdef'.
play_graphdef_msg :: Play_Opt -> Graphdef.Graphdef -> Message
play_graphdef_msg (nid,act,gid,param) g =
    let nm = ascii_to_string (Graphdef.graphdef_name g)
    in Command.s_new nm nid act gid param

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
    then async (Command.d_recv_bytes by)
    else liftIO (Graphdef.graphdefWrite fn g) >> async (Command.d_load fn)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playGraphdef :: Transport m => Play_Opt -> Graphdef.Graphdef -> m ()
playGraphdef opt g = recv_or_load_graphdef g >> sendMessage (play_graphdef_msg opt g)

-- | Send 'd_recv' and 's_new' messages to scsynth.
playSynthdef :: Transport m => Play_Opt -> Synthdef.Synthdef -> m ()
playSynthdef opt = playGraphdef opt . Synthdef.synthdef_to_graphdef

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: Transport m => Play_Opt -> UGen.UGen -> m ()
playUGen loc =
    playSynthdef loc .
    Synthdef.synthdef "Anonymous" .
    Composite.wrapOut Nothing

-- * Nrt

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to the initial 'Time', then send each message, asynchronously if
-- required.
run_bundle :: Transport m => Time -> Bundle -> m ()
run_bundle t0 b = do
  let t = t0 + bundleTime b
      latency = 0.1
  liftIO (pauseThreadUntil (t - latency))
  mapM_ (maybe_async_at t) (bundleMessages b)

{- | Play an 'Nrt' score (as would be rendered by 'writeNrt').

> let sc = Nrt [bundle 1 [s_new0 "default" (-1) AddToHead 1]
>              ,bundle 2 [n_set1 (-1) "gate" 0]]
> in withSC3 (nrt_play sc)

-}
nrt_play :: Transport m => Nrt.Nrt -> m ()
nrt_play sc = do
  t0 <- liftIO time
  mapM_ (run_bundle t0) (Nrt.nrt_bundles sc)

-- | Variant where asynchronous commands at time @0@ are separated out and run before
-- the initial time-stamp is taken.  This re-orders synchronous
-- commands in relation to asynchronous at time @0@.
nrt_play_reorder :: Transport m => Nrt.Nrt -> m ()
nrt_play_reorder s = do
  let (i,r) = Nrt.nrt_span (<= 0) s
      i' = concatMap bundleMessages i
      (a,b) = Command.partition_async i'
  mapM_ async a
  t <- liftIO time
  mapM_ (run_bundle t) (Bundle 0 b : r)

-- | 'withSC3' of 'nrt_play'.
nrt_audition :: Nrt.Nrt -> IO ()
nrt_audition = withSC3 . nrt_play

-- * Audible

-- | Class for values that can be encoded and send to @scsynth@ for audition.
class Audible e where
    playAt :: Transport m => Play_Opt -> e -> m ()
    -- | Variant where /id/ is @-1@.
    play :: Transport m => e -> m ()
    play = playAt (-1,Enum.AddToHead,1,[])

instance Audible Graphdef.Graphdef where
    playAt = playGraphdef

instance Audible Synthdef.Synthdef where
    playAt = playSynthdef

instance Audible UGen.UGen where
    playAt = playUGen

-- | 'withSC3At' of 'playAt'.
auditionAt :: Audible e => SC3_Address -> Play_Opt -> e -> IO ()
auditionAt loc opt = withSC3At loc . playAt opt

-- | 'withSC3AtSeq' of 'playAt'.
auditionAtSeq :: Audible e => SC3_Address -> Play_Opt -> Int -> e -> IO ()
auditionAtSeq loc opt k = withSC3AtSeq_ loc k . playAt opt

-- | Default 'Play_Opt', ie. (-1,addToHead,1,[])
def_play_opt :: Play_Opt
def_play_opt = (-1,Enum.AddToHead,1,[])

-- | 'auditionAt' 'def_play_opt'
audition :: Audible e => e -> IO ()
audition = auditionAt sc3_default_udp def_play_opt

-- | 'auditionAtSeq' 'def_play_opt'
auditionSeq :: Audible e => Int -> e -> IO ()
auditionSeq = auditionAtSeq sc3_default_udp def_play_opt

-- * Notifications

-- | Turn on notifications, run /f/, turn off notifications, return result.
withNotifications :: DuplexOsc m => m a -> m a
withNotifications f = do
  async_ (Command.notify True)
  r <- f
  async_ (Command.notify False)
  return r

-- * Buffer & control & node variants.

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3_tm 1.0 (b_getn1_data 0 (0,5))
b_getn1_data :: DuplexOsc m => Int -> (Int,Int) -> m [Double]
b_getn1_data b s = do
  let f m = let (_,_,_,r) = Command.unpack_b_setn_err m in r
  sendMessage (Command.b_getn1 b s)
  fmap f (waitReply "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
--
-- > withSC3_tm 1.0 (b_getn1_data_segment 1 0 (0,5))
b_getn1_data_segment :: DuplexOsc m =>
                        Int -> Int -> (Int,Int) -> m [Double]
b_getn1_data_segment n b (i,j) = do
  let ix = Command.b_indices n j i
  d <- mapM (b_getn1_data b) ix
  return (concat d)

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: DuplexOsc m => Int -> Int -> m [[Double]]
b_fetch n b = do
  let f m = let (_,nf,nc,_) = Command.unpack_b_info_err m
                ix = (0,nf * nc)
                deinterleave = transpose . Split.chunksOf nc
            in fmap deinterleave (b_getn1_data_segment n b ix)
  sendMessage (Command.b_query1 b)
  waitReply "/b_info" >>= f

-- | First channel of 'b_fetch', errors if there is no data.
--
-- > withSC3 (b_fetch1 512 123456789)
b_fetch1 :: DuplexOsc m => Int -> Int -> m [Double]
b_fetch1 n b = fmap (Safe.headNote "b_fetch1: no data") (b_fetch n b)

-- | Combination of 'b_query1_unpack' and 'b_fetch'.
b_fetch_hdr :: Transport m => Int -> Int -> m ((Int,Int,Int,Double),[[Double]])
b_fetch_hdr k b = do
  q <- b_query1_unpack b
  d <- b_fetch k b
  return (q,d)

-- | 'b_info_unpack_err' of 'b_query1'.
b_query1_unpack_generic :: (DuplexOsc m,Num n,Fractional r) => Int -> m (n,n,n,r)
b_query1_unpack_generic n = do
  sendMessage (Command.b_query1 n)
  q <- waitReply "/b_info"
  return (Generic.unpack_b_info_err q)

-- | Type specialised 'b_query1_unpack_generic'.
--
-- > withSC3 (b_query1_unpack 0)
b_query1_unpack :: DuplexOsc m => Command.Buffer_Id -> m (Int,Int,Int,Double)
b_query1_unpack = b_query1_unpack_generic

-- | Variant of 'c_getn1' that waits for the reply and unpacks the data.
c_getn1_data :: (DuplexOsc m,Floating t) => (Int,Int) -> m [t]
c_getn1_data s = do
  let f d = case d of
              Int32 _:Int32 _:x -> mapMaybe datum_floating x
              _ -> error "c_getn1_data"
  sendMessage (Command.c_getn1 s)
  fmap f (waitDatum "/c_setn")

-- | Apply /f/ to result of 'n_query'.
n_query1_unpack_f :: DuplexOsc m => (Message -> t) -> Command.Node_Id -> m t
n_query1_unpack_f f n = do
  sendMessage (Command.n_query [n])
  r <- waitReply "/n_info"
  return (f r)

-- | Variant of 'n_query' that waits for and unpacks the reply.
n_query1_unpack :: DuplexOsc m => Command.Node_Id -> m (Maybe (Int,Int,Int,Int,Int,Maybe (Int,Int)))
n_query1_unpack = n_query1_unpack_f Command.unpack_n_info

-- | Variant of 'n_query1_unpack' that returns plain (un-lifted) result.
n_query1_unpack_plain :: DuplexOsc m => Command.Node_Id -> m [Int]
n_query1_unpack_plain = n_query1_unpack_f Command.unpack_n_info_plain

-- | Variant of 'g_queryTree' that waits for and unpacks the reply.
g_queryTree1_unpack :: DuplexOsc m => Command.Group_Id -> m Status.Query_Node
g_queryTree1_unpack n = do
  sendMessage (Command.g_queryTree [(n,True)])
  r <- waitReply "/g_queryTree.reply"
  return (Status.queryTree (messageDatum r))

-- * Status

-- | Collect server status information.
--
-- > withSC3 serverStatus >>= mapM putStrLn
serverStatus :: DuplexOsc m => m [String]
serverStatus = fmap Status.statusFormat serverStatusData

-- | Collect server status information.
--
-- > withSC3 server_status_concise >>= putStrLn
server_status_concise :: DuplexOsc m => m String
server_status_concise = fmap Status.status_format_concise serverStatusData

-- | Read nominal sample rate of server.
--
-- > withSC3 serverSampleRateNominal
serverSampleRateNominal :: DuplexOsc m => m Double
serverSampleRateNominal = fmap (Status.extractStatusField 7) serverStatusData

-- | Read actual sample rate of server.
--
-- > withSC3 serverSampleRateActual
serverSampleRateActual :: DuplexOsc m => m Double
serverSampleRateActual = fmap (Status.extractStatusField 8) serverStatusData

-- | Retrieve status data from server.
serverStatusData :: DuplexOsc m => m [Datum]
serverStatusData = do
  sendMessage Command.status
  waitDatum "/status.reply"

-- * Tree

-- | Collect server node tree information.
--
-- > withSC3 serverTree >>= mapM_ putStrLn
serverTree :: DuplexOsc m => m [String]
serverTree = do
  qt <- g_queryTree1_unpack 0
  let tr = Status.queryTree_rt qt
  return ["***** SuperCollider Server Tree *****",Tree.drawTree (fmap Status.query_node_pp tr)]
