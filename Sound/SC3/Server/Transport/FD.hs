-- | /FD/ variant of interaction with the scsynth server.
--
-- This duplicates functions at 'Sound.SC3.Server.Transport.Monad' and
-- at some point at least part of the duplication will be removed.
module Sound.SC3.Server.Transport.FD where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import Data.List {- base -}
import qualified Data.List.Split as Split {- split -}
import System.FilePath {- filepath -}

import Sound.OSC.FD {- hosc -}

import Sound.SC3.Server.Command
import Sound.SC3.Server.Enum
import qualified Sound.SC3.Server.Graphdef as Graphdef
import Sound.SC3.Server.Nrt
import Sound.SC3.Server.Options
import Sound.SC3.Server.Status
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.Type

-- * hosc variants

-- | Send a 'Message' and 'waitReply' for a @\/done@ reply.
async :: Transport t => t -> Message -> IO Message
async fd m = sendMessage fd m >> waitReply fd "/done"

-- | If 'isAsync' then 'void' 'async' else 'sendMessage'.
maybe_async :: (Transport t) => t -> Message -> IO ()
maybe_async fd m = if isAsync m then void (async fd m) else sendMessage fd m

-- | Variant that timestamps synchronous messages.
maybe_async_at :: (Transport t) => t -> Time -> Message -> IO ()
maybe_async_at fd t m =
    if isAsync m
    then void (async fd m)
    else sendBundle fd (bundle t [m])

-- | Bracket @SC3@ communication.
withSC3 :: (UDP -> IO a) -> IO a
withSC3 = withTransport (openUDP "127.0.0.1" sc3_port_def)

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
playGraphdef :: Transport t => Int -> t -> Graphdef.Graphdef -> IO ()
playGraphdef k fd g = do
  let nm = ascii_to_string (Graphdef.graphdef_name g)
      fn = "/tmp" </> nm <.> "scsyndef"
      by = Graphdef.encode_graphdef g
      sz = L.length by
  if sz < 65507
    then void (async fd (d_recv_bytes by))
    else Graphdef.graphdefWrite fn g >> async fd (d_load fn) >> sendMessage fd (s_new0 nm k AddToTail 1)

-- | 'playGraphdef' of 'synthdef_to_graphdef'.
playSynthdef :: Transport t => Int -> t -> Synthdef -> IO ()
playSynthdef k fd = playGraphdef k fd . synthdef_to_graphdef

-- | Send an /anonymous/ instrument definition using 'playSynthdef'.
playUGen :: Transport t => Int -> t -> UGen -> IO ()
playUGen k fd = playSynthdef k fd . synthdef "Anonymous"

-- * Non-real time

-- | Wait ('pauseThreadUntil') until bundle is due to be sent relative
-- to initial 'Time', then send each message, asynchronously if
-- required.
run_bundle :: Transport t => t -> Time -> Bundle -> IO ()
run_bundle fd t0 b = do
  let t = t0 + bundleTime b
      latency = 0.1
  pauseThreadUntil (t - latency)
  mapM_ (maybe_async_at fd t) (bundleMessages b)

-- | Perform an 'Nrt' score (as would be rendered by 'writeNrt').  In
-- particular note that all timestamps /must/ be in 'NTPr' form.
nrt_play :: Transport t => t -> Nrt -> IO ()
nrt_play fd sc = time >>= \t0 -> mapM_ (run_bundle fd t0) (nrt_bundles sc)

-- | 'withSC3' of 'nrt_play'
nrt_audition :: Nrt -> IO ()
nrt_audition sc = withSC3 (`nrt_play` sc)

-- * Audible

-- | Class for values that can be encoded and sent to @scsynth@ for audition.
class Audible e where
    play_id :: Transport t => Int -> t -> e -> IO ()
    play :: Transport t => t -> e -> IO ()
    play = play_id (-1)

instance Audible Graphdef.Graphdef where
    play_id = playGraphdef

instance Audible Synthdef where
    play_id = playSynthdef

instance Audible UGen where
    play_id = playUGen

-- | 'withSC3' of 'play_id'
audition_id :: Audible e => Int -> e -> IO ()
audition_id k e = withSC3 (\fd -> play_id k fd e)

-- | 'audition_id' of @-1@.
audition :: Audible e => e -> IO ()
audition = audition_id (-1)

-- * Notifications

-- | Turn on notifications, run /f/, turn off notifications, return
-- result.
withNotifications :: Transport t => t -> (t -> IO a) -> IO a
withNotifications fd f = do
  _ <- async fd (notify True)
  r <- f fd
  _ <- async fd (notify False)
  return r

-- * Buffer

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3 (\fd -> b_getn1_data fd 0 (0,5))
b_getn1_data :: Transport t => t -> Int -> (Int,Int) -> IO [Double]
b_getn1_data fd b s = do
  let f m = let (_,_,_,r) = unpack_b_setn_err m in r
  sendMessage fd (b_getn1 b s)
  fmap f (waitReply fd "/b_setn")

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
b_fetch :: Transport t => t -> Int -> Int -> IO [[Double]]
b_fetch fd n b = do
  let f m = let (_,nf,nc,_) = unpack_b_info_err m
                ix = (0,nf * nc)
                deinterleave = transpose . Split.chunksOf nc
            in fmap deinterleave (b_getn1_data_segment fd n b ix)
  sendMessage fd (b_query1 b)
  waitReply fd "/b_info" >>= f

-- | 'head' of 'b_fetch'.
b_fetch1 :: Transport t => t -> Int -> Int -> IO [Double]
b_fetch1 fd n b = fmap head (b_fetch fd n b)

-- * Status

-- | Collect server status information.
serverStatus :: Transport t => t -> IO [String]
serverStatus = fmap statusFormat . serverStatusData

-- | Read nominal sample rate of server.
serverSampleRateNominal :: Transport t => t -> IO Double
serverSampleRateNominal = fmap (extractStatusField 7) . serverStatusData

-- | Read actual sample rate of server.
serverSampleRateActual :: Transport t => t -> IO Double
serverSampleRateActual = fmap (extractStatusField 8) . serverStatusData

-- | Retrieve status data from server.
serverStatusData :: Transport t => t -> IO [Datum]
serverStatusData fd = do
  sendMessage fd status
  waitDatum fd "/status.reply"
