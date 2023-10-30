-- | Generic constructors for the command set implemented by the SuperCollider synthesis server.
module Sound.Sc3.Server.Command.Generic where

import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Osc.Core {- hosc -}

import qualified Sound.Sc3.Common.Base as Common.Base
import qualified Sound.Sc3.Server.Command.Enum as Server.Command.Enum
import qualified Sound.Sc3.Server.Enum as Server.Enum
import qualified Sound.Sc3.Server.Graphdef as Server.Graphdef
import qualified Sound.Sc3.Server.Graphdef.Binary as Server.Graphdef
import qualified Sound.Sc3.Server.Synthdef as Server.Synthdef

cmd_check_arg :: String -> (t -> Bool) -> t -> t
cmd_check_arg e f x = if not (f x) then error e else x

-- * Buffer commands (b_)

-- | Buf-Num must be >= 0
b_bufnum :: Integral t => t -> Datum
b_bufnum = int32 . cmd_check_arg "buffer-number < 0?" (>= 0)

-- | Buf-Frame-Ix must be >= 0
b_ix :: Integral t => t -> Datum
b_ix = int32 . cmd_check_arg "buffer-ix < 0?" (>= 0)

-- | Buf-Channel must be >= 0
b_ch :: Integral t => t -> Datum
b_ch = int32 . cmd_check_arg "buffer-channel < 0?" (>= 0)

-- | Buf-Frame-Cnt must be >= 0
b_size :: Integral t => t -> Datum
b_size = int32 . cmd_check_arg "buffer-size < 0?" (>= 0)

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Integral i => i -> i -> i -> Message
b_alloc b frames channels = message "/b_alloc" [b_bufnum b,b_size frames,int32 channels]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Integral i => i -> String -> i -> i -> Message
b_allocRead bufferNumber fileName startFrame frameCount =
  message "/b_allocRead" [b_bufnum bufferNumber
                         ,string fileName
                         ,b_ix startFrame
                         ,b_ix frameCount]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Integral i => i -> String -> i -> i -> [i] -> Message
b_allocReadChannel bufferNumber fileName startFrame frameCount channels =
  message "/b_allocReadChannel" ([b_bufnum bufferNumber
                                 ,string fileName
                                 ,b_ix startFrame
                                 ,b_ix frameCount] ++ map b_ch channels)

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Integral i => i -> Message
b_close b = message "/b_close" [b_bufnum b]

-- | Fill ranges of sample values.
b_fill :: (Integral i,Real n) => i -> [(i,i,n)] -> Message
b_fill b l = message "/b_fill" (b_bufnum b : Common.Base.mk_triples int32 int32 float l)

-- | Free buffer data. (Asynchronous)
b_free :: Integral i => i -> Message
b_free b = message "/b_free" [b_bufnum b]

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: Integral i => i -> String -> [Datum] -> Message
b_gen b name arg = message "/b_gen" (b_bufnum b : string name : arg)

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: (Integral i,Real n) => i -> [Server.Enum.B_Gen] -> [n] -> Message
b_gen_sine1 z f n = b_gen z "sine1" (int32 (Server.Enum.b_gen_flag f) : map float n)

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: (Integral i,Real n) => i -> [Server.Enum.B_Gen] -> [(n,n)] -> Message
b_gen_sine2 z f n = b_gen z "sine2" (int32 (Server.Enum.b_gen_flag f) : Common.Base.mk_duples float float n)

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: (Integral i,Real n) => i -> [Server.Enum.B_Gen] -> [(n,n,n)] -> Message
b_gen_sine3 z f n = b_gen z "sine3" (int32 (Server.Enum.b_gen_flag f) : Common.Base.mk_triples float float float n)

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: (Integral i,Real n) => i -> [Server.Enum.B_Gen] -> [n] -> Message
b_gen_cheby z f n = b_gen z "cheby" (int32 (Server.Enum.b_gen_flag f) : map float n)

-- | Call @copy@ 'b_gen' command.
b_gen_copy :: Integral i => i -> i -> i -> i -> Maybe i -> Message
b_gen_copy dst_b dst_ix src_b src_ix nf =
    let nf' = fromMaybe (-1) nf
    in b_gen dst_b "copy" (map int32 [dst_ix,src_b,src_ix,nf'])

-- | Get sample values.
b_get :: Integral i => i -> [i] -> Message
b_get b i = message "/b_get" (b_bufnum b : map int32 i)

-- | Get ranges of sample values.
b_getn :: Integral i => i -> [(i,i)] -> Message
b_getn b l = message "/b_getn" (b_bufnum b : Common.Base.mk_duples b_ix b_size l)

-- | Request \/b_info messages.
b_query :: Integral i => [i] -> Message
b_query = message "/b_query" . map int32

{- | Read sound file data into an existing buffer. (Asynchronous)
     Param: bufId pathName startFrame numFrames bufFrame leaveOpen
-}
b_read :: Integral i => i -> String -> i -> i -> i -> Bool -> Message
b_read bufId pathName startFrame numFrames bufFrame leaveOpen =
  message "/b_read" [b_bufnum bufId,string pathName,int32 startFrame,int32 numFrames,int32 bufFrame,int32 (fromEnum leaveOpen)]

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: Integral i => i -> String -> i -> i -> i -> Bool -> [i] -> Message
b_readChannel b p f n f' z cs = message "/b_readChannel" ([b_bufnum b,string p,int32 f,int32 n,int32 f',int32 (fromEnum z)] ++ map int32 cs)

-- | Set sample values.
b_set :: (Integral i,Real n) => i -> [(i,n)] -> Message
b_set b l = message "/b_set" (b_bufnum b : Common.Base.mk_duples int32 float l)

-- | Set ranges of sample values.
b_setn :: (Integral i,Real n) => i -> [(i,[n])] -> Message
b_setn b l =
    let f (i,d) = int32 i : int32 (length d) : map float d
    in message "/b_setn" (b_bufnum b : concatMap f l)

-- | Write sound file data. (Asynchronous)
b_write :: Integral i => i -> String -> Server.Enum.SoundFileFormat -> Server.Enum.SampleFormat -> i -> i -> Bool -> Message
b_write b p h t f s z =
    let h' = string (Server.Enum.soundFileFormatString h)
        t' = string (Server.Enum.sampleFormatString t)
    in message "/b_write" [b_bufnum b,string p,h',t',int32 f,int32 s,int32 (fromEnum z)]

-- | Zero sample data. (Asynchronous)
b_zero :: Integral i => i -> Message
b_zero b = message "/b_zero" [b_bufnum b]

-- * Control bus commands (c_)

-- |  Fill ranges of bus values.
c_fill :: (Integral i,Real n) => [(i,i,n)] -> Message
c_fill = message "/c_fill" . Common.Base.mk_triples int32 int32 float

-- | Get bus values.
c_get :: Integral i => [i] -> Message
c_get = message "/c_get" . map int32

-- | Get ranges of bus values.
c_getn :: Integral i => [(i,i)] -> Message
c_getn = message "/c_getn" . Common.Base.mk_duples int32 int32

-- | Set bus values.
c_set :: (Integral i,Real n) => [(i,n)] -> Message
c_set = message "/c_set" . Common.Base.mk_duples int32 float

-- | Set ranges of bus values.
c_setn :: (Integral i,Real n) => [(i,[n])] -> Message
c_setn l =
    let f (i,d) = int32 i : int32 (length d) : map float d
    in message "/c_setn" (concatMap f l)

-- * Instrument definition commands (d_)

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv_bytes :: Blob -> Message
d_recv_bytes b = message "/d_recv" [Blob b]

-- | Graphdef encoding variant.
d_recv_gr :: Server.Graphdef.Graphdef -> Message
d_recv_gr = d_recv_bytes . Server.Graphdef.encode_graphdef

-- | Synthdef encoding variant.
d_recv :: Server.Synthdef.Synthdef -> Message
d_recv = d_recv_bytes . Server.Synthdef.synthdefData

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> Message
d_load p = message "/d_load" [string p]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> Message
d_loadDir p = message "/d_loadDir" [string p]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> Message
d_free = message "/d_free" . map string

-- * Group node commands (g_)

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: Integral i => [i] -> Message
g_deepFree = message "/g_deepFree" . map int32

-- | Delete all nodes in a group.
g_freeAll :: Integral i => [i] -> Message
g_freeAll = message "/g_freeAll" . map int32

-- | Add node to head of group.
g_head :: Integral i => [(i,i)] -> Message
g_head = message "/g_head" . Common.Base.mk_duples int32 int32

-- | Create a new group.
g_new :: Integral i => [(i,Server.Enum.AddAction,i)] -> Message
g_new = message "/g_new" . Common.Base.mk_triples int32 (int32 . fromEnum) int32

-- | Add node to tail of group.
g_tail :: Integral i => [(i,i)] -> Message
g_tail = message "/g_tail" . Common.Base.mk_duples int32 int32

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: Integral i => [(i,Bool)] -> Message
g_dumpTree = message "/g_dumpTree" . Common.Base.mk_duples int32 (int32 . fromEnum)

{- | Request a representation of a group's node subtree, optionally including the current control values for synths.

Replies to the sender with a @/g_queryTree.reply@ message listing all of the nodes contained within the group in the following format:

@
int32 - if synth control values are included 1, else 0
int32 - node ID of the requested group
int32 - number of child nodes contained within the requested group

For each node in the subtree:
[
  int32 - node ID
  int32 - number of child nodes contained within this node. If -1 this is a synth, if >= 0 it's a group.

  If this node is a synth:
    symbol - the SynthDef name for this node.

  If flag (see above) is true:
    int32 - numControls for this synth (M)
    [
      symbol or int: control name or index
      float or symbol: value or control bus mapping symbol (e.g. 'c1')
    ] * M
] * the number of nodes in the subtree
@
N.b. The order of nodes corresponds to their execution order on the server. Thus child nodes (those contained within a group) are listed immediately following their parent.
-}
g_queryTree :: Integral i => [(i,Bool)] -> Message
g_queryTree = message "/g_queryTree" . Common.Base.mk_duples int32 (int32 . fromEnum)

-- * Node commands (n_)

-- | Node-Id must be >= -1
n_id :: Integral t => t -> Datum
n_id = int32 . cmd_check_arg "node-id < -1?" (>= (-1))

-- | Place a node after another.
n_after :: Integral i => [(i,i)] -> Message
n_after = message "/n_after" . Common.Base.mk_duples n_id n_id

-- | Place a node before another.
n_before :: Integral i => [(i,i)] -> Message
n_before = message "/n_before" . Common.Base.mk_duples int32 int32

-- | Fill ranges of a node's control values.
n_fill :: (Integral i,Real f) => i -> [(String,i,f)] -> Message
n_fill n l = message "/n_fill" (n_id n : Common.Base.mk_triples string int32 float l)

-- | Delete a node.
n_free :: Integral i => [i] -> Message
n_free = message "/n_free" . map n_id

n_map :: Integral i => i -> [(String,i)] -> Message
n_map n l = message "/n_map" (n_id n : Common.Base.mk_duples string int32 l)

-- | Map a node's controls to read from buses.
--   n_mapn only works if the control is given as an index and not as a name (3.8.0).
n_mapn :: Integral i => i -> [(i,i,i)] -> Message
n_mapn n l = message "/n_mapn" (n_id n : Common.Base.mk_triples int32 int32 int32 l)

-- | Map a node's controls to read from an audio bus.
n_mapa :: Integral i => i -> [(String,i)] -> Message
n_mapa n l = message "/n_mapa" (n_id n : Common.Base.mk_duples string int32 l)

-- | Map a node's controls to read from audio buses.
n_mapan :: Integral i => i -> [(String,i,i)] -> Message
n_mapan n l = message "/n_mapan" (n_id n : Common.Base.mk_triples string int32 int32 l)

-- | Get info about a node.
n_query :: Integral i => [i] -> Message
n_query = message "/n_query" . map n_id

-- | Turn node on or off.
n_run :: Integral i => [(i,Bool)] -> Message
n_run = message "/n_run" . Common.Base.mk_duples n_id (int32 . fromEnum)

-- | Set a node's control values.
n_set :: (Integral i,Real n) => i -> [(String,n)] -> Message
n_set n c = message "/n_set" (n_id n : Common.Base.mk_duples string float c)

-- | Set ranges of a node's control values.
-- n_mapn and n_setn only work if the control is given as an index and not as a name.
n_setn :: (Integral i,Real n) => i -> [(i,[n])] -> Message
n_setn n l =
    let f (s,d) = int32 s : int32 (length d) : map float d
    in message "/n_setn" (n_id n : concatMap f l)

-- | Trace a node.
n_trace :: Integral i => [i] -> Message
n_trace = message "/n_trace" . map int32

-- | Move an ordered sequence of nodes.
n_order :: Integral i => Server.Enum.AddAction -> i -> [i] -> Message
n_order a n ns = message "/n_order" (int32 (fromEnum a) : int32 n : map int32 ns)

-- * Par commands (p_)

-- | Create a new parallel group (supernova specific).
p_new :: Integral i => [(i,Server.Enum.AddAction,i)] -> Message
p_new = message "/p_new" . Common.Base.mk_triples int32 (int32 . fromEnum) int32

-- * Synthesis node commands (s_)

-- | Get control values.
s_get :: Integral i => i -> [String] -> Message
s_get n i = message "/s_get" (n_id n : map string i)

-- | Get ranges of control values.
s_getn :: Integral i => i -> [(String,i)] -> Message
s_getn n l = message "/s_getn" (n_id n : Common.Base.mk_duples string int32 l)

-- | Create a new synth.
s_new :: (Integral i,Real n) => String -> i -> Server.Enum.AddAction -> i -> [(String,n)] -> Message
s_new synthdefName nodeId addAction targetId controlValues =
  message "/s_new" (string synthdefName :
                     int32 nodeId :
                     int32 (fromEnum addAction) :
                     int32 targetId :
                     Common.Base.mk_duples string float controlValues)

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: Integral i => [i] -> Message
s_noid = message "/s_noid" . map int32

-- * Ugen commands (u_)

-- | Send a command to a unit generator.
u_cmd :: Integral i => i -> i -> String -> [Datum] -> Message
u_cmd n uid name arg = message "/u_cmd" ([n_id n,int32 uid,string name] ++ arg)

-- * Server operation commands

-- | Send a plugin command.
cmd :: String -> [Datum] -> Message
cmd name = message "/cmd" . (string name :)

-- | Remove all bundles from the scheduling queue.
clearSched :: Message
clearSched = message "/clearSched" []

-- | Select printing of incoming Open Sound Control messages.
dumpOsc :: Server.Enum.PrintLevel -> Message
dumpOsc c = message "/dumpOSC" [int32 (fromEnum c)]

-- | Set error posting scope and mode.
errorMode :: Server.Enum.ErrorScope -> Server.Enum.ErrorMode -> Message
errorMode scope mode =
    let e = case scope of
              Server.Enum.Globally -> fromEnum mode
              Server.Enum.Locally  -> -1 - fromEnum mode
    in message "/error" [int32 e]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> Message
notify c = message "/notify" [int32 (fromEnum c)]

-- | End real time mode, close file (un-implemented).
nrt_end :: Message
nrt_end = message "/nrt_end" []

-- | Stop synthesis server.
quit :: Message
quit = message "/quit" []

-- | Request \/status.reply message.
status :: Message
status = message "/status" []

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Integral i => i -> Message
sync sid = message "/sync" [int32 sid]

-- * Modify existing message to include completion message

-- | Add a completion packet to an existing asynchronous command.
with_completion_packet :: Message -> Packet -> Message
with_completion_packet (Message c xs) cm =
    if c `elem` Server.Command.Enum.async_cmds
    then let xs' = xs ++ [Blob (encodePacket cm)]
         in Message c xs'
    else error ("with_completion_packet: not async: " ++ c)

{- | Add a completion message to an existing asynchronous command.

>>> let m = n_set1 0 "0" 0
>>> let e = encodeMessage m
>>> withCM (b_close 0) m == Message "/b_close" [Int32 0,Blob e]
True
-}
withCM :: Message -> Message -> Message
withCM m cm = with_completion_packet m (Packet_Message cm)

-- * Variants to simplify common cases

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: (Integral i,Real n) => i -> i -> [n] -> Message
b_alloc_setn1 b i xs =
    let k = i + genericLength xs
        xs' = genericReplicate i 0 ++ xs
    in withCM (b_alloc b k 1) (b_setn1 b 0 xs')

-- | Get ranges of sample values.
b_getn1 :: Integral i => i -> (i,i) -> Message
b_getn1 b = b_getn b . return

-- | Variant on 'b_query'.
b_query1 :: Integral i => i -> Message
b_query1 = b_query . return

-- | Set single sample value.
b_set1 :: (Integral i,Real n) => i -> i -> n -> Message
b_set1 b i x = b_set b [(i,x)]

-- | Set a range of sample values.
b_setn1 :: (Integral i,Real n) => i -> i -> [n] -> Message
b_setn1 b i xs = b_setn b [(i,xs)]

-- | Segmented variant of 'b_setn1'.
b_setn1_segmented :: (Integral i,Real n) => i -> i -> i -> [n] -> [Message]
b_setn1_segmented k b i d =
    if genericLength d < k
    then [b_setn1 b i d]
    else b_setn1 b i (genericTake k d) : b_setn1_segmented k b (i + k) (genericDrop k d)

-- | Get ranges of sample values.
c_getn1 :: Integral i => (i,i) -> Message
c_getn1 = c_getn . return

-- | Set single bus values.
c_set1 :: (Integral i,Real n) => i -> n -> Message
c_set1 i x = c_set [(i,x)]

-- | Set single range of bus values.
c_setn1 :: (Integral i,Real n) => (i,[n]) -> Message
c_setn1 = c_setn . return

-- | Turn a single node on or off.
n_run1 :: Integral i => i -> Bool -> Message
n_run1 n k = n_run [(n,k)]

-- | Set a single node control value.
n_set1 :: (Integral i,Real n) => i -> String -> n -> Message
n_set1 n k v = n_set n [(k,v)]

-- | @s_new@ with no parameters.
s_new0 :: Integral i => String -> i -> Server.Enum.AddAction -> i -> Message
s_new0 n i a t = s_new n i a t ([]::[(String,Double)])

-- * Buffer segmentation and indices

{- | Segment a request for /m/ places into sets of at most /n/.

>>> b_segment 1024 2056
[8,1024,1024]

>>> b_segment 1 5 == replicate 5 1
True
-}
b_segment :: Integral i => i -> i -> [i]
b_segment n m =
    let (q,r) = m `quotRem` n
        s = genericReplicate q n
    in if r == 0 then s else r : s

{- | Variant of 'b_segment' that takes a starting index and returns /(index,size)/ duples.

>>> b_indices 1 5 0 == zip [0..4] (replicate 5 1)
True

>>> b_indices 1024 2056 16
[(16,8),(24,1024),(1048,1024)]
-}
b_indices :: Integral i => i -> i -> i -> [(i,i)]
b_indices n m k =
    let s = b_segment n m
        i = 0 : Common.Base.dx_d s
    in zip (map (+ k) i) s

-- * Ugen commands.

-- | Generate accumulation buffer given time-domain IR buffer and FFT size.
partConv_preparePartConv :: Integral i => i -> i -> i -> Message
partConv_preparePartConv b irb fft_size = b_gen b "PreparePartConv" (map int32 [irb, fft_size])

-- * Unpack

-- | Result is null for non-conforming data, or has five or seven elements.
unpack_n_info_datum_plain :: Num i => [Datum] -> [i]
unpack_n_info_datum_plain m =
    let to_i = fromIntegral
    in case m of
         [Int32 i1,Int32 i2,Int32 i3,Int32 i4,Int32 i5] -> [to_i i1,to_i i2,to_i i3,to_i i4,to_i i5]
         [Int32 i1,Int32 i2,Int32 i3,Int32 i4,Int32 i5,Int32 i6,Int32 i7] -> [to_i i1,to_i i2,to_i i3,to_i i4,to_i i5,to_i i6,to_i i7]
         _ -> []

unpack_n_info_plain :: Num i => Message -> [i]
unpack_n_info_plain m =
    case m of
      Message "/n_info" dat -> unpack_n_info_datum_plain dat
      _ -> []

-- | Unpack @n_info@ message.
unpack_n_info :: Num i => Message -> Maybe (i,i,i,i,i,Maybe (i,i))
unpack_n_info m =
    case unpack_n_info_plain m of
      [i1,i2,i3,i4,i5] -> Just (i1,i2,i3,i4,i5,Nothing)
      [i1,i2,i3,i4,i5,i6,i7] -> Just (i1,i2,i3,i4,i5,Just (i6,i7))
      _ -> Nothing

unpack_n_info_err :: Num i => Message -> (i,i,i,i,i,Maybe (i,i))
unpack_n_info_err = fromMaybe (error "unpack_n_info") . unpack_n_info

-- | Unpack the '/tr' messages sent by 'sendTrig'.
unpack_tr :: (Num i,Fractional f) => Message -> Maybe (i,i,f)
unpack_tr m =
    let to_i = fromIntegral
        to_f = realToFrac
    in case m of
         Message "/tr" [Int32 p,Int32 q,Float r] -> Just (to_i p,to_i q,to_f r)
         _ -> Nothing

unpack_tr_err :: (Num i,Fractional f) => Message -> (i,i,f)
unpack_tr_err = fromMaybe (error "unpack_tr") . unpack_tr

unpack_b_setn :: (Num i,Fractional f) => Message -> Maybe (i,i,i,[f])
unpack_b_setn m =
    let to_i = fromIntegral
        to_f d = case d of
                    Float n -> realToFrac n
                    _ -> error "unpack_b_setn: non-float data"
    in case m of
         Message "/b_setn" (Int32 p:Int32 q:Int32 r:z) -> Just (to_i p,to_i q,to_i r,map to_f z)
         _ -> Nothing

unpack_b_setn_err :: (Num i,Fractional f) => Message -> (i,i,i,[f])
unpack_b_setn_err = fromMaybe (error "unpack_b_setn") . unpack_b_setn

-- | Unpack b_info message, fields are (id,frames,channels,sample-rate).
unpack_b_info :: (Num i,Fractional f) => Message -> Maybe (i,i,i,f)
unpack_b_info m =
    let to_i = fromIntegral
        to_f = realToFrac
    in case m of
         Message "/b_info" [Int32 p,Int32 q,Int32 r,Float s] -> Just (to_i p,to_i q,to_i r,to_f s)
         _ -> Nothing

-- | Variant generating 'error'.
unpack_b_info_err :: (Num i,Fractional f) => Message -> (i,i,i,f)
unpack_b_info_err = fromMaybe (error "unpack_b_info") . unpack_b_info

-- Local Variables:
-- truncate-lines:t
-- End:
