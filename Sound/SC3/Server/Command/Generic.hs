-- | Generic constructors for the command set implemented by the SuperCollider synthesis server.
module Sound.SC3.Server.Command.Generic where

import Data.List {- base -}
import Data.Maybe {- base -}
import Sound.OSC.Core {- hosc -}

import Sound.SC3.Internal
import Sound.SC3.Server.Command.Enum
import Sound.SC3.Server.Enum
import qualified Sound.SC3.Server.Graphdef as G
import Sound.SC3.Server.Synthdef

-- * Buffer commands (b_)

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: (Integral i) => i -> i -> i -> Message
b_alloc nid frames channels = message "/b_alloc" [int32 nid,int32 frames,int32 channels]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: (Integral i) => i -> String -> i -> i -> Message
b_allocRead nid p f n = message "/b_allocRead" [int32 nid,string p,int32 f,int32 n]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: (Integral i) => i -> String -> i -> i -> [i] -> Message
b_allocReadChannel nid p f n cs = message "/b_allocReadChannel" ([int32 nid,string p,int32 f,int32 n] ++ map int32 cs)

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: (Integral i) => i -> Message
b_close nid = message "/b_close" [int32 nid]

-- | Fill ranges of sample values.
b_fill :: (Integral i,Real n) => i -> [(i,i,n)] -> Message
b_fill nid l = message "/b_fill" (int32 nid : mk_triples int32 int32 float l)

-- | Free buffer data. (Asynchronous)
b_free :: (Integral i) => i -> Message
b_free nid = message "/b_free" [int32 nid]

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: (Integral i) => i -> String -> [Datum] -> Message
b_gen bid name arg = message "/b_gen" (int32 bid : string name : arg)

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: (Integral i,Real n) => i -> [B_Gen] -> [n] -> Message
b_gen_sine1 z f n = b_gen z "sine1" (int32 (b_gen_flag f) : map float n)

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: (Integral i,Real n) => i -> [B_Gen] -> [(n,n)] -> Message
b_gen_sine2 z f n = b_gen z "sine2" (int32 (b_gen_flag f) : mk_duples float float n)

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: (Integral i,Real n) => i -> [B_Gen] -> [(n,n,n)] -> Message
b_gen_sine3 z f n = b_gen z "sine3" (int32 (b_gen_flag f) : mk_triples float float float n)

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: (Integral i,Real n) => i -> [B_Gen] -> [n] -> Message
b_gen_cheby z f n = b_gen z "cheby" (int32 (b_gen_flag f) : map float n)

-- | Call @copy@ 'b_gen' command.
b_gen_copy :: (Integral i) => i -> i -> i -> i -> Maybe i -> Message
b_gen_copy z dst_ix src_b src_ix nf =
    let nf' = fromMaybe (-1) nf
    in b_gen z "copy" (map int32 [dst_ix,src_b,src_ix,nf'])

-- | Get sample values.
b_get :: (Integral i) => i -> [i] -> Message
b_get nid i = message "/b_get" (int32 nid : map int32 i)

-- | Get ranges of sample values.
b_getn :: (Integral i) => i -> [(i,i)] -> Message
b_getn nid l = message "/b_getn" (int32 nid : mk_duples int32 int32 l)

-- | Request \/b_info messages.
b_query :: (Integral i) => [i] -> Message
b_query = message "/b_query" . map int32

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: (Integral i) => i -> String -> i -> i -> i -> Bool -> Message
b_read nid p f n f' z = message "/b_read" [int32 nid,string p,int32 f,int32 n,int32 f',int32 (fromEnum z)]

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: (Integral i) => i -> String -> i -> i -> i -> Bool -> [i] -> Message
b_readChannel nid p f n f' z cs = message "/b_readChannel" ([int32 nid,string p,int32 f,int32 n,int32 f',int32 (fromEnum z)] ++ map int32 cs)

-- | Set sample values.
b_set :: (Integral i,Real n) => i -> [(i,n)] -> Message
b_set nid l = message "/b_set" (int32 nid : mk_duples int32 float l)

-- | Set ranges of sample values.
b_setn :: (Integral i,Real n) => i -> [(i,[n])] -> Message
b_setn nid l =
    let f (i,d) = int32 i : int32 (length d) : map float d
    in message "/b_setn" (int32 nid : concatMap f l)

-- | Write sound file data. (Asynchronous)
b_write :: (Integral i) => i -> String -> SoundFileFormat -> SampleFormat -> i -> i -> Bool -> Message
b_write nid p h t f s z = message "/b_write" [int32 nid,string p,string (soundFileFormatString h),string (sampleFormatString t),int32 f,int32 s,int32 (fromEnum z)]

-- | Zero sample data. (Asynchronous)
b_zero :: (Integral i) => i -> Message
b_zero nid = message "/b_zero" [int32 nid]

-- * Control bus commands (c_)

-- |  Fill ranges of bus values.
c_fill :: (Integral i,Real n) => [(i,i,n)] -> Message
c_fill = message "/c_fill" . mk_triples int32 int32 float

-- | Get bus values.
c_get :: (Integral i) => [i] -> Message
c_get = message "/c_get" . map int32

-- | Get ranges of bus values.
c_getn :: (Integral i) => [(i,i)] -> Message
c_getn = message "/c_getn" . mk_duples int32 int32

-- | Set bus values.
c_set :: (Integral i,Real n) => [(i,n)] -> Message
c_set = message "/c_set" . mk_duples int32 float

-- | Set ranges of bus values.
c_setn :: (Integral i,Real n) => [(i,[n])] -> Message
c_setn l =
    let f (i,d) = int32 i : int32 (length d) : map float d
    in message "/c_setn" (concatMap f l)

-- * Instrument definition commands (d_)

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv' :: G.Graphdef -> Message
d_recv' g = message "/d_recv" [Blob (G.encode_graphdef g)]

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: Synthdef -> Message
d_recv d = message "/d_recv" [Blob (synthdefData d)]

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
g_deepFree :: (Integral i) => [i] -> Message
g_deepFree = message "/g_deepFree" . map int32

-- | Delete all nodes in a group.
g_freeAll :: (Integral i) => [i] -> Message
g_freeAll = message "/g_freeAll" . map int32

-- | Add node to head of group.
g_head :: (Integral i) => [(i,i)] -> Message
g_head = message "/g_head" . mk_duples int32 int32

-- | Create a new group.
g_new :: (Integral i) => [(i,AddAction,i)] -> Message
g_new = message "/g_new" . mk_triples int32 (int32 . fromEnum) int32

-- | Add node to tail of group.
g_tail :: (Integral i) => [(i,i)] -> Message
g_tail = message "/g_tail" . mk_duples int32 int32

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: (Integral i) => [(i,Bool)] -> Message
g_dumpTree = message "/g_dumpTree" . mk_duples int32 (int32 . fromEnum)

-- | Request a representation of a group's node subtree, optionally including the current control values for synths.
--
-- Replies to the sender with a @/g_queryTree.reply@ message listing all of the nodes contained within the group in the following format:
--
-- > int32 - if synth control values are included 1, else 0
-- > int32 - node ID of the requested group
-- > int32 - number of child nodes contained within the requested group
-- >
-- > For each node in the subtree:
-- > [
-- >   int32 - node ID
-- >   int32 - number of child nodes contained within this node. If -1 this is a synth, if >= 0 it's a group.
-- >
-- >   If this node is a synth:
-- >     symbol - the SynthDef name for this node.
-- >
-- >   If flag (see above) is true:
-- >     int32 - numControls for this synth (M)
-- >     [
-- >       symbol or int: control name or index
-- >       float or symbol: value or control bus mapping symbol (e.g. 'c1')
-- >     ] * M
-- > ] * the number of nodes in the subtree
--
-- N.B. The order of nodes corresponds to their execution order on the server. Thus child nodes (those contained within a group) are listed immediately following their parent.
g_queryTree :: (Integral i) => [(i,Bool)] -> Message
g_queryTree = message "/g_queryTree" . mk_duples int32 (int32 . fromEnum)

-- * Node commands (n_)

-- | Place a node after another.
n_after :: (Integral i) => [(i,i)] -> Message
n_after = message "/n_after" . mk_duples int32 int32

-- | Place a node before another.
n_before :: (Integral i) => [(i,i)] -> Message
n_before = message "/n_before" . mk_duples int32 int32

-- | Fill ranges of a node's control values.
n_fill :: (Integral i,Real f) => i -> [(String,i,f)] -> Message
n_fill nid l = message "/n_fill" (int32 nid : mk_triples string int32 float l)

-- | Delete a node.
n_free :: (Integral i) => [i] -> Message
n_free = message "/n_free" . map int32

n_map :: (Integral i) => i -> [(String,i)] -> Message
n_map nid l = message "/n_map" (int32 nid : mk_duples string int32 l)

-- | Map a node's controls to read from buses.
n_mapn :: (Integral i) => i -> [(String,i,i)] -> Message
n_mapn nid l = message "/n_mapn" (int32 nid : mk_triples string int32 int32 l)

-- | Map a node's controls to read from an audio bus.
n_mapa :: (Integral i) => i -> [(String,i)] -> Message
n_mapa nid l = message "/n_mapa" (int32 nid : mk_duples string int32 l)

-- | Map a node's controls to read from audio buses.
n_mapan :: (Integral i) => i -> [(String,i,i)] -> Message
n_mapan nid l = message "/n_mapan" (int32 nid : mk_triples string int32 int32 l)

-- | Get info about a node.
n_query :: (Integral i) => [i] -> Message
n_query = message "/n_query" . map int32

-- | Turn node on or off.
n_run :: (Integral i) => [(i,Bool)] -> Message
n_run = message "/n_run" . mk_duples int32 (int32 . fromEnum)

-- | Set a node's control values.
n_set :: (Integral i,Real n) => i -> [(String,n)] -> Message
n_set nid c = message "/n_set" (int32 nid : mk_duples string float c)

-- | Set ranges of a node's control values.
n_setn :: (Integral i,Real n) => i -> [(String,[n])] -> Message
n_setn nid l =
    let f (s,d) = string s : int32 (length d) : map float d
    in message "/n_setn" (int32 nid : concatMap f l)

-- | Trace a node.
n_trace :: (Integral i) => [i] -> Message
n_trace = message "/n_trace" . map int32

-- | Move an ordered sequence of nodes.
n_order :: (Integral i) => AddAction -> i -> [i] -> Message
n_order a n ns = message "/n_order" (int32 (fromEnum a) : int32 n : map int32 ns)

-- * Par commands (p_)

-- | Create a new parallel group (supernova specific).
p_new :: (Integral i) => [(i,AddAction,i)] -> Message
p_new = message "/p_new" . mk_triples int32 (int32 . fromEnum) int32

-- * Synthesis node commands (s_)

-- | Get control values.
s_get :: (Integral i) => i -> [String] -> Message
s_get nid i = message "/s_get" (int32 nid : map string i)

-- | Get ranges of control values.
s_getn :: (Integral i) => i -> [(String,i)] -> Message
s_getn nid l = message "/s_getn" (int32 nid : mk_duples string int32 l)

-- | Create a new synth.
s_new :: (Integral i,Real n) => String -> i -> AddAction -> i -> [(String,n)] -> Message
s_new n i a t c = message "/s_new" (string n : int32 i : int32 (fromEnum a) : int32 t : mk_duples string float c)

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: (Integral i) => [i] -> Message
s_noid = message "/s_noid" . map int32

-- * UGen commands (u_)

-- | Send a command to a unit generator.
u_cmd :: (Integral i) => i -> i -> String -> [Datum] -> Message
u_cmd nid uid name arg = message "/u_cmd" ([int32 nid,int32 uid,string name] ++ arg)

-- * Server operation commands

-- | Send a plugin command.
cmd :: String -> [Datum] -> Message
cmd name = message "/cmd" . (string name :)

-- | Remove all bundles from the scheduling queue.
clearSched :: Message
clearSched = message "/clearSched" []

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: PrintLevel -> Message
dumpOSC c = message "/dumpOSC" [int32 (fromEnum c)]

-- | Set error posting scope and mode.
errorMode :: ErrorScope -> ErrorMode -> Message
errorMode scope mode =
    let e = case scope of
              Globally -> fromEnum mode
              Locally  -> -1 - fromEnum mode
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
sync :: (Integral i) => i -> Message
sync sid = message "/sync" [int32 sid]

-- * Modify existing message to include completion message

-- | Add a completion message (or bundle, the name is misleading) to
-- an existing asynchronous command.
--
-- > let {m = n_set1 0 "0" 0
-- >     ;m' = encodeMessage m}
-- > in withCM (b_close 0) m == Message "/b_close" [Int 0,Blob m']
withCM :: OSC o => Message -> o -> Message
withCM (Message c xs) cm =
    if c `elem` async_cmds
    then let xs' = xs ++ [Blob (encodeOSC cm)]
         in Message c xs'
    else error ("withCM: not async: " ++ c)

-- * Variants to simplify common cases

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: (Integral i,Real n) => i -> i -> [n] -> Message
b_alloc_setn1 nid i xs =
    let k = i + genericLength xs
        xs' = genericReplicate i 0 ++ xs
    in withCM (b_alloc nid k 1) (b_setn1 nid 0 xs')

-- | Get ranges of sample values.
b_getn1 :: (Integral i) => i -> (i,i) -> Message
b_getn1 nid = b_getn nid . return

-- | Variant on 'b_query'.
b_query1 :: (Integral i) => i -> Message
b_query1 = b_query . return

-- | Set single sample value.
b_set1 :: (Integral i,Real n) => i -> i -> n -> Message
b_set1 nid i x = b_set nid [(i,x)]

-- | Set a range of sample values.
b_setn1 :: (Integral i,Real n) => i -> i -> [n] -> Message
b_setn1 nid i xs = b_setn nid [(i,xs)]

-- | Get ranges of sample values.
c_getn1 :: (Integral i) => (i,i) -> Message
c_getn1 = c_getn . return

-- | Set single bus values.
c_set1 :: (Integral i,Real n) => i -> n -> Message
c_set1 i x = c_set [(i,x)]

-- | Set single range of bus values.
c_setn1 :: (Integral i,Real n) => (i,[n]) -> Message
c_setn1 = c_setn . return

-- | Set a single node control value.
n_set1 :: (Integral i,Real n) => i -> String -> n -> Message
n_set1 nid k n = n_set nid [(k,n)]

-- | @s_new@ with no parameters.
s_new0 :: (Integral i) => String -> i -> AddAction -> i -> Message
s_new0 n i a t = s_new n i a t ([]::[(String,Double)])

-- * Buffer segmentation and indices

-- | Segment a request for /m/ places into sets of at most /n/.
--
-- > b_segment 1024 2056 == [8,1024,1024]
-- > b_segment 1 5 == replicate 5 1
b_segment :: (Integral i) => i -> i -> [i]
b_segment n m =
    let (q,r) = m `quotRem` n
        s = genericReplicate q n
    in if r == 0 then s else r : s

-- | Variant of 'b_segment' that takes a starting index and returns
-- /(index,size)/ duples.
--
-- > b_indices 1 5 0 == zip [0..4] (replicate 5 1)
-- > b_indices 1024 2056 16 == [(16,8),(24,1024),(1048,1024)]
b_indices :: (Integral i) => i -> i -> i -> [(i,i)]
b_indices n m k =
    let dx_d = scanl1 (+)
        s = b_segment n m
        i = 0 : dx_d s
    in zip (map (+ k) i) s

-- * UGen commands.

-- | Generate accumulation buffer given time-domain IR buffer and FFT size.
pc_preparePartConv :: (Integral i) => i -> i -> i -> Message
pc_preparePartConv b irb fft_size =
    b_gen b "PreparePartConv" (map int32 [irb, fft_size])

-- Local Variables:
-- truncate-lines:t
-- End:
