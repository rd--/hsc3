-- | Constructors for the command set implemented by the SuperCollider
--   synthesis server.
module Sound.SC3.Server.Command where

import Data.Maybe
import Sound.OpenSoundControl {- hosc -}
import Sound.SC3.Server.Utilities
import Sound.SC3.Server.Synthdef
import Sound.SC3.Server.Synthdef.Type
import Sound.SC3.UGen.Enum

-- * Instrument definition commands

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: Synthdef -> Message
d_recv d = message "/d_recv" [Blob (synthdefData d)]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> Message
d_load p = message "/d_load" [String p]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> Message
d_loadDir p = message "/d_loadDir" [String p]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> Message
d_free = message "/d_free" . map String

-- * Node commands

-- | Place a node after another.
n_after :: [(Int, Int)] -> Message
n_after = message "/n_after" . mk_duples Int Int

-- | Place a node before another.
n_before :: [(Int, Int)] -> Message
n_before = message "/n_before" . mk_duples Int Int

-- | Fill ranges of a node's control values.
n_fill :: Int -> [(String, Int, Double)] -> Message
n_fill nid l = message "/n_fill" (Int nid : mk_triples String Int Float l)

-- | Delete a node.
n_free :: [Int] -> Message
n_free = message "/n_free" . map Int

-- | Map a node's controls to read from a bus.
n_map :: Int -> [(String, Int)] -> Message
n_map nid l = message "/n_map" (Int nid : mk_duples String Int l)

-- | Map a node's controls to read from buses.
n_mapn :: Int -> [(String, Int, Int)] -> Message
n_mapn nid l = message "/n_mapn" (Int nid : mk_triples String Int Int l)

-- | Map a node's controls to read from an audio bus.
n_mapa :: Int -> [(String, Int)] -> Message
n_mapa nid l = message "/n_mapa" (Int nid : mk_duples String Int l)

-- | Map a node's controls to read from audio buses.
n_mapan :: Int -> [(String, Int, Int)] -> Message
n_mapan nid l = message "/n_mapan" (Int nid : mk_triples String Int Int l)

-- | Get info about a node.
n_query :: [Int] -> Message
n_query = message "/n_query" . map Int

-- | Turn node on or off.
n_run :: [(Int, Bool)] -> Message
n_run = message "/n_run" . mk_duples Int (Int . fromEnum)

-- | Set a node's control values.
n_set :: Int -> [(String, Double)] -> Message
n_set nid c = message "/n_set" (Int nid : mk_duples String Float c)

-- | Set ranges of a node's control values.
n_setn :: Int -> [(String, [Double])] -> Message
n_setn nid l = message "/n_setn" (Int nid : concatMap f l)
    where f (s,d) = String s : Int (length d) : map Float d

-- | Trace a node.
n_trace :: [Int] -> Message
n_trace = message "/n_trace" . map Int

-- | Move an ordered sequence of nodes.
n_order :: AddAction -> Int -> [Int] -> Message
n_order a n ns = message "/n_order" (Int (fromEnum a) : Int n : map Int ns)

-- * Synthesis node commands

-- | Get control values.
s_get :: Int -> [String] -> Message
s_get nid i = message "/s_get" (Int nid : map String i)

-- | Get ranges of control values.
s_getn :: Int -> [(String, Int)] -> Message
s_getn nid l = message "/s_getn" (Int nid : mk_duples String Int l)

-- | Enumeration of possible locations to add new nodes (s_new and g_new).
data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

-- | Create a new synth.
s_new :: String -> Int -> AddAction -> Int -> [(String, Double)] -> Message
s_new n i a t c = message "/s_new" (String n : Int i : Int (fromEnum a) : Int t : mk_duples String Float c)

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: [Int] -> Message
s_noid = message "/s_noid" . map Int

-- * Group node commands

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: [Int] -> Message
g_deepFree = message "/g_deepFree" . map Int

-- | Delete all nodes in a group.
g_freeAll :: [Int] -> Message
g_freeAll = message "/g_freeAll" . map Int

-- | Add node to head of group.
g_head :: [(Int, Int)] -> Message
g_head = message "/g_head" . mk_duples Int Int

-- | Create a new group.
g_new :: [(Int, AddAction, Int)] -> Message
g_new = message "/g_new" . mk_triples Int (Int . fromEnum) Int

-- | Add node to tail of group.
g_tail :: [(Int, Int)] -> Message
g_tail = message "/g_tail" . mk_duples Int Int

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: [(Int, Bool)] -> Message
g_dumpTree = message "/g_dumpTree" . mk_duples Int (Int . fromEnum)

-- | Request a representation of a group's node subtree, optionally including the current control values for synths.
--
-- Replies to the sender with a @/g_queryTree.reply@ message listing all of the nodes contained within the group in the following format:
--
-- > int - if synth control values are included 1, else 0
-- > int - node ID of the requested group
-- > int - number of child nodes contained within the requested group
-- >
-- > For each node in the subtree:
-- > [
-- >   int - node ID
-- >   int - number of child nodes contained within this node. If -1 this is a synth, if >= 0 it's a group.
-- >
-- >   If this node is a synth:
-- >     symbol - the SynthDef name for this node.
-- >
-- >   If flag (see above) is true:
-- >     int - numControls for this synth (M)
-- >     [
-- >       symbol or int: control name or index
-- >       float or symbol: value or control bus mapping symbol (e.g. 'c1')
-- >     ] * M
-- > ] * the number of nodes in the subtree
--
-- N.B. The order of nodes corresponds to their execution order on the server. Thus child nodes (those contained within a group) are listed immediately following their parent.
g_queryTree :: [(Int, Bool)] -> Message
g_queryTree = message "/g_queryTree" . mk_duples Int (Int . fromEnum)

-- | Create a new parallel group (supernova specific).
p_new :: [(Int, AddAction, Int)] -> Message
p_new = message "/p_new" . mk_triples Int (Int . fromEnum) Int

-- * Plugin commands

-- | Send a plugin command.
cmd :: String -> [Datum] -> Message
cmd name = message "/cmd" . (String name :)

-- * Unit Generator commands

-- | Send a command to a unit generator.
u_cmd :: Int -> Int -> String -> [Datum] -> Message
u_cmd nid uid name arg = message "/u_cmd" ([Int nid, Int uid, String name] ++ arg)

-- * Buffer commands

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Int -> Int -> Int -> Message
b_alloc nid frames channels = message "/b_alloc" [Int nid, Int frames, Int channels]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Int -> String -> Int -> Int -> Message
b_allocRead nid p f n = message "/b_allocRead" [Int nid, String p, Int f, Int n]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Int -> String -> Int -> Int -> [Int] -> Message
b_allocReadChannel nid p f n cs = message "/b_allocReadChannel" ([Int nid, String p, Int f, Int n] ++ map Int cs)

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Int -> Message
b_close nid = message "/b_close" [Int nid]

-- | Fill ranges of sample values.
b_fill :: Int -> [(Int, Int, Double)] -> Message
b_fill nid l = message "/b_fill" (Int nid : mk_triples Int Int Float l)

-- | Free buffer data. (Asynchronous)
b_free :: Int -> Message
b_free nid = message "/b_free" [Int nid]

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: Int -> String -> [Datum] -> Message
b_gen bid name arg = message "/b_gen" (Int bid : String name : arg)

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: Int -> [B_Gen] -> [Double] -> Message
b_gen_sine1 z f n = b_gen z "sine1" (Int (b_gen_flag f) : map Float n)

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: Int -> [B_Gen] -> [(Double,Double)] -> Message
b_gen_sine2 z f n = b_gen z "sine2" (Int (b_gen_flag f) : mk_duples Float Float n)

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: Int -> [B_Gen] -> [(Double,Double,Double)] -> Message
b_gen_sine3 z f n = b_gen z "sine3" (Int (b_gen_flag f) : mk_triples Float Float Float n)

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: Int -> [B_Gen] -> [Double] -> Message
b_gen_cheby z f n = b_gen z "cheby" (Int (b_gen_flag f) : map Float n)

-- | Call @copy@ 'b_gen' command.
b_gen_copy :: Int -> Int -> Int -> Int -> Maybe Int -> Message
b_gen_copy z dst_ix src_b src_ix nf =
    let nf' = fromMaybe (-1) nf
    in b_gen z "copy" (map Int [dst_ix,src_b,src_ix,nf'])

-- | Get sample values.
b_get :: Int -> [Int] -> Message
b_get nid i = message "/b_get" (Int nid : map Int i)

-- | Get ranges of sample values.
b_getn :: Int -> [(Int, Int)] -> Message
b_getn nid l = message "/b_getn" (Int nid : mk_duples Int Int l)

-- | Request \/b_info messages.
b_query :: [Int] -> Message
b_query = message "/b_query" . map Int

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Int -> String -> Int -> Int -> Int -> Bool -> Message
b_read nid p f n f' z = message "/b_read" [Int nid, String p, Int f, Int n, Int f', Int (fromEnum z)]

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: Int -> String -> Int -> Int -> Int -> Bool -> [Int] -> Message
b_readChannel nid p f n f' z cs = message "/b_readChannel" ([Int nid, String p, Int f, Int n, Int f', Int (fromEnum z)] ++ map Int cs)

-- | Set sample values.
b_set :: Int -> [(Int, Double)] -> Message
b_set nid l = message "/b_set" (Int nid : mk_duples Int Float l)

-- | Set ranges of sample values.
b_setn :: Int -> [(Int, [Double])] -> Message
b_setn nid l = message "/b_setn" (Int nid : concatMap f l)
    where f (i,d) = Int i : Int (length d) : map Float d

-- | Write sound file data. (Asynchronous)
b_write :: Int -> String -> String -> String -> Int -> Int -> Bool -> Message
b_write nid p h t f s z = message "/b_write" [Int nid, String p, String h, String t, Int f, Int s, Int (fromEnum z)]

-- | Zero sample data. (Asynchronous)
b_zero :: Int -> Message
b_zero nid = message "/b_zero" [Int nid]

-- * Control bus commands

-- |  Fill ranges of bus values.
c_fill :: [(Int, Int, Double)] -> Message
c_fill = message "/c_fill" . mk_triples Int Int Float

-- | Get bus values.
c_get :: [Int] -> Message
c_get = message "/c_get" . map Int

-- | Get ranges of bus values.
c_getn :: [(Int, Int)] -> Message
c_getn = message "/c_getn" . mk_duples Int Int

-- | Set bus values.
c_set :: [(Int, Double)] -> Message
c_set = message "/c_set" . mk_duples Int Float

-- | Set ranges of bus values.
c_setn :: [(Int, [Double])] -> Message
c_setn l = message "/c_setn" (concatMap f l)
    where f (i,d) = Int i : Int (length d) : map Float d

-- * Server operation commands

-- | Remove all bundles from the scheduling queue.
clearSched :: Message
clearSched = message "/clearSched" []

-- | Enumeration of Message printer types.
data PrintLevel = NoPrinter
                | TextPrinter
                | HexPrinter
                | AllPrinter
                  deriving (Eq, Show, Enum)

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: PrintLevel -> Message
dumpOSC c = message "/dumpOSC" [Int (fromEnum c)]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> Message
notify c = message "/notify" [Int (fromEnum c)]

-- | Stop synthesis server.
quit :: Message
quit = message "/quit" []

-- | Request \/status.reply message.
status :: Message
status = message "/status" []

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Int -> Message
sync sid = message "/sync" [Int sid]

-- | Error posting scope.
data ErrorScope = Globally  -- ^ Global scope
                | Locally   -- ^ Bundle scope
                  deriving (Eq, Show, Enum)

-- | Error posting mode.
data ErrorMode = ErrorsOff  -- ^ Turn error posting off
               | ErrorsOn   -- ^ Turn error posting on
                 deriving (Eq, Show, Enum)

-- | Set error posting scope and mode.
errorMode :: ErrorScope -> ErrorMode -> Message
errorMode scope mode = message "/error" [Int e]
    where e = case scope of
                Globally -> fromEnum mode
                Locally  -> -1 - fromEnum mode

-- * Variants to simplify common cases

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: Int -> Int -> [Double] -> Message
b_alloc_setn1 nid i xs =
    let k = i + length xs
        xs' = replicate i 0 ++ xs
    in withCM (b_alloc nid k 1) (b_setn1 nid 0 xs')

-- | Get ranges of sample values.
b_getn1 :: Int -> (Int,Int) -> Message
b_getn1 nid = b_getn nid . return

-- | Set single sample value.
b_set1 :: Int -> Int -> Double -> Message
b_set1 nid i x = b_set nid [(i,x)]

-- | Set a range of sample values.
b_setn1 :: Int -> Int -> [Double] -> Message
b_setn1 nid i xs = b_setn nid [(i,xs)]

-- | Variant on 'b_query'.
b_query1 :: Int -> Message
b_query1 = b_query . return

-- | Set single bus values.
c_set1 :: Int -> Double -> Message
c_set1 i x = c_set [(i, x)]

-- | Set a signle node control value.
n_set1 :: Int -> String -> Double -> Message
n_set1 nid k n = n_set nid [(k, n)]

-- * Modify existing message to include completion message

-- | List of asynchronous server commands.
async_cmds :: [String]
async_cmds =
    ["/b_alloc"
    ,"/b_allocRead"
    ,"/b_allocReadChannel"
    ,"/b_close"
    ,"/b_free"
    ,"/b_read"
    ,"/b_readChannel"
    ,"/b_write"
    ,"/b_zero"
    ,"/d_load"
    ,"/d_loadDir"
    ,"/d_recv"
    ,"/notify"
    ,"/quit"
    ,"/sync"]

-- | 'True' if 'Message' is an asynchronous 'Message'.
--
-- > map isAsync [b_close 0,n_set1 0 "0" 0] == [True,False]
isAsync :: Message -> Bool
isAsync (Message a _) = a `elem` async_cmds

-- | Add a completion message to an existing asynchronous command.
--
-- > let {m = n_set1 0 "0" 0
-- >     ;m' = encodeMessage m}
-- > in withCM (b_close 0) m == Message "/b_close" [Int 0,Blob m']
withCM :: Message -> Message -> Message
withCM (Message c xs) cm =
    if c `elem` async_cmds
    then let xs' = xs ++ [Blob (encodeMessage cm)]
         in message c xs'
    else error ("withCM: not async: " ++ c)

-- * Buffer segmentation and indices

-- | Segment a request for /m/ places into sets of at most /n/.
--
-- > b_segment 1024 2056 == [8,1024,1024]
-- > b_segment 1 5 == replicate 5 1
b_segment :: Int -> Int -> [Int]
b_segment n m =
    let (q,r) = m `quotRem` n
        s = replicate q n
    in if r == 0 then s else r : s

-- | Variant of 'b_segment' that takes a starting index and returns
-- /(index,size)/ duples.
--
-- > b_indices 1 5 0 == zip [0..4] (replicate 5 1)
-- > b_indices 1024 2056 16 == [(16,8),(24,1024),(1048,1024)]
b_indices :: Int -> Int -> Int -> [(Int,Int)]
b_indices n m k =
    let dx_d = scanl1 (+)
        s = b_segment n m
        i = 0 : dx_d s
    in zip (map (+ k) i) s

-- Local Variables:
-- truncate-lines:t
-- End:
