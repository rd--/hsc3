-- | Constructors for the command set implemented by the SuperCollider
--   synthesis server.
module Sound.SC3.Server.Command where

import Sound.OpenSoundControl
import Sound.SC3.Server.Utilities
import Sound.SC3.Server.Synthdef (Synthdef)

-- * Instrument definition commands

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: Synthdef -> OSC
d_recv b = message "/d_recv" [Blob b]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> OSC
d_load p = message "/d_load" [String p]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> OSC
d_loadDir p = message "/d_loadDir" [String p]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> OSC
d_free = message "/d_free" . map String

-- * Node commands

-- | Place a node after another.
n_after :: [(Int, Int)] -> OSC
n_after = message "/n_after" . mk_duples Int Int

-- | Place a node before another.
n_before :: [(Int, Int)] -> OSC
n_before = message "/n_before" . mk_duples Int Int

-- | Fill ranges of a node's control values.
n_fill :: Int -> [(String, Int, Double)] -> OSC
n_fill nid l = message "/n_fill" (Int nid : mk_triples String Int Float l)

-- | Delete a node.
n_free :: [Int] -> OSC
n_free = message "/n_free" . map Int

-- | Map a node's controls to read from a bus.
n_map :: Int -> [(String, Int)] -> OSC
n_map nid l = message "/n_map" (Int nid : mk_duples String Int l)

-- | Map a node's controls to read from buses.
n_mapn :: Int -> [(String, Int, Int)] -> OSC
n_mapn nid l = message "/n_mapn" (Int nid : mk_triples String Int Int l)

-- | Get info about a node.
n_query :: [Int] -> OSC
n_query = message "/n_query" . map Int

-- | Turn node on or off.
n_run :: [(Int, Bool)] -> OSC
n_run = message "/n_run" . mk_duples Int (Int . fromEnum)

-- | Set a node's control values.
n_set :: Int -> [(String, Double)] -> OSC
n_set nid c = message "/n_set" (Int nid : mk_duples String Float c)

-- | Set ranges of a node's control values.
n_setn :: Int -> [(String, [Double])] -> OSC
n_setn nid l = message "/n_setn" (Int nid : concatMap f l)
    where f (s,d) = String s : Int (length d) : map Float d

-- | Trace a node.
n_trace :: [Int] -> OSC
n_trace = message "/n_trace" . map Int

-- * Synthesis node commands

-- | Get control values.
s_get :: Int -> [String] -> OSC
s_get nid i = message "/s_get" (Int nid : map String i)

-- | Get ranges of control values.
s_getn :: Int -> [(String, Int)] -> OSC
s_getn nid l = message "/s_getn" (Int nid : mk_duples String Int l)

-- | Enumeration of possible locations to add new nodes (s_new and g_new).
data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

-- | Create a new synth.
s_new :: String -> Int -> AddAction -> Int -> [(String, Double)] -> OSC
s_new n i a t c = message "/s_new" (String n : Int i : Int (fromEnum a) : Int t : mk_duples String Float c)

-- | Create a new synth.
s_newargs :: String -> Int -> AddAction -> Int -> [(String, [Double])] -> OSC
s_newargs n i a t c = message "/s_newargs" (String n : Int i : Int (fromEnum a) : Int t : mk_duples_l Int String Float c)

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: [Int] -> OSC
s_noid = message "/s_noid" . map Int

-- * Group node commands

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: [Int] -> OSC
g_deepFree = message "/g_deepFree" . map Int

-- | Delete all nodes in a group.
g_freeAll :: [Int] -> OSC
g_freeAll = message "/g_freeAll" . map Int

-- | Add node to head of group.
g_head :: [(Int, Int)] -> OSC
g_head = message "/g_head" . mk_duples Int Int

-- | Create a new group.
g_new :: [(Int, AddAction, Int)] -> OSC
g_new = message "/g_new" . mk_triples Int (Int . fromEnum) Int

-- | Add node to tail of group.
g_tail :: [(Int, Int)] -> OSC
g_tail = message "/g_tail" . mk_duples Int Int

-- * Unit Generator commands

-- | Send a command to a unit generator.
u_cmd :: Int -> Int -> String -> [Datum] -> OSC
u_cmd nid uid cmd arg = message "/u_cmd" ([Int nid, Int uid, String cmd] ++ arg)

-- * Buffer commands

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Int -> Int -> Int -> OSC
b_alloc nid frames channels = message "/b_alloc" [Int nid, Int frames, Int channels]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Int -> String -> Int -> Int -> OSC
b_allocRead nid p f n = message "/b_allocRead" [Int nid, String p, Int f, Int n]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Int -> String -> Int -> Int -> [Int] -> OSC
b_allocReadChannel nid p f n cs = message "/b_allocReadChannel" ([Int nid, String p, Int f, Int n] ++ map Int cs)

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Int -> OSC
b_close nid = message "/b_close" [Int nid]

-- | Fill ranges of sample values.
b_fill :: Int -> [(Int, Int, Double)] -> OSC
b_fill nid l = message "/b_fill" (Int nid : mk_triples Int Int Float l)

-- | Free buffer data. (Asynchronous)
b_free :: Int -> OSC
b_free nid = message "/b_free" [Int nid]

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: Int -> String -> [Double] -> OSC
b_gen bid cmd arg = message "/b_gen" (Int bid : String cmd : map Float arg)

-- | Get sample values.
b_get :: Int -> [Int] -> OSC
b_get nid i = message "/b_get" (Int nid : map Int i)

-- | Get ranges of sample values.
b_getn :: Int -> [(Int, Int)] -> OSC
b_getn nid l = message "/b_getn" (Int nid : mk_duples Int Int l)

-- | Request \/b_info messages.
b_query :: [Int] -> OSC
b_query = message "/b_query" . map Int

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Int -> String -> Int -> Int -> Int -> Bool -> OSC
b_read nid p f n f' z = message "/b_read" [Int nid, String p, Int f, Int n, Int f', Int z']
    where z' = if z then 1 else 0

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: Int -> String -> Int -> Int -> Int -> Bool -> [Int] -> OSC
b_readChannel nid p f n f' z cs = message "/b_readChannel" ([Int nid, String p, Int f, Int n, Int f', Int z'] ++ map Int cs)
    where z' = if z then 1 else 0

-- | Set sample values.
b_set :: Int -> [(Int, Double)] -> OSC
b_set nid l = message "/b_set" (Int nid : mk_duples Int Float l)

-- | Set ranges of sample values.
b_setn :: Int -> [(Int, [Double])] -> OSC
b_setn nid l = message "/b_setn" (Int nid : concatMap f l)
    where f (i,d) = Int i : Int (length d) : map Float d

-- | Write sound file data. (Asynchronous)
b_write :: Int -> String -> String -> String -> Int -> Int -> Bool -> OSC
b_write nid p h t f s z = message "/b_write" [Int nid, String p, String h, String t, Int f, Int s, Int z']
    where z' = if z then 1 else 0

-- | Zero sample data. (Asynchronous)
b_zero :: Int -> OSC
b_zero nid = message "/b_zero" [Int nid]

-- * Control bus commands

-- |  Fill ranges of bus values.
c_fill :: [(Int, Int, Double)] -> OSC
c_fill = message "/c_fill" . mk_triples Int Int Float

-- | Get bus values.
c_get :: [Int] -> OSC
c_get = message "/c_get" . map Int

-- | Get ranges of bus values.
c_getn :: [(Int, Int)] -> OSC
c_getn = message "/c_getn" . mk_duples Int Int

-- | Set bus values.
c_set :: [(Int, Double)] -> OSC
c_set = message "/c_set" . mk_duples Int Float

-- | Set ranges of bus values.
c_setn :: [(Int, [Double])] -> OSC
c_setn l = message "/c_setn" (concatMap f l)
    where f (i,d) = Int i : Int (length d) : map Float d

-- * Server operation commands

-- | Remove all bundles from the scheduling queue.
clearSched :: OSC
clearSched = message "/clearSched" []

-- | Enumeration of OSC printer types.
data PrintLevel = NoPrinter
                | TextPrinter
                | HexPrinter
                | AllPrinter
                  deriving (Eq, Show, Enum)

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: PrintLevel -> OSC
dumpOSC c = message "/dumpOSC" [Int (fromEnum c)]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> OSC
notify c = message "/notify" [Int (fromEnum c)]

-- | Stop synthesis server.
quit :: OSC
quit = message "/quit" []

-- | Request \/status.reply message.
status :: OSC
status = message "/status" []

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Int -> OSC
sync sid = message "/sync" [Int sid]

-- * Variants to simplify common cases

-- | Set single sample value.
b_set1 :: Int -> Int -> Double -> OSC
b_set1 nid i x = b_set nid [(i,x)]

-- | Set a range of sample values.
b_setn1 :: Int -> Int -> [Double] -> OSC
b_setn1 nid i xs = b_setn nid [(i,xs)]

-- | Set single bus values.
c_set1 :: Int -> Double -> OSC
c_set1 i x = c_set [(i, x)]

-- | Set a signle node control value.
n_set1 :: Int -> String -> Double -> OSC
n_set1 nid k n = n_set nid [(k, n)]

-- * Modify existing message to include completion message

-- List of asynchronous server commands.
async_cmds :: [String]
async_cmds = ["/d_recv", "/d_load", "/d_loadDir"
             ,"/b_alloc", "/b_allocRead", "/b_allocReadChannel"
             ,"/b_free", "/b_close"
             ,"/b_read", "/b_readChannel"
             ,"/b_write", "/b_zero"]

-- | Add a completion message to an existing asynchronous command.
withCM :: OSC -> OSC -> OSC
withCM (Message c xs) cm =
    if c `elem` async_cmds
    then let xs' = xs ++ [Blob (encodeOSC cm)]
         in message c xs'
    else error ("withCM: not async: " ++ c)
withCM _ _ = error "withCM: not message"

-- Local Variables:
-- truncate-lines:t
-- End:
