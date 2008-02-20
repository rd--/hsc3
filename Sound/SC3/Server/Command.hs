module Sound.SC3.Server.Command where

import Sound.OpenSoundControl
import Sound.SC3.Server.Utilities
import Data.Word (Word8)

-- * Instrument definition commands.

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: [Word8] -> OSC
d_recv b = message "/d_recv" [blob b]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> OSC
d_load p = message "/d_load" [string p]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> OSC
d_loadDir p = message "/d_loadDir" [string p]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> OSC
d_free n = message "/d_free" (map string n)

-- * Node commands.

-- | Place a node after another.
n_after :: [(Int, Int)] -> OSC
n_after l = message "/n_after" (mkDuples int int l)

-- | Place a node before another.
n_before :: [(Int, Int)] -> OSC
n_before l = message "/n_before" (mkDuples int int l)

-- | Fill ranges of a node's control values.
n_fill :: Int -> [(String, Int, Double)] -> OSC
n_fill nid l = message "/n_fill" (int nid : mkTriples string int float l)

-- | Delete a node.
n_free :: [Int] -> OSC
n_free nid = message "/n_free" (map int nid)

-- | Map a node's controls to read from a bus.
n_map :: Int -> [(String, Int)] -> OSC
n_map nid l = message "/n_map" (int nid : mkDuples string int l)

-- | Map a node's controls to read from buses.
n_mapn :: Int -> [(String, Int, Int)] -> OSC
n_mapn nid l = message "/n_mapn" (int nid : mkTriples string int int l)

-- | Get info about a node.
n_query :: [Int] -> OSC
n_query nid = message "/n_query" (map int nid)

-- | Turn node on or off.
n_run :: [(Int, Bool)] -> OSC
n_run l = message "/n_run" (mkDuples int (int . fromEnum) l)

-- | Set a node's control values.
n_set :: Int -> [(String, Double)] -> OSC
n_set nid c = message "/n_set" (int nid : mkDuples string float c)

-- | Set ranges of a node's control values.
n_setn :: Int -> [(String, [Double])] -> OSC
n_setn nid l = message "/n_setn" (int nid : concatMap f l)
    where f (s,d) = string s : int (length d) : (map float d)

-- | Trace a node.
n_trace :: [Int] -> OSC
n_trace nid = message "/n_trace" (map int nid)

-- * Synthesis node commands.

-- | Get control values.
s_get :: Int -> [String] -> OSC
s_get nid i = message "/s_get" (int nid : map string i)

-- | Get ranges of control values.
s_getn :: Int -> [(String, Int)] -> OSC
s_getn nid l = message "/s_getn" (int nid : mkDuples string int l)

-- | Enumeration of possible locations to add new nodes (s_new and g_new).
data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

-- | Create a new synth.
s_new :: String -> Int -> AddAction -> Int -> [(String, Double)] -> OSC
s_new n i a t c = message "/s_new" (string n : int i : int (fromEnum a) : int t : mkDuples string float c)

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: [Int] -> OSC
s_noid nid = message "/s_noid" (map int nid)

-- * Group node commands.

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: [Int] -> OSC
g_deepFree nid = message "/g_deepFree" (map int nid)

-- | Delete all nodes in a group.
g_freeAll :: [Int] -> OSC
g_freeAll nid = message "/g_freeAll" (map int nid)

-- | Add node to head of group.
g_head :: [(Int, Int)] -> OSC
g_head l = message "/g_head" (mkDuples int int l)

-- | Create a new group.
g_new :: [(Int, AddAction, Int)] -> OSC
g_new l = message "/g_new" (mkTriples int (int . fromEnum) int l)

-- | Add node to tail of group.
g_tail :: [(Int, Int)] -> OSC
g_tail l = message "/g_tail" (mkDuples int int l)

-- * Unit Generator commands.

-- | Send a command to a unit generator.
u_cmd :: Int -> Int -> String -> [Datum] -> OSC
u_cmd nid uid cmd arg = message "/u_cmd" ([int nid, int uid, string cmd] ++ arg)

-- * Buffer commands.

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Int -> Int -> Int -> OSC
b_alloc nid frames channels = message "/b_alloc" [int nid, int frames, int channels]

-- | Allocate buffer space and read a sound file.
b_allocRead :: Int -> String -> Int -> Int -> OSC
b_allocRead nid p f n = message "/b_allocRead" [int nid, string p, int f, int n]

-- | Close attached soundfile and write header information.
b_close :: Int -> OSC
b_close nid = message "/b_close" [int nid]

-- | Fill ranges of sample values.
b_fill :: Int -> [(Int, Int, Double)] -> OSC
b_fill nid l = message "/b_fill" (int nid : mkTriples int int float l)

-- | Free buffer data.
b_free :: Int -> OSC
b_free nid = message "/b_free" [int nid]

-- | Call a command to fill a buffer.
b_gen :: Int -> String -> [Double] -> OSC
b_gen bid cmd arg = message "/b_gen" (int bid : string cmd : map float arg)

-- | Get sample values.
b_get :: Int -> [Int] -> OSC
b_get nid i = message "/b_get" (int nid : map int i)

-- | Get ranges of sample values.
b_getn :: Int -> [(Int, Int)] -> OSC
b_getn nid l = message "/b_getn" (int nid : mkDuples int int l)

-- | Request \/b_info messages.
b_query :: [Int] -> OSC
b_query nid = message "/b_query" (map int nid)

-- | Read sound file data into an existing buffer.
b_read :: Int -> String -> Int -> Int -> Int -> Int -> OSC
b_read nid p f n f' z = message "/b_read" [int nid, string p, int f, int n, int f', int z]

-- | Set sample values.
b_set :: Int -> [(Int, Double)] -> OSC
b_set nid l = message "/b_set" (int nid : mkDuples int float l)

-- | Set ranges of sample values.
b_setn :: Int -> [(Int, [Double])] -> OSC
b_setn nid l = message "/b_setn" (int nid : concatMap f l)
    where f (i,d) = int i : int (length d) : map float d

-- | Write sound file data.
b_write :: Int -> String -> Int -> Int -> Int -> Int -> Int -> OSC
b_write nid p h t f s z = message "/b_write" [int nid, string p, int h, int t, int f, int s, int z]

-- | Zero sample data.
b_zero :: Int -> OSC
b_zero nid = message "/b_zero" [int nid]

-- * Control bus commands.

-- |  Fill ranges of bus values.
c_fill :: [(Int, Int, Double)] -> OSC
c_fill l = message "/c_fill" (mkTriples int int float l)

-- | Get bus values.
c_get :: [Int] -> OSC
c_get nid = message "/c_get" (map int nid)

-- | Get ranges of bus values.
c_getn :: [(Int, Int)] -> OSC
c_getn l = message "/c_getn" (mkDuples int int l)

-- | Set bus values.
c_set :: [(Int, Double)] -> OSC
c_set l = message "/c_set" (mkDuples int float l)

-- | Set ranges of bus values.
c_setn :: [(Int, [Double])] -> OSC
c_setn l = message "/c_setn" (concatMap f l)
    where f (i,d) = int i : int (length d) : map float d

-- * Server operation commands.

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
dumpOSC c = message "/dumpOSC" [int (fromEnum c)]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> OSC
notify c = message "/notify" [int (fromEnum c)]

-- | Stop synthesis server.
quit :: OSC
quit = message "/quit" []

-- | Request \/status.reply message.
status :: OSC
status = message "/status" []

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Int -> OSC
sync sid = message "/sync" [int sid]

-- * Variants to simplify common cases.

-- | Set single sample value.
b_set1 :: Int -> Int -> Double -> OSC
b_set1 nid i x = b_set nid [(i,x)]

-- | Set a range of sample values.
b_setn1 :: Int -> Int -> [Double] -> OSC
b_setn1 nid i xs = b_setn nid [(i,xs)]

-- Local Variables:
-- truncate-lines:t
-- End:
