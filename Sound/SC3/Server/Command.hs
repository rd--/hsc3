module Sound.SC3.Server.Command where

import Sound.OpenSoundControl (OSC(..), Datum(..))
import Data.Word (Word8)

-- * Instrument definition commands.

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: [Word8] -> OSC
d_recv b = Message "/d_recv" [Blob b]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> OSC
d_load p = Message "/d_load" [String p]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> OSC
d_loadDir p = Message "/d_loadDir" [String p]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> OSC
d_free n = Message "/d_free" (map String n)

-- * Node commands.

-- | Place a node after another.
n_after :: [(Int, Int)] -> OSC
n_after l = Message "/n_after" (mkDuples Int Int l)

-- | Place a node before another.
n_before :: [(Int, Int)] -> OSC
n_before l = Message "/n_before" (mkDuples Int Int l)

-- | Fill ranges of a node's control values.
n_fill :: Int -> [(String, Int, Double)] -> OSC
n_fill nid l = Message "/n_fill" (Int nid : mkTriples String Int Float l)

-- | Delete a node.
n_free :: [Int] -> OSC
n_free nid = Message "/n_free" (map Int nid)

-- | Map a node's controls to read from a bus.
n_map :: Int -> [(String, Int)] -> OSC
n_map nid l = Message "/n_map" (Int nid : mkDuples String Int l)

-- | Map a node's controls to read from buses.
n_mapn :: Int -> [(String, Int, Int)] -> OSC
n_mapn nid l = Message "/n_mapn" (Int nid : mkTriples String Int Int l)

-- | Get info about a node.
n_query :: [Int] -> OSC
n_query nid = Message "/n_query" (map Int nid)

-- | Turn node on or off.
n_run :: [(Int, Bool)] -> OSC
n_run l = Message "/n_run" (mkDuples Int (Int . fromEnum) l)

-- | Set a node's control values.
n_set :: Int -> [(String, Double)] -> OSC
n_set nid c = Message "/n_set" (Int nid : mkDuples String Float c)

-- | Set ranges of a node's control values.
n_setn :: Int -> [(String, [Double])] -> OSC
n_setn nid l = Message "/n_setn" (Int nid : concatMap f l)
    where f (s,d) = String s : Int (length d) : (map Float d)

-- | Trace a node.
n_trace :: [Int] -> OSC
n_trace nid = Message "/n_trace" (map Int nid)

-- * Synthesis node commands.

-- | Get control values.
s_get :: Int -> [String] -> OSC
s_get nid i = Message "/s_get" (Int nid : map String i)

-- | Get ranges of control values.
s_getn :: Int -> [(String, Int)] -> OSC
s_getn nid l = Message "/s_getn" (Int nid : mkDuples String Int l)

-- | Enumeration of possible locations to add new nodes (s_new and g_new).
data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

-- | Create a new synth.
s_new :: String -> Int -> AddAction -> Int -> [(String, Double)] -> OSC
s_new n i a t c = Message "/s_new" (String n : Int i : Int (fromEnum a) : Int t : mkDuples String Float c)

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: [Int] -> OSC
s_noid nid = Message "/s_noid" (map Int nid)

-- * Group node commands.

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: [Int] -> OSC
g_deepFree nid = Message "/g_deepFree" (map Int nid)

-- | Delete all nodes in a group.
g_freeAll :: [Int] -> OSC
g_freeAll nid = Message "/g_freeAll" (map Int nid)

-- | Add node to head of group.
g_head :: [(Int, Int)] -> OSC
g_head l = Message "/g_head" (mkDuples Int Int l)

-- | Create a new group.
g_new :: [(Int, AddAction, Int)] -> OSC
g_new l = Message "/g_new" (mkTriples Int (Int . fromEnum) Int l)

-- | Add node to tail of group.
g_tail :: [(Int, Int)] -> OSC
g_tail l = Message "/g_tail" (mkDuples Int Int l)

-- * Unit Generator commands.

-- | Send a command to a unit generator.
u_cmd :: Int -> Int -> String -> [Datum] -> OSC
u_cmd nid uid cmd arg = Message "/u_cmd" ([Int nid, Int uid, String cmd] ++ arg)

-- * Buffer commands.

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Int -> Int -> Int -> OSC
b_alloc nid frames channels = Message "/b_alloc" [Int nid, Int frames, Int channels]

-- | Allocate buffer space and read a sound file.
b_allocRead :: Int -> String -> Int -> Int -> OSC
b_allocRead nid p f n = Message "/b_allocRead" [Int nid, String p, Int f, Int n]

-- | Close attached soundfile and write header information.
b_close :: Int -> OSC
b_close nid = Message "/b_close" [Int nid]

-- | Fill ranges of sample values.
b_fill :: Int -> [(Int, Int, Double)] -> OSC
b_fill nid l = Message "/b_fill" (Int nid : mkTriples Int Int Float l)

-- | Free buffer data.
b_free :: Int -> OSC
b_free nid = Message "/b_free" [Int nid]

-- | Call a command to fill a buffer.
b_gen :: Int -> String -> [Double] -> OSC
b_gen bid cmd arg = Message "/b_gen" (Int bid : String cmd : map Float arg)

-- | Get sample values.
b_get :: Int -> [Int] -> OSC
b_get nid i = Message "/b_get" (Int nid : map Int i)

-- | Get ranges of sample values.
b_getn :: Int -> [(Int, Int)] -> OSC
b_getn nid l = Message "/b_getn" (Int nid : mkDuples Int Int l)

-- | Request \/b_info messages.
b_query :: [Int] -> OSC
b_query nid = Message "/b_query" (map Int nid)

-- | Read sound file data into an existing buffer.
b_read :: Int -> String -> Int -> Int -> Int -> Int -> OSC
b_read nid p f n f' z = Message "/b_read" [Int nid, String p, Int f, Int n, Int f', Int z]

-- | Set sample values.
b_set :: Int -> [(Int, Double)] -> OSC
b_set nid l = Message "/b_set" (Int nid : mkDuples Int Float l)

-- | Set ranges of sample values.
b_setn :: Int -> [(Int, [Double])] -> OSC
b_setn nid l = Message "/b_setn" (Int nid : concatMap f l)
    where f (i,d) = Int i : Int (length d) : map Float d

-- | Write sound file data.
b_write :: Int -> String -> Int -> Int -> Int -> Int -> Int -> OSC
b_write nid p h t f s z = Message "/b_write" [Int nid, String p, Int h, Int t, Int f, Int s, Int z]

-- | Zero sample data.
b_zero :: Int -> OSC
b_zero nid = Message "/b_zero" [Int nid]

-- * Control bus commands.

-- |  Fill ranges of bus values.
c_fill :: [(Int, Int, Double)] -> OSC
c_fill l = Message "/c_fill" (mkTriples Int Int Float l)

-- | Get bus values.
c_get :: [Int] -> OSC
c_get nid = Message "/c_get" (map Int nid)

-- | Get ranges of bus values.
c_getn :: [(Int, Int)] -> OSC
c_getn l = Message "/c_getn" (mkDuples Int Int l)

-- | Set bus values.
c_set :: [(Int, Double)] -> OSC
c_set l = Message "/c_set" (mkDuples Int Float l)

-- | Set ranges of bus values.
c_setn :: [(Int, [Double])] -> OSC
c_setn l = Message "/c_setn" (concatMap f l)
    where f (i,d) = Int i : Int (length d) : map Float d

-- * Server operation commands.

-- | Remove all bundles from the scheduling queue.
clearSched :: OSC
clearSched = Message "/clearSched" []

-- | Enumeration of OSC printer types.
data PrintLevel = NoPrinter
                | TextPrinter
                | HexPrinter
                | AllPrinter
                  deriving (Eq, Show, Enum)

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: PrintLevel -> OSC
dumpOSC c = Message "/dumpOSC" [Int (fromEnum c)]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> OSC
notify c = Message "/notify" [Int (fromEnum c)]

-- | Stop synthesis server.
quit :: OSC
quit = Message "/quit" []

-- | Request \/status.reply message.
status :: OSC
status = Message "/status" []

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Int -> OSC
sync sid = Message "/sync" [Int sid]

-- * Local utility functions.

mkDuples :: (a -> c) -> (b -> c) -> [(a, b)] -> [c]
mkDuples a b = concatMap (\(x,y) -> [a x, b y])

mkTriples :: (a -> d) -> (b -> d) -> (c -> d) -> [(a, b, c)] -> [d]
mkTriples a b c = concatMap (\(x,y,z) -> [a x, b y, c z])

-- Local Variables:
-- truncate-lines:t
-- End:
