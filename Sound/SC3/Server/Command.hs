module Sound.SC3.Server.Command where

import Sound.SC3.Server.OpenSoundControl (OSC(..), Datum(..))
import Data.Word (Word8)

data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq, Show, Enum)

data PrintLevel = NoPrinter 
                | TextPrinter
                | HexPrinter
                | AllPrinter
                  deriving (Eq, Show, Enum)

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
d_free :: String -> OSC
d_free n = Message "/d_free" [String n]

-- * Node commands.

n_before :: Int -> Int -> OSC
n_before a b = Message "/n_before" [Int a, Int b]

n_fill :: Int -> String -> Int -> Double -> OSC
n_fill nid i n v = Message "/n_fill" [Int nid, String i, Int n, Float v]

n_free :: Int -> OSC
n_free nid = Message "/n_free" [Int nid]

n_map :: Int -> String -> Int -> OSC
n_map nid i b = Message "/n_map" [Int nid, String i, Int b]

n_mapn :: Int -> String -> Int -> Int -> OSC
n_mapn nid i b n = Message "/n_mapn" [Int nid, String i, Int b, Int n]

n_query :: Int -> OSC
n_query nid = Message "/n_query" [Int nid]

n_run :: Int -> Bool -> OSC
n_run nid f = Message "/n_run" [Int nid, Int (fromEnum f)]

n_set :: Int -> String -> Double -> OSC
n_set nid i f = Message "/n_set" [Int nid, String i, Float f]

n_setn :: Int -> String -> [Double] -> OSC
n_setn nid i l = Message "/n_setn" $ [Int nid, String i, Int (length l)] ++ (map Float l)

n_trace :: Int -> OSC
n_trace nid = Message "/n_trace" [Int nid]

-- * Synthesis node commands.

s_get :: Int -> Int -> OSC
s_get nid i = Message "/s_get" [Int nid, Int i]

s_getn :: Int -> Int -> Int -> OSC
s_getn nid i n = Message "/s_getn" [Int nid, Int i, Int n]

s_new :: String -> Int -> AddAction -> Int -> OSC
s_new n i a t = Message "/s_new" [String n, Int i, Int (fromEnum a), Int t]

s_noid :: Int -> OSC
s_noid nid = Message "/s_noid" [Int nid]

-- * Group node commands.

g_deepFree :: Int -> OSC
g_deepFree nid = Message "/g_deepFree" [Int nid]

g_freeAll :: Int -> OSC
g_freeAll nid = Message "/g_freeAll" [Int nid]

g_head :: Int -> Int -> OSC
g_head g n = Message "/g_head" [Int g, Int n]

g_new :: Int -> AddAction -> Int -> OSC
g_new nid a t = Message "/g_new" [Int nid, Int (fromEnum a), Int t]

g_tail :: Int -> Int -> OSC
g_tail g n = Message "/g_tail" [Int g, Int n]

-- * Unit Generator commands.

u_cmd :: Int -> Int -> String -> [Datum] -> OSC
u_cmd nid uid cmd arg = Message "/u_cmd" ([Int nid, Int uid, String cmd] ++ arg)

-- * Buffer commands.

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Int -> Int -> Int -> OSC
b_alloc nid frames channels = Message "/b_alloc" [Int nid, Int frames, Int channels]

b_allocRead :: Int -> String -> Int -> Int -> OSC
b_allocRead nid p f n = Message "/b_allocRead" [Int nid, String p, Int f, Int n]

b_close :: Int -> OSC
b_close nid = Message "/b_close" [Int nid]

b_fill :: Int -> Int -> Int -> Double -> OSC
b_fill nid i n f = Message "/b_fill" [Int nid, Int i, Int n, Float f]

b_free :: Int -> OSC
b_free nid = Message "/b_free" [Int nid]

b_get :: Int -> Int -> OSC
b_get nid i = Message "/b_get" [Int nid, Int i]

b_getn :: Int -> Int -> Int -> OSC
b_getn nid i n = Message "/b_getn" [Int nid, Int i, Int n]

b_query :: Int -> OSC
b_query nid = Message "/b_query" [Int nid]

b_read :: Int -> String -> Int -> Int -> Int -> Int -> OSC
b_read nid p f n f' z = Message "/b_read" [Int nid, String p, Int f, Int n, Int f', Int z]

b_set :: Int -> Int -> Double -> OSC
b_set nid i f = Message "/b_set" [Int nid, Int i, Float f]

b_setn :: Int -> Int -> [Double] -> OSC
b_setn nid n l = Message "/b_setn" $ [Int nid, Int n, Int (length l)] ++ (map Float l)

b_write :: Int -> String -> Int -> Int -> Int -> Int -> Int -> OSC
b_write nid p h t f s z = Message "/b_write" [Int nid, String p, Int h, Int t, Int f, Int s, Int z]

b_zero :: Int -> OSC
b_zero nid = Message "/b_zero" [Int nid]

-- * Control bus commands.

c_fill :: Int -> Int -> Double -> OSC
c_fill nid i f = Message "/c_fill" [Int nid, Int i, Float f]

c_get :: Int -> OSC
c_get nid = Message "/c_get" [Int nid]

c_getn :: Int -> Int -> OSC
c_getn nid n = Message "/c_getn" [Int nid, Int n]

c_set :: Int -> Double -> OSC
c_set nid f = Message "/c_set" [Int nid, Float f]

c_setn :: Int -> [Double] -> OSC
c_setn nid f = Message "/c_setn" $ [Int nid, Int (length f)] ++ (map Float f)

-- * Server operation commands.

-- | Remove all bundles from the scheduling queue.
clearSched :: OSC
clearSched = Message "/clearSched" []

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

-- * Variants with variable argument support.

flatten_controls :: [(String, Double)] -> [Datum]
flatten_controls = concatMap (\(name,val) -> [String name, Float val])

n_set' :: Int -> [(String, Double)] -> OSC
n_set' nid c = Message "/n_set" (Int nid : flatten_controls c)

s_new' :: String -> Int -> AddAction -> Int -> [(String, Double)] -> OSC
s_new' n i a t c = Message "/s_new" ([String n, Int i, Int (fromEnum a), Int t] ++ flatten_controls c)

-- Local Variables:
-- truncate-lines:t
-- End:
