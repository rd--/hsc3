-- | Functions from "Sound.SC3.Server.Command.Generic" specialised to 'Int' and 'Double'.
module Sound.SC3.Server.Command.Plain where

import Sound.OSC.Core {- hosc -}

import qualified Sound.SC3.Server.Command.Generic as G
import Sound.SC3.Server.Enum
import qualified Sound.SC3.Server.Graphdef as G
import Sound.SC3.Server.Synthdef

-- * Buffer commands (b_)

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Int -> Int -> Int -> Message
b_alloc = G.b_alloc

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Int -> String -> Int -> Int -> Message
b_allocRead = G.b_allocRead

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Int -> String -> Int -> Int -> [Int] -> Message
b_allocReadChannel = G.b_allocReadChannel

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Int -> Message
b_close = G.b_close

-- | Free buffer data. (Asynchronous)
b_free :: Int -> Message
b_free = G.b_free

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: Int -> String -> [Datum] -> Message
b_gen = G.b_gen

-- | Call @copy@ 'b_gen' command.
b_gen_copy :: Int -> Int -> Int -> Int -> Maybe Int -> Message
b_gen_copy = G.b_gen_copy

-- | Get sample values.
b_get :: Int -> [Int] -> Message
b_get = G.b_get

-- | Get ranges of sample values.
b_getn :: Int -> [(Int,Int)] -> Message
b_getn = G.b_getn

b_info_unpack :: Message -> Maybe (Int,Int,Int,Double)
b_info_unpack = G.b_info_unpack

-- | Request \/b_info messages.
b_query :: [Int] -> Message
b_query = G.b_query

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Int -> String -> Int -> Int -> Int -> Bool -> Message
b_read = G.b_read

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: Int -> String -> Int -> Int -> Int -> Bool -> [Int] -> Message
b_readChannel = G.b_readChannel

-- | Write sound file data. (Asynchronous)
b_write :: Int -> String -> SoundFileFormat -> SampleFormat -> Int -> Int -> Bool -> Message
b_write = G.b_write

-- | Zero sample data. (Asynchronous)
b_zero :: Int -> Message
b_zero = G.b_zero

-- * Control bus commands

-- |  Fill ranges of bus values.
c_fill :: [(Int,Int,Double)] -> Message
c_fill = G.c_fill

-- | Get bus values.
c_get :: [Int] -> Message
c_get = G.c_get

-- | Get ranges of bus values.
c_getn :: [(Int,Int)] -> Message
c_getn = G.c_getn

-- | Set bus values.
c_set :: [(Int,Double)] -> Message
c_set = G.c_set

-- | Set ranges of bus values.
c_setn :: [(Int,[Double])] -> Message
c_setn = G.c_setn

-- * Instrument definition commands (d_)

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv' :: G.Graphdef -> Message
d_recv' = G.d_recv'

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: Synthdef -> Message
d_recv = G.d_recv

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> Message
d_load = G.d_load

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> Message
d_loadDir = G.d_loadDir

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> Message
d_free = G.d_free

-- * Group node commands (g_)

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: [Int] -> Message
g_deepFree = G.g_deepFree

-- | Delete all nodes in a group.
g_freeAll :: [Int] -> Message
g_freeAll = G.g_freeAll

-- | Add node to head of group.
g_head :: [(Int,Int)] -> Message
g_head = G.g_head

-- | Create a new group.
g_new :: [(Int,AddAction,Int)] -> Message
g_new = G.g_new

-- | Add node to tail of group.
g_tail :: [(Int,Int)] -> Message
g_tail = G.g_tail

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: [(Int,Bool)] -> Message
g_dumpTree = G.g_dumpTree

-- | Request a representation of a group's node subtree, optionally including the current control values for synths.
g_queryTree :: [(Int,Bool)] -> Message
g_queryTree = G.g_queryTree

-- * Node commands (n_)

-- | Place a node after another.
n_after :: [(Int,Int)] -> Message
n_after = G.n_after

-- | Place a node before another.
n_before :: [(Int,Int)] -> Message
n_before = G.n_before

-- | Fill ranges of a node's control values.
n_fill :: Int -> [(String,Int,Double)] -> Message
n_fill = G.n_fill

-- | Delete a node.
n_free :: [Int] -> Message
n_free = G.n_free

n_info_unpack :: Message -> Maybe [Int]
n_info_unpack = G.n_info_unpack

n_map :: Int -> [(String,Int)] -> Message
n_map = G.n_map

-- | Map a node's controls to read from buses.
n_mapn :: Int -> [(String,Int,Int)] -> Message
n_mapn = G.n_mapn

-- | Map a node's controls to read from an audio bus.
n_mapa :: Int -> [(String,Int)] -> Message
n_mapa = G.n_mapa

-- | Map a node's controls to read from audio buses.
n_mapan :: Int -> [(String,Int,Int)] -> Message
n_mapan = G.n_mapan

-- | Get info about a node.
n_query :: [Int] -> Message
n_query = G.n_query

-- | Turn node on or off.
n_run :: [(Int,Bool)] -> Message
n_run = G.n_run

-- | Set a node's control values.
n_set :: Int -> [(String,Double)] -> Message
n_set = G.n_set

-- | Set ranges of a node's control values.
n_setn :: Int -> [(String,[Double])] -> Message
n_setn = G.n_setn

-- | Trace a node.
n_trace :: [Int] -> Message
n_trace = G.n_trace

-- | Move an ordered sequence of nodes.
n_order :: AddAction -> Int -> [Int] -> Message
n_order = G.n_order

-- * Par commands (p_)

-- | Create a new parallel group (supernova specific).
p_new :: [(Int,AddAction,Int)] -> Message
p_new = G.p_new

-- * Synthesis node commands (s_)

-- | Get control values.
s_get :: Int -> [String] -> Message
s_get = G.s_get

-- | Get ranges of control values.
s_getn :: Int -> [(String,Int)] -> Message
s_getn = G.s_getn

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: [Int] -> Message
s_noid = G.s_noid

-- * Unit Generator commands (u_)

-- | Send a command to a unit generator.
u_cmd :: Int -> Int -> String -> [Datum] -> Message
u_cmd = G.u_cmd

-- * Server operation commands

-- | Send a plugin command.
cmd :: String -> [Datum] -> Message
cmd = G.cmd

-- | Remove all bundles from the scheduling queue.
clearSched :: Message
clearSched = G.clearSched

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: PrintLevel -> Message
dumpOSC = G.dumpOSC

-- | Set error posting scope and mode.
errorMode :: ErrorScope -> ErrorMode -> Message
errorMode = G.errorMode

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> Message
notify = G.notify

-- | End real time mode, close file (un-implemented).
nrt_end :: Message
nrt_end = G.nrt_end

-- | Stop synthesis server.
quit :: Message
quit = G.quit

-- | Request \/status.reply message.
status :: Message
status = G.status

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Int -> Message
sync = G.sync

-- * Variants to simplify common cases

-- | Get ranges of sample values.
b_getn1 :: Int -> (Int,Int) -> Message
b_getn1 = G.b_getn1

-- | Variant on 'b_query'.
b_query1 :: Int -> Message
b_query1 = b_query . return

-- | Get ranges of sample values.
c_getn1 :: (Int,Int) -> Message
c_getn1 = G.c_getn1

-- | Set single bus values.
c_set1 :: Int -> Double -> Message
c_set1 = G.c_set1

-- | Set single range of bus values.
c_setn1 :: (Int,[Double]) -> Message
c_setn1 = G.c_setn1

-- | Set a single node control value.
n_set1 :: Int -> String -> Double -> Message
n_set1 = G.n_set1

-- | @s_new@ with no parameters.
s_new0 :: String -> Int -> AddAction -> Int -> Message
s_new0 = G.s_new0

-- * Buffer segmentation and indices

-- | Segment a request for /m/ places into sets of at most /n/.
--
-- > b_segment 1024 2056 == [8,1024,1024]
-- > b_segment 1 5 == replicate 5 1
b_segment :: Int -> Int -> [Int]
b_segment = G.b_segment

-- | Variant of 'b_segment' that takes a starting index and returns /(index,size)/ duples.
--
-- > b_indices 1 5 0 == zip [0..4] (replicate 5 1)
-- > b_indices 1024 2056 16 == [(16,8),(24,1024),(1048,1024)]
b_indices :: Int -> Int -> Int -> [(Int,Int)]
b_indices = G.b_indices

-- | Create a new synth.
s_new :: String -> Int -> AddAction -> Int -> [(String,Double)] -> Message
s_new = G.s_new

-- | Fill ranges of sample values.
b_fill :: Int -> [(Int,Int,Double)] -> Message
b_fill = G.b_fill

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: Int -> [B_Gen] -> [Double] -> Message
b_gen_sine1 = G.b_gen_sine1

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: Int -> [B_Gen] -> [(Double,Double)] -> Message
b_gen_sine2 = G.b_gen_sine2

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: Int -> [B_Gen] -> [(Double,Double,Double)] -> Message
b_gen_sine3 = G.b_gen_sine3

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: Int -> [B_Gen] -> [Double] -> Message
b_gen_cheby = G.b_gen_cheby

-- | Set sample values.
b_set :: Int -> [(Int,Double)] -> Message
b_set = G.b_set

-- | Set ranges of sample values.
b_setn :: Int -> [(Int,[Double])] -> Message
b_setn = G.b_setn

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: Int -> Int -> [Double] -> Message
b_alloc_setn1 = G.b_alloc_setn1

-- | Set single sample value.
b_set1 :: Int -> Int -> Double -> Message
b_set1 = G.b_set1

-- | Set a range of sample values.
b_setn1 :: Int -> Int -> [Double] -> Message
b_setn1 = G.b_setn1

-- | Segmented variant of 'b_setn1'.
b_setn1_segmented :: Int -> Int -> Int -> [Double] -> [Message]
b_setn1_segmented = G.b_setn1_segmented

-- * UGen commands.

-- | Generate accumulation buffer given time-domain IR buffer and FFT size.
pc_preparePartConv :: Int -> Int -> Int -> Message
pc_preparePartConv = G.pc_preparePartConv

-- Local Variables:
-- truncate-lines:t
-- End:
