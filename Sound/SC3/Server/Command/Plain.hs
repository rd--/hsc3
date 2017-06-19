-- | Functions from "Sound.SC3.Server.Command.Generic" specialised to 'Int' and 'Double'.
module Sound.SC3.Server.Command.Plain where

import Sound.OSC.Core (Datum,Message) {- hosc -}

import qualified Sound.SC3.Server.Command.Generic as G
import qualified Sound.SC3.Server.Enum as E
import Sound.SC3.Server.Graphdef (Graphdef)
import Sound.SC3.Server.Synthdef (Synthdef)

-- * Types

-- | Buffer identifier (buffer number).
type Buffer_Id = Int

-- | Buffer index (frame index).
type Buffer_Ix = Int

-- | File connection flag.
type Buffer_Leave_File_Open = Bool

-- | Control bus identifier (number).
type Bus_Id = Int

-- | Node identifier (number).
type Node_Id = Int

-- | Group-node identifier (number).
type Group_Id = Int

-- | Synth-node identifier (number).
type Synth_Id = Int

-- * Buffer commands (b_)

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Buffer_Id -> Int -> Int -> Message
b_alloc = G.b_alloc

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Buffer_Id -> String -> Int -> Int -> Message
b_allocRead = G.b_allocRead

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Buffer_Id -> String -> Int -> Int -> [Int] -> Message
b_allocReadChannel = G.b_allocReadChannel

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Buffer_Id -> Message
b_close = G.b_close

-- | Fill ranges of sample values.
b_fill :: Buffer_Id -> [(Buffer_Ix,Int,Double)] -> Message
b_fill = G.b_fill

-- | Free buffer data. (Asynchronous)
b_free :: Buffer_Id -> Message
b_free = G.b_free

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: Buffer_Id -> String -> [Datum] -> Message
b_gen = G.b_gen

-- | Get sample values.
b_get :: Buffer_Id -> [Buffer_Ix] -> Message
b_get = G.b_get

-- | Get ranges of sample values.
b_getn :: Buffer_Id -> [(Buffer_Ix,Int)] -> Message
b_getn = G.b_getn

-- | Request \/b_info messages.
b_query :: [Buffer_Id] -> Message
b_query = G.b_query

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Buffer_Id -> String -> Int -> Int -> Buffer_Ix -> Buffer_Leave_File_Open -> Message
b_read = G.b_read

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: Buffer_Id -> String -> Int -> Int -> Buffer_Ix -> Buffer_Leave_File_Open -> [Int] -> Message
b_readChannel = G.b_readChannel

-- | Set sample values.
b_set :: Buffer_Id -> [(Buffer_Ix,Double)] -> Message
b_set = G.b_set

-- | Set ranges of sample values.
b_setn :: Buffer_Id -> [(Buffer_Ix,[Double])] -> Message
b_setn = G.b_setn

-- | Write sound file data. (Asynchronous)
b_write :: Buffer_Id -> String -> E.SoundFileFormat -> E.SampleFormat -> Int -> Buffer_Ix -> Buffer_Leave_File_Open -> Message
b_write = G.b_write

-- | Zero sample data. (Asynchronous)
b_zero :: Buffer_Id -> Message
b_zero = G.b_zero

-- * Control bus commands

-- |  Fill ranges of bus values.
c_fill :: [(Bus_Id,Int,Double)] -> Message
c_fill = G.c_fill

-- | Get bus values.
c_get :: [Bus_Id] -> Message
c_get = G.c_get

-- | Get ranges of bus values.
c_getn :: [(Bus_Id,Int)] -> Message
c_getn = G.c_getn

-- | Set bus values.
c_set :: [(Bus_Id,Double)] -> Message
c_set = G.c_set

-- | Set ranges of bus values.
c_setn :: [(Bus_Id,[Double])] -> Message
c_setn = G.c_setn

-- * Instrument definition commands (d_)

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv' :: Graphdef -> Message
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
g_deepFree :: [Group_Id] -> Message
g_deepFree = G.g_deepFree

-- | Delete all nodes in a set of groups.
g_freeAll :: [Group_Id] -> Message
g_freeAll = G.g_freeAll

-- | Add node to head of group.
g_head :: [(Group_Id,Node_Id)] -> Message
g_head = G.g_head

-- | Create a new group.
g_new :: [(Group_Id,E.AddAction,Node_Id)] -> Message
g_new = G.g_new

-- | Add node to tail of group.
g_tail :: [(Group_Id,Node_Id)] -> Message
g_tail = G.g_tail

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: [(Group_Id,Bool)] -> Message
g_dumpTree = G.g_dumpTree

-- | Request a representation of a group's node subtree, optionally including the current control values for synths.
g_queryTree :: [(Group_Id,Bool)] -> Message
g_queryTree = G.g_queryTree

-- * Node commands (n_)

-- | Place a node after another.
n_after :: [(Node_Id,Node_Id)] -> Message
n_after = G.n_after

-- | Place a node before another.
n_before :: [(Node_Id,Node_Id)] -> Message
n_before = G.n_before

-- | Fill ranges of a node's control values.
n_fill :: Node_Id -> [(String,Int,Double)] -> Message
n_fill = G.n_fill

-- | Delete a node.
n_free :: [Node_Id] -> Message
n_free = G.n_free

n_map :: Node_Id -> [(String,Bus_Id)] -> Message
n_map = G.n_map

-- | Map a node's controls to read from buses.
n_mapn :: Node_Id -> [(String,Bus_Id,Int)] -> Message
n_mapn = G.n_mapn

-- | Map a node's controls to read from an audio bus.
n_mapa :: Node_Id -> [(String,Bus_Id)] -> Message
n_mapa = G.n_mapa

-- | Map a node's controls to read from audio buses.
n_mapan :: Node_Id -> [(String,Bus_Id,Int)] -> Message
n_mapan = G.n_mapan

-- | Get info about a node.
n_query :: [Node_Id] -> Message
n_query = G.n_query

-- | Turn node on or off.
n_run :: [(Node_Id,Bool)] -> Message
n_run = G.n_run

-- | Set a node's control values.
n_set :: Node_Id -> [(String,Double)] -> Message
n_set = G.n_set

-- | Set ranges of a node's control values.
n_setn :: Node_Id -> [(String,[Double])] -> Message
n_setn = G.n_setn

-- | Trace a node.
n_trace :: [Node_Id] -> Message
n_trace = G.n_trace

-- | Move and order a sequence of nodes.
n_order :: E.AddAction -> Node_Id -> [Node_Id] -> Message
n_order = G.n_order

-- * Par commands (p_)

-- | Create a new parallel group (supernova specific).
p_new :: [(Group_Id,E.AddAction,Node_Id)] -> Message
p_new = G.p_new

-- * Synthesis node commands (s_)

-- | Get control values.
s_get :: Synth_Id -> [String] -> Message
s_get = G.s_get

-- | Get ranges of control values.
s_getn :: Synth_Id -> [(String,Int)] -> Message
s_getn = G.s_getn

-- | Create a new synth.
s_new :: String -> Synth_Id -> E.AddAction -> Node_Id -> [(String,Double)] -> Message
s_new = G.s_new

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: [Synth_Id] -> Message
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
dumpOSC :: E.PrintLevel -> Message
dumpOSC = G.dumpOSC

-- | Set error posting scope and mode.
errorMode :: E.ErrorScope -> E.ErrorMode -> Message
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
b_getn1 :: Buffer_Id -> (Buffer_Ix,Int) -> Message
b_getn1 = G.b_getn1

-- | Variant on 'b_query'.
b_query1 :: Buffer_Id -> Message
b_query1 = b_query . return

-- | Get ranges of sample values.
c_getn1 :: (Bus_Id,Int) -> Message
c_getn1 = G.c_getn1

-- | Set single bus values.
c_set1 :: Bus_Id -> Double -> Message
c_set1 = G.c_set1

-- | Set single range of bus values.
c_setn1 :: (Bus_Id,[Double]) -> Message
c_setn1 = G.c_setn1

-- | Turn a single node on or off.
n_run1 :: Node_Id -> Bool -> Message
n_run1 = G.n_run1

-- | Set a single node control value.
n_set1 :: Node_Id -> String -> Double -> Message
n_set1 = G.n_set1

-- | @s_new@ with no parameters.
s_new0 :: String -> Synth_Id -> E.AddAction -> Node_Id -> Message
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

-- | Call @copy@ 'b_gen' command.
b_gen_copy :: Buffer_Id -> Int -> Buffer_Id -> Int -> Maybe Int -> Message
b_gen_copy = G.b_gen_copy

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: Buffer_Id -> [E.B_Gen] -> [Double] -> Message
b_gen_sine1 = G.b_gen_sine1

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: Buffer_Id -> [E.B_Gen] -> [(Double,Double)] -> Message
b_gen_sine2 = G.b_gen_sine2

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: Buffer_Id -> [E.B_Gen] -> [(Double,Double,Double)] -> Message
b_gen_sine3 = G.b_gen_sine3

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: Buffer_Id -> [E.B_Gen] -> [Double] -> Message
b_gen_cheby = G.b_gen_cheby

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: Buffer_Id -> Buffer_Ix -> [Double] -> Message
b_alloc_setn1 = G.b_alloc_setn1

-- | Set single sample value.
b_set1 :: Buffer_Id -> Buffer_Ix -> Double -> Message
b_set1 = G.b_set1

-- | Set a range of sample values.
b_setn1 :: Buffer_Id -> Buffer_Ix -> [Double] -> Message
b_setn1 = G.b_setn1

-- | Segmented variant of 'b_setn1'.
b_setn1_segmented :: Int -> Buffer_Id -> Buffer_Ix -> [Double] -> [Message]
b_setn1_segmented = G.b_setn1_segmented

-- * UGen commands.

-- | Generate accumulation buffer given time-domain IR buffer and FFT size.
pc_preparePartConv :: Int -> Int -> Int -> Message
pc_preparePartConv = G.pc_preparePartConv

-- * Unpack

unpack_n_info_plain :: Message -> [Int]
unpack_n_info_plain = G.unpack_n_info_plain

unpack_n_info :: Message -> Maybe (Int,Int,Int,Int,Int,Maybe (Int,Int))
unpack_n_info = G.unpack_n_info

unpack_n_info_err :: Message -> (Int,Int,Int,Int,Int,Maybe (Int,Int))
unpack_n_info_err = G.unpack_n_info_err

unpack_tr :: Message -> Maybe (Int,Int,Double)
unpack_tr = G.unpack_tr

unpack_tr_err :: Message -> (Int,Int,Double)
unpack_tr_err = G.unpack_tr_err

unpack_b_setn :: Message -> Maybe (Int,Int,Int,[Double])
unpack_b_setn = G.unpack_b_setn

unpack_b_setn_err :: Message -> (Int,Int,Int,[Double])
unpack_b_setn_err = G.unpack_b_setn_err

unpack_b_info :: Message -> Maybe (Int,Int,Int,Double)
unpack_b_info = G.unpack_b_info

unpack_b_info_err :: Message -> (Int,Int,Int,Double)
unpack_b_info_err = G.unpack_b_info_err

-- Local Variables:
-- truncate-lines:t
-- End:
