-- | This module provides variations of the asynchronous server commands that
-- expect a /completion packet/ as the first argument. The completion packet
-- is executed by the server when the asynchronous command has finished. Note
-- that this mechanism is for synchronizing server side processes only, for
-- client side synchronization use @\/done@ message notification or the
-- @\/sync@ barrier.
module Sound.SC3.Server.Command.Completion
  ( -- *Synthdef handling
    d_recv'
  , d_load'
  , d_loadDir'
  -- *Buffer allocation
  , b_alloc'
  , b_allocRead'
  , b_allocReadChannel'
  , b_free'
  , b_close'
  -- *Buffer reading
  , b_read'
  , b_readChannel'
  -- *Buffer writing
  , b_write'
  -- *Buffer operations
  , b_zero'
  ) where

import           Sound.OpenSoundControl.OSC.Encoding
import           Sound.OpenSoundControl.OSC.Type
import           Sound.SC3.Server.Synthdef (Synthdef)

-- Encode an OSC packet as an OSC blob.
encode_osc_blob :: OSC -> Datum
encode_osc_blob = Blob . encodeOSC

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv' :: OSC -> Synthdef -> OSC
d_recv' osc b = message "/d_recv" [Blob b, encode_osc_blob osc]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load' :: OSC -> String -> OSC
d_load' osc p = message "/d_load" [String p, encode_osc_blob osc]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir' :: OSC -> String -> OSC
d_loadDir' osc p = message "/d_loadDir" [String p, encode_osc_blob osc]

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc' :: OSC -> Int -> Int -> Int -> OSC
b_alloc' osc nid frames channels = message "/b_alloc" [Int nid, Int frames, Int channels, encode_osc_blob osc]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead' :: OSC -> Int -> String -> Int -> Int -> OSC
b_allocRead' osc nid p f n = message "/b_allocRead" [Int nid, String p, Int f, Int n, encode_osc_blob osc]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel' :: OSC -> Int -> String -> Int -> Int -> [Int] -> OSC
b_allocReadChannel' osc nid p f n cs = message "/b_allocReadChannel" ([Int nid, String p, Int f, Int n] ++ map Int cs ++ [encode_osc_blob osc])

-- | Free buffer data. (Asynchronous)
b_free' :: OSC -> Int -> OSC
b_free' osc nid = message "/b_free" [Int nid, encode_osc_blob osc]

-- | Close attached soundfile and write header information. (Asynchronous)
b_close' :: OSC -> Int -> OSC
b_close' osc nid = message "/b_close" [Int nid, encode_osc_blob osc]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read' :: OSC -> Int -> String -> Int -> Int -> Int -> Bool -> OSC
b_read' osc nid p f n f' z = message "/b_read" [Int nid, String p, Int f, Int n, Int f', Int (fromEnum z), encode_osc_blob osc]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_readChannel' :: OSC -> Int -> String -> Int -> Int -> Int -> Bool -> [Int] -> OSC
b_readChannel' osc nid p f n f' z cs = message "/b_readChannel" ([Int nid, String p, Int f, Int n, Int f', Int (fromEnum z)] ++ map Int cs ++ [encode_osc_blob osc])

-- | Write sound file data. (Asynchronous)
b_write' :: OSC -> Int -> String -> String -> String -> Int -> Int -> Bool -> OSC
b_write' osc nid p h t f s z = message "/b_write" [Int nid, String p, String h, String t, Int f, Int s, Int (fromEnum z), encode_osc_blob osc]

-- | Zero sample data. (Asynchronous)
b_zero' :: OSC -> Int -> OSC
b_zero' osc nid = message "/b_zero" [Int nid, encode_osc_blob osc]

-- Local Variables:
-- truncate-lines:t
-- End:
