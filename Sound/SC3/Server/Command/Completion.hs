-- | This module provides variations of the asynchronous server commands that
-- expect a /completion packet/ as the first argument. The completion packet
-- is executed by the server when the asynchronous command has finished. Note
-- that this mechanism is for synchronizing server side processes only, for
-- client side synchronization use @\/done@ message notification or the
-- @\/sync@ barrier.
module Sound.SC3.Server.Command.Completion
  ( -- *Synthdef handling
   d_recv'
  ,d_load'
  ,d_loadDir'
  -- *Buffer allocation
  ,b_alloc'
  ,b_allocRead'
  ,b_allocReadChannel'
  ,b_free'
  ,b_close'
  -- *Buffer reading
  ,b_read'
  ,b_readChannel'
  -- *Buffer writing
  ,b_write'
  -- *Buffer operations
  ,b_zero'
  ) where

import Sound.OSC {- hosc -}
import Sound.SC3.Server.Enum
import Sound.SC3.Server.Synthdef
import Sound.SC3.Server.Synthdef.Type

-- Encode an OSC packet as an OSC blob.
encode_blob :: OSC o => o -> Datum
encode_blob = Blob . encodeOSC

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv' :: OSC o => o -> Synthdef -> Message
d_recv' osc d = message "/d_recv" [Blob (synthdefData d),encode_blob osc]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load' :: OSC o => o -> String -> Message
d_load' osc p = message "/d_load" [string p,encode_blob osc]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir' :: OSC o => o -> String -> Message
d_loadDir' osc p = message "/d_loadDir" [string p,encode_blob osc]

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc' :: OSC o => o -> Int -> Int -> Int -> Message
b_alloc' osc nid frames channels = message "/b_alloc" [int32 nid,int32 frames,int32 channels,encode_blob osc]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead' :: OSC o => o -> Int -> String -> Int -> Int -> Message
b_allocRead' osc nid p f n = message "/b_allocRead" [int32 nid,string p,int32 f,int32 n,encode_blob osc]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel' :: OSC o => o -> Int -> String -> Int -> Int -> [Int] -> Message
b_allocReadChannel' osc nid p f n cs = message "/b_allocReadChannel" ([int32 nid,string p,int32 f,int32 n] ++ map int32 cs ++ [encode_blob osc])

-- | Free buffer data. (Asynchronous)
b_free' :: OSC o => o -> Int -> Message
b_free' osc nid = message "/b_free" [int32 nid,encode_blob osc]

-- | Close attached soundfile and write header information. (Asynchronous)
b_close' :: OSC o => o -> Int -> Message
b_close' osc nid = message "/b_close" [int32 nid,encode_blob osc]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read' :: OSC o => o -> Int -> String -> Int -> Int -> Int -> Bool -> Message
b_read' osc nid p f n f' z = message "/b_read" [int32 nid,string p,int32 f,int32 n,int32 f',int32 (fromEnum z),encode_blob osc]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_readChannel' :: OSC o => o -> Int -> String -> Int -> Int -> Int -> Bool -> [Int] -> Message
b_readChannel' osc nid p f n f' z cs = message "/b_readChannel" ([int32 nid,string p,int32 f,int32 n,int32 f',int32 (fromEnum z)] ++ map int32 cs ++ [encode_blob osc])

-- | Write sound file data. (Asynchronous)
b_write' :: OSC o => o -> Int -> String -> SoundFileFormat -> SampleFormat -> Int -> Int -> Bool -> Message
b_write' osc nid p h t f s z = message "/b_write" [int32 nid,string p,string (soundFileFormatString h),string (sampleFormatString t),int32 f,int32 s,int32 (fromEnum z),encode_blob osc]

-- | Zero sample data. (Asynchronous)
b_zero' :: OSC o => o -> Int -> Message
b_zero' osc nid = message "/b_zero" [int32 nid,encode_blob osc]

-- Local Variables:
-- truncate-lines:t
-- End:
