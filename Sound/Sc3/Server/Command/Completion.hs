-- | This module provides variations of the asynchronous server commands that
-- expect a /completion packet/ as the first argument. The completion packet
-- is executed by the server when the asynchronous command has finished. Note
-- that this mechanism is for synchronizing server side processes only, for
-- client side synchronization use @\/done@ message notification or the
-- @\/sync@ barrier.
module Sound.Sc3.Server.Command.Completion where

import Sound.Osc.Core {- hosc -}

import Sound.Sc3.Server.Enum
import Sound.Sc3.Server.Synthdef

-- | Encode an Osc packet as an Osc blob.
encode_blob :: Packet -> Datum
encode_blob = Blob . encodePacket

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: Packet -> Synthdef -> Message
d_recv pkt d = Message "/d_recv" [Blob (synthdefData d),encode_blob pkt]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: Packet -> String -> Message
d_load pkt p = Message "/d_load" [string p,encode_blob pkt]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: Packet -> String -> Message
d_loadDir pkt p = Message "/d_loadDir" [string p,encode_blob pkt]

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Packet -> Int -> Int -> Int -> Message
b_alloc pkt nid frames channels = Message "/b_alloc" [int32 nid,int32 frames,int32 channels,encode_blob pkt]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Packet -> Int -> String -> Int -> Int -> Message
b_allocRead pkt nid p f n = Message "/b_allocRead" [int32 nid,string p,int32 f,int32 n,encode_blob pkt]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Packet -> Int -> String -> Int -> Int -> [Int] -> Message
b_allocReadChannel pkt nid p f n cs = Message "/b_allocReadChannel" ([int32 nid,string p,int32 f,int32 n] ++ map int32 cs ++ [encode_blob pkt])

-- | Free buffer data. (Asynchronous)
b_free :: Packet -> Int -> Message
b_free pkt nid = Message "/b_free" [int32 nid,encode_blob pkt]

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Packet -> Int -> Message
b_close pkt nid = Message "/b_close" [int32 nid,encode_blob pkt]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Packet -> Int -> String -> Int -> Int -> Int -> Bool -> Message
b_read pkt nid p f n f' z = Message "/b_read" [int32 nid,string p,int32 f,int32 n,int32 f',int32 (fromEnum z),encode_blob pkt]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_readChannel :: Packet -> Int -> String -> Int -> Int -> Int -> Bool -> [Int] -> Message
b_readChannel pkt nid p f n f' z cs = Message "/b_readChannel" ([int32 nid,string p,int32 f,int32 n,int32 f',int32 (fromEnum z)] ++ map int32 cs ++ [encode_blob pkt])

-- | Write sound file data. (Asynchronous)
b_write :: Packet -> Int -> String -> SoundFileFormat -> SampleFormat -> Int -> Int -> Bool -> Message
b_write pkt nid p h t f s z = Message "/b_write" [int32 nid,string p,string (soundFileFormatString h),string (sampleFormatString t),int32 f,int32 s,int32 (fromEnum z),encode_blob pkt]

-- | Zero sample data. (Asynchronous)
b_zero :: Packet -> Int -> Message
b_zero pkt nid = Message "/b_zero" [int32 nid,encode_blob pkt]

-- Local Variables:
-- truncate-lines:t
-- End:
