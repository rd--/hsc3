-- | Recording scsynth.
module Sound.Sc3.Server.Recorder where

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}

import Sound.Sc3.Common.Rate
import Sound.Sc3.Server.Command
import Sound.Sc3.Server.Enum
import Sound.Sc3.Server.Nrt
import Sound.Sc3.Server.Synthdef
import Sound.Sc3.Ugen.Bindings
import Sound.Sc3.Ugen.Ugen

-- | Parameters for recording scsynth.
data SC3_Recorder =
    SC3_Recorder {rec_sftype :: SoundFileFormat -- ^ Sound file format.
                 ,rec_coding :: SampleFormat -- ^ Sample format.
                 ,rec_fname :: FilePath -- ^ File name.
                 ,rec_nc :: Int -- ^ Number of channels.
                 ,rec_bus :: Int -- ^ Bus number.
                 ,rec_buf_id :: Int -- ^ ID of buffer to allocate.
                 ,rec_buf_frames :: Int -- ^ Number of frames at buffer.
                 ,rec_node_id :: Int -- ^ ID to allocate for node.
                 ,rec_group_id :: Int -- ^ Group to allocate node within.
                 ,rec_dur :: Maybe Time -- ^ Recoring duration if fixed.
                 }

-- | Default recording structure.
default_SC3_Recorder :: SC3_Recorder
default_SC3_Recorder =
    SC3_Recorder {rec_sftype = Wave
                 ,rec_coding = PcmFloat
                 ,rec_fname = "/tmp/sc3-recorder.wav"
                 ,rec_nc = 2
                 ,rec_bus = 0
                 ,rec_buf_id = 10
                 ,rec_buf_frames = 65536
                 ,rec_node_id = 2001
                 ,rec_group_id = 0
                 ,rec_dur = Just 60}

-- | The name indicates the number of channels.
rec_synthdef_nm :: Int -> String
rec_synthdef_nm nc = "sc3-recorder-" ++ show nc

-- | Generate 'Synthdef' with required number of channels.
--
-- > Sound.Sc3.Ugen.Dot.draw (rec_synthdef 2)
rec_synthdef :: Int -> Synthdef
rec_synthdef nc =
    let bufnum = control kr "bufnum" 0
        bus = control kr "bus" 0
    in synthdef (rec_synthdef_nm nc) (diskOut bufnum (in' nc ar bus))

-- | Asyncronous initialisation 'Message's ('d_recv', 'b_alloc' and 'b_write').
--
-- > withSC3 (sendBundle (bundle immediately (rec_init_m def)))
rec_init_m :: SC3_Recorder -> [Message]
rec_init_m r =
    let buf = rec_buf_id r
    in [d_recv (rec_synthdef (rec_nc r))
       ,b_alloc buf (rec_buf_frames r) (rec_nc r)
       ,b_write buf (rec_fname r) (rec_sftype r) (rec_coding r) (-1) 0 True]

-- | Begin recording 'Message' ('s_new').
--
-- > withSC3 (sendMessage (rec_begin_m def))
rec_begin_m :: SC3_Recorder -> Message
rec_begin_m r =
    s_new (rec_synthdef_nm (rec_nc r))
          (rec_node_id r)
          AddToTail
          (rec_group_id r)
          [("bus",fromIntegral (rec_bus r))
          ,("bufnum",fromIntegral (rec_buf_id r))]

-- | End recording 'Message's ('n_free', 'b_close' and 'b_free').
--
-- > withSC3 (sendBundle (bundle immediately (rec_end_m def)))
rec_end_m :: SC3_Recorder -> [Message]
rec_end_m r =
    [n_free [rec_node_id r]
    ,b_close (rec_buf_id r)
    ,b_free (rec_buf_id r)]

{- | 'Nrt' score for recorder, if 'rec_dur' is given schedule 'rec_end_m'.

> import Sound.Sc3
> withSC3 (Sound.Osc.sendMessage (dumpOSC TextPrinter))
> audition (out 0 (sinOsc ar (mce2 440 441) 0 * 0.1))
> let rc = default_SC3_Recorder {rec_dur = Just 5.0}
> nrt_audition (sc3_recorder rc)

-}
sc3_recorder :: SC3_Recorder -> Nrt
sc3_recorder r =
    let b0 = bundle 0 (rec_init_m r ++ [rec_begin_m r])
    in case rec_dur r of
         Nothing -> Nrt [b0]
         Just d -> Nrt [b0,bundle d (rec_end_m r)]
