-- | Server input enumerations.
module Sound.SC3.Server.Enum where

-- | Enumeration of possible locations to add new nodes (s_new and g_new).
data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq,Show,Enum)

-- | Sound file format.
data SoundFileFormat = Aiff | Flac | Ircam | Next | Raw | Wave
  deriving (Enum, Eq, Read, Show)

-- | Sample format.
data SampleFormat =
    PcmInt8 | PcmInt16 | PcmInt24 | PcmInt32
  | PcmFloat | PcmDouble
  | PcmMulaw | PcmAlaw
  deriving (Enum, Eq, Read, Show)

soundFileFormatString :: SoundFileFormat -> String
soundFileFormatString f =
    case f of
      Aiff -> "aiff"
      Flac -> "flac"
      Ircam -> "ircam"
      Next -> "next"
      Raw -> "raw"
      Wave -> "wav"

sampleFormatString :: SampleFormat -> String
sampleFormatString f =
    case f of
      PcmInt8 -> "int8"
      PcmInt16 -> "int16"
      PcmInt24 -> "int24"
      PcmInt32 -> "int32"
      PcmFloat -> "float"
      PcmDouble -> "double"
      PcmMulaw -> "mulaw"
      PcmAlaw -> "alaw"
