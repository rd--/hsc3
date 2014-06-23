-- | Server input enumerations.
module Sound.SC3.Server.Enum where

-- | Enumeration of possible locations to add new nodes (s_new and g_new).
data AddAction = AddToHead
               | AddToTail
               | AddBefore
               | AddAfter
               | AddReplace
                 deriving (Eq,Show,Enum)

-- | Enumeration of flags for '/b_gen' command.
data B_Gen = Normalise | Wavetable | Clear
             deriving (Eq,Enum,Bounded,Show)

-- | 'B_Gen' to bit number.
--
-- > map b_gen_bit [minBound .. maxBound]
b_gen_bit :: B_Gen -> Int
b_gen_bit = fromEnum

-- | Set of 'B_Gen' to flag.
--
-- > b_gen_flag [minBound .. maxBound] == 7
b_gen_flag :: [B_Gen] -> Int
b_gen_flag = sum . map ((2 ^) . b_gen_bit)

-- | Error posting scope.
data ErrorScope = Globally  -- ^ Global scope
                | Locally   -- ^ Bundle scope
                  deriving (Eq,Show,Enum)

-- | Error posting mode.
data ErrorMode = ErrorsOff  -- ^ Turn error posting off
               | ErrorsOn   -- ^ Turn error posting on
                 deriving (Eq,Show,Enum)

-- | Enumeration of Message printer types.
data PrintLevel = NoPrinter
                | TextPrinter
                | HexPrinter
                | AllPrinter
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

-- | Sample format to standard file extension name.
soundFileFormatString :: SoundFileFormat -> String
soundFileFormatString f =
    case f of
      Aiff -> "aiff"
      Flac -> "flac"
      Ircam -> "ircam"
      Next -> "next"
      Raw -> "raw"
      Wave -> "wav"

-- | Infer sample format from file extension name.
soundFileFormat_from_extension :: String -> Maybe SoundFileFormat
soundFileFormat_from_extension =
    let tbl = [("aif",Aiff)
              ,("aiff",Aiff)
              ,("flac",Flac)
              ,("ircam",Ircam)
              ,("next",Next)
              ,("raw",Raw)
              ,("wav",Wave)]
    in flip lookup tbl

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
