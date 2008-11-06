-- | Reader for ATS analyis data files.
module Sound.SC3.UGen.External.ATS ( ATS(..)
                                   , ATSHeader(..)
                                   , ATSFrame
                                   , atsRead
                                   , atsSC3 ) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List
import Sound.OpenSoundControl
import System.IO

-- | ATS analysis data.
data ATS = ATS { atsHeader :: ATSHeader
               , atsFrames :: [ATSFrame] }
           deriving (Eq, Show)

-- | ATS analysis meta-data.
data ATSHeader = ATSHeader { atsSampleRate :: Double
                           , atsFrameSize :: Int
                           , atsWindowSize :: Int
                           , atsNPartials :: Int
                           , atsNFrames :: Int
                           , atsMaxAmplitude :: Double
                           , atsMaxFrequency :: Double
                           , atsAnalysisDuration :: Double
                           , atsFileType :: Int
                           } deriving (Eq, Show)

-- | ATS analysis frame data. 
type ATSFrame = [Double]

-- | Read an ATS data file.
atsRead :: FilePath -> IO ATS
atsRead fn = do
  h <- openFile fn ReadMode
  v <- B.hGet h 8
  let reader = get_reader v
  hdr_r <- replicateM 9 (reader h)
  let f j = hdr_r !! (j - 1)
      g = floor . f
      ft = g 9
      (n, x) = ftype_n ft
      np = g 4
      nf = g 5
      fl = np * n + x
      hdr = ATSHeader (f 1) (g 2) (g 3) np nf (f 6) (f 7) (f 8) ft
      get_f = replicateM fl (reader h)
  d <- replicateM nf get_f
  hClose h
  return (ATS hdr d)

-- | Analysis data in format required by the sc3 ATS UGens.
atsSC3 :: ATS -> [Double]
atsSC3 (ATS h d) = 
    let f = fromIntegral
        td = transpose d
    in ( f (atsFileType h) :
         f (atsNPartials h) :
         f (atsNFrames h) :
         f (atsWindowSize h) :
         concatMap (td !!) (atsSC3Indices h) )

-- be
read_f64 :: Handle -> IO Double
read_f64 h = B.hGet h 8 >>= return . decode_f64

-- le
read_f64LE :: Handle -> IO Double
read_f64LE h = B.hGet h 8 >>= return . decode_f64 . B.reverse

-- Determine endianess and hence reader.
get_reader :: B.ByteString -> (Handle -> IO Double)
get_reader v = if decode_f64 v == 123.0
               then read_f64
               else read_f64LE

-- Calculate partial depth and frame constant.
ftype_n :: Int -> (Int, Int)
ftype_n 1 = (2, 1)
ftype_n 2 = (3, 1)
ftype_n 3 = (2, 26)
ftype_n 4 = (3, 26)
ftype_n _ = undefined

-- Indices for track data in the order required by sc3.
atsSC3Indices :: ATSHeader -> [Int]
atsSC3Indices h = 
    let np = atsNPartials h
        o = 3 * (np - 1)
        a = [1,4 .. (1 + o)]
        f = map (+ 1) a
        p = map (+ 1) f
        n = map (+ (4+o)) [0..24]
    in if atsFileType h == 4
       then a ++ f ++ p ++ n
       else error "atsSC3Indices: illegal ATS file type (/= 4)"
