-- | Reader for ATS analyis data files.
module Sound.SC3.UGen.External.ATS (ATS(..)
                                   ,ATSHeader(..)
                                   ,ATSFrame,atsFrames
                                   ,atsRead) where

import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.List.Split
import Sound.OpenSoundControl.Coding.Byte

-- | ATS analysis data.
data ATS = ATS { atsHeader :: ATSHeader
               , atsData :: [Double] }
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
                           , atsFrameLength :: Int
                           } deriving (Eq, Show)

-- | ATS analysis frame data.
type ATSFrame = [Double]

bSep :: Int64 -> Int64 -> B.ByteString -> [B.ByteString]
bSep n i d =
    if i == 1
    then [d]
    else let (p,q) = B.splitAt n d
         in p : bSep n (i - 1) q

atsParse :: FilePath -> IO [Double]
atsParse fn = do
  d <- B.readFile fn
  let n = B.length d `div` 8
      v = B.take 8 d
      f = get_decoder v
  return (map f (bSep 8 n d))

-- | Read an ATS data file.
atsRead :: FilePath -> IO ATS
atsRead fn = do
  d <- atsParse fn
  let f j = d !! j
      g = floor . f
      ft = g 9
      (n, x) = ftype_n ft
      np = g 4
      nf = g 5
      fl = np * n + x
      hdr = ATSHeader (f 1) (g 2) (g 3) np nf (f 6) (f 7) (f 8) ft fl
  return (ATS hdr d)

-- | Extract set of 'ATSFrame's from 'ATS'.
atsFrames :: ATS -> [ATSFrame]
atsFrames a = chunksOf (atsFrameLength (atsHeader a)) (atsData a)

-- Determine endianess and hence decoder.
get_decoder :: B.ByteString -> B.ByteString -> Double
get_decoder v =
    if decode_f64 v == 123.0
    then decode_f64
    else decode_f64 . B.reverse

-- Calculate partial depth and frame constant.
ftype_n :: Int -> (Int, Int)
ftype_n n =
    case n of
      1 -> (2, 1)
      2 -> (3, 1)
      3 -> (2, 26)
      4 -> (3, 26)
      _ -> error "ftype_n"

{-
-- | Analysis data in format required by the sc3 ATS UGens.
atsSC3 :: ATS -> [Double]
atsSC3 (ATS h d) =
    let f = fromIntegral
        td = transpose d
    in f (atsFileType h) :
       f (atsNPartials h) :
       f (atsNFrames h) :
       f (atsWindowSize h) :
       concatMap (td !!) (atsSC3Indices h)

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
-}
