module Sound.SC3.UGen.External.LPC ( LPC(..)
                                   , LPCHeader(..)
                                   , LPCFrame
                                   , lpcRead
                                   , lpcSC3 ) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List
import Sound.OpenSoundControl
import System.IO

read_i32 :: Handle -> IO Int
read_i32 h = B.hGet h 4 >>= return . decode_i32

read_f32 :: Handle -> IO Double
read_f32 h = B.hGet h 4 >>= return . decode_f32

data LPCHeader = LPCHeader { lpcHeaderSize :: Int
                           , lpcMagic :: Int
                           , lpcNPoles :: Int
                           , lpcFrameSize :: Int
                           , lpcFrameRate :: Double
                           , lpcSampleRate :: Double
                           , lpcAnalysisDuration :: Double
                           , lpcNFrames :: Int
                           } deriving (Eq, Show)

type LPCFrame = [Double]

data LPC = LPC { lpcHeader :: LPCHeader
               , lpcFrames :: [LPCFrame] }
           deriving (Eq, Show)

-- | Read an lpanal format LPC data file.
lpcRead :: FilePath -> IO LPC
lpcRead fn = do
  h <- openFile fn ReadMode
  l <- hFileSize h
  hs <- read_i32 h
  lm <- read_i32 h
  np <- read_i32 h
  fs <- read_i32 h
  fr <- read_f32 h
  sr <- read_f32 h
  fd <- read_f32 h
  let nf = ((fromIntegral l - hs) `div` 4) `div` fs
      hdr = LPCHeader hs lm np fs fr sr fd nf
      hc = hs - (7 * 4)
      get_f = replicateM fs (read_f32 h)
  B.hGet h hc
  d <- replicateM nf get_f
  hClose h
  return (LPC hdr d)

-- | Return data in format required by the SC3 LPC UGens.
lpcSC3 :: LPC -> [Double]
lpcSC3 (LPC h d) = let f = fromIntegral
                       np = f (lpcNPoles h)
                       nf = f (lpcNFrames h)
                       fs = f (lpcFrameSize h)
                   in np : nf : fs : concat (transpose d)
