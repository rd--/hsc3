-- | Reader for LPC analysis data files.
module Sound.SC3.UGen.Bindings.HW.External.LPC ( LPC(..)
                                               , LPCHeader(..)
                                               , LPCFrame
                                               , lpcRead
                                               , lpcSC3 ) where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.List {- base -}
import System.IO {- base -}

import Sound.OSC.Coding.Byte {- hosc -}

-- | LPC analysis data.
data LPC = LPC { lpcHeader :: LPCHeader
               , lpcFrames :: [LPCFrame] }
           deriving (Eq, Show)

-- | LPC analysis meta-data.
data LPCHeader = LPCHeader { lpcHeaderSize :: Int
                           , lpcMagic :: Int
                           , lpcNPoles :: Int
                           , lpcFrameSize :: Int
                           , lpcFrameRate :: Float
                           , lpcSampleRate :: Float
                           , lpcAnalysisDuration :: Float
                           , lpcNFrames :: Int
                           } deriving (Eq, Show)

-- | LPC analysis frame data.
type LPCFrame = [Float]

-- | Read an lpanal format LPC data file.
lpcRead :: FilePath -> IO LPC
lpcRead fn = do
  h <- openFile fn ReadMode
  l <- hFileSize h
  [hs, lm, np, fs] <- replicateM 4 (read_i32 h)
  [fr, sr, fd] <- replicateM 3 (read_f32 h)
  let nf = ((fromIntegral l - hs) `div` 4) `div` fs
      hdr = LPCHeader hs lm np fs fr sr fd nf
      hc = hs - (7 * 4)
      get_f = replicateM fs (read_f32 h)
  _ <- B.hGet h hc
  d <- replicateM nf get_f
  hClose h
  return (LPC hdr d)

-- | Analysis data in format required by the sc3 LPC UGens.
lpcSC3 :: LPC -> [Float]
lpcSC3 (LPC h d) = let f = fromIntegral
                       np = f (lpcNPoles h)
                       nf = f (lpcNFrames h)
                       fs = f (lpcFrameSize h)
                   in np : nf : fs : concat (transpose d)

read_i32 :: Handle -> IO Int
read_i32 h = liftM decode_i32 (B.hGet h 4)

read_f32 :: Handle -> IO Float
read_f32 h = liftM decode_f32 (B.hGet h 4)
