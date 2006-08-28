module Hsc.SndFile where

import Hsc.U8v

auMagic :: Int
auMagic = 0x2e736e64

data AuEncoding = AuUnspecified
                | AuMuLaw8
                | AuLinear8
                | AuLinear16
                | AuLinear24
                | AuLinear32
                | AuFloat
                | AuDouble
                  deriving (Eq, Show, Enum)

auSizeOf :: AuEncoding -> Int
auSizeOf AuUnspecified = error "auSizeOf: unspecified format"
auSizeOf AuMuLaw8      = 1
auSizeOf AuLinear8     = 1
auSizeOf AuLinear16    = 2
auSizeOf AuLinear24    = 3
auSizeOf AuLinear32    = 4
auSizeOf AuFloat       = 4
auSizeOf AuDouble      = 8

auMkHdr :: (Int, AuEncoding, Int, Int) -> [U8]
auMkHdr (nf, enc, sr, nc) = f [auMagic, 28, nb, (fromEnum enc), sr, nc, 0]
    where f  = concatMap i32_u8v
          nb = nf * nc * (auSizeOf enc)

auUnHdr :: [U8] -> (Int, AuEncoding, Int, Int)
auUnHdr u = (nf, enc, sr, nc)
    where f n = u8v_i32 (take 4 (drop n u))
          nf' = f 8
          enc = toEnum (f 12)
          sr  = f 16
          nc  = f 20
          nf  = nf' `div` (nc * auSizeOf enc)

auF32 :: (AuEncoding, Float32 -> U8v)
auF32 = (AuFloat,  f32_u8v)

auF64 :: (AuEncoding, Float64 -> U8v)
auF64 = (AuDouble, f64_u8v)

writeSndFile :: (AuEncoding, a -> [U8]) ->
   Int -> Int -> FilePath -> [a] -> IO ()
writeSndFile (enc, encdr) sr nc fn d = u8vWrite fn b
    where nf = length d `div` nc
          h  = auMkHdr (nf, enc, sr, nc)
          d' = concatMap encdr d
          b  = h ++ d'
