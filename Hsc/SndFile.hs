module Hsc.SndFile where

import Hsc.U8v

auMagic       = 0x2e736e64
auEncLinear8  = 2
auEncLinear16 = 3
auEncLinear32 = 5
auEncFloat    = 6
auEncDouble   = 7

auSizeOf n | n == auEncLinear8  = 1
           | n == auEncLinear16 = 2
           | n == auEncLinear32 = 4
           | n == auEncFloat    = 4
           | n == auEncDouble   = 8
           | otherwise          = error "auSizeOf: illegal encoding"

auMkHdr (nf, enc, sr, nc) = f [auMagic, 28, nb, enc, sr, nc, 0]
    where f  = concatMap i32_u8v
          nb = nf * nc * (auSizeOf enc)

auUnHdr u = (nf, enc, sr, nc)
    where f n = u8v_i32 (take 4 (drop n u))
          nf' = f 8
          enc = f 12
          sr  = f 16
          nc  = f 20
          nf  = nf' `div` (nc * auSizeOf enc)

auF32 sr d = auMkHdr (length d, auEncFloat, sr, 1) ++ d'
    where d' = concatMap f32_u8v d

