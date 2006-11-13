module Sound.SC3.Server.OpenSoundControl where

import Sound.SC3.Server.Schedule (utc_ntp)
import Sound.SC3.Server.U8v
import Sound.SC3.UGen.Graph (elemIndex')

data OscT = OscInt Int
          | OscFloat Double
          | OscDouble Double
          | OscString String
          | OscBlob [U8]
            deriving (Eq, Show)

data Osc  = OscM String [OscT]
          | OscB Double [Osc]
            deriving (Eq, Show)

-- | OSC types have single character identifiers.
oscTag :: OscT -> Char
oscTag (OscInt _)    = 'i'
oscTag (OscFloat _)  = 'f'
oscTag (OscDouble _) = 'd'
oscTag (OscString _) = 's'
oscTag (OscBlob _)   = 'b'

-- | The number of bytes required to align an OSC value.
oscAlign :: Int -> Int
oscAlign n | m == 0    = 0
           | otherwise = 4 - m
    where m = mod n 4

-- | Align a byte string if required.
oscExtend :: [a] -> a -> [a]
oscExtend s p | n == 0    = s
              | otherwise = s ++ replicate n p
    where n = oscAlign (length s)

-- | Encode an OSC value.
osc_u8v :: OscT -> [U8]
osc_u8v (OscInt i)    = i32_u8v i
osc_u8v (OscFloat f)  = f32_u8v (f64_f32 f)
osc_u8v (OscDouble d) = f64_u8v d
osc_u8v (OscString s) = oscExtend (cstr_u8v s) 0
osc_u8v (OscBlob b)   = i32_u8v (length b) ++ (oscExtend b) 0

osc_desc :: [OscT] -> [U8]
osc_desc l = osc_u8v (OscString $ ',' : map oscTag l)

-- | Encode an OSC packet.
osc :: Osc -> [U8]
osc (OscM c l) = osc_u8v (OscString c) ++ 
                 osc_desc l ++ 
                 concatMap osc_u8v l
osc (OscB t l) = osc_u8v (OscString "#bundle") ++ 
                 u64_u8v (utc_ntp t) ++
                 concatMap (osc_u8v . OscBlob . osc) l

-- | The plain byte count of an OSC value.
oscSize :: Char -> [U8] -> Int
oscSize 'i' _ = 4
oscSize 'f' _ = 4
oscSize 'd' _ = 8
oscSize 's' b = elemIndex' 0 b
oscSize 'b' b = u8v_i32 (take 4 b)
oscSize _   _ = error "illegal osc type"

-- | The storage byte count of an OSC value.
oscStorage :: Char -> [U8] -> Int
oscStorage 's'  b = n + oscAlign n where n = oscSize 's' b + 1
oscStorage 'b'  b = n + oscAlign n + 4 where n = oscSize 'b' b
oscStorage c    _ = oscSize c []

-- | Decode an OSC value.
u8v_osc :: Char -> [U8] -> OscT
u8v_osc 'i' b = OscInt $ u8v_i32 b
u8v_osc 'f' b = OscFloat $ f32_f64 (u8v_f32 b)
u8v_osc 'd' b = OscDouble $ u8v_f64 b
u8v_osc 's' b = OscString $ u8v_str $ take n b where n = oscSize 's' b
u8v_osc 'b' b = OscBlob $ take n (drop 4 b) where n = oscSize 'b' b
u8v_osc _   _ = error "illegal osc type"

unosc' :: [Char] -> [U8] -> [OscT]
unosc' []     _ = []
unosc' (c:cs) b = u8v_osc c tb : unosc' cs db
    where n       = oscStorage c b
          (tb,db) = splitAt n b

-- | Decode an OSC bytecode packet.
unosc :: [U8] -> Osc
unosc b = OscM cmd arg
    where n               = oscStorage 's' b
          (OscString cmd) = u8v_osc 's' b
          m               = oscStorage 's' (drop n b)
          (OscString dsc) = u8v_osc 's' (drop n b)
          arg             = unosc' (drop 1 dsc) (drop (n + m) b)

osc_show' :: OscT -> String
osc_show' (OscInt n)    = show n
osc_show' (OscFloat n)  = show n
osc_show' (OscDouble n) = show n
osc_show' (OscString s) = show s
osc_show' (OscBlob b)   = show b
