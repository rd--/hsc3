module Hsc.OpenSoundControl where

import Hsc.List (elemIndex')
import Hsc.U8v

data Osc = OscInt Int
         | OscFloat Float
         | OscDouble Double
         | OscString String
         | OscBlob U8v
           deriving (Eq, Show)

osc_tag :: Osc -> Char
osc_tag (OscInt _)    = 'i'
osc_tag (OscFloat _)  = 'f'
osc_tag (OscDouble _) = 'd'
osc_tag (OscString _) = 's'
osc_tag (OscBlob _)   = 'b'

osc_pad' :: Int -> Int
osc_pad' n = n + 4 - (mod n 4)

osc_pad :: [a] -> a -> [a]
osc_pad s p = s ++ (replicate n p)
    where n = 4 - (mod (length s) 4)

osc_u8v :: Osc -> U8v
osc_u8v (OscInt i)    = i32_u8v i
osc_u8v (OscFloat f)  = f32_u8v f
osc_u8v (OscDouble d) = f64_u8v d
osc_u8v (OscString s) = osc_pad (str_u8v s) 0
osc_u8v (OscBlob b)   = osc_u8v (OscInt n) ++ b'
    where b' = osc_pad b 0
          n  = length b'

osc_desc :: [Osc] -> U8v
osc_desc l = osc_u8v (OscString $ ',' : (map osc_tag l))

osc :: String -> [Osc] -> U8v
osc cmd l = osc_u8v (OscString cmd) ++ osc_desc l ++ concatMap osc_u8v l

osc_sz :: Char -> U8v -> Int
osc_sz 'i' _ = 4
osc_sz 'f' _ = 4
osc_sz 'd' _ = 8
osc_sz 's' b = elemIndex' 0 b
osc_sz 'b' b = u8v_i32 (take 4 b)
osc_sz _   _ = error "illegal osc type"

osc_sz' :: Char -> U8v -> Int
osc_sz' 'i' _ = 4
osc_sz' 'f' _ = 4
osc_sz' 'd' _ = 8
osc_sz' 's' b = osc_pad' $ elemIndex' 0 b
osc_sz' 'b' b = osc_pad' $ u8v_i32 (take 4 b)
osc_sz' _   _ = error "illegal osc type"

u8v_osc :: Char -> U8v -> Osc
u8v_osc 'i' b = OscInt $ u8v_i32 b
u8v_osc 'f' b = OscFloat $ u8v_f32 b
u8v_osc 'd' b = OscDouble $ u8v_f64 b
u8v_osc 's' b = OscString $ u8v_str $ take n b where n = osc_sz 's' b
u8v_osc 'b' b = OscBlob $ take n (drop 4 b) where n = osc_sz 'b' b
u8v_osc _   _ = error "illegal osc type"

unosc' :: [Char] -> U8v -> [Osc]
unosc' []     _ = []
unosc' (c:cs) b = u8v_osc c (take n b) : unosc' cs (drop n b)
    where n = osc_sz' c b

type OscM = (String, [Osc])
type OscB = (Double, [OscM])

unosc :: U8v -> OscM
unosc b = (cmd, arg)
    where n               = osc_sz'  's' b
          (OscString cmd) = u8v_osc 's' b
          m               = osc_sz'  's' (drop n b)
          (OscString dsc) = u8v_osc 's' (drop n b)
          arg             = unosc' (drop 1 dsc) (drop (n + m) b)
