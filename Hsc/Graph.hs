module Hsc.Graph where

import Hsc.UGen
import Hsc.List (uniq, findIndex')
import Hsc.U8v

data Graph = Graph [UGen] [UGen] [UGen] 
             deriving (Eq, Show)

data Input = Input Int Int 
             deriving (Eq, Show)

implicit :: Int -> UGen
implicit n = UGen KR "Control" [] (replicate n KR) 0

graph :: UGen -> Graph
graph root = Graph n c u'
  where e  = uniq $ nodes root
        n  = filter isConstant e
        c  = filter isControl e
        u  = reverse $ filter isUGen e
        u' = if null c then u else implicit (length c) : u

index' :: (Eq a) => [a] -> a -> Int
index' c x = findIndex' (== x) c

uindex :: Graph -> UGen -> Int
uindex (Graph _ _ u) x = index' u x

cindex (Graph _ c _) x = index' c x
nindex (Graph n _ _) x = index' n x

mkinput :: Graph -> UGen -> Input
mkinput g u 
    | isConstant u      = Input (-1) (nindex g u)
    | isControl u       = Input 0 (cindex g u)
    | isUGen u          = Input (uindex g u) 0
mkinput g (Proxy u n) = Input (uindex g u) n

nvalue   (Constant n)    = n
cdefault (Control _ _ n) = n

input_u8v :: Input -> U8v
input_u8v (Input u p) = i16_u8v u ++ i16_u8v p

ugen_u8v :: Graph -> UGen -> U8v
ugen_u8v g c@(Control r n v) = pstr_u8v n ++ i16_u8v (cindex g c)
ugen_u8v g (UGen r n i o s)  = pstr_u8v n ++
                               i8_u8v (rateId r) ++
                               i16_u8v (length i) ++
                               i16_u8v (length o) ++
                               i16_u8v s ++ 
                               (concat $ map (input_u8v . (mkinput g)) i) ++
                               (concat $ map (i8_u8v . rateId) o)

graphdef :: String -> Graph -> U8v
graphdef s g@(Graph n c u) = str_u8v "SCgf" ++
                             i32_u8v 0 ++
                             i16_u8v 1 ++
                             pstr_u8v s ++
                             i16_u8v (length n) ++
                             (concat $ map (f32_u8v . nvalue) n) ++
                             i16_u8v (length c) ++
                             (concat $ map (f32_u8v . cdefault) c) ++
                             i16_u8v (length c) ++
                             (concat $ map (ugen_u8v g) c) ++
                             i16_u8v (length u) ++
                             (concat $ map (ugen_u8v g) u)


