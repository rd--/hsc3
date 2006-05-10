module Hsc.Graph where

import Hsc.UId
import Hsc.UGen
import Hsc.Rate
import Hsc.List (findIndex')
import Hsc.U8v

import Data.List (nub)

data Graph = Graph [UGen] [UGen] [UGen]
             deriving (Eq, Show)

data Input = Input Int Int
             deriving (Eq, Show)

implicit :: Int -> UGen
implicit n = UGen KR "Control" [] (replicate n KR) 0 (UId 0)

graph :: UGen -> Graph
graph root = Graph n c u'
  where e  = nub $ nodes root
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

mkInput :: Graph -> UGen -> Input
mkInput g u
    | isConstant u      = Input (-1) (nindex g u)
    | isControl u       = Input 0 (cindex g u)
    | isUGen u          = Input (uindex g u) 0
mkInput g (Proxy u n)   = Input (uindex g u) n
mkInput g u             = error ("illegal input: " ++ show u)

nvalue   (Constant n)    = n
cdefault (Control _ _ n) = n

input_u8v :: Input -> U8v
input_u8v (Input u p) = i16_u8v u ++ i16_u8v p

ugen_u8v :: Graph -> UGen -> U8v
ugen_u8v g c@(Control _ n _)    = pstr_u8v n ++ i16_u8v (cindex g c)
ugen_u8v g (UGen r n i o s _)   = pstr_u8v n ++
                                  i8_u8v (rateId r) ++
                                  i16_u8v (length i) ++
                                  i16_u8v (length o) ++
                                  i16_u8v s ++
                                  concatMap (input_u8v . mkInput g) i ++
                                  concatMap (i8_u8v . rateId) o

graphdef :: String -> Graph -> U8v
graphdef s g@(Graph n c u) = str_u8v "SCgf" ++
                             i32_u8v 0 ++
                             i16_u8v 1 ++
                             pstr_u8v s ++
                             i16_u8v (length n) ++
                             concatMap (f32_u8v . nvalue) n ++
                             i16_u8v (length c) ++
                             concatMap (f32_u8v . cdefault) c ++
                             i16_u8v (length c) ++
                             concatMap (ugen_u8v g) c ++
                             i16_u8v (length u) ++
                             concatMap (ugen_u8v g) u
