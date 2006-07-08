module Hsc.Graph where

import Hsc.UGen (UGen(..), UId(UId), nodes, isConstant, isControl, isUGen)
import Hsc.Rate (Rate(KR), rateId)
import Hsc.List (findIndex', nub')
import Hsc.U8v

data Graph = Graph [UGen] [UGen] [UGen]
             deriving (Eq, Show)

data Input = Input Int Int
             deriving (Eq, Show)

implicit :: Int -> UGen
implicit n = UGen KR "Control" [] (replicate n KR) 0 (UId 0)

graph :: UGen -> Graph
graph root = Graph n c u'
  where e  = nub' $ nodes root
        n  = filter isConstant e
        c  = filter isControl e
        u  = filter isUGen e
        u' = if null c then u else implicit (length c) : u

index' :: (Eq a) => [a] -> a -> Int
index' c x = findIndex' (== x) c

uindex :: Graph -> UGen -> Int
uindex (Graph _ _ u) x = index' u x

cindex (Graph _ c _) x = index' c x
nindex (Graph n _ _) x = index' n x

mkInput :: Graph -> UGen -> Input
mkInput g u@(UGen _ _ _ _ _ _) = Input (uindex g u) 0
mkInput g u@(Constant _)       = Input (-1) (nindex g u)
mkInput g u@(Control _ _ _)    = Input 0 (cindex g u)
mkInput g (Proxy u n)          = Input (uindex g u) n
mkInput g u                    = error ("mkInput: illegal input: " ++ show (g,u))

nvalue   (Constant n)    = n
nvalue   _               = error "nvalue: non constant input"

cdefault (Control _ _ n) = n
cdefault  _              = error "cdefault: non control input"

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
ugen_u8v _ _                    = error "illegal input"

graphdef :: String -> Graph -> U8v
graphdef s g@(Graph n c u) = str_u8v "SCgf" ++
                             i32_u8v 0 ++
                             i16_u8v 1 ++
                             pstr_u8v s ++
                             i16_u8v (length n) ++
                             concatMap (f32_u8v . f64_f32 . nvalue) n ++
                             i16_u8v (length c) ++
                             concatMap (f32_u8v . f64_f32 . cdefault) c ++
                             i16_u8v (length c) ++
                             concatMap (ugen_u8v g) c ++
                             i16_u8v (length u) ++
                             concatMap (ugen_u8v g) u
