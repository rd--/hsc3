module Sound.SC3.UGen.PP where

import Data.List {- split -}
import Data.Ratio {- base -}
import Numeric {- base -}

import Sound.SC3.UGen.MCE
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | The default show is odd, 0.05 shows as 5.0e-2.
double_pp :: Int -> Double -> String
double_pp k n =
    let f = reverse . dropWhile (== '0') . reverse
    in f (showFFloat (Just k) n "")

-- | Print as integer if integral, else as real.
real_pp :: Double -> String
real_pp n =
    let r = toRational n
    in if denominator r == 1 then show (numerator r) else double_pp 5 n

bracketed :: (a,a) -> [a] -> [a]
bracketed (l,r) x = l : x ++ [r]

-- | Print constants and labels directly, primitives as un-adorned
-- names, mce as [p,q], mrg as p&q, contols as nm=def and proxies as
-- u@n.
ugen_concise_pp :: UGen -> String
ugen_concise_pp u =
    let prim_pp (Primitive _ nm _ _ sp _) = ugen_user_name nm sp
    in case u of
         Constant_U (Constant n) -> real_pp n
         Control_U (Control _ _ nm def _ _) -> nm ++ "=" ++ real_pp def
         Label_U (Label s) -> bracketed ('"','"') s
         Primitive_U p -> prim_pp p
         Proxy_U (Proxy p n) -> prim_pp p ++ "@" ++ show n
         MCE_U (MCE_Unit u') -> ugen_concise_pp u'
         MCE_U (MCE_Vector v) -> bracketed ('[',']') (intercalate "," (map ugen_concise_pp v))
         MRG_U (MRG l r) -> unwords [ugen_concise_pp l,"&",ugen_concise_pp r]
