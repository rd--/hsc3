-- | 'UGen' pretty-printer.
module Sound.SC3.UGen.PP where

import Data.List {- split -}

import Sound.SC3.Common.Math
import Sound.SC3.Common.Mce

import Sound.SC3.UGen.Types
import Sound.SC3.UGen.UGen

{- | Print constants and labels directly,
     primitives as un-adorned names,
     mce as [p,q],
     mrg as p&q,
     contols as nm=def,
     proxies as u@n.
     Brackets are not printed.
-}
ugen_concise_pp :: UGen -> String
ugen_concise_pp u =
    let bracketed (l,r) x = l : x ++ [r]
        prim_pp (Primitive _ nm _ _ sp _ _) = ugen_user_name nm sp
        k = 5
    in case u of
         Constant_U (Constant n _) -> real_pp k n
         Control_U (Control _ _ nm def _ _ _) -> nm ++ "=" ++ real_pp k def
         Label_U (Label s) -> bracketed ('"','"') s
         Primitive_U p -> prim_pp p
         Proxy_U (Proxy p n) -> prim_pp p ++ "@" ++ show n
         Mce_U (Mce_Scalar s) -> ugen_concise_pp s
         Mce_U (Mce_Vector v) -> bracketed ('[',']') (intercalate "," (map (ugen_concise_pp . mce_scalar_value) v))
         Mrg_U (Mrg l r) -> unwords [ugen_concise_pp l,"&",ugen_concise_pp r]
