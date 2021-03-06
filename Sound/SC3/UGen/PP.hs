-- | 'UGen' pretty-printer.
module Sound.SC3.UGen.PP where

import Data.List {- split -}

import Sound.SC3.Common.Math
import Sound.SC3.Common.Mce
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Place /x/ in (/l/,/r/) brackets.
bracketed :: (a,a) -> [a] -> [a]
bracketed (l,r) x = l : x ++ [r]

-- | Print constants and labels directly, primitives as un-adorned
-- names, mce as [p,q], mrg as p&q, contols as nm=def and proxies as
-- u@n.
ugen_concise_pp :: UGen -> String
ugen_concise_pp u =
    let prim_pp (Primitive _ nm _ _ sp _) = ugen_user_name nm sp
        k = 5
    in case u of
         UGen (CConstant (Constant n)) -> real_pp k n
         UGen (CControl (Control _ _ nm def _ _)) -> nm ++ "=" ++ real_pp k def
         UGen (CLabel (Label s)) -> bracketed ('"','"') s
         UGen (CPrimitive p) -> prim_pp p
         UGen (CProxy (Proxy p n _)) -> ugen_concise_pp p ++ "@" ++ show n
         UGen (CMce (Mce_Unit u') _) -> ugen_concise_pp u'
         UGen (CMce (Mce_Vector v) _) -> bracketed ('[',']') (intercalate "," (map ugen_concise_pp v))
         UGen (CMrg (Mrg l r) _) -> unwords [ugen_concise_pp l,"&",ugen_concise_pp r]
