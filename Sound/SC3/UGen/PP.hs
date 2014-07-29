module Sound.SC3.UGen.PP where

import qualified Data.Char as C {- base -}
import Data.List {- split -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Numeric {- base -}

import Sound.SC3.UGen.MCE
import Sound.SC3.UGen.Meta
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

-- | Print FORTH (reverse polish) notation for graph, see hsc3-forth
-- for details.  The flag controls printing of 'UId' entries.
ugen_graph_forth_pp :: Bool -> UGen -> String
ugen_graph_forth_pp print_uid u =
    let recur = ugen_graph_forth_pp print_uid
        prim_pp (Primitive rt nm i _ sp k) =
            let rt' = if ugen_is_operator nm || ugen_is_filter nm
                      then ""
                      else ('.' : map C.toLower (show rt))
                nm' = concat [ugen_user_name nm sp,rt']
                k' = case k of
                       NoId -> Nothing
                       UId uid -> if print_uid then Just (show uid ++ " uid") else Nothing
            in unwords (map recur i ++ catMaybes [k',Just nm'])
    in case u of
         Constant_U (Constant n) -> real_pp n
         Label_U (Label s) -> concat ["s\" ",show s,"\""]
         Primitive_U p -> prim_pp p
         Proxy_U (Proxy p n) -> prim_pp p ++ "@" ++ show n
         MCE_U (MCE_Unit u') -> recur u'
         MCE_U (MCE_Vector v) ->
             if mce_is_direct_proxy (MCE_Vector v)
             then prim_pp (proxySource (fromJust (un_proxy (head v))))
             else unwords (map recur v ++ [show (length v),"mce"])
         MRG_U (MRG l r) -> unwords [recur l,recur r,"2","mrg"]
         _ -> show u
