module Sound.SC3.Server.Synthdef.Reconstruct where

import Data.Char
import Data.Function
import Data.List
import Sound.SC3.Server.Synthdef {- hsc3 -}
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.UGen
import Text.Printf

node_sort :: [Node] -> [Node]
node_sort = sortBy (compare `on` node_id)

from_port_label :: Char -> FromPort -> String
from_port_label jn fp =
    case fp of
      FromPort_C n -> printf "c_%d" n
      FromPort_K n _ -> printf "k_%d" n
      FromPort_U n Nothing -> printf "u_%d" n
      FromPort_U n (Just i) -> printf "u_%d%co_%d" n jn i

is_operator_name :: String -> Bool
is_operator_name nm =
    case nm of
      c:_ -> not (isLetter c)
      _ -> False

parenthesise_operator :: String -> String
parenthesise_operator nm =
    if is_operator_name nm
    then printf "(%s)" nm
    else nm

-- > import Sound.SC3.ID
--
-- > let u = out (control KR "bus" 0) ((sinOsc AR 440 0 + whiteNoise 'a' AR) * 0.1)
-- > in putStrLn (reconstruct_graph (synth u))
reconstruct_graph :: Graph -> String
reconstruct_graph g =
    let (Graph _ c k u) = g
        ls = concat [map reconstruct_c (node_sort c)
                    ,map reconstruct_k (node_sort k)
                    ,map (reconstruct_u g) u]
    in unlines (filter (not . null) ls)

reconstruct_c :: Node -> String
reconstruct_c u =
    let l = node_label u
        c = node_c_value u
    in printf "%s = constant %f" l c

reconstruct_k :: Node -> String
reconstruct_k u =
    let l = node_label u
        r = node_k_rate u
        n = node_k_name u
        d = node_k_default u
    in printf "%s = control %s \"%s\" %f" l (show r) n d

ugen_qname :: String -> Special -> (String,String)
ugen_qname nm (Special n) =
    case nm of
      "UnaryOpUGen" -> ("uop",unaryName n)
      "BinaryOpUGen" -> ("binop",binaryName n)
      _ -> ("ugen",nm)

reconstruct_u :: Graph -> Node -> String
reconstruct_u _ u =
    let l = node_label u
        r = node_u_rate u
        i = node_u_inputs u
        i_s = unwords (map (from_port_label '_') i)
        i_l = intercalate "," (map (from_port_label '_') i)
        s = node_u_special u
        (q,n) = ugen_qname (node_u_name u) s
        z = node_id u
        o = length (node_u_outputs u)
        u_s = printf "%s = ugen \"%s\" %s [%s] %d" l n (show r) i_l o
        nd_s = printf "%s = nondet \"%s\" (UId %d) %s [%s] %d" l n z (show r) i_l o
    in if is_implicit_control u
       then ""
       else case q of
              "ugen" -> if node_u_ugenid u == NoId then u_s else nd_s
              _ -> printf "%s = %s \"%s\" %s %s" l q n (show r) i_s

{-
--import Sound.SC3.UGen.Plain
--import Sound.SC3.UGen.Rate
-- > Sound.SC3.UGen.Dot.draw test
test :: UGen
test =
    let c_1 = constant 440.0
        c_2 = constant 0.0
        c_6 = constant 0.1
        k_0 = control KR "bus" 0.0
        u_3 = ugen "SinOsc" AR [c_1,c_2] 1
        u_4 = nondet "WhiteNoise" (UId 4) AR [] 1
        u_5 = binop "+" AR u_3 u_4
        u_7 = binop "*" AR u_5 c_6
        u_8 = ugen "Out" AR [k_0,u_7] 0
    in u_8
-}
