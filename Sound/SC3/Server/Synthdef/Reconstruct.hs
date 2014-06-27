-- | A /disasembler/ for UGen graphs.
module Sound.SC3.Server.Synthdef.Reconstruct where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Text.Printf {- base -}

import Sound.SC3.Server.Graph
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

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

-- | Generate a reconstruction of a 'Graph'.
--
-- > import Sound.SC3.ID
--
-- > let {k = control KR "bus" 0
-- >     ;o = sinOsc AR 440 0 + whiteNoise 'a' AR
-- >     ;u = out k (pan2 (o * 0.1) 0 1)
-- >     ;m = mrg [u,out 1 (impulse AR 1 0 * 0.1)]}
-- > in putStrLn (reconstruct_graph_str (synth m))
reconstruct_graph_str :: Graph -> String
reconstruct_graph_str g =
    let (Graph _ c k u) = g
        ls = concat [map reconstruct_c_str (node_sort c)
                    ,map reconstruct_k_str (node_sort k)
                    ,concatMap reconstruct_u_str u
                    ,[reconstruct_mrg_str u]]
    in unlines (filter (not . null) ls)

reconstruct_c_str :: Node -> String
reconstruct_c_str u =
    let l = node_label u
        c = node_c_value u
    in printf "%s = constant (%f::Float)" l c

reconstruct_c_ugen :: Node -> UGen
reconstruct_c_ugen u = constant (node_c_value u)

-- | Discards index.
reconstruct_k_rnd :: Node -> (Rate,String,Float)
reconstruct_k_rnd u =
    let r = node_k_rate u
        n = node_k_name u
        d = node_k_default u
    in (r,n,d)

reconstruct_k_str :: Node -> String
reconstruct_k_str u =
    let l = node_label u
        (r,n,d) = reconstruct_k_rnd u
    in printf "%s = control %s \"%s\" %f" l (show r) n d

reconstruct_k_ugen :: Node -> UGen
reconstruct_k_ugen u =
    let (r,n,d) = reconstruct_k_rnd u
    in control_f32 r Nothing n d

ugen_qname :: String -> Special -> (String,String)
ugen_qname nm (Special n) =
    case nm of
      "UnaryOpUGen" -> ("uop",unaryName n)
      "BinaryOpUGen" -> ("binop",binaryName n)
      _ -> ("ugen",nm)

reconstruct_mce_str :: Node -> String
reconstruct_mce_str u =
    let o = length (node_u_outputs u)
        l = node_label u
        p = map (printf "%s_o_%d" l) [0 .. o - 1]
        p' = intercalate "," p
    in if o <= 1
       then ""
       else printf "[%s] = mceChannels %s" p' l

reconstruct_u_str :: Node -> [String]
reconstruct_u_str u =
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
        nd_s = let t = "%s = nondet \"%s\" (UId %d) %s [%s] %d"
               in printf t l n z (show r) i_l o
        c = case q of
              "ugen" -> if node_u_ugenid u == NoId then u_s else nd_s
              _ -> printf "%s = %s \"%s\" %s %s" l q n (show r) i_s
        m = reconstruct_mce_str u
    in if is_implicit_control u
       then []
       else if null m then [c] else [c,m]

reconstruct_mrg_str :: [Node] -> String
reconstruct_mrg_str u =
    let zero_out n = not (is_implicit_control n) && null (node_u_outputs n)
    in case map node_label (filter zero_out u) of
         [] -> error "reconstruct_mrg_str"
         [o] -> printf "%s" o
         o -> printf "mrg [%s]" (intercalate "," o)
