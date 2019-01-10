-- | A /disassembler/ for UGen graphs.
module Sound.SC3.UGen.Graph.Reconstruct where

import Data.Char {- base -}
import Data.List {- base -}
import Text.Printf {- base -}

import qualified Sound.SC3.Common.Math.Operator as Operator
import Sound.SC3.Common.Rate
import qualified Sound.SC3.UGen.Graph as Graph
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

from_port_label :: Char -> Graph.From_Port -> String
from_port_label jn fp =
    case fp of
      Graph.From_Port_C n -> printf "c_%d" n
      Graph.From_Port_K n _ -> printf "k_%d" n
      Graph.From_Port_U n Nothing -> printf "u_%d" n
      Graph.From_Port_U n (Just i) -> printf "u_%d%co_%d" n jn i

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

reconstruct_graph :: Graph.U_Graph -> ([String],String)
reconstruct_graph g =
    let (Graph.U_Graph _ c k u) = g
        ls = concat [map reconstruct_c_str (Graph.u_node_sort c)
                    ,map reconstruct_k_str (Graph.u_node_sort k)
                    ,concatMap reconstruct_u_str u]
    in (filter (not . null) ls,reconstruct_mrg_str u)

reconstruct_graph_module :: String -> Graph.U_Graph -> [String]
reconstruct_graph_module nm gr =
  let imp = ["import Sound.SC3"
            ,"import Sound.SC3.Common.Base"
            ,"import Sound.SC3.UGen.Plain"]
      (b0:bnd,res) = reconstruct_graph gr
      hs = ("  let " ++ b0) : map ("      " ++ ) bnd ++ ["  in " ++ res]
      pre = [nm ++ " :: UGen",nm ++ " ="]
  in (imp ++ pre ++ hs)

{- | Generate a reconstruction of a 'Graph'.

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Graph {- hsc3 -}
> import Sound.SC3.UGen.Graph.Reconstruct {- hsc3 -}

> let k = control KR "bus" 0
> let o = sinOsc AR 440 0 + whiteNoise 'α' AR
> let u = out k (pan2 (o * 0.1) 0 1)
> let m = mrg [u,out 1 (impulse AR 1 0 * 0.1)]
> putStrLn (reconstruct_graph_str "anon" (ugen_to_graph m))

-}
reconstruct_graph_str :: String -> Graph.U_Graph -> String
reconstruct_graph_str nm = unlines . reconstruct_graph_module nm

reconstruct_c_str :: Graph.U_Node -> String
reconstruct_c_str u =
    let l = Graph.u_node_label u
        c = Graph.u_node_c_value u
    in printf "%s = constant (%f::Sample)" l c

reconstruct_c_ugen :: Graph.U_Node -> UGen
reconstruct_c_ugen u = constant (Graph.u_node_c_value u)

-- | Discards index.
reconstruct_k_rnd :: Graph.U_Node -> (Rate,String,Sample)
reconstruct_k_rnd u =
    let r = Graph.u_node_k_rate u
        n = Graph.u_node_k_name u
        d = Graph.u_node_k_default u
    in (r,n,d)

reconstruct_k_str :: Graph.U_Node -> String
reconstruct_k_str u =
    let l = Graph.u_node_label u
        (r,n,d) = reconstruct_k_rnd u
    in printf "%s = control %s \"%s\" %f" l (show r) n d

reconstruct_k_ugen :: Graph.U_Node -> UGen
reconstruct_k_ugen u =
    let (r,n,d) = reconstruct_k_rnd u
    in control_f64 r Nothing n d

ugen_qname :: String -> Special -> (String,String)
ugen_qname nm (Special n) =
    case nm of
      "UnaryOpUGen" -> ("uop CS",Operator.unaryName n)
      "BinaryOpUGen" -> ("binop CS",Operator.binaryName n)
      _ -> ("ugen",nm)

reconstruct_mce_str :: Graph.U_Node -> String
reconstruct_mce_str u =
    let o = length (Graph.u_node_u_outputs u)
        l = Graph.u_node_label u
        p = map (printf "%s_o_%d" l) [0 .. o - 1]
        p' = intercalate "," p
    in if o <= 1
       then ""
       else printf "[%s] = mceChannels %s" p' l

reconstruct_u_str :: Graph.U_Node -> [String]
reconstruct_u_str u =
    let l = Graph.u_node_label u
        r = Graph.u_node_u_rate u
        i = Graph.u_node_u_inputs u
        i_s = unwords (map (from_port_label '_') i)
        i_l = intercalate "," (map (from_port_label '_') i)
        s = Graph.u_node_u_special u
        (q,n) = ugen_qname (Graph.u_node_u_name u) s
        z = Graph.u_node_id u
        o = length (Graph.u_node_u_outputs u)
        u_s = printf "%s = ugen \"%s\" %s [%s] %d" l n (show r) i_l o
        nd_s = let t = "%s = nondet \"%s\" (UId %d) %s [%s] %d"
               in printf t l n z (show r) i_l o
        c = case q of
              "ugen" -> if Graph.u_node_u_ugenid u == NoId then u_s else nd_s
              _ -> printf "%s = %s \"%s\" %s %s" l q n (show r) i_s
        m = reconstruct_mce_str u
    in if Graph.u_node_is_implicit_control u
       then []
       else if null m then [c] else [c,m]

reconstruct_mrg_str :: [Graph.U_Node] -> String
reconstruct_mrg_str u =
    let zero_out n = not (Graph.u_node_is_implicit_control n) && null (Graph.u_node_u_outputs n)
    in case map Graph.u_node_label (filter zero_out u) of
         [] -> error "reconstruct_mrg_str"
         [o] -> printf "%s" o
         o -> printf "mrg [%s]" (intercalate "," o)
