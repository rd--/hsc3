module Sound.SC3.Server.Graphdef.Graph where

import Data.Maybe{- base -}

import Sound.OSC.Type {- hosc -}

import qualified Sound.SC3.Server.Graphdef as G
import Sound.SC3.UGen.Graph
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

-- * Encode to 'G.Graphdef'

-- | Construct 'Input' form required by byte-code generator.
make_input :: Maps -> FromPort -> G.Input
make_input (cs,ks,_,us,kt) fp =
    case fp of
      FromPort_C n -> G.Input (-1) (fetch n cs)
      FromPort_K n t -> let i = ktype_map_lookup t kt
                        in G.Input i (fetch_k n t ks)
      FromPort_U n p -> G.Input (fetch n us) (fromMaybe 0 p)

node_k_to_control :: Maps -> Node -> G.Control
node_k_to_control (_,_,ks,_,_) nd =
    case nd of
      NodeK n _ _ nm _ _ _ -> (ascii nm,fetch n ks)
      _ -> error "node_k_to_control"

-- | Byte-encode 'NodeU' primitive node.
node_u_to_ugen :: Maps -> Node -> G.UGen
node_u_to_ugen m n =
    case n of
      NodeU _ r nm i o (Special s) _ ->
          let i' = map (make_input m) i
          in (ascii nm,rateId r,i',map rateId o,s)
      _ -> error "encode_node_u: illegal input"

-- | Construct instrument definition bytecode.
graph_to_graphdef :: String -> Graph -> G.Graphdef
graph_to_graphdef nm g =
    let Graph _ cs ks us = g
        cs' = map node_c_value cs
        mm = mk_maps g
        ks_def = map node_k_default ks
        ks_ctl = map (node_k_to_control mm) ks
        us' = map (node_u_to_ugen mm) us
    in G.Graphdef (ascii nm) cs' (zip ks_ctl ks_def) us'
