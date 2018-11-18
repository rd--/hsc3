-- | Transform 'Graph' to 'Graphdef'.
module Sound.SC3.Server.Graphdef.Graph where

import Data.Maybe {- base -}

import qualified Sound.OSC.Datum as Datum {- hosc -}

import qualified Sound.SC3.UGen.Graph as Graph
import qualified Sound.SC3.UGen.Rate as Rate
import qualified Sound.SC3.UGen.Type as Type
import qualified Sound.SC3.Server.Graphdef as Graphdef

-- | Construct 'Input' form required by byte-code generator.
make_input :: Graph.Maps -> Graph.FromPort -> Graphdef.Input
make_input (cs,ks,_,us,kt) fp =
    case fp of
      Graph.FromPort_C n -> Graphdef.Input (-1) (Graph.fetch n cs)
      Graph.FromPort_K n t ->
        let i = Graph.ktype_map_lookup t kt
        in Graphdef.Input i (Graph.fetch_k n t ks)
      Graph.FromPort_U n p -> Graphdef.Input (Graph.fetch n us) (fromMaybe 0 p)

node_k_to_control :: Graph.Maps -> Graph.Node -> Graphdef.Control
node_k_to_control (_,_,ks,_,_) nd =
    case nd of
      Graph.NodeK n _ _ nm _ _ _ -> (Datum.ascii nm,Graph.fetch n ks)
      _ -> error "node_k_to_control"

-- | Byte-encode 'NodeU' primitive node.
node_u_to_ugen :: Graph.Maps -> Graph.Node -> Graphdef.UGen
node_u_to_ugen m n =
    case n of
      Graph.NodeU _ r nm i o (Type.Special s) _ ->
          let i' = map (make_input m) i
          in (Datum.ascii nm,Rate.rateId r,i',map Rate.rateId o,s)
      _ -> error "encode_node_u: illegal input"

-- | Construct instrument definition bytecode.
graph_to_graphdef :: String -> Graph.Graph -> Graphdef.Graphdef
graph_to_graphdef nm g =
    let Graph.Graph _ cs ks us = g
        cs' = map Graph.node_c_value cs
        mm = Graph.mk_maps g
        ks_def = map Graph.node_k_default ks
        ks_ctl = map (node_k_to_control mm) ks
        us' = map (node_u_to_ugen mm) us
    in Graphdef.Graphdef (Datum.ascii nm) cs' (zip ks_ctl ks_def) us'
