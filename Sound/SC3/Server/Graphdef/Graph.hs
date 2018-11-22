-- | Transform 'Graph.U_Graph' to 'Graphdef.Graphdef'.
module Sound.SC3.Server.Graphdef.Graph where

import qualified Data.IntMap as M {- containers -}
import Data.Maybe {- base -}

import qualified Sound.OSC.Datum as Datum {- hosc -}

import qualified Sound.SC3.UGen.Graph as Graph
import qualified Sound.SC3.UGen.Rate as Rate
import qualified Sound.SC3.UGen.Type as Type
import qualified Sound.SC3.Server.Graphdef as Graphdef

-- * Maps

-- | (Int,Int) map.
type Int_Map = M.IntMap Int

-- | (constants-map,controls,controls-map,ugen-map,ktype-map)
type Encoding_Maps = (Int_Map,[Graph.U_Node],Int_Map,Int_Map,[(Rate.K_Type,Int)])

-- | Generate 'Encoding_Maps' translating node identifiers to synthdef indexes.
mk_encoding_maps :: Graph.U_Graph -> Encoding_Maps
mk_encoding_maps (Graph.U_Graph _ cs ks us) =
    (M.fromList (zip (map Graph.u_node_id cs) [0..])
    ,ks
    ,M.fromList (zip (map Graph.u_node_id ks) [0..])
    ,M.fromList (zip (map Graph.u_node_id us) [0..])
    ,Graph.u_node_mk_ktype_map us)

-- | Locate index in map given node identifer 'UID_t'.
uid_lookup :: Type.UID_t -> Int_Map -> Int
uid_lookup = M.findWithDefault (error "uid_lookup")

-- | Lookup 'K_Type' index from map (erroring variant of 'lookup').
ktype_map_lookup :: Rate.K_Type -> [(Rate.K_Type,Int)] -> Int
ktype_map_lookup k =
    let e = error (show ("ktype_map_lookup",k))
    in fromMaybe e . lookup k

-- * Encoding

-- | Byte-encode 'Graph.From_Port' primitive node.
make_input :: Encoding_Maps -> Graph.From_Port -> Graphdef.Input
make_input (cs,ks,_,us,kt) fp =
    case fp of
      Graph.From_Port_C n -> Graphdef.Input (-1) (uid_lookup n cs)
      Graph.From_Port_K n t ->
        let i = ktype_map_lookup t kt
        in Graphdef.Input i (Graph.u_node_fetch_k n t ks)
      Graph.From_Port_U n p -> Graphdef.Input (uid_lookup n us) (fromMaybe 0 p)

-- | Byte-encode 'Graph.U_Node_K' primitive node.
make_control :: Encoding_Maps -> Graph.U_Node -> Graphdef.Control
make_control (_,_,ks,_,_) nd =
    case nd of
      Graph.U_Node_K n _ _ nm _ _ _ -> (Datum.ascii nm,uid_lookup n ks)
      _ -> error "make_control"

-- | Byte-encode 'Graph.U_Node_U' primitive node.
make_ugen :: Encoding_Maps -> Graph.U_Node -> Graphdef.UGen
make_ugen m n =
    case n of
      Graph.U_Node_U _ r nm i o (Type.Special s) _ ->
          let i' = map (make_input m) i
          in (Datum.ascii nm,Rate.rateId r,i',map Rate.rateId o,s)
      _ -> error "encode_node_u: illegal input"

-- | Construct instrument definition bytecode.
graph_to_graphdef :: String -> Graph.U_Graph -> Graphdef.Graphdef
graph_to_graphdef nm g =
    let Graph.U_Graph _ cs ks us = g
        cs' = map Graph.u_node_c_value cs
        mm = mk_encoding_maps g
        ks_def = map Graph.u_node_k_default ks
        ks_ctl = map (make_control mm) ks
        us' = map (make_ugen mm) us
    in Graphdef.Graphdef (Datum.ascii nm) cs' (zip ks_ctl ks_def) us'
