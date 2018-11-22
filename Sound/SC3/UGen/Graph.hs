{- | 'U_Graph' and related types.

The UGen type is recursive, inputs to UGens are UGens.

This makes writing UGen graphs simple, but manipulating them awkward.

UGen equality is structural, and can be slow to determine for some UGen graph structures.

A U_Node is a non-recursive notation for a UGen, all U_Nodes have unique identifiers.

A U_Graph is constructed by a stateful traversal of a UGen.

A U_Graph is represented as a partioned (by type) set of U_Nodes, edges are implicit.

-}
module Sound.SC3.UGen.Graph where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Sound.SC3.UGen.Analysis as Analysis
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- * Types

-- | Port index.
type Port_Index = Int

-- | Type to represent the left hand side of an edge in a unit generator graph.
data From_Port = From_Port_C {port_nid :: UID_t}
               | From_Port_K {port_nid :: UID_t,port_kt :: K_Type}
               | From_Port_U {port_nid :: UID_t,port_idx :: Maybe Port_Index}
               deriving (Eq,Show)

-- | A destination port.
data To_Port = To_Port UID_t Port_Index deriving (Eq,Show)

-- | A connection from 'From_Port' to 'To_Port'.
type U_Edge = (From_Port,To_Port)

-- | Sum-type to represent nodes in unit generator graph.
--   _C = constant, _K = control, _U = ugen, _P = proxy.
data U_Node = U_Node_C {u_node_id :: UID_t
                       ,u_node_c_value :: Sample}
            | U_Node_K {u_node_id :: UID_t
                       ,u_node_k_rate :: Rate
                       ,u_node_k_index :: Maybe Int
                       ,u_node_k_name :: String
                       ,u_node_k_default :: Sample
                       ,u_node_k_type :: K_Type
                       ,u_node_k_meta :: Maybe (C_Meta Sample)}
            | U_Node_U {u_node_id :: UID_t
                       ,u_node_u_rate :: Rate
                       ,u_node_u_name :: String
                       ,u_node_u_inputs :: [From_Port]
                       ,u_node_u_outputs :: [Output]
                       ,u_node_u_special :: Special
                       ,u_node_u_ugenid :: UGenId}
            | U_Node_P {u_node_id :: UID_t
                       ,u_node_p_node :: U_Node
                       ,u_node_p_index :: Port_Index}
            deriving (Eq,Show)

-- | Type to represent a unit generator graph.
data U_Graph = U_Graph {ug_next_id :: UID_t
                       ,ug_constants :: [U_Node]
                       ,ug_controls :: [U_Node]
                       ,ug_ugens :: [U_Node]}
             deriving (Show)

-- * Ports

-- | Get 'port_idx' for 'From_Port_U', else @0@.
port_idx_or_zero :: From_Port -> Port_Index
port_idx_or_zero p =
    case p of
      From_Port_U _ (Just x) -> x
      _ -> 0

-- | Is 'From_Port' 'From_Port_U'.
is_from_port_u :: From_Port -> Bool
is_from_port_u p =
    case p of
      From_Port_U _ _ -> True
      _ -> False

-- * Nodes

-- | Is 'U_Node' a /constant/.
is_u_node_c :: U_Node -> Bool
is_u_node_c n =
    case n of
      U_Node_C _ _ -> True
      _ -> False

-- | Predicate to determine if 'U_Node' is a constant with indicated /value/.
is_u_node_c_of :: Sample -> U_Node -> Bool
is_u_node_c_of x n =
    case n of
      U_Node_C _ y -> x == y
      _ -> error "is_u_node_c_of: non U_Node_C"

-- | Is 'U_Node' a /control/.
is_u_node_k :: U_Node -> Bool
is_u_node_k n =
    case n of
      U_Node_K {} -> True
      _ -> False

-- | Predicate to determine if 'U_Node' is a control with indicated
-- /name/.  Names must be unique.
is_u_node_k_of :: String -> U_Node -> Bool
is_u_node_k_of x n =
    case n of
      U_Node_K _ _ _ y _ _ _ -> x == y
      _ -> error "is_u_node_k_of"

-- | Is 'U_Node' a /UGen/.
is_u_node_u :: U_Node -> Bool
is_u_node_u n =
    case n of
      U_Node_U {} -> True
      _ -> False

-- | Compare 'U_Node_K' values 'on' 'u_node_k_type'.
u_node_k_cmp :: U_Node -> U_Node -> Ordering
u_node_k_cmp = compare `on` u_node_k_type

-- | Sort by 'u_node_id'.
u_node_sort :: [U_Node] -> [U_Node]
u_node_sort = sortBy (compare `on` u_node_id)

-- | Equality test, error if not U_Node_K.
u_node_k_eq :: U_Node -> U_Node -> Bool
u_node_k_eq p q =
  if is_u_node_k p && is_u_node_k q
  then p == q
  else error "u_node_k_eq? not U_Node_K"

-- | 'Rate' of 'U_Node', ie. 'IR' for constants & see through 'U_Node_P'.
u_node_rate :: U_Node -> Rate
u_node_rate n =
    case n of
      U_Node_C {} -> IR
      U_Node_K {} -> u_node_k_rate n
      U_Node_U {} -> u_node_u_rate n
      U_Node_P _ n' _ -> u_node_rate n'

-- | Generate a label for 'U_Node' using the /type/ and the 'u_node_id'.
u_node_label :: U_Node -> String
u_node_label nd =
    case nd of
      U_Node_C n _ -> "c_" ++ show n
      U_Node_K n _ _ _ _ _ _ -> "k_" ++ show n
      U_Node_U n _ _ _ _ _ _ -> "u_" ++ show n
      U_Node_P n _ _ -> "p_" ++ show n

-- | Calculate all in edges for a 'U_Node_U'.
u_node_in_edges :: U_Node -> [U_Edge]
u_node_in_edges n =
    case n of
      U_Node_U x _ _ i _ _ _ -> zip i (map (To_Port x) [0..])
      _ -> error "u_node_in_edges: non U_Node_U input node"

-- | Transform 'U_Node' to 'From_Port'.
u_node_from_port :: U_Node -> From_Port
u_node_from_port d =
    case d of
      U_Node_C n _ -> From_Port_C n
      U_Node_K n _ _ _ _ t _ -> From_Port_K n t
      U_Node_U n _ _ _ o _ _ ->
          case o of
            [_] -> From_Port_U n Nothing
            _ -> error (show ("u_node_from_port: non unary U_Node_U",d))
      U_Node_P _ u p -> From_Port_U (u_node_id u) (Just p)

-- | If controls have been given indices they must be coherent.
u_node_sort_controls :: [U_Node] -> [U_Node]
u_node_sort_controls c =
    let u_node_k_ix n = fromMaybe maxBound (u_node_k_index n)
        cmp = compare `on` u_node_k_ix
        c' = sortBy cmp c
        coheres z = maybe True (== z) . u_node_k_index
        coherent = all id (zipWith coheres [0..] c')
    in if coherent then c' else error (show ("u_node_sort_controls: incoherent",c))

-- | Determine 'K_Type' of a /control/ UGen at 'U_Node_U', or not.
u_node_ktype :: U_Node -> Maybe K_Type
u_node_ktype n =
    case (u_node_u_name n,u_node_u_rate n) of
      ("Control",IR) -> Just K_IR
      ("Control",KR) -> Just K_KR
      ("TrigControl",KR) -> Just K_TR
      ("AudioControl",AR) -> Just K_AR
      _ -> Nothing

-- | Is 'U_Node' an /implicit/ control UGen?
u_node_is_implicit_control :: U_Node -> Bool
u_node_is_implicit_control n =
    let cs = ["AudioControl","Control","TrigControl"]
    in case n of
        U_Node_U x _ s _ _ _ _ -> x == -1 && s `elem` cs
        _ -> False

-- | Is U_Node implicit?
u_node_is_implicit :: U_Node -> Bool
u_node_is_implicit n = u_node_u_name n == "MaxLocalBufs" || u_node_is_implicit_control n

-- | Zero if no local buffers, or if maxLocalBufs is given.
u_node_localbuf_count :: [U_Node] -> Int
u_node_localbuf_count us =
    case find ((==) "MaxLocalBufs" . u_node_u_name) us of
      Nothing -> length (filter ((==) "LocalBuf" . u_node_u_name) us)
      Just _ -> 0

-- | Controls are a special case.  We need to know not the overall
-- index but the index in relation to controls of the same type.
u_node_fetch_k :: UID_t -> K_Type -> [U_Node] -> Int
u_node_fetch_k z t =
    let recur i ns =
            case ns of
              [] -> error "u_node_fetch_k"
              n:ns' -> if z == u_node_id n
                       then i
                       else if t == u_node_k_type n
                            then recur (i + 1) ns'
                            else recur i ns'
    in recur 0

-- | All the elements of a U_Node_U, except the u_node_id.
type U_Node_NOID = (Rate,String,[From_Port],[Output],Special,UGenId)

-- | Predicate to locate primitive, names must be unique.
u_node_eq_noid :: U_Node_NOID -> U_Node -> Bool
u_node_eq_noid x nd =
    case nd of
      U_Node_U _ r n i o s d -> (r,n,i,o,s,d) == x
      _ ->  error "u_node_eq_noid"

-- | Make map associating 'K_Type' with UGen index.
u_node_mk_ktype_map :: [U_Node] -> [(K_Type,Int)]
u_node_mk_ktype_map =
    let f (i,n) = let g ty = (ty,i) in fmap g (u_node_ktype n)
    in mapMaybe f . zip [0..]

-- * Nodes (Implicit)

-- | 4-tuple to count 'K_Type's, ie. (IR,KR,TR,AR).
type U_NODE_KS_COUNT = (Int,Int,Int,Int)

-- | Count the number of /controls/ of each 'K_Type'.
u_node_ks_count :: [U_Node] -> U_NODE_KS_COUNT
u_node_ks_count =
    let recur r ns =
            let (i,k,t,a) = r
            in case ns of
                 [] -> r
                 n:ns' -> let r' = case u_node_k_type n of
                                     K_IR -> (i+1,k,t,a)
                                     K_KR -> (i,k+1,t,a)
                                     K_TR -> (i,k,t+1,a)
                                     K_AR -> (i,k,t,a+1)
                          in recur r' ns'
    in recur (0,0,0,0)

-- | Construct implicit /control/ unit generator 'U_Nodes'.  Unit
-- generators are only constructed for instances of control types that
-- are present.
u_node_mk_implicit_ctl :: [U_Node] -> [U_Node]
u_node_mk_implicit_ctl ks =
    let (ni,nk,nt,na) = u_node_ks_count ks
        mk_n t n o =
            let (nm,r) = case t of
                            K_IR -> ("Control",IR)
                            K_KR -> ("Control",KR)
                            K_TR -> ("TrigControl",KR)
                            K_AR -> ("AudioControl",AR)
                i = replicate n r
            in if n == 0
               then Nothing
               else Just (U_Node_U (-1) r nm [] i (Special o) no_id)
    in catMaybes [mk_n K_IR ni 0
                 ,mk_n K_KR nk ni
                 ,mk_n K_TR nt (ni + nk)
                 ,mk_n K_AR na (ni + nk + nt)]

-- * Edges

-- | List of 'From_Port_U' at /e/ with multiple out edges.
u_edge_multiple_out_edges :: [U_Edge] -> [From_Port]
u_edge_multiple_out_edges e =
    let p = filter is_from_port_u (map fst e)
        p' = group (sortBy (compare `on` port_nid) p)
    in map head (filter ((> 1) . length) p')

-- * Graph

-- | Calculate all edges of a 'U_Graph'.
ug_edges :: U_Graph -> [U_Edge]
ug_edges = concatMap u_node_in_edges . ug_ugens

-- | The empty 'U_Graph'.
ug_empty_graph :: U_Graph
ug_empty_graph = U_Graph 0 [] [] []

-- | Find the maximum 'UID_t' used at 'U_Graph'.  It is an error if this is not 'ug_next_id'.
ug_maximum_id :: U_Graph -> UID_t
ug_maximum_id (U_Graph z c k u) =
  let z' = maximum (map u_node_id (c ++ k ++ u))
  in if z' /= z
     then error (show ("ug_maximum_id: not ug_next_id?",z,z'))
     else z

-- | Find 'U_Node' with indicated 'UID_t'.
ug_find_node :: U_Graph -> UID_t -> Maybe U_Node
ug_find_node (U_Graph _ cs ks us) n =
    let f x = u_node_id x == n
    in find f (cs ++ ks ++ us)

-- | Locate 'U_Node' of 'From_Port' in 'U_Graph'.
ug_from_port_node :: U_Graph -> From_Port -> Maybe U_Node
ug_from_port_node g fp = ug_find_node g (port_nid fp)

-- | Erroring variant.
ug_from_port_node_err :: U_Graph -> From_Port -> U_Node
ug_from_port_node_err g fp =
    let e = error "ug_from_port_node_err"
    in fromMaybe e (ug_from_port_node g fp)

-- * Graph (Building)

-- | Insert a constant 'U_Node' into the 'U_Graph'.
ug_push_c :: Sample -> U_Graph -> (U_Node,U_Graph)
ug_push_c x g =
    let n = U_Node_C (ug_next_id g) x
    in (n,g {ug_constants = n : ug_constants g
            ,ug_next_id = ug_next_id g + 1})

-- | Either find existing 'Constant' 'U_Node', or insert a new 'U_Node'.
ug_mk_node_c :: Constant -> U_Graph -> (U_Node,U_Graph)
ug_mk_node_c (Constant x) g =
    let y = find (is_u_node_c_of x) (ug_constants g)
    in maybe (ug_push_c x g) (\y' -> (y',g)) y

-- | Insert a control node into the 'U_Graph'.
ug_push_k :: Control -> U_Graph -> (U_Node,U_Graph)
ug_push_k (Control r ix nm d tr meta) g =
    let n = U_Node_K (ug_next_id g) r ix nm d (ktype r tr) meta
    in (n,g {ug_controls = n : ug_controls g
            ,ug_next_id = ug_next_id g + 1})

-- | Either find existing 'Control' 'U_Node', or insert a new 'U_Node'.
ug_mk_node_k :: Control -> U_Graph -> (U_Node,U_Graph)
ug_mk_node_k c g =
    let nm = controlName c
        y = find (is_u_node_k_of nm) (ug_controls g)
    in maybe (ug_push_k c g) (\y' -> (y',g)) y

-- | Insert a /primitive/ 'U_Node_U' into the 'U_Graph'.
ug_push_u :: U_Node_NOID -> U_Graph -> (U_Node,U_Graph)
ug_push_u (r,nm,i,o,s,d) g =
    let n = U_Node_U (ug_next_id g) r nm i o s d
    in (n,g {ug_ugens = n : ug_ugens g
            ,ug_next_id = ug_next_id g + 1})

-- | Recursively traverse set of UGen calling 'ug_mk_node'.
ug_mk_node_rec :: [UGen] -> [U_Node] -> U_Graph -> ([U_Node],U_Graph)
ug_mk_node_rec u n g =
    case u of
      [] -> (reverse n,g)
      x:xs -> let (y,g') = ug_mk_node x g
              in ug_mk_node_rec xs (y:n) g'

-- | Run 'ug_mk_node_rec' at inputs and either find existing primitive
-- node or insert a new one.
ug_mk_node_u :: Primitive -> U_Graph -> (U_Node,U_Graph)
ug_mk_node_u (Primitive r nm i o s d) g =
    let (i',g') = ug_mk_node_rec i [] g
        i'' = map u_node_from_port i'
        u = (r,nm,i'',o,s,d)
        y = find (u_node_eq_noid u) (ug_ugens g')
    in maybe (ug_push_u u g') (\y' -> (y',g')) y

-- | Proxies do not get stored in the graph.
ug_mk_node_p :: U_Node -> Port_Index -> U_Graph -> (U_Node,U_Graph)
ug_mk_node_p n p g =
    let z = ug_next_id g
    in (U_Node_P z n p,g {ug_next_id = z + 1})

-- | Transform 'UGen' into 'U_Graph', appending to existing 'U_Graph'.
--   Allow RHS of MRG node to be MCE (splice all nodes into graph).
ug_mk_node :: UGen -> U_Graph -> (U_Node,U_Graph)
ug_mk_node u g =
    case u of
      Constant_U c -> ug_mk_node_c c g
      Control_U k -> ug_mk_node_k k g
      Label_U _ -> error (show ("ug_mk_node: label",u))
      Primitive_U p -> ug_mk_node_u p g
      Proxy_U p ->
          let (n,g') = ug_mk_node_u (proxySource p) g
          in ug_mk_node_p n (proxyIndex p) g'
      MRG_U m ->
          let f g' l = case l of
                         [] -> g'
                         n:l' -> let (_,g'') = ug_mk_node n g' in f g'' l'
          in ug_mk_node (mrgLeft m) (f g (mceChannels (mrgRight m)))
      MCE_U _ -> error (show ("ug_mk_node: mce",u))

-- | Add implicit /control/ UGens to 'U_Graph'.
ug_add_implicit_ctl :: U_Graph -> U_Graph
ug_add_implicit_ctl g =
    let (U_Graph z cs ks us) = g
        ks' = sortBy u_node_k_cmp ks
        im = if null ks' then [] else u_node_mk_implicit_ctl ks'
        us' = im ++ us
    in U_Graph z cs ks' us'

-- | Add implicit 'maxLocalBufs' if not present.
ug_add_implicit_buf :: U_Graph -> U_Graph
ug_add_implicit_buf g =
    case u_node_localbuf_count (ug_ugens g) of
      0 -> g
      n -> let (c,g') = ug_mk_node_c (Constant (fromIntegral n)) g
               p = u_node_from_port c
               u = U_Node_U (-1) IR "MaxLocalBufs" [p] [] (Special 0) no_id
           in g' {ug_ugens = u : ug_ugens g'}

-- | 'ug_add_implicit_buf' and 'ug_add_implicit_ctl'.
ug_add_implicit :: U_Graph -> U_Graph
ug_add_implicit = ug_add_implicit_buf . ug_add_implicit_ctl

-- | Remove implicit UGens from 'U_Graph'
ug_remove_implicit :: U_Graph -> U_Graph
ug_remove_implicit g =
    let u = filter (not . u_node_is_implicit) (ug_ugens g)
    in g {ug_ugens = u}

-- * Graph (Queries)

-- | Descendents at 'U_Graph' of 'U_Node'.
u_node_descendents :: U_Graph -> U_Node -> [U_Node]
u_node_descendents g n =
    let e = ug_edges g
        c = filter ((== u_node_id n) . port_nid . fst) e
        f (To_Port k _) = k
    in mapMaybe (ug_find_node g . f . snd) c

-- * PV edge accounting

-- | List @PV@ 'U_Node's at 'U_Graph' with multiple out edges.
ug_pv_multiple_out_edges :: U_Graph -> [U_Node]
ug_pv_multiple_out_edges g =
    let e = ug_edges g
        p = u_edge_multiple_out_edges e
        n = mapMaybe (ug_find_node g . port_nid) p
    in filter (Analysis.primitive_is_pv_rate . u_node_u_name) n

-- | Error string if graph has an invalid @PV@ subgraph, ie. multiple out edges
-- at @PV@ node not connecting to @Unpack1FFT@ & @PackFFT@, else Nothing.
ug_pv_check :: U_Graph -> Maybe String
ug_pv_check g =
    case ug_pv_multiple_out_edges g of
      [] -> Nothing
      n ->
        let d = concatMap (map u_node_u_name . u_node_descendents g) n
        in if any Analysis.primitive_is_pv_rate d || any (`elem` ["IFFT"]) d
           then Just (show ("PV: multiple out edges, see pv_split",map u_node_u_name n,d))
           else Nothing

-- | Variant that runs 'error' as required.
ug_pv_validate :: U_Graph -> U_Graph
ug_pv_validate g =
    case ug_pv_check g of
      Nothing -> g
      Just err -> error err

-- * UGen to U_Graph

{- | Transform a unit generator into a graph.
     'ug_mk_node' begins with an empty graph,
     then reverses the resulting 'UGen' list and sorts the 'Control' list,
     and finally adds implicit nodes and validates PV sub-graphs.

> import Sound.SC3 {- hsc3 -}
> ugen_to_graph (out 0 (pan2 (sinOsc AR 440 0) 0.5 0.1))

-}
ugen_to_graph :: UGen -> U_Graph
ugen_to_graph u =
    let (_,g) = ug_mk_node (prepare_root u) ug_empty_graph
        g' = g {ug_ugens = reverse (ug_ugens g)
               ,ug_controls = u_node_sort_controls (ug_controls g)}
    in ug_pv_validate (ug_add_implicit g')

-- * Stat

-- | Simple statistical analysis of a unit generator graph.
ug_stat_ln :: U_Graph -> [String]
ug_stat_ln s =
    let cs = ug_constants s
        ks = ug_controls s
        us = ug_ugens s
        u_nm z = ugen_user_name (u_node_u_name z) (u_node_u_special z)
        hist pp_f =
          let h (x:xs) = (x,length (x:xs))
              h [] = error "graph_stat_ln"
          in unwords . map (\(p,q) -> pp_f p ++ "×" ++ show q) . map h . group . sort
    in ["number of constants       : " ++ show (length cs)
       ,"number of controls        : " ++ show (length ks)
       ,"control rates             : " ++ hist show (map u_node_k_rate ks)
       ,"control names             : " ++ unwords (map u_node_k_name ks)
       ,"number of unit generators : " ++ show (length us)
       ,"unit generator rates      : " ++ hist show (map u_node_u_rate us)
       ,"unit generator set        : " ++ hist id (map u_nm us)
       ,"unit generator sequence   : " ++ unwords (map u_nm us)]

-- | 'unlines' of 'ug_stat_ln'.
ug_stat :: U_Graph -> String
ug_stat = unlines . ug_stat_ln

-- * Indices

-- | Find indices of all instances of the named UGen at 'Graph'.
-- The index is required when using 'Sound.SC3.Server.Command.u_cmd'.
ug_ugen_indices :: (Num n,Enum n) => String -> U_Graph -> [n]
ug_ugen_indices nm =
    let f (k,nd) =
            case nd of
              U_Node_U _ _ nm' _ _ _ _ -> if nm == nm' then Just k else Nothing
              _ -> Nothing
    in mapMaybe f . zip [0..] . ug_ugens
