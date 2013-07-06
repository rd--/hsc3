module Sound.SC3.Server.Synthdef.Internal where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.IntMap as M {- containers -}
import Data.Function {- base -}
import Data.List{- base -}
import Data.Maybe{- base -}
import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Coding.Cast {- hosc -}

import Sound.SC3.Server.Synthdef.Type
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- * Building

-- | Find 'Node' with indicated 'NodeId'.
find_node :: Graph -> NodeId -> Maybe Node
find_node (Graph _ cs ks us) n =
    let f x = node_id x == n
    in find f (cs ++ ks ++ us)

-- | Generate a label for 'Node' using the /type/ and the 'node_id'.
node_label :: Node -> String
node_label nd =
    case nd of
      NodeC n _ -> "c_" ++ show n
      NodeK n _ _ _ _ _ -> "k_" ++ show n
      NodeU n _ _ _ _ _ _ -> "u_" ++ show n
      NodeP n _ _ -> "p_" ++ show n

-- | Get 'port_idx' for 'FromPort_U', else @0@.
port_idx_or_zero :: FromPort -> PortIndex
port_idx_or_zero p =
    case p of
      FromPort_U _ (Just x) -> x
      _ -> 0

-- | Is 'Node' a /constant/.
is_node_c :: Node -> Bool
is_node_c n =
    case n of
      NodeC _ _ -> True
      _ -> False

-- | Is 'Node' a /control/.
is_node_k :: Node -> Bool
is_node_k n =
    case n of
      NodeK {} -> True
      _ -> False

-- | Is 'Node' a /UGen/.
is_node_u :: Node -> Bool
is_node_u n =
    case n of
      NodeU {} -> True
      _ -> False

-- | Calculate all edges given a set of 'NodeU'.
edges :: [Node] -> [Edge]
edges =
    let f n = case n of
                NodeU x _ _ i _ _ _ -> zip i (map (ToPort x) [0..])
                _ -> error "edges: non NodeU input node"
    in concatMap f

-- | Transform 'Node' to 'FromPort'.
as_from_port :: Node -> FromPort
as_from_port d =
    case d of
      NodeC n _ -> FromPort_C n
      NodeK n _ _ _ _ t -> FromPort_K n t
      NodeU n _ _ _ o _ _ ->
          case o of
            [_] -> FromPort_U n Nothing
            _ -> error (show ("as_from_port: non unary NodeU",d))
      NodeP _ u p -> FromPort_U (node_id u) (Just p)

-- | Locate 'Node' of 'FromPort' in 'Graph'.
from_port_node :: Graph -> FromPort -> Maybe Node
from_port_node g fp = find_node g (port_nid fp)

-- | The empty 'Graph'.
empty_graph :: Graph
empty_graph = Graph 0 [] [] []

-- | Find the maximum 'NodeId' used at 'Graph' (this ought normally be
-- the 'nextId').
graph_maximum_id :: Graph -> NodeId
graph_maximum_id (Graph _ c k u) = maximum (map node_id (c ++ k ++ u))

-- | Compare 'NodeK' values 'on' 'node_k_type'.
node_k_cmp :: Node -> Node -> Ordering
node_k_cmp = compare `on` node_k_type

-- | Determine class of control given 'Rate' and /trigger/ status.
ktype :: Rate -> Bool -> KType
ktype r tr =
    if tr
    then case r of
           KR -> K_TR
           _ -> error "ktype: non KR trigger control"
    else case r of
           IR -> K_IR
           KR -> K_KR
           AR -> K_AR
           DR -> error "ktype: DR control"

-- | Predicate to determine if 'Node' is a constant with indicated /value/.
find_c_p :: Float -> Node -> Bool
find_c_p x n =
    case n of
      NodeC _ y -> x == y
      _ -> error "find_c_p: non NodeC"

-- | Insert a constant 'Node' into the 'Graph'.
push_c :: Float -> Graph -> (Node,Graph)
push_c x g =
    let n = NodeC (nextId g) x
    in (n,g {constants = n : constants g
            ,nextId = nextId g + 1})

-- | Either find existing 'Constant' 'Node', or insert a new 'Node'.
mk_node_c :: Constant -> Graph -> (Node,Graph)
mk_node_c (Constant x) g =
    let y = find (find_c_p x) (constants g)
    in maybe (push_c x g) (\y' -> (y',g)) y

-- | Predicate to determine if 'Node' is a control with indicated
-- /name/.  Names must be unique.
find_k_p :: String -> Node -> Bool
find_k_p x n =
    case n of
      NodeK _ _ _ y _ _ -> x == y
      _ -> error "find_k_p"

-- | Insert a control node into the 'Graph'.
push_k :: (Rate,Maybe Int,String,Float,Bool) -> Graph -> (Node,Graph)
push_k (r,ix,nm,d,tr) g =
    let n = NodeK (nextId g) r ix nm d (ktype r tr)
    in (n,g {controls = n : controls g
            ,nextId = nextId g + 1})

-- | Either find existing 'Control' 'Node', or insert a new 'Node'.
mk_node_k :: Control -> Graph -> (Node,Graph)
mk_node_k (Control r ix nm d tr) g =
    let y = find (find_k_p nm) (controls g)
    in maybe (push_k (r,ix,nm,d,tr) g) (\y' -> (y',g)) y

type UGenParts = (Rate,String,[FromPort],[Output],Special,UGenId)

-- | Predicate to locate primitive, names must be unique.
find_u_p :: UGenParts -> Node -> Bool
find_u_p (r,n,i,o,s,d) nd =
    case nd of
      NodeU _ r' n' i' o' s' d' ->
          r == r' && n == n' && i == i' && o == o' && s == s' && d == d'
      _ ->  error "find_u_p"

-- | Insert a /primitive/ 'NodeU' into the 'Graph'.
push_u :: UGenParts -> Graph -> (Node,Graph)
push_u (r,nm,i,o,s,d) g =
    let n = NodeU (nextId g) r nm i o s d
    in (n,g {ugens = n : ugens g
            ,nextId = nextId g + 1})

mk_node_u_acc :: [UGen] -> [Node] -> Graph -> ([Node],Graph)
mk_node_u_acc u n g =
    case u of
      [] -> (reverse n,g)
      x:xs -> let (y,g') = mk_node x g
              in mk_node_u_acc xs (y:n) g'

-- | Either find existing 'Primitive' node, or insert a new 'Node'.
mk_node_u :: Primitive -> Graph -> (Node,Graph)
mk_node_u (Primitive r nm i o s d) g =
    let (i',g') = mk_node_u_acc i [] g
        i'' = map as_from_port i'
        u = (r,nm,i'',o,s,d)
        y = find (find_u_p u) (ugens g')
    in maybe (push_u u g') (\y' -> (y',g')) y

-- | Proxies do not get stored in the graph.
mk_node_p :: Node -> PortIndex -> Graph -> (Node,Graph)
mk_node_p n p g =
    let z = nextId g
    in (NodeP z n p,g {nextId = z + 1})

-- | Transform 'UGen' into 'Graph', appending to existing 'Graph'.
mk_node :: UGen -> Graph -> (Node,Graph)
mk_node u g =
    case u of
      Constant_U c -> mk_node_c c g
      Control_U k -> mk_node_k k g
      Label_U _ -> error "mk_node: label"
      Primitive_U p -> mk_node_u p g
      Proxy_U p ->
          let (n,g') = mk_node_u (proxySource p) g
          in mk_node_p n (proxyIndex p) g'
      MRG_U m ->
          let (_,g') = mk_node (mrgRight m) g
          in mk_node (mrgLeft m) g'
      MCE_U _ -> error "mk_node: mce"

-- | Transform /mce/ nodes to /mrg/ nodes
prepare_root :: UGen -> UGen
prepare_root u =
    case u of
      MCE_U m -> mrg (mceProxies m)
      MRG_U m -> mrg2 (prepare_root (mrgLeft m)) (prepare_root (mrgRight m))
      _ -> u

-- | If controls have been given indices they must be coherent.
sort_controls :: [Node] -> [Node]
sort_controls c =
    let node_k_ix n = maybe maxBound id (node_k_index n)
        cmp = compare `on` node_k_ix
        c' = sortBy cmp c
        coheres z = maybe True (== z) . node_k_index
        coherent = all id (zipWith coheres [0..] c')
    in if coherent then c' else error (show ("sort_controls: incoherent",c))

-- | Variant on 'mk_node' starting with an empty graph, reverses the
-- 'UGen' list and sorts the 'Control' list, and adds implicit nodes.
mk_graph :: UGen -> Graph
mk_graph u =
    let (_,g) = mk_node (prepare_root u) empty_graph
        g' = g {ugens = reverse (ugens g)
               ,controls = sort_controls (controls g)}
    in add_implicit g'

-- * Encoding

type Map = M.IntMap Int

type Maps = (Map,[Node],Map,Map,[(KType,Int)])

data Input = Input Int Int
             deriving (Eq,Show)

-- | Determine 'KType' of a /control/ UGen at 'NodeU', or not.
node_ktype :: Node -> Maybe KType
node_ktype n =
    case (node_u_name n,node_u_rate n) of
      ("Control",IR) -> Just K_IR
      ("Control",KR) -> Just K_KR
      ("TrigControl",KR) -> Just K_TR
      ("AudioControl",AR) -> Just K_AR
      _ -> Nothing

-- | Map associating 'KType' with UGen index.
mk_ktype_map :: [Node] -> [(KType,Int)]
mk_ktype_map =
    let f (i,n) = let g ty = (ty,i) in fmap g (node_ktype n)
    in mapMaybe f . zip [0..]

-- | Lookup 'KType' index from map (erroring variant of 'lookup').
ktype_map_lookup :: KType -> [(KType,Int)] -> Int
ktype_map_lookup k =
    let e = error (show ("ktype_map_lookup",k))
    in fromMaybe e . lookup k

-- | Generate 'Maps' translating node identifiers to synthdef indexes.
mk_maps :: Graph -> Maps
mk_maps (Graph _ cs ks us) =
    (M.fromList (zip (map node_id cs) [0..])
    ,ks
    ,M.fromList (zip (map node_id ks) [0..])
    ,M.fromList (zip (map node_id us) [0..])
    ,mk_ktype_map us)

-- | Locate index in map given node identifer 'NodeId'.
fetch :: NodeId -> Map -> Int
fetch = M.findWithDefault (error "fetch")

-- | Controls are a special case.  We need to know not the overall
-- index but the index in relation to controls of the same type.
fetch_k :: NodeId -> KType -> [Node] -> Int
fetch_k z t =
    let rec i ns =
            case ns of
              [] -> error "fetch_k"
              n:ns' -> if z == node_id n
                       then i
                       else if t == node_k_type n
                            then rec (i + 1) ns'
                            else rec i ns'
    in rec 0

-- | Construct 'Input' form required by byte-code generator.
make_input :: Maps -> FromPort -> Input
make_input (cs,ks,_,us,kt) fp =
    case fp of
      FromPort_C n -> Input (-1) (fetch n cs)
      FromPort_K n t -> let i = ktype_map_lookup t kt
                        in Input i (fetch_k n t ks)
      FromPort_U n p -> Input (fetch n us) (fromMaybe 0 p)

-- | Byte-encode 'Input' value.
encode_input :: Input -> B.ByteString
encode_input (Input u p) = B.append (encode_i16 u) (encode_i16 p)

-- | Byte-encode 'NodeK' control node.
encode_node_k :: Maps -> Node -> B.ByteString
encode_node_k (_,_,ks,_,_) nd =
    case nd of
      NodeK n _ _ nm _ _ -> B.concat [B.pack (str_pstr nm)
                                     ,encode_i16 (fetch n ks)]
      _ -> error "encode_node_k"

-- | Byte-encode 'NodeU' primitive node.
encode_node_u :: Maps -> Node -> B.ByteString
encode_node_u m n =
    case n of
      NodeU _ r nm i o s _ ->
          let i' = map (encode_input . make_input m) i
              o' = map (encode_i8 . rateId) o
              (Special s') = s
          in B.concat [B.pack (str_pstr nm)
                      ,encode_i8 (rateId r)
                      ,encode_i16 (length i)
                      ,encode_i16 (length o)
                      ,encode_i16 s'
                      ,B.concat i'
                      ,B.concat o']
      _ -> error "encode_node_u: illegal input"

-- | Construct instrument definition bytecode.
encode_graphdef :: Graph -> B.ByteString
encode_graphdef g =
    let (Graph _ cs ks us) = g
        mm = mk_maps g
    in B.concat
           [encode_i16 (length cs)
           ,B.concat (map (encode_f32 . node_c_value) cs)
           ,encode_i16 (length ks)
           ,B.concat (map (encode_f32 . node_k_default) ks)
           ,encode_i16 (length ks)
           ,B.concat (map (encode_node_k mm) ks)
           ,encode_i16 (length us)
           ,B.concat (map (encode_node_u mm) us)]

-- * Implicit (Control, MaxLocalBuf)

-- | 4-tuple to count 'KType's.
type KS_COUNT = (Int,Int,Int,Int)

-- | Count the number of /controls/ of each 'KType'.
ks_count :: [Node] -> KS_COUNT
ks_count =
    let rec r ns =
            let (i,k,t,a) = r
            in case ns of
                 [] -> r
                 n:ns' -> let r' = case node_k_type n of
                                     K_IR -> (i+1,k,t,a)
                                     K_KR -> (i,k+1,t,a)
                                     K_TR -> (i,k,t+1,a)
                                     K_AR -> (i,k,t,a+1)
                          in rec r' ns'
    in rec (0,0,0,0)

-- | Construct implicit /control/ unit generator 'Nodes'.  Unit
-- generators are only constructed for instances of control types that
-- are present.
mk_implicit_ctl :: [Node] -> [Node]
mk_implicit_ctl ks =
    let (ni,nk,nt,na) = ks_count ks
        mk_n t n o =
            let (nm,r) = case t of
                            K_IR -> ("Control",IR)
                            K_KR -> ("Control",KR)
                            K_TR -> ("TrigControl",KR)
                            K_AR -> ("AudioControl",AR)
                i = replicate n r
            in if n == 0
               then Nothing
               else Just (NodeU (-1) r nm [] i (Special o) no_id)
    in catMaybes [mk_n K_IR ni 0
                 ,mk_n K_KR nk ni
                 ,mk_n K_TR nt (ni + nk)
                 ,mk_n K_AR na (ni + nk + nt)]

-- | Add implicit /control/ UGens to 'Graph'.
add_implicit_ctl :: Graph -> Graph
add_implicit_ctl g =
    let (Graph z cs ks us) = g
        ks' = sortBy node_k_cmp ks
        im = if null ks' then [] else mk_implicit_ctl ks'
        us' = im ++ us
    in Graph z cs ks' us'

-- | Zero if no local buffers, or if maxLocalBufs is given.
localbuf_count :: [Node] -> Int
localbuf_count us =
    case find ((==) "MaxLocalBufs" . node_u_name) us of
      Nothing -> length (filter ((==) "LocalBuf" . node_u_name) us)
      Just _ -> 0

-- | Add implicit 'maxLocalBufs' if not present.
add_implicit_buf :: Graph -> Graph
add_implicit_buf g =
    case localbuf_count (ugens g) of
      0 -> g
      n -> let (c,g') = mk_node_c (Constant (fromIntegral n)) g
               p = as_from_port c
               u = NodeU (-1) IR "MaxLocalBufs" [p] [] (Special 0) no_id
           in g' {ugens = u : ugens g'}

-- | 'add_implicit_buf' and 'add_implicit_ctl'.
add_implicit :: Graph -> Graph
add_implicit = add_implicit_buf . add_implicit_ctl

-- | Is 'Node' an /implicit/ control UGen?
is_implicit_control :: Node -> Bool
is_implicit_control n =
    let cs = ["AudioControl","Control","TrigControl"]
    in case n of
        NodeU x _ s _ _ _ _ -> x == -1 && s `elem` cs
        _ -> False

-- | Is Node implicit?
is_implicit :: Node -> Bool
is_implicit n = node_u_name n == "MaxLocalBufs" || is_implicit_control n

-- | Remove implicit UGens from 'Graph'
remove_implicit :: Graph -> Graph
remove_implicit g =
    let u = filter (not . is_implicit) (ugens g)
    in g {ugens = u}

-- * Queries

-- | Is 'FromPort' 'FromPort_U'.
is_from_port_u :: FromPort -> Bool
is_from_port_u p =
    case p of
      FromPort_U _ _ -> True
      _ -> False

-- | List of 'FromPort_U' at /e/ with multiple out edges.
multiple_u_out_edges :: [Edge] -> [FromPort]
multiple_u_out_edges e =
    let p = filter is_from_port_u (map fst e)
        p' = group (sortBy (compare `on` port_nid) p)
    in map head (filter ((> 1) . length) p')

-- | Descendents at 'Graph' of 'Node'.
node_descendents :: Graph -> Node -> [Node]
node_descendents g n =
    let e = edges (ugens g)
        c = filter ((== node_id n) . port_nid . fst) e
        f (ToPort k _) = k
    in mapMaybe (find_node g) (map (f . snd) c)

-- * PV edge accounting

-- | List @PV@ 'Node's at 'Graph' with multiple out edges.
pv_multiple_out_edges :: Graph -> [Node]
pv_multiple_out_edges g =
    let e = edges (ugens g)
        p = multiple_u_out_edges e
        n = mapMaybe (find_node g) (map port_nid p)
    in filter (primitive_is_pv_rate . node_u_name) n

-- | Error if graph has invalid @PV@ subgraph, ie. multiple out edges
-- at @PV@ node not connecting to @Unpack1FFT@ & @PackFFT@.
pv_validate :: Graph -> Graph
pv_validate g =
    case pv_multiple_out_edges g of
      [] -> g
      n -> let d = concatMap (map node_u_name . node_descendents g) n
           in if all (`elem` ["Unpack1FFT","PackFFT"]) d
              then g
              else error (show
                          ("pv_validate: multiple out edges, see pv_split"
                          ,map node_u_name n
                          ,d))
