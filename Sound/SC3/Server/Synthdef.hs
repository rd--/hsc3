-- | The unit-generator graph structure implemented by the
--   SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.IntMap as M {- containers -}
import Data.Function
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Coding.Byte {- hosc -}
import Sound.OpenSoundControl.Coding.Cast
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Rate
import System.FilePath {- filepath -}

-- * Data types

-- | Node identifier.
type NodeId = Int

-- | Port index.
type PortIndex = Int

-- | Type to represent unit generator graph.
data Graph = Graph {nextId :: NodeId
                   ,constants :: [Node]
                   ,controls :: [Node]
                   ,ugens :: [Node]}
            deriving (Eq,Show)

-- | Enumeration of the four operating rates for controls.
data KType = K_IR | K_KR | K_TR | K_AR
             deriving (Eq,Show,Ord)

-- | Type to represent the left hand side of an edge in a unit
--   generator graph.
data FromPort = FromPort_C {port_nid :: NodeId}
              | FromPort_K {port_nid :: NodeId,port_kt :: KType}
              | FromPort_U {port_nid :: NodeId,port_idx :: Maybe PortIndex}
                deriving (Eq,Show)

-- | A destination port.
data ToPort = ToPort NodeId PortIndex deriving (Eq,Show)

-- | A connection from 'FromPort' to 'ToPort'.
type Edge = (FromPort,ToPort)

-- | Type to represent nodes in unit generator graph.
data Node = NodeC {node_id :: NodeId
                  ,node_c_value :: Double}
          | NodeK {node_id :: NodeId
                  ,node_k_rate :: Rate
                  ,node_k_name :: String
                  ,node_k_default :: Double
                  ,node_k_type :: KType}
          | NodeU {node_id :: NodeId
                  ,node_u_rate :: Rate
                  ,node_u_name :: String
                  ,node_u_inputs :: [FromPort]
                  ,node_u_outputs :: [Output]
                  ,node_u_special :: Special
                  ,node_u_ugenid :: UGenId}
          | NodeP {node_id :: NodeId
                  ,node_p_node :: Node
                  ,node_p_index :: PortIndex}
            deriving (Eq,Show)

-- | Binary representation of a unit generator graph.
type Graphdef = B.ByteString

-- | Binary representation of a unit generator synth definition.
data Synthdef = Synthdef {synthdefName :: String
                         ,synthdefGraph :: Graph}
                deriving (Eq,Show)

-- * User functions

-- | Find the maximum 'NodeId' used at 'Graph'.
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

-- | Remove implicit /control/ UGens from 'Graph'
remove_implicit :: Graph -> Graph
remove_implicit g =
    let u = filter (not . is_implicit_control) (ugens g)
    in g {ugens = u}

-- | Add implicit /control/ UGens to 'Graph'
add_implicit :: Graph -> Graph
add_implicit g =
    let (Graph _ cs ks us) = g
        ks' = sortBy node_k_cmp ks
        im = if null ks' then [] else mk_implicit ks'
        us' = im ++ us
    in Graph (-1) cs ks' us'

-- | Transform a unit generator into a graph.
synth :: UGen -> Graph
synth u =
    let (_,g) = mk_node (prepare_root u) empty_graph
        g' = g {ugens = reverse (ugens g)}
    in add_implicit g'

-- | Transform a unit generator graph into bytecode.
graphdef :: Graph -> Graphdef
graphdef = encode_graphdef

-- | Encode 'Synthdef' as a binary data stream.
synthdefData :: Synthdef -> B.ByteString
synthdefData (Synthdef s g) =
    B.concat [encode_str "SCgf"
             ,encode_i32 0
             ,encode_i16 1
             ,B.pack (str_pstr s)
             ,encode_graphdef g]

-- | Transform a unit generator synth definition into bytecode.
synthdef :: String -> UGen -> Synthdef
synthdef s u = Synthdef s (synth u)

-- | Write 'Synthdef' to indicated directory.  The filename is the
-- 'synthdefName' with the appropriate extension (@scsyndef@).
synthdefWrite :: Synthdef -> FilePath -> IO ()
synthdefWrite s dir =
    let nm = dir </> synthdefName s <.> "scsyndef"
    in B.writeFile nm (synthdefData s)

-- | Simple statistical analysis of a unit generator graph.
synthstat :: UGen -> String
synthstat u =
    let s = synth u
        cs = constants s
        ks = controls s
        us = ugens s
        f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "synthstat"
              in show . map h . group . sort . map g
    in unlines ["number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"control rates             : " ++ f node_k_rate ks
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f node_u_rate us]

-- | Find 'Node' with indicated 'NodeId'.
find_node :: Graph -> NodeId -> Maybe Node
find_node (Graph _ cs ks us) n =
    let f x = node_id x == n
    in find f (cs ++ ks ++ us)

-- | Is 'Node' an /implicit/ control UGen?
is_implicit_control :: Node -> Bool
is_implicit_control n =
    let cs = ["AudioControl","Control","TrigControl"]
    in case n of
        NodeU x _ s _ _ _ _ -> x == -1 && s `elem` cs
        _ -> False

-- | Generate a label for 'Node' using the /type/ and the 'node_id'.
node_label :: Node -> String
node_label nd =
    case nd of
      NodeC n _ -> "c_" ++ show n
      NodeK n _ _ _ _ -> "k_" ++ show n
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
      NodeK _ _ _ _ _ -> True
      _ -> False

-- | Is 'Node' a /UGen/.
is_node_u :: Node -> Bool
is_node_u n =
    case n of
      NodeU _ _ _ _ _ _ _-> True
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
      NodeK n _ _ _ t -> FromPort_K n t
      NodeU n _ _ _ _ _ _ -> FromPort_U n Nothing
      NodeP _ u p -> FromPort_U (node_id u) (Just p)

-- | Locate 'Node' of 'FromPort' in 'Graph'.
from_port_node :: Graph -> FromPort -> Maybe Node
from_port_node g fp = find_node g (port_nid fp)

-- | The empty 'Graph'.
empty_graph :: Graph
empty_graph = Graph 0 [] [] []

-- * Internal functions

-- | Predicate to determine if 'Node' is a constant with indicated /value/.
find_c_p :: Double -> Node -> Bool
find_c_p x n =
    case n of
      NodeC _ y -> x == y
      _ -> error "find_c_p: non NodeC"

-- | Insert a constant 'Node' into the 'Graph'.
push_c :: Double -> Graph -> (Node,Graph)
push_c x g =
    let n = NodeC (nextId g) x
    in (n,g {constants = n : constants g
            ,nextId = nextId g + 1})

-- | Either find existing 'Constant' 'Node', or insert a new 'Node'.
mk_node_c :: UGen -> Graph -> (Node,Graph)
mk_node_c u g =
    case u of
      Constant x ->
          let y = find (find_c_p x) (constants g)
          in maybe (push_c x g) (\y' -> (y',g)) y
      _ -> error "mk_node_c"

-- | Predicate to determine if 'Node' is a control with indicated
-- /name/.  Names must be unique.
find_k_p :: String -> Node -> Bool
find_k_p x n =
    case n of
      NodeK _ _ y _ _ -> x == y
      _ -> error "find_k_p"

-- | Insert a control node into the 'Graph'.
push_k :: (Rate,String,Double,Bool) -> Graph -> (Node,Graph)
push_k (r,nm,d,tr) g =
    let n = NodeK (nextId g) r nm d (ktype r tr)
    in (n,g {controls = n : controls g
            ,nextId = nextId g + 1})

-- | Either find existing 'Control' 'Node', or insert a new 'Node'.
mk_node_k :: UGen -> Graph -> (Node,Graph)
mk_node_k u g =
    case u of
      Control r nm d tr ->
          let y = find (find_k_p nm) (controls g)
          in maybe (push_k (r,nm,d,tr) g) (\y' -> (y',g)) y
      _ -> error "mk_node_k"

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
mk_node_u :: UGen -> Graph -> (Node,Graph)
mk_node_u ug g =
    case ug of
      Primitive r nm i o s d ->
          let (i',g') = mk_node_u_acc i [] g
              i'' = map as_from_port i'
              u = (r,nm,i'',o,s,d)
              y = find (find_u_p u) (ugens g')
          in maybe (push_u u g') (\y' -> (y',g')) y
      _ -> error "mk_node_u"

-- | Proxies do not get stored in the graph.
mk_node_p :: Node -> PortIndex -> Graph -> (Node,Graph)
mk_node_p n p g =
    let z = nextId g
    in (NodeP z n p,g {nextId = z + 1})

mk_node :: UGen -> Graph -> (Node,Graph)
mk_node u g =
    case ugenType u of
      Constant_U -> mk_node_c u g
      Control_U -> mk_node_k u g
      Label_U -> error "mk_node: label"
      Primitive_U -> mk_node_u u g
      Proxy_U ->
          let (n,g') = mk_node_u (proxySource u) g
          in mk_node_p n (proxyIndex u) g'
      MRG_U ->
          let (_,g') = mk_node (mrgRight u) g
          in mk_node (mrgLeft u) g'
      MCE_U -> error "mk_node: mce"

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
      NodeK n _ nm _ _ -> B.concat [B.pack (str_pstr nm)
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
    in B.concat [encode_i16 (length cs)
                ,B.concat (map (encode_f32 . node_c_value) cs)
                ,encode_i16 (length ks)
                ,B.concat (map (encode_f32 . node_k_default) ks)
                ,encode_i16 (length ks)
                ,B.concat (map (encode_node_k mm) ks)
                ,encode_i16 (length us)
                ,B.concat (map (encode_node_u mm) us)]

-- | 4-tuple to count 'KType's.
type KS_COUNT = (Int,Int,Int,Int)

-- | Count the number of /controls/ if each 'KType'.
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
mk_implicit :: [Node] -> [Node]
mk_implicit ks =
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

-- | Transform /mce/ nodes to /mrg/ nodes
prepare_root :: UGen -> UGen
prepare_root u =
    case ugenType u of
      MCE_U -> mrg (mceProxies u)
      MRG_U -> MRG (prepare_root (mrgLeft u)) (prepare_root (mrgRight u))
      _ -> u
