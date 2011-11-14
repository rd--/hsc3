-- | The unit-generator graph structure implemented by the
--   SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef (Node(..),FromPort(..)
                                 ,Graph(..),Graphdef,graphdef
                                 ,Synthdef(..),synthdefData,synth,synthdef
                                 ,synthstat) where

import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as M
import Data.List
import Sound.OpenSoundControl.Coding.Byte
import Sound.OpenSoundControl.Coding.Cast
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Rate

-- | Node identifier.
type NodeId = Int

-- | Port index.
type PortIndex = Int

-- | Type to represent unit generator graph.
data Graph = Graph { nextId :: NodeId
                   , constants :: [Node]
                   , controls :: [Node]
                   , ugens :: [Node] }
            deriving (Eq,Show)

-- | Type to represent nodes in unit generator graph.
data Node = NodeC { node_id :: NodeId
                  , node_c_value :: Double }
          | NodeK { node_id :: NodeId
                  , node_k_rate :: Rate
                  , node_k_name :: String
                  , node_k_default :: Double
                  , node_k_type :: KType }
          | NodeU { node_id :: NodeId
                  , node_u_rate :: Rate
                  , node_u_name :: String
                  , node_u_inputs :: [FromPort]
                  , node_u_outputs :: [Output]
                  , node_u_special :: Special
                  , node_u_ugenid :: UGenId }
          | NodeP { node_id :: NodeId
                  , node_p_node :: Node
                  , node_p_index :: PortIndex }
            deriving (Eq, Show)

-- There are four classes of controls.
data KType = K_IR | K_KR | K_TR | K_AR
             deriving (Eq, Show, Ord)

node_k_cmp :: Node -> Node -> Ordering
node_k_cmp p q = compare (node_k_type p) (node_k_type q)

-- Determine class of control given rate and name.
ktype :: Rate -> Bool -> KType
ktype r tr =
    if tr
    then case r of
           KR -> K_TR
           _ -> error "ktype"
    else case r of
           IR -> K_IR
           KR -> K_KR
           AR -> K_AR
           DR -> error "ktype"

-- | Type to represent the left hand side of an edge in a unit
--   generator graph.
data FromPort = FromPort_C {port_nid :: NodeId}
              | FromPort_K {port_nid :: NodeId,port_kt :: KType}
              | FromPort_U {port_nid :: NodeId,port_idx :: PortIndex}
                deriving (Eq, Show)

-- | Transform a unit generator into a graph.
synth :: UGen -> Graph
synth u =
    let (_, g) = mk_node (prepare_root u) empty_graph
        (Graph _ cs ks us) = g
        ks' = sortBy node_k_cmp ks
        us' = if null ks'
              then reverse us
              else implicit ks' ++ reverse us
    in Graph (-1) cs ks' us'

-- | Binary representation of a unit generator graph.
type Graphdef = B.ByteString

-- | Transform a unit generator graph into bytecode.
graphdef :: Graph -> Graphdef
graphdef = encode_graphdef

-- | Binary representation of a unit generator synth definition.
data Synthdef = Synthdef {synthdefName :: String
                         ,synthdefGraph :: Graph}
                deriving (Eq,Show)

-- | Encode 'Synthdef' as binary data stream.
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

-- | Simple statistical analysis of a unit generator graph.
synthstat :: UGen -> String
synthstat u =
    let s = synth u
        cs = constants s
        ks = controls s
        us = ugens s
        f g = let h (x:xs) = (x, length (x:xs))
                  h [] = error "synthstat"
              in show . map h . group . sort . map g
    in unlines ["number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"control rates             : " ++ f node_k_rate ks
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f node_u_rate us]

as_from_port :: Node -> FromPort
as_from_port d =
    case d of
      NodeC n _ -> FromPort_C n
      NodeK n _ _ _ t -> FromPort_K n t
      NodeU n _ _ _ _ _ _ -> FromPort_U n 0
      NodeP _ u p -> FromPort_U (node_id u) p

-- The empty graph.
empty_graph :: Graph
empty_graph = Graph 0 [] [] []

-- Predicate to locate constant.
find_c_p :: Double -> Node -> Bool
find_c_p x n =
    case n of
      NodeC _ y -> x == y
      _ -> error "find_c_p"

-- Insert a constant node into the graph.
push_c :: Double -> Graph -> (Node, Graph)
push_c x g =
    let n = NodeC (nextId g) x
    in (n, g { constants = n : constants g
             , nextId = nextId g + 1 })

-- Either find existing constant node, or insert a new node.
mk_node_c :: UGen -> Graph -> (Node, Graph)
mk_node_c u g =
    case u of
      Constant x ->
          let y = find (find_c_p x) (constants g)
          in maybe (push_c x g) (\y' -> (y', g)) y
      _ -> error "mk_node_c"

-- Predicate to locate control, names must be unique.
find_k_p :: String -> Node -> Bool
find_k_p x n =
    case n of
      NodeK _ _ y _ _ -> x == y
      _ -> error "find_k_p"

-- Insert a control node into the graph.
push_k :: (Rate, String, Double, Bool) -> Graph -> (Node, Graph)
push_k (r, nm, d, tr) g =
    let n = NodeK (nextId g) r nm d (ktype r tr)
    in (n, g { controls = n : controls g
             , nextId = nextId g + 1 })

-- Either find existing control node, or insert a new node.
mk_node_k :: UGen -> Graph -> (Node, Graph)
mk_node_k u g =
    case u of
      Control r nm d tr ->
          let y = find (find_k_p nm) (controls g)
          in maybe (push_k (r, nm, d, tr) g) (\y' -> (y', g)) y
      _ -> error "mk_node_k"

acc :: [UGen] -> [Node] -> Graph -> ([Node], Graph)
acc [] n g = (reverse n, g)
acc (x:xs) ys g =
    let (y, g') = mk_node x g
    in acc xs (y:ys) g'

type UGenParts = (Rate, String, [FromPort], [Output], Special, UGenId)

-- Predicate to locate primitive, names must be unique.
find_u_p :: UGenParts -> Node -> Bool
find_u_p (r, n, i, o, s, d) nd =
    case nd of
      NodeU _ r' n' i' o' s' d' ->
          r == r' && n == n' && i == i' && o == o' && s == s' && d == d'
      _ ->  error "find_u_p"

-- Insert a primitive node into the graph.
push_u :: UGenParts -> Graph -> (Node, Graph)
push_u (r, nm, i, o, s, d) g =
    let n = NodeU (nextId g) r nm i o s d
    in (n, g { ugens = n : ugens g
             , nextId = nextId g + 1 })

-- Either find existing control node, or insert a new node.
mk_node_u :: UGen -> Graph -> (Node, Graph)
mk_node_u ug g =
    case ug of
      Primitive r nm i o s d ->
          let (i', g') = acc i [] g
              i'' = map as_from_port i'
              u = (r, nm, i'', o, s, d)
              y = find (find_u_p u) (ugens g')
          in maybe (push_u u g') (\y' -> (y', g')) y
      _ -> error "mk_node_u"

-- Proxies do not get stored in the graph.
mk_node_p :: Node -> PortIndex -> Graph -> (Node, Graph)
mk_node_p n p g =
    let z = nextId g
    in (NodeP z n p, g { nextId = z + 1 })

mk_node :: UGen -> Graph -> (Node, Graph)
mk_node u g =
    case ugenType u of
      Constant_U -> mk_node_c u g
      Control_U -> mk_node_k u g
      Primitive_U -> mk_node_u u g
      Proxy_U ->
          let (n, g') = mk_node_u (proxySource u) g
          in mk_node_p n (proxyIndex u) g'
      MRG_U ->
          let (_, g') = mk_node (mrgRight u) g
          in mk_node (mrgLeft u) g'
      MCE_U -> error "mk_node: mce"

type Map = M.IntMap Int
type Maps = (Map, [Node], Map, Map)

-- Generate maps from node identifiers to synthdef indexes.
mk_maps :: Graph -> Maps
mk_maps (Graph _ cs ks us) =
    ( M.fromList (zip (map node_id cs) [0..])
    , ks
    , M.fromList (zip (map node_id ks) [0..])
    , M.fromList (zip (map node_id us) [0..]) )

-- Locate index in map given node identifer.
fetch :: NodeId -> Map -> Int
fetch = M.findWithDefault (error "fetch")

data Input = Input Int Int
             deriving (Eq, Show)

-- For controls we need to know not the overall index
-- but in relation to controls of the same type.
fetch_k :: NodeId -> KType -> [Node] -> Int
fetch_k n t ks =
    let f _ [] = error "fetch_k"
        f i (x:xs) =
            if n == node_id x
            then i
            else if t == node_k_type x
                 then f (i + 1) xs
                 else f i xs
    in f 0 ks

-- Construct input form required by byte-code generator.
make_input :: Maps -> FromPort -> Input
make_input (cs, ks, _, us) fp =
    case fp of
      FromPort_C n -> Input (-1) (fetch n cs)
      FromPort_K n t -> let i = case t of
                                  K_IR -> 0
                                  K_KR -> 1
                                  K_TR -> 2
                                  K_AR -> 3
                        in Input i (fetch_k n t ks)
      FromPort_U n p -> Input (fetch n us) p

-- Byte-encode input value.
encode_input :: Input -> B.ByteString
encode_input (Input u p) = B.append (encode_i16 u) (encode_i16 p)

-- Byte-encode control node.
encode_node_k :: Maps -> Node -> B.ByteString
encode_node_k (_, _, ks, _) nd =
    case nd of
      NodeK n _ nm _ _ -> B.concat [ B.pack (str_pstr nm)
                                   , encode_i16 (fetch n ks) ]
      _ -> error "encode_node_k"

-- Byte-encode primitive node.
encode_node_u :: Maps -> Node -> B.ByteString
encode_node_u m n =
    case n of
      NodeU _ r nm i o s _ ->
          let i' = map (encode_input . make_input m) i
              o' = map (encode_i8 . rateId) o
              (Special s') = s
          in B.concat [ B.pack (str_pstr nm)
                      , encode_i8 (rateId r)
                      , encode_i16 (length i)
                      , encode_i16 (length o)
                      , encode_i16 s'
                      , B.concat i'
                      , B.concat o' ]
      _ -> error "encode_node_u: illegal input"

-- Construct instrument definition bytecode.
encode_graphdef :: Graph -> B.ByteString
encode_graphdef g =
    let (Graph _ cs ks us) = g
        mm = mk_maps g
    in B.concat [ encode_i16 (length cs)
                , B.concat (map (encode_f32 . node_c_value) cs)
                , encode_i16 (length ks)
                , B.concat (map (encode_f32 . node_k_default) ks)
                , encode_i16 (length ks)
                , B.concat (map (encode_node_k mm) ks)
                , encode_i16 (length us)
                , B.concat (map (encode_node_u mm) us) ]

type KS_COUNT = (Int,Int,Int,Int)

ks_count :: [Node] -> KS_COUNT
ks_count ks =
    let f r [] = r
        f (i,k,t,a) (x:xs) =
            let r' = case node_k_type x of
                       K_IR -> (i+1,k,t,a)
                       K_KR -> (i,k+1,t,a)
                       K_TR -> (i,k,t+1,a)
                       K_AR -> (i,k,t,a+1)
            in f r' xs
    in f (0,0,0,0) ks

-- Construct implicit control unit generator nodes.
implicit :: [Node] -> [Node]
implicit ks =
    let (ni,nk,nt,na) = ks_count ks
        mk_n t n o =
            let (nm, r) = case t of
                            K_IR -> ("Control", IR)
                            K_KR -> ("Control", KR)
                            K_TR -> ("TrigControl", KR)
                            K_AR -> ("AudioControl", AR)
                i = replicate n r
            in NodeU (-1) r nm [] i (Special o) NoId
    in [mk_n K_IR ni 0
       ,mk_n K_KR nk ni
       ,mk_n K_TR nt (ni + nk)
       ,mk_n K_AR na (ni + nk + nt)]

-- Transform mce nodes to mrg nodes
prepare_root :: UGen -> UGen
prepare_root u =
    case ugenType u of
      MCE_U -> mrg (mceProxies u)
      MRG_U -> MRG (prepare_root (mrgLeft u)) (prepare_root (mrgRight u))
      _ -> u
