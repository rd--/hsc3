module Sound.SC3.Server.Synthdef ( Node(..), FromPort(..), Graph(..)
                                  , synth, synthdef ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Char as C
import qualified Data.IntMap as M
import Data.List
import Data.Word
import Sound.OpenSoundControl
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
            deriving (Eq, Show)

-- | Type to represent nodes in unit generator graph.
data Node = NodeC { node_id :: NodeId
                  , node_c_value :: Double }
          | NodeK { node_id :: NodeId
                  , node_k_rate :: Rate
                  , node_k_name :: String
                  , node_k_default :: Double }
          | NodeU { node_id :: NodeId
                  , node_u_rate :: Rate
                  , node_u_name :: String
                  , node_u_inputs :: [FromPort]
                  , node_u_outputs :: [Output]
                  , node_u_special :: Special
                  , node_u_ugenid :: Maybe UGenId }
          | NodeP { node_id :: NodeId 
                  , node_p_node :: Node
                  , node_p_index :: PortIndex }
            deriving (Eq, Show)

-- | Type to represent the left hand side of an edge in a unit
--   generator graph.
data FromPort = C NodeId
              | K NodeId
              | U NodeId PortIndex
                deriving (Eq, Show)

-- | Transform a unit generator into a graph.
synth :: UGen -> Graph
synth u = let (_, g) = mk_node (prepare_root u) empty_graph
              (Graph _ cs ks us) = g
              us' = if null ks 
                    then reverse us
                    else implicit (length ks) : reverse us
          in Graph (-1) cs ks us'

-- | Transform a unit generator into bytecode.
synthdef :: String -> UGen -> [Word8]
synthdef s u = B.unpack (encode_graphdef s (synth u))

as_from_port :: Node -> FromPort
as_from_port (NodeC n _) = C n
as_from_port (NodeK n _ _ _) = K n
as_from_port (NodeU n _ _ _ _ _ _) = U n 0
as_from_port (NodeP _ u p) = U (node_id u) p

-- The empty graph.
empty_graph :: Graph
empty_graph = Graph 0 [] [] []

-- Predicate to locate constant.
find_c_p :: Double -> Node -> Bool
find_c_p x (NodeC _ y) = x == y
find_c_p _ _ = error "find_c_p"

-- Insert a constant node into the graph.
push_c :: Double -> Graph -> (Node, Graph)
push_c x g = let n = NodeC (nextId g) x
             in (n, g { constants = n : constants g
                      , nextId = nextId g + 1 })

-- Either find existing constant node, or insert a new node.
mk_node_c :: UGen -> Graph -> (Node, Graph)
mk_node_c (Constant x) g =
    let y = find (find_c_p x) (constants g)
    in maybe (push_c x g) (\y' -> (y', g)) y
mk_node_c _ _ = error "mk_node_c"

-- Predicate to locate control, names must be unique.
find_k_p :: String -> Node -> Bool
find_k_p x (NodeK _ _ y _) = x == y
find_k_p _ _ = error "find_k_p"

-- Insert a control node into the graph.
push_k :: (Rate, String, Double) -> Graph -> (Node, Graph)
push_k (r, nm, d) g =
    let n = NodeK (nextId g) r nm d
    in (n, g { controls = n : controls g
             , nextId = nextId g + 1 })

-- Either find existing control node, or insert a new node.
mk_node_k :: UGen -> Graph -> (Node, Graph)
mk_node_k (Control r nm d) g =
    let y = find (find_k_p nm) (controls g)
    in maybe (push_k (r, nm, d) g) (\y' -> (y', g)) y
mk_node_k _ _ = error "mk_node_k"

acc :: [UGen] -> [Node] -> Graph -> ([Node], Graph)
acc [] n g = (reverse n, g)
acc (x:xs) ys g = let (y, g') = mk_node x g
                  in acc xs (y:ys) g'

type UGenParts = (Rate, String, [FromPort], [Output], Special, Maybe UGenId)

-- Predicate to locate primitive, names must be unique.
find_u_p :: UGenParts -> Node -> Bool
find_u_p (r, n, i, o, s, d) (NodeU _ r' n' i' o' s' d')
    = r == r' && n == n' && i == i' && o == o' && s == s' && d == d'
find_u_p _ _ = error "find_u_p"

-- Insert a primitive node into the graph.
push_u :: UGenParts -> Graph -> (Node, Graph)
push_u (r, nm, i, o, s, d) g =
    let n = NodeU (nextId g) r nm i o s d
    in (n, g { ugens = n : ugens g
             , nextId = nextId g + 1 })

-- Either find existing control node, or insert a new node.
mk_node_u :: UGen -> Graph -> (Node, Graph)
mk_node_u (Primitive r nm i o s d) g =
    let (i', g') = acc i [] g
        i'' = map as_from_port i'
        u = (r, nm, i'', o, s, d)
        y = find (find_u_p u) (ugens g')
    in maybe (push_u u g') (\y' -> (y', g')) y
mk_node_u _ _ = error "mk_node_u"

-- Proxies do not get stored in the graph.
mk_node_p :: Node -> PortIndex -> Graph -> (Node, Graph)
mk_node_p n p g = let z = nextId g
                  in (NodeP z n p, g { nextId = z + 1 })

mk_node :: UGen -> Graph -> (Node, Graph)
mk_node u g 
    | isConstant u = mk_node_c u g
    | isControl u = mk_node_k u g
    | isUGen u = mk_node_u u g
    | isProxy u = let (n, g') = mk_node_u (proxySource u) g
                  in mk_node_p n (proxyIndex u) g'
    | isMRG u = let (_, g') = mk_node (mrgRight u) g
                in mk_node (mrgLeft u) g'
    | isMCE u = error "mk_node: mce"
    | otherwise = error "mk_node"

type Map = M.IntMap Int
type Maps = (Map, Map, Map)

-- Generate maps from node identifiers to synthdef indexes.
mk_maps :: Graph -> Maps
mk_maps (Graph _ cs ks us) = 
    ( M.fromList (zip (map node_id cs) [0..])
    , M.fromList (zip (map node_id ks) [0..])
    , M.fromList (zip (map node_id us) [0..]) )

-- Locate index in map give node identifer.
fetch :: NodeId -> Map -> Int
fetch = M.findWithDefault (error "fetch")

data Input = Input Int Int
             deriving (Eq, Show)

-- Construct input form required by byte-code generator.
make_input :: Maps -> FromPort -> Input
make_input (cs, _, _) (C n) = Input (-1) (fetch n cs)
make_input (_, ks, _) (K n) = Input 0 (fetch n ks)
make_input (_, _, us) (U n p) = Input (fetch n us) p

-- Byte-encode input value.
encode_input :: Input -> B.ByteString
encode_input (Input u p) = B.append (encode_i16 u) (encode_i16 p)

-- Byte-encode control node.
encode_node_k :: Maps -> Node -> B.ByteString
encode_node_k (_, ks, _) (NodeK n _ nm _) =
    B.concat [ B.pack (str_pstr nm)
             , encode_i16 (fetch n ks) ]
encode_node_k _ _ = error "encode_node_k"

-- Byte-encode primitive node.
encode_node_u :: Maps -> Node -> B.ByteString
encode_node_u m (NodeU _ r nm i o s _) =
    B.concat [ B.pack (str_pstr nm)
             , encode_i8 (rateId r)
             , encode_i16 (length i)
             , encode_i16 (length o)
             , encode_i16 s'
             , B.concat i'
             , B.concat o' ]
    where i' = map (encode_input . make_input m) i
          o' = map (encode_i8 . rateId) o
          (Special s') = s
encode_node_u _ _ = error "encode_ugen: illegal input"

-- Construct instrument definition bytecode.
encode_graphdef :: String -> Graph -> B.ByteString
encode_graphdef s g =
    B.concat [ encode_str "SCgf"
             , encode_i32 0
             , encode_i16 1
             , B.pack (str_pstr s)
             , encode_i16 (length cs)
             , B.concat (map (encode_f32 . node_c_value) cs)
             , encode_i16 (length ks)
             , B.concat (map (encode_f32 . node_k_default) ks)
             , encode_i16 (length ks)
             , B.concat (map (encode_node_k mm) ks)
             , encode_i16 (length us)
             , B.concat (map (encode_node_u mm) us) ]
    where (Graph _ cs ks us) = g
          mm = mk_maps g

-- Construct implicit control unit generator node (k-rate only).
implicit :: Int -> Node
implicit n = NodeU (-1) KR "Control" [] (replicate n KR) (Special 0) Nothing

-- Transform mce nodes to mrg nodes
prepare_root :: UGen -> UGen
prepare_root u 
    | isMCE u = mrg (mceProxies u)
    | isMRG u = MRG (prepare_root (mrgLeft u)) (prepare_root (mrgRight u))
    | otherwise = u
