module Sound.SC3.Server.Synthdef (synth, synthdef) where

import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as M
import Data.List
import Data.Word
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.Cast
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Predicate
import Sound.SC3.UGen.Rate

type NodeId = Int
type PortIndex = Int

data FromPort = C NodeId
              | K NodeId
              | U NodeId PortIndex
                deriving (Eq, Show)

{-
data ToPort = ToPort NodeId PortIndex
              deriving (Eq, Show)
-}

data Input = Input Int Int
             deriving (Eq, Show)

data Node = NodeC NodeId Double
          | NodeK NodeId Rate Name Double
          | NodeU NodeId Rate Name [FromPort] [Output] Special (Maybe UGenId)
          | NodeP NodeId Node PortIndex
            deriving (Eq, Show)

data Graph = Graph { nextId :: NodeId
                   , constants :: [Node]
                   , controls :: [Node]
                   , ugens :: [Node] }
            deriving (Eq, Show)

type Map = M.IntMap Int
type Maps = (Map, Map, Map)

node_id :: Node -> NodeId
node_id (NodeC n _) = n
node_id (NodeK n _ _ _) = n
node_id (NodeU n _ _ _ _ _ _) = n
node_id (NodeP n _ _) = n

as_from_port :: Node -> FromPort
as_from_port (NodeC n _) = C n
as_from_port (NodeK n _ _ _) = K n
as_from_port (NodeU n _ _ _ _ _ _) = U n 0
as_from_port (NodeP _ u p) = U (node_id u) p

empty_graph :: Graph
empty_graph = Graph 0 [] [] []

find_c_p :: Double -> Node -> Bool
find_c_p x (NodeC _ y) = x == y
find_c_p _ _ = error "find_c_p"

push_c :: Double -> Graph -> (Node, Graph)
push_c x g = let n = NodeC (nextId g) x
             in (n, g { constants = n : constants g
                      , nextId = nextId g + 1 })

mk_node_c :: UGen -> Graph -> (Node, Graph)
mk_node_c (Constant x) g =
    let y = find (find_c_p x) (constants g)
    in maybe (push_c x g) (\y' -> (y', g)) y
mk_node_c _ _ = error "mk_node_c"

-- names must be unique
find_k_p :: Name -> Node -> Bool
find_k_p x (NodeK _ _ y _) = x == y
find_k_p _ _ = error "find_k_p"

push_k :: (Rate, Name, Double) -> Graph -> (Node, Graph)
push_k (r, nm, d) g =
    let n = NodeK (nextId g) r nm d
    in (n, g { controls = n : controls g
             , nextId = nextId g + 1 })

mk_node_k :: UGen -> Graph -> (Node, Graph)
mk_node_k (Control r nm d) g =
    let y = find (find_k_p nm) (controls g)
    in maybe (push_k (r, nm, d) g) (\y' -> (y', g)) y
mk_node_k _ _ = error "mk_node_k"

acc :: [UGen] -> [Node] -> Graph -> ([Node], Graph)
acc [] n g = (reverse n, g)
acc (x:xs) ys g = let (y, g') = mk_node x g
                  in acc xs (y:ys) g'

find_u_p :: (Rate, Name, [FromPort], [Output], Special, Maybe UGenId) -> Node -> Bool
find_u_p (r, n, i, o, s, d) (NodeU _ r' n' i' o' s' d')
    = r == r' && n == n' && i == i' && o == o' && s == s' && d == d'
find_u_p _ _ = error "find_u_p"

push_u :: (Rate, Name, [FromPort], [Output], Special, Maybe UGenId) -> Graph -> (Node, Graph)
push_u (r, nm, i, o, s, d) g =
    let n = NodeU (nextId g) r nm i o s d
    in (n, g { ugens = n : ugens g
             , nextId = nextId g + 1 })

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
mk_node u g | isConstant u = mk_node_c u g
           | isControl u = mk_node_k u g
           | isUGen u = mk_node_u u g
           | isProxy u = let (n, g') = mk_node_u (proxySource u) g
                         in mk_node_p n (proxyIndex u) g'
           | isMRG u = let (_, g') = mk_node (mrgRight u) g
                       in mk_node (mrgLeft u) g'
           | otherwise = error ("mk_node" ++ show u)

mk_maps :: Graph -> Maps
mk_maps (Graph _ cs ks us) = ( M.fromList (zip (map node_id cs) [0..])
                             , M.fromList (zip (map node_id ks) [0..])
                             , M.fromList (zip (map node_id (us)) [0..]) )

fetch :: NodeId -> Map -> Int
fetch = M.findWithDefault (error "fetch")

make_input :: Maps -> FromPort -> Input
make_input (cs, _, _) (C n) = Input (-1) (fetch n cs)
make_input (_, ks, _) (K n) = Input 0 (fetch n ks)
make_input (_, _, us) (U n p) = Input (fetch n us) p

-- | Byte-encode Input value.
encode_input :: Input -> B.ByteString
encode_input (Input u p) = B.append (encode_i16 u) (encode_i16 p)

-- | Byte-encode Control value.
encode_node_k :: Maps -> Node -> B.ByteString
encode_node_k (_, ks, _) (NodeK n _ nm _) =
    B.concat [ B.pack (str_pstr nm)
             , encode_i16 (fetch n ks) ]
encode_node_k _ _ = error "encode_node_k"

-- | Byte-encode UGen value.
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

node_c_value :: Node -> Double
node_c_value (NodeC _ x) = x
node_c_value _ = error "node_c_value"

node_k_default :: Node -> Double
node_k_default (NodeK _ _ _ x) = x
node_k_default _ = error "node_k_default"

-- | Construct instrument definition bytecode.
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

synth :: UGen -> Graph
synth u = let (_, g) = mk_node u empty_graph
              (Graph _ cs ks us) = g
          in Graph (-1) cs ks (reverse us)

synthdef :: String -> UGen -> [Word8]
synthdef s u = B.unpack (encode_graphdef s (synth u))

{-
type Edges = [(FromPort, ToPort)]

edges :: Graph -> Edges
edges g = concatMap f (ugens g)
    where f (NodeU x _ _ i _ _ _) = zip i (map (\n -> ToPort x n) [0..])
-}
