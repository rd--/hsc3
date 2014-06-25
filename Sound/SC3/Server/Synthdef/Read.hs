-- | Decode a binary 'Graph Definition'.
module Sound.SC3.Server.Synthdef.Read where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import System.IO {- base -}

import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Type {- hosc -}

import qualified Sound.SC3.Server.Synthdef as S
import qualified Sound.SC3.Server.Synthdef.Type as G
import qualified Sound.SC3.UGen.Rate as R
import qualified Sound.SC3.UGen.Type as U

read_i8 :: Handle -> IO Int
read_i8 h = fmap decode_i8 (L.hGet h 1)

read_i16 :: Handle -> IO Int
read_i16 h = fmap decode_i16 (L.hGet h 2)

read_i32 :: Handle -> IO Int
read_i32 h = fmap decode_i32 (L.hGet h 4)

read_f32 :: Handle -> IO Float
read_f32 h = fmap decode_f32 (L.hGet h 4)

read_pstr :: Handle -> IO ASCII
read_pstr h = do
  n <- fmap decode_u8 (L.hGet h 1)
  fmap decode_str (L.hGet h n)

ascii_to_string :: ASCII -> String
ascii_to_string = C.unpack

type Name = ASCII
type Control = (Name,Int)

read_control :: Handle -> IO Control
read_control h = do
  nm <- read_pstr h
  ix <- read_i16 h
  return (nm,ix)

type Input = (Int,Int)

input_ugen_ix :: Input -> Maybe Int
input_ugen_ix (u,p) = if p == -1 then Nothing else Just u

read_input :: Handle -> IO Input
read_input h = do
  u <- read_i16 h
  p <- read_i16 h
  return (u,p)

type Output = Int

read_output :: Handle -> IO Int
read_output = read_i8

type Rate = Int

type Special = Int

type UGen = (Name,Rate,[Input],[Output],Special)

ugen_inputs :: UGen -> [Input]
ugen_inputs (_,_,i,_,_) = i

ugen_outputs :: UGen -> [Output]
ugen_outputs (_,_,_,o,_) = o

read_ugen :: Handle -> IO UGen
read_ugen h = do
  name <- read_pstr h
  rate <- read_i8 h
  number_of_inputs <- read_i16 h
  number_of_outputs <- read_i16 h
  special <- read_i16 h
  inputs <- replicateM number_of_inputs (read_input h)
  outputs <- replicateM number_of_outputs (read_output h)
  return (name
         ,rate
         ,inputs
         ,outputs
         ,special)

data Graphdef = Graphdef {graphdef_name :: Name
                         ,graphdef_constants :: [Float]
                         ,graphdef_controls :: [(Control,Float)]
                         ,graphdef_ugens :: [UGen]}
                deriving (Eq,Show)

graphdef_ugen :: Graphdef -> Int -> UGen
graphdef_ugen g = (graphdef_ugens g !!)

graphdef_control :: Graphdef -> Int -> (Control,Float)
graphdef_control g = (graphdef_controls g !!)

graphdef_constant_nid :: Graphdef -> Int -> Int
graphdef_constant_nid _ = id

graphdef_control_nid :: Graphdef -> Int -> Int
graphdef_control_nid g = (+) (length (graphdef_constants g))

graphdef_ugen_nid :: Graphdef -> Int -> Int
graphdef_ugen_nid g n = graphdef_control_nid g 0 + length (graphdef_controls g) + n

read_graphdef :: Handle -> IO Graphdef
read_graphdef h = do
  magic <- L.hGet h 4
  version <- read_i32 h
  number_of_definitions <- read_i16 h
  when (magic /= L.pack (map (fromIntegral . fromEnum) "SCgf"))
       (error "read_graphdef: illegal magic string")
  when (version /= 0)
       (error "read_graphdef: version not at zero")
  when (number_of_definitions /= 1)
       (error "read_graphdef: non unary graphdef file")
  name <- read_pstr h
  number_of_constants <- read_i16 h
  constants <- replicateM number_of_constants (read_f32 h)
  number_of_control_defaults <- read_i16 h
  control_defaults <- replicateM number_of_control_defaults (read_f32 h)
  number_of_controls <- read_i16 h
  controls <- replicateM number_of_controls (read_control h)
  number_of_ugens <- read_i16 h
  ugens <- replicateM number_of_ugens (read_ugen h)
  return (Graphdef name
                   constants
                   (zip controls control_defaults)
                   ugens)

-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/simple.scsyndef"
-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/with-ctl.scsyndef"
-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/mce.scsyndef"
-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/mrg.scsyndef"
read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file nm = do
  h <- openFile nm ReadMode
  g <- read_graphdef h
  hClose h
  return g

mk_node_k :: Graphdef -> G.NodeId -> (Control,Float) -> G.Node
mk_node_k g z ((nm,ix),v) =
    let z' = graphdef_control_nid g z
        nm' = ascii_to_string nm
    in G.NodeK z' R.KR (Just ix) nm' v G.K_KR Nothing

is_control_ugen :: UGen -> Bool
is_control_ugen (nm,_,_,_,_) = ascii_to_string nm `elem` ["Control","LagControl","TrigControl"]

is_control_input :: Graphdef -> Input -> Bool
is_control_input g (u,_) =
    if u == -1
    then False
    else is_control_ugen (graphdef_ugen g u)

input_to_from_port :: Graphdef -> Input -> G.FromPort
input_to_from_port g (u,p) =
    if u == -1
    then G.FromPort_C (graphdef_constant_nid g p)
    else if is_control_input g (u,p)
         then if u /= 0
              then error "multiple control UGens..."
              else G.FromPort_K (graphdef_control_nid g p) G.K_KR
         else let ugen = graphdef_ugens g !! u
                  port = if length (ugen_outputs ugen) > 1
                         then Just p
                         else Nothing
              in G.FromPort_U (graphdef_ugen_nid g u) port

mk_node_u :: Graphdef -> G.NodeId -> UGen -> G.Node
mk_node_u g z u =
    let (name,rate,inputs,outputs,special) = u
        z' = graphdef_ugen_nid g z
        rate' = toEnum rate
        name' = ascii_to_string name
        inputs' = map (input_to_from_port g) inputs
        outputs' = map toEnum outputs
        special' = U.Special special
    in G.NodeU z' rate' name' inputs' outputs' special' (U.UId z')

graphdef_to_synthdef :: Graphdef -> S.Synthdef
graphdef_to_synthdef g =
    let constants_nd = zipWith G.NodeC [0..] (graphdef_constants g)
        controls_nd = zipWith (mk_node_k g) [0 ..] (graphdef_controls g)
        ugens_nd = zipWith (mk_node_u g) [0 ..] (graphdef_ugens g)
        nm = ascii_to_string (graphdef_name g)
        gr = G.Graph (-1) constants_nd controls_nd ugens_nd
    in S.Synthdef nm gr
