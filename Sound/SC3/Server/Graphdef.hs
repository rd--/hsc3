-- | Binary 'Graph Definition'.
module Sound.SC3.Server.Graphdef where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import System.IO {- base -}

import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Type {- hosc -}

-- * Type

type Name = ASCII

type Control = (Name,Int)

data Input = Input Int Int deriving (Eq,Show)

input_ugen_ix :: Input -> Maybe Int
input_ugen_ix (Input u p) = if p == -1 then Nothing else Just u

type Output = Int

type Rate = Int

type Special = Int

type UGen = (Name,Rate,[Input],[Output],Special)

ugen_inputs :: UGen -> [Input]
ugen_inputs (_,_,i,_,_) = i

ugen_outputs :: UGen -> [Output]
ugen_outputs (_,_,_,o,_) = o

ugen_is_control :: UGen -> Bool
ugen_is_control (nm,_,_,_,_) = ascii_to_string nm `elem` ["Control","LagControl","TrigControl"]

input_is_control :: Graphdef -> Input -> Bool
input_is_control g (Input u _) =
    if u == -1
    then False
    else ugen_is_control (graphdef_ugen g u)

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

-- * Read

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

read_control :: Handle -> IO Control
read_control h = do
  nm <- read_pstr h
  ix <- read_i16 h
  return (nm,ix)

read_input :: Handle -> IO Input
read_input h = do
  u <- read_i16 h
  p <- read_i16 h
  return (Input u p)

read_output :: Handle -> IO Int
read_output = read_i8

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

-- * Encode

-- | Byte-encode 'Input' value.
encode_input :: Input -> L.ByteString
encode_input (Input u p) = L.append (encode_i16 u) (encode_i16 p)

encode_control :: Control -> L.ByteString
encode_control (nm,k) = L.concat [encode_str nm,encode_i16 k]

-- | Byte-encode 'UGen'.
encode_ugen :: UGen -> L.ByteString
encode_ugen (nm,r,i,o,s) =
    L.concat [encode_str nm
             ,encode_i8 r
             ,encode_i16 (length i)
             ,encode_i16 (length o)
             ,encode_i16 s
             ,L.concat (map encode_input i)
             ,L.concat (map encode_i8 o)]

encode_graphdef :: Graphdef -> L.ByteString
encode_graphdef (Graphdef nm cs ks us) =
    let (ks_ctl,ks_def) = unzip ks
    in L.concat [encode_str (C.pack "SCgf")
                ,encode_i32 0 -- version
                ,encode_i16 1 -- number of graphs
                ,encode_str nm
                ,encode_i16 (length cs)
                ,L.concat (map encode_f32 cs)
                ,encode_i16 (length ks_def)
                ,L.concat (map encode_f32 ks_def)
                ,encode_i16 (length ks_ctl)
                ,L.concat (map encode_control ks_ctl)
                ,encode_i16 (length us)
                ,L.concat (map encode_ugen us)]
