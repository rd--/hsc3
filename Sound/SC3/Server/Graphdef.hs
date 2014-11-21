-- | Binary 'Graph Definition' as understood by @scsynth@.
module Sound.SC3.Server.Graphdef where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import Data.List
import System.IO {- base -}

import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Coding.Cast {- hosc -}
import Sound.OSC.Type {- hosc -}

-- * Type

type Name = ASCII

type Control = (Name,Int)

type Sample = Double

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

ugen_rate :: UGen -> Rate
ugen_rate (_,r,_,_,_) = r

input_is_control :: Graphdef -> Input -> Bool
input_is_control g (Input u _) =
    if u == -1
    then False
    else ugen_is_control (graphdef_ugen g u)

data Graphdef = Graphdef {graphdef_name :: Name
                         ,graphdef_constants :: [Sample]
                         ,graphdef_controls :: [(Control,Sample)]
                         ,graphdef_ugens :: [UGen]}
                deriving (Eq,Show)

graphdef_ugen :: Graphdef -> Int -> UGen
graphdef_ugen g = (graphdef_ugens g !!)

graphdef_control :: Graphdef -> Int -> (Control,Sample)
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

read_sample :: Handle -> IO Sample
read_sample h = fmap (realToFrac . decode_f32) (L.hGet h 4)

read_pstr :: Handle -> IO ASCII
read_pstr h = do
  n <- fmap decode_u8 (L.hGet h 1)
  fmap decode_str (L.hGet h n)

read_control :: (Handle -> IO Int) -> Handle -> IO Control
read_control read_i h = do
  nm <- read_pstr h
  ix <- read_i h
  return (nm,ix)

read_input :: (Handle -> IO Int) -> Handle -> IO Input
read_input read_i h = do
  u <- read_i h
  p <- read_i h
  return (Input u p)

read_output :: Handle -> IO Int
read_output = read_i8

read_ugen :: (Handle -> IO Int) -> Handle -> IO UGen
read_ugen read_i h = do
  name <- read_pstr h
  rate <- read_i8 h
  number_of_inputs <- read_i h
  number_of_outputs <- read_i h
  special <- read_i16 h
  inputs <- replicateM number_of_inputs (read_input read_i h)
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
  let read_i =
          case version of
            0 -> read_i16
            2 -> read_i32
            _ -> error ("read_graphdef: version not at {zero | two}: " ++ show version)
  number_of_definitions <- read_i16 h
  when (magic /= L.pack (map (fromIntegral . fromEnum) "SCgf"))
       (error "read_graphdef: illegal magic string")
  when (number_of_definitions /= 1)
       (error "read_graphdef: non unary graphdef file")
  name <- read_pstr h
  number_of_constants <- read_i h
  constants <- replicateM number_of_constants (read_sample h)
  number_of_control_defaults <- read_i h
  control_defaults <- replicateM number_of_control_defaults (read_sample h)
  number_of_controls <- read_i h
  controls <- replicateM number_of_controls (read_control read_i h)
  number_of_ugens <- read_i h
  ugens <- replicateM number_of_ugens (read_ugen read_i h)
  -- ignore variants...
  return (Graphdef name
                   constants
                   (zip controls control_defaults)
                   ugens)

-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/simple.scsyndef"
-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/with-ctl.scsyndef"
-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/mce.scsyndef"
-- > g <- read_graphdef_file "/home/rohan/sw/rsc3-disassembler/scsyndef/mrg.scsyndef"
-- > g <- read_graphdef_file "/tmp/1071318657.scsyndef"
-- > putStrLn$ graphdef_stat g
read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file nm = do
  h <- openFile nm ReadMode
  g <- read_graphdef h
  hClose h
  return g

-- * Encode, we write version zero files

-- | Pascal (length prefixed) encoding of string.
encode_pstr :: ASCII -> L.ByteString
encode_pstr = L.pack . str_pstr . ascii_to_string

-- | Byte-encode 'Input' value.
encode_input :: Input -> L.ByteString
encode_input (Input u p) = L.append (encode_i16 u) (encode_i16 p)

encode_control :: Control -> L.ByteString
encode_control (nm,k) = L.concat [encode_pstr nm,encode_i16 k]

-- | Byte-encode 'UGen'.
encode_ugen :: UGen -> L.ByteString
encode_ugen (nm,r,i,o,s) =
    L.concat [encode_pstr nm
             ,encode_i8 r
             ,encode_i16 (length i)
             ,encode_i16 (length o)
             ,encode_i16 s
             ,L.concat (map encode_input i)
             ,L.concat (map encode_i8 o)]

encode_sample :: Sample -> L.ByteString
encode_sample = encode_f32 . realToFrac

encode_graphdef :: Graphdef -> L.ByteString
encode_graphdef (Graphdef nm cs ks us) =
    let (ks_ctl,ks_def) = unzip ks
    in L.concat [encode_str (C.pack "SCgf")
                ,encode_i32 0 -- version
                ,encode_i16 1 -- number of graphs
                ,encode_pstr nm
                ,encode_i16 (length cs)
                ,L.concat (map encode_sample cs)
                ,encode_i16 (length ks_def)
                ,L.concat (map encode_sample ks_def)
                ,encode_i16 (length ks_ctl)
                ,L.concat (map encode_control ks_ctl)
                ,encode_i16 (length us)
                ,L.concat (map encode_ugen us)]

-- * Stat

graphdef_stat :: Graphdef -> String
graphdef_stat (Graphdef _ cs ks us) =
    let u_nm (sc3_nm,_,_,_,_) = ascii_to_string sc3_nm
        f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "graphdef_stat"
              in show . map h . group . sort . map g
        sq = intercalate "," (map u_nm us)
    in unlines ["number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f ugen_rate us
               ,"unit generator sequence   : " ++ sq]
