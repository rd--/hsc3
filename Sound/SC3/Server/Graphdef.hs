-- | Binary 'Graph Definition' as understood by @scsynth@.
--   There are both encoders and decoders.
module Sound.SC3.Server.Graphdef where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import Data.List {- base -}
import System.IO {- base -}

import qualified Sound.OSC.Coding.Byte as Byte {- hosc -}
import qualified Sound.OSC.Coding.Cast as Cast {- hosc -}
import qualified Sound.OSC.Datum as Datum {- hosc -}

-- * Type

-- | Names are ASCII strings.
type Name = Datum.ASCII

-- | Controls are a name and a ugen-index.
type Control = (Name,Int)

-- | Constants are floating point.
type Sample = Double

-- | Inputs are a ugen-index and a port-index.
--   If the ugen-index is -1 it indicates a constant.
data Input = Input Int Int deriving (Eq,Show)

-- | Read ugen-index of input, else Nothing.
input_ugen_ix :: Input -> Maybe Int
input_ugen_ix (Input u p) = if p == -1 then Nothing else Just u

-- | Rates are encoded as integers (IR = 0, KR = 1, AR = 2, DR = 3).
type Rate = Int

-- | Outputs each indicate a Rate.
type Output = Rate

-- | Secondary (special) index, used by operator UGens to select operation.
type Special = Int

-- | Unit generator type.
type UGen = (Name,Rate,[Input],[Output],Special)

ugen_rate :: UGen -> Rate
ugen_rate (_,r,_,_,_) = r

ugen_inputs :: UGen -> [Input]
ugen_inputs (_,_,i,_,_) = i

ugen_outputs :: UGen -> [Output]
ugen_outputs (_,_,_,o,_) = o

-- | Predicate to examine Ugen name and decide if it is a control.
ugen_is_control :: UGen -> Bool
ugen_is_control (nm,_,_,_,_) =
  Datum.ascii_to_string nm `elem` ["Control","LagControl","TrigControl"]

-- | Input is a UGen and the UGen is a control.
input_is_control :: Graphdef -> Input -> Bool
input_is_control g (Input u _) =
    if u == -1
    then False
    else ugen_is_control (graphdef_ugen g u)

-- | Graph definition type.
data Graphdef = Graphdef {graphdef_name :: Name
                         ,graphdef_constants :: [Sample]
                         ,graphdef_controls :: [(Control,Sample)]
                         ,graphdef_ugens :: [UGen]}
                deriving (Eq,Show)

-- | Lookup UGen by index.
graphdef_ugen :: Graphdef -> Int -> UGen
graphdef_ugen g = (graphdef_ugens g !!)

-- | Lookup Control and default value by index.
graphdef_control :: Graphdef -> Int -> (Control,Sample)
graphdef_control g = (graphdef_controls g !!)

graphdef_constant_nid :: Graphdef -> Int -> Int
graphdef_constant_nid _ = id

graphdef_control_nid :: Graphdef -> Int -> Int
graphdef_control_nid g = (+) (length (graphdef_constants g))

graphdef_ugen_nid :: Graphdef -> Int -> Int
graphdef_ugen_nid g n = graphdef_control_nid g 0 + length (graphdef_controls g) + n

-- * Read (version 0 or 2).

-- | Read a 'Sample'.
read_sample :: Handle -> IO Sample
read_sample = fmap realToFrac . Byte.read_f32

-- | Read a 'Control'.
read_control :: (Handle -> IO Int) -> Handle -> IO Control
read_control read_i h = do
  nm <- Byte.read_pstr h
  ix <- read_i h
  return (nm,ix)

-- | Read an 'Input'.
read_input :: (Handle -> IO Int) -> Handle -> IO Input
read_input read_i h = do
  u <- read_i h
  p <- read_i h
  return (Input u p)

-- | Read an 'output'.
read_output :: Handle -> IO Output
read_output = Byte.read_i8

-- | Read a 'UGen'.
read_ugen :: (Handle -> IO Int) -> Handle -> IO UGen
read_ugen read_i h = do
  name <- Byte.read_pstr h
  rate <- Byte.read_i8 h
  number_of_inputs <- read_i h
  number_of_outputs <- read_i h
  special <- Byte.read_i16 h
  inputs <- replicateM number_of_inputs (read_input read_i h)
  outputs <- replicateM number_of_outputs (read_output h)
  return (name
         ,rate
         ,inputs
         ,outputs
         ,special)

-- | Read a 'Graphdef'. Ignores variants.
read_graphdef :: Handle -> IO Graphdef
read_graphdef h = do
  magic <- fmap Byte.decode_str (L.hGet h 4)
  version <- Byte.read_i32 h
  let read_i =
          case version of
            0 -> Byte.read_i16
            2 -> Byte.read_i32
            _ -> error ("read_graphdef: version not at {zero | two}: " ++ show version)
  number_of_definitions <- Byte.read_i16 h
  when (magic /= Datum.ascii "SCgf")
       (error "read_graphdef: illegal magic string")
  when (number_of_definitions /= 1)
       (error "read_graphdef: non unary graphdef file")
  name <- Byte.read_pstr h
  number_of_constants <- read_i h
  constants <- replicateM number_of_constants (read_sample h)
  number_of_control_defaults <- read_i h
  control_defaults <- replicateM number_of_control_defaults (read_sample h)
  number_of_controls <- read_i h
  controls <- replicateM number_of_controls (read_control read_i h)
  number_of_ugens <- read_i h
  ugens <- replicateM number_of_ugens (read_ugen read_i h)
  return (Graphdef name
                   constants
                   (zip controls control_defaults)
                   ugens)

{- | Read Graphdef from file.

> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= putStrLn . graphdef_stat
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"

-}
read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file nm = do
  h <- openFile nm ReadMode
  g <- read_graphdef h
  hClose h
  return g

-- * Encode (version zero)

-- | Pascal (length prefixed) encoding of string.
encode_pstr :: Name -> L.ByteString
encode_pstr = L.pack . Cast.str_pstr . Datum.ascii_to_string

-- | Byte-encode 'Input'.
encode_input :: Input -> L.ByteString
encode_input (Input u p) = L.append (Byte.encode_i16 u) (Byte.encode_i16 p)

-- | Byte-encode 'Control'.
encode_control :: Control -> L.ByteString
encode_control (nm,k) = L.concat [encode_pstr nm,Byte.encode_i16 k]

-- | Byte-encode 'UGen'.
encode_ugen :: UGen -> L.ByteString
encode_ugen (nm,r,i,o,s) =
    L.concat [encode_pstr nm
             ,Byte.encode_i8 r
             ,Byte.encode_i16 (length i)
             ,Byte.encode_i16 (length o)
             ,Byte.encode_i16 s
             ,L.concat (map encode_input i)
             ,L.concat (map Byte.encode_i8 o)]

-- | Encode 'Sample' as 32-bit IEEE float.
encode_sample :: Sample -> L.ByteString
encode_sample = Byte.encode_f32 . realToFrac

-- | Encode 'Graphdef'.
encode_graphdef :: Graphdef -> L.ByteString
encode_graphdef (Graphdef nm cs ks us) =
    let (ks_ctl,ks_def) = unzip ks
    in L.concat [Byte.encode_str (Datum.ascii "SCgf")
                ,Byte.encode_i32 0 -- version
                ,Byte.encode_i16 1 -- number of graphs
                ,encode_pstr nm
                ,Byte.encode_i16 (length cs)
                ,L.concat (map encode_sample cs)
                ,Byte.encode_i16 (length ks_def)
                ,L.concat (map encode_sample ks_def)
                ,Byte.encode_i16 (length ks_ctl)
                ,L.concat (map encode_control ks_ctl)
                ,Byte.encode_i16 (length us)
                ,L.concat (map encode_ugen us)]

-- * Stat

-- | Simple statistics printer for 'Graphdef'.
graphdef_stat :: Graphdef -> String
graphdef_stat (Graphdef nm cs ks us) =
    let u_nm (sc3_nm,_,_,_,_) = Datum.ascii_to_string sc3_nm
        f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "graphdef_stat"
              in show . map h . group . sort . map g
        sq pp_f = intercalate "," (pp_f (map u_nm us))
    in unlines ["name                      : " ++ show nm
               ,"number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f ugen_rate us
               ,"unit generator set        : " ++ sq (nub . sort)
               ,"unit generator sequence   : " ++ sq id]

