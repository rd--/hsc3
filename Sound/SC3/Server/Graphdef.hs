-- | Binary 'Graph Definition' as understood by @scsynth@.
--   There are both encoders and decoders.
module Sound.SC3.Server.Graphdef where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified Data.Binary.Get as Get {- binary -}
import qualified Data.Binary.IEEE754 as IEEE754 {- data-binary-ieee754 -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Numeric {- base -}
import qualified Safe {- safe -}

import qualified Sound.OSC.Coding.Byte as Byte {- hosc -}
import qualified Sound.OSC.Coding.Cast as Cast {- hosc -}
import qualified Sound.OSC.Datum as Datum {- hosc -}

import qualified Sound.SC3.Common.Math.Operator as Operator {- hsc3 -}
import qualified Sound.SC3.Common.Rate as Rate {- hsc3 -}

-- * Type

-- | Names are ASCII strings (ie. ByteString.Char8)
type Name = Datum.ASCII

-- | Controls are a name and a ugen-index.
type Control = (Name,Int)

-- | Constants are floating point.
type Sample = Double

-- | UGen indices are Int.
type UGen_Index = Int

-- | Port indices are Int.
type Port_Index = Int

-- | Index used to indicate constants at UGen inputs.
--   Ie. if the ugen-index is this value (-1) it indicates a constant.
constant_index :: UGen_Index
constant_index = -1

-- | Inputs are a ugen-index and a port-index.
data Input = Input UGen_Index Port_Index deriving (Eq,Show)

-- | Rates are encoded as integers (IR = 0, KR = 1, AR = 2, DR = 3).
type Rate = Int

-- | Outputs each indicate a Rate.
type Output = Rate

-- | Secondary (special) index, used by operator UGens to select operation.
type Special = Int

-- | Unit generator type.
type UGen = (Name,Rate,[Input],[Output],Special)

-- | 'UGen' name.
ugen_name_str :: UGen -> String
ugen_name_str (nm,_,_,_,_) = Datum.ascii_to_string nm

-- | 'UGen' name, using operator name if appropriate.
ugen_name_op :: UGen -> String
ugen_name_op (nm,_,_,_,k) =
  let s = Datum.ascii_to_string nm
  in fromMaybe s (Operator.ugen_operator_name s k)

-- | 'UGen' 'Rate'.
ugen_rate :: UGen -> Rate
ugen_rate (_,r,_,_,_) = r

ugen_rate_enum :: UGen -> Rate.Rate
ugen_rate_enum = toEnum . ugen_rate

-- | 'UGen' 'Input's.
ugen_inputs :: UGen -> [Input]
ugen_inputs (_,_,i,_,_) = i

-- | 'UGen' 'Output's.
ugen_outputs :: UGen -> [Output]
ugen_outputs (_,_,_,o,_) = o

-- | Predicate to examine UGen name and decide if it is a control.
ugen_is_control :: UGen -> Bool
ugen_is_control =
  (`elem` ["Control","LagControl","TrigControl"]) .
  ugen_name_str

-- | Input is a UGen (ie. not a constant, indicated by a ugen-index of -1) and the UGen is a control.
input_is_control :: Graphdef -> Input -> Bool
input_is_control g (Input u _) = (u /= constant_index) && ugen_is_control (graphdef_ugen g u)

-- | Graph definition type.
data Graphdef = Graphdef {graphdef_name :: Name
                         ,graphdef_constants :: [Sample]
                         ,graphdef_controls :: [(Control,Sample)]
                         ,graphdef_ugens :: [UGen]}
                deriving (Eq,Show)

-- | Lookup UGen by index.
graphdef_ugen :: Graphdef -> UGen_Index -> UGen
graphdef_ugen g = Safe.atNote "graphdef_ugen" (graphdef_ugens g)

-- | Lookup Control and default value by index.
graphdef_control :: Graphdef -> Int -> (Control,Sample)
graphdef_control g = Safe.atNote "graphdef_controls" (graphdef_controls g)

-- | nid of constant.
graphdef_constant_nid :: Graphdef -> Int -> Int
graphdef_constant_nid _ = id

-- | nid of control.
graphdef_control_nid :: Graphdef -> Int -> Int
graphdef_control_nid g = (+) (length (graphdef_constants g))

-- | nid of UGen.
graphdef_ugen_nid :: Graphdef -> Int -> Int
graphdef_ugen_nid g n = graphdef_control_nid g 0 + length (graphdef_controls g) + n

-- * BINARY GET (version 0 or 2)

-- | Get a 'Sample'
get_sample :: Get.Get Sample
get_sample = fmap realToFrac IEEE754.getFloat32be

-- | Get a 'Name' (Pascal string).
get_pstr :: Get.Get Name
get_pstr = do
  n <- fmap fromIntegral Get.getWord8
  fmap Byte.decode_ascii (Get.getLazyByteString n)

-- | Get a 'Control'.
get_control :: Get.Get Int -> Get.Get Control
get_control get_i = do
  nm <- get_pstr
  ix <- get_i
  return (nm,ix)

-- | Get an 'Input'.
get_input :: Get.Get Int -> Get.Get Input
get_input get_i = do
  u <- get_i
  p <- get_i
  return (Input u p)

-- | Get an 'Output'
get_output :: Get.Get Output
get_output = fmap fromIntegral Get.getInt8

-- | Get a 'UGen'
get_ugen :: Get.Get Int -> Get.Get UGen
get_ugen get_i = do
  name <- get_pstr
  rate <- fmap fromIntegral Get.getInt8
  number_of_inputs <- get_i
  number_of_outputs <- get_i
  special <- fmap fromIntegral Get.getInt16be
  inputs <- replicateM number_of_inputs (get_input get_i)
  outputs <- replicateM number_of_outputs get_output
  return (name
         ,rate
         ,inputs
         ,outputs
         ,special)

-- | Get a 'Graphdef'.  Ignores variants.
get_graphdef :: Get.Get Graphdef
get_graphdef = do
  magic <- Get.getInt32be
  version <- Get.getInt32be
  let get_i =
          case version of
            0 -> fmap fromIntegral Get.getInt16be
            2 -> fmap fromIntegral Get.getInt32be
            _ -> error ("get_graphdef: version not at {zero | two}: " ++ show version)
  number_of_definitions <- Get.getInt16be
  when (magic /= scgf_i32)
       (error "get_graphdef: illegal magic string")
  when (number_of_definitions /= 1)
       (error "get_graphdef: non unary graphdef file")
  name <- get_pstr
  number_of_constants <- get_i
  constants <- replicateM number_of_constants get_sample
  number_of_control_defaults <- get_i
  control_defaults <- replicateM number_of_control_defaults get_sample
  number_of_controls <- get_i
  controls <- replicateM number_of_controls (get_control get_i)
  number_of_ugens <- get_i
  ugens <- replicateM number_of_ugens (get_ugen get_i)
  return (Graphdef name
                   constants
                   (zip controls control_defaults)
                   ugens)

-- * READ

{- | Read Graphdef from .scsyndef file.

> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= putStrLn . graphdef_stat
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"
-}
read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file nm = do
  b <- L.readFile nm
  return (Get.runGet get_graphdef b)

-- * STAT

-- | 'read_graphdef_file' and run 'graphdef_stat'.
scsyndef_stat :: FilePath -> IO String
scsyndef_stat fn = do
  g <- read_graphdef_file fn
  return (graphdef_stat g)

{-
import qualified Control.Monad.State as S {- mtl -}

-- * LIST INPUT

-- | Read the next value from a list.
list_read :: S.State [t] t
list_read = do
  l <- S.get
  when (null l) (error "list_read")
  S.put (tail l)
  return (head l)

-- | 'flip' 'evalState'.
with_list :: [t] -> S.State [t] u -> u
with_list = flip S.evalState
-}

-- * Encode (version zero)

-- | (join_f,str_f,i8_f,i16_f,i32_f,f32_f,com_f)
type ENCODE_F t = ([t] -> t,Name -> t,Int -> t,Int -> t,Int -> t,Double -> t,String -> t)

-- | 'ENCODE_F' for 'L.ByteString'
enc_bytestring :: ENCODE_F L.ByteString
enc_bytestring =
  (L.concat,encode_pstr,Byte.encode_i8,Byte.encode_i16,Byte.encode_i32,encode_sample
  ,const L.empty)

-- | Pascal (length prefixed) encoding of 'Name'.
encode_pstr :: Name -> L.ByteString
encode_pstr = L.pack . Cast.str_pstr . Datum.ascii_to_string

encode_input_f :: ENCODE_F t -> Input -> t
encode_input_f (join_f,_,_,i16_f,_,_,_) (Input u p) = join_f (map i16_f [u,p])

-- | Byte-encode 'Input'.
encode_input :: Input -> L.ByteString
encode_input = encode_input_f enc_bytestring

encode_control_f :: ENCODE_F t -> Control -> t
encode_control_f (join_f,str_f,_,i16_f,_,_,_) (nm,k) = join_f [str_f nm,i16_f k]

-- | Byte-encode 'Control'.
encode_control :: Control -> L.ByteString
encode_control = encode_control_f enc_bytestring

encode_ugen_f :: ENCODE_F t -> UGen -> t
encode_ugen_f enc (nm,r,i,o,s) =
  let (join_f,str_f,i8_f,i16_f,_,_,com_f) = enc
  in join_f [com_f "ugen-name",str_f nm
            ,com_f "ugen-rate",i8_f r
            ,com_f "ugen-number-of-inputs",i16_f (length i)
            ,com_f "ugen-number-of-outputs",i16_f (length o)
            ,com_f "ugen-special",i16_f s
            ,com_f "ugen-inputs (ugen-index,port-index)",join_f (map (encode_input_f enc) i)
            ,com_f "ugen-output-rates",join_f (map i8_f o)
            ]

-- | Byte-encode 'UGen'.
encode_ugen :: UGen -> L.ByteString
encode_ugen = encode_ugen_f enc_bytestring

-- | Encode 'Sample' as 32-bit IEEE float.
encode_sample :: Sample -> L.ByteString
encode_sample = Byte.encode_f32 . realToFrac

encode_graphdef_f :: ENCODE_F t -> Graphdef -> t
encode_graphdef_f enc (Graphdef nm cs ks us) =
    let (join_f,str_f,_,i16_f,i32_f,f32_f,com_f) = enc
        (ks_ctl,ks_def) = unzip ks
    in join_f [com_f "SCgf",i32_f scgf_i32
              ,com_f "version",i32_f 0
              ,com_f "number of graphs",i16_f 1
              ,com_f "name",str_f nm
              ,com_f "number-of-constants",i16_f (length cs)
              ,com_f "constant-values",join_f (map f32_f cs)
              ,com_f "number-of-controls",i16_f (length ks_def)
              ,com_f "control-default-values",join_f (map f32_f ks_def)
              ,com_f "number-of-controls",i16_f (length ks_ctl)
              ,com_f "controls",join_f (map (encode_control_f enc) ks_ctl)
              ,com_f "number-of-ugens",i16_f (length us)
              ,join_f (map (encode_ugen_f enc) us)]

-- | Encode 'Graphdef'.
encode_graphdef :: Graphdef -> L.ByteString
encode_graphdef = encode_graphdef_f enc_bytestring

-- | "SCgf" encoded as 32-bit unsigned integer.
--
-- > Byte.decode_i32 (Byte.encode_ascii (Datum.ascii "SCgf"))
scgf_i32 :: Num n => n
scgf_i32 = 1396926310

-- | * PRINT

-- | Print string.  Strings must not have internal whitespace or semi-colons.
print_string :: Datum.ASCII -> String
print_string a =
  let s = Datum.ascii_to_string a
  in if any isSpace s || ';' `elem` s then error "print_string" else s

enc_text :: (String -> String) -> ENCODE_F String
enc_text com_f =
  (unwords . filter (not . null),print_string,show,show,show,\n -> Numeric.showFFloat Nothing n ""
  ,com_f)

{- | 'encode_graphdef_f' of 'enc_text'

> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= putStrLn . print_graphdef True
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"
-}
print_graphdef :: Bool -> Graphdef -> String
print_graphdef with_com =
    let com_f = if with_com then \c -> concat ["\n; ",c,"\n"] else const ""
    in encode_graphdef_f (enc_text com_f)

-- * IO

-- | Write 'Graphdef' to indicated file.
graphdefWrite :: FilePath -> Graphdef -> IO ()
graphdefWrite fn = L.writeFile fn . encode_graphdef

-- | Write 'Graphdef' to indicated directory.  The filename is the
-- 'graphdef_name' with the appropriate extension (@scsyndef@).
graphdefWrite_dir :: FilePath -> Graphdef -> IO ()
graphdefWrite_dir dir s =
    let fn = dir </> Datum.ascii_to_string (graphdef_name s) <.> "scsyndef"
    in graphdefWrite fn s

-- * Stat

-- | Simple statistics printer for 'Graphdef'.
graphdef_stat :: Graphdef -> String
graphdef_stat (Graphdef nm cs ks us) =
    let f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "graphdef_stat"
              in show . map h . group . sort . map g
        sq pp_f = intercalate "," (pp_f (map ugen_name_op us))
    in unlines ["name                      : " ++ show nm
               ,"number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f ugen_rate us
               ,"unit generator set        : " ++ sq (nub . sort)
               ,"unit generator sequence   : " ++ sq id]

-- * Dump UGens

-- | Pretty print UGen in the manner of SynthDef>>dumpUGens.
ugen_dump_ugen_str :: [Sample] -> [UGen] -> UGen_Index -> UGen -> String
ugen_dump_ugen_str c_sq u_sq ix u =
  let in_brackets :: String -> String
      in_brackets x = printf "[%s]" x
      input_pp (Input i j) =
        let ui = u_sq !! i
        in if i >= 0
           then if length (ugen_outputs ui) > 1
                then printf "%d_%s:%d" i (ugen_name_op ui) j
                else printf "%d_%s" i (ugen_name_op ui)
           else printf "%f" (c_sq !! j)
      inputs_pp = in_brackets . intercalate "," . map input_pp
  in printf "%d_%s, %s, %s" ix (ugen_name_op u) (show (ugen_rate_enum u)) (inputs_pp (ugen_inputs u))

-- | Print graphdef in format equivalent to SynthDef>>dumpUGens in SuperCollider
graphdef_dump_ugens_str :: Graphdef -> [String]
graphdef_dump_ugens_str (Graphdef _nm cs _ks us) = zipWith (ugen_dump_ugen_str cs us) [0..] us

{- | 'putStrLn' of 'unlines' of 'graphdef_dump_ugens_str'

> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= graphdef_dump_ugens
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"
-}
graphdef_dump_ugens :: Graphdef -> IO ()
graphdef_dump_ugens = putStrLn . unlines . graphdef_dump_ugens_str
