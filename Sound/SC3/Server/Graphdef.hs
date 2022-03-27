{- | Binary 'Graph Definition' as understood by @scsynth@.
     There are both binary and text encoders and decoders.
-}
module Sound.SC3.Server.Graphdef where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Safe {- safe -}

import qualified Sound.Osc.Datum as Datum {- hosc -}

import qualified Sound.SC3.Common.Math.Operator as Operator {- hsc3 -}
import qualified Sound.SC3.Common.Rate as Rate {- hsc3 -}

-- * Type

-- | Names are Ascii strings (ie. ByteString.Char8)
type Name = Datum.Ascii

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

{- | "SCgf" encoded as 32-bit unsigned integer.

> map fromEnum "SCgf" == [83, 67, 103, 102]
> Byte.decode_i32 (Byte.encode_ascii (Datum.ascii "SCgf"))
-}
scgf_i32 :: Num n => n
scgf_i32 = 1396926310

-- * Get

-- | Get functions for Graphdef types, (str_f,i8_f,i16_f,i32_f,f32_f)
type Get_Functions m = (m Name,m Int,m Int,m Int,m Double)

-- | Get a 'Control'.
get_control :: Monad m => (Get_Functions m,m Int) -> m Control
get_control ((get_str,_,_,_,_),get_i) = do
  nm <- get_str
  ix <- get_i
  return (nm,ix)

-- | Get an 'Input'.
get_input :: Monad m => m Int -> m Input
get_input get_i = liftM2 Input get_i get_i

-- | Get a 'UGen'
get_ugen :: Monad m => (Get_Functions m,m Int) -> m UGen
get_ugen ((get_str,get_i8,get_i16,_,_),get_i) = do
  name <- get_str
  rate <- get_i8
  number_of_inputs <- get_i
  number_of_outputs <- get_i
  special <- get_i16
  inputs <- replicateM number_of_inputs (get_input get_i)
  outputs <- replicateM number_of_outputs get_i8
  return (name
         ,rate
         ,inputs
         ,outputs
         ,special)

-- | Get a 'Graphdef'. Supports version 0|1 and version 2 files.  Ignores variants.
get_graphdef :: Monad m => Get_Functions m -> m Graphdef
get_graphdef c@(get_str,_,get_i16,get_i32,get_f32) = do
  magic <- get_i32
  version <- get_i32
  let get_i =
          case version of
            0 -> get_i16
            1 -> get_i16 -- version one allows variants, which are not allowed by hsc3
            2 -> get_i32
            _ -> error ("get_graphdef: version not at {zero | one | two}: " ++ show version)
  number_of_definitions <- get_i16
  when (magic /= scgf_i32)
       (error "get_graphdef: illegal magic string")
  when (number_of_definitions /= 1)
       (error "get_graphdef: non unary graphdef file")
  name <- get_str
  number_of_constants <- get_i
  constants <- replicateM number_of_constants get_f32
  number_of_control_defaults <- get_i
  control_defaults <- replicateM number_of_control_defaults get_f32
  number_of_controls <- get_i
  controls <- replicateM number_of_controls (get_control (c,get_i))
  number_of_ugens <- get_i
  ugens <- replicateM number_of_ugens (get_ugen (c,get_i))
  return (Graphdef name
                   constants
                   (zip controls control_defaults)
                   ugens)

-- * Encode (version zero)

-- | Encode functions for Graphdef types (join_f,str_f,i8_f,i16_f,i32_f,f32_f,com_f)
type Encode_Functions t = ([t] -> t,Name -> t,Int -> t,Int -> t,Int -> t,Double -> t,String -> t)

encode_input_f :: Encode_Functions t -> Input -> t
encode_input_f (join_f,_,_,i16_f,_,_,_) (Input u p) = join_f (map i16_f [u,p])

encode_control_f :: Encode_Functions t -> Control -> t
encode_control_f (join_f,str_f,_,i16_f,_,_,_) (nm,k) = join_f [str_f nm,i16_f k]

encode_ugen_f :: Encode_Functions t -> UGen -> t
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

encode_graphdef_f :: Encode_Functions t -> Graphdef -> t
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

> import Sound.SC3.Server.Graphdef
> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= graphdef_dump_ugens
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"
-}
graphdef_dump_ugens :: Graphdef -> IO ()
graphdef_dump_ugens = putStrLn . unlines . graphdef_dump_ugens_str
