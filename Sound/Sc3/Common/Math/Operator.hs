{- | Non-standard mathematical enumerations, classes and base instances.

Enumerations of the unary and binary math unit generators.
Names that conflict with existing names have a @_@ suffix.

The Eq and Ord classes in the Prelude require Bool, hence EqE and OrdE.
True is 1.0, False is 0.0

The RealFrac class requires Integral results, hence RealFracE.

-}
module Sound.Sc3.Common.Math.Operator where

import Control.Monad {- base -}
import qualified Data.Fixed as F {- base -}
import Data.Int {- base -}
import Data.Maybe {- base -}

import qualified Sound.Sc3.Common.Base as Base {- hsc3 -}
import qualified Sound.Sc3.Common.Math as Math {- hsc3 -}

-- * Unary

{- | Enumeration of @Sc3@ unary operator Ugens.
     The names here are from the enumeration at "server/plugins/UnaryOpUgens.cpp".
     The capitalisation is edited since these names become function names in rsc3.
     Names have a _ suffix if they conflict with Ugen names.

> zip (map show [minBound :: Sc3_Unary_Op .. maxBound]) [0..]
-}
data Sc3_Unary_Op
  = OpNeg -- -
  | OpNot -- !
  | OpIsNil
  | OpNotNil
  | OpBitNot
  | OpAbs -- 5
  | OpAsFloat
  | OpAsInt
  | OpCeil -- 8
  | OpFloor -- 9
  | OpFrac -- 10
  | OpSign -- 11
  | OpSquared -- 12
  | OpCubed
  | OpSqrt -- 14
  | OpExp -- 15
  | OpRecip -- 16
  | OpMidiCps -- 17
  | OpCpsMidi -- 18
  | OpMidiRatio -- 19
  | OpRatioMidi -- 20
  | OpDbAmp -- 21
  | OpAmpDb -- 22
  | OpOctCps
  | OpCpsOct
  | OpLog -- 25 (natural, base e)
  | OpLog2 -- 26 (base 2)
  | OpLog10 -- 27 (base 10)
  | OpSin -- 28
  | OpCos -- 29
  | OpTan -- 30
  | OpArcSin
  | OpArcCos
  | OpArcTan
  | OpSinh
  | OpCosh -- 35
  | OpTanh -- 36
  | OpRand_ -- 37 ; Ugen
  | OpRand2
  | OpLinRand_ -- 39 ; Ugen
  | OpBiLinRand -- 40
  | OpSum3Rand
  | OpDistort -- 42
  | OpSoftClip -- 43
  | OpCoin
  | OpDigitValue -- 45
  | OpSilence
  | OpThru
  | OpRectWindow
  | OpHanWindow
  | OpWelchWindow -- 50
  | OpTriWindow
  | OpRamp_ -- 52 ; Ugen
  | OpScurve
  deriving (Eq,Show,Enum,Bounded,Read)

-- | Enum name without Op prefix.
sc3_unary_op_name :: Sc3_Unary_Op -> String
sc3_unary_op_name = drop 2 . show

{- | 'Base.parse_enum' with Op prefix.

> mapMaybe (parse_unary Base.CS) (words "Abs Rand") == [OpAbs,OpRand]
-}
parse_unary :: Base.Case_Rule -> String -> Maybe Sc3_Unary_Op
parse_unary cr = Base.parse_enum cr . (++) "Op"

-- | Table of operator names (non-symbolic) and indices.
--
-- > map fst sc3_unary_op_tbl
sc3_unary_op_tbl :: [(String,Int)]
sc3_unary_op_tbl = zip (map sc3_unary_op_name [minBound .. maxBound]) [0..]

-- | Table of symbolic names for standard unary operators.
unary_sym_tbl :: [(Sc3_Unary_Op,String)]
unary_sym_tbl = [] -- (Neg,"-"),(Not,"!")

-- | Lookup possibly symbolic name for standard unary operators.
unaryName :: Int -> String
unaryName n =
  let e = toEnum n
  in fromMaybe (sc3_unary_op_name e) (lookup e unary_sym_tbl)

-- | Given name of unary operator derive index.
--
-- > mapMaybe (unaryIndex Base.CI) (words "abs CUBED midicps NEG") == [5,13,17,0]
-- > unaryIndex Base.CS "SinOsc" == Nothing
unaryIndex :: Base.Case_Rule -> String -> Maybe Int
unaryIndex cr nm =
    let ix = Base.rlookup_str cr nm unary_sym_tbl
        ix' = parse_unary cr nm
    in fmap fromEnum (mplus ix' ix)

-- | 'isJust' of 'unaryIndex'.
--
-- > map (is_unary CI) (words "Abs MidiCps Neg")
-- > map (is_unary CI) (words "- RAND")
-- > map (is_unary CI) (words "arctan atan")
is_unary :: Base.Case_Rule -> String -> Bool
is_unary cr = isJust . unaryIndex cr

-- * Binary

-- | Enumeration of @Sc3@ unary operator Ugens.
--   The names here are from the enumeration at "server/plugins/BinaryOpUgens.cpp".
--
-- > zip (map show [minBound :: Sc3_Binary_Op .. maxBound]) [0..]
data Sc3_Binary_Op
  = OpAdd -- 0
  | OpSub -- 1
  | OpMul -- 2
  | OpIdiv -- 3
  | OpFdiv -- 4
  | OpMod -- 5
  | OpEq -- 6
  | OpNe -- 7
  | OpLt -- 8
  | OpGt -- 9
  | OpLe -- 10
  | OpGe -- 11
  | OpMin -- 12
  | OpMax -- 13
  | OpBitAnd -- 14
  | OpBitOr -- 15
  | OpBitXor
  | OpLcm -- 17
  | OpGcd -- 18
  | OpRound -- 19
  | OpRoundUp -- 20
  | OpTrunc -- 21
  | OpAtan2
  | OpHypot
  | OpHypotx
  | OpPow -- 25
  | OpShiftLeft -- 26
  | OpShiftRight -- 27
  | OpUnsignedShift
  | OpFill
  | OpRing1 -- 30
  | OpRing2
  | OpRing3
  | OpRing4
  | OpDifSqr
  | OpSumSqr -- 35
  | OpSqrSum
  | OpSqrDif
  | OpAbsDif
  | OpThresh
  | OpAmClip -- 40
  | OpScaleNeg
  | OpClip2 -- 42
  | OpExcess
  | OpFold2 -- 44
  | OpWrap2
  | OpFirstArg
  | OpRandRange
  | OpExpRandRange
  deriving (Eq,Show,Enum,Bounded,Read)

-- | Enum name without Op prefix.
sc3_binary_op_name :: Sc3_Binary_Op -> String
sc3_binary_op_name = drop 2 . show

-- | Table of operator names (non-symbolic) and indices.
sc3_binary_op_tbl :: [(String,Int)]
sc3_binary_op_tbl = zip (map sc3_binary_op_name [minBound .. maxBound]) [0..]

{- | 'parse_enum' with Op prefix.

> parse_binary Base.CI "mul" == Just OpMul
-}
parse_binary :: Base.Case_Rule -> String -> Maybe Sc3_Binary_Op
parse_binary cr = Base.parse_enum cr . (++) "Op"

-- | Table of symbolic names for standard binary operators.
binary_sym_tbl :: [(Sc3_Binary_Op,String)]
binary_sym_tbl =
    [(OpAdd,"+")
    ,(OpSub,"-")
    ,(OpMul,"*")
    ,(OpFdiv,"/")
    ,(OpMod,"%")
    ,(OpEq,"==")
    ,(OpNe,"/=") -- or !=
    ,(OpLt,"<")
    ,(OpGt,">")
    ,(OpLe,"<=")
    ,(OpGe,">=")
    ,(OpBitAnd,".&.") -- or &
    ,(OpBitOr,".|.") -- or |
    ,(OpPow,"**")]

-- | Table of operator names (non-symbolic) and indices.
--
-- > map fst sc3_binary_op_sym_tbl
sc3_binary_op_sym_tbl :: [(String,Int)]
sc3_binary_op_sym_tbl =
  let f x = fromMaybe (sc3_binary_op_name x) (lookup x binary_sym_tbl)
  in zip (map f [minBound .. maxBound]) [0..]

-- | Lookup possibly symbolic name for standard binary operators.
--
-- > map binaryName [1,2,8,12] == ["-","*","<","Min"]
binaryName :: Int -> String
binaryName n =
  let e = toEnum n
  in fromMaybe (sc3_binary_op_name e) (lookup e binary_sym_tbl)

{- | Given name of binary operator derive index.

> mapMaybe (binaryIndex Base.CI) (words "* mul ring1 +") == [2,2,30,0]
> binaryIndex Base.CI "sinosc" == Nothing
> map (\x -> (x,binaryIndex Base.CI x)) (map snd binary_sym_tbl)
-}
binaryIndex :: Base.Case_Rule -> String -> Maybe Int
binaryIndex cr nm =
    let ix = Base.rlookup_str cr nm binary_sym_tbl
        ix' = parse_binary cr nm
    in fmap fromEnum (mplus ix' ix)

-- | 'isJust' of 'binaryIndex'.
--
-- > map (is_binary CI) (words "== > % TRUNC MAX")
is_binary :: Base.Case_Rule -> String -> Bool
is_binary cr = isJust . binaryIndex cr

-- * Operator

-- | Lookup operator name for operator Ugens, else Ugen name.
ugen_operator_name :: String -> Int -> Maybe String
ugen_operator_name nm n =
    case nm of
      "UnaryOpUGen" -> Just (unaryName n)
      "BinaryOpUGen" -> Just (binaryName n)
      _ -> Nothing

{- | Order of lookup: binary then unary.

> map (resolve_operator Sound.Sc3.Common.Base.CI) (words "+ - ADD SUB NEG ABS")
> map (resolve_operator Sound.Sc3.Common.Base.CS) (words "Abs")
-}
resolve_operator :: Base.Case_Rule -> String -> (String,Maybe Int)
resolve_operator cr nm =
    case binaryIndex cr nm of
      Just sp -> ("BinaryOpUGen",Just sp)
      Nothing -> case unaryIndex cr nm of
                   Just sp -> ("UnaryOpUGen",Just sp)
                   _ -> (nm,Nothing)

-- | Case-insensitive resolve_operator.
resolve_operator_ci :: String -> (String,Maybe Int)
resolve_operator_ci = resolve_operator Base.CI

-- * Classes

-- | Variant on 'Eq' class, result is of the same type as the values compared.
class (Eq a,Num a) => EqE a where
  equal_to :: a -> a -> a
  equal_to = Math.sc3_eq
  not_equal_to :: a -> a -> a
  not_equal_to = Math.sc3_neq

instance EqE Int where
instance EqE Integer where
instance EqE Int32 where
instance EqE Int64 where
instance EqE Float where
instance EqE Double where

-- | Variant on Ord class, result is of the same type as the values compared.
class (Ord a,Num a) => OrdE a where
    less_than :: a -> a -> a
    less_than = Math.sc3_lt
    less_than_or_equal_to :: a -> a -> a
    less_than_or_equal_to = Math.sc3_lte
    greater_than :: a -> a -> a
    greater_than = Math.sc3_gt
    greater_than_or_equal_to :: a -> a -> a
    greater_than_or_equal_to = Math.sc3_gte

instance OrdE Int
instance OrdE Integer
instance OrdE Int32
instance OrdE Int64
instance OrdE Float
instance OrdE Double

-- | Variant of 'RealFrac' with non 'Integral' results.
class RealFrac a => RealFracE a where
  properFractionE :: a -> (a,a)
  properFractionE = Math.sc3_properFraction
  truncateE :: a -> a
  truncateE = Math.sc3_truncate
  roundE :: a -> a
  roundE = Math.sc3_round
  ceilingE :: a -> a
  ceilingE = Math.sc3_ceiling
  floorE :: a -> a
  floorE = Math.sc3_floor

instance RealFracE Float
instance RealFracE Double

-- | Unary operator class.
--
-- > map (floor . (* 1e4) . dbAmp) [-90,-60,-30,0] == [0,10,316,10000]
class (Floating a, Ord a) => UnaryOp a where
    ampDb :: a -> a
    ampDb = Math.amp_to_db
    asFloat :: a -> a
    asFloat = error "asFloat"
    asInt :: a -> a
    asInt = error "asInt"
    cpsMidi :: a -> a
    cpsMidi = Math.cps_to_midi
    cpsOct :: a -> a
    cpsOct = Math.cps_to_oct
    cubed :: a -> a
    cubed n = n * n * n
    dbAmp :: a -> a
    dbAmp = Math.db_to_amp
    distort :: a -> a
    distort = Math.sc3_distort
    frac :: a -> a
    frac = error "frac"
    isNil :: a -> a
    isNil a = if a == 0.0 then 0.0 else 1.0
    log10 :: a -> a
    log10 = logBase 10
    log2 :: a -> a
    log2 = logBase 2
    midiCps :: a -> a
    midiCps = Math.midi_to_cps
    midiRatio :: a -> a
    midiRatio = Math.midi_to_ratio
    notE :: a -> a
    notE a = if a > 0.0 then 0.0 else 1.0
    notNil :: a -> a
    notNil a = if a /= 0.0 then 0.0 else 1.0
    octCps :: a -> a
    octCps = Math.oct_to_cps
    ramp_ :: a -> a
    ramp_ _ = error "ramp_"
    ratioMidi :: a -> a
    ratioMidi = Math.ratio_to_midi
    softClip :: a -> a
    softClip = Math.sc3_softclip
    squared :: a -> a
    squared = \z -> z * z

instance UnaryOp Float where
instance UnaryOp Double where

-- | Sc3_Binary_Op operator class.
class (Floating a,RealFrac a, Ord a) => BinaryOp a where
    absDif :: a -> a -> a
    absDif a b = abs (a - b)
    amClip :: a -> a -> a
    amClip a b = if b <= 0 then 0 else a * b
    atan2E :: a -> a -> a
    atan2E a b = atan (b/a)
    clip2 :: a -> a -> a
    clip2 a b = Math.sc3_clip a (-b) b
    difSqr :: a -> a -> a
    difSqr = Math.sc3_dif_sqr
    excess :: a -> a -> a
    excess a b = a - Math.sc3_clip a (-b) b
    exprandRange :: a -> a -> a
    exprandRange = error "exprandRange"
    fill :: a -> a -> a
    fill = error "fill"
    firstArg :: a -> a -> a
    firstArg a _ = a
    fold2 :: a -> a -> a
    fold2 a b = Math.sc3_fold a (-b) b
    gcdE :: a -> a -> a
    gcdE = Math.sc3_gcd
    hypot :: a -> a -> a
    hypot = Math.sc3_hypot
    hypotx :: a -> a -> a
    hypotx = Math.sc3_hypotx
    iDiv :: a -> a -> a
    iDiv = Math.sc3_idiv
    lcmE :: a -> a -> a
    lcmE = Math.sc3_lcm
    modE :: a -> a -> a
    modE = Math.sc3_mod
    randRange :: a -> a -> a
    randRange = error "randRange"
    ring1 :: a -> a -> a
    ring1 a b = a * b + a
    ring2 :: a -> a -> a
    ring2 a b = a * b + a + b
    ring3 :: a -> a -> a
    ring3 a b = a * a * b
    ring4 :: a -> a -> a
    ring4 a b = a * a * b - a * b * b
    roundUp :: a -> a -> a
    roundUp = error "roundUp"
    scaleNeg :: a -> a -> a
    scaleNeg a b = (abs a - a) * b' + a where b' = 0.5 * b + 0.5
    sqrDif :: a -> a -> a
    sqrDif a b = (a-b) * (a-b)
    sqrSum :: a -> a -> a
    sqrSum a b = (a+b) * (a+b)
    sumSqr :: a -> a -> a
    sumSqr a b = (a*a) + (b*b)
    thresh :: a -> a -> a
    thresh a b = if a <  b then 0 else a
    trunc :: a -> a -> a
    trunc = error "trunc"
    wrap2 :: a -> a -> a
    wrap2 = error "wrap2"

instance BinaryOp Float where
    fold2 a b = Math.sc3_fold a (-b) b
    modE = F.mod'
    roundUp a b = if b == 0 then a else ceilingE (a/b + 0.5) * b
    wrap2 a b = Math.sc3_wrap_ni (-b) b a

instance BinaryOp Double where
    fold2 a b = Math.sc3_fold a (-b) b
    modE = F.mod'
    roundUp a b = if b == 0 then a else ceilingE (a/b + 0.5) * b
    wrap2 a b = Math.sc3_wrap_ni (-b) b a

-- * Infix

(==**) :: EqE a => a -> a -> a
(==**) = equal_to

(/=**) :: EqE a => a -> a -> a
(/=**) = not_equal_to

(<**) :: OrdE a => a -> a -> a
(<**) = less_than

(<=**) :: OrdE a => a -> a -> a
(<=**) = less_than_or_equal_to

(>**) :: OrdE a => a -> a -> a
(>**) = greater_than

(>=**) :: OrdE a => a -> a -> a
(>=**) = greater_than_or_equal_to

-- * Tables

-- | Association table for 'Sc3_Binary_Op' to haskell function implementing operator.
binop_hs_tbl :: (Real n,Floating n,RealFrac n) => [(Sc3_Binary_Op,n -> n -> n)]
binop_hs_tbl =
    [(OpAdd,(+))
    ,(OpSub,(-))
    ,(OpFdiv,(/))
    ,(OpIdiv,Math.sc3_idiv)
    ,(OpMod,Math.sc3_mod)
    ,(OpEq,Math.sc3_eq)
    ,(OpNe,Math.sc3_neq)
    ,(OpLt,Math.sc3_lt)
    ,(OpLe,Math.sc3_lte)
    ,(OpGt,Math.sc3_gt)
    ,(OpGe,Math.sc3_gte)
    ,(OpMin,min)
    ,(OpMax,max)
    ,(OpMul,(*))
    ,(OpPow,(**))
    ,(OpMin,min)
    ,(OpMax,max)
    ,(OpRound,Math.sc3_round_to)]

-- | 'lookup' 'binop_hs_tbl' via 'toEnum'.
binop_special_hs :: (RealFrac n,Floating n) => Int -> Maybe (n -> n -> n)
binop_special_hs z = lookup (toEnum z) binop_hs_tbl

-- | Association table for 'Unary' to haskell function implementing operator.
uop_hs_tbl :: (RealFrac n,Floating n) => [(Sc3_Unary_Op,n -> n)]
uop_hs_tbl =
    [(OpNeg,negate)
    ,(OpNot,\z -> if z > 0 then 0 else 1)
    ,(OpAbs,abs)
    ,(OpCeil,Math.sc3_ceiling)
    ,(OpFloor,Math.sc3_floor)
    ,(OpSquared,\z -> z * z)
    ,(OpCubed,\z -> z * z * z)
    ,(OpSqrt,sqrt)
    ,(OpRecip,recip)
    ,(OpMidiCps,Math.midi_to_cps)
    ,(OpCpsMidi,Math.cps_to_midi)
    ,(OpSin,sin)
    ,(OpCos,cos)
    ,(OpTan,tan)]

-- | 'lookup' 'uop_hs_tbl' via 'toEnum'.
uop_special_hs :: (RealFrac n,Floating n) => Int -> Maybe (n -> n)
uop_special_hs z = lookup (toEnum z) uop_hs_tbl
