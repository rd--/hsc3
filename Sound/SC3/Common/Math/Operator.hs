{- | Non-standard mathematical enumerations, classes and base instances.

Enumerations of the unary and binary math unit generators.
Names that conflict with existing names have a @_@ suffix.

The Eq and Ord classes in the Prelude require Bool, hence EqE and OrdE.
True is 1.0, False is 0.0

The RealFrac class requires Integral results, hence RealFracE.

-}
module Sound.SC3.Common.Math.Operator where

import Control.Monad {- base -}
import qualified Data.Fixed as F {- base -}
import Data.Int {- base -}
import Data.Maybe {- base -}

import qualified Sound.SC3.Common.Base as Base {- hsc3 -}
import qualified Sound.SC3.Common.Math as Math {- hsc3 -}

-- * Unary

-- | Enumeration of @SC3@ unary operator UGens.
data SC3_Unary_Op
            = Neg -- -
            | Not -- !
            | IsNil
            | NotNil
            | BitNot
            | Abs
            | AsFloat
            | AsInt
            | Ceil
            | Floor
            | Frac
            | Sign
            | Squared
            | Cubed
            | Sqrt
            | Exp
            | Recip
            | MIDICPS
            | CPSMIDI
            | MIDIRatio
            | RatioMIDI
            | DbAmp
            | AmpDb
            | OctCPS
            | CPSOct
            | Log
            | Log2
            | Log10
            | Sin
            | Cos
            | Tan
            | ArcSin
            | ArcCos
            | ArcTan
            | SinH
            | CosH
            | TanH
            | Rand_ -- UGen
            | Rand2
            | LinRand_ -- UGen
            | BiLinRand
            | Sum3Rand
            | Distort
            | SoftClip
            | Coin
            | DigitValue
            | Silence
            | Thru
            | RectWindow
            | HanWindow
            | WelchWindow
            | TriWindow
            | Ramp_ -- UGen
            | SCurve
              deriving (Eq,Show,Enum,Bounded,Read)

-- | Type-specialised 'Base.parse_enum'.
--
-- > mapMaybe (parse_unary Base.CS) (words "Abs Rand_") == [Abs,Rand_]
parse_unary :: Base.Case_Rule -> String -> Maybe SC3_Unary_Op
parse_unary = Base.parse_enum

-- | Table of operator names (non-symbolic) and indices.
--
-- > map fst sc3_unary_op_tbl
sc3_unary_op_tbl :: [(String,Int)]
sc3_unary_op_tbl = zip (map show [Neg .. SCurve]) [0..]

-- | Table of symbolic names for standard unary operators.
unary_sym_tbl :: [(SC3_Unary_Op,String)]
unary_sym_tbl = [] -- (Neg,"-"),(Not,"!")

-- | Lookup possibly symbolic name for standard unary operators.
unaryName :: Int -> String
unaryName n =
    let e = toEnum n
    in fromMaybe (show e) (lookup e unary_sym_tbl)

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
-- > map (is_unary CI) (words "ABS MIDICPS NEG")
-- > map (is_unary CI) (words "- RAND")
-- > map (is_unary CI) (words "arctan atan")
is_unary :: Base.Case_Rule -> String -> Bool
is_unary cr = isJust . unaryIndex cr

-- * Binary

-- | Enumeration of @SC3@ unary operator UGens.
--
-- > map show [minBound :: SC3_Binary_Op .. maxBound]
data SC3_Binary_Op
            = Add -- 0
            | Sub -- 1
            | Mul -- 2
            | IDiv
            | FDiv -- 4
            | Mod -- 5
            | EQ_ -- 6
            | NE -- 7
            | LT_ -- 8
            | GT_ -- 9
            | LE -- 10
            | GE -- 11
            | Min -- 12
            | Max
            | BitAnd
            | BitOr
            | BitXor
            | LCM
            | GCD
            | Round
            | RoundUp
            | Trunc
            | Atan2
            | Hypot
            | Hypotx
            | Pow -- 25
            | ShiftLeft
            | ShiftRight
            | UnsignedShift
            | Fill
            | Ring1
            | Ring2
            | Ring3
            | Ring4
            | DifSqr
            | SumSqr
            | SqrSum
            | SqrDif
            | AbsDif
            | Thresh
            | AMClip
            | ScaleNeg
            | Clip2
            | Excess
            | Fold2
            | Wrap2
            | FirstArg
            | RandRange
            | ExpRandRange
              deriving (Eq,Show,Enum,Bounded,Read)

-- | Table of operator names (non-symbolic) and indices.
sc3_binary_op_tbl :: [(String,Int)]
sc3_binary_op_tbl = zip (map show [Add .. ExpRandRange]) [0..]

-- | Type-specialised 'parse_enum'.
parse_binary :: Base.Case_Rule -> String -> Maybe SC3_Binary_Op
parse_binary = Base.parse_enum

-- | Table of symbolic names for standard binary operators.
binary_sym_tbl :: [(SC3_Binary_Op,String)]
binary_sym_tbl =
    [(Add,"+")
    ,(Sub,"-")
    ,(Mul,"*")
    ,(FDiv,"/")
    ,(Mod,"%")
    ,(EQ_,"==")
    ,(NE,"/=") -- !=
    ,(LT_,"<")
    ,(GT_,">")
    ,(LE,"<=")
    ,(GE,">=")
    ,(BitAnd,".&.") -- &
    ,(BitOr,".|.") -- |
    ,(Pow,"**")]

-- | Table of operator names (non-symbolic) and indices.
--
-- > map fst sc3_binary_op_sym_tbl
sc3_binary_op_sym_tbl :: [(String,Int)]
sc3_binary_op_sym_tbl =
  let f x = maybe (show x) id (lookup x binary_sym_tbl)
  in zip (map f [Add .. ExpRandRange]) [0..]

-- | Lookup possibly symbolic name for standard binary operators.
--
-- > map binaryName [1,2,8,12] == ["-","*","<","Min"]
binaryName :: Int -> String
binaryName n =
    let e = toEnum n
    in fromMaybe (show e) (lookup e binary_sym_tbl)

-- | Given name of binary operator derive index.
--
-- > mapMaybe (binaryIndex Base.CI) (words "* MUL RING1 +") == [2,2,30,0]
-- > binaryIndex Base.CI "SINOSC" == Nothing
-- > map (\x -> (x,binaryIndex Base.CI x)) (map snd binary_sym_tbl)
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

-- | Lookup operator name for operator UGens, else UGen name.
ugen_operator_name :: String -> Int -> Maybe String
ugen_operator_name nm n =
    case nm of
      "UnaryOpUGen" -> Just (unaryName n)
      "BinaryOpUGen" -> Just (binaryName n)
      _ -> Nothing

-- | Order of lookup: binary then unary.
--
-- > map (resolve_operator Sound.SC3.Common.Base.CI) (words "+ - ADD SUB NEG")
resolve_operator :: Base.Case_Rule -> String -> (String,Maybe Int)
resolve_operator cr nm =
    case binaryIndex cr nm of
      Just sp -> ("BinaryOpUGen",Just sp)
      Nothing -> case unaryIndex cr nm of
                   Just sp -> ("UnaryOpUGen",Just sp)
                   _ -> (nm,Nothing)

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
    cpsMIDI :: a -> a
    cpsMIDI = Math.cps_to_midi
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
    midiCPS :: a -> a
    midiCPS = Math.midi_to_cps
    midiRatio :: a -> a
    midiRatio = Math.midi_to_ratio
    notE :: a -> a
    notE a = if a > 0.0 then 0.0 else 1.0
    notNil :: a -> a
    notNil a = if a /= 0.0 then 0.0 else 1.0
    octCPS :: a -> a
    octCPS = Math.oct_to_cps
    ramp_ :: a -> a
    ramp_ _ = error "ramp_"
    ratioMIDI :: a -> a
    ratioMIDI = Math.ratio_to_midi
    softClip :: a -> a
    softClip = Math.sc3_softclip
    squared :: a -> a
    squared = \z -> z * z

instance UnaryOp Float where
instance UnaryOp Double where

-- | SC3_Binary_Op operator class.
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

-- | Association table for 'SC3_Binary_Op' to haskell function implementing operator.
binop_hs_tbl :: (Real n,Floating n,RealFrac n) => [(SC3_Binary_Op,n -> n -> n)]
binop_hs_tbl =
    [(Add,(+))
    ,(Sub,(-))
    ,(FDiv,(/))
    ,(IDiv,Math.sc3_idiv)
    ,(Mod,Math.sc3_mod)
    ,(EQ_,Math.sc3_eq)
    ,(NE,Math.sc3_neq)
    ,(LT_,Math.sc3_lt)
    ,(LE,Math.sc3_lte)
    ,(GT_,Math.sc3_gt)
    ,(GE,Math.sc3_gte)
    ,(Min,min)
    ,(Max,max)
    ,(Mul,(*))
    ,(Pow,(**))
    ,(Min,min)
    ,(Max,max)
    ,(Round,Math.sc3_round_to)]

-- | 'lookup' 'binop_hs_tbl' via 'toEnum'.
binop_special_hs :: (RealFrac n,Floating n) => Int -> Maybe (n -> n -> n)
binop_special_hs z = lookup (toEnum z) binop_hs_tbl

-- | Association table for 'Unary' to haskell function implementing operator.
uop_hs_tbl :: (RealFrac n,Floating n) => [(SC3_Unary_Op,n -> n)]
uop_hs_tbl =
    [(Neg,negate)
    ,(Not,\z -> if z > 0 then 0 else 1)
    ,(Abs,abs)
    ,(Ceil,Math.sc3_ceiling)
    ,(Floor,Math.sc3_floor)
    ,(Squared,\z -> z * z)
    ,(Cubed,\z -> z * z * z)
    ,(Sqrt,sqrt)
    ,(Recip,recip)
    ,(MIDICPS,Math.midi_to_cps)
    ,(CPSMIDI,Math.cps_to_midi)
    ,(Sin,sin)
    ,(Cos,cos)
    ,(Tan,tan)]

-- | 'lookup' 'uop_hs_tbl' via 'toEnum'.
uop_special_hs :: (RealFrac n,Floating n) => Int -> Maybe (n -> n)
uop_special_hs z = lookup (toEnum z) uop_hs_tbl
