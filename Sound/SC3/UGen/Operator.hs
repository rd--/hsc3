-- | Enumerations of the unary and binary math unit generators.  Names
-- that conflict with existing names have a @_@ suffix.
module Sound.SC3.UGen.Operator where

import Control.Monad {- base -}
import Data.Maybe {- base -}

import Sound.SC3.Common.Prelude {- hsc3 -}

-- * Unary

-- | Enumeration of @SC3@ unary operator UGens.
data Unary  = Neg
            | Not
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

-- | Type-specialised 'parse_enum'.
parse_unary :: Case_Rule -> String -> Maybe Unary
parse_unary = parse_enum

-- | Table of symbolic names for standard unary operators.
unaryTable :: [(Unary,String)]
unaryTable = [] -- (Neg,"-")

-- | Lookup possibly symbolic name for standard unary operators.
unaryName :: Int -> String
unaryName n =
    let e = toEnum n
    in fromMaybe (show e) (lookup e unaryTable)

-- | Given name of unary operator derive index.
--
-- > mapMaybe (unaryIndex CI) (words "NEG CUBED") == [0,13]
-- > unaryIndex CS "SinOsc" == Nothing
unaryIndex :: Case_Rule -> String -> Maybe Int
unaryIndex cr nm =
    let ix = rlookup_str cr nm unaryTable
        ix' = parse_unary cr nm
    in fmap fromEnum (mplus ix' ix)

-- | 'isJust' of 'unaryIndex'.
--
-- > map (is_unary CI) (words "ABS MIDICPS NEG")
-- > map (is_unary CI) (words "- RAND")
is_unary :: Case_Rule -> String -> Bool
is_unary cr = isJust . unaryIndex cr

-- * Binary

-- | Enumeration of @SC3@ unary operator UGens.
data Binary = Add -- 0
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

-- | Type-specialised 'parse_enum'.
parse_binary :: Case_Rule -> String -> Maybe Binary
parse_binary = parse_enum

-- | Table of symbolic names for standard binary operators.
binaryTable :: [(Binary,String)]
binaryTable =
    [(Add,"+")
    ,(Sub,"-")
    ,(Mul,"*")
    ,(FDiv,"/")
    ,(Mod,"%")
    ,(EQ_,"==")
    ,(NE,"/=")
    ,(LT_,"<")
    ,(GT_,">")
    ,(LE,"<=")
    ,(GE,">=")
    ,(Pow,"**")]

-- | Lookup possibly symbolic name for standard binary operators.
--
-- > map binaryName [1,2,8,12] == ["-","*","<","Min"]
binaryName :: Int -> String
binaryName n =
    let e = toEnum n
    in fromMaybe (show e) (lookup e binaryTable)

-- | Given name of binary operator derive index.
--
-- > mapMaybe (binaryIndex CI) (words "* MUL RING1") == [2,2,30]
-- > binaryIndex CI "SINOSC" == Nothing
binaryIndex :: Case_Rule -> String -> Maybe Int
binaryIndex cr nm =
    let ix = rlookup_str cr nm binaryTable
        ix' = parse_binary cr nm
    in fmap fromEnum (mplus ix' ix)

-- | 'isJust' of 'binaryIndex'.
--
-- > map (is_binary CI) (words "== > % TRUNC MAX")
is_binary :: Case_Rule -> String -> Bool
is_binary cr = isJust . binaryIndex cr

-- * Operator

-- | Order of lookup: binary then unary.
--
-- > map (resolve_operator CI) (words "+ - ADD SUB NEG")
resolve_operator :: Case_Rule -> String -> (String,Maybe Int)
resolve_operator cr nm =
    case binaryIndex cr nm of
      Just sp -> ("BinaryOpUGen",Just sp)
      Nothing -> case unaryIndex cr nm of
                   Just sp -> ("UnaryOpUGen",Just sp)
                   _ -> (nm,Nothing)
