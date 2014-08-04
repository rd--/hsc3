-- | Enumerations of the unary and binary math unit generators.  Names
-- that conflict with existing names have a @_@ suffix.
module Sound.SC3.UGen.Operator where

import Data.Maybe {- base -}
import Data.List {- base -}

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
            | Ramp
            | SCurve
              deriving (Eq,Show,Enum,Bounded,Read)

-- | Type-specialised 'reads_exact'.
parse_unary :: String -> Maybe Unary
parse_unary = reads_exact

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
-- > mapMaybe unaryIndex ["Neg","Cubed"] == [0,13]
-- > unaryIndex "SinOsc" == Nothing
unaryIndex :: String -> Maybe Int
unaryIndex nm =
    let ix = rlookup nm unaryTable
        ix' = parse_unary nm
    in fmap fromEnum (maybe ix' Just ix)

-- | 'isJust' of 'unaryIndex'.
--
-- > map is_uop (words "Abs MIDICPS Neg")
-- > map is_uop (words "- Rand")
is_unary :: String -> Bool
is_unary = isJust . unaryIndex

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
            | Min
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

-- | Type-specialised 'reads_exact'.
parse_binary :: String -> Maybe Binary
parse_binary = reads_exact

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
-- > map binaryName [1,2,8] == ["-","*","<"]
binaryName :: Int -> String
binaryName n =
    let e = toEnum n
    in fromMaybe (show e) (lookup e binaryTable)

-- | Given name of binary operator derive index.
--
-- > mapMaybe binaryIndex ["*","Mul","Ring1"] == [2,2,30]
-- > binaryIndex "SinOsc" == Nothing
binaryIndex :: String -> Maybe Int
binaryIndex nm =
    let ix = rlookup nm binaryTable
        ix' = parse_binary nm
    in fmap fromEnum (maybe ix' Just ix)

-- | 'isJust' of 'binaryIndex'.
--
-- > map is_binary (words "== > % Trunc Max")
is_binary :: String -> Bool
is_binary = isJust . binaryIndex

-- * Operator

-- | Order of lookup: binary then unary.
--
-- > map resolve_operator (words "+ - Add Sub Neg")
resolve_operator :: String -> (String,Maybe Int)
resolve_operator nm =
    case binaryIndex nm of
      Just sp -> ("BinaryOpUGen",Just sp)
      Nothing -> case unaryIndex nm of
                   Just sp -> ("UnaryOpUGen",Just sp)
                   _ -> (nm,Nothing)

-- * UTIL

-- | Variant of 'reads' requiring exact match.
reads_exact :: Read a => String -> Maybe a
reads_exact s =
    case reads s of
      [(r,"")] -> Just r
      _ -> Nothing

-- | Reverse 'lookup'.
rlookup :: Eq b => b -> [(a,b)] -> Maybe a
rlookup x = fmap fst . find ((== x) . snd)
