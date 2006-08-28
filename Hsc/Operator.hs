module Hsc.Operator where

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
            | Rand
            | Rand2
            | LinRand
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
              deriving (Eq, Show, Enum)

data Binary = Add
            | Sub
            | Mul
            | IDiv
            | FDiv
            | Mod
            | EQ
            | NE
            | LT
            | GT
            | LE
            | GE
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
            | Pow
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
              deriving (Eq, Show, Enum)

unaryName :: Int -> String
unaryName   0 = "-"
unaryName   n = show (toEnum n :: Unary)

binaryName :: Int -> String
binaryName  0 = "+"
binaryName  1 = "-"
binaryName  2 = "*"
binaryName  4 = "/"
binaryName  5 = "%"
binaryName  6 = "=="
binaryName  7 = "/="
binaryName  8 = "<"
binaryName  9 = ">"
binaryName 10 = "<="
binaryName 11 = ">="
binaryName 25 = "**"
binaryName n  = show (toEnum n :: Binary)
