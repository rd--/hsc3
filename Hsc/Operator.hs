module Hsc.Operator where

data UOp = Neg 
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

data BOp = Add
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

uOpName  0 = "-"
uOpName  n = show (toEnum n :: UOp)

bOpName  0 = "+"
bOpName  1 = "-"
bOpName  2 = "*"
bOpName  4 = "/"
bOpName  5 = "%"
bOpName  6 = "=="
bOpName  7 = "/="
bOpName  8 = "<"
bOpName  9 = ">"
bOpName 10 = "<="
bOpName 11 = ">="
bOpName 25 = "**"
bOpName n  = show (toEnum n :: BOp)

