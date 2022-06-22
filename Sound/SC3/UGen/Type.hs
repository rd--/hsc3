-- | Unit generator and associated types and instances.
module Sound.SC3.UGen.Type where

import Data.Bits {- base -}
import Data.Either {- base -}
import qualified Data.Fixed as Fixed {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Safe {- safe -}
import qualified System.Random as Random {- random -}

import qualified Sound.SC3.Common.Math as Math
import Sound.SC3.Common.Math.Operator
import Sound.SC3.Common.Rate
import Sound.SC3.Common.Mce

import Sound.SC3.UGen.Brackets
import Sound.SC3.UGen.Constant
import Sound.SC3.UGen.Control
import Sound.SC3.UGen.Label
import Sound.SC3.UGen.Mrg
import Sound.SC3.UGen.Primitive
import Sound.SC3.UGen.Proxy

-- * Basic types

{- | SC3 samples are 32-bit 'Float'.
hsc3 uses 64-bit 'Double'.
If 'UGen' values are used more generally (ie. see hsc3-forth) 'Float' may be too imprecise, i.e. for representing time stamps.
-}
type Sample = Double

-- | Union type of Unit Generator forms.
data UGen
  = Constant_U Constant
  | Control_U Control
  | Label_U Label
  | Primitive_U (Primitive UGen)
  | Proxy_U (Proxy UGen)
  | Mce_U (Mce UGen)
  | Mrg_U (Mrg UGen)
  deriving (Eq,Read,Show)

instance EqE UGen where
    equal_to = mkBinaryOperator OpEq Math.sc3_eq
    not_equal_to = mkBinaryOperator OpNe Math.sc3_neq

instance OrdE UGen where
    less_than = mkBinaryOperator OpLt Math.sc3_lt
    less_than_or_equal_to = mkBinaryOperator OpLe Math.sc3_lte
    greater_than = mkBinaryOperator OpGt Math.sc3_gt
    greater_than_or_equal_to = mkBinaryOperator OpGe Math.sc3_gte

-- | 'UGen' form or 'Math.sc3_round_to'.
roundTo :: UGen -> UGen -> UGen
roundTo = mkBinaryOperator OpRound Math.sc3_round_to

instance RealFracE UGen where
    properFractionE = error "UGen.properFractionE"
    truncateE = error "UGen.truncateE"
    roundE i = roundTo i 1
    ceilingE = mkUnaryOperator OpCeil ceilingE
    floorE = mkUnaryOperator OpFloor floorE

instance UnaryOp UGen where
    ampDb = mkUnaryOperator OpAmpDb ampDb
    asFloat = mkUnaryOperator OpAsFloat asFloat
    asInt = mkUnaryOperator OpAsInt asInt
    cpsMidi = mkUnaryOperator OpCpsMidi cpsMidi
    cpsOct = mkUnaryOperator OpCpsOct cpsOct
    cubed = mkUnaryOperator OpCubed cubed
    dbAmp = mkUnaryOperator OpDbAmp dbAmp
    distort = mkUnaryOperator OpDistort distort
    frac = mkUnaryOperator OpFrac frac
    isNil = mkUnaryOperator OpIsNil isNil
    log10 = mkUnaryOperator OpLog10 log10
    log2 = mkUnaryOperator OpLog2 log2
    midiCps = mkUnaryOperator OpMidiCps midiCps
    midiRatio = mkUnaryOperator OpMidiRatio midiRatio
    notE = mkUnaryOperator OpNot notE
    notNil = mkUnaryOperator OpNotNil notNil
    octCps = mkUnaryOperator OpOctCps octCps
    ramp_ = mkUnaryOperator OpRamp_ ramp_
    ratioMidi = mkUnaryOperator OpRatioMidi ratioMidi
    softClip = mkUnaryOperator OpSoftClip softClip
    squared = mkUnaryOperator OpSquared squared

instance BinaryOp UGen where
    iDiv = mkBinaryOperator OpIdiv iDiv
    modE = mkBinaryOperator OpMod Fixed.mod'
    lcmE = mkBinaryOperator OpLcm lcmE
    gcdE = mkBinaryOperator OpGcd gcdE
    roundUp = mkBinaryOperator OpRoundUp roundUp
    trunc = mkBinaryOperator OpTrunc trunc
    atan2E = mkBinaryOperator OpAtan2 atan2E
    hypot = mkBinaryOperator OpHypot hypot
    hypotx = mkBinaryOperator OpHypotx hypotx
    fill = mkBinaryOperator OpFill fill
    ring1 = mkBinaryOperator OpRing1 ring1
    ring2 = mkBinaryOperator OpRing2 ring2
    ring3 = mkBinaryOperator OpRing3 ring3
    ring4 = mkBinaryOperator OpRing4 ring4
    difSqr = mkBinaryOperator OpDifSqr difSqr
    sumSqr = mkBinaryOperator OpSumSqr sumSqr
    sqrSum = mkBinaryOperator OpSqrSum sqrSum
    sqrDif = mkBinaryOperator OpSqrDif sqrDif
    absDif = mkBinaryOperator OpAbsDif absDif
    thresh = mkBinaryOperator OpThresh thresh
    amClip = mkBinaryOperator OpAmClip amClip
    scaleNeg = mkBinaryOperator OpScaleNeg scaleNeg
    clip2 = mkBinaryOperator OpClip2 clip2
    excess = mkBinaryOperator OpExcess excess
    fold2 = mkBinaryOperator OpFold2 fold2
    wrap2 = mkBinaryOperator OpWrap2 wrap2
    firstArg = mkBinaryOperator OpFirstArg firstArg
    randRange = mkBinaryOperator OpRandRange randRange
    exprandRange = mkBinaryOperator OpExpRandRange exprandRange

--instance MulAdd UGen where mul_add = mulAdd

-- * Parser

-- | 'constant' of 'parse_double'.
parse_constant :: String -> Maybe UGen
parse_constant = fmap constant . Math.parse_double

-- * Accessors

-- | See into 'Constant_U'.
un_constant :: UGen -> Maybe Constant
un_constant u =
    case u of
      Constant_U c -> Just c
      _ -> Nothing

-- | Value of 'Constant_U' 'Constant'.
u_constant :: UGen -> Maybe Sample
u_constant = fmap constantValue . un_constant

-- | Erroring variant.
u_constant_err :: UGen -> Sample
u_constant_err = fromMaybe (error "u_constant") . u_constant

-- * Mrg

-- | Multiple root graph constructor.
mrg :: [UGen] -> UGen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> Mrg_U (Mrg x (mrg xs))

-- | See into 'Mrg_U', follows leftmost rule until arriving at non-Mrg node.
mrg_leftmost :: UGen -> UGen
mrg_leftmost u =
    case u of
      Mrg_U m -> mrg_leftmost (mrgLeft m)
      _ -> u

-- * Predicates

-- | Constant node predicate.
isConstant :: UGen -> Bool
isConstant = isJust . un_constant

-- | True if input is a sink 'UGen', ie. has no outputs.  Sees into Mrg.
isSink :: UGen -> Bool
isSink u =
    case mrg_leftmost u of
      Primitive_U p -> null (ugenOutputs p)
      Mce_U m -> all isSink (mce_to_list m)
      _ -> False

-- | See into 'Proxy_U'.
un_proxy :: UGen -> Maybe (Proxy UGen)
un_proxy u =
    case u of
      Proxy_U p -> Just p
      _ -> Nothing

-- | Is 'UGen' a 'Proxy'?
isProxy :: UGen -> Bool
isProxy = isJust . un_proxy

-- | Get Primitive from UGen if UGen is a Primitive.
ugenPrimitive :: UGen -> Maybe (Primitive UGen)
ugenPrimitive u =
  case u of
    Primitive_U p -> Just p
    _ -> Nothing

-- | Is 'UGen' a 'Primitive'?
isPrimitive :: UGen -> Bool
isPrimitive = isJust . ugenPrimitive

-- * Mce

-- | Multiple channel expansion node constructor.
mce :: [UGen] -> UGen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> Mce_U (Mce_Scalar x)
      _ -> Mce_U (mce_from_list xs)

-- | Type specified 'mce_to_list'.
mceProxies :: Mce UGen -> [UGen]
mceProxies = mce_to_list

-- | Multiple channel expansion node ('Mce_U') predicate.  Sees into Mrg.
isMce :: UGen -> Bool
isMce u =
    case mrg_leftmost u of
      Mce_U _ -> True
      _ -> False

-- | Output channels of UGen as a list.  If required, preserves the RHS of and Mrg node in channel 0.
mceChannels :: UGen -> [UGen]
mceChannels u =
    case u of
      Mce_U m -> mce_to_list m
      Mrg_U (Mrg x y) -> let r:rs = mceChannels x in Mrg_U (Mrg r y) : rs
      _ -> [u]

-- | Number of channels to expand to.  This function sees into Mrg, and is defined only for Mce nodes.
mceDegree :: UGen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      Mce_U m -> Just (length (mceProxies m))
      _ -> Nothing

-- | Erroring variant.
mceDegree_err :: UGen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree

-- | Extend UGen to specified degree.  Follows "leftmost" rule for Mrg nodes.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n u =
    case u of
      Mce_U m -> mceProxies (mce_extend n m)
      Mrg_U (Mrg x y) -> let (r:rs) = mceExtend n x
                         in Mrg_U (Mrg r y) : rs
      _ -> replicate n u

-- | Is Mce required, ie. are any input values Mce?
mceRequired :: [UGen] -> Bool
mceRequired = any isMce

{- | Apply Mce transform to a list of inputs.
The transform extends each input so all are of equal length, and then transposes the matrix.

> mceInputTransform [mce2 1 2,mce2 3 4] == Just [[1,3],[2,4]]
> mceInputTransform [mce2 1 2,mce2 3 4,mce3 5 6 7] == Just [[1,3,5],[2,4,6],[1,3,7]]
> mceInputTransform [mce2 (mce2 1 2) (mce2 3 4),mce2 5 6] == Just [[mce2 1 2,5],[mce2 3 4,6]]
-}
mceInputTransform :: [UGen] -> Maybe [[UGen]]
mceInputTransform i =
    if mceRequired i
    then let n = maximum (map mceDegree_err (filter isMce i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

-- | Build a UGen after Mce transformation of inputs.
mceBuild :: ([UGen] -> UGen) -> [UGen] -> UGen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> let xs = map (mceBuild f) i' in Mce_U (mce_from_list xs)

{- | True if Mce is an immediate proxy for a multiple-out Primitive.
This is useful when disassembling graphs, ie. ugen_graph_forth_pp at hsc3-db.
It's also useful when editing a Primitive after it is constructed, as in bracketUGen.
-}
mce_is_direct_proxy :: Mce UGen -> Bool
mce_is_direct_proxy m =
    case m of
      Mce_Scalar _ -> False
      Mce_Vector _ ->
          let p = map un_proxy (mce_to_list m)
              p' = catMaybes p
          in all isJust p &&
             length (nub (map proxySource p')) == 1 &&
             map proxyIndex p' `isPrefixOf` [0..]

-- * Bracketed

{- | Attach Brackets (initialisation and cleanup message sequences) to UGen.
     For simplicity and clarity, brackets can only be attached to Primitive, Constant and Control nodes.
     This will look into the direct (immediate) proxies of a Primitive.
-}
bracketUGen :: UGen -> Brackets -> UGen
bracketUGen u (pre, post) =
  let err = error "bracketUGen: only Constants or Primitive UGens or immediate proxies may have brackets"
      rw_proxy pxy =
        case pxy of
          Proxy_U (Proxy p pix) ->
            let (lhs, rhs) = primitiveBrackets p in Proxy_U (Proxy (p {primitiveBrackets = (lhs ++ pre, rhs ++ post)}) pix)
          _ -> err
  in case u of
       Constant_U c -> let (lhs, rhs) = constantBrackets c in Constant_U (c {constantBrackets = (lhs ++ pre, rhs ++ post)})
       Control_U c -> let (lhs, rhs) = controlBrackets c in Control_U (c {controlBrackets = (lhs ++ pre, rhs ++ post)})
       Primitive_U p -> let (lhs, rhs) = primitiveBrackets p in Primitive_U (p {primitiveBrackets = (lhs ++ pre, rhs ++ post)})
       Mce_U m ->
         if mce_is_direct_proxy m
         then Mce_U (mce_map rw_proxy m)
         else err
       _ -> err

-- | Retrieve Brackets from UGen.
ugenBrackets :: UGen -> Brackets
ugenBrackets u =
  case u of
    Constant_U c -> constantBrackets c
    Control_U c -> controlBrackets c
    Primitive_U p -> primitiveBrackets p
    _ -> emptyBrackets

-- * Validators

-- | Ensure input 'UGen' is valid, ie. not a sink.
checkInput :: UGen -> UGen
checkInput u =
    if isSink u
    then error ("checkInput: " ++ show u)
    else u

-- * Constructors

-- | Constant value node constructor.
constant :: Real n => n -> UGen
constant = Constant_U . flip Constant emptyBrackets . realToFrac

-- | Type specialised 'constant'.
int_to_ugen :: Int -> UGen
int_to_ugen = constant

-- | Type specialised 'constant'.
float_to_ugen :: Float -> UGen
float_to_ugen = constant

-- | Type specialised 'constant'.
double_to_ugen :: Double -> UGen
double_to_ugen = constant

-- | Unit generator proxy node constructor.
proxy :: UGen -> Int -> UGen
proxy u n =
    case u of
      Primitive_U p -> Proxy_U (Proxy p n)
      _ -> error "proxy: not primitive?"

-- | Determine the rate of a UGen.
rateOf :: UGen -> Rate
rateOf u =
    case u of
      Constant_U _ -> InitialisationRate
      Control_U c -> controlOperatingRate c
      Label_U _ -> InitialisationRate
      Primitive_U p -> ugenRate p
      Proxy_U p -> ugenRate (proxySource p)
      Mce_U _ -> maximum (map rateOf (mceChannels u))
      Mrg_U m -> rateOf (mrgLeft m)

-- | Apply proxy transformation if required.
proxify :: UGen -> UGen
proxify u =
    case u of
      Mce_U m -> mce (map proxify (mce_to_list m))
      Mrg_U m -> mrg [proxify (mrgLeft m), mrgRight m]
      Primitive_U p ->
          let o = ugenOutputs p
          in case o of
               _:_:_ -> mce (map (proxy u) [0 .. length o - 1])
               _ -> u
      Constant_U _ -> u
      _ -> error "proxify: illegal ugen"

{- | Filters with DemandRate inputs run at ControlRate.
This is a little unfortunate, it'd be nicer if the rate in this circumstance could be given.
-}
mk_ugen_select_rate :: String -> [UGen] -> [Rate] -> Either Rate [Int] -> Rate
mk_ugen_select_rate nm h rs r =
  let r' = either id (maximum . map (rateOf . Safe.atNote ("mkUGen: " ++ nm) h)) r
  in if isRight r && r' == DemandRate && DemandRate `notElem` rs
     then if ControlRate `elem` rs then ControlRate else error "mkUGen: DemandRate input to non-ControlRate filter"
     else if r' `elem` rs || r' == DemandRate
          then r'
          else error ("mkUGen: rate restricted: " ++ show (r,r',rs,nm))

{- | Construct proxied and multiple channel expanded UGen.

cf = constant function, rs = rate set, r = rate, nm = name, i = inputs, i_mce = list of Mce inputs, o = outputs.
-}
mkUGen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [UGen] -> Maybe [UGen] -> Int -> Special -> UGenId -> UGen
mkUGen cf rs r nm i i_mce o s z =
    let i' = maybe i ((i ++) . concatMap mceChannels) i_mce
        f h = let r' = mk_ugen_select_rate nm h rs r
                  o' = replicate o r'
                  u = Primitive_U (Primitive r' nm h o' s z emptyBrackets)
              in case cf of
                   Just cf' ->
                     if all isConstant h
                     then constant (cf' (mapMaybe u_constant h))
                     else u
                   Nothing -> u
    in proxify (mceBuild f (map checkInput i'))

-- * Operators

-- | Operator UGen constructor.
mkOperator :: ([Sample] -> Sample) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUGen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId

-- | Unary math constructor.
mkUnaryOperator :: SC3_Unary_Op -> (Sample -> Sample) -> UGen -> UGen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUGen" [a] (fromEnum i)

-- | Binary math constructor with constant optimisation.
--
-- > constant 2 * constant 3 == constant 6
--
-- > let o = sinOsc ar 440 0
--
-- > o * 1 == o && 1 * o == o && o * 2 /= o
-- > o + 0 == o && 0 + o == o && o + 1 /= o
-- > o - 0 == o && 0 - o /= o
-- > o / 1 == o && 1 / o /= o
-- > o ** 1 == o && o ** 2 /= o
mkBinaryOperator_optimise_constants :: SC3_Binary_Op -> (Sample -> Sample -> Sample) ->
                                       (Either Sample Sample -> Bool) ->
                                       UGen -> UGen -> UGen
mkBinaryOperator_optimise_constants i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (Constant_U (Constant a' ([],[])),_) ->
                 if o (Left a') then Just b else Nothing
             (_,Constant_U (Constant b' ([],[]))) ->
                 if o (Right b') then Just a else Nothing
             _ -> Nothing
   in fromMaybe (mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)) r

-- | Plain (non-optimised) binary math constructor.
mkBinaryOperator :: SC3_Binary_Op -> (Sample -> Sample -> Sample) -> UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

-- * Numeric instances

-- | Is /u/ the primitive for the named UGen.
is_primitive_for :: String -> UGen -> Bool
is_primitive_for k u =
    case u of
      Primitive_U (Primitive _ nm [_,_] [_] _ _ _) -> nm == k
      _ -> False

-- | Is /u/ the primitive for the named UGen.
is_constant_of :: Sample -> UGen -> Bool
is_constant_of k u =
    case u of
      Constant_U c -> constantValue c == k
      _ -> False

-- | Is /u/ a binary math operator with SPECIAL of /k/.
is_math_binop :: Int -> UGen -> Bool
is_math_binop k u =
    case u of
      Primitive_U (Primitive _ "BinaryOpUGen" [_,_] [_] (Special s) NoId _) -> s == k
      _ -> False

-- | Is /u/ an ADD operator?
is_add_operator :: UGen -> Bool
is_add_operator = is_math_binop 0

assert_is_add_operator :: String -> UGen -> UGen
assert_is_add_operator msg u = if is_add_operator u then u else error ("assert_is_add_operator: " ++ msg)

-- | Is /u/ an MUL operator?
is_mul_operator :: UGen -> Bool
is_mul_operator = is_math_binop 2

{- | MulAdd re-writer, applicable only directly at add operator UGen.
The MulAdd UGen is very sensitive to input rates.
Add=AudioRate with In|Mul=InitialisationRate|Const will crash scsynth.
This only considers primitives that do not have bracketing messages.
-}
mul_add_optimise_direct :: UGen -> UGen
mul_add_optimise_direct u =
  let reorder (i,j,k) =
        let (ri,rj,rk) = (rateOf i,rateOf j,rateOf k)
        in if rk > max ri rj
           then Nothing
           else Just (max (max ri rj) rk,if rj > ri then (j,i,k) else (i,j,k))
  in case assert_is_add_operator "MUL-ADD" u of
       Primitive_U
         (Primitive _ _ [Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId ([],[])),k] [_] _ NoId ([],[])) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> Primitive_U (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId ([],[]))
           Nothing -> u
       Primitive_U
         (Primitive _ _ [k,Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId ([],[]))] [_] _ NoId ([],[])) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> Primitive_U (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId ([],[]))
           Nothing -> u
       _ -> u

{- | MulAdd optimiser, applicable at any UGen (ie. checks /u/ is an ADD ugen)

> import Sound.SC3
> g1 = sinOsc ar 440 0 * 0.1 + control ir "x" 0.05
> g2 = sinOsc ar 440 0 * control ir "x" 0.1 + 0.05
> g3 = control ir "x" 0.1 * sinOsc ar 440 0 + 0.05
> g4 = 0.05 + sinOsc ar 440 0 * 0.1
-}
mul_add_optimise :: UGen -> UGen
mul_add_optimise u = if is_add_operator u then mul_add_optimise_direct u else u

{- | Sum3 re-writer, applicable only directly at add operator UGen.
     This only considers nodes that have no bracketing messages.
-}
sum3_optimise_direct :: UGen -> UGen
sum3_optimise_direct u =
  case assert_is_add_operator "SUM3" u of
    Primitive_U (Primitive r _ [Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId ([],[])),k] [_] _ NoId ([],[])) ->
      Primitive_U (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[]))
    Primitive_U (Primitive r _ [k,Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId ([],[]))] [_] _ NoId ([],[])) ->
      Primitive_U (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[]))
    _ -> u

-- | /Sum3/ optimiser, applicable at any /u/ (ie. checks if /u/ is an ADD operator).
sum3_optimise :: UGen -> UGen
sum3_optimise u = if is_add_operator u then sum3_optimise_direct u else u

-- | 'sum3_optimise' of 'mul_add_optimise'.
add_optimise :: UGen -> UGen
add_optimise = sum3_optimise . mul_add_optimise

-- | Unit generators are numbers.
instance Num UGen where
    negate = mkUnaryOperator OpNeg negate
    (+) = fmap add_optimise .
          mkBinaryOperator_optimise_constants OpAdd (+) (`elem` [Left 0,Right 0])
    (-) = mkBinaryOperator_optimise_constants OpSub (-) (Right 0 ==)
    (*) = mkBinaryOperator_optimise_constants OpMul (*) (`elem` [Left 1,Right 1])
    abs = mkUnaryOperator OpAbs abs
    signum = mkUnaryOperator OpSign signum
    fromInteger = Constant_U . flip Constant ([],[]) . fromInteger

-- | Unit generators are fractional.
instance Fractional UGen where
    recip = mkUnaryOperator OpRecip recip
    (/) = mkBinaryOperator_optimise_constants OpFdiv (/) (Right 1 ==)
    fromRational = Constant_U . flip Constant ([],[]) . fromRational

-- | Unit generators are floating point.
instance Floating UGen where
    pi = Constant_U (Constant pi ([],[]))
    exp = mkUnaryOperator OpExp exp
    log = mkUnaryOperator OpLog log
    sqrt = mkUnaryOperator OpSqrt sqrt
    (**) = mkBinaryOperator_optimise_constants OpPow (**) (Right 1 ==)
    logBase a b = log b / log a
    sin = mkUnaryOperator OpSin sin
    cos = mkUnaryOperator OpCos cos
    tan = mkUnaryOperator OpTan tan
    asin = mkUnaryOperator OpArcSin asin
    acos = mkUnaryOperator OpArcCos acos
    atan = mkUnaryOperator OpArcTan atan
    sinh = mkUnaryOperator OpSinh sinh
    cosh = mkUnaryOperator OpCosh cosh
    tanh = mkUnaryOperator OpTanh tanh
    asinh x = log (sqrt (x*x+1) + x)
    acosh x = log (sqrt (x*x-1) + x)
    atanh x = (log (1+x) - log (1-x)) / 2

-- | Unit generators are real.
instance Real UGen where
    toRational (Constant_U (Constant n ([],[]))) = toRational n
    toRational _ = error "UGen.toRational: only un-bracketed constants considered"

-- | Unit generators are integral.
instance Integral UGen where
    quot = mkBinaryOperator OpIdiv (error "UGen.quot")
    rem = mkBinaryOperator OpMod (error "UGen.rem")
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator OpIdiv (error "UGen.div")
    mod = mkBinaryOperator OpMod (error "UGen.mod")
    toInteger (Constant_U (Constant n ([],[]))) = floor n
    toInteger _ = error "UGen.toInteger: only un-bracketed constants considered"

instance RealFrac UGen where
  properFraction = error "UGen.properFraction, see properFractionE"
  round = error "UGen.round, see roundE"
  ceiling = error "UGen.ceiling, see ceilingE"
  floor = error "UGen.floor, see floorE"

-- | Unit generators are orderable (when 'Constants').
--
-- > (constant 2 > constant 1) == True
instance Ord UGen where
    (Constant_U a) < (Constant_U b) = a < b
    _ < _ = error "UGen.<, see <*"
    (Constant_U a) <= (Constant_U b) = a <= b
    _ <= _ = error "UGen.<= at, see <=*"
    (Constant_U a) > (Constant_U b) = a > b
    _ > _ = error "UGen.>, see >*"
    (Constant_U a) >= (Constant_U b) = a >= b
    _ >= _ = error "UGen.>=, see >=*"
    min = mkBinaryOperator OpMin min
    max = mkBinaryOperator OpMax max

-- | Unit generators are enumerable.
instance Enum UGen where
    succ u = u + 1
    pred u = u - 1
    toEnum n = Constant_U (Constant (fromIntegral n) ([],[]))
    fromEnum (Constant_U (Constant n ([],[]))) = truncate n
    fromEnum _ = error "UGen.fromEnum: non-constant"
    enumFrom = iterate (+1)
    enumFromThen n m = iterate (+(m-n)) n
    enumFromTo n m = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m =
        let p = if n' >= n then (>=) else (<=)
        in takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')

{- | Unit generators are stochastic.
Only un-bracketed constant values are considered.
-}
instance Random.Random UGen where
    randomR (Constant_U (Constant l ([],[])), Constant_U (Constant r ([],[]))) g =
        let (n, g') = Random.randomR (l,r) g
        in (Constant_U (Constant n ([],[])), g')
    randomR _ _ = error "UGen.randomR: non constant (l,r)"
    random = Random.randomR (-1.0, 1.0)

-- | UGens are bit patterns.
instance Bits UGen where
    (.&.) = mkBinaryOperator OpBitAnd undefined
    (.|.) = mkBinaryOperator OpBitOr undefined
    xor = mkBinaryOperator OpBitXor undefined
    complement = mkUnaryOperator OpBitNot undefined
    shift = error "UGen.shift"
    rotate = error "UGen.rotate"
    bitSize = error "UGen.bitSize"
    bit = error "UGen.bit"
    testBit = error "UGen.testBit"
    popCount = error "UGen.popCount"
    bitSizeMaybe = error "UGen.bitSizeMaybe"
    isSigned _ = True

{-
import qualified GHC.Exts as Exts {- base -}

instance Exts.IsList UGen where
  type Item UGen = UGen
  fromList = mce
  toList = mceChannels
-}
