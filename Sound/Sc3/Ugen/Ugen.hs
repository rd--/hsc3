-- | Unit generator (Ugen) type and instances.
module Sound.Sc3.Ugen.Ugen where

import Data.Bits {- base -}
import qualified Data.Fixed as Fixed {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified System.Random as Random {- random -}

import qualified Sound.Sc3.Common.Math as Math
import Sound.Sc3.Common.Math.Operator
import Sound.Sc3.Common.Rate
import Sound.Sc3.Common.Mce

import Sound.Sc3.Ugen.Brackets
import Sound.Sc3.Ugen.Constant
import Sound.Sc3.Ugen.Control
import Sound.Sc3.Ugen.Label
import Sound.Sc3.Ugen.Mrg
import Sound.Sc3.Ugen.Primitive
import Sound.Sc3.Ugen.Proxy

-- * Basic types

{- | Sc3 samples are 32-bit 'Float'.
hsc3 uses 64-bit 'Double'.
If 'Ugen' values are used more generally (ie. see hsc3-forth) 'Float' may be too imprecise, i.e. for representing time stamps.
-}
type Sample = Double

-- | Union type of Unit Generator forms.
data Ugen
  = Constant_U Constant
  | Control_U Control
  | Label_U Label
  | Primitive_U (Primitive Ugen)
  | Proxy_U (Proxy Ugen)
  | Mce_U (Mce Ugen)
  | Mrg_U (Mrg Ugen)
  deriving (Eq,Read,Show)

-- * Name

-- | Lookup operator name for operator Ugens, else Ugen name.
ugen_user_name :: String -> Special -> String
ugen_user_name nm (Special n) = fromMaybe nm (ugen_operator_name nm n)

-- * Instances

instance EqE Ugen where
    equal_to = mkBinaryOperator OpEq Math.sc3_eq
    not_equal_to = mkBinaryOperator OpNe Math.sc3_neq

instance OrdE Ugen where
    less_than = mkBinaryOperator OpLt Math.sc3_lt
    less_than_or_equal_to = mkBinaryOperator OpLe Math.sc3_lte
    greater_than = mkBinaryOperator OpGt Math.sc3_gt
    greater_than_or_equal_to = mkBinaryOperator OpGe Math.sc3_gte

-- | 'Ugen' form or 'Math.sc3_round_to'.
roundTo :: Ugen -> Ugen -> Ugen
roundTo = mkBinaryOperator OpRoundTo Math.sc3_round_to

instance RealFracE Ugen where
    properFractionE = error "Ugen.properFractionE"
    truncateE = error "Ugen.truncateE"
    roundE i = roundTo i 1
    ceilingE = mkUnaryOperator OpCeil ceilingE
    floorE = mkUnaryOperator OpFloor floorE

instance UnaryOp Ugen where
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

instance BinaryOp Ugen where
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

--instance MulAdd Ugen where mul_add = mulAdd

-- * Parser

-- | 'constant' of 'parse_double'.
parse_constant :: String -> Maybe Ugen
parse_constant = fmap constant . Math.parse_double

-- * Accessors

-- | See into 'Constant_U'.
un_constant :: Ugen -> Maybe Constant
un_constant u =
    case u of
      Constant_U c -> Just c
      _ -> Nothing

-- | Value of 'Constant_U' 'Constant'.
u_constant :: Ugen -> Maybe Sample
u_constant = fmap constantValue . un_constant

-- | Erroring variant.
u_constant_err :: Ugen -> Sample
u_constant_err = fromMaybe (error "u_constant") . u_constant

-- * Mrg

-- | Multiple root graph constructor.
mrg :: [Ugen] -> Ugen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> Mrg_U (Mrg x (mrg xs))

-- | See into 'Mrg_U', follows leftmost rule until arriving at non-Mrg node.
mrg_leftmost :: Ugen -> Ugen
mrg_leftmost u =
    case u of
      Mrg_U m -> mrg_leftmost (mrgLeft m)
      _ -> u

-- * Predicates

-- | Constant node predicate.
isConstant :: Ugen -> Bool
isConstant = isJust . un_constant

-- | True if input is a sink 'Ugen', ie. has no outputs.  Sees into Mrg.
isSink :: Ugen -> Bool
isSink u =
    case mrg_leftmost u of
      Primitive_U p -> null (ugenOutputs p)
      Mce_U m -> all isSink (mce_to_list m)
      _ -> False

-- | See into 'Proxy_U'.
un_proxy :: Ugen -> Maybe (Proxy Ugen)
un_proxy u =
    case u of
      Proxy_U p -> Just p
      _ -> Nothing

-- | Is 'Ugen' a 'Proxy'?
isProxy :: Ugen -> Bool
isProxy = isJust . un_proxy

-- | Get Primitive from Ugen if Ugen is a Primitive.
ugenPrimitive :: Ugen -> Maybe (Primitive Ugen)
ugenPrimitive u =
  case u of
    Primitive_U p -> Just p
    _ -> Nothing

-- | Is 'Ugen' a 'Primitive'?
isPrimitive :: Ugen -> Bool
isPrimitive = isJust . ugenPrimitive

-- * Mce

-- | Multiple channel expansion node constructor.
mce :: [Ugen] -> Ugen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> Mce_U (Mce_Scalar x)
      _ -> Mce_U (mce_from_list xs)

-- | Type specified 'mce_to_list'.
mceProxies :: Mce Ugen -> [Ugen]
mceProxies = mce_to_list

-- | Multiple channel expansion node ('Mce_U') predicate.  Sees into Mrg.
isMce :: Ugen -> Bool
isMce u =
    case mrg_leftmost u of
      Mce_U _ -> True
      _ -> False

-- | Output channels of Ugen as a list.  If required, preserves the RHS of an Mrg node in channel 0.
mceChannels :: Ugen -> [Ugen]
mceChannels u =
    case u of
      Mce_U m -> mce_to_list m
      Mrg_U (Mrg x y) ->
        case mceChannels x of
          r:rs -> Mrg_U (Mrg r y) : rs
          _ -> error "mceChannels"
      _ -> [u]

-- | Number of channels to expand to.  This function sees into Mrg, and is defined only for Mce nodes.
mceDegree :: Ugen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      Mce_U m -> Just (length (mceProxies m))
      _ -> Nothing

-- | Erroring variant.
mceDegree_err :: Ugen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree

-- | Extend Ugen to specified degree.  Follows "leftmost" rule for Mrg nodes.
mceExtend :: Int -> Ugen -> [Ugen]
mceExtend n u =
    case u of
      Mce_U m -> mceProxies (mce_extend n m)
      Mrg_U (Mrg x y) ->
        case mceExtend n x of
          r:rs -> Mrg_U (Mrg r y) : rs
          _ -> error "mceExtend"
      _ -> replicate n u

-- | Is Mce required, ie. are any input values Mce?
mceRequired :: [Ugen] -> Bool
mceRequired = any isMce

{- | Apply Mce transform to a list of inputs.
The transform extends each input so all are of equal length, and then transposes the matrix.

>>> mceInputTransform [mce [1, 2],mce [3, 4]] == Just [[1,3],[2,4]]
True

>>> mceInputTransform [mce [1, 2],mce [3, 4], mce [5, 6, 7]] == Just [[1,3,5],[2,4,6],[1,3,7]]
True

>>> mceInputTransform [mce [mce [1, 2], mce [3, 4]], mce [5, 6]] == Just [[mce [1, 2],5],[mce [3, 4],6]]
True
-}
mceInputTransform :: [Ugen] -> Maybe [[Ugen]]
mceInputTransform i =
    if mceRequired i
    then let n = maximum (map mceDegree_err (filter isMce i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

-- | Build a Ugen after Mce transformation of inputs.
mceBuild :: ([Ugen] -> Ugen) -> [Ugen] -> Ugen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> let xs = map (mceBuild f) i' in Mce_U (mce_from_list xs)

{- | True if Mce is an immediate proxy for a multiple-out Primitive.
This is useful when disassembling graphs, ie. ugen_graph_forth_pp at hsc3-db.
It's also useful when editing a Primitive after it is constructed, as in bracketUgen.
-}
mce_is_direct_proxy :: Mce Ugen -> Bool
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

{- | Attach Brackets (initialisation and cleanup message sequences) to Ugen.
     For simplicity and clarity, brackets can only be attached to Primitive, Constant and Control nodes.
     This will look into the direct (immediate) proxies of a Primitive.
-}
bracketUgen :: Ugen -> Brackets -> Ugen
bracketUgen u (pre, post) =
  let err = error "bracketUgen: only Constants or Primitive Ugens or immediate proxies may have brackets"
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

-- | Retrieve Brackets from Ugen.
ugenBrackets :: Ugen -> Brackets
ugenBrackets u =
  case u of
    Constant_U c -> constantBrackets c
    Control_U c -> controlBrackets c
    Primitive_U p -> primitiveBrackets p
    _ -> emptyBrackets

-- * Validators

-- | Ensure input 'Ugen' is valid, ie. not a sink.
checkInput :: Ugen -> Ugen
checkInput u =
    if isSink u
    then error ("checkInput: " ++ show u)
    else u

-- * Constructors

-- | Constant value node constructor.
constant :: Real n => n -> Ugen
constant = Constant_U . flip Constant emptyBrackets . realToFrac

-- | Type specialised 'constant'.
int_to_ugen :: Int -> Ugen
int_to_ugen = constant

-- | Type specialised 'constant'.
float_to_ugen :: Float -> Ugen
float_to_ugen = constant

-- | Type specialised 'constant'.
double_to_ugen :: Double -> Ugen
double_to_ugen = constant

-- | Unit generator proxy node constructor.
proxy :: Ugen -> Int -> Ugen
proxy u n =
    case u of
      Primitive_U p -> Proxy_U (Proxy p n)
      _ -> error "proxy: not primitive?"

-- | Determine the rate of a Ugen.
rateOf :: Ugen -> Rate
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
proxify :: Ugen -> Ugen
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
mk_ugen_select_rate :: String -> [Ugen] -> [Rate] -> Either Rate [Int] -> Rate
mk_ugen_select_rate nm h rs r =
  let at_note note list index = if index < 0 || index >= length list then error note else list !! index -- hugs...
      is_right e = case e of { Right _ -> True; _ -> False } -- hugs...
      r' = either id (maximum . map (rateOf . at_note ("mkUgen: " ++ nm) h)) r
  in if is_right r && r' == DemandRate && DemandRate `notElem` rs
     then if ControlRate `elem` rs then ControlRate else error "mkUgen: DemandRate input to non-ControlRate filter"
     else if r' `elem` rs || r' == DemandRate
          then r'
          else error ("mkUgen: rate restricted: " ++ show (r,r',rs,nm))

{- | Construct proxied and multiple channel expanded Ugen.

cf = constant function, rs = rate set, r = rate, nm = name, i = inputs, i_mce = list of Mce inputs, o = outputs.
-}
mkUgen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [Ugen] -> Maybe [Ugen] -> Int -> Special -> UgenId -> Ugen
mkUgen cf rs r nm i i_mce o s z =
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

-- | Operator Ugen constructor.
mkOperator :: ([Sample] -> Sample) -> String -> [Ugen] -> Int -> Ugen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUgen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId

-- | Unary math constructor.
mkUnaryOperator :: Sc3_Unary_Op -> (Sample -> Sample) -> Ugen -> Ugen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUGen" [a] (fromEnum i)

{- | Binary math constructor with constant optimisation.

>>> constant 2 * constant 3 == constant 6
True

>>> let o = mkUgen Nothing [AudioRate] (Left AudioRate) "SinOsc" [constant 440, constant 0] Nothing 1 (Special 0) (Uid 0)
>>> o * 1 == o && 1 * o == o && o * 2 /= o
True

>>> o + 0 == o && 0 + o == o && o + 1 /= o
True

>>> o - 0 == o && 0 - o /= o
True

>>> o / 1 == o && 1 / o /= o
True

>>> o ** 1 == o && o ** 2 /= o
True
-}
mkBinaryOperator_optimise_constants :: Sc3_Binary_Op -> (Sample -> Sample -> Sample) ->
                                       (Either Sample Sample -> Bool) ->
                                       Ugen -> Ugen -> Ugen
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
mkBinaryOperator :: Sc3_Binary_Op -> (Sample -> Sample -> Sample) -> Ugen -> Ugen -> Ugen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

-- * Numeric instances

-- | Is /u/ the primitive for the named Ugen.
is_primitive_for :: String -> Ugen -> Bool
is_primitive_for k u =
    case u of
      Primitive_U (Primitive _ nm [_,_] [_] _ _ _) -> nm == k
      _ -> False

-- | Is /u/ the primitive for the named Ugen.
is_constant_of :: Sample -> Ugen -> Bool
is_constant_of k u =
    case u of
      Constant_U c -> constantValue c == k
      _ -> False

-- | Is /u/ a binary math operator with SPECIAL of /k/.
is_math_binop :: Int -> Ugen -> Bool
is_math_binop k u =
    case u of
      Primitive_U (Primitive _ "BinaryOpUGen" [_,_] [_] (Special s) NoId _) -> s == k
      _ -> False

-- | Is /u/ an ADD operator?
is_add_operator :: Ugen -> Bool
is_add_operator = is_math_binop 0

assert_is_add_operator :: String -> Ugen -> Ugen
assert_is_add_operator msg u = if is_add_operator u then u else error ("assert_is_add_operator: " ++ msg)

-- | Is /u/ an MUL operator?
is_mul_operator :: Ugen -> Bool
is_mul_operator = is_math_binop 2

{- | MulAdd re-writer, applicable only directly at add operator Ugen.
The MulAdd Ugen is very sensitive to input rates.
Add=AudioRate with In|Mul=InitialisationRate|Const will crash scsynth.
This only considers primitives that do not have bracketing messages.
-}
mul_add_optimise_direct :: Ugen -> Ugen
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

{- | MulAdd optimiser, applicable at any Ugen (ie. checks /u/ is an ADD ugen)

> import Sound.Sc3
> g1 = sinOsc ar 440 0 * 0.1 + control ir "x" 0.05
> g2 = sinOsc ar 440 0 * control ir "x" 0.1 + 0.05
> g3 = control ir "x" 0.1 * sinOsc ar 440 0 + 0.05
> g4 = 0.05 + sinOsc ar 440 0 * 0.1
-}
mul_add_optimise :: Ugen -> Ugen
mul_add_optimise u = if is_add_operator u then mul_add_optimise_direct u else u

{- | Sum3 re-writer, applicable only directly at add operator Ugen.
     This only considers nodes that have no bracketing messages.
-}
sum3_optimise_direct :: Ugen -> Ugen
sum3_optimise_direct u =
  case assert_is_add_operator "SUM3" u of
    Primitive_U (Primitive r _ [Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId ([],[])),k] [_] _ NoId ([],[])) ->
      Primitive_U (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[]))
    Primitive_U (Primitive r _ [k,Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId ([],[]))] [_] _ NoId ([],[])) ->
      Primitive_U (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[]))
    _ -> u

-- | /Sum3/ optimiser, applicable at any /u/ (ie. checks if /u/ is an ADD operator).
sum3_optimise :: Ugen -> Ugen
sum3_optimise u = if is_add_operator u then sum3_optimise_direct u else u

-- | 'sum3_optimise' of 'mul_add_optimise'.
add_optimise :: Ugen -> Ugen
add_optimise = sum3_optimise . mul_add_optimise

-- | Unit generators are numbers.
instance Num Ugen where
    negate = mkUnaryOperator OpNeg negate
    (+) = fmap add_optimise .
          mkBinaryOperator_optimise_constants OpAdd (+) (`elem` [Left 0,Right 0])
    (-) = mkBinaryOperator_optimise_constants OpSub (-) (Right 0 ==)
    (*) = mkBinaryOperator_optimise_constants OpMul (*) (`elem` [Left 1,Right 1])
    abs = mkUnaryOperator OpAbs abs
    signum = mkUnaryOperator OpSign signum
    fromInteger = Constant_U . flip Constant ([],[]) . fromInteger

-- | Unit generators are fractional.
instance Fractional Ugen where
    recip = mkUnaryOperator OpRecip recip
    (/) = mkBinaryOperator_optimise_constants OpFdiv (/) (Right 1 ==)
    fromRational = Constant_U . flip Constant ([],[]) . fromRational

-- | Unit generators are floating point.
instance Floating Ugen where
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
instance Real Ugen where
    toRational (Constant_U (Constant n ([],[]))) = toRational n
    toRational _ = error "Ugen.toRational: only un-bracketed constants considered"

-- | Unit generators are integral.
instance Integral Ugen where
    quot = mkBinaryOperator OpIdiv (error "Ugen.quot")
    rem = mkBinaryOperator OpMod (error "Ugen.rem")
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator OpIdiv (error "Ugen.div")
    mod = mkBinaryOperator OpMod (error "Ugen.mod")
    toInteger (Constant_U (Constant n ([],[]))) = floor n
    toInteger _ = error "Ugen.toInteger: only un-bracketed constants considered"

instance RealFrac Ugen where
  properFraction = error "Ugen.properFraction, see properFractionE"
  round = error "Ugen.round, see roundE"
  ceiling = error "Ugen.ceiling, see ceilingE"
  floor = error "Ugen.floor, see floorE"

{- | Unit generators are orderable (when 'Constants').

>>> constant 2 > constant 1
True
-}
instance Ord Ugen where
    (Constant_U a) < (Constant_U b) = a < b
    _ < _ = error "Ugen.<, see <*"
    (Constant_U a) <= (Constant_U b) = a <= b
    _ <= _ = error "Ugen.<= at, see <=*"
    (Constant_U a) > (Constant_U b) = a > b
    _ > _ = error "Ugen.>, see >*"
    (Constant_U a) >= (Constant_U b) = a >= b
    _ >= _ = error "Ugen.>=, see >=*"
    min = mkBinaryOperator OpMin min
    max = mkBinaryOperator OpMax max

-- | Unit generators are enumerable.
instance Enum Ugen where
    succ u = u + 1
    pred u = u - 1
    toEnum n = Constant_U (Constant (fromIntegral n) ([],[]))
    fromEnum (Constant_U (Constant n ([],[]))) = truncate n
    fromEnum _ = error "Ugen.fromEnum: non-constant"
    enumFrom = iterate (+1)
    enumFromThen n m = iterate (+(m-n)) n
    enumFromTo n m = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m =
        let p = if n' >= n then (>=) else (<=)
        in takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')

{- | Unit generators are stochastic.
Only un-bracketed constant values are considered.
-}
instance Random.Random Ugen where
    randomR (Constant_U (Constant l ([],[])), Constant_U (Constant r ([],[]))) g =
        let (n, g') = Random.randomR (l,r) g
        in (Constant_U (Constant n ([],[])), g')
    randomR _ _ = error "Ugen.randomR: non constant (l,r)"
    random = Random.randomR (-1.0, 1.0)

-- * Bitwise

-- | 'Operator.OpBitAnd'
bitAnd :: Ugen -> Ugen -> Ugen
bitAnd = mkBinaryOperator OpBitAnd undefined

-- | 'Operator.OpBitOr'
bitOr :: Ugen -> Ugen -> Ugen
bitOr = mkBinaryOperator OpBitOr undefined

-- | 'OpBitXor'
bitXOr :: Ugen -> Ugen -> Ugen
bitXOr = mkBinaryOperator OpBitXor undefined

-- | 'OpBitNot'
bitNot :: Ugen -> Ugen
bitNot = mkUnaryOperator OpBitNot undefined

-- | 'OpShiftLeft'
shiftLeft :: Ugen -> Ugen -> Ugen
shiftLeft = mkBinaryOperator OpShiftLeft undefined

-- | 'OpShiftRight'
shiftRight :: Ugen -> Ugen -> Ugen
shiftRight = mkBinaryOperator OpShiftRight undefined

-- | 'OpUnsignedShift'
unsignedShift :: Ugen -> Ugen -> Ugen
unsignedShift = mkBinaryOperator OpUnsignedShift undefined

-- | Ugens are bit patterns.
instance Bits Ugen where
    (.&.) = bitAnd
    (.|.) = bitOr
    xor = bitXOr
    complement = bitNot
    shiftL p q = shiftLeft p (constant q)
    shiftR p q = shiftRight p (constant q)
    rotate = error "Ugen.rotate"
    bitSize = error "Ugen.bitSize"
    bit = error "Ugen.bit"
    testBit = error "Ugen.testBit"
    popCount = error "Ugen.popCount" -- hugs...
    bitSizeMaybe = error "Ugen.bitSizeMaybe" -- hugs...
    isSigned _ = True

{-
import qualified GHC.Exts as Exts {- base -}

instance Exts.IsList Ugen where
  type Item Ugen = Ugen
  fromList = mce
  toList = mceChannels
-}
