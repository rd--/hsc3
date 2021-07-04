{-# Language TypeFamilies #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | Unit Generator ('UGen') and associated types and instances.
module Sound.SC3.UGen.Type where

import Data.Bits {- base -}
import Data.Either {- base -}
import qualified Data.Fixed as F {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified GHC.Exts as Exts {- base -}

import qualified Data.Traversable as Traversable {- containers -}
import qualified Data.Reify as Reify {- data-reify -}
import qualified Safe {- safe -}
import qualified System.Random as Random {- random -}

import qualified Sound.SC3.Common.Math as Math
import Sound.SC3.Common.Math.Operator
import Sound.SC3.Common.Rate
import Sound.SC3.Common.Mce
import qualified Sound.SC3.Common.UId as UId

-- * Basic types

-- | Identifier used to distinguish otherwise equal non-deterministic nodes.
data UGenId = NoId | UId UId.Id
              deriving (Eq,Read,Show)

-- | Alias of 'NoId', the 'UGenId' used for deterministic UGens.
no_id :: UGenId
no_id = NoId

-- | SC3 samples are 32-bit 'Float'.  hsc3 represents data as 64-bit
-- 'Double'.  If 'UGen' values are used more generally (ie. see
-- hsc3-forth) 'Float' may be too imprecise, ie. for representing time
-- stamps.
type Sample = Double

-- | Constants.
--
-- > Constant 3 == Constant 3
-- > (Constant 3 > Constant 1) == True
newtype Constant = Constant {constantValue :: Sample} deriving (Eq,Ord,Read,Show)

-- | Control meta-data.
data Control_Meta n =
    Control_Meta {ctl_min :: n -- ^ Minimum
                 ,ctl_max :: n -- ^ Maximum
                 ,ctl_warp :: String -- ^ @(0,1)@ @(min,max)@ transfer function.
                 ,ctl_step :: n -- ^ The step to increment & decrement by.
                 ,ctl_units :: String -- ^ Unit of measure (ie hz, ms etc.).
                 ,controlGroup :: Maybe Control_Group -- ^ Control group.
                 }
    deriving (Eq,Read,Show)

-- | 3-tuple form of 'Control_Meta' data.
type Control_Meta_T3 n = (n,n,String)

-- | Lift 'Control_Meta_T3' to 'Control_Meta' allowing type coercion.
control_meta_t3 :: Num m => (n -> m) -> Control_Meta_T3 n -> Control_Meta m
control_meta_t3 f (l,r,w) = Control_Meta (f l) (f r) w 0 "" Nothing

-- | 5-tuple form of 'Control_Meta' data.
type Control_Meta_T5 n = (n,n,String,n,String)

-- | Lift 'Control_Meta_T5' to 'Control_Meta' allowing type coercion.
control_meta_t5 :: (n -> m) -> Control_Meta_T5 n -> Control_Meta m
control_meta_t5 f (l,r,w,stp,u) = Control_Meta (f l) (f r) w (f stp) u Nothing

{- | Controls may form part of a control group. -}
data Control_Group
  = Control_Range
  | Control_Array Int
  | Control_XY
  deriving (Eq,Read,Show)

-- | The number of elements in a control group.
control_group_degree :: Control_Group -> Int
control_group_degree grp =
  case grp of
    Control_Range -> 2
    Control_Array n -> n
    Control_XY -> 2

{- | Grouped controls have names that have equal prefixes and identifying suffixes.
     Range controls have two elements, minima and maxima, suffixes are [ and ].
     Array controls have N elements and have IX suffixes.
     XY controls have two elements, X and Y coordinates, suffixes are X and Y.
-}
control_group_suffixes :: Control_Group -> [String]
control_group_suffixes grp =
  case grp of
    Control_Range -> ["[","]"]
    Control_Array n -> map (printf "%02d") [0 .. n - 1]
    Control_XY -> ["X","Y"]

-- | Control inputs.  It is an invariant that controls with equal
-- names within a UGen graph must be equal in all other respects.
data Control = Control {controlOperatingRate :: Rate
                       ,controlIndex :: Maybe Int
                       ,controlName :: String
                       ,controlDefault :: Sample
                       ,controlTriggered :: Bool
                       ,controlMeta :: Maybe (Control_Meta Sample)}
               deriving (Eq,Read,Show)

-- | Labels.
newtype Label = Label {ugenLabel :: String} deriving (Eq,Read,Show)

-- | Unit generator output descriptor.
type Output = Rate

-- | Operating mode of unary and binary operators.
newtype Special = Special Int
    deriving (Eq,Read,Show)

-- | UGen primitives.
data Primitive t =
  Primitive {ugenRate :: Rate
            ,ugenName :: String
            ,ugenInputs :: [t]
            ,ugenOutputs :: [Output]
            ,ugenSpecial :: Special
            ,ugenId :: UGenId}
  deriving (Functor, Foldable, Traversable, Eq, Read, Show)

-- | Proxy indicating an output port at a multi-channel primitive.
data Proxy t =
  Proxy {proxySource :: Primitive t
        ,proxyIndex :: Int}
  deriving (Functor, Foldable, Traversable, Eq, Read, Show)

-- | Multiple root graph.
data Mrg t =
  Mrg {mrgLeft :: t
      ,mrgRight :: t}
  deriving (Functor, Foldable, Traversable, Eq, Read, Show)

{-
-- | Control type
data CVarTy = CVarInit | CVarControl | CVarTrigger deriving (Eq,Read,Show)
CVar CVarTy String Double -- ^ Control input (named)
-}

-- | Circuit
data Circuit t
  = CConstant Constant
  | CControl Control
  | CLabel Label
  | CPrimitive (Primitive t)
  | CProxy (Proxy t) -- ^ Output port at multi-channel primitive
  | CMce (Mce t) -- ^ Multiple channel expansion
  | CMrg (Mrg t) -- ^ Multiple root graph
  deriving (Functor, Foldable, Traversable, Eq, Read, Show)

-- | UGen
data UGen = UGen (Circuit UGen) deriving (Eq,Read,Show)

instance Reify.MuRef UGen where
  type DeRef UGen = Circuit
  mapDeRef f (UGen c) = Traversable.traverse f c

ugenCircuit :: UGen -> Circuit UGen
ugenCircuit (UGen c) = c

{-
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
-}

instance EqE UGen where
    equal_to = mkBinaryOperator EQ_ Math.sc3_eq
    not_equal_to = mkBinaryOperator NE Math.sc3_neq

instance OrdE UGen where
    less_than = mkBinaryOperator LT_ Math.sc3_lt
    less_than_or_equal_to = mkBinaryOperator LE Math.sc3_lte
    greater_than = mkBinaryOperator GT_ Math.sc3_gt
    greater_than_or_equal_to = mkBinaryOperator GE Math.sc3_gte

-- | 'UGen' form or 'Math.sc3_round_to'.
roundTo :: UGen -> UGen -> UGen
roundTo = mkBinaryOperator Round Math.sc3_round_to

instance RealFracE UGen where
    properFractionE = error "UGen.properFractionE"
    truncateE = error "UGen.truncateE"
    roundE i = roundTo i 1
    ceilingE = mkUnaryOperator Ceil ceilingE
    floorE = mkUnaryOperator Floor floorE

instance UnaryOp UGen where
    ampDb = mkUnaryOperator AmpDb ampDb
    asFloat = mkUnaryOperator AsFloat asFloat
    asInt = mkUnaryOperator AsInt asInt
    cpsMIDI = mkUnaryOperator CpsMidi cpsMIDI
    cpsOct = mkUnaryOperator CpsOct cpsOct
    cubed = mkUnaryOperator Cubed cubed
    dbAmp = mkUnaryOperator DbAmp dbAmp
    distort = mkUnaryOperator Distort distort
    frac = mkUnaryOperator Frac frac
    isNil = mkUnaryOperator IsNil isNil
    log10 = mkUnaryOperator Log10 log10
    log2 = mkUnaryOperator Log2 log2
    midiCPS = mkUnaryOperator MidiCps midiCPS
    midiRatio = mkUnaryOperator MidiRatio midiRatio
    notE = mkUnaryOperator Not notE
    notNil = mkUnaryOperator NotNil notNil
    octCPS = mkUnaryOperator OctCps octCPS
    ramp_ = mkUnaryOperator Ramp_ ramp_
    ratioMIDI = mkUnaryOperator RatioMidi ratioMIDI
    softClip = mkUnaryOperator SoftClip softClip
    squared = mkUnaryOperator Squared squared

instance BinaryOp UGen where
    iDiv = mkBinaryOperator IDiv iDiv
    modE = mkBinaryOperator Mod F.mod'
    lcmE = mkBinaryOperator LCM lcmE
    gcdE = mkBinaryOperator GCD gcdE
    roundUp = mkBinaryOperator RoundUp roundUp
    trunc = mkBinaryOperator Trunc trunc
    atan2E = mkBinaryOperator Atan2 atan2E
    hypot = mkBinaryOperator Hypot hypot
    hypotx = mkBinaryOperator Hypotx hypotx
    fill = mkBinaryOperator Fill fill
    ring1 = mkBinaryOperator Ring1 ring1
    ring2 = mkBinaryOperator Ring2 ring2
    ring3 = mkBinaryOperator Ring3 ring3
    ring4 = mkBinaryOperator Ring4 ring4
    difSqr = mkBinaryOperator DifSqr difSqr
    sumSqr = mkBinaryOperator SumSqr sumSqr
    sqrSum = mkBinaryOperator SqrSum sqrSum
    sqrDif = mkBinaryOperator SqrDif sqrDif
    absDif = mkBinaryOperator AbsDif absDif
    thresh = mkBinaryOperator Thresh thresh
    amClip = mkBinaryOperator AmClip amClip
    scaleNeg = mkBinaryOperator ScaleNeg scaleNeg
    clip2 = mkBinaryOperator Clip2 clip2
    excess = mkBinaryOperator Excess excess
    fold2 = mkBinaryOperator Fold2 fold2
    wrap2 = mkBinaryOperator Wrap2 wrap2
    firstArg = mkBinaryOperator FirstArg firstArg
    randRange = mkBinaryOperator RandRange randRange
    exprandRange = mkBinaryOperator ExpRandRange exprandRange

--instance MulAdd UGen where mul_add = mulAdd

-- * Parser

-- | 'constant' of 'parse_double'.
{-
parse_constant :: String -> Maybe UGen
parse_constant = fmap constant . Math.parse_double
-}
parse_constant :: String -> Maybe UGen
parse_constant = fmap constant . Math.parse_double

-- * Accessors

{-
-- | See into 'Constant_U'.
un_constant :: UGen -> Maybe Constant
un_constant u =
    case u of
      Constant_U c -> Just c
      _ -> Nothing

-- | Value of 'Constant_U' 'Constant'.
u_constant :: UGen -> Maybe Sample
u_constant = fmap constantValue . un_constant
-}
u_constant :: UGen -> Maybe Double
u_constant u =
    case u of
      UGen (CConstant c) -> Just (constantValue c)
      _ -> Nothing

-- | Erroring variant.
u_constant_err :: UGen -> Sample
u_constant_err = fromMaybe (error "u_constant") . u_constant

-- * Mrg

-- | Multiple root graph constructor.
{-
mrg :: [UGen] -> UGen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> Mrg_U (Mrg x (mrg xs))
-}
mrg :: [UGen] -> UGen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> UGen (CMrg (Mrg x (mrg xs)))

-- | See into 'Mrg_U', follows leftmost rule until arriving at non-Mrg node.
{-
mrg_leftmost :: UGen -> UGen
mrg_leftmost u =
    case u of
      Mrg_U m -> mrg_leftmost (mrgLeft m)
      _ -> u
-}
mrg_leftmost :: UGen -> UGen
mrg_leftmost u =
  case u of
    UGen (CMrg m) -> mrg_leftmost (mrgLeft m)
    _ -> u

-- * Predicates

-- | Constant node predicate.
{-
isConstant :: UGen -> Bool
isConstant = isJust . un_constant
-}
isConstant :: UGen -> Bool
isConstant = isJust . u_constant

-- | True if input is a sink 'UGen', ie. has no outputs.  Sees into Mrg.
{-
isSink :: UGen -> Bool
isSink u =
    case mrg_leftmost u of
      Primitive_U p -> null (ugenOutputs p)
      Mce_U m -> all isSink (mce_elem m)
      _ -> False
-}
isSink :: UGen -> Bool
isSink u =
    case mrg_leftmost u of
      UGen (CPrimitive p) -> null (ugenOutputs p)
      UGen (CMce m) -> all isSink (mce_elem m)
      _ -> False

-- | See into 'Proxy_U'.
{-
un_proxy :: UGen -> Maybe (Proxy UGen)
un_proxy u =
    case u of
      Proxy_U p -> Just p
      _ -> Nothing
-}
un_proxy :: UGen -> Maybe (Proxy UGen)
un_proxy u =
    case u of
      UGen (CProxy p) -> Just p
      _ -> Nothing

-- | Is 'UGen' a 'Proxy'?
{-
isProxy :: UGen -> Bool
isProxy = isJust . un_proxy
-}
isProxy :: UGen -> Bool
isProxy = isJust . un_proxy

-- * Mce

-- | Multiple channel expansion node constructor.
{-
mce :: [UGen] -> UGen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> Mce_U (Mce_Unit x)
      _ -> Mce_U (Mce_Vector xs)
-}
mce :: [UGen] -> UGen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> UGen (CMce (Mce_Unit x))
      _ -> UGen (CMce (Mce_Vector xs))

-- | Type specified 'mce_elem'.
{-
mceProxies :: Mce UGen -> [UGen]
mceProxies = mce_elem
-}
mceProxies :: Mce UGen -> [UGen]
mceProxies = mce_elem

-- | Multiple channel expansion node ('Mce_U') predicate.  Sees into Mrg.
{-
isMce :: UGen -> Bool
isMce u =
    case mrg_leftmost u of
      Mce_U _ -> True
      _ -> False
-}
isMce :: UGen -> Bool
isMce u =
    case mrg_leftmost u of
      UGen (CMce _) -> True
      _ -> False

-- | Output channels of UGen as a list.  If required, preserves the RHS of and Mrg node in channel 0.
{-
mceChannels :: UGen -> [UGen]
mceChannels u =
    case u of
      Mce_U m -> mce_elem m
      Mrg_U (Mrg x y) -> let r:rs = mceChannels x in Mrg_U (Mrg r y) : rs
      _ -> [u]
-}
mceChannels :: UGen -> [UGen]
mceChannels u =
    case u of
      UGen (CMce m) -> mce_elem m
      UGen (CMrg (Mrg x y)) -> let r:rs = mceChannels x in UGen (CMrg (Mrg r y)) : rs
      _ -> [u]

-- | Number of channels to expand to.  This function sees into Mrg, and is defined only for Mce nodes.
{-
mceDegree :: UGen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      Mce_U m -> Just (length (mceProxies m))
      _ -> Nothing
-}
mceDegree :: UGen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      UGen (CMce m) -> Just (length (mceProxies m))
      _ -> Nothing

-- | Erroring variant.
{-
mceDegree_err :: UGen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree
-}
mceDegree_err :: UGen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree

-- | Extend UGen to specified degree.  Follows "leftmost" rule for Mrg nodes.
{-
mceExtend :: Int -> UGen -> [UGen]
mceExtend n u =
    case u of
      Mce_U m -> mceProxies (mce_extend n m)
      Mrg_U (Mrg x y) -> let (r:rs) = mceExtend n x
                         in Mrg_U (Mrg r y) : rs
      _ -> replicate n u
-}
mceExtend :: Int -> UGen -> [UGen]
mceExtend n u =
    case u of
      UGen (CMce m) -> mceProxies (mce_extend n m)
      UGen (CMrg (Mrg x y)) -> let (r:rs) = mceExtend n x
                               in UGen (CMrg (Mrg r y)) : rs
      _ -> replicate n u

-- | Is Mce required, ie. are any input values Mce?
{-
mceRequired :: [UGen] -> Bool
mceRequired = any isMce
-}
mceRequired :: [UGen] -> Bool
mceRequired = any isMce

{- | Apply Mce transform to a list of inputs.
     The transform extends each input so all are of equal length, and then transposes the matrix.

> mceInputTransform [mce2 1 2,mce2 3 4] == Just [[1,3],[2,4]]
> mceInputTransform [mce2 1 2,mce2 3 4,mce3 5 6 7] == Just [[1,3,5],[2,4,6],[1,3,7]]
-}
{-
mceInputTransform :: [UGen] -> Maybe [[UGen]]
mceInputTransform i =
    if mceRequired i
    then let n = maximum (map mceDegree_err (filter isMce i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing
-}
mceInputTransform :: [UGen] -> Maybe [[UGen]]
mceInputTransform i =
    if mceRequired i
    then let n = maximum (map mceDegree_err (filter isMce i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

-- | Build a UGen after Mce transformation of inputs.
{-
mceBuild :: ([UGen] -> UGen) -> [UGen] -> UGen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> Mce_U (Mce_Vector (map (mceBuild f) i'))
-}
mceBuild :: ([UGen] -> UGen) -> [UGen] -> UGen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> UGen (CMce (Mce_Vector (map (mceBuild f) i')))

-- | True if Mce is an immediate proxy for a multiple-out Primitive.
--   This is useful when disassembling graphs, ie. ugen_graph_forth_pp at hsc3-db.
mce_is_direct_proxy :: Mce UGen -> Bool
mce_is_direct_proxy m =
    case m of
      Mce_Unit _ -> False
      Mce_Vector v ->
          let p = map un_proxy v
              p' = catMaybes p
          in all isJust p &&
             length (nub (map proxySource p')) == 1 &&
             map proxyIndex p' `isPrefixOf` [0..]

-- * Validators

-- | Ensure input 'UGen' is valid, ie. not a sink.
{-
checkInput :: UGen -> UGen
checkInput u =
    if isSink u
    then error ("checkInput: " ++ show u)
    else u
-}
checkInput :: UGen -> UGen
checkInput u =
    if isSink u
    then error ("checkInput: " ++ show u)
    else u

-- * Constructors

-- | Constant value node constructor.
{-
constant :: Real n => n -> UGen
constant = Constant_U . Constant . realToFrac
-}
constant :: Real n => n -> UGen
constant = UGen . CConstant . Constant . realToFrac

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
{-
proxy :: UGen -> Int -> UGen
proxy u n =
    case u of
      Primitive_U p -> Proxy_U (Proxy p n)
      _ -> error "proxy: not primitive?"
-}
proxy :: UGen -> Int -> UGen
proxy u n =
    case u of
      UGen (CPrimitive p) -> UGen (CProxy (Proxy p n))
      _ -> error "proxy: not primitive?"

-- | Determine the rate of a UGen.
{-
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
-}
rateOf :: UGen -> Rate
rateOf u =
    case u of
      UGen (CConstant _) -> InitialisationRate
      UGen (CControl c) -> controlOperatingRate c
      UGen (CLabel _) -> InitialisationRate
      UGen (CPrimitive p) -> ugenRate p
      UGen (CProxy p) -> ugenRate (proxySource p)
      UGen (CMce _) -> maximum (map rateOf (mceChannels u))
      UGen (CMrg m) -> rateOf (mrgLeft m)

-- | Apply proxy transformation if required.
{-
proxify :: UGen -> UGen
proxify u =
    case u of
      Mce_U m -> mce (map proxify (mce_elem m))
      Mrg_U m -> mrg [proxify (mrgLeft m), mrgRight m]
      Primitive_U p ->
          let o = ugenOutputs p
          in case o of
               _:_:_ -> mce (map (proxy u) [0 .. length o - 1])
               _ -> u
      Constant_U _ -> u
      _ -> error "proxify: illegal ugen"
-}
proxify :: UGen -> UGen
proxify u =
    case u of
      UGen (CMce m) -> mce (map proxify (mce_elem m))
      UGen (CMrg m) -> mrg [proxify (mrgLeft m), mrgRight m]
      UGen (CPrimitive p) ->
          let o = ugenOutputs p
          in case o of
               _:_:_ -> mce (map (proxy u) [0 .. length o - 1])
               _ -> u
      UGen (CConstant _) -> u
      _ -> error "proxify: illegal ugen"

-- | Filters with DemandRate inputs run at ControlRate.  This is a little unfortunate,
-- it'd be nicer if the rate in this circumstance could be given.
{-
mk_ugen_select_rate :: String -> [UGen] -> [Rate] -> Either Rate [Int] -> Rate
mk_ugen_select_rate nm h rs r =
  let r' = either id (maximum . map (rateOf . Safe.atNote ("mkUGen: " ++ nm) h)) r
  in if isRight r && r' == DemandRate && DemandRate `notElem` rs
     then if ControlRate `elem` rs then ControlRate else error "mkUGen: DemandRate input to non-ControlRate filter"
     else if r' `elem` rs || r' == DemandRate
          then r'
          else error ("mkUGen: rate restricted: " ++ show (r,r',rs,nm))
-}
mk_ugen_select_rate :: String -> [UGen] -> [Rate] -> Either Rate [Int] -> Rate
mk_ugen_select_rate nm h rs r =
  let r' = either id (maximum . map (rateOf . Safe.atNote ("mkUGen: " ++ nm) h)) r
  in if isRight r && r' == DemandRate && DemandRate `notElem` rs
     then if ControlRate `elem` rs then ControlRate else error "mkUGen: DemandRate input to non-ControlRate filter"
     else if r' `elem` rs || r' == DemandRate
          then r'
          else error ("mkUGen: rate restricted: " ++ show (r,r',rs,nm))

-- | Construct proxied and multiple channel expanded UGen.
--
-- cf = constant function, rs = rate set, r = rate, nm = name, i =
-- inputs, i_mce = list of Mce inputs, o = outputs.
{-
mkUGen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [UGen] -> Maybe [UGen] -> Int -> Special -> UGenId -> UGen
mkUGen cf rs r nm i i_mce o s z =
    let i' = maybe i ((i ++) . concatMap mceChannels) i_mce
        f h = let r' = mk_ugen_select_rate nm h rs r
                  o' = replicate o r'
                  u = Primitive_U (Primitive r' nm h o' s z)
              in case cf of
                   Just cf' ->
                     if all isConstant h
                     then constant (cf' (mapMaybe u_constant h))
                     else u
                   Nothing -> u
    in proxify (mceBuild f (map checkInput i'))
-}
mkUGen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [UGen] -> Maybe [UGen] -> Int -> Special -> UGenId -> UGen
mkUGen cf rs r nm i i_mce o s z =
    let i' = maybe i ((i ++) . concatMap mceChannels) i_mce
        f h = let r' = mk_ugen_select_rate nm h rs r
                  o' = replicate o r'
                  u = UGen (CPrimitive (Primitive r' nm h o' s z))
              in case cf of
                   Just cf' ->
                     if all isConstant h
                     then constant (cf' (mapMaybe u_constant h))
                     else u
                   Nothing -> u
    in proxify (mceBuild f (map checkInput i'))

-- * Operators

-- | Operator UGen constructor.
{-
mkOperator :: ([Sample] -> Sample) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUGen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId
-}
mkOperator :: ([Sample] -> Sample) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUGen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId

-- | Unary math constructor.
{-
mkUnaryOperator :: SC3_Unary_Op -> (Sample -> Sample) -> UGen -> UGen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUGen" [a] (fromEnum i)
-}
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
{-
mkBinaryOperator_optimise_constants :: SC3_Binary_Op -> (Sample -> Sample -> Sample) ->
                                       (Either Sample Sample -> Bool) ->
                                       UGen -> UGen -> UGen
mkBinaryOperator_optimise_constants i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (Constant_U (Constant a'),_) ->
                 if o (Left a') then Just b else Nothing
             (_,Constant_U (Constant b')) ->
                 if o (Right b') then Just a else Nothing
             _ -> Nothing
   in fromMaybe (mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)) r
-}
mkBinaryOperator_optimise_constants :: SC3_Binary_Op -> (Sample -> Sample -> Sample) ->
                                       (Either Sample Sample -> Bool) ->
                                       UGen -> UGen -> UGen
mkBinaryOperator_optimise_constants i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (UGen (CConstant (Constant a')),_) ->
                 if o (Left a') then Just b else Nothing
             (_,UGen (CConstant (Constant b'))) ->
                 if o (Right b') then Just a else Nothing
             _ -> Nothing
   in fromMaybe (mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)) r

-- | Plain (non-optimised) binary math constructor.
{-
mkBinaryOperator :: SC3_Binary_Op -> (Sample -> Sample -> Sample) -> UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)
-}
mkBinaryOperator :: SC3_Binary_Op -> (Sample -> Sample -> Sample) -> UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

-- * Numeric instances

-- | Is /u/ a binary math operator with SPECIAL of /k/.
{-
is_math_binop :: Int -> UGen -> Bool
is_math_binop k u =
    case u of
      Primitive_U (Primitive _ "BinaryOpUGen" [_,_] [_] (Special s) NoId) -> s == k
      _ -> False
-}
is_math_binop :: Int -> UGen -> Bool
is_math_binop k u =
    case u of
      UGen (CPrimitive (Primitive _ "BinaryOpUGen" [_,_] [_] (Special s) NoId)) -> s == k
      _ -> False

-- | Is /u/ an ADD operator?
{-
is_add_operator :: UGen -> Bool
is_add_operator = is_math_binop 0
-}
is_add_operator :: UGen -> Bool
is_add_operator = is_math_binop 0

{-
assert_is_add_operator :: String -> UGen -> UGen
assert_is_add_operator msg u = if is_add_operator u then u else error ("assert_is_add_operator: " ++ msg)
-}
assert_is_add_operator :: String -> UGen -> UGen
assert_is_add_operator msg u = if is_add_operator u then u else error ("assert_is_add_operator: " ++ msg)

-- | Is /u/ an MUL operator?
{-
is_mul_operator :: UGen -> Bool
is_mul_operator = is_math_binop 2
-}
is_mul_operator :: UGen -> Bool
is_mul_operator = is_math_binop 2

-- | MulAdd re-writer, applicable only directly at add operator UGen.
--   The MulAdd UGen is very sensitive to input rates.
--   ADD=AudioRate with IN|MUL=InitialisationRate|CONST will CRASH scsynth.
{-
mul_add_optimise_direct :: UGen -> UGen
mul_add_optimise_direct u =
  let reorder (i,j,k) =
        let (ri,rj,rk) = (rateOf i,rateOf j,rateOf k)
        in if rk > max ri rj
           then Nothing
           else Just (max (max ri rj) rk,if rj > ri then (j,i,k) else (i,j,k))
  in case assert_is_add_operator "MUL-ADD" u of
       Primitive_U
         (Primitive _ _ [Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId),k] [_] _ NoId) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> Primitive_U (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId)
           Nothing -> u
       Primitive_U
         (Primitive _ _ [k,Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId)] [_] _ NoId) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> Primitive_U (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId)
           Nothing -> u
       _ -> u
-}
mul_add_optimise_direct :: UGen -> UGen
mul_add_optimise_direct u =
  let reorder (i,j,k) =
        let (ri,rj,rk) = (rateOf i,rateOf j,rateOf k)
        in if rk > max ri rj
           then Nothing
           else Just (max (max ri rj) rk,if rj > ri then (j,i,k) else (i,j,k))
  in case assert_is_add_operator "MUL-ADD" u of
       UGen (CPrimitive (Primitive _ _ [UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId)),k] [_] _ NoId)) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> UGen (CPrimitive (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId))
           Nothing -> u
       UGen (CPrimitive (Primitive _ _ [k,UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId))] [_] _ NoId)) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> UGen (CPrimitive (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId))
           Nothing -> u
       _ -> u

{- | MulAdd optimiser, applicable at any UGen (ie. checks /u/ is an ADD ugen)

> import Sound.SC3
> g1 = sinOsc ar 440 0 * 0.1 + control ir "x" 0.05
> g2 = sinOsc ar 440 0 * control ir "x" 0.1 + 0.05
> g3 = control ir "x" 0.1 * sinOsc ar 440 0 + 0.05
> g4 = 0.05 + sinOsc ar 440 0 * 0.1
-}
{-
mul_add_optimise :: UGen -> UGen
mul_add_optimise u = if is_add_operator u then mul_add_optimise_direct u else u
-}
mul_add_optimise :: UGen -> UGen
mul_add_optimise u = if is_add_operator u then mul_add_optimise_direct u else u

-- | Sum3 re-writer, applicable only directly at add operator UGen.
{-
sum3_optimise_direct :: UGen -> UGen
sum3_optimise_direct u =
  case assert_is_add_operator "SUM3" u of
    Primitive_U (Primitive r _ [Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId),k] [_] _ NoId) ->
      Primitive_U (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId)
    Primitive_U (Primitive r _ [k,Primitive_U (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId)] [_] _ NoId) ->
      Primitive_U (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId)
    _ -> u
-}
sum3_optimise_direct :: UGen -> UGen
sum3_optimise_direct u =
  case assert_is_add_operator "SUM3" u of
    UGen (CPrimitive (Primitive r _ [UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId)),k] [_] _ NoId)) ->
      UGen (CPrimitive (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId))
    UGen (CPrimitive (Primitive r _ [k,UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId))] [_] _ NoId)) ->
      UGen (CPrimitive (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId))
    _ -> u

-- | /Sum3/ optimiser, applicable at any /u/ (ie. checks if /u/ is an ADD operator).
{-
sum3_optimise :: UGen -> UGen
sum3_optimise u = if is_add_operator u then sum3_optimise_direct u else u
-}
sum3_optimise :: UGen -> UGen
sum3_optimise u = if is_add_operator u then sum3_optimise_direct u else u

-- | 'sum3_optimise' of 'mul_add_optimise'.
{-
add_optimise :: UGen -> UGen
add_optimise = sum3_optimise . mul_add_optimise
-}
add_optimise :: UGen -> UGen
add_optimise = sum3_optimise . mul_add_optimise

-- | Unit generators are numbers.
instance Num UGen where
    negate = mkUnaryOperator Neg negate
    (+) = fmap add_optimise .
          mkBinaryOperator_optimise_constants Add (+) (`elem` [Left 0,Right 0])
    (-) = mkBinaryOperator_optimise_constants Sub (-) (Right 0 ==)
    (*) = mkBinaryOperator_optimise_constants Mul (*) (`elem` [Left 1,Right 1])
    abs = mkUnaryOperator Abs abs
    signum = mkUnaryOperator Sign signum
    fromInteger = UGen . CConstant . Constant . fromInteger

-- | Unit generators are fractional.
instance Fractional UGen where
    recip = mkUnaryOperator Recip recip
    (/) = mkBinaryOperator_optimise_constants FDiv (/) (Right 1 ==)
    fromRational = UGen . CConstant . Constant . fromRational

-- | Unit generators are floating point.
instance Floating UGen where
    pi = UGen (CConstant (Constant pi))
    exp = mkUnaryOperator Exp exp
    log = mkUnaryOperator Log log
    sqrt = mkUnaryOperator Sqrt sqrt
    (**) = mkBinaryOperator_optimise_constants Pow (**) (Right 1 ==)
    logBase a b = log b / log a
    sin = mkUnaryOperator Sin sin
    cos = mkUnaryOperator Cos cos
    tan = mkUnaryOperator Tan tan
    asin = mkUnaryOperator ArcSin asin
    acos = mkUnaryOperator ArcCos acos
    atan = mkUnaryOperator ArcTan atan
    sinh = mkUnaryOperator SinH sinh
    cosh = mkUnaryOperator CosH cosh
    tanh = mkUnaryOperator TanH tanh
    asinh x = log (sqrt (x*x+1) + x)
    acosh x = log (sqrt (x*x-1) + x)
    atanh x = (log (1+x) - log (1-x)) / 2

-- | Unit generators are real.
instance Real UGen where
    toRational (UGen (CConstant (Constant n))) = toRational n
    toRational _ = error "UGen.toRational: non-constant"

-- | Unit generators are integral.
instance Integral UGen where
    quot = mkBinaryOperator IDiv (error "UGen.quot")
    rem = mkBinaryOperator Mod (error "UGen.rem")
    quotRem a b = (quot a b, rem a b)
    div = mkBinaryOperator IDiv (error "UGen.div")
    mod = mkBinaryOperator Mod (error "UGen.mod")
    toInteger (UGen (CConstant (Constant n))) = floor n
    toInteger _ = error "UGen.toInteger: non-constant"

instance RealFrac UGen where
  properFraction = error "UGen.properFraction, see properFractionE"
  round = error "UGen.round, see roundE"
  ceiling = error "UGen.ceiling, see ceilingE"
  floor = error "UGen.floor, see floorE"

-- | Unit generators are orderable (when 'Constants').
--
-- > (constant 2 > constant 1) == True
instance Ord UGen where
    (UGen (CConstant a)) < (UGen (CConstant b)) = a < b
    _ < _ = error "UGen.<, see <*"
    (UGen (CConstant a)) <= (UGen (CConstant b)) = a <= b
    _ <= _ = error "UGen.<= at, see <=*"
    (UGen (CConstant a)) > (UGen (CConstant b)) = a > b
    _ > _ = error "UGen.>, see >*"
    (UGen (CConstant a)) >= (UGen (CConstant b)) = a >= b
    _ >= _ = error "UGen.>=, see >=*"
    min = mkBinaryOperator Min min
    max = mkBinaryOperator Max max

-- | Unit generators are enumerable.
instance Enum UGen where
    succ u = u + 1
    pred u = u - 1
    toEnum n = UGen (CConstant (Constant (fromIntegral n)))
    fromEnum (UGen (CConstant (Constant n))) = truncate n
    fromEnum _ = error "UGen.fromEnum: non-constant"
    enumFrom = iterate (+1)
    enumFromThen n m = iterate (+(m-n)) n
    enumFromTo n m = takeWhile (<= m+1/2) (enumFrom n)
    enumFromThenTo n n' m =
        let p = if n' >= n then (>=) else (<=)
        in takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')

-- | Unit generators are stochastic.
instance Random.Random UGen where
    randomR (UGen (CConstant (Constant l)),UGen (CConstant (Constant r))) g =
        let (n, g') = Random.randomR (l,r) g
        in (UGen (CConstant (Constant n)), g')
    randomR _ _ = error "UGen.randomR: non constant (l,r)"
    random = Random.randomR (-1.0, 1.0)

-- | UGens are bit patterns.
instance Bits UGen where
    (.&.) = mkBinaryOperator BitAnd undefined
    (.|.) = mkBinaryOperator BitOr undefined
    xor = mkBinaryOperator BitXor undefined
    complement = mkUnaryOperator BitNot undefined
    shift = error "UGen.shift"
    rotate = error "UGen.rotate"
    bitSize = error "UGen.bitSize"
    bit = error "UGen.bit"
    testBit = error "UGen.testBit"
    popCount = error "UGen.popCount"
    bitSizeMaybe = error "UGen.bitSizeMaybe"
    isSigned _ = True

instance Exts.IsList UGen where
  type Item UGen = UGen
  fromList = mce
  toList = mceChannels
