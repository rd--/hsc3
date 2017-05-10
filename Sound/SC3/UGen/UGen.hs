-- | UGen data structure representation and associated functions.
module Sound.SC3.UGen.UGen where

import qualified Data.Char as C {- base -}
import Data.Maybe {- base -}
import Data.List {- base -}

import qualified Sound.SC3.Common.Prelude as P
import qualified Sound.SC3.UGen.Identifier as ID
import qualified Sound.SC3.UGen.Operator as O
import qualified Sound.SC3.UGen.Rate as R
import Sound.SC3.UGen.Type

-- | 'UId' of 'resolveID'.
toUId :: ID.ID a => a -> UGenId
toUId = UId . ID.resolveID

-- | Lookup operator name for operator UGens, else UGen name.
ugen_user_name :: String -> Special -> String
ugen_user_name nm (Special n) =
    case nm of
      "UnaryOpUGen" -> O.unaryName n
      "BinaryOpUGen" -> O.binaryName n
      _ -> nm

-- * UGen graph functions

-- | Depth first traversal of graph at `u' applying `f' to each node.
ugenTraverse :: (UGen -> UGen) -> UGen -> UGen
ugenTraverse f u =
    let recur = ugenTraverse f
    in case u of
         Primitive_U p ->
             let i = ugenInputs p
             in f (Primitive_U (p {ugenInputs = map recur i}))
         Proxy_U p ->
             let s = Primitive_U (proxySource p)
             in case recur s of
                  Primitive_U p' -> f (Proxy_U (p {proxySource = p'}))
                  _ -> error "ugenTraverse"
         MCE_U m -> f (mce (map recur (mceProxies m)))
         MRG_U (MRG l r) -> f (MRG_U (MRG (recur l) (recur r)))
         _ -> f u

-- | Right fold of UGen graph.
ugenFoldr :: (UGen -> a -> a) -> a -> UGen -> a
ugenFoldr f st u =
    let recur = flip (ugenFoldr f)
    in case u of
         Primitive_U p ->
             let i = ugenInputs p
             in f u (foldr recur st i)
         Proxy_U p ->
             let s = proxySource p
             in f u (f (Primitive_U s) st)
         MCE_U m -> f u (foldr recur st (mceProxies m))
         MRG_U (MRG l r) -> f u (f l (f r st))
         _ -> f u st

-- * Unit generator node constructors

-- | Control input node constructor.
control_f64 :: R.Rate -> Maybe Int -> String -> Sample -> UGen
control_f64 r ix nm d = Control_U (Control r ix nm d False Nothing)

-- | Control input node constructor.
--
-- Note that if the name begins with a t_ prefix the control is /not/
-- converted to a triggered control.  Please see 'tr_control'.
control :: R.Rate -> String -> Double -> UGen
control r = control_f64 r Nothing

-- | Variant of 'control' with meta data.
meta_control :: R.Rate -> String -> Double -> C_Meta_T5 Double -> UGen
meta_control rt nm df meta =
    let m = c_meta_t5 id meta
    in Control_U (Control rt Nothing nm df False (Just m))

-- | Triggered (kr) control input node constructor.
tr_control_f64 :: Maybe Int -> String -> Sample -> UGen
tr_control_f64 ix nm d = Control_U (Control R.KR ix nm d True Nothing)

-- | Triggered (kr) control input node constructor.
tr_control :: String -> Double -> UGen
tr_control = tr_control_f64 Nothing

-- | Set indices at a list of controls.
control_set :: [UGen] -> [UGen]
control_set =
    let f ix u = case u of
                   Control_U c -> Control_U (c {controlIndex = Just ix})
                   _ -> error "control_set: non control input?"
    in zipWith f [0..]

-- | Multiple root graph node constructor.
mrg2 :: UGen -> UGen -> UGen
mrg2 u = MRG_U . MRG u

-- * Multiple channel expansion

-- | Multiple channel expansion for two inputs.
mce2 :: UGen -> UGen -> UGen
mce2 x y = mce [x,y]

-- | Extract two channels from possible MCE, if there is only one
-- channel it is duplicated.
mce2c :: UGen -> (UGen,UGen)
mce2c u =
    case mceChannels u of
      [] -> error "mce2c"
      [p] -> (p,p)
      p:q:_ -> (p,q)

-- | Multiple channel expansion for two inputs.
mce3 :: UGen -> UGen -> UGen -> UGen
mce3 x y z = mce [x,y,z]

-- | Apply a function to each channel at a unit generator.
mceMap :: (UGen -> UGen) -> UGen -> UGen
mceMap f u = mce (map f (mceChannels u))

-- | Map with element index.
map_ix :: ((Int,a) -> b) -> [a] -> [b]
map_ix f = map f . zip [0..]

-- | Variant of 'mceMap' with element index.
mce_map_ix :: ((Int,UGen) -> UGen) -> UGen -> UGen
mce_map_ix f u = mce (map_ix f (mceChannels u))

-- | Apply UGen list operation on MCE contents.
mceEdit :: ([UGen] -> [UGen]) -> UGen -> UGen
mceEdit f u =
    case u of
      MCE_U m -> mce (f (mceProxies m))
      _ -> error "mceEdit: non MCE value"

-- | Reverse order of channels at MCE.
mceReverse :: UGen -> UGen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n u =
    case u of
      MCE_U m -> mceProxies m !! n
      _ -> error "mceChannel: non MCE value"

-- | Transpose rows and columns, ie. {{a,b},{c,d}} to {{a,c},{b,d}}.
mceTranspose :: UGen -> UGen
mceTranspose = mce . map mce . transpose . map mceChannels . mceChannels

-- | Collapse mce by summing (see also mix and mixN).
mceSum :: UGen -> UGen
mceSum = sum . mceChannels

-- * Transform

-- | Given /unmce/ function make halt mce transform.
halt_mce_transform' :: (a -> [a]) -> [a] -> [a]
halt_mce_transform' f l =
    let (l',e) = fromMaybe (error "halt_mce_transform: null?") (P.sep_last l)
    in l' ++ f e

-- | The halt MCE transform, ie. lift channels of last input into list.
--
-- > halt_mce_transform [1,2,mce2 3 4] == [1,2,3,4]
halt_mce_transform :: [UGen] -> [UGen]
halt_mce_transform = halt_mce_transform' mceChannels

-- * Multiple root graphs

-- * Labels

-- | Lift a 'String' to a UGen label (ie. for 'poll').
label :: String -> UGen
label = Label_U . Label

-- | Unpack a label to a length prefixed list of 'Constant's.  There
-- is a special case for mce nodes, but it requires labels to be equal
-- length.  Properly, 'poll' would not unpack the label, it would be
-- done by the synthdef builder.
unpackLabel :: UGen -> [UGen]
unpackLabel u =
    case u of
      Label_U (Label s) ->
          let q = fromEnum '?'
              f c = if C.isAscii c then fromEnum c else q
              s' = map (fromIntegral . f) s
              n = fromIntegral (length s)
          in n : s'
      MCE_U m ->
          let x = map unpackLabel (mceProxies m)
          in if P.equal_length_p x
             then map mce (transpose x)
             else error (show ("unpackLabel: mce length /=",x))
      _ -> error (show ("unpackLabel: non-label",u))

-- * Bitwise

bitAnd :: UGen -> UGen -> UGen
bitAnd = mkBinaryOperator O.BitAnd undefined

bitOr :: UGen -> UGen -> UGen
bitOr = mkBinaryOperator O.BitOr undefined

bitXOr :: UGen -> UGen -> UGen
bitXOr = mkBinaryOperator O.BitXor undefined

bitNot :: UGen -> UGen
bitNot = mkUnaryOperator O.BitNot undefined

shiftLeft :: UGen -> UGen -> UGen
shiftLeft = mkBinaryOperator O.ShiftLeft undefined

shiftRight :: UGen -> UGen -> UGen
shiftRight = mkBinaryOperator O.ShiftRight undefined

unsignedShift :: UGen -> UGen -> UGen
unsignedShift = mkBinaryOperator O.UnsignedShift undefined

(.<<.) :: UGen -> UGen -> UGen
(.<<.) = shiftLeft

(.>>.) :: UGen -> UGen -> UGen
(.>>.) = shiftRight
