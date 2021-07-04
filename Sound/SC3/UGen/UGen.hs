-- | UGen data structure representation and associated functions.
module Sound.SC3.UGen.UGen where

import qualified Data.Char {- base -}
import Data.Maybe {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Sound.SC3.Common.Envelope as Envelope {- hsc3 -}
import qualified Sound.SC3.Common.Base as Base {- hsc3 -}
import qualified Sound.SC3.Common.Math.Operator as Operator {- hsc3 -}
import qualified Sound.SC3.Common.Rate as Rate {- hsc3 -}
import qualified Sound.SC3.Common.UId as UId {- hsc3 -}

import Sound.SC3.UGen.Type {- hsc3 -}

-- | 'UId' of 'resolveID'.
toUId :: UId.ID a => a -> UGenId
toUId = UId . UId.resolveID

-- | Lookup operator name for operator UGens, else UGen name.
ugen_user_name :: String -> Special -> String
ugen_user_name nm (Special n) = fromMaybe nm (Operator.ugen_operator_name nm n)

-- * UGen graph functions

-- | Depth first traversal of graph at `u', stopping at `halt_f', else applying `map_f' to each node.
ugenTraverse :: (UGen -> Bool) -> (UGen -> UGen) -> UGen -> UGen
ugenTraverse halt_f map_f u =
  if halt_f u
  then u
  else
    let recur = ugenTraverse halt_f map_f
    in case u of
         UGen (CPrimitive p) ->
             let i = ugenInputs p
             in map_f (UGen (CPrimitive (p {ugenInputs = map recur i})))
         UGen (CProxy p) ->
             let s = UGen (CPrimitive (proxySource p))
             in case recur s of
                  UGen (CPrimitive p') -> map_f (UGen (CProxy (p {proxySource = p'})))
                  _ -> error "ugenTraverse"
         UGen (CMce m) -> map_f (mce (map recur (mceProxies m)))
         UGen (CMrg (Mrg l r)) -> map_f (UGen (CMrg (Mrg (recur l) (recur r))))
         _ -> map_f u

{- | Right fold of UGen graph.

> map Sound.SC3.UGen.PP.ugen_concise_pp $ ugenFoldr (:) [] (pan2 (sinOsc AudioRate 440 0) 0.25 0.1)
-}
ugenFoldr :: (UGen -> a -> a) -> a -> UGen -> a
ugenFoldr f st u =
    let recur = flip (ugenFoldr f)
    in case u of
         UGen (CPrimitive p) -> f u (foldr recur st (ugenInputs p))
         UGen (CProxy p) -> f u (ugenFoldr f st (UGen (CPrimitive (proxySource p))))
         UGen (CMce m) -> f u (foldr recur st (mceProxies m))
         UGen (CMrg (Mrg l r)) -> f u (f l (f r st))
         _ -> f u st

-- * Unit generator node constructors

-- | Control input node constructor.
control_f64 :: Rate.Rate -> Maybe Int -> String -> Sample -> UGen
control_f64 r ix nm d = UGen (CControl (Control r ix nm d False Nothing))

-- | Control input node constructor.
--
-- Note that if the name begins with a t_ prefix the control is /not/
-- converted to a triggered control.  Please see 'trigControl'.
control :: Rate.Rate -> String -> Double -> UGen
control r = control_f64 r Nothing

-- | Variant of 'control' with meta data.
control_m :: Rate.Rate -> String -> Double -> Control_Meta_T3 Double -> UGen
control_m rt nm df meta =
    let m = control_meta_t3 id meta
    in UGen (CControl (Control rt Nothing nm df False (Just m)))

-- | Generate group of two controls.  Names are generated according to 'control_group_suffixes'
control_pair :: Control_Group -> Rate.Rate -> String -> (Double,Double) -> Control_Meta_T3 Double -> (UGen,UGen)
control_pair grp rt nm (df1,df2) meta =
    let m = (control_meta_t3 id meta) {controlGroup = Just grp}
    in case control_group_suffixes grp of
         [lhs,rhs] ->
           (UGen (CControl (Control rt Nothing (nm ++ lhs) df1 False (Just m)))
           ,UGen (CControl (Control rt Nothing (nm ++ rhs) df2 False (Just m))))
         _ -> error "control_pair"

-- | Generate range controls.  Names are generated according to 'control_group_suffixes'
control_rng :: Rate.Rate -> String -> (Double,Double) -> Control_Meta_T3 Double -> (UGen,UGen)
control_rng = control_pair Control_Range

-- | Triggered (kr) control input node constructor.
trigControl_f64 :: Maybe Int -> String -> Sample -> UGen
trigControl_f64 ix nm d = UGen (CControl (Control Rate.ControlRate ix nm d True Nothing))

-- | Triggered (kr) control input node constructor.
trigControl :: String -> Double -> UGen
trigControl = trigControl_f64 Nothing

-- | Set indices at a list of controls.
control_set :: [UGen] -> [UGen]
control_set =
    let f ix u = case u of
                   UGen (CControl c) -> UGen (CControl (c {controlIndex = Just ix}))
                   _ -> error "control_set: non control input?"
    in zipWith f [0..]

-- | Multiple root graph node constructor (left input is output)
mrg2 :: UGen -> UGen -> UGen
mrg2 u = UGen . CMrg . Mrg u

-- * Multiple channel expansion

mce1 :: UGen -> UGen
mce1 = mce . return

-- | Multiple channel expansion for two inputs.
mce2 :: UGen -> UGen -> UGen
mce2 x y = mce [x,y]

-- | Extract two channels from possible Mce, if there is only one channel it is duplicated.
mce2c :: UGen -> (UGen,UGen)
mce2c u =
    case mceChannels u of
      [] -> error "mce2c"
      [p] -> (p,p)
      p:q:_ -> (p,q)

-- | Variant of 'mce2c' that requires input to have two channels.
unmce2 :: UGen -> (UGen, UGen)
unmce2 = Base.t2_from_list . mceChannels

-- | Multiple channel expansion for two inputs.
mce3 :: UGen -> UGen -> UGen -> UGen
mce3 x y z = mce [x,y,z]

-- | Variant of 'mce2c' that requires input to have two channels.
unmce3 :: UGen -> (UGen, UGen, UGen)
unmce3 = Base.t3_from_list . mceChannels

-- | Apply a function to each channel at a unit generator.
mceMap :: (UGen -> UGen) -> UGen -> UGen
mceMap f u = mce (map f (mceChannels u))

-- | Map with element index.
map_ix :: ((Int,a) -> b) -> [a] -> [b]
map_ix f = zipWith (curry f) [0..]

-- | Variant of 'mceMap' with element index.
mce_map_ix :: ((Int,UGen) -> UGen) -> UGen -> UGen
mce_map_ix f u = mce (map_ix f (mceChannels u))

-- | Apply UGen list operation on Mce contents.
mceEdit :: ([UGen] -> [UGen]) -> UGen -> UGen
mceEdit f u =
    case u of
      UGen (CMce m) -> mce (f (mceProxies m))
      _ -> error "mceEdit: non Mce value"

-- | Reverse order of channels at Mce.
mceReverse :: UGen -> UGen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at Mce.
mceChannel :: Int -> UGen -> UGen
mceChannel n u =
    case u of
      UGen (CMce m) -> mceProxies m !! n
      _ -> if n == 0 then u else error "mceChannel: non Mce value, non ZERO index"

-- | Transpose rows and columns, ie. {{a,b},{c,d}} to {{a,c},{b,d}}.
mceTranspose :: UGen -> UGen
mceTranspose = mce . map mce . transpose . map mceChannels . mceChannels

-- | Rotate mce /k/ places to the right, ie. {a,b,c,d} to {d,a,b,c}
--
-- > mceRotate 1 (mce [1,2,3,4]) == mce [4,1,2,3]
mceRotate :: Int -> UGen -> UGen
mceRotate k =
  let rotateRight n p = let (b,a) = splitAt (length p - n) p in a ++ b
  in mce . rotateRight k . mceChannels

-- | 'concat' at mce channels of each input, ie. {{a,b},{c,d}} to {a,b,c,d}.
--
-- > mceConcat (map mce [[1,2],[3,4]]) == mce [1..4]
mceConcat :: [UGen] -> UGen
mceConcat = mce . concatMap mceChannels

-- | Collect subarrays of mce.
--
-- > mceClump 2 (mce [1,2,3,4]) == mce2 (mce2 1 2) (mce2 3 4)
mceClump :: Int -> UGen -> UGen
mceClump k = mce . map mce . Split.chunksOf k . mceChannels

-- * Transform

-- | Given /unmce/ function make halt mce transform.
halt_mce_transform_f :: (a -> [a]) -> [a] -> [a]
halt_mce_transform_f f l =
    let (l',e) = fromMaybe (error "halt_mce_transform: null?") (Base.sep_last l)
    in l' ++ f e

-- | The halt Mce transform, ie. lift channels of last input into list.
--   This is not used by hsc3, but it is used by hsc3-forth and stsc3.
--
-- > halt_mce_transform [1,2,mce2 3 4] == [1,2,3,4]
halt_mce_transform :: [UGen] -> [UGen]
halt_mce_transform = halt_mce_transform_f mceChannels

-- | If the root node of a UGen graph is /mce/, transform to /mrg/.
prepare_root :: UGen -> UGen
prepare_root u =
    case u of
      UGen (CMce m) -> mrg (mceProxies m)
      UGen (CMrg m) -> mrg2 (prepare_root (mrgLeft m)) (prepare_root (mrgRight m))
      _ -> u

-- * Multiple root graphs

-- * Labels

-- | Lift a 'String' to a UGen label (ie. for 'poll').
label :: String -> UGen
label = UGen . CLabel . Label

{- | Unpack a label to a length prefixed list of 'Constant's.  There
is a special case for mce nodes, but it requires labels to be equal
length.  Properly, 'poll' would not unpack the label, it would be
done by the synthdef builder.

> unpackLabel False (label "/tmp")

-}
unpackLabel :: Bool -> UGen -> [UGen]
unpackLabel length_prefix u =
    case u of
      UGen (CLabel (Label s)) ->
          let q = fromEnum '?'
              f c = if Data.Char.isAscii c then fromEnum c else q
              s' = map (fromIntegral . f) s
          in if length_prefix then fromIntegral (length s) : s' else s'
      UGen (CMce m) ->
          let x = map (unpackLabel length_prefix) (mceProxies m)
          in if Base.equal_length_p x
             then map mce (transpose x)
             else error (show ("unpackLabel: mce length /=",x))
      _ -> error (show ("unpackLabel: non-label",u))

-- * Envelope

-- | 'mce' of 'Envelope.envelope_sc3_array'.
envelope_to_ugen :: Envelope.Envelope UGen -> UGen
envelope_to_ugen =
    let err = error "envGen: bad Envelope"
    in mce . fromMaybe err . Envelope.envelope_sc3_array

-- | 'mce' of 'Envelope.envelope_sc3_ienvgen_array'.
envelope_to_ienvgen_ugen :: Envelope.Envelope UGen -> UGen
envelope_to_ienvgen_ugen =
    let err = error "envGen: bad Envelope"
    in mce . fromMaybe err . Envelope.envelope_sc3_ienvgen_array

-- * Bitwise

-- | 'Operator.BitAnd'
bitAnd :: UGen -> UGen -> UGen
bitAnd = mkBinaryOperator Operator.BitAnd undefined

-- | 'Operator.BitOr'
bitOr :: UGen -> UGen -> UGen
bitOr = mkBinaryOperator Operator.BitOr undefined

-- | 'Operator.BitXor'
bitXOr :: UGen -> UGen -> UGen
bitXOr = mkBinaryOperator Operator.BitXor undefined

-- | 'Operator.BitNot'
bitNot :: UGen -> UGen
bitNot = mkUnaryOperator Operator.BitNot undefined

-- | 'Operator.ShiftLeft'
shiftLeft :: UGen -> UGen -> UGen
shiftLeft = mkBinaryOperator Operator.ShiftLeft undefined

-- | 'Operator.ShiftRight'
shiftRight :: UGen -> UGen -> UGen
shiftRight = mkBinaryOperator Operator.ShiftRight undefined

-- | 'Operator.UnsignedShift'
unsignedShift :: UGen -> UGen -> UGen
unsignedShift = mkBinaryOperator Operator.UnsignedShift undefined

-- | 'shiftLeft' operator.
(.<<.) :: UGen -> UGen -> UGen
(.<<.) = shiftLeft

-- | 'shiftRight' operator.
(.>>.) :: UGen -> UGen -> UGen
(.>>.) = shiftRight

-- * Rate Flow

-- | Traverse graph rewriting audio rate nodes as control rate.
rewriteUGenRates :: (Rate.Rate -> Bool) -> Rate.Rate -> UGen -> UGen
rewriteUGenRates sel_f set_rt =
  let f u = case u of
              UGen (CPrimitive p) ->
                let Primitive rt nm i o s z = p
                in UGen (CPrimitive (if sel_f rt then Primitive set_rt nm i o s z else p))
              _ -> u
  in ugenTraverse (const False) f -- requires endRewrite node (see rsc3-arf)

-- | Traverse graph rewriting audio rate nodes as control rate.
rewriteToControlRate :: UGen -> UGen
rewriteToControlRate = rewriteUGenRates (== Rate.AudioRate) Rate.ControlRate

-- | Traverse graph rewriting all nodes as demand rate.
rewriteToDemandRate :: UGen -> UGen
rewriteToDemandRate = rewriteUGenRates (const True) Rate.DemandRate

-- | Traverse graph rewriting audio and control nodes as initialisation rate.
rewriteToInitialisationRate :: UGen -> UGen
rewriteToInitialisationRate = rewriteUGenRates (`elem` [Rate.ControlRate,Rate.AudioRate]) Rate.InitialisationRate

-- | Select rewriting function given 'Rate.Rate'.
rewriteToRate :: Rate.Rate -> UGen -> UGen
rewriteToRate rt =
  case rt of
    Rate.ControlRate -> rewriteToControlRate
    Rate.DemandRate -> rewriteToDemandRate
    Rate.InitialisationRate -> rewriteToInitialisationRate
    Rate.AudioRate -> error "rewriteToRate: AudioRate?"
