-- | Utility function over Ugen data structure.
module Sound.Sc3.Ugen.Util where

import qualified Data.Char {- base -}
import Data.Maybe {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Sound.Sc3.Common.Envelope as Envelope {- hsc3 -}
import qualified Sound.Sc3.Common.Base as Base {- hsc3 -}
import qualified Sound.Sc3.Common.Mce as Mce {- hsc3 -}
import qualified Sound.Sc3.Common.Rate as Rate {- hsc3 -}
import qualified Sound.Sc3.Common.Uid as Uid {- hsc3 -}

import Sound.Sc3.Ugen.Brackets {- hsc3 -}
import Sound.Sc3.Ugen.Control {- hsc3 -}
import Sound.Sc3.Ugen.Label {- hsc3 -}
import Sound.Sc3.Ugen.Mrg {- hsc3 -}
import Sound.Sc3.Ugen.Primitive {- hsc3 -}
import Sound.Sc3.Ugen.Proxy {- hsc3 -}
import Sound.Sc3.Ugen.Ugen {- hsc3 -}

-- | 'Uid' of 'resolveID'.
toUid :: Uid.ID a => a -> UgenId
toUid = Uid . Uid.resolveID

-- * Ugen graph functions

-- | Depth first traversal of graph at `u', stopping at `halt_f', else applying `map_f' to each node.
ugenTraverse :: (Ugen -> Bool) -> (Ugen -> Ugen) -> Ugen -> Ugen
ugenTraverse halt_f map_f u =
  if halt_f u
  then u
  else
    let recur = ugenTraverse halt_f map_f
    in case u of
         Primitive_U p ->
             let i = ugenInputs p
             in map_f (Primitive_U (p {ugenInputs = map recur i}))
         Proxy_U (Proxy p ix) ->
             let i = ugenInputs p
             in map_f (Proxy_U (Proxy (p {ugenInputs = map recur i}) ix))
         Mce_U m -> map_f (mce (map recur (mceProxies m)))
         Mrg_U (Mrg l r) -> map_f (Mrg_U (Mrg (recur l) (recur r)))
         _ -> map_f u

{- | Right fold of Ugen graph.

> map Sound.Sc3.Ugen.PP.ugen_concise_pp $ ugenFoldr (:) [] (sinOsc ar 440 0 * 0.1)
> map Sound.Sc3.Ugen.PP.ugen_concise_pp $ ugenFoldr (:) [] (pan2 (sinOsc ar 440 0) 0.25 0.1)
-}
ugenFoldr :: (Ugen -> a -> a) -> a -> Ugen -> a
ugenFoldr f st u =
    let recur = flip (ugenFoldr f)
    in case u of
         Primitive_U p -> f u (foldr recur st (ugenInputs p))
         Proxy_U (Proxy p _) -> f u (foldr recur st (ugenInputs p)) -- ...
         Mce_U m -> f u (foldr recur st (mceProxies m))
         Mrg_U (Mrg l r) -> f u (f l (f r st))
         _ -> f u st

-- | Fold over Ugen and collect all bracketing messages from all Primitive nodes.
ugenCollectBrackets :: Ugen -> Brackets
ugenCollectBrackets =
  concatBrackets .
  map ugenBrackets .
  nub .
  ugenFoldr (:) []

-- | Are there any brackets at Ugen.
ugenHasAnyBrackets :: Ugen -> Bool
ugenHasAnyBrackets = (/= ([],[])) . ugenCollectBrackets

-- * Unit generator node constructors

-- | Control input node constructor.
control_f64 :: Rate.Rate -> Maybe Int -> String -> Sample -> Ugen
control_f64 r ix nm d = Control_U (Control r ix nm d False Nothing emptyBrackets)

-- | Control input node constructor.
--
-- Note that if the name begins with a t_ prefix the control is /not/
-- converted to a triggered control.  Please see 'trigControl'.
control :: Rate.Rate -> String -> Double -> Ugen
control r = control_f64 r Nothing

-- | Variant of 'control' with meta data.
control_m :: Rate.Rate -> String -> Double -> Control_Meta_T3 Double -> Ugen
control_m rt nm df meta =
    let m = control_meta_t3 id meta
    in Control_U (Control rt Nothing nm df False (Just m) emptyBrackets)

-- | Generate group of two controls.  Names are generated according to 'control_group_suffixes'
control_pair :: Control_Group -> Rate.Rate -> String -> (Double,Double) -> Control_Meta_T3 Double -> (Ugen,Ugen)
control_pair grp rt nm (df1,df2) meta =
    let m = (control_meta_t3 id meta) {controlGroup = Just grp}
    in case control_group_suffixes grp of
         [lhs,rhs] ->
           (Control_U (Control rt Nothing (nm ++ lhs) df1 False (Just m) emptyBrackets)
           ,Control_U (Control rt Nothing (nm ++ rhs) df2 False (Just m) emptyBrackets))
         _ -> error "control_pair"

-- | Generate range controls.  Names are generated according to 'control_group_suffixes'
control_rng :: Rate.Rate -> String -> (Double,Double) -> Control_Meta_T3 Double -> (Ugen,Ugen)
control_rng = control_pair Control_Range

-- | Triggered (kr) control input node constructor.
trigControl_f64 :: Maybe Int -> String -> Sample -> Ugen
trigControl_f64 ix nm d = Control_U (Control Rate.ControlRate ix nm d True Nothing emptyBrackets)

-- | Triggered (kr) control input node constructor.
trigControl :: String -> Double -> Ugen
trigControl = trigControl_f64 Nothing

-- | Set indices at a list of controls.
control_set :: [Ugen] -> [Ugen]
control_set =
    let f ix u = case u of
                   Control_U c -> Control_U (c {controlIndex = Just ix})
                   _ -> error "control_set: non control input?"
    in zipWith f [0..]

-- * Multiple channel expansion

mce1 :: Ugen -> Ugen
mce1 = mce . return

-- | Multiple channel expansion for two inputs.
mce2 :: Ugen -> Ugen -> Ugen
mce2 x y = mce [x,y]

-- | Extract two channels from possible Mce, if there is only one channel it is duplicated.
mce2c :: Ugen -> (Ugen,Ugen)
mce2c u =
    case mceChannels u of
      [] -> error "mce2c"
      [p] -> (p,p)
      p:q:_ -> (p,q)

-- | Variant of 'mce2c' that requires input to have two channels.
unmce2 :: Ugen -> (Ugen, Ugen)
unmce2 = Base.t2_from_list . mceChannels

-- | Multiple channel expansion for two inputs.
mce3 :: Ugen -> Ugen -> Ugen -> Ugen
mce3 x y z = mce [x,y,z]

-- | Variant of 'mce2c' that requires input to have two channels.
unmce3 :: Ugen -> (Ugen, Ugen, Ugen)
unmce3 = Base.t3_from_list . mceChannels

-- | Apply a function to each channel at a unit generator.
mceMap :: (Ugen -> Ugen) -> Ugen -> Ugen
mceMap f u = mce (map f (mceChannels u))

-- | Map with element index.
map_ix :: ((Int,a) -> b) -> [a] -> [b]
map_ix f = zipWith (curry f) [0..]

-- | Variant of 'mceMap' with element index.
mce_map_ix :: ((Int,Ugen) -> Ugen) -> Ugen -> Ugen
mce_map_ix f u = mce (map_ix f (mceChannels u))

-- | Apply Ugen list operation on Mce contents.
mceEdit :: ([Ugen] -> [Ugen]) -> Ugen -> Ugen
mceEdit f u =
    case u of
      Mce_U m -> mce (f (mceProxies m))
      _ -> error "mceEdit: non Mce value"

-- | Reverse order of channels at Mce.
mceReverse :: Ugen -> Ugen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at Mce.
mceChannel :: Int -> Ugen -> Ugen
mceChannel n u =
    case u of
      Mce_U m -> mceProxies m !! n
      _ -> if n == 0 then u else error "mceChannel: non Mce value, non ZERO index"

{- | Obtain indexed channel at Mce, indicex wrap around.

> map (\ix -> mceChannelWrap ix (mce [1,2,3,4,5])) [0 .. 9]
-}
mceChannelWrap :: Int -> Ugen -> Ugen
mceChannelWrap n u =
    case u of
      Mce_U m -> mceProxies m !! (n `mod` Mce.mce_length m)
      _ -> u

-- | Transpose rows and columns, ie. {{a,b},{c,d}} to {{a,c},{b,d}}.
mceTranspose :: Ugen -> Ugen
mceTranspose = mce . map mce . transpose . map mceChannels . mceChannels

{- | Rotate mce /k/ places to the right, ie. {a,b,c,d} to {d,a,b,c}

>>> mceRotate 1 (mce [1,2,3,4]) == mce [4,1,2,3]
True
-}
mceRotate :: Int -> Ugen -> Ugen
mceRotate k =
  let rotateRight n p = let (b,a) = splitAt (length p - n) p in a ++ b
  in mce . rotateRight k . mceChannels

{- | 'concat' at mce channels of each input, ie. {{a,b},{c,d}} to {a,b,c,d}.

>>> mceConcat (map mce [[1,2],[3,4]]) == mce [1..4]
True
-}
mceConcat :: [Ugen] -> Ugen
mceConcat = mce . concatMap mceChannels

{- | Collect subarrays of mce.

>>> mceClump 2 (mce [1,2,3,4]) == mce2 (mce2 1 2) (mce2 3 4)
True
-}
mceClump :: Int -> Ugen -> Ugen
mceClump k = mce . map mce . Split.chunksOf k . mceChannels

-- | Foldl1 at channels of mce.
mceReduce :: (Ugen -> Ugen -> Ugen) -> Ugen -> Ugen
mceReduce f = foldl1 f . mceChannels

-- | mceReduce of *.
mceProduct :: Ugen -> Ugen
mceProduct = mceReduce (*)

-- * Transform

-- | Given /unmce/ function make halt mce transform.
halt_mce_transform_f :: (a -> [a]) -> [a] -> [a]
halt_mce_transform_f f l =
    let (l',e) = fromMaybe (error "halt_mce_transform: null?") (Base.sep_last l)
    in l' ++ f e

{- | The halt Mce transform, ie. lift channels of last input into list.
This is not used by hsc3, but it is used by hsc3-forth and stsc3.

>>> halt_mce_transform [1,2,mce2 3 4] == [1,2,3,4]
True
-}
halt_mce_transform :: [Ugen] -> [Ugen]
halt_mce_transform = halt_mce_transform_f mceChannels

-- | If the root node of a Ugen graph is /mce/, transform to /mrg/.
prepare_root :: Ugen -> Ugen
prepare_root u =
    case u of
      Mce_U m -> mrg (mceProxies m)
      Mrg_U m -> mrg2 (prepare_root (mrgLeft m)) (prepare_root (mrgRight m))
      _ -> u

-- * Multiple root graphs

-- | Multiple root graph node constructor (left input is output)
mrg2 :: Ugen -> Ugen -> Ugen
mrg2 u = Mrg_U . Mrg u

-- * Labels

-- | Lift a 'String' to a Ugen label (ie. for 'poll').
label :: String -> Ugen
label = Label_U . Label

{- | Unpack a label to a length prefixed list of 'Constant's.  There
is a special case for mce nodes, but it requires labels to be equal
length.  Properly, 'poll' would not unpack the label, it would be
done by the synthdef builder.

> unpackLabel False (label "/tmp")
-}
unpackLabel :: Bool -> Ugen -> [Ugen]
unpackLabel length_prefix u =
    case u of
      Label_U (Label s) ->
          let q = fromEnum '?'
              f c = if Data.Char.isAscii c then fromEnum c else q
              s' = map (fromIntegral . f) s
          in if length_prefix then fromIntegral (length s) : s' else s'
      Mce_U m ->
          let x = map (unpackLabel length_prefix) (mceProxies m)
          in if Base.equal_length_p x
             then map mce (transpose x)
             else error (show ("unpackLabel: mce length /=",x))
      _ -> error (show ("unpackLabel: non-label",u))

-- * Envelope

-- | 'mce' of 'Envelope.envelope_sc3_array'.
envelope_to_ugen :: Envelope.Envelope Ugen -> Ugen
envelope_to_ugen =
    let err = error "envGen: bad Envelope"
    in mce . fromMaybe err . Envelope.envelope_sc3_array

-- | 'mce' of 'Envelope.envelope_sc3_ienvgen_array'.
envelope_to_ienvgen_ugen :: Envelope.Envelope Ugen -> Ugen
envelope_to_ienvgen_ugen =
    let err = error "envGen: bad Envelope"
    in mce . fromMaybe err . Envelope.envelope_sc3_ienvgen_array

-- * Rate Flow

-- | Traverse graph rewriting audio rate nodes as control rate.
rewriteUgenRates :: (Rate.Rate -> Bool) -> Rate.Rate -> Ugen -> Ugen
rewriteUgenRates sel_f set_rt =
  let f u = case u of
              Primitive_U (Primitive rt nm i o s z b) -> Primitive_U (Primitive (if sel_f rt then set_rt else rt) nm i o s z b)
              _ -> u
  in ugenTraverse (const False) f -- requires endRewrite node (see rsc3-arf)

-- | Traverse graph rewriting audio rate nodes as control rate.
rewriteToControlRate :: Ugen -> Ugen
rewriteToControlRate = rewriteUgenRates (== Rate.AudioRate) Rate.ControlRate

-- | Traverse graph rewriting all nodes as demand rate.
rewriteToDemandRate :: Ugen -> Ugen
rewriteToDemandRate = rewriteUgenRates (const True) Rate.DemandRate

-- | Traverse graph rewriting audio and control nodes as initialisation rate.
rewriteToInitialisationRate :: Ugen -> Ugen
rewriteToInitialisationRate = rewriteUgenRates (`elem` [Rate.ControlRate,Rate.AudioRate]) Rate.InitialisationRate

-- | Select rewriting function given 'Rate.Rate'.
rewriteToRate :: Rate.Rate -> Ugen -> Ugen
rewriteToRate rt =
  case rt of
    Rate.ControlRate -> rewriteToControlRate
    Rate.DemandRate -> rewriteToDemandRate
    Rate.InitialisationRate -> rewriteToInitialisationRate
    Rate.AudioRate -> error "rewriteToRate: AudioRate?"
