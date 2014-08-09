-- | UGen data structure representation and associated functions.
module Sound.SC3.UGen.UGen where

import qualified Data.Char as C {- base -}
import Data.Maybe {- base -}
import Data.List {- base -}

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.MCE
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

-- | 'UId' of 'resolveID'.
toUId :: (ID a) => a -> UGenId
toUId = UId . resolveID

-- | Lookup operator name for operator UGens, else UGen name.
ugen_user_name :: String -> Special -> String
ugen_user_name nm (Special n) =
    case nm of
      "UnaryOpUGen" -> unaryName n
      "BinaryOpUGen" -> binaryName n
      _ -> nm

-- * UGen graph functions

-- | Depth first traversal of graph at `u' applying `f' to each node.
ugenTraverse :: (UGen -> UGen) -> UGen -> UGen
ugenTraverse f u =
    let rec = ugenTraverse f
    in case u of
         Primitive_U p ->
             let i = ugenInputs p
             in f (Primitive_U (p {ugenInputs = map rec i}))
         Proxy_U p ->
             let s = Primitive_U (proxySource p)
             in case rec s of
                  Primitive_U p' -> f (Proxy_U (p {proxySource = p'}))
                  _ -> error "ugenTraverse"
         MCE_U m -> f (mce (map rec (mceProxies m)))
         MRG_U (MRG l r) -> f (MRG_U (MRG (rec l) (rec r)))
         _ -> f u

-- | Right fold of UGen graph.
ugenFoldr :: (UGen -> a -> a) -> a -> UGen -> a
ugenFoldr f st u =
    let rec = flip (ugenFoldr f)
    in case u of
         Primitive_U p ->
             let i = ugenInputs p
             in f u (foldr rec st i)
         Proxy_U p ->
             let s = proxySource p
             in f u (f (Primitive_U s) st)
         MCE_U m -> f u (foldr rec st (mceProxies m))
         MRG_U (MRG l r) -> f u (f l (f r st))
         _ -> f u st

-- * Unit generator node constructors

-- | Control input node constructor.
control_f64 :: Rate -> Maybe Int -> String -> Sample -> UGen
control_f64 r ix nm d = Control_U (Control r ix nm d False Nothing)

-- | Control input node constructor.
--
-- Note that if the name begins with a t_ prefix the control is /not/
-- converted to a triggered control.  Please see 'tr_control'.
control :: Rate -> String -> Double -> UGen
control r nm = control_f64 r Nothing nm -- . realToFrac

-- | Variant of 'control' with meta data.
meta_control :: Rate -> String -> Double -> C_Meta' Double -> UGen
meta_control rt nm df meta =
    let m = c_meta' id meta
    in Control_U (Control rt Nothing nm df False (Just m))

-- | Triggered (kr) control input node constructor.
tr_control_f64 :: Maybe Int -> String -> Sample -> UGen
tr_control_f64 ix nm d = Control_U (Control KR ix nm d True Nothing)

-- | Triggered (kr) control input node constructor.
tr_control :: String -> Double -> UGen
tr_control nm = tr_control_f64 Nothing nm -- . realToFrac

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

-- | Extract two channels from possible MCE.
mce2c :: UGen -> (UGen,UGen)
mce2c u =
    case u of
      MCE_U m -> case mceProxies m of
                     [] -> error "mce2c: nil mce"
                     p:[] -> (p,p)
                     p:q:_ -> (p,q)
      _ -> (u,u)

-- | Multiple channel expansion for two inputs.
mce3 :: UGen -> UGen -> UGen -> UGen
mce3 x y z = mce [x,y,z]

-- | Apply a function to each channel at a unit generator.
mceMap :: (UGen -> UGen) -> UGen -> UGen
mceMap f u = mce (map f (mceChannels u))

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

-- | Separate first list element.
--
-- > sep_first "astring" == Just ('a',"string")
sep_first :: [t] -> Maybe (t,[t])
sep_first l =
    case l of
      e:l' -> Just (e,l')
      _ -> Nothing

-- | Separate last list element.
--
-- > sep_last "stringb" == Just ("string",'b')
sep_last :: [t] -> Maybe ([t], t)
sep_last =
    let f (e,l) = (reverse l,e)
    in fmap f . sep_first . reverse

-- | Given /unmce/ function make halt mce transform.
halt_mce_transform' :: (a -> [a]) -> [a] -> [a]
halt_mce_transform' f l =
    let (l',e) = fromMaybe (error "halt_mce_transform: null?") (sep_last l)
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

-- | Are lists of equal length?
--
-- > equal_length_p ["t1","t2"] == True
-- > equal_length_p ["t","t1","t2"] == False
equal_length_p :: [[a]] -> Bool
equal_length_p = (== 1) . length . nub . map length

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
          in if equal_length_p x
             then map mce (transpose x)
             else error (show ("unpackLabel: mce length /=",x))
      _ -> error (show ("unpackLabel: non-label",u))

-- * Bitwise

bitAnd :: UGen -> UGen -> UGen
bitAnd = mkBinaryOperator BitAnd undefined

bitOr :: UGen -> UGen -> UGen
bitOr = mkBinaryOperator BitOr undefined

bitXOr :: UGen -> UGen -> UGen
bitXOr = mkBinaryOperator BitXor undefined

bitNot :: UGen -> UGen
bitNot = mkUnaryOperator BitNot undefined

shiftLeft :: UGen -> UGen -> UGen
shiftLeft = mkBinaryOperator ShiftLeft undefined

shiftRight :: UGen -> UGen -> UGen
shiftRight = mkBinaryOperator ShiftRight undefined

unsignedShift :: UGen -> UGen -> UGen
unsignedShift = mkBinaryOperator UnsignedShift undefined

(.<<.) :: UGen -> UGen -> UGen
(.<<.) = shiftLeft

(.>>.) :: UGen -> UGen -> UGen
(.>>.) = shiftRight

-- * Analysis

-- | UGen primitive.  Sees through Proxy and MRG, possible multiple
-- primitives for MCE.
ugen_primitive :: UGen -> [Primitive]
ugen_primitive u =
    case u of
      Constant_U _ -> []
      Control_U _ -> []
      Label_U _ -> []
      Primitive_U p -> [p]
      Proxy_U p -> [proxySource p]
      MCE_U m -> concatMap ugen_primitive (mce_elem m)
      MRG_U m -> ugen_primitive (mrgLeft m)

-- | Heuristic based on primitive name (@FFT@, @PV_@).  Note that
-- @IFFT@ is at /control/ rate, not @PV@ rate.
primitive_is_pv_rate :: String -> Bool
primitive_is_pv_rate nm = nm == "FFT" || "PV_" `isPrefixOf` nm

-- | Variant on primitive_is_pv_rate.
ugen_is_pv_rate :: UGen -> Bool
ugen_is_pv_rate = any (primitive_is_pv_rate . ugenName)
                  . ugen_primitive

-- | Traverse input graph until an @FFT@ or @PV_Split@ node is
-- encountered, and then locate the buffer input.  Biases left at MCE
-- nodes.
--
-- > import Sound.SC3
-- > let z = soundIn 4
-- > let f1 = fft 10 z 0.5 0 1 0
-- > let f2 = ffta 'a' 1024 z 0.5 0 1 0
-- > pv_track_buffer (pv_BrickWall f1 0.5) == Right 10
-- > pv_track_buffer (pv_BrickWall f2 0.5) == Right (localBuf 'a' 1024 1)
pv_track_buffer :: UGen -> Either String UGen
pv_track_buffer u =
    case ugen_primitive u of
      [] -> Left "pv_track_buffer: not located"
      p:_ -> case ugenName p of
               "FFT" -> Right (ugenInputs p !! 0)
               "PV_Split" -> Right (ugenInputs p !! 1)
               _ -> pv_track_buffer (ugenInputs p !! 0)

-- | Buffer node number of frames. Biases left at MCE nodes.  Sees
-- through @LocalBuf@, otherwise uses 'bufFrames'.
--
-- > buffer_nframes 10 == bufFrames IR 10
-- > buffer_nframes (control KR "b" 0) == bufFrames KR (control KR "b" 0)
-- > buffer_nframes (localBuf 'α' 2048 1) == 2048
buffer_nframes :: UGen -> UGen
buffer_nframes u =
    let b = mkUGen Nothing [IR,KR] (Left (rateOf u)) "BufFrames" [u] Nothing 1 (Special 0) NoId
    in case ugen_primitive u of
         [] -> b
         p:_ -> case ugenName p of
                  "LocalBuf" -> ugenInputs p !! 1
                  _ -> b

-- | 'pv_track_buffer' then 'buffer_nframes'.
pv_track_nframes :: UGen -> Either String UGen
pv_track_nframes u = pv_track_buffer u >>= Right . buffer_nframes
