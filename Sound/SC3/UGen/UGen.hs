-- | UGen data structure representation and associated functions.
module Sound.SC3.UGen.UGen where

import qualified Data.Char as C {- base -}
import Data.List {- base -}

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

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
control_f32 :: Rate -> Maybe Int -> String -> Float -> UGen
control_f32 r ix nm d = Control_U (Control r ix nm d False)

-- | Control input node constructor.
--
-- Note that if the name begins with a t_ prefix the control is /not/
-- converted to a triggered control.  Please see tr_control.
control :: Rate -> String -> Double -> UGen
control r nm = control_f32 r Nothing nm . realToFrac

-- | Triggered (kr) control input node constructor.
tr_control_f32 :: Maybe Int -> String -> Float -> UGen
tr_control_f32 ix nm d = Control_U (Control KR ix nm d True)

-- | Triggered (kr) control input node constructor.
tr_control :: String -> Double -> UGen
tr_control nm = tr_control_f32 Nothing nm . realToFrac

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

-- * Unit generator function builders

-- | Oscillator constructor with constrained set of operating 'Rate's.
mk_osc :: [Rate] -> UGenId -> Rate -> String -> [UGen] -> Int -> UGen
mk_osc rs z r c i o =
    if r `elem` rs
    then mkUGen Nothing rs (Just r) c i o (Special 0) z
    else error ("mk_osc: rate restricted: " ++ show (r, rs, c))

-- | 'UGenId' used for deterministic UGens.
no_id :: UGenId
no_id = NoId

-- | Oscillator constructor with 'all_rates'.
mkOsc :: Rate -> String -> [UGen] -> Int -> UGen
mkOsc = mk_osc all_rates no_id

-- | Oscillator constructor, rate restricted variant.
mkOscR :: [Rate] -> Rate -> String -> [UGen] -> Int -> UGen
mkOscR rs = mk_osc rs no_id

toUId :: (ID a) => a -> UGenId
toUId = UId . resolveID

-- | Rate restricted oscillator constructor, setting identifier.
mkOscIdR :: (ID a) => [Rate] -> a -> Rate -> String -> [UGen] -> Int -> UGen
mkOscIdR rr z = mk_osc rr (toUId z)

-- | Oscillator constructor, setting identifier.
mkOscId :: (ID a) => a -> Rate -> String -> [UGen] -> Int -> UGen
mkOscId z = mk_osc all_rates (toUId z)

-- | Provided 'UGenId' variant of 'mkOscMCE'.
mk_osc_mce :: UGenId -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mk_osc_mce z r c i j =
    let i' = i ++ mceChannels j
    in mk_osc all_rates z r c i'

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCE :: Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCE = mk_osc_mce no_id

-- | Variant oscillator constructor with MCE collapsing input.
mkOscMCEId :: ID a => a -> Rate -> String -> [UGen] -> UGen -> Int -> UGen
mkOscMCEId z = mk_osc_mce (toUId z)

-- | Rate constrained filter 'UGen' constructor.
mk_filter :: [Rate] -> UGenId -> String -> [UGen] -> Int -> UGen
mk_filter rs z c i o = mkUGen Nothing rs Nothing c i o (Special 0) z

-- | Filter 'UGen' constructor.
mkFilter :: String -> [UGen] -> Int -> UGen
mkFilter = mk_filter all_rates no_id

-- | Filter UGen constructor.
mkFilterR :: [Rate] -> String -> [UGen] -> Int -> UGen
mkFilterR rs = mk_filter rs no_id

-- | Filter UGen constructor.
mkFilterId :: (ID a) => a -> String -> [UGen] -> Int -> UGen
mkFilterId z = mk_filter all_rates (toUId z)

-- | Variant filter with rate derived from keyed input.
mkFilterKeyed :: String -> Int -> [UGen] -> Int -> UGen
mkFilterKeyed c k i o =
    let r = rateOf (i !! k)
    in mkUGen Nothing all_rates (Just r) c i o (Special 0) no_id

-- | Provided 'UGenId' filter with 'mce' input.
mk_filter_mce :: [Rate] -> UGenId -> String -> [UGen] -> UGen -> Int -> UGen
mk_filter_mce rs z c i j = mk_filter rs z c (i ++ mceChannels j)

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCER :: [Rate] -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCER rs = mk_filter_mce rs no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCE :: String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCE = mk_filter_mce all_rates no_id

-- | Variant filter constructor with MCE collapsing input.
mkFilterMCEId :: ID a => a -> String -> [UGen] -> UGen -> Int -> UGen
mkFilterMCEId z = mk_filter_mce all_rates (toUId z)

-- | Information unit generators are very specialized.
mkInfo :: String -> UGen
mkInfo name = mkOsc IR name [] 1

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

-- | UGen primitive.  Sees through Proxy and MRG.  Errors on MCE.
ugen_primitive :: UGen -> Maybe Primitive
ugen_primitive u =
    case u of
      Constant_U _ -> Nothing
      Control_U _ -> Nothing
      Label_U _ -> Nothing
      Primitive_U p -> Just p
      Proxy_U p -> Just (proxySource p)
      MCE_U _ -> error "ugen_primitive: MCE"
      MRG_U m -> ugen_primitive (mrgLeft m)

-- | Heuristic, based on primitive name (@FFT@, @IFFT@, @PV_@).
ugen_is_pv_rate :: UGen -> Bool
ugen_is_pv_rate u =
    case fmap ugenName (ugen_primitive u) of
      Just nm -> nm `elem` ["FFT","IFFT"] || "PV_" `isPrefixOf` nm
      Nothing -> False

-- | Traverse input graph until an @FFT@ or @PV_Split@ node is
-- encountered, and then locates the buffer input.
pv_track_buffer :: UGen -> Either String UGen
pv_track_buffer u =
    case ugen_primitive u of
      Nothing -> Left "pv_track_buffer: not located"
      Just p -> case ugenName p of
                  "FFT" -> Right (ugenInputs p !! 0)
                  "PV_Split" -> Right (ugenInputs p !! 1)
                  _ -> pv_track_buffer (ugenInputs p !! 0)

-- | Buffer node number of frames, only implemented for @LocalBuf@.
buffer_nframes :: UGen -> Either String UGen
buffer_nframes u =
    case ugen_primitive u of
      Nothing -> Left "buffer_nframes: not primitive"
      Just p -> case ugenName p of
                  "LocalBuf" -> Right (ugenInputs p !! 1)
                  _ -> Left "buffer_nframes: not LocalBuf"

-- | 'pv_track_buffer' then 'buffer_nframes'.
pv_track_nframes :: UGen -> Either String UGen
pv_track_nframes u = pv_track_buffer u >>= buffer_nframes
