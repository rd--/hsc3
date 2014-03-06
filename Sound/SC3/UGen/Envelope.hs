-- | Envelope generators.
module Sound.SC3.UGen.Envelope where

import Data.List
import Data.Maybe
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- * Envelope

-- | SC3 envelope segment model
data Envelope a =
    Envelope {env_levels :: [a] -- ^ Set of /n/ levels, n is >= 1
             ,env_times :: [a] -- ^ Set of /n-1/ time intervals
             ,env_curves :: [Envelope_Curve a] -- ^ Possibly empty curve set
             ,env_release_node :: Maybe Int -- ^ Maybe index to release node
             ,env_loop_node :: Maybe Int -- ^ Maybe index to loop node
             }
    deriving (Eq,Show)

-- | Duration of 'Envelope', ie. 'sum' '.' 'env_times'.
envelope_duration :: Num n => Envelope n -> n
envelope_duration = sum . env_times

-- | Number of segments at 'Envelope', ie. 'length' '.' 'env_times'.
envelope_n_segments :: (Num n,Integral i) => Envelope n -> i
envelope_n_segments = genericLength . env_times

-- | Determine which envelope segment a given time /t/ falls in.
envelope_segment_ix :: (Ord a, Num a) => Envelope a -> a -> Maybe Int
envelope_segment_ix e t =
    let d = dx_d (env_times e)
    in findIndex (>= t) d

-- | A set of start time, start level, end time, end level and curve.
type Envelope_Segment t = (t,t,t,t,Envelope_Curve t)

-- | Extract envelope segment given at index /i/.
envelope_segment :: Num t => Envelope t -> Int -> Envelope_Segment t
envelope_segment e i =
    let l = env_levels e
        t = env_times e
        x0 = l !! i
        x1 = l !! (i + 1)
        t0 = (0 : dx_d t) !! i
        t1 = t0 + t !! i
        c = envelope_curves e !! i
    in (t0,x0,t1,x1,c)

-- | Get value for 'Envelope' at time /t/, or zero if /t/ is out of
-- range.
envelope_at :: (Ord t, Floating t) => Envelope t -> t -> t
envelope_at e t =
    case envelope_segment_ix e t of
      Just n -> let (t0,x0,t1,x1,c) = envelope_segment e n
                    d = t1 - t0
                    t' = (t - t0) / d
                    f = env_curve_interpolation_f c
                in f x0 x1 t'
      Nothing -> 0

-- | Render 'Envelope' to breakpoint set of /n/ places.
envelope_render :: (Ord t, Floating t, Enum t) => t -> Envelope t -> [(t,t)]
envelope_render n e =
    let d = envelope_duration e
        k = d / (n - 1)
        t = [0,k .. d]
    in zip t (map (envelope_at e) t)

-- | Contruct a lookup table of /n/ places from 'Envelope'.
envelope_table :: (Ord t, Floating t, Enum t) => t -> Envelope t -> [t]
envelope_table n = map snd . envelope_render n

-- | Variant on 'env_curves' that expands the, possibly empty, user
-- list by cycling (if not empty) or by filling with 'EnvLin'.
envelope_curves :: Num a => Envelope a -> [Envelope_Curve a]
envelope_curves e =
    let c = env_curves e
        n = envelope_n_segments e
    in if null c
       then replicate n EnvLin
       else take n (cycle c)

-- | Linear SC3 form of 'Envelope' data.
--
-- > let {l = [0,0.6,0.3,1.0,0]
-- >     ;t = [0.1,0.02,0.4,1.1]
-- >     ;c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
-- >     ;e = Envelope l t c Nothing Nothing
-- >     ;r = [0,4,-99,-99,0.6,0.1,1,0,0.3,0.02,2,0,1,0.4,5,-6,0,1.1,3,0]}
-- > in envelope_sc3_array e == Just r
envelope_sc3_array :: Num a => Envelope a -> Maybe [a]
envelope_sc3_array e =
    let Envelope l t _ rn ln = e
        n = length t
        n' = fromIntegral n
        rn' = fromIntegral (fromMaybe (-99) rn)
        ln' = fromIntegral (fromMaybe (-99) ln)
        c = envelope_curves e
        f i j k = [i,j,env_curve_shape k,env_curve_value k]
    in case l of
         l0:l' -> Just (l0 : n' : rn' : ln' : concat (zipWith3 f l' t c))
         _ -> Nothing

-- | @IEnvGen@ SC3 form of 'Envelope' data.  Offset not supported (zero).
--
-- > let {l = [0,0.6,0.3,1.0,0]
-- >     ;t = [0.1,0.02,0.4,1.1]
-- >     ;c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
-- >     ;e = Envelope l t c Nothing Nothing
-- >     ;r = [0,0,4,1.62,0.1,1,0,0.6,0.02,2,0,0.3,0.4,5,-6,1,1.1,3,0,0]}
-- > in envelope_sc3_ienvgen_array e == Just r
envelope_sc3_ienvgen_array :: Num a => Envelope a -> Maybe [a]
envelope_sc3_ienvgen_array e =
    let Envelope l t _ _ _ = e
        n = length t
        n' = fromIntegral n
        c = envelope_curves e
        f i j k = [j,env_curve_shape k,env_curve_value k,i]
    in case l of
         l0:l' -> Just (0 : l0 : n' : sum t : concat (zipWith3 f l' t c))
         _ -> Nothing

-- | 'True' if 'env_release_node' is not 'Nothing'.
env_is_sustained :: Envelope a -> Bool
env_is_sustained = isJust . env_release_node

-- | Delay the onset of the envelope.
env_delay :: Envelope a -> a -> Envelope a
env_delay (Envelope l t c rn ln) d =
    let (l0:_) = l
        l' = l0 : l
        t' = d : t
        c' = EnvLin : c
        rn' = fmap (+ 1) rn
        ln' = fmap (+ 1) ln
    in Envelope l' t' c' rn' ln'

-- | Connect releaseNode (or end) to first node of envelope.
env_circle :: (Num a,Fractional a) => Envelope a -> a -> Envelope_Curve a -> Envelope a
env_circle (Envelope l t c rn _) tc cc =
    let z = 1 {- 1 - impulse KR 0 0 -}
        n = length t
    in case rn of
         Nothing -> let l' = 0 : l ++ [0]
                        t' = z * tc : t ++ [9e8]
                        c' = cc : take n (cycle c) ++ [EnvLin]
                        rn' = Just (n + 1)
                    in Envelope l' t' c' rn' (Just 0)
         Just i -> let l' = 0 : l
                       t' = z * tc : t
                       c' = cc : take n (cycle c)
                       rn' = Just (i + 1)
                   in  Envelope l' t' c' rn' (Just 0)

-- * UGen

-- | Segment based envelope generator.
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> Envelope UGen -> UGen
envGen r gate lvl bias scale act e =
    let err = error "envGen: bad Envelope"
        z = fromMaybe err (envelope_sc3_array e)
        i = [gate, lvl, bias, scale, from_done_action act] ++ z
    in mkOsc r "EnvGen" i 1

-- | Envelope generator for polling values from an Env
iEnvGen :: Rate -> UGen -> Envelope UGen -> UGen
iEnvGen r ix e =
    let err = error "iEnvGen: bad Envelope"
        z = fromMaybe err (envelope_sc3_ienvgen_array e)
        i = ix : z
    in mkOscR [AR,KR] r "IEnvGen" i 1

-- | Line generator.
line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line r start end dur act = mkOsc r "Line" [start, end, dur, from_done_action act] 1

-- | Exponential line generator.
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine r start end dur act = mkOsc r "XLine" [start, end, dur, from_done_action act] 1

-- | Free node on trigger.
freeSelf :: UGen -> UGen
freeSelf i = mkFilter "FreeSelf" [i] 1

-- | Free node on done action at source.
freeSelfWhenDone :: UGen -> UGen
freeSelfWhenDone i = mkFilter "FreeSelfWhenDone" [i] 1

-- | Pause specified node on trigger.
pause :: UGen -> UGen -> UGen
pause t n = mkFilter "Pause" [t, n] 1

-- | Pause node on trigger.
pauseSelf :: UGen -> UGen
pauseSelf i = mkFilter "PauseSelf" [i] 1

-- | Pause node on done action at source.
pauseSelfWhenDone :: UGen -> UGen
pauseSelfWhenDone i = mkFilter "PauseSelfWhenDone" [i] 1

-- | One while the source is marked done, else zero.
done :: UGen -> UGen
done i = mkFilter "Done" [i] 1

-- | Raise specified done action when input goes silent.
detectSilence ::  UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence i a t act = mkFilter "DetectSilence" [i, a, t, from_done_action act] 1

-- | When triggered free specified node.
free :: UGen -> UGen -> UGen
free i n = mkFilter "Free" [i, n] 1

-- | Linear envelope generator.
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
linen g at sl rt da = mkFilter "Linen" [g, at, sl, rt, from_done_action da] 1

-- | Singleton fade envelope.
envGate :: UGen -> UGen -> UGen -> DoneAction -> Envelope_Curve UGen -> UGen
envGate level gate fadeTime doneAction curve =
    let startVal = fadeTime <=* 0
        e = Envelope [startVal,1,0] [1,1] [curve] (Just 1) Nothing
    in envGen KR gate level 0 fadeTime doneAction e

-- | Variant with default values for all inputs.  @gate@ and
-- @fadeTime@ are 'control's, @doneAction@ is 'RemoveSynth', @curve@
-- is 'EnvSin'.
envGate' :: UGen
envGate' =
    let level = 1
        gate = meta_control KR "gate" 1 (0,1,"lin",1,"")
        fadeTime = meta_control KR "fadeTime" 0.02 (0,10,"lin",0,"s")
        doneAction = RemoveSynth
        curve = EnvSin
    in envGate level gate fadeTime doneAction curve

-- * List

-- > d_dx [0,1,3,6] == [0,1,2,3]
d_dx :: (Num a) => [a] -> [a]
d_dx l = zipWith (-) l (0:l)

-- > dx_d (d_dx [0,1,3,6]) == [0,1,3,6]
-- > dx_d [0.5,0.5] == [0.5,1]
dx_d :: Num n => [n] -> [n]
dx_d = scanl1 (+)
