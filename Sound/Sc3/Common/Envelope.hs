-- | Envelope generators.
module Sound.Sc3.Common.Envelope where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Safe {- safe -}

import qualified Sound.Sc3.Common.Base as Base {- hsc3 -}
import qualified Sound.Sc3.Common.Math.Interpolate as Interpolate {- hsc3 -}

-- * Curve

-- | Envelope curve indicator input.
data Envelope_Curve a =
  EnvStep
  | EnvLin
  | EnvExp
  | EnvSin
  | EnvWelch -- ^ Note: not implemented at Sc3
  | EnvNum a
  | EnvSqr
  | EnvCub
  | EnvHold
  deriving (Eq, Show)

-- | Envelope curve pair.
type Envelope_Curve_2 a = Base.T2 (Envelope_Curve a)

-- | Envelope curve triple.
type Envelope_Curve_3 a = Base.T3 (Envelope_Curve a)

-- | Envelope curve quadruple.
type Envelope_Curve_4 a = Base.T4 (Envelope_Curve a)

{- | Convert 'Envelope_Curve' to shape value.

>>> map env_curve_shape [EnvSin,EnvSqr]
[3,6]
-}
env_curve_shape :: Num a => Envelope_Curve a -> a
env_curve_shape e =
    case e of
      EnvStep -> 0
      EnvLin -> 1
      EnvExp -> 2
      EnvSin -> 3
      EnvWelch -> 4
      EnvNum _ -> 5
      EnvSqr -> 6
      EnvCub -> 7
      EnvHold -> 8

{- | The /value/ of 'EnvCurve' is non-zero for 'EnvNum'.

>>> map env_curve_value [EnvWelch,EnvNum 2]
[0,2]
-}
env_curve_value :: Num a => Envelope_Curve a -> a
env_curve_value e =
    case e of
      EnvNum u -> u
      _ -> 0

-- | 'Interpolation_f' of 'Envelope_Curve'.
env_curve_interpolation_f :: (Ord t, Floating t) => Envelope_Curve t -> Interpolate.Interpolation_f t
env_curve_interpolation_f c =
    case c of
      EnvStep -> Interpolate.step
      EnvLin -> Interpolate.linear
      EnvExp -> Interpolate.exponential
      EnvSin -> Interpolate.sine
      EnvWelch -> Interpolate.welch
      EnvNum n -> Interpolate.curve n
      EnvSqr -> Interpolate.squared
      EnvCub -> Interpolate.cubed
      EnvHold -> undefined

-- | Apply /f/ to 'EnvNum' value.
env_curve_map :: (a -> b) -> Envelope_Curve a -> Envelope_Curve b
env_curve_map f e =
    case e of
      EnvStep -> EnvStep
      EnvLin -> EnvLin
      EnvExp -> EnvExp
      EnvSin -> EnvSin
      EnvWelch -> EnvWelch
      EnvNum x -> EnvNum (f x)
      EnvSqr -> EnvSqr
      EnvCub -> EnvCub
      EnvHold -> EnvHold

-- * Envelope

-- | Sc3 envelope segment model
data Envelope a =
    Envelope
    {env_levels :: [a] -- ^ Set of /n/ levels, n is >= 1
    ,env_times :: [a] -- ^ Set of /n-1/ time intervals
    ,env_curves :: [Envelope_Curve a] -- ^ Possibly empty curve set
    ,env_release_node :: Maybe Int -- ^ Maybe index to release node
    ,env_loop_node :: Maybe Int -- ^ Maybe index to loop node
    ,env_offset :: a -- ^ An offset for all time values (IEnvGen only)
    }
    deriving (Eq,Show)

-- | Apply /f/ to all /a/ at 'Envelope'.
envelope_map :: (a -> b) -> Envelope a -> Envelope b
envelope_map f e =
    let Envelope l t c rn ln os = e
    in Envelope (map f l) (map f t) (map (env_curve_map f) c) rn ln (f os)

-- | fmap = 'envelope_map'
instance Functor Envelope where
  fmap = envelope_map

{- | Variant without release and loop node inputs (defaulting to nil).

> Sound.Sc3.Plot.plotEnvelope [envelope [0,1,0] [3,2] [EnvSin,EnvSin]]
-}
envelope :: Num a => [a] -> [a] -> [Envelope_Curve a] -> Envelope a
envelope l t c = Envelope l t c Nothing Nothing 0

-- | Duration of 'Envelope', ie. 'sum' '.' 'env_times'.
envelope_duration :: Num n => Envelope n -> n
envelope_duration = sum . env_times

-- | Number of segments at 'Envelope', ie. 'length' '.' 'env_times'.
envelope_n_segments :: Integral i => Envelope n -> i
envelope_n_segments = genericLength . env_times

-- | Determine which envelope segment a given time /t/ falls in.
envelope_segment_ix :: (Ord a, Num a) => Envelope a -> a -> Maybe Int
envelope_segment_ix e t =
    let d = Base.dx_d (env_times e)
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
        t0 = (0 : Base.dx_d t) !! i
        t1 = t0 + t !! i
        c = envelope_curves e !! i
    in (t0,x0,t1,x1,c)

-- | Extract all segments.
envelope_segments :: Num t => Envelope t -> [Envelope_Segment t]
envelope_segments e =
    let n = envelope_n_segments e
    in map (envelope_segment e) [0 .. n - 1]

-- | Transform list of 'Envelope_Segment's into lists ('env_levels','env_times','env_curves').
pack_envelope_segments :: Num t => [Envelope_Segment t] -> ([t],[t],[Envelope_Curve t])
pack_envelope_segments s =
    case s of
      [] -> error ""
      [(t0,l0,t1,l1,c)] -> ([l0,l1],[t1 - t0],[c])
      (_,l0,_,_,_) : _ ->
          let t (t0,_,t1,_,_) = t1 - t0
              c (_,_,_,_,x) = x
              l (_,_,_,x,_) = x
          in (l0 : map l s,map t s,map c s)

-- | An envelope is /normal/ if it has no segments with zero duration.
envelope_is_normal :: (Eq n,Num n) => Envelope n -> Bool
envelope_is_normal = notElem 0 . env_times

-- | Normalise envelope by deleting segments of zero duration.
envelope_normalise :: (Num a, Ord a) => Envelope a -> Envelope a
envelope_normalise e =
    let s = envelope_segments e
        f (t0,_,t1,_,_) = t1 <= t0
        s' = filter (not . f) s
        (l,t,c) = pack_envelope_segments s'
    in case e of
         Envelope _ _ _ Nothing Nothing  os -> Envelope l t c Nothing Nothing os
         _ -> error "envelope_normalise: has release or loop node..."

-- | Get value for 'Envelope' at time /t/, or zero if /t/ is out of
-- range.  By convention if the envelope has a segment of zero
-- duration we give the rightmost value.
envelope_at :: (Ord t, Floating t) => Envelope t -> t -> t
envelope_at e t =
    case envelope_segment_ix e t of
      Just n -> let (t0,x0,t1,x1,c) = envelope_segment e n
                    d = t1 - t0
                    t' = (t - t0) / d
                    f = env_curve_interpolation_f c
                in if d <= 0
                   then x1
                   else f x0 x1 t'
      Nothing -> 0

-- | Render 'Envelope' to breakpoint set of /n/ equi-distant places.
envelope_render :: (Ord t, Floating t, Enum t) => Int -> Envelope t -> [(t,t)]
envelope_render n e =
    let d = envelope_duration e
        k = d / (fromIntegral n - 1)
        t = [0,k .. d]
    in zip t (map (envelope_at e) t)

-- | Contruct a lookup table of /n/ places from 'Envelope'.
envelope_table :: (Ord t, Floating t, Enum t) => Int -> Envelope t -> [t]
envelope_table n = map snd . envelope_render n

-- | Variant on 'env_curves' that expands the, possibly empty, user
-- list by cycling (if not empty) or by filling with 'EnvLin'.
envelope_curves :: Envelope a -> [Envelope_Curve a]
envelope_curves e =
    let c = env_curves e
        n = envelope_n_segments e
    in if null c
       then replicate n EnvLin
       else take n (cycle c)

{- | Linear Sc3 form of 'Envelope' data.

Form is: l0 #t reset loop l1 t0 c0 c0' ...

>>> envelope_sc3_array (envelope [0,1] [0.1] [EnvLin])
Just [0.0,1.0,-99.0,-99.0,1.0,0.1,1.0,0.0]

>>> let l = [0,0.6,0.3,1.0,0]
>>> let t = [0.1,0.02,0.4,1.1]
>>> let c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
>>> envelope_sc3_array (Envelope l t c Nothing Nothing 0)
Just [0.0,4.0,-99.0,-99.0,0.6,0.1,1.0,0.0,0.3,2.0e-2,2.0,0.0,1.0,0.4,5.0,-6.0,0.0,1.1,3.0,0.0]
-}
envelope_sc3_array :: Num a => Envelope a -> Maybe [a]
envelope_sc3_array e =
    let Envelope l t _ rn ln _ = e
        n = length t
        n' = fromIntegral n
        rn' = fromIntegral (fromMaybe (-99) rn)
        ln' = fromIntegral (fromMaybe (-99) ln)
        c = envelope_curves e
        f i j k = [i,j,env_curve_shape k,env_curve_value k]
    in case l of
         l0:l' -> Just (l0 : n' : rn' : ln' : concat (zipWith3 f l' t c))
         _ -> Nothing

{- | @IEnvGen@ Sc3 form of 'Envelope' data.

>>> let l = [0,0.6,0.3,1.0,0]
>>> let t = [0.1,0.02,0.4,1.1]
>>> let c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
>>> let e = Envelope l t c Nothing Nothing 0
>>> envelope_sc3_ienvgen_array e
Just [0.0,0.0,4.0,1.62,0.1,1.0,0.0,0.6,2.0e-2,2.0,0.0,0.3,0.4,5.0,-6.0,1.0,1.1,3.0,0.0,0.0]
-}
envelope_sc3_ienvgen_array :: Num a => Envelope a -> Maybe [a]
envelope_sc3_ienvgen_array e =
    let Envelope l t _ _ _ os = e
        n = length t
        n' = fromIntegral n
        c = envelope_curves e
        f i j k = [j,env_curve_shape k,env_curve_value k,i]
    in case l of
         l0:l' -> Just (os : l0 : n' : sum t : concat (zipWith3 f l' t c))
         _ -> Nothing

-- | 'True' if 'env_release_node' is not 'Nothing'.
env_is_sustained :: Envelope a -> Bool
env_is_sustained = isJust . env_release_node

-- | Delay the onset of the envelope (add initial segment).
env_delay :: Envelope a -> a -> Envelope a
env_delay (Envelope l t c rn ln os) d =
    let l0 = Safe.headNote "env_delay" l
        l' = l0 : l
        t' = d : t
        c' = EnvLin : c
        rn' = fmap (+ 1) rn
        ln' = fmap (+ 1) ln
    in Envelope l' t' c' rn' ln' os

-- | Connect releaseNode (or end) to first node of envelope.
-- z is a value that is first zero and thereafter one.
-- tc & cc are time and curve from first to last.
env_circle_z :: Fractional a => a -> a -> Envelope_Curve a -> Envelope a -> Envelope a
env_circle_z z tc cc (Envelope l t c rn _ os) =
    let n = length t
    in case rn of
         Nothing -> let l' = 0 : l ++ [0]
                        t' = z * tc : t ++ [1] -- inf (but drawings are poor)
                        c' = cc : take n (cycle c) ++ [EnvLin]
                        rn' = Just (n + 1)
                    in Envelope l' t' c' rn' (Just 0) os
         Just i -> let l' = 0 : l
                       t' = z * tc : t
                       c' = cc : take n (cycle c)
                       rn' = Just (i + 1)
                   in  Envelope l' t' c' rn' (Just 0) os

-- | env_circle_z with cycle time of zero.
env_circle_0 :: Fractional a => Envelope a -> Envelope a
env_circle_0 = env_circle_z 1 0 EnvLin

-- * Construct

{- | Trapezoidal envelope generator.

Requires (<=) and (>=) functions returning @1@ for true and @0@ for false.

The arguments are: 1. @shape@ determines the sustain time as a
proportion of @dur@, zero is a triangular envelope, one a rectangular
envelope; 2. @skew@ determines the attack\/decay ratio, zero is an
immediate attack and a slow decay, one a slow attack and an immediate
decay; 3. @duration@ in seconds; 4. @amplitude@ as linear gain.

-}
envTrapezoid_f :: Num t => (t -> t -> t,t -> t -> t) -> t -> t -> t -> t -> Envelope t
envTrapezoid_f (lte_f,gte_f) shape skew dur amp =
    let x1 = skew * (1 - shape)
        bp = [(0,lte_f skew 0)
             ,(x1,1)
             ,(shape + x1,1)
             ,(1,gte_f skew 1)]
    in envCoord bp dur amp EnvLin

{- | Coordinate based static envelope generator.  Points are (time,value) pairs.

>>> let e = envCoord [(0,0),(1/4,1),(1,0)] 1 1 EnvLin
>>> envelope_sc3_array e
Just [0.0,2.0,-99.0,-99.0,1.0,0.25,1.0,0.0,0.0,0.75,1.0,0.0]

> import Sound.Sc3.Plot {- hsc3-plot -}
> plotEnvelope [envCoord [(0,0),(1/4,1),(1,0)] 1 1 EnvLin]
-}
envCoord :: Num n => [(n,n)] -> n -> n -> Envelope_Curve n -> Envelope n
envCoord xy dur amp c = envXyc (map (\(x,y) -> (x * dur,y * amp,c)) xy)

{- | Segments given as pairs of (time,level).
The input is sorted by time before processing.

>>> env_levels (envPairs [(0, 1), (3, 1.4), (2.1, 0.5)] EnvSin)
[1.0,0.5,1.4]
-}
envPairs :: (Num n,Ord n) => [(n,n)] -> Envelope_Curve n -> Envelope n
envPairs xy = envCoord (Base.sort_on fst xy) 1 1

-- | Percussive envelope, with attack, release, level and curve inputs.
envPerc_c :: Num a => a -> a -> a -> Envelope_Curve_2 a -> Envelope a
envPerc_c atk rls lvl (c0,c1) =
    let c = [c0,c1]
    in Envelope [0,lvl,0] [atk,rls] c Nothing Nothing 0

-- | Percussive envelope, with attack and release inputs.
envPerc :: Num a => a -> a -> Envelope a
envPerc atk rls =
    let cn = EnvNum (-4)
    in envPerc_c atk rls 1 (cn,cn)

{- | Triangular envelope, with duration and level inputs.

>>> let e = envTriangle 1 0.1
>>> envelope_sc3_array e
Just [0.0,2.0,-99.0,-99.0,0.1,0.5,1.0,0.0,0.0,0.5,1.0,0.0]
-}
envTriangle :: Fractional a => a -> a -> Envelope a
envTriangle dur lvl =
    let c = replicate 2 EnvLin
        d = replicate 2 (dur / 2)
    in Envelope [0,lvl,0] d c Nothing Nothing 0

{- | Sine envelope, with duration and level inputs.

>>> envelope_sc3_array (envSine 0 0.1)
Just [0.0,2.0,-99.0,-99.0,0.1,0.0,3.0,0.0,0.0,0.0,3.0,0.0]
-}
envSine :: Fractional a => a -> a -> Envelope a
envSine dur lvl =
    let c = replicate 2 EnvSin
        d = replicate 2 (dur / 2)
    in Envelope [0,lvl,0] d c Nothing Nothing 0

-- | Parameters for Linen envelopes.
data Linen a =
  Linen
  {linen_attackTime :: a
  ,linen_sustainTime :: a
  ,linen_releaseTime :: a
  ,linen_level :: a
  ,linen_curve :: Envelope_Curve_3 a}

-- | Sc3 defaults for Linen.
linen_def :: Fractional t => Linen t
linen_def = let c = EnvLin in Linen 0.01 1 1 1 (c,c,c)

-- | Record ('Linen') variant of 'envLinen'.
envLinen_r :: Num a => Linen a -> Envelope a
envLinen_r (Linen aT sT rT lv (c0,c1,c2)) =
    let l = [0,lv,lv,0]
        t = [aT,sT,rT]
        c = [c0,c1,c2]
    in Envelope l t c Nothing Nothing 0

-- | Variant of 'envLinen' with user specified 'Envelope_Curve a'.
envLinen_c :: Num a => a -> a -> a -> a -> Envelope_Curve_3 a -> Envelope a
envLinen_c aT sT rT lv c = envLinen_r (Linen aT sT rT lv c)

{- | Linear envelope parameter constructor.

>>> let e = envLinen 0 1 0 1
>>> let s = envelope_segments e
>>> let p = pack_envelope_segments s
>>> p == (env_levels e,env_times e,env_curves e)
True
-}
envLinen :: Num a => a -> a -> a -> a -> Envelope a
envLinen aT sT rT lv =
    let c = (EnvLin,EnvLin,EnvLin)
    in envLinen_c aT sT rT lv c

-- | Parameters for Adsr envelopes.
--   The sustain level is given as a proportion of the peak level.
data Adsr a =
  Adsr
  {adsr_attackTime :: a
  ,adsr_decayTime :: a
  ,adsr_sustainLevel :: a
  ,adsr_releaseTime :: a
  ,adsr_peakLevel :: a
  ,adsr_curve :: Envelope_Curve_3 a
  ,adsr_bias :: a}

-- | Sc3 defaults for Adsr.
adsr_def :: Fractional n => Adsr n
adsr_def = let c = EnvNum (-4) in Adsr 0.01 0.3 0.5 1 1 (c,c,c) 0

-- | Attack, decay, sustain, release envelope parameter constructor.
envAdsr :: Num a => a -> a -> a -> a -> a -> Envelope_Curve a -> a -> Envelope a
envAdsr aT dT sL rT pL c b = envAdsr_r (Adsr aT dT sL rT pL (c,c,c) b)

-- | Variant with defaults for pL, c and b.
envAdsr_def :: Num a => a -> a -> a -> a -> Envelope a
envAdsr_def aT dT sL rT = envAdsr aT dT sL rT 1 (EnvNum (-4)) 0

-- | Record ('Adsr') variant of 'envAdsr'.
envAdsr_r :: Num a => Adsr a -> Envelope a
envAdsr_r (Adsr aT dT sL rT pL (c0,c1,c2) b) =
    let l = map (+ b) [0,pL,pL*sL,0]
        t = [aT,dT,rT]
        c = [c0,c1,c2]
    in Envelope l t c (Just 2) Nothing 0

-- | Parameters for Roland type Adssr envelopes.
data Adssr a =
  Adssr
  {adssr_attackTime :: a
  ,adssr_attackLevel :: a
  ,adssr_decayTime :: a
  ,adssr_decayLevel :: a
  ,adssr_slopeTime :: a
  ,adssr_sustainLevel :: a
  ,adssr_releaseTime :: a
  ,adssr_curve :: Envelope_Curve_4 a
  ,adssr_bias :: a}

-- | Attack, decay, slope, sustain, release envelope parameter constructor.
envAdssr :: Num a => a -> a -> a -> a -> a -> a -> a -> Envelope_Curve a -> a -> Envelope a
envAdssr t1 l1 t2 l2 t3 l3 t4 c b = envAdssr_r (Adssr t1 l1 t2 l2 t3 l3 t4 (c,c,c,c) b)

-- | Record ('Adssr') variant of 'envAdssr'.
envAdssr_r :: Num a => Adssr a -> Envelope a
envAdssr_r (Adssr t1 l1 t2 l2 t3 l3 t4 (c1,c2,c3,c4) b) =
    let l = map (+ b) [0,l1,l2,l3,0]
        t = [t1,t2,t3,t4]
        c = [c1,c2,c3,c4]
    in Envelope l t c (Just 3) Nothing 0

-- | Parameters for Asr envelopes.
data Asr a =
  Asr
  {asr_attackTime :: a
  ,asr_sustainLevel :: a
  ,asr_releaseTime :: a
  ,asr_curve :: Envelope_Curve_2 a}

-- | Sc3 default values for Asr.
asr_def :: Fractional t => Asr t
asr_def = let c = EnvNum (-4) in Asr 0.01 1 1 (c,c)

-- | Sc3 .asr has singular curve argument, hence _c suffix.
envAsr_c :: Num a => a -> a -> a -> Envelope_Curve_2 a -> Envelope a
envAsr_c aT sL rT c = envAsr_r (Asr aT sL rT c)

{- | Attack, sustain, release envelope parameter constructor.

>>> envelope_sc3_array (envAsr 3 0.1 2 EnvSin)
Just [0.0,2.0,1.0,-99.0,0.1,3.0,3.0,0.0,0.0,2.0,3.0,0.0]
-}
envAsr :: Num a => a -> a -> a -> Envelope_Curve a -> Envelope a
envAsr aT sL rT c = envAsr_c aT sL rT (c,c)

-- | Record ('Asr') variant of 'envAsr'.
envAsr_r :: Num a => Asr a -> Envelope a
envAsr_r (Asr aT sL rT (c0,c1)) =
    let l = [0,sL,0]
        t = [aT,rT]
        c' = [c0,c1]
    in Envelope l t c' (Just 1) Nothing 0

-- | All segments are horizontal lines.
envStep :: Num a => [a] -> [a] -> Maybe Int -> Maybe Int -> Envelope a
envStep levels times releaseNode loopNode =
    if length levels /= length times
    then error "envStep: levels and times must have same size"
    else let levels' = Safe.headNote "envStep" levels : levels
         in Envelope levels' times [EnvStep] releaseNode loopNode 0

{- | Segments given as triples of (time,level,curve).  The final curve is ignored.

>>> env_levels (envXyc [(0, 1, EnvSin), (2.1, 0.5, EnvLin), (3, 1.4, EnvLin)])
[1.0,0.5,1.4]
-}
envXyc :: Num n => [(n,n,Envelope_Curve n)] -> Envelope n
envXyc xyc =
  let n = length xyc
      (times,levels,curves) = unzip3 xyc
      offset = times !! 0
  in Envelope levels (Base.d_dx' times) (take (n - 1) curves) Nothing Nothing offset

{- | Variant where the input is sorted by time before processing.

>>> env_levels (envXyc_sort [(0, 1, EnvSin), (3, 1.4, EnvLin), (2.1, 0.5, EnvLin)])
[1.0,0.5,1.4]
-}
envXyc_sort :: (Num n,Ord n) => [(n,n,Envelope_Curve n)] -> Envelope n
envXyc_sort = envXyc . Base.sort_on (\(x,_,_) -> x)
