-- | Envelope generators.
module Sound.SC3.UGen.Envelope where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Sound.SC3.UGen.Enum as E {- hsc3 -}
import qualified Sound.SC3.UGen.Type as U {- hsc3 -}

-- * Envelope

-- | Apply /f/ to all /a/ at 'Envelope'.
envelope_coerce :: (a -> b) -> Envelope a -> Envelope b
envelope_coerce f e =
    let Envelope l t c rn ln = e
    in Envelope (map f l) (map f t) (map (E.env_curve_coerce f) c) rn ln

-- | SC3 envelope segment model
data Envelope a =
    Envelope {env_levels :: [a] -- ^ Set of /n/ levels, n is >= 1
             ,env_times :: [a] -- ^ Set of /n-1/ time intervals
             ,env_curves :: [E.Envelope_Curve a] -- ^ Possibly empty curve set
             ,env_release_node :: Maybe Int -- ^ Maybe index to release node
             ,env_loop_node :: Maybe Int -- ^ Maybe index to loop node
             }
    deriving (Eq,Show)

-- | Variant without release and loop node inputs (defaulting to nil).
envelope :: [a] -> [a] -> [E.Envelope_Curve a] -> Envelope a
envelope l t c = Envelope l t c Nothing Nothing

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
type Envelope_Segment t = (t,t,t,t,E.Envelope_Curve t)

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

-- | Extract all segments.
envelope_segments :: Num t => Envelope t -> [Envelope_Segment t]
envelope_segments e =
    let n = envelope_n_segments e
    in map (envelope_segment e) [0 .. n - 1]

-- | Transform list of 'Envelope_Segment's into lists ('env_levels','env_times','env_curves').
pack_envelope_segments :: Num t => [Envelope_Segment t] -> ([t],[t],[E.Envelope_Curve t])
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
envelope_is_normal = not . any (== 0) . env_times

-- | Normalise envelope by deleting segments of zero duration.
envelope_normalise :: (Num a, Ord a) => Envelope a -> Envelope a
envelope_normalise e =
    let s = envelope_segments e
        f (t0,_,t1,_,_) = t1 <= t0
        s' = filter (not . f) s
        (l,t,c) = pack_envelope_segments s'
    in case e of
         Envelope _ _ _ Nothing Nothing -> Envelope l t c Nothing Nothing
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
                    f = E.env_curve_interpolation_f c
                in if d <= 0
                   then x1
                   else f x0 x1 t'
      Nothing -> 0

-- | Render 'Envelope' to breakpoint set of /n/ equi-distant places.
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
envelope_curves :: Num a => Envelope a -> [E.Envelope_Curve a]
envelope_curves e =
    let c = env_curves e
        n = envelope_n_segments e
    in if null c
       then replicate n E.EnvLin
       else take n (cycle c)

-- | Linear SC3 form of 'Envelope' data.
--
-- Form is: l0 #t reset loop l1 t0 c0 c0' ...
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
        f i j k = [i,j,E.env_curve_shape k,E.env_curve_value k]
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
        f i j k = [j,E.env_curve_shape k,E.env_curve_value k,i]
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
        c' = E.EnvLin : c
        rn' = fmap (+ 1) rn
        ln' = fmap (+ 1) ln
    in Envelope l' t' c' rn' ln'

-- | Connect releaseNode (or end) to first node of envelope.
env_circle :: (Num a,Fractional a) => Envelope a -> a -> E.Envelope_Curve a -> Envelope a
env_circle (Envelope l t c rn _) tc cc =
    let z = 1 {- 1 - impulse KR 0 0 -}
        n = length t
    in case rn of
         Nothing -> let l' = 0 : l ++ [0]
                        t' = z * tc : t ++ [9e8]
                        c' = cc : take n (cycle c) ++ [E.EnvLin]
                        rn' = Just (n + 1)
                    in Envelope l' t' c' rn' (Just 0)
         Just i -> let l' = 0 : l
                       t' = z * tc : t
                       c' = cc : take n (cycle c)
                       rn' = Just (i + 1)
                   in  Envelope l' t' c' rn' (Just 0)

-- * UGen

envelope_to_ugen :: Envelope U.UGen -> U.UGen
envelope_to_ugen =
    let err = error "envGen: bad Envelope"
    in U.mce . fromMaybe err . envelope_sc3_array

-- * List

-- > d_dx [0,1,3,6] == [0,1,2,3]
d_dx :: (Num a) => [a] -> [a]
d_dx l = zipWith (-) l (0:l)

-- > dx_d (d_dx [0,1,3,6]) == [0,1,3,6]
-- > dx_d [0.5,0.5] == [0.5,1]
dx_d :: Num n => [n] -> [n]
dx_d = scanl1 (+)

-- > d_dx' [0,1,3,6] == [1,2,3]
d_dx' :: Num n => [n] -> [n]
d_dx' l = zipWith (-) (tail l) l

-- > dx_d' (d_dx' [0,1,3,6]) == [0,1,3,6]
-- > dx_d' [0.5,0.5] == [0,0.5,1]
dx_d' :: Num n => [n] -> [n]
dx_d' = (0 :) . scanl1 (+)
