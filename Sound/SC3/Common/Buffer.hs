-- | Signals & wavetables
module Sound.SC3.Common.Buffer where

import Data.List {- base -}

import qualified Sound.SC3.Common.Math as S {- hsc3 -}

-- | /z/ ranges from 0 (for /i/) to 1 (for /j/).
--
-- > > 1.5.blend(2.0,0.50) == 1.75
-- > > 1.5.blend(2.0,0.75) == 1.875
--
-- > blend 0.50 1.5 2 == 1.75
-- > blend 0.75 1.5 2 == 1.875
blend :: Num a => a -> a -> a -> a
blend z i j = i + (z * (j - i))

-- | Variant of '(!!)' but values for index greater than the size of
-- the collection will be clipped to the last index.
clipAt :: Int -> [a] -> a
clipAt ix c =
    if ix > length c - 1
    then last c
    else c !! ix

-- | 'abs' of '(-)'.
absdif :: Num a => a -> a -> a
absdif i j = abs (j - i)

-- | 'blendAt' with @clip@ function as argument.
blendAtBy :: (Integral i,RealFrac n) => (i -> t -> n) -> n -> t -> n
blendAtBy f ix c =
    let m = floor ix
        m' = fromIntegral m
    in blend (absdif ix m') (f m c) (f (m + 1) c)

-- | @SequenceableCollection.blendAt@ returns a linearly interpolated
-- value between the two closest indices.  Inverse operation is
-- 'indexInBetween'.
--
-- > > [2,5,6].blendAt(0.4) == 3.2
--
-- > blendAt 0 [2,5,6] == 2
-- > blendAt 0.4 [2,5,6] == 3.2
blendAt :: RealFrac a => a -> [a] -> a
blendAt = blendAtBy clipAt

-- | Resampling function, /n/ is destination length, /r/ is source
-- length, /f/ is the indexing function, /c/ is the collection.
resamp1_gen :: (Integral i,RealFrac n) => i -> i -> (i -> t -> n) -> t -> i -> n
resamp1_gen n r f c =
    let n' = fromIntegral n
        fwd = (fromIntegral r - 1) / (n' - 1)
        gen i = blendAtBy f (fromIntegral i * fwd) c
    in gen

-- | @SequenceableCollection.resamp1@ returns a new collection of the
-- desired length, with values resampled evenly-spaced from the
-- receiver with linear interpolation.
--
-- > > [1].resamp1(3) == [1,1,1]
-- > > [1,2,3,4].resamp1(12)
-- > > [1,2,3,4].resamp1(3) == [1,2.5,4]
--
-- > resamp1 3 [1] == [1,1,1]
-- > resamp1 12 [1,2,3,4]
-- > resamp1 3 [1,2,3,4] == [1,2.5,4]
resamp1 :: (Enum n,RealFrac n) => Int -> [n] -> [n]
resamp1 n c =
    let gen = resamp1_gen n (length c) clipAt c
    in map gen [0 .. n - 1]

-- | @ArrayedCollection.normalizeSum@ ensures sum of elements is one.
--
-- > > [1,2,3].normalizeSum == [1/6,1/3,0.5]
-- > normalizeSum [1,2,3] == [1/6,2/6,3/6]
normalizeSum :: (Fractional a) => [a] -> [a]
normalizeSum l =
    let n = sum l
    in map (/ n) l

-- | Variant that specifies range of input sequence separately.
normalise_rng :: Fractional n => (n,n) -> (n,n) -> [n] -> [n]
normalise_rng (il,ir) (l,r) = map (\e -> S.linlin e il ir l r)

-- | @ArrayedCollection.normalize@ returns a new Array with the receiver
-- items normalized between min and max.
--
-- > > [1,2,3].normalize == [0,0.5,1]
-- > > [1,2,3].normalize(-20,10) == [-20,-5,10]
--
-- > normalize 0 1 [1,2,3] == [0,0.5,1]
-- > normalize (-20) 10 [1,2,3] == [-20,-5,10]
normalize :: (Fractional n, Ord n) => n -> n -> [n] -> [n]
normalize l r c = normalise_rng (minimum c,maximum c) (l,r) c

-- | List of 2-tuples of elements at distance (stride) /n/.
--
-- > t2_window 3 [1..9] == [(1,2),(4,5),(7,8)]
t2_window :: Integral i => i -> [t] -> [(t,t)]
t2_window n x =
    case x of
      i:j:_ -> (i,j) : t2_window n (genericDrop n x)
      _ -> []

-- | List of 2-tuples of adjacent elements.
--
-- > t2_adjacent [1..6] == [(1,2),(3,4),(5,6)]
-- > t2_adjacent [1..5] == [(1,2),(3,4)]
t2_adjacent :: [t] -> [(t,t)]
t2_adjacent = t2_window (2::Int)

-- | List of 2-tuples of overlapping elements.
--
-- > t2_overlap [1..4] == [(1,2),(2,3),(3,4)]
t2_overlap :: [b] -> [(b,b)]
t2_overlap x = zip x (tail x)

-- | Concat of 2-tuples.
--
-- > t2_concat (t2_adjacent [1..6]) == [1..6]
-- > t2_concat (t2_overlap [1..4]) == [1,2,2,3,3,4]
t2_concat :: [(a,a)] -> [a]
t2_concat x =
    case x of
      [] -> []
      (i,j):x' -> i : j : t2_concat x'

-- | A Signal is half the size of a Wavetable, each element is the sum
-- of two adjacent elements of the Wavetable.
--
-- > from_wavetable [-0.5,0.5,0,0.5,1.5,-0.5,1,-0.5] == [0.0,0.5,1.0,0.5]
-- > let s = [0,0.5,1,0.5] in from_wavetable (to_wavetable s) == s
from_wavetable :: Num n => [n] -> [n]
from_wavetable = map (uncurry (+)) . t2_adjacent

-- | A Wavetable is has /n * 2 + 2/ elements, where /n/ is the number
-- of elements of the Signal.  Each signal element /e0/ expands to the
-- two elements /(2 * e0 - e1, e1 - e0)/ where /e1/ is the next
-- element, or zero at the final element.  Properly wavetables are
-- only of power of two element signals.
--
-- > > Signal[0,0.5,1,0.5].asWavetable == Wavetable[-0.5,0.5,0,0.5,1.5,-0.5,1,-0.5]
--
-- > to_wavetable [0,0.5,1,0.5] == [-0.5,0.5,0,0.5,1.5,-0.5,1,-0.5]
to_wavetable :: Num a => [a] -> [a]
to_wavetable =
    let f (e0,e1) = (2 * e0 - e1,e1 - e0)
    in t2_concat . map f . t2_overlap . (++ [0])

-- | Variant of 'sineFill' that gives each component table.
--
-- > let t = sineGen 1024 (map recip [1,2,3,5,8,13,21,34,55]) (replicate 9 0)
-- > map length t == replicate 9 1024
--
-- > import Sound.SC3.Plot
-- > plotTable t
sineGen :: (Floating n,Enum n) => Int -> [n] -> [n] -> [[n]]
sineGen n =
    let incr = (2 * pi) / fromIntegral n
        ph partial = take n [0,incr * partial ..]
        f h amp iph = map (\z -> sin (z + iph) * amp) (ph h)
    in zipWith3 f [1..]

-- | @Signal.*sineFill@ is a table generator.  Frequencies are
-- partials, amplitudes and initial phases are as given.  Result is
-- normalised.
--
-- > let t = let a = [[21,5,34,3,2,13,1,8,55]
-- >                 ,[13,8,55,34,5,21,3,1,2]
-- >                 ,[55,34,1,3,2,13,5,8,21]]
-- >         in map (\amp -> sineFill 1024 (map recip amp) (replicate 9 0)) a
--
-- > import Sound.SC3.Plot
-- > plotTable t
sineFill :: (Ord n,Floating n,Enum n) => Int -> [n] -> [n] -> [n]
sineFill n amp iph =
    let t = sineGen n amp iph
    in normalize (-1) 1 (map sum (transpose t))

