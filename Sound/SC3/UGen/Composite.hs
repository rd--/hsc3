-- | Common unit generator graphs.
module Sound.SC3.UGen.Composite where

import Control.Monad
import Data.List
import Data.List.Split
import Sound.SC3.UGen.Buffer
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Information
import Sound.SC3.UGen.IO
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Noise.ID
import Sound.SC3.UGen.Oscillator
import Sound.SC3.UGen.Panner
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

-- | Dynamic klang, dynamic sine oscillator bank
dynKlang :: Rate -> UGen -> UGen -> UGen -> UGen
dynKlang r fs fo s =
    let gen (f:a:ph:xs) = sinOsc r (f * fs + fo) ph * a + gen xs
        gen _ = 0
    in gen (mceChannels s)

-- | Dynamic klank, set of non-fixed resonating filters.
dynKlank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dynKlank i fs fo ds s =
    let gen (f:a:d:xs) = ringz i (f * fs + fo) (d * ds) * a + gen xs
        gen _ = 0
    in gen (mceChannels s)

-- | Frequency shifter, in terms of Hilbert UGen.
freqShift :: UGen -> UGen -> UGen -> UGen
freqShift i f p =
    let o = sinOsc AR f (mce [p + 0.5 * pi, p])
        h = hilbert i
    in mix (h * o)

-- | Linear interpolating variant on index.
indexL :: UGen -> UGen -> UGen
indexL b i =
    let x = index b i
        y = index b (i + 1)
    in linLin (frac i) 0 1 x y

-- | Map from one linear range to another linear range.
linLin :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linLin i sl sr dl dr =
    let m = (dr - dl) / (sr - sl)
        a = dl - (m * sl)
    in mulAdd i m a

-- | Collapse possible mce by summing.
mix :: UGen -> UGen
mix = sum . mceChannels

-- | Mix variant, sum to n channels.
mixN :: Int -> UGen -> UGen
mixN n u =
    let xs = transpose (splitEvery n (mceChannels u))
    in mce (map sum xs)

-- | Construct and sum a set of UGens.
mixFill :: Integral n => Int -> (n -> UGen) -> UGen
mixFill n f = mix (mce (map f [0 .. fromIntegral n - 1]))

-- | Monad variant on mixFill.
mixFillM :: (Integral n,Monad m) => Int -> (n -> m UGen) -> m UGen
mixFillM n f = liftM sum (mapM f [0 .. fromIntegral n - 1])

-- | Variant that is randomly pressed.
mouseButton' :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton' rt l r tm =
    let o = lfClipNoise 'z' rt 1
    in lag (linLin o (-1) 1 l r) tm

-- | Randomised mouse UGen (see also 'mouseX'' and 'mouseY'').
mouseR :: ID a => a -> Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseR z rt l r ty tm =
  let f = case ty of
            Linear -> linLin
            Exponential -> linExp
            _ -> undefined
  in lag (f (lfNoise1 z rt 1) (-1) 1 l r) tm

-- | Variant that randomly traverses the mouseX space.
mouseX' :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseX' = mouseR 'x'

-- | Variant that randomly traverses the mouseY space.
mouseY' :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseY' = mouseR 'y'

-- | PM oscillator.
pmOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pmOsc r cf mf pm mp = sinOsc r cf (sinOsc r mf mp * pm)

-- | Scale uni-polar (0,1) input to linear (l,r) range
--
-- > map (urange 3 4) [0,0.5,1] == [3,3.5,4]
urange :: Fractional c => c -> c -> c -> c
urange l r =
    let m = r - l
    in (+ l) . (* m)

-- | Scale bi-polar (-1,1) input to linear (l,r) range
--
-- > map (range 3 4) [-1,0,1] == [3,3.5,4]
range :: Fractional c => c -> c -> c -> c
range l r =
    let m = (r - l) * 0.5
        a = m + l
    in (+ a) . (* m)

-- | Mix one output from many sources
selectX :: UGen -> UGen -> UGen
selectX ix xs =
    let s0 = select (roundTo ix 2) xs
        s1 = select (trunc ix 2 + 1) xs
    in xFade2 s0 s1 (fold2 (ix * 2 - 1) 1) 1

-- | Silence.
silent :: Int -> UGen
silent n = let s = dc AR 0 in mce (replicate n s)

-- | Zero indexed audio input buses.
soundIn :: UGen -> UGen
soundIn u =
    let r = in' 1 AR (numOutputBuses + u)
    in case u of
         MCE_U (MCE ns) ->
             if all (==1) (zipWith (-) (tail ns) ns)
             then in' (length ns) AR (numOutputBuses + head ns)
             else r
         _ -> r

-- | Pan a set of channels across the stereo field.
splay :: UGen -> UGen -> UGen -> UGen -> Bool -> UGen
splay i s l c lc =
    let n = fromIntegral (mceDegree i)
        m = n - 1
        p = map ( (+ (-1.0)) . (* (2 / m)) ) [0 .. m]
        a = if lc then sqrt (1 / n) else 1
    in mix (pan2 i (mce p * s + c) 1) * l * a
