-- | Common unit generator graphs.
module Sound.SC3.UGen.Composite where

import Control.Monad
import Data.List
import Data.List.Split

import Sound.SC3.UGen.Buffer
import Sound.SC3.UGen.Demand
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Envelope
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

-- | Triggers when a value changes
changed :: UGen -> UGen -> UGen
changed input threshold = abs (hpz1 input) >* threshold

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

-- | Sum of 'numInputBuses' and 'numOutputBuses'.
firstPrivateBus :: UGen
firstPrivateBus = numInputBuses + numOutputBuses

-- | Frequency shifter, in terms of 'hilbert' (see also 'freqShift').
freqShift_hilbert :: UGen -> UGen -> UGen -> UGen
freqShift_hilbert i f p =
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
    let (m,a) = linLin_muladd sl sr dl dr
    in mulAdd i m a

-- | 'linLin' where source is (0,1).
linLin_u :: UGen -> UGen -> UGen -> UGen
linLin_u i = linLin i 0 1

-- | 'linLin' where source is (-1,1).
linLin_b :: UGen -> UGen -> UGen -> UGen
linLin_b i = linLin i (-1) 1

-- | Optimised sum function.
sum_opt :: [UGen] -> UGen
sum_opt l =
    case l of
      p:q:r:s:l' -> sum_opt (sum4 p q r s : l')
      p:q:r:l' -> sum_opt (sum3 p q r : l')
      _ -> sum l

-- | Collapse possible mce by summing.
mix :: UGen -> UGen
mix = sum_opt . mceChannels

-- | Mix variant, sum to n channels.
mixN :: Int -> UGen -> UGen
mixN n u =
    let xs = transpose (chunksOf n (mceChannels u))
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

-- | Variant of 'poll' that generates an 'mrg' value with the input
-- signal at left, and that allows a constant /frequency/ input in
-- place of a trigger.
poll' :: UGen -> UGen -> UGen -> UGen -> UGen
poll' t i l tr =
    let t' = if isConstant t then impulse KR t 0 else t
    in mrg [i,poll t' i l tr]

-- | Variant of 'in'' offset so zero if the first private bus.
privateIn :: Int -> Rate -> UGen -> UGen
privateIn nc rt k = in' nc rt (k + firstPrivateBus)

-- | Variant of 'out' offset so zero if the first private bus.
privateOut :: UGen -> UGen -> UGen
privateOut k = out (k + firstPrivateBus)

-- | RMS variant of 'runningSum'.
runningSumRMS :: UGen -> UGen -> UGen
runningSumRMS z n = sqrt (runningSum (z * z) n * (recip n))

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
         MCE_U m ->
             let n = mceProxies m
             in if all (==1) (zipWith (-) (tail n) n)
                then in' (length n) AR (numOutputBuses + head n)
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

-- * wslib

playBufCF :: Int -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen -> Int -> UGen
playBufCF nc bufnum rate trigger startPos loop lag' n =
    let trigger' = if rateOf trigger == DR
                   then tDuty AR trigger 0 DoNothing 1 0
                   else trigger
        index' = stepper trigger' 0 0 (constant n - 1) 1 0
        on = map
             (\i -> inRange index' (i - 0.5) (i + 0.5))
             [0 .. constant n - 1]
        rate' = case rateOf rate of
                  DR -> map (\on' -> demand on' 0 rate) on
                  KR -> map (\on' -> gate rate on') on
                  AR -> map (\on' -> gate rate on') on
                  IR -> map (const rate) on
        startPos' = if rateOf startPos == DR
                    then demand trigger' 0 startPos
                    else startPos
        lag'' = 1 / lag'
        s = map
            (\(on',r) -> let p = playBuf nc AR bufnum r on' startPos' loop DoNothing
                         in p * sqrt (slew on' lag'' lag''))
            (zip on rate')
    in sum s

-- * adc

-- | An oscillator that reads through a table once.
osc1 :: UGen -> UGen -> DoneAction -> UGen
osc1 buf dur doneAction =
    let ph = line AR 0 (bufFrames IR buf - 1) dur doneAction
    in bufRd 1 AR buf ph NoLoop LinearInterpolation
