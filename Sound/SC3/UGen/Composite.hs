-- | Common unit generator graphs.
module Sound.SC3.UGen.Composite where

import Control.Monad
import Data.List
import Data.List.Split

import Sound.SC3.UGen.Bindings
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Monad
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct
import Sound.SC3.UGen.UId

-- | Generate a localBuf and use setBuf to initialise it.
asLocalBuf :: ID i => i -> [UGen] -> UGen
asLocalBuf i xs =
    let b = localBuf i (fromIntegral (length xs)) 1
        s = setBuf b xs 0
    in mrg2 b s

-- | Calculate coefficients for bi-quad low pass filter.
bLowPassCoef :: Floating a => a -> a -> a -> (a,a,a,a,a)
bLowPassCoef sr freq rq =
    let w0 = pi * 2 * freq * (1 / sr)
        cos_w0 = cos w0
        i = 1 - cos_w0
        alpha = sin w0 * 0.5 * rq
        b0rz = recip (1 + alpha)
        a0 = i * 0.5 * b0rz
        a1 = i * b0rz
        b1 = cos_w0 * 2 * b0rz
        b2 = (1 - alpha) * negate b0rz
    in (a0,a1,a0,b1,b2)

-- | Buffer reader (no interpolation).
bufRdN :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdN n r b p l = bufRd n r b p l NoInterpolation

-- | Buffer reader (linear interpolation).
bufRdL :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdL n r b p l = bufRd n r b p l LinearInterpolation

-- | Buffer reader (cubic interpolation).
bufRdC :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdC n r b p l = bufRd n r b p l CubicInterpolation

-- | Triggers when a value changes
changed :: UGen -> UGen -> UGen
changed input threshold = abs (hpz1 input) >* threshold

-- | 'mce' variant of 'lchoose'.
choose :: ID m => m -> UGen -> UGen
choose e = lchoose e . mceChannels

-- | 'liftUId' of 'choose'.
chooseM :: UId m => UGen -> m UGen
chooseM = liftUId choose

-- | Demand rate (:) function.
dcons :: ID m => (m,m,m) -> UGen -> UGen -> UGen
dcons (z0,z1,z2) x xs =
    let i = dseq z0 1 (mce2 0 1)
        a = dseq z1 1 (mce2 x xs)
    in dswitch z2 i a

-- | Demand rate (:) function.
dconsM :: (UId m) => UGen -> UGen -> m UGen
dconsM x xs = do
  i <- dseqM 1 (mce2 0 1)
  a <- dseqM 1 (mce2 x xs)
  dswitchM i a

-- | Demand rate weighted random sequence generator.
dwrand :: ID i => i -> UGen -> UGen -> UGen -> UGen
dwrand z repeats weights list_ =
    let n = mceDegree list_
        weights' = mceExtend n weights
        inp = repeats : constant n : weights'
    in mkUGen Nothing [DR] (Left DR) "Dwrand" inp (Just list_) 1 (Special 0) (toUId z)

-- | Demand rate weighted random sequence generator.
dwrandM :: (UId m) => UGen -> UGen -> UGen -> m UGen
dwrandM = liftUId3 dwrand

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

-- | Variant FFT constructor with default values for hop size (0.5),
-- window type (0), active status (1) and window size (0).
fft' :: UGen -> UGen -> UGen
fft' buf i = fft buf i 0.5 0 1 0

-- | 'fft' variant that allocates 'localBuf'.
--
-- > let c = ffta 'α' 2048 (soundIn 0) 0.5 0 1 0
-- > in audition (out 0 (ifft c 0 0))
ffta :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
ffta z nf i h wt a ws =
    let b = localBuf z nf 1
    in fft b i h wt a ws

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: UGen -> UGen -> UGen -> UGen
fftTrigger b h p = mkOsc KR "FFTTrigger" [b,h,p] 1

-- | Sum of 'numInputBuses' and 'numOutputBuses'.
firstPrivateBus :: UGen
firstPrivateBus = numInputBuses + numOutputBuses

-- | Frequency shifter, in terms of 'hilbert' (see also 'freqShift').
freqShift_hilbert :: UGen -> UGen -> UGen -> UGen
freqShift_hilbert i f p =
    let o = sinOsc AR f (mce [p + 0.5 * pi, p])
        h = hilbert i
    in mix (h * o)

-- | Variant ifft with default value for window type.
ifft' :: UGen -> UGen
ifft' buf = ifft buf 0 0

{-
-- | Linear interpolating variant on index.
indexL :: UGen -> UGen -> UGen
indexL b i =
    let x = index b i
        y = index b (i + 1)
    in linLin (frac i) 0 1 x y
-}

-- | Format frequency, amplitude and phase data as required for klang.
klangSpec :: [UGen] -> [UGen] -> [UGen] -> UGen
klangSpec f a p = mce ((concat . transpose) [f, a, p])

-- | Variant of 'klangSpec' for non-UGen inputs.
klangSpec' :: Real n => [n] -> [n] -> [n] -> UGen
klangSpec' f a p =
    let u = map constant
    in klangSpec (u f) (u a) (u p)

-- | Variant of 'klangSpec' for 'MCE' inputs.
klangSpec_mce :: UGen -> UGen -> UGen -> UGen
klangSpec_mce f a p =
    let m = mceChannels
    in klangSpec (m f) (m a) (m p)

-- | Format frequency, amplitude and decay time data as required for klank.
klankSpec :: [UGen] -> [UGen] -> [UGen] -> UGen
klankSpec f a dt = mce ((concat . transpose) [f,a,dt])

-- | Variant for non-UGen inputs.
klankSpec' :: Real n => [n] -> [n] -> [n] -> UGen
klankSpec' f a dt =
    let u = map constant
    in klankSpec (u f) (u a) (u dt)

-- | Variant of 'klankSpec' for 'MCE' inputs.
klankSpec_mce :: UGen -> UGen -> UGen -> UGen
klankSpec_mce f a dt =
    let m = mceChannels
    in klankSpec (m f) (m a) (m dt)

-- | Randomly select one of a list of UGens (initialiastion rate).
lchoose :: ID m => m -> [UGen] -> UGen
lchoose e a = select (iRand e 0 (fromIntegral (length a))) (mce a)

-- | 'liftUId' of 'lchoose'.
lchooseM :: UId m => [UGen] -> m UGen
lchooseM = liftUId lchoose

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

-- | Variant with defaults of zero.
localIn' :: Int -> Rate -> UGen
localIn' nc r = localIn nc r (mce (replicate nc 0))

-- | Count 'mce' channels.
mceN :: UGen -> UGen
mceN = constant . length . mceChannels

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
packFFT b sz from to z mp =
    let n = constant (mceDegree mp)
    in mkOscMCE KR "PackFFT" [b, sz, from, to, z, n] mp 1

-- | Format magnitude and phase data data as required for packFFT.
packFFTSpec :: [UGen] -> [UGen] -> UGen
packFFTSpec m p = mce (interleave m p)
    where interleave x = concat . zipWith (\a b -> [a,b]) x

-- | Calculate size of accumulation buffer given FFT and IR sizes.
pc_calcAccumSize :: Int -> Int -> Int
pc_calcAccumSize fft_size ir_length =
    let partition_size = fft_size `div` 2
        num_partitions = (ir_length `div` partition_size) + 1
    in fft_size * num_partitions

-- | Poll value of input UGen when triggered.
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll t i l tr = mkFilter "Poll" ([t,i,tr] ++ unpackLabel l) 0

-- | Apply function /f/ to each bin of an @FFT@ chain, /f/ receives
-- magnitude, phase and index and returns a (magnitude,phase).
pvcollect :: UGen -> UGen -> (UGen -> UGen -> UGen -> (UGen, UGen)) -> UGen -> UGen -> UGen -> UGen
pvcollect c nf f from to z = packFFT c nf from to z mp
  where m = unpackFFT c nf from to 0
        p = unpackFFT c nf from to 1
        i = [from .. to]
        e = zipWith3 f m p i
        mp = uncurry packFFTSpec (unzip e)

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

-- | Send a reply message from the server back to the all registered clients.
sendReply :: UGen -> UGen -> String -> [UGen] -> UGen
sendReply i k n v =
    let n' = map (fromIntegral . fromEnum) n
        s = fromIntegral (length n')
    in mkFilter "SendReply" ([i,k,s] ++ n' ++ v) 0

-- | Set local buffer values.
setBuf :: UGen -> [UGen] -> UGen -> UGen
setBuf b xs o =
    let i = [b, o, fromIntegral (length xs)] ++ xs
    in mkUGen Nothing [IR] (Left IR) "SetBuf" i Nothing 1 (Special 0) NoId

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

-- | Randomly select one of several inputs on trigger.
tChoose :: ID m => m -> UGen -> UGen -> UGen
tChoose z t a = select (tIRand z 0 (mceN a) t) a

-- | Randomly select one of several inputs.
tChooseM :: (UId m) => UGen -> UGen -> m UGen
tChooseM t a = do
  r <- tIRandM 0 (constant (length (mceChannels a))) t
  return (select r a)

-- | Randomly select one of several inputs on trigger (weighted).
tWChoose :: ID m => m -> UGen -> UGen -> UGen -> UGen -> UGen
tWChoose z t a w n =
    let i = tWindex z t n w
    in select i a

-- | Randomly select one of several inputs (weighted).
tWChooseM :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
tWChooseM t a w n = do
  i <- tWindexM t n w
  return (select i a)

-- | Unpack a single value (magnitude or phase) from an FFT chain
unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf size index' which = mkOsc DR "Unpack1FFT" [buf, size, index', which] 1

-- | Unpack an FFT chain into separate demand-rate FFT bin streams.
unpackFFT :: UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]
unpackFFT c nf from to w = map (\i -> unpack1FFT c nf i w) [from .. to]

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
osc1 :: Rate -> UGen -> UGen -> DoneAction -> UGen
osc1 rt buf dur doneAction =
    let ph = line rt 0 (bufFrames IR buf - 1) dur doneAction
    in bufRd 1 rt buf ph NoLoop LinearInterpolation
