-- | Common unit generator graphs.
module Sound.Sc3.Ugen.Bindings.Composite where

import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Sc3.Common.Enum
import Sound.Sc3.Common.Envelope
import Sound.Sc3.Common.Math
import Sound.Sc3.Common.Math.Filter.Beq
import Sound.Sc3.Common.Math.Operator
import Sound.Sc3.Common.Rate
import Sound.Sc3.Common.Uid
import Sound.Sc3.Common.Unsafe

import Sound.Sc3.Ugen.Bindings.Db
import Sound.Sc3.Ugen.Bindings.Hw
import Sound.Sc3.Ugen.Math
import Sound.Sc3.Ugen.Mce
import Sound.Sc3.Ugen.Types
import Sound.Sc3.Ugen.Util

-- | Generate a localBuf and use setBuf to initialise it.
asLocalBufId :: ID i => i -> [Ugen] -> Ugen
asLocalBufId z xs =
    let b = localBufId z 1 (fromIntegral (length xs))
        s = setBuf' b xs 0
    in mrg2 b s

asLocalBufM :: Uid m => [Ugen] -> m Ugen
asLocalBufM = liftUid1 asLocalBufId

asLocalBuf :: [Ugen] -> Ugen
asLocalBuf = liftUnsafe1 asLocalBufM

-- | balance2 with Mce input.
balanceStereo :: Ugen -> Ugen -> Ugen -> Ugen
balanceStereo sig pos level = let (x,y) = unmce2 sig in balance2 x y pos level

-- | 24db/oct rolloff - 4th order resonant Low Pass Filter
bLowPass4 :: Ugen -> Ugen -> Ugen -> Ugen
bLowPass4 i f rq =
  let (a0, a1, a2, b1, b2) = bLowPassCoef sampleRate f rq
      flt z = sos z a0 a1 a2 b1 b2
  in flt (flt i)

-- | 24db/oct rolloff - 4th order resonant Hi Pass Filter
bHiPass4 :: Ugen -> Ugen -> Ugen -> Ugen
bHiPass4 i f rq =
  let (a0, a1, a2, b1, b2) = bHiPassCoef sampleRate f rq
      flt z = sos z a0 a1 a2 b1 b2
  in flt (flt i)

-- | Buffer reader (no interpolation).
bufRdN :: Int -> Rate -> Ugen -> Ugen -> Loop Ugen -> Ugen
bufRdN n r b p l = bufRd n r b p l NoInterpolation

-- | Buffer reader (linear interpolation).
bufRdL :: Int -> Rate -> Ugen -> Ugen -> Loop Ugen -> Ugen
bufRdL n r b p l = bufRd n r b p l LinearInterpolation

-- | Buffer reader (cubic interpolation).
bufRdC :: Int -> Rate -> Ugen -> Ugen -> Loop Ugen -> Ugen
bufRdC n r b p l = bufRd n r b p l CubicInterpolation

-- | Triggers when a value changes
changed :: Ugen -> Ugen -> Ugen
changed input threshold = abs (hpz1 input) `greater_than` threshold

-- | 'mce' variant of 'lchoose'.
chooseId :: ID m => m -> Ugen -> Ugen
chooseId z = lchooseId z . mceChannels

-- | 'liftUid' of 'choose'.
chooseM :: Uid m => Ugen -> m Ugen
chooseM = liftUid1 chooseId

choose :: Ugen -> Ugen
choose = liftUnsafe1 chooseM

-- | 'clearBuf' of 'localBuf'.
clearLocalBufId :: ID a => a -> Ugen -> Ugen -> Ugen
clearLocalBufId z nc nf = clearBuf (localBufId z nc nf)

clearLocalBufM :: Uid m => Ugen -> Ugen -> m Ugen
clearLocalBufM = liftUid2 clearLocalBufId

clearLocalBuf :: Ugen -> Ugen -> Ugen
clearLocalBuf = liftUnsafe2 clearLocalBufM

-- | Demand rate (:) function.
dconsId :: ID m => (m,m,m) -> Ugen -> Ugen -> Ugen
dconsId (z0,z1,z2) x xs =
    let i = dseqId z0 1 (mce2 0 1)
        a = dseqId z1 1 (mce2 x xs)
    in dswitchId z2 i a

-- | Demand rate (:) function.
dconsM :: (Uid m) => Ugen -> Ugen -> m Ugen
dconsM x xs = do
  i <- dseqM 1 (mce2 0 1)
  a <- dseqM 1 (mce2 x xs)
  dswitchM i a

dcons :: Ugen -> Ugen -> Ugen
dcons = liftUnsafe2 dconsM

-- | Dynamic klang, dynamic sine oscillator bank
dynKlang :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
dynKlang r fs fo s =
    let gen (f:a:ph:xs) = sinOsc r (f * fs + fo) ph * a + gen xs
        gen _ = 0
    in gen (mceChannels s)

-- | Dynamic klank, set of non-fixed resonating filters.
dynKlank :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dynKlank i fs fo ds s =
    let gen (f:a:d:xs) = ringz i (f * fs + fo) (d * ds) * a + gen xs
        gen _ = 0
    in gen (mceChannels s)

-- | 'linExp' with input range of (-1,1).
exprange :: Ugen -> Ugen -> Ugen -> Ugen
exprange l r s = linExp s (-1) 1 l r

-- | Variant of `exprange` with arguments to make writing post-fix nicer.
in_exprange :: Ugen -> (Ugen, Ugen) -> Ugen
in_exprange s (l,r) = exprange l r s

-- | Variant FFT constructor with default values for hop size (0.5),
-- window type (0), active status (1) and window size (0).
fft' :: Ugen -> Ugen -> Ugen
fft' buf i = fft buf i 0.5 0 1 0

-- | 'fft' variant that allocates 'localBuf'.
--
-- > let c = ffta 'α' 2048 (soundIn 0) 0.5 0 1 0
-- > in audition (out 0 (ifft c 0 0))
fftAllocId :: ID i => i -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fftAllocId z nf i h wt a ws =
    let b = localBufId z 1 nf
    in fft b i h wt a ws

fftAllocM :: Uid m => Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
fftAllocM = liftUid6 fftAllocId

fftAlloc :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
fftAlloc = liftUnsafe6 fftAllocM

-- | Sum of 'numInputBuses' and 'numOutputBuses'.
firstPrivateBus :: Ugen
firstPrivateBus = numInputBuses + numOutputBuses

-- | Frequency shifter, in terms of 'hilbert' (see also 'freqShift').
freqShift_hilbert :: Ugen -> Ugen -> Ugen -> Ugen
freqShift_hilbert i f p =
    let o = sinOsc ar f (mce [p + 0.5 * pi, p])
        h = hilbert i
    in mix (h * o)

{- | Ugen function to re-trigger an EnvGen envelope.
     Inputs are /gate/ (as set at EnvGen) and /reset/.
     The four state logic is: (1,0)->1 (1,1)->-1 (0,1)->0 (0,0)->0.
     If the gate input to EnvGen.kr is -1 the envelope ramps to zero in one control period.
     The reset input sequence 0,1,0 when the gate is open produces (1,-1,1), which resets the envelope.

> map (uncurry gateReset) [(1,0),(1,1),(0,1),(0,0)] == [1,-1,0,0]
-}
gateReset :: Num a => a -> a -> a
gateReset gt tr = gt - (gt * tr * 2)

-- | Variant of 'hilbert' using FFT (with a delay) for better results.
-- Buffer should be 2048 or 1024.
-- 2048 = better results, more delay.
-- 1024 = less delay, little choppier results.
hilbertFIR :: Ugen -> Ugen -> Ugen
hilbertFIR s b =
  let c0 = fft' b s
      c1 = pv_PhaseShift90 c0
      delay = bufDur kr b
  in mce2 (delayN s delay delay) (ifft' c1)

-- | Variant ifft with default value for window type.
ifft' :: Ugen -> Ugen
ifft' buf = ifft buf 0 0

{-
-- | Linear interpolating variant on index.
indexL :: Ugen -> Ugen -> Ugen
indexL b i =
    let x = index b i
        y = index b (i + 1)
    in linLin (frac i) 0 1 x y
-}

-- | Generalised Klan(k/g) specification rule.  /f/ unwraps inputs, /g/ wraps output.
--
-- > let r = [220,0.2,0,219,0.1,1,221,0.1,2]
-- > in klanx_spec_f id id [220,219,221] [0.2,0.1,0.1] [0,1,2] == r
klanx_spec_f :: (a -> [b]) -> ([b] -> c) -> a -> a -> a -> c
klanx_spec_f f g fr am z = g ((concat . transpose) [f fr,f am,f z])

-- | Format frequency, amplitude and decay time data as required for klank.
klangSpec :: [Ugen] -> [Ugen] -> [Ugen] -> Ugen
klangSpec = klanx_spec_f id mce

-- | Variant of 'klangSpec' for non-Ugen inputs.
klangSpec_k :: Real n => [n] -> [n] -> [n] -> Ugen
klangSpec_k = klanx_spec_f (map constant) mce

-- | Variant of 'klangSpec' for 'Mce' inputs.
klangSpec_mce :: Ugen -> Ugen -> Ugen -> Ugen
klangSpec_mce = klanx_spec_f mceChannels mce

-- | Format frequency, amplitude and decay time data as required for klank.
klankSpec :: [Ugen] -> [Ugen] -> [Ugen] -> Ugen
klankSpec = klanx_spec_f id mce

-- | Variant for non-Ugen inputs.
klankSpec_k :: Real n => [n] -> [n] -> [n] -> Ugen
klankSpec_k = klanx_spec_f (map constant) mce

-- | Variant of 'klankSpec' for 'Mce' inputs.
klankSpec_mce :: Ugen -> Ugen -> Ugen -> Ugen
klankSpec_mce = klanx_spec_f mceChannels mce

-- | Randomly select one of a list of Ugens (initialisation rate).
lchooseId :: ID m => m -> [Ugen] -> Ugen
lchooseId z a = select (iRandId z 0 (fromIntegral (length a))) (mce a)

-- | 'liftUid' of 'lchoose'.
lchooseM :: Uid m => [Ugen] -> m Ugen
lchooseM = liftUid1 lchooseId

lchoose :: [Ugen] -> Ugen
lchoose = liftUnsafe1 lchooseM

-- | 'linExp' of (-1,1).
linExp_b :: Ugen -> Ugen -> Ugen -> Ugen
linExp_b i = linExp i (-1) 1

-- | 'linExp' of (0,1).
linExp_u :: Ugen -> Ugen -> Ugen -> Ugen
linExp_u i = linExp i 0 1

-- | Map from one linear range to another linear range.
linLin :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
linLin = linlin_ma mulAdd

-- | 'linLin' where source is (0,1).
linLin_u :: Ugen -> Ugen -> Ugen -> Ugen
linLin_u i = linLin i 0 1

-- | 'linLin' where source is (-1,1).
linLin_b :: Ugen -> Ugen -> Ugen -> Ugen
linLin_b i = linLin i (-1) 1

-- | Variant with defaults of zero.
localIn' :: Int -> Rate -> Ugen
localIn' nc r = localIn nc r (mce (replicate nc 0))

-- | Generate an 'envGen' Ugen with @fadeTime@ and @gate@ controls.
--
-- > import Sound.Sc3
-- > audition (out 0 (makeFadeEnv 1 * sinOsc ar 440 0 * 0.1))
-- > withSc3 (send (n_set1 (-1) "gate" 0))
makeFadeEnv :: Double -> Ugen
makeFadeEnv fadeTime =
    let dt = control kr "fadeTime" (realToFrac fadeTime)
        gate_ = control kr "gate" 1
        startVal = dt `less_than_or_equal_to` 0
        env = Envelope [startVal,1,0] [1,1] [EnvLin,EnvLin] (Just 1) Nothing 0
    in envGen kr gate_ 1 0 dt RemoveSynth env

-- | Variant that is randomly pressed.
mouseButtonRand :: Rate -> Ugen -> Ugen -> Ugen -> Ugen
mouseButtonRand rt l r tm =
    let o = lfClipNoiseId 'z' rt 1
    in lag (linLin o (-1) 1 l r) tm

-- | Randomised mouse Ugen (see also 'mouseX'' and 'mouseY'').
mouseRandId :: ID a => a -> Rate -> Ugen -> Ugen -> Warp Ugen -> Ugen -> Ugen
mouseRandId z rt l r ty tm =
  let f = case ty of
            Linear -> linLin
            Exponential -> linExp
            _ -> undefined
  in lag (f (lfNoise1Id z rt 1) (-1) 1 l r) tm

mouseRandM :: Uid m => Rate -> Ugen -> Ugen -> Warp Ugen -> Ugen -> m Ugen
mouseRandM = liftUid5 mouseRandId

mouseRand :: Rate -> Ugen -> Ugen -> Warp Ugen -> Ugen -> Ugen
mouseRand = liftUnsafe5 mouseRandM

-- | Variant that randomly traverses the mouseX space.
mouseXRand :: Rate -> Ugen -> Ugen -> Warp Ugen -> Ugen -> Ugen
mouseXRand = mouseRandId 'x'

-- | Variant that randomly traverses the mouseY space.
mouseYRand :: Rate -> Ugen -> Ugen -> Warp Ugen -> Ugen -> Ugen
mouseYRand = mouseRandId 'y'

-- | Translate onset type string to constant Ugen value.
onsetType :: Num a => String -> a
onsetType s =
    let t = ["power", "magsum", "complex", "rcomplex", "phase", "wphase", "mkl"]
    in fromIntegral (fromMaybe 3 (elemIndex s t))

-- | Onset detector with default values for minor parameters.
onsetsDefault :: Ugen -> Ugen -> Ugen -> Ugen
onsetsDefault c t o = onsets c t o 1 0.1 10 11 1 0

-- | Format magnitude and phase data data as required for packFFT.
packFFTSpec :: [Ugen] -> [Ugen] -> Ugen
packFFTSpec m p =
    let interleave x = concat . zipWith (\a b -> [a,b]) x
    in mce (interleave m p)

-- | Calculate size of accumulation buffer given FFT and IR sizes.
partConv_calcAccumSize :: Int -> Int -> Int
partConv_calcAccumSize fft_size ir_length =
    let partition_size = fft_size `div` 2
        num_partitions = (ir_length `div` partition_size) + 1
    in fft_size * num_partitions

-- | PM oscillator.
-- cf = carrier frequency, mf = modulation frequency, pm = pm-index = 0.0, mp = mod-phase = 0.0
pmOsc :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pmOsc r cf mf pm mp = sinOsc r cf (sinOsc r mf mp * pm)

-- | Variant of 'poll' that generates an 'mrg' value with the input
-- signal at left, and that allows a constant /frequency/ input in
-- place of a trigger.
pollExt :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pollExt optTrig in_ label_ trigId =
    let tr = if isConstant optTrig then impulse kr optTrig 0 else optTrig
    in mrg [in_,poll tr in_ label_ trigId]

-- | Variant of 'in'' offset so zero if the first private bus.
privateIn :: Int -> Rate -> Ugen -> Ugen
privateIn nc rt k = in' nc rt (k + firstPrivateBus)

-- | Variant of 'out' offset so zero if the first private bus.
privateOut :: Ugen -> Ugen -> Ugen
privateOut k = out (k + firstPrivateBus)

-- | Apply function /f/ to each bin of an @FFT@ chain, /f/ receives
-- magnitude, phase and index and returns a (magnitude,phase).
pvcollect :: Ugen -> Int -> (Ugen -> Ugen -> Int -> (Ugen, Ugen)) -> Int -> Int -> Ugen -> Ugen
pvcollect c nf f from to z =
    let m = unpackFFT c nf from to 0
        p = unpackFFT c nf from to 1
        i = [from .. to]
        e = zipWith3 f m p i
        mp = uncurry packFFTSpec (unzip e)
    in packFFT c nf from to z mp

-- | /dur/ and /hop/ are in seconds, /frameSize/ and /sampleRate/ in
-- frames, though the latter maybe fractional.
--
-- > pv_calcPVRecSize 4.2832879818594 1024 0.25 48000.0 == 823299
pv_calcPVRecSize :: Double -> Int -> Double -> Double -> Int
pv_calcPVRecSize dur frame_size hop sample_rate =
    let frame_size' = fromIntegral frame_size
        raw_size = ceiling ((dur * sample_rate) / frame_size') * frame_size
    in ceiling (fromIntegral raw_size * recip hop + 3)

-- | 'rand' with left edge set to zero.
rand0Id :: ID a => a -> Ugen -> Ugen
rand0Id z = randId z 0

-- | 'Uid' form of 'rand0'.
rand0M :: Uid m => Ugen -> m Ugen
rand0M = randM 0

rand0 :: Ugen -> Ugen
rand0 = liftUnsafe1 rand0M

{- | 'rand' with left edge set to negative /n/.
     Note rand2 is also a UnaryOp Ugen, however hsc3 does not store Ids for operators.
-}
rand2Id :: ID a => a -> Ugen -> Ugen
rand2Id z n = randId z (negate n) n

-- | 'Uid' form of 'rand2'.
rand2M :: Uid m => Ugen -> m Ugen
rand2M n = randM (negate n) n

rand2 :: Ugen -> Ugen
rand2 = liftUnsafe1 rand2M

-- | rotate2 with Mce input.
rotateStereo :: Ugen -> Ugen -> Ugen
rotateStereo sig pos = let (x,y) = unmce2 sig in rotate2 x y pos

-- | RMS variant of 'runningSum'.
runningSumRMS :: Ugen -> Ugen -> Ugen
runningSumRMS z n = sqrt (runningSum (z * z) n * recip n)

-- | Mix one output from many sources
selectX :: Ugen -> Ugen -> Ugen
selectX ix xs =
    let s0 = select (roundTo ix 2) xs
        s1 = select (trunc ix 2 + 1) xs
    in xFade2 s0 s1 (fold2 (ix * 2 - 1) 1) 1

-- | Set local buffer values.
setBuf' :: Ugen -> [Ugen] -> Ugen -> Ugen
setBuf' b xs o = Sound.Sc3.Ugen.Bindings.Db.setBuf b o (fromIntegral (length xs)) (mce xs)

-- | Silence.
silent :: Int -> Ugen
silent n = let s = dc ar 0 in mce (replicate n s)

{- | Zero indexed audio input buses.
     Optimises case of consecutive Ugens.

> soundIn (mce2 0 1) == in' 2 ar numOutputBuses
> soundIn (mce2 0 2) == in' 1 ar (numOutputBuses + mce2 0 2)

-}
soundIn :: Ugen -> Ugen
soundIn u =
    let r = in' 1 ar (numOutputBuses + u)
    in case u of
         Mce_U m ->
             let n = mceProxies m
             in if all (==1) (zipWith (-) (tail n) n)
                then in' (length n) ar (numOutputBuses + head n)
                else r
         _ -> r

-- | Pan a set of channels across the stereo field.
--
-- > input, spread:1, level:1, center:0, levelComp:true
splay :: Ugen -> Ugen -> Ugen -> Ugen -> Bool -> Ugen
splay i s l c lc =
    let n = max 2 (fromIntegral (fromMaybe 1 (mceDegree i)))
        m = n - 1
        p = map ((+ (-1.0)) . (* (2 / m))) [0 .. m]
        a = if lc then sqrt (1 / n) else 1
    in mix (pan2 i (s * mce p + c) 1) * l * a

-- | Single tap into a delayline.  ar only.
tap :: Int -> Rate -> Ugen -> Ugen -> Ugen
tap numChannels rt bufnum delaytime =
    let n = delaytime * negate sampleRate
    in playBuf numChannels rt bufnum 1 0 n Loop DoNothing

-- | Randomly select one of several inputs on trigger.
tChooseId :: ID m => m -> Ugen -> Ugen -> Ugen
tChooseId z t a = select (tiRandId z 0 (mceSize a - 1) t) a

-- | Randomly select one of several inputs.
tChooseM :: (Uid m) => Ugen -> Ugen -> m Ugen
tChooseM t a = do
  r <- tiRandM 0 (constant (length (mceChannels a) - 1)) t
  return (select r a)

tChoose :: Ugen -> Ugen -> Ugen
tChoose = liftUnsafe2 tChooseM

-- | Triggered Line, implemented in terms of EnvGen.
tLine :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tLine rt start end dur trig_ =
  let p = envCoord [(0,0),(0,start),(dur,end)] 1 1 EnvLin
  in envGen rt trig_ 1 0 1 DoNothing p

-- | Triggered xLine, implemented in terms of EnvGen.
tXLine :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tXLine rt start end dur trig_ =
  let p = envCoord [(0,0),(0,start),(dur,end)] 1 1 EnvExp
  in envGen rt trig_ 1 0 1 DoNothing p

-- | Triangle wave as sum of /n/ sines.
-- For partial n, amplitude is (1 / square n) and phase is pi at every other odd partial.
triAS :: Int -> Ugen -> Ugen
triAS n f0 =
    let mk_freq i = f0 * fromIntegral i
        mk_amp i = if even i then 0 else 1 / fromIntegral (i * i)
        mk_ph i = if i + 1 `mod` 4 == 0 then pi else 0
        m = [1,3 .. n]
        param = zip3 (map mk_freq m) (map mk_ph m) (map mk_amp m)
    in sum_opt (map (\(fr,ph,am) -> sinOsc ar fr ph * am) param)

-- | Randomly select one of several inputs on trigger (weighted).
tWChooseId :: ID m => m -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tWChooseId z t a w n =
    let i = tWindexId z t n w
    in select i a

-- | Randomly select one of several inputs (weighted).
tWChooseM :: (Uid m) => Ugen -> Ugen -> Ugen -> Ugen -> m Ugen
tWChooseM t a w n = do
  i <- tWindexM t n w
  return (select i a)

tWChoose :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
tWChoose = liftUnsafe4 tWChooseM

-- | Unpack an FFT chain into separate demand-rate FFT bin streams.
unpackFFT :: Ugen -> Int -> Int -> Int -> Ugen -> [Ugen]
unpackFFT c nf from to w = map (\i -> unpack1FFT c (constant nf) (constant i) w) [from .. to]

-- | VarLag in terms of envGen.  Note: in SC3 curvature and warp are separate arguments.
varLag_env :: Ugen -> Ugen -> Envelope_Curve Ugen -> Maybe Ugen -> Ugen
varLag_env in_ time warp start =
  let rt = rateOf in_
      start_ = fromMaybe in_ start
      e = Envelope [start_,in_] [time] [warp] Nothing Nothing 0
      -- e[6] = curve; e[7] = curvature;
      time_ch = if rateOf time == InitialisationRate then 0 else changed time 0
      tr = changed in_ 0 + time_ch + impulse rt 0 0
  in envGen rt tr 1 0 1 DoNothing e

{- | k channel white noise.

> whiteNoiseN 2 ar * 0.1
-}
whiteNoiseMN :: Uid m => Int -> Rate -> m Ugen
whiteNoiseMN k r = fmap mce (mapM (\_ -> whiteNoiseM r) [1 .. k])

whiteNoiseN :: Int -> Rate -> Ugen
whiteNoiseN k = liftUnsafe1 (whiteNoiseMN k)

{- | If @z@ isn't a sink node route to an @out@ node writing to @bus@.
     If @fadeTime@ is given multiply by 'makeFadeEnv'.

> import Sound.Sc3 {- hsc3 -}
> audition (wrapOut (Just 1) (sinOsc ar 440 0 * 0.1))
> import Sound.Osc {- hosc -}
> withSc3 (sendMessage (n_set1 (-1) "gate" 0))
-}
wrapOut :: Maybe Double -> Ugen -> Ugen
wrapOut fadeTime z =
  if isSink z
  then z
  else out (control kr "out" 0) (maybe z ((* z) . makeFadeEnv) fadeTime)

-- * wslib

-- | Cross-fading version of 'playBuf'.
playBufCF :: Int -> Ugen -> Ugen -> Ugen -> Ugen -> Loop Ugen -> Ugen -> Int -> Ugen
playBufCF nc bufnum rate trigger startPos loop lag' n =
    let trigger' = if rateOf trigger == DemandRate
                   then tDuty ar trigger 0 DoNothing 1 0
                   else trigger
        index' = stepper trigger' 0 0 (constant n - 1) 1 0
        on = map
             (\i -> inRange index' (i - 0.5) (i + 0.5))
             [0 .. constant n - 1]
        rate' = case rateOf rate of
                  DemandRate -> map (\on' -> demand on' 0 rate) on
                  ControlRate -> map (gate rate) on
                  AudioRate -> map (gate rate) on
                  InitialisationRate -> map (const rate) on
        startPos' = if rateOf startPos == DemandRate
                    then demand trigger' 0 startPos
                    else startPos
        lag'' = 1 / lag'
        s = zipWith
            (\on' r -> let p = playBuf nc ar bufnum r on' startPos' loop DoNothing
                       in p * sqrt (slew on' lag'' lag''))
            on rate'
    in sum_opt s

-- * adc

-- | An oscillator that reads through a table once.
osc1 :: Rate -> Ugen -> Ugen -> DoneAction Ugen -> Ugen
osc1 rt buf dur doneAction =
    let ph = line rt 0 (bufFrames ir buf - 1) dur doneAction
    in bufRd 1 rt buf ph NoLoop LinearInterpolation
