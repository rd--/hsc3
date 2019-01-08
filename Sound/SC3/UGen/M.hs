module Sound.SC3.UGen.M where

import Control.Monad.Trans.State.Lazy {- transformers -}
import Data.Functor.Identity {- base -}
import Data.Maybe {- base -}

import Sound.SC3.Common.UId {- hsc3 -}
import Sound.SC3.UGen.Rate {- hsc3 -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

-- * TYPE

newtype UGen m = UGen {mk_ugen :: m SC3.UGen}

type F1 t = t -> t
type F2 t = t -> t -> t
type F3 t = t -> t -> t -> t
type F4 t = t -> t -> t -> t -> t
type F5 t = t -> t -> t -> t -> t -> t
type F6 t = t -> t -> t -> t -> t -> t -> t
type F7 t = t -> t -> t -> t -> t -> t -> t -> t
type F8 t = t -> t -> t -> t -> t -> t -> t -> t -> t
type F9 t = t -> t -> t -> t -> t -> t -> t -> t -> t -> t
type F10 t = t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t

-- * LIFT

lift_ugen_0 :: Monad m => SC3.UGen -> UGen m
lift_ugen_0 f = UGen (return f)

lift_ugen_1 :: Monad m => F1 SC3.UGen -> F1 (UGen m)
lift_ugen_1 f (UGen p) = UGen $ do
  p' <- p
  return (f p')

lift_ugen_2 :: Monad m => F2 SC3.UGen -> F2 (UGen m)
lift_ugen_2 f (UGen p) (UGen q) = UGen $ do
  p' <- p
  q' <- q
  return (f p' q')

lift_ugen_3 :: Monad m => F3 SC3.UGen -> F3 (UGen m)
lift_ugen_3 f (UGen p) (UGen q) (UGen r) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  return (f p' q' r')

lift_ugen_4 :: Monad m => F4 SC3.UGen -> F4 (UGen m)
lift_ugen_4 f (UGen p) (UGen q) (UGen r) (UGen s) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  return (f p' q' r' s')

lift_ugen_5 :: Monad m => F5 SC3.UGen -> F5 (UGen m)
lift_ugen_5 f (UGen p) (UGen q) (UGen r) (UGen s) (UGen t) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- t
  return (f p' q' r' s' t')

lift_ugen_6 :: Monad m => F6 SC3.UGen -> F6 (UGen m)
lift_ugen_6 f (UGen p) (UGen q) (UGen r) (UGen s) (UGen t) (UGen u) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- t
  u' <- u
  return (f p' q' r' s' t' u')

lift_ugen_7 :: Monad m => F7 SC3.UGen -> F7 (UGen m)
lift_ugen_7 f (UGen p) (UGen q) (UGen r) (UGen s) (UGen t) (UGen u) (UGen v) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- t
  u' <- u
  v' <- v
  return (f p' q' r' s' t' u' v')

lift_ugen_8 :: Monad m => F8 SC3.UGen -> F8 (UGen m)
lift_ugen_8 f (UGen p) (UGen q) (UGen r) (UGen s) (UGen t) (UGen u) (UGen v) (UGen w) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- t
  u' <- u
  v' <- v
  w' <- w
  return (f p' q' r' s' t' u' v' w')

lift_ugen_9 :: Monad m => F9 SC3.UGen -> F9 (UGen m)
lift_ugen_9 f (UGen p) (UGen q) (UGen r) (UGen s) (UGen t) (UGen u) (UGen v) (UGen w) (UGen x) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- t
  u' <- u
  v' <- v
  w' <- w
  x' <- x
  return (f p' q' r' s' t' u' v' w' x')


lift_ugen_10 :: Monad m => F10 SC3.UGen -> F10 (UGen m)
lift_ugen_10 f (UGen p) (UGen q) (UGen r) (UGen s) (UGen t) (UGen u) (UGen v) (UGen w) (UGen x) (UGen y) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- t
  u' <- u
  v' <- v
  w' <- w
  x' <- x
  y' <- y
  return (f p' q' r' s' t' u' v' w' x' y')

-- * LIFT M

lift_ugenM_0 :: Monad m => m SC3.UGen -> UGen m
lift_ugenM_0 = UGen

lift_ugenM_1 :: Monad m => (SC3.UGen -> m SC3.UGen) -> UGen m -> UGen m
lift_ugenM_1 f (UGen p) = UGen $ do
  p' <- p
  f p'

lift_ugenM_2 :: Monad m => (SC3.UGen -> SC3.UGen -> m SC3.UGen) -> UGen m -> UGen m -> UGen m
lift_ugenM_2 f (UGen p) (UGen q) = UGen $ do
  p' <- p
  q' <- q
  f p' q'

lift_ugenM_3 :: Monad m => (SC3.UGen -> SC3.UGen -> SC3.UGen -> m SC3.UGen) -> UGen m -> UGen m -> UGen m -> UGen m
lift_ugenM_3 f (UGen p) (UGen q) (UGen r) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  f p' q' r'

lift_ugenM_4 :: Monad m => (SC3.UGen -> SC3.UGen -> SC3.UGen -> SC3.UGen -> m SC3.UGen) -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
lift_ugenM_4 f (UGen p) (UGen q) (UGen r) (UGen s) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  f p' q' r' s'

-- * INSTANCES

instance Monad m => Num (UGen m) where
  (+) = lift_ugen_2 (+)
  (*) = lift_ugen_2 (*)
  (-) = lift_ugen_2 (-)
  abs = lift_ugen_1 abs
  signum = lift_ugen_1 signum
  negate = lift_ugen_1 negate
  fromInteger = UGen . return . fromInteger

instance Monad m => Fractional (UGen m) where
  (/) = lift_ugen_2 (/)
  recip = lift_ugen_1 recip
  fromRational = UGen . return . fromRational

instance Monad m => Floating (UGen m) where
  pi = lift_ugen_0 pi
  exp = lift_ugen_1 exp
  log = lift_ugen_1 log
  sqrt = lift_ugen_1 sqrt
  (**) = lift_ugen_2 (**)
  logBase = lift_ugen_2 logBase
  sin = lift_ugen_1 sin
  cos = lift_ugen_1 cos
  tan = lift_ugen_1 tan
  asin = lift_ugen_1 asin
  acos = lift_ugen_1 acos
  atan = lift_ugen_1 atan
  sinh = lift_ugen_1 sinh
  cosh = lift_ugen_1 cosh
  tanh = lift_ugen_1 tanh
  asinh = lift_ugen_1 asinh
  acosh = lift_ugen_1 acosh
  atanh = lift_ugen_1 atanh

instance Monad m => Eq (UGen m) where
  (==) = error "UGen: =="

instance Monad m => Ord (UGen m) where
{-
    (<) = lift_ugen_2 (<)
    (<=) = lift_ugen_2 (<=)
    (>) = lift_ugen_2 (>)
    (>=) = lift_ugen_2 (>=)
-}
  compare = error "UGen: compare"
  min = lift_ugen_2 min
  max = lift_ugen_2 max

instance Monad m => SC3.UnaryOp (UGen m) where
  midiCPS = lift_ugen_1 SC3.midiCPS


{-
import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

draw :: UGen (StateT Int Identity) -> IO ()
draw (UGen u) = Dot.draw (uid_st_eval u)
-}

audition :: UGen (StateT Int Identity) -> IO ()
audition (UGen u) = SC3.audition (uid_st_eval u)

-- > draw t0
-- > audition t0
t0 :: UId m => UGen m
t0 = out 0 (mce2 (sinOsc AR 440 0 * 0.1) ((whiteNoise AR - whiteNoise AR) * 0.1))

-- * UTIL

mce :: Monad m => [UGen m] -> UGen m
mce x = UGen $ do
  x' <- sequence (map mk_ugen x)
  return (SC3.mce x')

mce2 :: Monad m => UGen m -> UGen m -> UGen m
mce2 = lift_ugen_2 SC3.mce2

-- * AUTOGEN

a2k :: Monad m => UGen m -> UGen m
a2k = lift_ugen_1 SC3.a2k

apf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
apf = lift_ugen_3 SC3.apf

allpassC :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
allpassC = lift_ugen_4 SC3.allpassC

allpassL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
allpassL = lift_ugen_4 SC3.allpassL

allpassN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
allpassN = lift_ugen_4 SC3.allpassN

ampComp :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
ampComp rate = lift_ugen_3 (SC3.ampComp rate)

ampCompA :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
ampCompA rate = lift_ugen_4 (SC3.ampCompA rate)

amplitude :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
amplitude rate = lift_ugen_3 (SC3.amplitude rate)

bAllPass :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bAllPass = lift_ugen_3 SC3.bAllPass

bBandPass :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bBandPass = lift_ugen_3 SC3.bBandPass

bBandStop :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bBandStop = lift_ugen_3 SC3.bBandStop

bHiPass :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bHiPass = lift_ugen_3 SC3.bHiPass

bHiShelf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bHiShelf = lift_ugen_4 SC3.bHiShelf

bLowPass :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bLowPass = lift_ugen_3 SC3.bLowPass

bLowShelf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bLowShelf = lift_ugen_4 SC3.bLowShelf

bpf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bpf = lift_ugen_3 SC3.bpf

bpz2 :: Monad m => UGen m -> UGen m
bpz2 = lift_ugen_1 SC3.bpz2

bPeakEQ :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bPeakEQ = lift_ugen_4 SC3.bPeakEQ

brf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
brf = lift_ugen_3 SC3.brf

brz2 :: Monad m => UGen m -> UGen m
brz2 = lift_ugen_1 SC3.brz2

balance2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
balance2 = lift_ugen_4 SC3.balance2

ball :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
ball rate = lift_ugen_4 (SC3.ball rate)

beatTrack :: Monad m => Rate -> UGen m -> UGen m -> UGen m
beatTrack rate = lift_ugen_2 (SC3.beatTrack rate)

beatTrack2 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
beatTrack2 rate = lift_ugen_6 (SC3.beatTrack2 rate)

biPanB2 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
biPanB2 rate = lift_ugen_4 (SC3.biPanB2 rate)

binaryOpUGen :: Monad m => UGen m -> UGen m -> UGen m
binaryOpUGen = lift_ugen_2 SC3.binaryOpUGen

blip :: Monad m => Rate -> UGen m -> UGen m -> UGen m
blip rate = lift_ugen_2 (SC3.blip rate)

blockSize :: Monad m => UGen m
blockSize = lift_ugen_0 SC3.blockSize

brownNoise :: UId m => Rate -> UGen m
brownNoise rate = lift_ugenM_0 (SC3.brownNoiseM rate)

bufAllpassC :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bufAllpassC = lift_ugen_4 SC3.bufAllpassC

bufAllpassL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bufAllpassL = lift_ugen_4 SC3.bufAllpassL

bufAllpassN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bufAllpassN = lift_ugen_4 SC3.bufAllpassN

bufChannels :: Monad m => Rate -> UGen m -> UGen m
bufChannels rate = lift_ugen_1 (SC3.bufChannels rate)

bufCombC :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bufCombC = lift_ugen_4 SC3.bufCombC

bufCombL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bufCombL = lift_ugen_4 SC3.bufCombL

bufCombN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
bufCombN = lift_ugen_4 SC3.bufCombN

bufDelayC :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bufDelayC = lift_ugen_3 SC3.bufDelayC

bufDelayL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bufDelayL = lift_ugen_3 SC3.bufDelayL

bufDelayN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
bufDelayN = lift_ugen_3 SC3.bufDelayN

bufDur :: Monad m => Rate -> UGen m -> UGen m
bufDur rate = lift_ugen_1 (SC3.bufDur rate)

bufFrames :: Monad m => Rate -> UGen m -> UGen m
bufFrames rate = lift_ugen_1 (SC3.bufFrames rate)

bufRateScale :: Monad m => Rate -> UGen m -> UGen m
bufRateScale rate = lift_ugen_1 (SC3.bufRateScale rate)

--bufRd :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--bufRd rate = lift_ugen_4 (SC3.bufRd rate)

bufSampleRate :: Monad m => Rate -> UGen m -> UGen m
bufSampleRate rate = lift_ugen_1 (SC3.bufSampleRate rate)

bufSamples :: Monad m => Rate -> UGen m -> UGen m
bufSamples rate = lift_ugen_1 (SC3.bufSamples rate)

--bufWr :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--bufWr = lift_ugen_4 SC3.bufWr

cOsc :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
cOsc rate = lift_ugen_3 (SC3.cOsc rate)

checkBadValues :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
checkBadValues = lift_ugen_3 SC3.checkBadValues

clip :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
clip = lift_ugen_3 SC3.clip

clipNoise :: UId m => Rate -> UGen m
clipNoise rate = lift_ugenM_0 (SC3.clipNoiseM rate)

coinGate :: UId m => UGen m -> UGen m -> UGen m
coinGate = lift_ugenM_2 SC3.coinGateM

combC :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
combC = lift_ugen_4 SC3.combC

combL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
combL = lift_ugen_4 SC3.combL

combN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
combN = lift_ugen_4 SC3.combN

compander :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
compander = lift_ugen_7 SC3.compander

companderD :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
companderD rate = lift_ugen_6 (SC3.companderD rate)

controlDur :: Monad m => UGen m
controlDur = lift_ugen_0 SC3.controlDur

controlRate :: Monad m => UGen m
controlRate = lift_ugen_0 SC3.controlRate

convolution :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
convolution rate = lift_ugen_3 (SC3.convolution rate)

convolution2 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
convolution2 rate = lift_ugen_4 (SC3.convolution2 rate)

convolution2L :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
convolution2L rate = lift_ugen_5 (SC3.convolution2L rate)

convolution3 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
convolution3 rate = lift_ugen_4 (SC3.convolution3 rate)

crackle :: Monad m => Rate -> UGen m -> UGen m
crackle rate = lift_ugen_1 (SC3.crackle rate)

cuspL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
cuspL rate = lift_ugen_4 (SC3.cuspL rate)

cuspN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
cuspN rate = lift_ugen_4 (SC3.cuspN rate)

dc :: Monad m => Rate -> UGen m -> UGen m
dc rate = lift_ugen_1 (SC3.dc rate)

dbrown :: UId m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
dbrown = lift_ugenM_4 SC3.dbrownM

--dbufrd :: UId m => UGen m -> UGen m -> UGen m -> UGen m
--dbufrd = lift_ugenM_3 SC3.dbufrdM

--dbufwr :: UId m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--dbufwr = lift_ugenM_4 SC3.dbufwrM

dconst :: UId m => UGen m -> UGen m -> UGen m -> UGen m
dconst = lift_ugenM_3 SC3.dconstM

decay :: Monad m => UGen m -> UGen m -> UGen m
decay = lift_ugen_2 SC3.decay

decay2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
decay2 = lift_ugen_3 SC3.decay2

decodeB2 :: Monad m => Int -> Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
decodeB2 numChannels rate = lift_ugen_4 (SC3.decodeB2 numChannels rate)

degreeToKey :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
degreeToKey = lift_ugen_3 SC3.degreeToKey

delTapRd :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
delTapRd = lift_ugen_4 SC3.delTapRd

delTapWr :: Monad m => UGen m -> UGen m -> UGen m
delTapWr = lift_ugen_2 SC3.delTapWr

delay1 :: Monad m => UGen m -> UGen m
delay1 = lift_ugen_1 SC3.delay1

delay2 :: Monad m => UGen m -> UGen m
delay2 = lift_ugen_1 SC3.delay2

delayC :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
delayC = lift_ugen_3 SC3.delayC

delayL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
delayL = lift_ugen_3 SC3.delayL

delayN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
delayN = lift_ugen_3 SC3.delayN

demand :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
demand = lift_ugen_3 SC3.demand

--demandEnvGen :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--demandEnvGen rate = lift_ugen_10 (SC3.demandEnvGen rate)

detectIndex :: Monad m => UGen m -> UGen m -> UGen m
detectIndex = lift_ugen_2 SC3.detectIndex

--detectSilence :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--detectSilence = lift_ugen_4 SC3.detectSilence

dgeom :: UId m => UGen m -> UGen m -> UGen m -> UGen m
dgeom = lift_ugenM_3 SC3.dgeomM

dibrown :: UId m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
dibrown = lift_ugenM_4 SC3.dibrownM

--diskIn :: Monad m => UGen m -> UGen m -> UGen m
--diskIn = lift_ugen_2 SC3.diskIn

diskOut :: Monad m => UGen m -> UGen m -> UGen m
diskOut = lift_ugen_2 SC3.diskOut

diwhite :: UId m => UGen m -> UGen m -> UGen m -> UGen m
diwhite = lift_ugenM_3 SC3.diwhiteM

donce :: UId m => UGen m -> UGen m
donce = lift_ugenM_1 SC3.donceM

done :: Monad m => UGen m -> UGen m
done = lift_ugen_1 SC3.done

dpoll :: UId m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
dpoll = lift_ugenM_4 SC3.dpollM

drand :: UId m => UGen m -> UGen m -> UGen m
drand = lift_ugenM_2 SC3.drandM

dreset :: UId m => UGen m -> UGen m -> UGen m
dreset = lift_ugenM_2 SC3.dresetM

dseq :: UId m => UGen m -> UGen m -> UGen m
dseq = lift_ugenM_2 SC3.dseqM

dser :: UId m => UGen m -> UGen m -> UGen m
dser = lift_ugenM_2 SC3.dserM

dseries :: UId m => UGen m -> UGen m -> UGen m -> UGen m
dseries = lift_ugenM_3 SC3.dseriesM

dshuf :: UId m => UGen m -> UGen m -> UGen m
dshuf = lift_ugenM_2 SC3.dshufM

dstutter :: UId m => UGen m -> UGen m -> UGen m
dstutter = lift_ugenM_2 SC3.dstutterM

dswitch :: UId m => UGen m -> UGen m -> UGen m
dswitch = lift_ugenM_2 SC3.dswitchM

dswitch1 :: UId m => UGen m -> UGen m -> UGen m
dswitch1 = lift_ugenM_2 SC3.dswitch1M

dunique :: UId m => UGen m -> UGen m -> UGen m -> UGen m
dunique = lift_ugenM_3 SC3.duniqueM

dust :: UId m => Rate -> UGen m -> UGen m
dust rate = lift_ugenM_1 (SC3.dustM rate)

dust2 :: UId m => Rate -> UGen m -> UGen m
dust2 rate = lift_ugenM_1 (SC3.dust2M rate)

--duty :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--duty rate = lift_ugen_4 (SC3.duty rate)

dwhite :: UId m => UGen m -> UGen m -> UGen m -> UGen m
dwhite = lift_ugenM_3 SC3.dwhiteM

dwrand :: UId m => UGen m -> UGen m -> UGen m -> UGen m
dwrand = lift_ugenM_3 SC3.dwrandM

dxrand :: UId m => UGen m -> UGen m -> UGen m
dxrand = lift_ugenM_2 SC3.dxrandM

envelope_to_ugen :: Monad m => SC3.Envelope (UGen m) -> [UGen m]
envelope_to_ugen =
    let err = error "envGen: bad Envelope"
    in fromMaybe err . SC3.envelope_sc3_array

envGen :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> SC3.DoneAction (UGen m) -> SC3.Envelope (UGen m) -> UGen m
envGen rate (UGen p) (UGen q) (UGen r) (UGen s) t u = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  t' <- mk_ugen (SC3.from_done_action t)
  u' <- sequence (map mk_ugen (envelope_to_ugen u))
  return (SC3.envGen_ll rate p' q' r' s' t' (SC3.mce u'))

expRand :: UId m => UGen m -> UGen m -> UGen m
expRand = lift_ugenM_2 SC3.expRandM

fbSineC :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
fbSineC rate = lift_ugen_7 (SC3.fbSineC rate)

fbSineL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
fbSineL rate = lift_ugen_7 (SC3.fbSineL rate)

fbSineN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
fbSineN rate = lift_ugen_7 (SC3.fbSineN rate)

fft :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
fft = lift_ugen_6 SC3.fft

fos :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
fos = lift_ugen_4 SC3.fos

fSinOsc :: Monad m => Rate -> UGen m -> UGen m -> UGen m
fSinOsc rate = lift_ugen_2 (SC3.fSinOsc rate)

fold :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
fold = lift_ugen_3 SC3.fold

formant :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
formant rate = lift_ugen_3 (SC3.formant rate)

formlet :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
formlet = lift_ugen_4 SC3.formlet

free :: Monad m => UGen m -> UGen m -> UGen m
free = lift_ugen_2 SC3.free

freeSelf :: Monad m => UGen m -> UGen m
freeSelf = lift_ugen_1 SC3.freeSelf

freeSelfWhenDone :: Monad m => UGen m -> UGen m
freeSelfWhenDone = lift_ugen_1 SC3.freeSelfWhenDone

freeVerb :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
freeVerb = lift_ugen_4 SC3.freeVerb

freeVerb2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
freeVerb2 = lift_ugen_5 SC3.freeVerb2

freqShift :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
freqShift = lift_ugen_3 SC3.freqShift

gVerb :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
gVerb = lift_ugen_10 SC3.gVerb

gate :: Monad m => UGen m -> UGen m -> UGen m
gate = lift_ugen_2 SC3.gate

gbmanL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
gbmanL rate = lift_ugen_3 (SC3.gbmanL rate)

gbmanN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
gbmanN rate = lift_ugen_3 (SC3.gbmanN rate)

{-
gendy1 :: UId m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
gendy1 rate = lift_ugenM_10 (SC3.gendy1M rate)

gendy2 :: UId m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
gendy2 rate = lift_ugenM_12 (SC3.gendy2M rate)

gendy3 :: UId m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
gendy3 rate = lift_ugenM_9 (SC3.gendy3M rate)
-}

grainBuf :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
grainBuf numChannels = lift_ugen_9 (SC3.grainBuf numChannels)

grainFM :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
grainFM numChannels = lift_ugen_8 (SC3.grainFM numChannels)

grainIn :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
grainIn numChannels = lift_ugen_6 (SC3.grainIn numChannels)

grainSin :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
grainSin numChannels = lift_ugen_6 (SC3.grainSin numChannels)

grayNoise :: UId m => Rate -> UGen m
grayNoise rate = lift_ugenM_0 (SC3.grayNoiseM rate)

hpf :: Monad m => UGen m -> UGen m -> UGen m
hpf = lift_ugen_2 SC3.hpf

hpz1 :: Monad m => UGen m -> UGen m
hpz1 = lift_ugen_1 SC3.hpz1

hpz2 :: Monad m => UGen m -> UGen m
hpz2 = lift_ugen_1 SC3.hpz2

hasher :: Monad m => UGen m -> UGen m
hasher = lift_ugen_1 SC3.hasher

henonC :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
henonC rate = lift_ugen_5 (SC3.henonC rate)

henonL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
henonL rate = lift_ugen_5 (SC3.henonL rate)

henonN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
henonN rate = lift_ugen_5 (SC3.henonN rate)

hilbert :: Monad m => UGen m -> UGen m
hilbert = lift_ugen_1 SC3.hilbert

--iEnvGen :: Monad m => Rate -> UGen m -> UGen m -> UGen m
--iEnvGen rate = lift_ugen_2 (SC3.iEnvGen rate)

ifft :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
ifft = lift_ugen_3 SC3.ifft

iRand :: UId m => UGen m -> UGen m -> UGen m
iRand = lift_ugenM_2 SC3.iRandM

impulse :: Monad m => Rate -> UGen m -> UGen m -> UGen m
impulse rate = lift_ugen_2 (SC3.impulse rate)

in' :: Monad m => Int -> Rate -> UGen m -> UGen m
in' numChannels rate = lift_ugen_1 (SC3.in' numChannels rate)

inFeedback :: Monad m => Int -> UGen m -> UGen m
inFeedback numChannels = lift_ugen_1 (SC3.inFeedback numChannels)

inRange :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
inRange = lift_ugen_3 SC3.inRange

inRect :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
inRect rate = lift_ugen_3 (SC3.inRect rate)

inTrig :: Monad m => Int -> UGen m -> UGen m
inTrig numChannels = lift_ugen_1 (SC3.inTrig numChannels)

index :: Monad m => UGen m -> UGen m -> UGen m
index = lift_ugen_2 SC3.index

indexInBetween :: Monad m => UGen m -> UGen m -> UGen m
indexInBetween = lift_ugen_2 SC3.indexInBetween

indexL :: Monad m => UGen m -> UGen m -> UGen m
indexL = lift_ugen_2 SC3.indexL

infoUGenBase :: Monad m => Rate -> UGen m
infoUGenBase rate = lift_ugen_0 (SC3.infoUGenBase rate)

integrator :: Monad m => UGen m -> UGen m -> UGen m
integrator = lift_ugen_2 SC3.integrator

k2a :: Monad m => UGen m -> UGen m
k2a = lift_ugen_1 SC3.k2a

keyState :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
keyState rate = lift_ugen_4 (SC3.keyState rate)

keyTrack :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
keyTrack rate = lift_ugen_3 (SC3.keyTrack rate)

klang :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
klang rate = lift_ugen_3 (SC3.klang rate)

klank :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
klank = lift_ugen_5 SC3.klank

lfClipNoise :: UId m => Rate -> UGen m -> UGen m
lfClipNoise rate = lift_ugenM_1 (SC3.lfClipNoiseM rate)

lfCub :: Monad m => Rate -> UGen m -> UGen m -> UGen m
lfCub rate = lift_ugen_2 (SC3.lfCub rate)

lfdClipNoise :: UId m => Rate -> UGen m -> UGen m
lfdClipNoise rate = lift_ugenM_1 (SC3.lfdClipNoiseM rate)

lfdNoise0 :: UId m => Rate -> UGen m -> UGen m
lfdNoise0 rate = lift_ugenM_1 (SC3.lfdNoise0M rate)

lfdNoise1 :: UId m => Rate -> UGen m -> UGen m
lfdNoise1 rate = lift_ugenM_1 (SC3.lfdNoise1M rate)

lfdNoise3 :: UId m => Rate -> UGen m -> UGen m
lfdNoise3 rate = lift_ugenM_1 (SC3.lfdNoise3M rate)

--lfGauss :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--lfGauss rate = lift_ugen_5 (SC3.lfGauss rate)

lfNoise0 :: UId m => Rate -> UGen m -> UGen m
lfNoise0 rate = lift_ugenM_1 (SC3.lfNoise0M rate)

lfNoise1 :: UId m => Rate -> UGen m -> UGen m
lfNoise1 rate = lift_ugenM_1 (SC3.lfNoise1M rate)

lfNoise2 :: UId m => Rate -> UGen m -> UGen m
lfNoise2 rate = lift_ugenM_1 (SC3.lfNoise2M rate)

lfPar :: Monad m => Rate -> UGen m -> UGen m -> UGen m
lfPar rate = lift_ugen_2 (SC3.lfPar rate)

lfPulse :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
lfPulse rate = lift_ugen_3 (SC3.lfPulse rate)

lfSaw :: Monad m => Rate -> UGen m -> UGen m -> UGen m
lfSaw rate = lift_ugen_2 (SC3.lfSaw rate)

lfTri :: Monad m => Rate -> UGen m -> UGen m -> UGen m
lfTri rate = lift_ugen_2 (SC3.lfTri rate)

lpf :: Monad m => UGen m -> UGen m -> UGen m
lpf = lift_ugen_2 SC3.lpf

lpz1 :: Monad m => UGen m -> UGen m
lpz1 = lift_ugen_1 SC3.lpz1

lpz2 :: Monad m => UGen m -> UGen m
lpz2 = lift_ugen_1 SC3.lpz2

lag :: Monad m => UGen m -> UGen m -> UGen m
lag = lift_ugen_2 SC3.lag

lag2 :: Monad m => UGen m -> UGen m -> UGen m
lag2 = lift_ugen_2 SC3.lag2

lag2UD :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
lag2UD = lift_ugen_3 SC3.lag2UD

lag3 :: Monad m => UGen m -> UGen m -> UGen m
lag3 = lift_ugen_2 SC3.lag3

lag3UD :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
lag3UD = lift_ugen_3 SC3.lag3UD

lagIn :: Monad m => Int -> UGen m -> UGen m -> UGen m
lagIn numChannels = lift_ugen_2 (SC3.lagIn numChannels)

lagUD :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
lagUD = lift_ugen_3 SC3.lagUD

lastValue :: Monad m => UGen m -> UGen m -> UGen m
lastValue = lift_ugen_2 SC3.lastValue

latch :: Monad m => UGen m -> UGen m -> UGen m
latch = lift_ugen_2 SC3.latch

latoocarfianC :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
latoocarfianC rate = lift_ugen_7 (SC3.latoocarfianC rate)

latoocarfianL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
latoocarfianL rate = lift_ugen_7 (SC3.latoocarfianL rate)

latoocarfianN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
latoocarfianN rate = lift_ugen_7 (SC3.latoocarfianN rate)

leakDC :: Monad m => UGen m -> UGen m -> UGen m
leakDC = lift_ugen_2 SC3.leakDC

leastChange :: Monad m => Rate -> UGen m -> UGen m -> UGen m
leastChange rate = lift_ugen_2 (SC3.leastChange rate)

limiter :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
limiter = lift_ugen_3 SC3.limiter

linCongC :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
linCongC rate = lift_ugen_5 (SC3.linCongC rate)

linCongL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
linCongL rate = lift_ugen_5 (SC3.linCongL rate)

linCongN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
linCongN rate = lift_ugen_5 (SC3.linCongN rate)

linExp :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
linExp = lift_ugen_5 SC3.linExp

linPan2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
linPan2 = lift_ugen_3 SC3.linPan2

linRand :: UId m => UGen m -> UGen m -> UGen m -> UGen m
linRand = lift_ugenM_3 SC3.linRandM

linXFade2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
linXFade2 = lift_ugen_4 SC3.linXFade2

--line :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--line rate = lift_ugen_4 (SC3.line rate)

--linen :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--linen = lift_ugen_5 SC3.linen

localBuf :: UId m => UGen m -> UGen m -> UGen m
localBuf = lift_ugenM_2 SC3.localBufM

localIn :: Monad m => Int -> Rate -> UGen m -> UGen m
localIn numChannels rate = lift_ugen_1 (SC3.localIn numChannels rate)

localOut :: Monad m => UGen m -> UGen m
localOut = lift_ugen_1 SC3.localOut

logistic :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
logistic rate = lift_ugen_3 (SC3.logistic rate)

lorenzL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
lorenzL rate = lift_ugen_8 (SC3.lorenzL rate)

loudness :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
loudness = lift_ugen_3 SC3.loudness

mfcc :: Monad m => Rate -> UGen m -> UGen m -> UGen m
mfcc rate = lift_ugen_2 (SC3.mfcc rate)

mantissaMask :: Monad m => UGen m -> UGen m -> UGen m
mantissaMask = lift_ugen_2 SC3.mantissaMask

median :: Monad m => UGen m -> UGen m -> UGen m
median = lift_ugen_2 SC3.median

midEQ :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
midEQ = lift_ugen_4 SC3.midEQ

modDif :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
modDif rate = lift_ugen_3 (SC3.modDif rate)

moogFF :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
moogFF = lift_ugen_4 SC3.moogFF

mostChange :: Monad m => UGen m -> UGen m -> UGen m
mostChange = lift_ugen_2 SC3.mostChange

mouseButton :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
mouseButton rate = lift_ugen_3 (SC3.mouseButton rate)

--mouseX :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--mouseX rate = lift_ugen_4 (SC3.mouseX rate)

--mouseY :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--mouseY rate = lift_ugen_4 (SC3.mouseY rate)

nRand :: UId m => UGen m -> UGen m -> UGen m -> UGen m
nRand = lift_ugenM_3 SC3.nRandM

nodeID :: Monad m => Rate -> UGen m
nodeID rate = lift_ugen_0 (SC3.nodeID rate)

normalizer :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
normalizer = lift_ugen_3 SC3.normalizer

numAudioBuses :: Monad m => UGen m
numAudioBuses = lift_ugen_0 SC3.numAudioBuses

numBuffers :: Monad m => UGen m
numBuffers = lift_ugen_0 SC3.numBuffers

numControlBuses :: Monad m => UGen m
numControlBuses = lift_ugen_0 SC3.numControlBuses

numInputBuses :: Monad m => UGen m
numInputBuses = lift_ugen_0 SC3.numInputBuses

numOutputBuses :: Monad m => UGen m
numOutputBuses = lift_ugen_0 SC3.numOutputBuses

numRunningSynths :: Monad m => UGen m
numRunningSynths = lift_ugen_0 SC3.numRunningSynths

offsetOut :: Monad m => UGen m -> UGen m -> UGen m
offsetOut = lift_ugen_2 SC3.offsetOut

onePole :: Monad m => UGen m -> UGen m -> UGen m
onePole = lift_ugen_2 SC3.onePole

oneZero :: Monad m => UGen m -> UGen m -> UGen m
oneZero = lift_ugen_2 SC3.oneZero

onsets :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
onsets = lift_ugen_9 SC3.onsets

osc :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
osc rate = lift_ugen_3 (SC3.osc rate)

oscN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
oscN rate = lift_ugen_3 (SC3.oscN rate)

out :: Monad m => UGen m -> UGen m -> UGen m
out = lift_ugen_2 SC3.out

pSinGrain :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
pSinGrain rate = lift_ugen_3 (SC3.pSinGrain rate)

pv_Add :: Monad m => UGen m -> UGen m -> UGen m
pv_Add = lift_ugen_2 SC3.pv_Add

pv_BinScramble :: UId m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pv_BinScramble = lift_ugenM_4 SC3.pv_BinScrambleM

pv_BinShift :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pv_BinShift = lift_ugen_4 SC3.pv_BinShift

pv_BinWipe :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pv_BinWipe = lift_ugen_3 SC3.pv_BinWipe

pv_BrickWall :: Monad m => UGen m -> UGen m -> UGen m
pv_BrickWall = lift_ugen_2 SC3.pv_BrickWall

pv_ChainUGen :: Monad m => UGen m -> UGen m
pv_ChainUGen = lift_ugen_1 SC3.pv_ChainUGen

pv_ConformalMap :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pv_ConformalMap = lift_ugen_3 SC3.pv_ConformalMap

pv_Conj :: Monad m => UGen m -> UGen m
pv_Conj = lift_ugen_1 SC3.pv_Conj

pv_Copy :: Monad m => UGen m -> UGen m -> UGen m
pv_Copy = lift_ugen_2 SC3.pv_Copy

pv_CopyPhase :: Monad m => UGen m -> UGen m -> UGen m
pv_CopyPhase = lift_ugen_2 SC3.pv_CopyPhase

pv_Diffuser :: Monad m => UGen m -> UGen m -> UGen m
pv_Diffuser = lift_ugen_2 SC3.pv_Diffuser

pv_Div :: Monad m => UGen m -> UGen m -> UGen m
pv_Div = lift_ugen_2 SC3.pv_Div

--pv_HainsworthFoote :: Monad m => UGen m -> UGen m
--pv_HainsworthFoote = lift_ugen_1 SC3.pv_HainsworthFoote

pv_JensenAndersen :: Monad m => UGen m -> UGen m
pv_JensenAndersen = lift_ugen_1 SC3.pv_JensenAndersen

pv_LocalMax :: Monad m => UGen m -> UGen m -> UGen m
pv_LocalMax = lift_ugen_2 SC3.pv_LocalMax

pv_MagAbove :: Monad m => UGen m -> UGen m -> UGen m
pv_MagAbove = lift_ugen_2 SC3.pv_MagAbove

pv_MagBelow :: Monad m => UGen m -> UGen m -> UGen m
pv_MagBelow = lift_ugen_2 SC3.pv_MagBelow

pv_MagClip :: Monad m => UGen m -> UGen m -> UGen m
pv_MagClip = lift_ugen_2 SC3.pv_MagClip

pv_MagDiv :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pv_MagDiv = lift_ugen_3 SC3.pv_MagDiv

pv_MagFreeze :: Monad m => UGen m -> UGen m -> UGen m
pv_MagFreeze = lift_ugen_2 SC3.pv_MagFreeze

pv_MagMul :: Monad m => UGen m -> UGen m -> UGen m
pv_MagMul = lift_ugen_2 SC3.pv_MagMul

pv_MagNoise :: Monad m => UGen m -> UGen m
pv_MagNoise = lift_ugen_1 SC3.pv_MagNoise

pv_MagShift :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pv_MagShift = lift_ugen_3 SC3.pv_MagShift

pv_MagSmear :: Monad m => UGen m -> UGen m -> UGen m
pv_MagSmear = lift_ugen_2 SC3.pv_MagSmear

pv_MagSquared :: Monad m => UGen m -> UGen m
pv_MagSquared = lift_ugen_1 SC3.pv_MagSquared

pv_Max :: Monad m => UGen m -> UGen m -> UGen m
pv_Max = lift_ugen_2 SC3.pv_Max

pv_Min :: Monad m => UGen m -> UGen m -> UGen m
pv_Min = lift_ugen_2 SC3.pv_Min

pv_Mul :: Monad m => UGen m -> UGen m -> UGen m
pv_Mul = lift_ugen_2 SC3.pv_Mul

pv_PhaseShift :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pv_PhaseShift = lift_ugen_3 SC3.pv_PhaseShift

pv_PhaseShift270 :: Monad m => UGen m -> UGen m
pv_PhaseShift270 = lift_ugen_1 SC3.pv_PhaseShift270

pv_PhaseShift90 :: Monad m => UGen m -> UGen m
pv_PhaseShift90 = lift_ugen_1 SC3.pv_PhaseShift90

pv_RandComb :: UId m => UGen m -> UGen m -> UGen m -> UGen m
pv_RandComb = lift_ugenM_3 SC3.pv_RandCombM

pv_RandWipe :: UId m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pv_RandWipe = lift_ugenM_4 SC3.pv_RandWipeM

pv_RectComb :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pv_RectComb = lift_ugen_4 SC3.pv_RectComb

pv_RectComb2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pv_RectComb2 = lift_ugen_5 SC3.pv_RectComb2

pan2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pan2 = lift_ugen_3 SC3.pan2

pan4 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pan4 rate = lift_ugen_4 (SC3.pan4 rate)

panAz :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
panAz numChannels  = lift_ugen_5 (SC3.panAz numChannels)

panB :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
panB rate = lift_ugen_4 (SC3.panB rate)

panB2 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
panB2 rate = lift_ugen_3 (SC3.panB2 rate)

partConv :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
partConv = lift_ugen_3 SC3.partConv

pause :: Monad m => UGen m -> UGen m -> UGen m
pause = lift_ugen_2 SC3.pause

pauseSelf :: Monad m => UGen m -> UGen m
pauseSelf = lift_ugen_1 SC3.pauseSelf

pauseSelfWhenDone :: Monad m => UGen m -> UGen m
pauseSelfWhenDone = lift_ugen_1 SC3.pauseSelfWhenDone

peak :: Monad m => UGen m -> UGen m -> UGen m
peak = lift_ugen_2 SC3.peak

peakFollower :: Monad m => UGen m -> UGen m -> UGen m
peakFollower = lift_ugen_2 SC3.peakFollower

phasor :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
phasor rate = lift_ugen_5 (SC3.phasor rate)

pinkNoise :: UId m => Rate -> UGen m
pinkNoise rate = lift_ugenM_0 (SC3.pinkNoiseM rate)

--pitch :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--pitch = lift_ugen_11 SC3.pitch

pitchShift :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pitchShift = lift_ugen_5 SC3.pitchShift

--playBuf :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--playBuf rate = lift_ugen_6 (SC3.playBuf rate)

pluck :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
pluck = lift_ugen_6 SC3.pluck

poll :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
poll = lift_ugen_4 SC3.poll

pulse :: Monad m => Rate -> UGen m -> UGen m -> UGen m
pulse rate = lift_ugen_2 (SC3.pulse rate)

pulseCount :: Monad m => UGen m -> UGen m -> UGen m
pulseCount = lift_ugen_2 SC3.pulseCount

pulseDivider :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
pulseDivider = lift_ugen_3 SC3.pulseDivider

quadC :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
quadC rate = lift_ugen_5 (SC3.quadC rate)

quadL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
quadL rate = lift_ugen_5 (SC3.quadL rate)

quadN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
quadN rate = lift_ugen_5 (SC3.quadN rate)

rhpf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
rhpf = lift_ugen_3 SC3.rhpf

rlpf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
rlpf = lift_ugen_3 SC3.rlpf

radiansPerSample :: Monad m => UGen m
radiansPerSample = lift_ugen_0 SC3.radiansPerSample

ramp :: Monad m => UGen m -> UGen m -> UGen m
ramp = lift_ugen_2 SC3.ramp

rand :: UId m => UGen m -> UGen m -> UGen m
rand = lift_ugenM_2 SC3.randM

randID :: Monad m => Rate -> UGen m -> UGen m
randID rate = lift_ugen_1 (SC3.randID rate)

randSeed :: Monad m => Rate -> UGen m -> UGen m -> UGen m
randSeed rate = lift_ugen_2 (SC3.randSeed rate)

--recordBuf :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--recordBuf rate = lift_ugen_9 (SC3.recordBuf rate)

replaceOut :: Monad m => UGen m -> UGen m -> UGen m
replaceOut = lift_ugen_2 SC3.replaceOut

resonz :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
resonz = lift_ugen_3 SC3.resonz

ringz :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
ringz = lift_ugen_3 SC3.ringz

rotate2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
rotate2 = lift_ugen_3 SC3.rotate2

runningMax :: Monad m => UGen m -> UGen m -> UGen m
runningMax = lift_ugen_2 SC3.runningMax

runningMin :: Monad m => UGen m -> UGen m -> UGen m
runningMin = lift_ugen_2 SC3.runningMin

runningSum :: Monad m => UGen m -> UGen m -> UGen m
runningSum = lift_ugen_2 SC3.runningSum

sos :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
sos = lift_ugen_6 SC3.sos

sampleDur :: Monad m => UGen m
sampleDur = lift_ugen_0 SC3.sampleDur

sampleRate :: Monad m => UGen m
sampleRate = lift_ugen_0 SC3.sampleRate

saw :: Monad m => Rate -> UGen m -> UGen m
saw rate = lift_ugen_1 (SC3.saw rate)

schmidt :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
schmidt = lift_ugen_3 SC3.schmidt

scopeOut :: Monad m => Rate -> UGen m -> UGen m -> UGen m
scopeOut rate = lift_ugen_2 (SC3.scopeOut rate)

scopeOut2 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
scopeOut2 rate = lift_ugen_4 (SC3.scopeOut2 rate)

select :: Monad m => UGen m -> UGen m -> UGen m
select = lift_ugen_2 SC3.select

sendTrig :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
sendTrig = lift_ugen_3 SC3.sendTrig

setResetFF :: Monad m => UGen m -> UGen m -> UGen m
setResetFF = lift_ugen_2 SC3.setResetFF

shaper :: Monad m => UGen m -> UGen m -> UGen m
shaper = lift_ugen_2 SC3.shaper

sinOsc :: Monad m => Rate -> UGen m -> UGen m -> UGen m
sinOsc rate = lift_ugen_2 (SC3.sinOsc rate)

sinOscFB :: Monad m => Rate -> UGen m -> UGen m -> UGen m
sinOscFB rate = lift_ugen_2 (SC3.sinOscFB rate)

slew :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
slew = lift_ugen_3 SC3.slew

slope :: Monad m => UGen m -> UGen m
slope = lift_ugen_1 SC3.slope

specCentroid :: Monad m => Rate -> UGen m -> UGen m
specCentroid rate = lift_ugen_1 (SC3.specCentroid rate)

specFlatness :: Monad m => Rate -> UGen m -> UGen m
specFlatness rate = lift_ugen_1 (SC3.specFlatness rate)

specPcile :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
specPcile rate = lift_ugen_3 (SC3.specPcile rate)

spring :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
spring rate = lift_ugen_3 (SC3.spring rate)

standardL :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
standardL rate = lift_ugen_4 (SC3.standardL rate)

standardN :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
standardN rate = lift_ugen_4 (SC3.standardN rate)

stepper :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
stepper = lift_ugen_6 SC3.stepper

stereoConvolution2L :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
stereoConvolution2L rate = lift_ugen_6 (SC3.stereoConvolution2L rate)

subsampleOffset :: Monad m => UGen m
subsampleOffset = lift_ugen_0 SC3.subsampleOffset

sum3 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
sum3 = lift_ugen_3 SC3.sum3

sum4 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
sum4 = lift_ugen_4 SC3.sum4

sweep :: Monad m => UGen m -> UGen m -> UGen m
sweep = lift_ugen_2 SC3.sweep

syncSaw :: Monad m => Rate -> UGen m -> UGen m -> UGen m
syncSaw rate = lift_ugen_2 (SC3.syncSaw rate)

t2a :: Monad m => UGen m -> UGen m -> UGen m
t2a = lift_ugen_2 SC3.t2a

t2k :: Monad m => UGen m -> UGen m
t2k = lift_ugen_1 SC3.t2k

tBall :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
tBall rate = lift_ugen_4 (SC3.tBall rate)

tDelay :: Monad m => UGen m -> UGen m -> UGen m
tDelay = lift_ugen_2 SC3.tDelay

--tDuty :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--tDuty rate = lift_ugen_5 (SC3.tDuty rate)

tExpRand :: UId m => UGen m -> UGen m -> UGen m -> UGen m
tExpRand = lift_ugenM_3 SC3.tExpRandM

tGrains :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
tGrains numChannels = lift_ugen_8 (SC3.tGrains numChannels)

tiRand :: UId m => UGen m -> UGen m -> UGen m -> UGen m
tiRand = lift_ugenM_3 SC3.tiRandM

tRand :: UId m => UGen m -> UGen m -> UGen m -> UGen m
tRand = lift_ugenM_3 SC3.tRandM

tWindex :: UId m => UGen m -> UGen m -> UGen m -> UGen m
tWindex = lift_ugenM_3 SC3.tWindexM

timer :: Monad m => UGen m -> UGen m
timer = lift_ugen_1 SC3.timer

toggleFF :: Monad m => UGen m -> UGen m
toggleFF = lift_ugen_1 SC3.toggleFF

trig :: Monad m => UGen m -> UGen m -> UGen m
trig = lift_ugen_2 SC3.trig

trig1 :: Monad m => UGen m -> UGen m -> UGen m
trig1 = lift_ugen_2 SC3.trig1

trigControl :: Monad m => Rate -> UGen m -> UGen m
trigControl rate = lift_ugen_1 (SC3.trigControl rate)

twoPole :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
twoPole = lift_ugen_3 SC3.twoPole

twoZero :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
twoZero = lift_ugen_3 SC3.twoZero

unaryOpUGen :: Monad m => UGen m -> UGen m
unaryOpUGen = lift_ugen_1 SC3.unaryOpUGen

--vDiskIn :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--vDiskIn = lift_ugen_4 SC3.vDiskIn

vOsc :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
vOsc rate = lift_ugen_3 (SC3.vOsc rate)

vOsc3 :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
vOsc3 rate = lift_ugen_4 (SC3.vOsc3 rate)

varLag :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
varLag = lift_ugen_3 SC3.varLag

varSaw :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m
varSaw rate = lift_ugen_3 (SC3.varSaw rate)

--vibrato :: UId m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--vibrato rate = lift_ugenM_9 (SC3.vibratoM rate)

warp1 :: Monad m => Int -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
warp1 numChannels = lift_ugen_8 (SC3.warp1 numChannels)

whiteNoise :: UId m => Rate -> UGen m
whiteNoise rate = lift_ugenM_0 (SC3.whiteNoiseM rate)

wrap :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
wrap = lift_ugen_3 SC3.wrap

wrapIndex :: Monad m => UGen m -> UGen m -> UGen m
wrapIndex = lift_ugen_2 SC3.wrapIndex

xFade2 :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
xFade2 = lift_ugen_4 SC3.xFade2

--xLine :: Monad m => Rate -> UGen m -> UGen m -> UGen m -> UGen m -> UGen m
--xLine rate = lift_ugen_4 (SC3.xLine rate)

xOut :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
xOut = lift_ugen_3 SC3.xOut

zeroCrossing :: Monad m => UGen m -> UGen m
zeroCrossing = lift_ugen_1 SC3.zeroCrossing

maxLocalBufs :: Monad m => UGen m -> UGen m
maxLocalBufs = lift_ugen_1 SC3.maxLocalBufs

mulAdd :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
mulAdd = lift_ugen_3 SC3.mulAdd

setBuf :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
setBuf = lift_ugen_4 SC3.setBuf

