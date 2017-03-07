-- | Functions to generate break point data for standard envelope
--   types.
module Sound.SC3.UGen.Envelope.Construct where

import Sound.SC3.UGen.Bindings
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Envelope
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

{- | Co-ordinate based static envelope generator.  Points are (time,value) pairs.

> let e = envCoord [(0,0),(1/4,1),(1,0)] 1 1 EnvLin
> in envelope_sc3_array e == Just [0,2,-99,-99,1,1/4,1,0,0,3/4,1,0]

> import Sound.SC3.Plot {- hsc3-plot -}

> plotEnvelope [envCoord [(0,0),(1/4,1),(1,0)] 1 1 EnvLin]

-}
envCoord :: Num a => [(a,a)] -> a -> a -> Envelope_Curve a -> Envelope a
envCoord bp dur amp c =
    let l = map ((* amp) . snd) bp
        t = map (* dur) (tail (d_dx (map fst bp)))
    in Envelope l t [c] Nothing Nothing

{- | Trapezoidal envelope generator.

The arguments are: 1. @shape@ determines the sustain time as a
proportion of @dur@, zero is a triangular envelope, one a rectangular
envelope; 2. @skew@ determines the attack\/decay ratio, zero is an
immediate attack and a slow decay, one a slow attack and an immediate
decay; 3. @duration@ in seconds; 4. @amplitude@ as linear gain.

> plotEnvelope [envTrapezoid 0.99 0.5 1 1]

-}
envTrapezoid :: OrdE a => a -> a -> a -> a -> Envelope a
envTrapezoid shape skew dur amp =
    let x1 = skew * (1 - shape)
        bp = [(0,skew <=* 0)
             ,(x1,1)
             ,(shape + x1,1)
             ,(1,skew >=* 1)]
    in envCoord bp dur amp EnvLin

-- | Variant 'envPerc' with user specified 'Envelope_Curve a'.
envPerc' :: Num a => a -> a -> a -> Envelope_Curve2 a -> Envelope a
envPerc' atk rls lvl (c0,c1) =
    let c = [c0,c1]
    in Envelope [0,lvl,0] [atk,rls] c Nothing Nothing

-- | Percussive envelope, with attack, release, level and curve
--   inputs.
envPerc :: Num a => a -> a -> Envelope a
envPerc atk rls =
    let cn = EnvNum (-4)
    in envPerc' atk rls 1 (cn,cn)

-- | Triangular envelope, with duration and level inputs.
--
-- > let e = envTriangle 1 0.1
-- > in envelope_sc3_array e = Just [0,2,-99,-99,0.1,0.5,1,0,0,0.5,1,0]
envTriangle :: Fractional a => a -> a -> Envelope a
envTriangle dur lvl =
    let c = replicate 2 EnvLin
        d = replicate 2 (dur / 2)
    in Envelope [0,lvl,0] d c Nothing Nothing

-- | Sine envelope, with duration and level inputs.
--
-- > let e = envSine 0 0.1
-- > in envelope_sc3_array e == Just [0,2,-99,-99,0.1,0,3.0,0,0,0,3,0]
envSine :: Fractional a => a -> a -> Envelope a
envSine dur lvl =
    let c = replicate 2 EnvSin
        d = replicate 2 (dur / 2)
    in Envelope [0,lvl,0] d c Nothing Nothing

-- | Parameters for LINEN envelopes.
data LINEN a = LINEN {linen_attackTime :: a
                     ,linen_sustainTime :: a
                     ,linen_releaseTime :: a
                     ,linen_level :: a
                     ,linen_curve :: Envelope_Curve3 a}

-- | Record ('LINEN') variant of 'envLinen'.
envLinen_r :: Num a => LINEN a -> Envelope a
envLinen_r (LINEN aT sT rT lv (c0,c1,c2)) =
    let l = [0,lv,lv,0]
        t = [aT,sT,rT]
        c = [c0,c1,c2]
    in Envelope l t c Nothing Nothing

-- | Variant of 'envLinen' with user specified 'Envelope_Curve a'.
envLinen' :: Num a => a -> a -> a -> a -> Envelope_Curve3 a -> Envelope a
envLinen' aT sT rT lv c = envLinen_r (LINEN aT sT rT lv c)

-- | Linear envelope parameter constructor.
--
-- > let {e = envLinen 0 1 0 1
-- >     ;s = envelope_segments e
-- >     ;p = pack_envelope_segments s}
-- > in p == (env_levels e,env_times e,env_curves e)
envLinen :: Num a => a -> a -> a -> a -> Envelope a
envLinen aT sT rT l =
    let c = (EnvLin,EnvLin,EnvLin)
    in envLinen' aT sT rT l c

-- | Parameters for ADSR envelopes.  The sustain level is given as a proportion of the peak level.
data ADSR a = ADSR {adsr_attackTime :: a
                   ,adsr_decayTime :: a
                   ,adsr_sustainLevel :: a
                   ,adsr_releaseTime :: a
                   ,adsr_peakLevel :: a
                   ,adsr_curve :: Envelope_Curve3 a
                   ,adsr_bias :: a}

adsrDefault :: Fractional n => ADSR n
adsrDefault = let c = EnvNum (-4) in ADSR 0.01 0.3 0.5 1 1 (c,c,c) 0

-- | Attack, decay, sustain, release envelope parameter constructor.
envADSR :: Num a => a -> a -> a -> a -> a -> Envelope_Curve a -> a -> Envelope a
envADSR aT dT sL rT pL c b = envADSR_r (ADSR aT dT sL rT pL (c,c,c) b)

-- | Vairant with defaults for pL, c and b.
envADSR' :: Num a => a -> a -> a -> a -> Envelope a
envADSR' aT dT sL rT = envADSR aT dT sL rT 1 (EnvNum (-4)) 0

-- | Record ('ADSR') variant of 'envADSR'.
envADSR_r :: Num a => ADSR a -> Envelope a
envADSR_r (ADSR aT dT sL rT pL (c0,c1,c2) b) =
    let l = map (+ b) [0,pL,pL*sL,0]
        t = [aT,dT,rT]
        c = [c0,c1,c2]
    in Envelope l t c (Just 2) Nothing

-- | Parameters for Roland type ADSSR envelopes.
data ADSSR a = ADSSR {adssr_attackTime :: a
                     ,adssr_attackLevel :: a
                     ,adssr_decayTime :: a
                     ,adssr_decayLevel :: a
                     ,adssr_slopeTime :: a
                     ,adssr_sustainLevel :: a
                     ,adssr_releaseTime :: a
                     ,adssr_curve :: Envelope_Curve4 a
                     ,adssr_bias :: a}

-- | Attack, decay, slope, sustain, release envelope parameter constructor.
envADSSR :: Num a => a -> a -> a -> a -> a -> a -> a -> Envelope_Curve a -> a -> Envelope a
envADSSR t1 l1 t2 l2 t3 l3 t4 c b = envADSSR_r (ADSSR t1 l1 t2 l2 t3 l3 t4 (c,c,c,c) b)

-- | Record ('ADSSR') variant of 'envADSSR'.
envADSSR_r :: Num a => ADSSR a -> Envelope a
envADSSR_r (ADSSR t1 l1 t2 l2 t3 l3 t4 (c1,c2,c3,c4) b) =
    let l = map (+ b) [0,l1,l2,l3,0]
        t = [t1,t2,t3,t4]
        c = [c1,c2,c3,c4]
    in Envelope l t c (Just 3) Nothing

-- | Parameters for ASR envelopes.
data ASR a = ASR {asr_attackTime :: a
                 ,asr_sustainLevel :: a
                 ,asr_releaseTime :: a
                 ,asr_curve :: Envelope_Curve2 a}

-- | Attack, sustain, release envelope parameter constructor.
--
-- > let {c = 3
-- >     ;r = Just [0,2,1,-99,0.1,3,c,0,0,2,c,0]}
-- > in envelope_sc3_array (envASR 3 0.1 2 EnvSin) == r
envASR :: Num a => a -> a -> a -> Envelope_Curve a -> Envelope a
envASR aT sL rT c = envASR_r (ASR aT sL rT (c,c))

-- | Record ('ASR') variant of 'envASR'.
envASR_r :: Num a => ASR a -> Envelope a
envASR_r (ASR aT sL rT (c0,c1)) =
    let l = [0,sL,0]
        t = [aT,rT]
        c' = [c0,c1]
    in Envelope l t c' (Just 1) Nothing

-- | All segments are horizontal lines.
envStep :: [a] -> [a] -> Maybe Int -> Maybe Int -> Envelope a
envStep levels times releaseNode loopNode =
    if length levels /= length times
    then error "envStep: levels and times must have same size"
    else let levels' = head levels : levels
         in Envelope levels' times [EnvStep] releaseNode loopNode

-- | Singleton fade envelope.
envGate :: UGen -> UGen -> UGen -> DoneAction -> Envelope_Curve UGen -> UGen
envGate level gate_ fadeTime doneAction curve =
    let startVal = fadeTime <=* 0
        e = Envelope [startVal,1,0] [1,1] [curve] (Just 1) Nothing
    in envGen KR gate_ level 0 fadeTime doneAction e

-- | Variant with default values for all inputs.  @gate@ and
-- @fadeTime@ are 'control's, @doneAction@ is 'RemoveSynth', @curve@
-- is 'EnvSin'.
envGate' :: UGen
envGate' =
    let level = 1
        gate_ = meta_control KR "gate" 1 (0,1,"lin",1,"")
        fadeTime = meta_control KR "fadeTime" 0.02 (0,10,"lin",0,"s")
        doneAction = RemoveSynth
        curve = EnvSin
    in envGate level gate_ fadeTime doneAction curve
