-- | Functions to generate break point data for standard envelope
--   types.
module Sound.SC3.UGen.Envelope.Construct (Envelope(..)
                                         ,envelope_sc3_array
                                         ,envCoord
                                         ,envTrapezoid
                                         ,envPerc', envPerc
                                         ,envTriangle
                                         ,envSine
                                         ,envLinen', envLinen
                                         ,envADSR
                                         ,envADSR_r,ADSR(..)
                                         ,envASR) where

import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Envelope

-- | Co-ordinate based static envelope generator.
envCoord :: Num a => [(a,a)] -> a -> a -> Envelope_Curve a -> Envelope a
envCoord bp dur amp c =
    let l = map ((* amp) . snd) bp
        t = map (* dur) (d_dx (map fst bp))
    in Envelope l t [c] Nothing Nothing

-- | Trapezoidal envelope generator.  The arguments are: 1. @shape@
-- determines the sustain time as a proportion of @dur@, zero is a
-- triangular envelope, one a rectangular envelope; 2. @skew@
-- determines the attack\/decay ratio, zero is an immediate attack and
-- a slow decay, one a slow attack and an immediate decay;
-- 3. @duration@ in seconds; 4. @amplitude@ as linear gain.
envTrapezoid :: (Num a,OrdE a) => a -> a -> a -> a -> Envelope a
envTrapezoid shape skew dur amp =
    let x1 = skew * (1 - shape)
        bp = [ (0, skew <=* 0)
             , (x1, 1)
             , (shape + x1, 1)
             , (1, skew >=* 1) ]
    in envCoord bp dur amp EnvLin

-- | Variant 'envPerc' with user specified 'Envelope_Curve a'.
envPerc' :: Num a => a -> a -> a -> (Envelope_Curve a,Envelope_Curve a) -> Envelope a
envPerc' atk rls lvl (c0, c1) =
    let c = [c0, c1]
    in Envelope [0, lvl, 0] [atk, rls] c Nothing Nothing

-- | Percussive envelope, with attack, release, level and curve
--   inputs.
envPerc :: Num a => a -> a -> Envelope a
envPerc atk rls =
    let cn = EnvNum (-4)
    in envPerc' atk rls 1 (cn, cn)

-- | Triangular envelope, with duration and level inputs.
envTriangle :: (Num a,Fractional a) => a -> a -> Envelope a
envTriangle dur lvl =
    let c = replicate 2 EnvLin
        d = replicate 2 (dur / 2)
    in Envelope [0,lvl,0] d c Nothing Nothing

-- | Sine envelope, with duration and level inputs.
envSine :: (Num a,Fractional a) => a -> a -> Envelope a
envSine dur lvl =
    let c = replicate 2 EnvSin
        d = replicate 2 (dur / 2)
    in Envelope [0,lvl,0] d c Nothing Nothing

-- | Variant of 'envLinen' with user specified 'Envelope_Curve a'.
envLinen' :: Num a => a -> a -> a -> a -> (Envelope_Curve a,Envelope_Curve a,Envelope_Curve a) -> Envelope a
envLinen' aT sT rT l (c0, c1, c2) =
    Envelope [0, l, l, 0] [aT, sT, rT] [c0, c1, c2] Nothing Nothing

-- | Linear envelope parameter constructor.
envLinen :: Num a => a -> a -> a -> a -> Envelope a
envLinen aT sT rT l =
    let c = (EnvLin, EnvLin, EnvLin)
    in envLinen' aT sT rT l c

-- | Parameters for ADSR envelopes.
data ADSR a = ADSR {attackTime :: a
                   ,decayTime :: a
                   ,sustainLevel :: a
                   ,releaseTime :: a
                   ,peakLevel :: a
                   ,curve :: (Envelope_Curve a,Envelope_Curve a,Envelope_Curve a)
                   ,bias :: a}

-- | Attack, decay, sustain, release envelope parameter constructor.
envADSR :: Num a => a -> a -> a -> a -> a -> Envelope_Curve a -> a -> Envelope a
envADSR aT dT sL rT pL c b = envADSR_r (ADSR aT dT sL rT pL (c,c,c) b)

-- | Record ('ADSR') variant of 'envADSR'.
envADSR_r :: Num a => ADSR a -> Envelope a
envADSR_r (ADSR aT dT sL rT pL (c0,c1,c2) b) =
    let l = map (+ b) [0,pL,pL*sL,0]
        t = [aT,dT,rT]
        c = [c0,c1,c2]
    in Envelope l t c (Just 2) Nothing

-- | Attack, sustain, release envelope parameter constructor.
envASR :: Num a => a -> a -> a -> Envelope_Curve a -> Envelope a
envASR aT sL rT c =
    let l = [0,sL,0]
        t = [aT,rT]
        c' = [c,c]
    in Envelope l t c' (Just 1) Nothing

d_dx :: (Num a) => [a] -> [a]
d_dx xs = zipWith (-) (drop 1 xs) xs
