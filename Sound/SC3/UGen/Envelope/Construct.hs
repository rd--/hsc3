-- | Functions to generate break point data for standard envelope
--   types.
module Sound.SC3.UGen.Envelope.Construct (env
                                         ,envCoord
                                         ,envTrapezoid
                                         ,envPerc', envPerc
                                         ,envTriangle
                                         ,envSine
                                         ,envLinen', envLinen
                                         ,envADSR
                                         ,envADSR_r,ADSR(..)
                                         ,envASR
                                         ,env_curve,env_value) where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Enum

-- | Basic envelope data constructor.  The curve argument are cycled
--   if required.
env :: [UGen] -> [UGen] -> [EnvCurve] -> UGen -> UGen -> [UGen]
env [] _   _   _   _  = error "env: illegal specification"
env (l:ls) tms crv rls lp =
    let f l' t c = [l', t, env_curve c, env_value c]
        n = length tms
        n' = fromIntegral n
        crv' = take n (cycle crv)
    in [l, n', rls, lp] ++ concat (zipWith3 f ls tms crv')

-- | Co-ordinate based static envelope generator.
envCoord :: [(UGen, UGen)] -> UGen -> UGen -> EnvCurve -> [UGen]
envCoord bp dur amp c =
    let l = map ((* amp) . snd) bp
        t = map (* dur) (d_dx (map fst bp))
    in env l t (repeat c) (-1) (-1)

-- | Trapezoidal envelope generator.  The arguments are: 1. @shape@
-- determines the sustain time as a proportion of @dur@, zero is a
-- triangular envelope, one a rectangular envelope; 2. @skew@
-- determines the attack\/decay ratio, zero is an immediate attack and
-- a slow decay, one a slow attack and an immediate decay;
-- 3. @duration@ in seconds; 4. @amplitude@ as linear gain.
envTrapezoid :: UGen -> UGen -> UGen -> UGen -> [UGen]
envTrapezoid shape skew dur amp =
    let x1 = skew * (1 - shape)
        bp = [ (0, skew <=* 0)
             , (x1, 1)
             , (shape + x1, 1)
             , (1, skew >=* 1) ]
    in envCoord bp dur amp EnvLin

-- | Variant 'envPerc' with user specified 'EnvCurve'.
envPerc' :: UGen -> UGen -> UGen -> (EnvCurve, EnvCurve) -> [UGen]
envPerc' atk rls lvl (c0, c1) =
    let c = [c0, c1]
    in env [0.0, lvl, 0.0] [atk, rls] c (-1.0) (-1.0)

-- | Percussive envelope, with attack, release, level and curve
--   inputs.
envPerc :: UGen -> UGen -> [UGen]
envPerc atk rls =
    let cn = EnvNum (-4.0)
    in envPerc' atk rls 1.0 (cn, cn)

-- | Triangular envelope, with duration and level inputs.
envTriangle :: UGen -> UGen -> [UGen]
envTriangle dur lvl =
    let c = replicate 2 EnvLin
        d = replicate 2 (dur / 2.0)
    in env [0.0, lvl, 0.0] d c (-1.0) (-1.0)

-- | Sine envelope, with duration and level inputs.
envSine :: UGen -> UGen -> [UGen]
envSine dur lvl =
    let c = replicate 2 EnvSin
        d = replicate 2 (dur / 2.0)
    in env [0.0, lvl, 0.0] d c (-1.0) (-1.0)

-- | Variant of 'envLinen' with user specified 'EnvCurve'.
envLinen' :: UGen -> UGen -> UGen -> UGen -> (EnvCurve, EnvCurve, EnvCurve) -> [UGen]
envLinen' aT sT rT l (c0, c1, c2) =
    env [0, l, l, 0] [aT, sT, rT] [c0, c1, c2] (-1) (-1)

-- | Linear envelope parameter constructor.
envLinen :: UGen -> UGen -> UGen -> UGen -> [UGen]
envLinen aT sT rT l =
    let c = (EnvLin, EnvLin, EnvLin)
    in envLinen' aT sT rT l c

-- | Parameters for ADSR envelopes.
data ADSR a = ADSR {attackTime :: a
                   ,decayTime :: a
                   ,sustainLevel :: a
                   ,releaseTime :: a
                   ,peakLevel :: a
                   ,curve :: (EnvCurve,EnvCurve,EnvCurve)
                   ,bias :: a}

-- | Attack, decay, sustain, release envelope parameter constructor.
envADSR :: UGen -> UGen -> UGen -> UGen -> UGen -> EnvCurve -> UGen -> [UGen]
envADSR aT dT sL rT pL c b = envADSR_r (ADSR aT dT sL rT pL (c,c,c) b)

-- | Record ('ADSR') variant of 'envADSR'.
envADSR_r :: ADSR UGen -> [UGen]
envADSR_r (ADSR aT dT sL rT pL (c0,c1,c2) b) =
    let l = map (+ b) [0,pL,pL*sL,0]
        t = [aT,dT,rT]
        c = [c0,c1,c2]
    in env l t c 2 (-1)

-- | Attack, sustain, release envelope parameter constructor.
envASR :: UGen -> UGen -> UGen -> EnvCurve -> [UGen]
envASR aT sL rT c =
    let l = [0,sL,0]
        t = [aT,rT]
        c' = [c,c]
    in env l t c' 1 (-1)

d_dx :: (Num a) => [a] -> [a]
d_dx xs = zipWith (-) (drop 1 xs) xs

-- | Convert 'EnvCurve' to constant 'UGen' value.
env_curve :: EnvCurve -> UGen
env_curve e =
    case e of
      EnvStep -> Constant 0.0
      EnvLin -> Constant 1.0
      EnvExp -> Constant 2.0
      EnvSin -> Constant 3.0
      EnvCos -> Constant 4.0
      EnvNum _ -> Constant 5.0
      EnvSqr -> Constant 6.0
      EnvCub -> Constant 7.0

-- | The /value/ of 'EnvCurve' is non-zero for 'EnvNum'.
env_value :: EnvCurve -> UGen
env_value e =
    case e of
      EnvNum u -> u
      _ -> Constant 0.0
