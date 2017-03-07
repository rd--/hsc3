-- | Interpolation functions, ie. for envelope segments.
module Sound.SC3.Common.Math.Interpolate where

import Sound.SC3.Common.Math

-- | An interpolation function takes three arguments. /x0/ is the left
-- or begin value, /x1/ is the right or end value, and /t/ is a (0,1)
-- index.
type Interpolation_F t = t -> t -> t -> t

-- | Step function, ignores /t/ and returns /x1/.
step :: Interpolation_F t
step _ x1 _ = x1

-- | Linear interpolation.
--
-- > import Sound.SC3.Plot {- hsc3-plot -}
-- > plotTable1 (map (linear (-1) 1) [0,0.01 .. 1])
linear :: Num t => Interpolation_F t
linear x0 x1 t = t * (x1 - x0) + x0

-- | Exponential interpolation, /x0/ must not be @0@, (/x0/,/x1/) must
-- not span @0@.
--
-- > plotTable1 (map (exponential 0.001 1) [0,0.01 .. 1])
exponential :: Floating t => Interpolation_F t
exponential x0 x1 t = x0 * ((x1 / x0) ** t)

-- | Variant that allows /x0/ to be @0@, though (/x0/,/x1/) must not
-- span @0@.
--
-- > plotTable1 (map (exponential' 0 1) [0,0.01 .. 1])
-- > plotTable1 (map (exponential' 0 (-1)) [0,0.01 .. 1])
exponential' :: (Eq t,Floating t) => Interpolation_F t
exponential' x0 x1 =
    let epsilon = 1e-6
        x0' = if x0 == 0 then epsilon * signum x1 else x0
    in exponential x0' x1

-- | 'linear' of 'exponential'', ie. allows (/x0/,/x1/) to span @0@.
--
-- > plotTable1 (map (exponential'' (-1) 1) [0,0.01 .. 1])
exponential'' :: (Eq t,Floating t) => Interpolation_F t
exponential'' x0 x1 t = linear x0 x1 (exponential' 0 1 t)

-- | 'linear' with /t/ transformed by sine function over (-pi/2,pi/2).
--
-- > plotTable1 (map (sine (-1) 1) [0,0.01 .. 1])
sine :: Floating t => Interpolation_F t
sine x0 x1 t =
    let t' = - cos (pi * t) * 0.5 + 0.5
    in linear x0 x1 t'

-- | If /x0/ '<' /x1/ rising sine segment (0,pi/2), else falling
-- segment (pi/2,pi).
--
-- > plotTable1 (map (welch (-1) 1) [0,0.01 .. 1])
-- > plotTable1 (map (welch 1 (-1)) [0,0.01 .. 1])
welch :: (Ord t, Floating t) => Interpolation_F t
welch x0 x1 t =
    if x0 < x1
    then x0 + (x1 - x0) * sin (half_pi * t)
    else x1 - (x1 - x0) * sin (half_pi - (half_pi * t))

-- | Curvature controlled by single parameter /c/.  @0@ is 'linear',
-- increasing /c/ approaches 'exponential'.
--
-- > plotTable (map (\c-> map (curve c (-1) 1) [0,0.01 .. 1]) [0 .. 25])
curve :: (Ord t, Floating t) => t -> Interpolation_F t
curve c x0 x1 t =
    if abs c < 0.0001
    then t * (x1 - x0) + x0
    else let d = 1 - exp c
             n = 1 - exp (t * c)
         in x0 + (x1 - x0) * (n/d)

-- | Square of 'linear' of 'sqrt' of /x0/ and /x1/, therefore neither
-- may be negative.
--
-- > plotTable1 (map (squared 0 1) [0,0.01 .. 1])
squared :: Floating t => Interpolation_F t
squared x0 x1 t =
    let x0' = sqrt x0
        x1' = sqrt x1
        l = linear x0' x1' t
    in l * l

-- | Cubic variant of 'squared'.
--
-- > plotTable1 (map (cubed 0 1) [0,0.01 .. 1])
cubed :: Floating t => Interpolation_F t
cubed x0 x1 t =
    let x0' = x0 ** (1/3)
        x1' = x1 ** (1/3)
        l = linear x0' x1' t
    in l * l * l

-- | x0 until end, then immediately x1.
--
-- > plotTable1 (map (hold 0 1) [0,0.01 .. 1])
hold :: (Num t,Ord t) => Interpolation_F t
hold x0 x1 t = if t >= 1 then x1 else x0
