-- | Interpolation function for envelope segments.  Each function
-- takes three arguments, /x0/ is the left or begin value, /x1/ is the
-- right or end value, and /t/ is a (0,1) index.
module Sound.SC3.UGen.Envelope.Interpolate where

type Interpolation_F t = t -> t -> t -> t

step :: Interpolation_F t
step _ x1 _ = x1

linear :: Num t => Interpolation_F t
linear x0 x1 t = t * (x1 - x0) + x0

exponential :: Floating t => Interpolation_F t
exponential x0 x1 t = x0 * ((x1 / x0) ** t)

sine :: Floating t => Interpolation_F t
sine x0 x1 t = x0 + (x1 - x0) * (- cos (pi * t) * 0.5 + 0.5)

half_pi :: Floating a => a
half_pi = pi / 2

welch :: (Ord t, Floating t) => Interpolation_F t
welch x0 x1 t =
    if x0 < x1
    then x0 + (x1 - x0) * sin (half_pi * t)
    else x1 - (x1 - x0) * sin (half_pi - (half_pi * t))

curve :: (Ord t, Floating t) => t -> Interpolation_F t
curve c x0 x1 t =
    if abs c < 0.0001
    then t * (x1 - x0) + x0
    else let d = 1 - exp c
             n = 1 - exp (t * c)
         in x0 + (x1 - x0) * (n/d)

squared :: Floating t => Interpolation_F t
squared x0 x1 t =
    let x0' = sqrt x0
        x1' = sqrt x1
        l = t * (x1' - x0') + x0'
    in l * l

cubed :: Floating t => Interpolation_F t
cubed x0 x1 t =
    let x0' = x0 ** (1/3)
        x1' = x1 ** (1/3)
        l = t * (x1' - x0') + x0'
    in l * l * l

