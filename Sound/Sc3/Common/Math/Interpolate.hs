-- | Interpolation functions, ie. for envelope segments.  Present naming is for qualified import.
module Sound.Sc3.Common.Math.Interpolate where

import Sound.Sc3.Common.Math {- hsc3 -}

{- | An interpolation function takes three arguments.
     x0 is the left or begin value, x1 is the right or end value, t is a (0,1) index.
-}
type Interpolation_f t = t -> t -> t -> t

-- | Clip x to (0,1) and run f.
--
-- > interpolate linear (-1,1) 0.5 == 0
interpolate :: (Num t,Ord t) => Interpolation_f t -> (t,t) -> t -> t
interpolate f (l,r) x = if x < 0 then l else if x > 1 then r else f l r x

-- | Step function, ignores t and returns x1.
step :: Interpolation_f t
step _ x1 _ = x1

{- | Linear interpolation funtion.

> map (linear 1 10) [0,0.25 .. 1] == [1,3.25,5.5,7.75,10]

> import Sound.Sc3.Plot {- hsc3-plot -}
> plot_fn_r1_ln (linear (-1) 1) (0,1)
-}
linear :: Num t => Interpolation_f t
linear x0 x1 t = t * (x1 - x0) + x0

{- | Exponential interpolation.
     x0 must not be zero and (x0,x1) must not span zero.

> plot_fn_r1_ln (exponential 0.001 1) (0,1)
> plot_fn_r1_ln (exponential 1 2) (0,1)
> plot_fn_r1_ln (exponential 20 20000) (0,1)
-}
exponential :: Floating t => Interpolation_f t
exponential x0 x1 t = x0 * ((x1 / x0) ** t)

{- | Variant that allows x0 to be zero, though (x0,x1) must not span zero.

> plot_fn_r1_ln (exponential_0 0 1) (0,1)
> plot_fn_r1_ln (exponential_0 0 (-1)) (0,1)
-}
exponential_0 :: (Eq t,Floating t) => Interpolation_f t
exponential_0 x0 x1 =
    let epsilon = 1e-6
        x0' = if x0 == 0 then epsilon * signum x1 else x0
    in exponential x0' x1

-- | 'linear' of 'exponential_0' of (0,1), ie. allows (x0,x1) to span zero.
--
-- > plot_fn_r1_ln (exponential_lin (-1) 1) (0,1)
exponential_lin :: (Eq t,Floating t) => Interpolation_f t
exponential_lin x0 x1 t = linear x0 x1 (exponential_0 0 1 t)

-- | 'linear' with t transformed by sine function over (-pi/2,pi/2).
--
-- > plot_fn_r1_ln (sine (-1) 1) (0,1)
sine :: Floating t => Interpolation_f t
sine x0 x1 t =
    let t' = - cos (pi * t) * 0.5 + 0.5
    in linear x0 x1 t'

-- | If x0 '<' x1 rising sine segment (0,pi/2), else falling segment (pi/2,pi).
--
-- > plot_fn_r1_ln (welch (-1) 1) (0,1)
-- > plot_fn_r1_ln (welch 1 (-1)) (0,1)
welch :: (Ord t, Floating t) => Interpolation_f t
welch x0 x1 t =
    if x0 < x1
    then x0 + (x1 - x0) * sin (half_pi * t)
    else x1 - (x1 - x0) * sin (half_pi - (half_pi * t))

{- | Curvature controlled by single parameter c.
     Zero is 'linear', increasing c approaches 'exponential' and continues past it.
     The value for c at which the curve is close to exponential depends on the range.

> plot_p1_ln (map (\c-> map (curve c (-1) 1) [0,0.01 .. 1]) [-6,-4 .. 6])
> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [curve 4.4 1 100,exponential 1 100,curve 4.5 1 100])
> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [exponential 20 20000,curve 7 20 20000])
> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [fader 0 2,curve 2 0 2])
-}
curve :: (Ord t, Floating t) => t -> Interpolation_f t
curve c x0 x1 t =
    if abs c < 0.0001
    then linear x0 x1 t
    else let d = 1 - exp c
             n = 1 - exp (t * c)
         in x0 + (x1 - x0) * (n/d)

{- | Square of 'linear' of 'sqrt' of x0 and x1, therefore neither may be negative.

> plot_fn_r1_ln (squared 0 1) (0,1)
> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [curve 2.05 0 1,squared 0 1])
-}
squared :: Floating t => Interpolation_f t
squared x0 x1 t =
    let x0' = sqrt x0
        x1' = sqrt x1
        l = linear x0' x1' t
    in l * l

{- | Cubic variant of 'squared'.

> plot_fn_r1_ln (cubed 0 1) (0,1)
> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [curve 3.25 0 1,cubed 0 1])
-}
cubed :: Floating t => Interpolation_f t
cubed x0 x1 t =
    let x0' = x0 ** (1/3)
        x1' = x1 ** (1/3)
        l = linear x0' x1' t
    in l * l * l

-- | x0 until end, then immediately x1.
--
-- > plot_fn_r1_ln (hold 0 1) (0,2)
hold :: (Num t,Ord t) => Interpolation_f t
hold x0 x1 t = if t >= 1 then x1 else x0

{- | Fader curve, equal to 'squared' when x1 > x0.

> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [squared 0 1,fader 0 1])
> plot_p1_ln (map (\f -> map f [0,0.01 .. 1]) [curve 2 1 0,fader 1 0])
-}
fader :: (Num t,Ord t) => Interpolation_f t
fader x0 x1 t =
  let rng = x1 - x0
      sqr i = i * i
  in sqr (if rng > 0 then t else 1 - (1 - t)) * rng + x0
