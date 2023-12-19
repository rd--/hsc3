-- | A warp is a mapping from the space @[0,1]@ to a user defined space @[l,r]@.
module Sound.Sc3.Common.Math.Warp where

import Numeric {- base -}

import qualified Sound.Sc3.Common.Math as Math {- hsc3 -}

-- | A warp function is lhs -> rhs -> x -> y
type Warp_f t = t -> t -> t -> t

{- | Linear real value map.

>>> map (warp_lin 1 2) [0,1/2,1] == [1,3/2,2]
True

>>> map (warp_lin (-1) 1) [0,1/2,1] == [-1,0,1]
True
-}
warp_lin :: Fractional t => Warp_f t
warp_lin l r n = let z = r - l in n * z + l

{- | Inverse of 'warp_lin'

>>> map (warp_lin_inv 1 2) [1,3/2,2] == [0,1/2,1]
True

>>> map (warp_lin_inv (-1) 1) [-1,0,1] == [0,1/2,1]
True
-}
warp_lin_inv :: Fractional t => Warp_f t
warp_lin_inv l r n = let z = r - l in (n - l) / z

{- | The left and right must both be non zero and have the same sign.

>>> map (warp_exp 1 2) [0,0.5,1] == [1,2 ** 0.5,2]
True

> import Sound.Sc3.Plot {\- hsc3-plot -\}
> plot_p1_ln [map (warp_exp 1 2) [0,0.01 .. 1]]
-}
warp_exp :: Floating a => Warp_f a
warp_exp l r n = let z = r / l in (z ** n) * l

warp_exp_inv :: Floating a => Warp_f a
warp_exp_inv l r n = let z = r / l in logBase z (n / l)

{- | Cosine warp

> map (warp_cos 1 2) [0,0.25,0.5,0.75,1]
> plot_p1_ln [map (warp_cos 1 2) [0,0.01 .. 1]]
-}
warp_cos :: Floating t => Warp_f t
warp_cos l r n = warp_lin 0 (r - l) (0.5 - (cos (pi * n) / 2))

warp_cos_inv :: Floating a => Warp_f a
warp_cos_inv l r n = acos (1.0 - (warp_lin_inv 0 (r - l) n * 2)) / pi

{- | Sine warp

> map (warp_sin 1 2) [0,0.25,0.5,0.75,1]
> plot_p1_ln [map (warp_sin 1 2) [0,0.01 .. 1]]
-}
warp_sin :: Floating t => Warp_f t
warp_sin l r n = warp_lin 0 (r - l) (sin (pi * 0.5 * n))

warp_sin_inv :: Floating t => Warp_f t
warp_sin_inv l r n = asin (warp_lin_inv 0 (r - l) n) / (pi / 2)

{- | Fader warp.  Left and right values are ordinarily zero and one.

> map (warp_amp 0 1) [0,0.5,1] == [0,0.25,1]

> plot_p1_ln [map (warp_amp 0 2) [0,0.01 .. 1]]
> plot_p1_ln [map (warp_amp_inv 0 1 . warp_amp 0 1) [0,0.01 .. 1]]
-}
warp_amp :: Num a => Warp_f a
warp_amp l r n = (n * n) * (r - l) + l

warp_amp_inv :: Floating a => Warp_f a
warp_amp_inv l r n = sqrt ((n - l) / (r - l))

{- | DB fader warp. Left and right values are ordinarily negative
infinity and zero.  An input of @0@ gives @-180@.

> map (round . warp_db (-180) 0) [0,0.5,1] == [-180,-12,0]

> plot_p1_ln [map (warp_db (-60) 0) [0,0.01 .. 1]]
> plot_p1_ln [map (warp_db_inv 0 60) [0 .. 60]]
-}
warp_db :: (Eq a, Floating a) => Warp_f a
warp_db l r n =
  let n' = if n == 0 then -180 else Math.amp_to_db (n * n)
  in Math.sc3_linlin n' (-180) 0 l r

warp_db_inv :: Floating a => Warp_f a
warp_db_inv l r n = sqrt (Math.db_to_amp (Math.sc3_linlin n l r (-180) 0))

{- | A curve warp given by a real /n/.

> warp_curve (-3) 1 2 0.25 == 1.5552791692202022
> warp_curve (-3) 1 2 0.50 == 1.8175744761936437

> plot_p1_ln [map (warp_curve (-3) 1 2) [0,0.01 .. 1]]
> plot_p1_ln (map (\c -> map (warp_curve c 1 2) [0,0.01 .. 1]) [0,3,6,9])
> plot_p1_ln [map (warp_curve_inv 7 20 20000 . warp_curve 7 20 20000) [0,0.01 .. 1]]
-}
warp_curve :: (Ord a, Floating a) => a -> Warp_f a
warp_curve k l r n =
  if abs k < 0.001
    then warp_lin l r n
    else
      let e = exp k
          a = (r - l) / (1 - e)
          b = l + a
      in b - ((e ** n) * a)

warp_curve_inv :: (Ord a, Floating a) => a -> Warp_f a
warp_curve_inv k l r n =
  if abs k < 0.001
    then warp_lin l r n
    else
      let e = exp k
          a = (r - l) / (1 - e)
          b = l + a
      in log ((b - n) / a) / k

{- | Select warp functions by name.  Numerical names are interpreted as /curve/ values for 'warpCurve'.

> let Just w = warp_named "lin"
> let Just w = warp_named "-3"
> let Just w = warp_named "6"
> plot_p1_ln [map ((fst w) 1 2) [0,0.01 .. 1]]
-}
warp_named :: (Floating t, RealFrac t) => String -> Maybe (Warp_f t, Warp_f t)
warp_named nm =
  case nm of
    "lin" -> Just (warp_lin, warp_lin_inv)
    "exp" -> Just (warp_exp, warp_exp_inv)
    "sin" -> Just (warp_sin, warp_sin_inv)
    "cos" -> Just (warp_cos, warp_cos_inv)
    "amp" -> Just (warp_amp, warp_amp_inv)
    "db" -> Just (warp_db, warp_db_inv)
    _ -> case readSigned readFloat nm of
      [(c, "")] -> Just (warp_curve c, warp_curve_inv c)
      _ -> Nothing
