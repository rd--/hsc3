-- | Noise generating functions.
module Sound.SC3.Common.Math.Noise where

import Sound.SC3.Common.Math {- hsc3 -}

-- | x(n+1) = a - b * sqrt(|x(n)|)
cusp_f :: Floating t => t -> t -> t -> t
cusp_f a b x = a - (b * sqrt (abs x))

-- | x(n+1) = sin(im * y(n) + fb * x(n))
--   y(n+1) = (a * y(n) + c) % 2pi
fbSine_f :: (Floating t, RealFrac t) => t -> t -> t -> t -> (t, t) -> (t, t)
fbSine_f im fb a c (x,y) = (sin ((im * y) + (fb * x)),((a * y) + c) `sc3_mod` (2 * pi))

-- | x(n+2) = 1 - a * x(n+1)^2 + b * x(n)
henon_f :: Floating t => t -> t -> (t, t) -> (t, t)
henon_f a b (x1,x0) = (1 - (a * (x1 ** 2)) + (b * x0),x1)

-- | x(n+1) = sin(b * y(n)) + c * sin(b * x(n))
--   y(n+1) = sin(a * x(n)) + d * sin(a * y(n))
latoocarfian_f :: Floating t => t -> t -> t -> t -> (t, t) -> (t, t)
latoocarfian_f a b c d (x,y) = (sin(b * y) + (c * sin (b * x)),sin(a * x) + (d * sin (a * y)))
