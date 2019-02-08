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

-- | x(n+1) = (a * x(n) + c) % m
linCong_f :: RealFrac t => t -> t -> t -> t -> t
linCong_f a c m x = (a * x + c) `sc3_mod` m

-- | x(n+1) = a * x * (1.0 - x)
logistic_f :: Num t => t -> t -> t
logistic_f a x = a * x * (1 - x)

-- | x' = s * (y - x)
--   y' = x * (r - z) - y
--   z' = x * y - b * z
lorenz_f :: Num t => t -> t -> t -> (t, t, t) -> (t, t, t)
lorenz_f s r b (x,y,z) = (s * (y - x),x * (r - z) - y,x * y - b * z)

-- | x(n+1) = a * x(n)^2 + b * x(n) + c
quad_f :: Floating t => t -> t -> t -> t -> t
quad_f a b c x = (a * (x ** 2)) + (b * x) + c

-- | x(n+1) = (x(n) + y(n+1)) % 2pi
--   y(n+1) = (y(n) + k * sin(x(n))) % 2pi
standard_f :: (RealFrac t, Floating t) => t -> (t, t) -> (t, t)
standard_f k (x,y) =
  let y' = (y + (k * sin x)) `sc3_mod` two_pi
  in ((x + y') `sc3_mod` two_pi,y')
