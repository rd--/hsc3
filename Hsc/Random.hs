module Hsc.Random where

import System.Random

rrand :: Float -> Float -> IO Float
rrand l r = do n <- getStdRandom (randomR (l,r))
               return n

rrandi :: Int -> Int -> IO Int
rrandi l r = do n <- getStdRandom (randomR (l,r))
                return n

-- Normal value in [0,1]

nrrand = rrand 0 1

-- Linearly distributed in [0,1) with a mean value of 0.2929.  The
-- density function is given by 'f(x) = 2 * (1 - x)'.

nrrand_linear = do a <- nrrand
                   b <- nrrand
                   return (min a b)

-- Linearly distributed in [0,1) with a mean value of 0.6969.  The
-- density function is given by 'f(x) = 2 * (x - 1)'.

nrrand_inverse_linear = do a <- nrrand
                           b <- nrrand
                           return (max a b)

-- Triangularly distributed in [0,1) with a mean value of 0.5.

nrrand_triangular = do a <- nrrand
                       b <- nrrand
                       return (0.5 * (a + b))

-- Exponentialy distributed with a mean value of '0.69315 / l'.  There
-- is no upper limit on the value however there is only a one in
-- one-thousand chance of generating a number greater than '6.9078 /
-- l'.  The density function is given by: 'f(x) = l ^ (-l * x)'.

nrrand_exponential l = do u <- nrrand
                          return ((- (log u)) / l)

-- Exponential variant, very common in music work.

rrandx minima maxima = do a <- rrand 0.0 1.0
                          return ((ratio ** a) * minima)
    where ratio = maxima / minima

-- Random list element

choose :: [a] -> IO a
choose l = do n <- getStdRandom (randomR (0,(length l)-1))
              return (l !! n)
