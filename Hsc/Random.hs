module Hsc.Random where

import Hsc.UGen(UGen(Constant, MCE))

import System.Random (getStdRandom, randomR)
import Control.Monad (liftM, liftM2, replicateM)

rrand :: Double -> Double -> IO Double
rrand l r = getStdRandom (randomR (l,r))

rrandi :: Int -> Int -> IO Int
rrandi l r = getStdRandom (randomR (l,r))

nrrand :: IO Double
nrrand = rrand 0 1

-- auxiliary function
merge2rand :: (Double -> Double -> r) -> IO r
merge2rand f = liftM2 f nrrand nrrand

{- |
Linearly distributed in [0,1) with a mean value of 0.2929.  The
density function is given by @f(x) = 2 * (1 - x)@.
-}
nrrand_linear :: IO Double
nrrand_linear = merge2rand min

{- |
Linearly distributed in [0,1) with a mean value of 0.6969.  The
density function is given by @f(x) = 2 * (x - 1)@.
-}
nrrand_inverse_linear :: IO Double
nrrand_inverse_linear = merge2rand max

nrrand_triangular :: IO Double
nrrand_triangular = merge2rand (\a b -> (a+b)/2)

nrrand_exponential :: Double -> IO Double
nrrand_exponential l = do u <- nrrand
                          return (- log u / l)

rrandx :: Double -> Double -> IO Double
rrandx l r = do a <- rrand 0.0 1.0
                return ((r / l) ** a * l)

rrandl :: Int -> Double -> Double -> IO [Double]
rrandl n l r = replicateM n (rrand l r)

{- | Random list element -}
choose :: [a] -> IO a
choose l = liftM (l!!) (getStdRandom (randomR (0, length l - 1)))

rrandc :: Double -> Double -> IO UGen
rrandc l r = liftM Constant (rrand l r)

rrandmce :: Int -> Double -> Double -> IO UGen
rrandmce n l r = liftM (MCE . map Constant) (rrandl n l r)
