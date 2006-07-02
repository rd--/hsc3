module Hsc.Random where

import Hsc.UGen(UGen(Constant, MCE))

import Control.Monad
import System.Random

rrand :: Double -> Double -> IO Double
rrand l r = do n <- getStdRandom (randomR (l,r))
               return n

rrandi :: Int -> Int -> IO Int
rrandi l r = do n <- getStdRandom (randomR (l,r))
                return n

nrrand = rrand 0 1

nrrand_linear = do a <- nrrand
                   b <- nrrand
                   return (min a b)

nrrand_inverse_linear = do a <- nrrand
                           b <- nrrand
                           return (max a b)

nrrand_triangular = do a <- nrrand
                       b <- nrrand
                       return (0.5 * (a + b))

nrrand_exponential l = do u <- nrrand
                          return ((- (log u)) / l)

rrandx l r = do a <- rrand 0.0 1.0
                return (((r / l) ** a) * l)

rrandl n l r = do rs <- replicateM n (rrand l r)
                  return rs

choose l = do n <- getStdRandom (randomR (0,(length l)-1))
              return (l !! n)

rrandc l r = do r <- rrand l r
                return (Constant r)

rrandmce n l r = do rs <- rrandl n l r
                    return (MCE (map Constant rs))

