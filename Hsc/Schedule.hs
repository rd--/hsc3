module Hsc.Schedule where

import Control.Concurrent
import GHC.Real (floor)
import System.Time

dti :: Double -> Int
dti = GHC.Real.floor

itd :: Integer -> Double
itd = fromIntegral

utc :: IO Double
utc = do TOD s p <- getClockTime
         return (itd s + (itd p / 1000000000000.0))

secdif :: Integer
secdif   = (70 * 365 * 24 * 60 * 60) + (17 * 24 * 60 * 60)

secntp :: Double -> Integer
secntp i = round (i * itd 2^32)

utc_ntp :: Double -> Integer
utc_ntp n = secntp (n + itd secdif)

ntp :: IO Integer
ntp = do t <- utc
         return (utc_ntp t)

pause :: Double -> IO ()
pause n = threadDelay i
    where i = dti $ n * 1000000.0

pauseUntil t = do n <- utc
                  pause (t - n)

at t f = do n <- f t
            pauseUntil (t + n)
            at (t + n) f

at' f = do t <- utc
           forkIO (at t f)
