module Hsc.Schedule where

import Control.Concurrent (threadDelay)
import System.Time (ClockTime(TOD), getClockTime)
import Control.Monad (liftM, when)

dti :: Double -> Int
dti = floor

itd :: Integer -> Double
itd = fromIntegral

utc :: IO Double
utc = do TOD s p <- getClockTime
         return (itd s + itd p / 1e12)

secdif :: Integer
secdif = (70 * 365 + 17) * 24 * 60 * 60

secntp :: Double -> Integer
secntp i = round (i * itd 2^32)

utc_ntp :: Double -> Integer
utc_ntp n = secntp (n + itd secdif)

ntp :: IO Integer
ntp = liftM utc_ntp utc

pause :: Double -> IO ()
pause n = when (n>0) (threadDelay (dti (n * 1e6)))

pauseUntil t = do n <- utc
                  pause (t - n)

at t f = do n <- f t
            pauseUntil (t + n)
            at (t + n) f

at' f = do t <- utc
           at t f
