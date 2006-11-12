module Sound.SC3.Server.Schedule (utc, ntp, utc_ntp, pause, at) where

import Control.Concurrent (threadDelay)
import System.Time (ClockTime(TOD), getClockTime)
import Control.Monad (liftM, when)

dti :: Double -> Int
dti = floor

itd :: Integer -> Double
itd = fromIntegral

secdif :: Integer
secdif = (70 * 365 + 17) * 24 * 60 * 60

secntp :: Double -> Integer
secntp i = round (i * itd (2^(32::Int)))

-- | Convert UTC timestamp `t' to NTP timestamp.
utc_ntp :: Double -> Integer
utc_ntp t = secntp (t + itd secdif)

-- | Read current UTC timestamp.
utc :: IO Double
utc = do TOD s p <- getClockTime
         return (itd s + itd p / 1e12)

-- | Read current NTP timestamp.
ntp :: IO Integer
ntp = liftM utc_ntp utc

-- | Pause for `n' seconds.
pause :: Double -> IO ()
pause n = when (n>0) (threadDelay (dti (n * 1e6)))

-- | Pause until UTC time `t'. 
pauseUntil :: Double -> IO ()
pauseUntil t = do n <- utc
                  pause (t - n)

at' :: Double -> (Double -> IO Double) -> IO t
at' t f = do n <- f t
             pauseUntil (t + n)
             at' (t + n) f

-- | Pause until UTC time `t', apply `f' to `t', reschedule `f' at returned delta.
at :: Double -> (Double -> IO Double) -> IO t
at t f = do pauseUntil t
            at' t f
