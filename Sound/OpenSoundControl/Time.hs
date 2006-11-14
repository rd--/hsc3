module Sound.OpenSoundControl.Time (utc, ntp, utc_ntp) where

import System.Time (ClockTime(TOD), getClockTime)
import Control.Monad (liftM)

secdif :: Double
secdif = (70 * 365 + 17) * 24 * 60 * 60

secntp :: Double -> Integer
secntp i = round (i * 4294967296.0)

-- | Convert UTC timestamp to NTP timestamp.
utc_ntp :: Double {-^ UTC timestamp -} -> Integer
utc_ntp t = secntp (t + secdif)

-- | Read current UTC timestamp.
utc :: IO Double
utc = do TOD s p <- getClockTime
         return (fromIntegral s + fromIntegral p / 1e12)

-- | Read current NTP timestamp.
ntp :: IO Integer
ntp = liftM utc_ntp utc
