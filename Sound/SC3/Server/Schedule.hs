module Sound.SC3.Server.Schedule (pause, pauseUntil, at) where

import Sound.OpenSoundControl.Time (utc)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

-- | threadDelay variant with duration given in seconds.
pause :: Double -> IO ()
pause n = when (n>0) (threadDelay (floor (n * 1e6)))

-- | Pause until specified UTC time.
pauseUntil :: Double -> IO ()
pauseUntil t = do n <- utc
                  pause (t - n)

at' :: Double -> (Double -> IO Double) -> IO t
at' t f = do n <- f t
             pauseUntil (t + n)
             at' (t + n) f

-- | Pause until UTC time @t@, apply @f@ to @t@, reschedule @f@ at returned delta.
at :: Double {-^ @t@ -} -> (Double -> IO Double) {-^ @f@ -} -> IO t
at t f = do pauseUntil t
            at' t f
