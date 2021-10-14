-- | Linear congruential generator and related utilities
module Sound.SC3.Common.Random where

import Data.Int {- base -}
import System.CPUTime {- base -}

-- | Linear congruential generator given modulo function for type.
--
-- See <http://en.wikipedia.org/wiki/Linear_congruential_generator> for possible parameters.
lcg :: Num t => (t -> t) -> t -> t -> t -> t
lcg modFunc a c x0 = modFunc (a * x0 + c)

-- | 'Int32' linear congruential generator, hence signed modulo of @2^32@.
lcgInt32 :: Int32 -> Int32 -> Int32 -> Int32
lcgInt32 = lcg id

-- | 'Int64' linear congruential generator, hence signed modulo of @2^64@.
lcgInt64 :: Int64 -> Int64 -> Int64 -> Int64
lcgInt64 = lcg id

{- | 'lcgInt64' 6364136223846793005 1442695040888963407, so in (minBound,maxBound).

> take 5 (iterate lcgInt64Knuth 147092873413)
> (maxBound :: Int64) == 9223372036854775807
-}
lcgInt64Knuth :: Int64 -> Int64
lcgInt64Knuth = lcgInt64 6364136223846793005 1442695040888963407

{- | 'lcgInt32' 1103515245 12345

> take 5 (iterate lcgInt32Glibc 873413)
> (maxBound :: Int32) == 2147483647
-}
lcgInt32Glibc :: Int32 -> Int32
lcgInt32Glibc = lcgInt32 1103515245 12345

-- | Run getCPUTime and convert to Int64
cpuTimeSeedInt64 :: IO Int64
cpuTimeSeedInt64 = fmap (fromIntegral . flip div cpuTimePrecision) getCPUTime

-- | Run getCPUTime and convert to Int32
cpuTimeSeedInt32 :: IO Int32
cpuTimeSeedInt32 = fmap (fromIntegral . flip div cpuTimePrecision) getCPUTime

{- | lcgInt64Knuth seeded by cpuTimeSeedInt64

> fmap (take 4) lcgInt64CpuTime
-}
lcgInt64CpuTime :: IO [Int64]
lcgInt64CpuTime = fmap (iterate lcgInt64Knuth) cpuTimeSeedInt64

{- | lcgInt64Knuth seeded by cpuTimeSeedInt64

> fmap (map int64ToUnitRational . take 2) lcgInt64CpuTime
-}
int64ToUnitRational :: Int64 -> Rational
int64ToUnitRational n = fromIntegral n / fromIntegral (maxBound :: Int64)

{- | realToFrac of int64ToUnitRational

> fmap (map int64ToUnitDouble . take 4) lcgInt64CpuTime
-}
int64ToUnitDouble :: Int64 -> Double
int64ToUnitDouble = realToFrac . int64ToUnitRational
