-- | Linear congruential generator and related utilities.  Ordinarily use System.Random.
module Sound.Sc3.Common.Random where

import Data.Bits {- base -}
import Data.Int {- base -}
import Data.Word {- base -}
import System.CPUTime {- base -}

{- | Linear congruential generator given modulo function for type.

See <http://en.wikipedia.org/wiki/Linear_congruential_generator> for possible parameters.
-}
lcg :: Num t => (t -> t) -> t -> t -> t -> t
lcg modFunc a c x0 = modFunc (a * x0 + c)

{- | 'lcg' 6364136223846793005 1442695040888963407, so in (0, 18446744073709551615)

> take 5 (iterate lcgWord64Knuth 147092873413)
> (maxBound :: Word64) == (2 ^ 64 - 1)
-}
lcgWord64Knuth :: Word64 -> Word64
lcgWord64Knuth = lcg id 6364136223846793005 1442695040888963407

{- | 'lcg' 1103515245 12345, so in (-2147483648, 2147483647)

> take 5 (iterate lcgInt32Glibc 873413)
> (minBound :: Int32,maxBound :: Int32) == (-2147483648, 2147483647)
-}
lcgInt32Glibc :: Int32 -> Int32
lcgInt32Glibc = lcg id 1103515245 12345

-- | Run getCPUTime and convert to Word64
cpuTimeSeedWord64 :: IO Word64
cpuTimeSeedWord64 = fmap (fromIntegral . flip div cpuTimePrecision) getCPUTime

-- | Run getCPUTime and convert to Int32
cpuTimeSeedInt32 :: IO Int32
cpuTimeSeedInt32 = fmap (fromIntegral . flip div cpuTimePrecision) getCPUTime

-- | Iterate lcgWord64Knuth using cpuTimeSeedWord64.
lcgWord64KnuthCpuTime :: IO [Word64]
lcgWord64KnuthCpuTime = fmap (iterate lcgWord64Knuth) cpuTimeSeedWord64

{- | Convert Word64 to Double in range (0, 1).
     Shifts input right 11 places (ie. discards 11 least significant bits) then divide by 2^53.
-}
word64ToUnitDouble :: Word64 -> Double
word64ToUnitDouble n = realToFrac (shiftR n 11) / realToFrac (shiftL (1 :: Word64) 53)

{- | word64ToUnitDouble of lcgWord64KnuthCpuTime

> x <- fmap (take 256) lcgDoubleKnuthCpuTime
> Sound.Sc3.Plot.plot_p1_ln [x]
-}
lcgDoubleKnuthCpuTime :: IO [Double]
lcgDoubleKnuthCpuTime = fmap (map word64ToUnitDouble) lcgWord64KnuthCpuTime
