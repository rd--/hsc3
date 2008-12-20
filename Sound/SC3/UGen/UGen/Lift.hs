module Sound.SC3.UGen.UGen.Lift where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UId
import System.IO.Unsafe

-- | Lift base UGen to monadic form.
liftU :: (UId m) => (UGenId -> a -> UGen) -> (a -> m UGen)
liftU f a = do n <- generateUId
               return (f (UGenId n) a)

-- | Lift base UGen to monadic form.
liftU2 :: (UId m) => (UGenId -> a -> b -> UGen) -> (a -> b -> m UGen)
liftU2 f a b = do n <- generateUId
                  return (f (UGenId n) a b)

-- | Lift base UGen to monadic form.
liftU3 :: (UId m) => (UGenId -> a -> b -> c -> UGen) -> (a -> b -> c -> m UGen)
liftU3 f a b c = do n <- generateUId
                    return (f (UGenId n) a b c)

-- | Lift base UGen to monadic form.
liftU4 :: (UId m) => (UGenId -> a -> b -> c -> d -> UGen) -> (a -> b -> c -> d -> m UGen)
liftU4 f a b c d = do n <- generateUId
                      return (f (UGenId n) a b c d)

-- | Lift monadic UGen to unsafe form.
liftP :: (a -> IO UGen) -> (a -> UGen)
liftP f = unsafePerformIO . f

-- | Lift monadic UGen to unsafe form.
liftP2 :: (a -> b -> IO UGen) -> (a -> b -> UGen)
liftP2 f a = unsafePerformIO . f a

-- | Lift monadic UGen to unsafe form.
liftP3 :: (a -> b -> c -> IO UGen) -> (a -> b -> c -> UGen)
liftP3 f a b = unsafePerformIO . f a b

-- | Lift monadic UGen to unsafe form.
liftP4 :: (a -> b -> c -> d -> IO UGen) -> (a -> b -> c -> d -> UGen)
liftP4 f a b c = unsafePerformIO . f a b c
