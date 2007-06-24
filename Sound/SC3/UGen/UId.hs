module Sound.SC3.UGen.UId where

import Control.Monad
import Data.Unique

class (Monad m) => UId m where
   uid :: m Int

instance UId IO where
   uid = liftM hashUnique newUnique


{- |
cf. "Control.Monad.State" of the Monad Template Library MTL

Example:

> evalState $
>    do noiseGen <- Noise.Monadic.brownNoise AR
>       expRand  <- Noise.Monadic.expRand 2 3
>       return (noiseGen + expRand)

or shorter

> evalState $ liftM2 (+)
>    (Noise.Monadic.brownNoise AR)
>    (Noise.Monadic.expRand 2 3)

-}
newtype State a = State { runState :: Int -> (a, Int) }


evalState :: State a -> a
evalState m = fst (runState m 0)

instance Functor State where
   fmap f m = State $ \s -> let
      (a, s') = runState m s
      in (f a, s')

instance Monad State where
   return a = State $ \s -> (a, s)
   m >>= k  = State $ \s -> let
      (a, s') = runState m s
      in runState (k a) s'

instance UId State where
   uid = State $ \u -> (u, succ u)
