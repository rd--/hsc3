-- | Lifting functions from explicit identifier 'UGen' functions to
-- monadic 'UGen' constructors.
module Sound.SC3.UGen.UGen.Lift where

import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UId

-- | Lift base UGen to monadic form.
liftU :: (UId m) => (Int -> a -> UGen) -> a -> m UGen
liftU f a = do
  n <- generateUId
  return (f n a)

-- | Lift base UGen to monadic form.
liftU2 :: (UId m) => (Int -> a -> b -> UGen) -> a -> b -> m UGen
liftU2 f a b = do
  n <- generateUId
  return (f n a b)

-- | Lift base UGen to monadic form.
liftU3 :: (UId m) => (Int -> a -> b -> c -> UGen) -> a -> b -> c -> m UGen
liftU3 f a b c = do
  n <- generateUId
  return (f n a b c)

-- | Lift base UGen to monadic form.
liftU4 :: (UId m) =>
          (Int -> a -> b -> c -> d -> UGen) ->
          a -> b -> c -> d -> m UGen
liftU4 f a b c d = do
  n <- generateUId
  return (f n a b c d)
