-- | Unique identifier class for use by non-deterministic (noise) and
-- non-sharable (demand) unit generators.
module Sound.SC3.UGen.UId where

import Control.Applicative {- base -}
import Control.Monad {- base -}
import Control.Monad.IO.Class as M {- transformers -}
import Control.Monad.Trans.Reader {- transformers -}
import Data.Unique {- base -}
import Sound.OSC.Transport.FD as T {- hosc -}

-- | A class indicating a monad that will generate a sequence of
--   unique integer identifiers.
class (Functor m,Applicative m,M.MonadIO m) => UId m where
   generateUId :: m Int
   generateUId = fmap hashUnique (M.liftIO newUnique)

instance UId IO where
    generateUId = liftM hashUnique newUnique

instance (T.Transport t,Functor io,Applicative io,MonadIO io) =>
    UId (ReaderT t io) where
   generateUId = ReaderT (M.liftIO . const generateUId)

-- | Unary function.
type Fn1 a b = a -> b

-- | Binary function.
type Fn2 a b c = a -> b -> c

-- | Ternary function.
type Fn3 a b c d = a -> b -> c -> d

-- | Quaternary function.
type Fn4 a b c d e = a -> b -> c -> d -> e

-- | Unary UId lift.
liftUId :: UId m => (Int -> Fn1 a b) -> Fn1 a (m b)
liftUId f a = do
  n <- generateUId
  return (f n a)

-- | Binary UId lift.
liftUId2 :: UId m => (Int -> Fn2 a b c) -> Fn2 a b (m c)
liftUId2 f a b = do
  n <- generateUId
  return (f n a b)

-- | Ternary UId lift.
liftUId3 :: UId m => (Int -> Fn3 a b c d) -> Fn3 a b c (m d)
liftUId3 f a b c = do
  n <- generateUId
  return (f n a b c)

-- | Quaternary UId lift.
liftUId4 :: UId m => (Int -> Fn4 a b c d e) -> Fn4 a b c d (m e)
liftUId4 f a b c d = do
  n <- generateUId
  return (f n a b c d)
