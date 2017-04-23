-- | Unique identifier class for use by non-deterministic (noise) and
-- non-sharable (demand) unit generators.
module Sound.SC3.UGen.UId where

import Control.Monad {- base -}
import qualified Control.Monad.IO.Class as M {- base -}
import qualified Data.Unique as U {- base -}

import qualified Control.Monad.Trans.Reader as R {- transformers -}

import Sound.SC3.UGen.Type

{-

UId as written has MonadIO as a pre-condition on m, but it needn't...

{-# LANGUAGE FlexibleInstances #-}

import Data.Functor.Identity {- base -}
import qualified Control.Monad.Trans.State as S {- transformers -}

type UId_ST = S.State Int

-- > S.runState (generateUId_st >> generateUId_st) 0
generateUId_st :: UId_ST Int
generateUId_st = do
  n <- S.get
  S.put (n + 1)
  return n

class (Functor m,Applicative m,Monad m) => UId' m where
   generateUId' :: m Int

instance UId' (S.StateT Int Identity) where
   generateUId' = generateUId_st

-}

generateUId_mio :: M.MonadIO m => m Int
generateUId_mio = fmap U.hashUnique (M.liftIO U.newUnique)

generateUId_io :: IO Int
generateUId_io = liftM U.hashUnique U.newUnique

-- | A class indicating a monad that will generate a sequence of
--   unique integer identifiers.
class (Functor m,Applicative m,M.MonadIO m) => UId m where
   generateUId :: m Int
   generateUId = generateUId_mio

instance UId IO where
    generateUId = generateUId_io

instance (Functor m,Applicative m,M.MonadIO m) => UId (R.ReaderT t m) where
   generateUId = R.ReaderT (M.liftIO . const generateUId)

-- * Lift

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

-- * Clone

-- | Clone a unit generator (mce . replicateM).
clone :: (UId m) => Int -> m UGen -> m UGen
clone n = liftM mce . replicateM n
