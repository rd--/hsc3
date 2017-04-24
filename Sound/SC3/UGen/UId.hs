{-# LANGUAGE FlexibleInstances #-}

-- | Unique identifier class for use by non-deterministic (noise) and
-- non-sharable (demand) unit generators.
module Sound.SC3.UGen.UId where

import Control.Monad {- base -}
import Data.Functor.Identity {- base -}
import Data.List {- base -}
import qualified Data.Unique as U {- base -}

import qualified Control.Monad.Trans.Reader as R {- transformers -}
import qualified Control.Monad.Trans.State as S {- transformers -}

import Sound.SC3.UGen.Type {- hsc3 -}

-- | A class indicating a monad (and functor and applicative) that will
-- generate a sequence of unique integer identifiers.
class (Functor m,Applicative m,Monad m) => UId m where
   generateUId :: m Int

-- | 'S.State' UId.
type UId_ST = S.State Int

-- | 'S.evalState' with initial state of zero.
--
-- > uid_st_eval (replicateM 3 generateUId) == [0,1,2]
uid_st_eval :: UId_ST t -> t
uid_st_eval x = S.evalState x 0

-- | Thread state through sequence of 'S.runState'.
uid_st_seq :: [UId_ST t] -> ([t],Int)
uid_st_seq =
    let swap (p,q) = (q,p)
        step_f n x = swap (S.runState x n)
    in swap . mapAccumL step_f 0

-- | 'fst' of 'uid_st_seq'.
--
-- > uid_st_seq_ (replicate 3 generateUId) == [0,1,2]
uid_st_seq_ :: [UId_ST t] -> [t]
uid_st_seq_ = fst . uid_st_seq

instance UId (S.StateT Int Identity) where
    generateUId = S.get >>= \n -> S.put (n + 1) >> return n

instance UId IO where
    generateUId = liftM U.hashUnique U.newUnique

instance UId m => UId (R.ReaderT t m) where
   generateUId = R.ReaderT (const generateUId)

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
