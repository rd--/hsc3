{-# Language FlexibleInstances #-}

{- | Unique identifier types and classes.
Used by non-deterministic (noise) and non-sharable (demand) unit generators.
-}
module Sound.Sc3.Common.Uid where

import Data.Functor.Identity {- base -}
import Data.List {- base -}
import qualified Data.Unique as Unique {- base -}

import qualified Control.Monad.Trans.Reader as Reader {- transformers -}
import qualified Control.Monad.Trans.State as State {- transformers -}

import qualified Data.Digest.Murmur32 as Murmur32 {- murmur-hash -}

import qualified Sound.Sc3.Common.Base as Base {- hsc3 -}

-- * Id & Uid

-- | Identifiers are integers.
type Id = Int

-- | A class indicating a monad (and functor and applicative) that will generate a sequence of unique integer identifiers.
class (Functor m, Applicative m, Monad m) => Uid m where
   generateUid :: m Int

-- | Requires FlexibleInstances.
instance Uid (State.StateT Int Identity) where
    generateUid = State.get >>= \n -> State.put (n + 1) >> return n

instance Uid IO where
    generateUid = fmap Unique.hashUnique Unique.newUnique

instance Uid m => Uid (Reader.ReaderT t m) where
   generateUid = Reader.ReaderT (const generateUid)

-- * Uid_St

-- | 'State.State' Uid.
type Uid_St = State.State Int

-- | Alias for 'runIdentity'.
uid_id_eval :: Identity t -> t
uid_id_eval = runIdentity

{- | 'State.evalState' with initial state of zero.

> uid_st_eval (replicateM 3 generateUid) == [0, 1, 2]
-}
uid_st_eval :: Uid_St t -> t
uid_st_eval x = State.evalState x 0

-- | Thread state through sequence of 'State.runState'.
uid_st_seq :: [Uid_St t] -> ([t],Int)
uid_st_seq =
    let swap (p,q) = (q,p)
        step_f n x = swap (State.runState x n)
    in swap . mapAccumL step_f 0

{- | 'fst' of 'uid_st_seq'.

> uid_st_seq_ (replicate 3 generateUid) == [0, 1, 2]
-}
uid_st_seq_ :: [Uid_St t] -> [t]
uid_st_seq_ = fst . uid_st_seq

-- * Lift

-- | Unary Uid lift.
liftUid1 :: Uid m => (Int -> Base.Fn1 a b) -> Base.Fn1 a (m b)
liftUid1 fn a = do
  z <- generateUid
  return (fn z a)

-- | Binary Uid lift.
liftUid2 :: Uid m => (Int -> Base.Fn2 a b c) -> Base.Fn2 a b (m c)
liftUid2 fn a b = do
  z <- generateUid
  return (fn z a b)

-- | Ternary Uid lift.
liftUid3 :: Uid m => (Int -> Base.Fn3 a b c d) -> Base.Fn3 a b c (m d)
liftUid3 fn a b c = do
  z <- generateUid
  return (fn z a b c)

-- | Quaternary Uid lift.
liftUid4 :: Uid m => (Int -> Base.Fn4 a b c d e) -> Base.Fn4 a b c d (m e)
liftUid4 fn a b c d = do
  z <- generateUid
  return (fn z a b c d)

-- | 5-parameter Uid lift.
liftUid5 :: Uid m => (Int -> Base.Fn5 a b c d e f) -> Base.Fn5 a b c d e (m f)
liftUid5 fn a b c d e = do
  z <- generateUid
  return (fn z a b c d e)

-- | 6-parameter Uid lift.
liftUid6 :: Uid m => (Int -> Base.Fn6 a b c d e f g) -> Base.Fn6 a b c d e f (m g)
liftUid6 fn a b c d e f = do
  z <- generateUid
  return (fn z a b c d e f)

-- | 10-parameter Uid lift.
liftUid10 :: Uid m => (Int -> Base.Fn10 a b c d e f g h i j k) -> Base.Fn10 a b c d e f g h i j (m k)
liftUid10 fn a b c d e f g h i j = do
  z <- generateUid
  return (fn z a b c d e f g h i j)

-- | 11-parameter Uid lift.
liftUid11 :: Uid m => (Int -> Base.Fn11 a b c d e f g h i j k l) -> Base.Fn11 a b c d e f g h i j k (m l)
liftUid11 fn a b c d e f g h i j k = do
  z <- generateUid
  return (fn z a b c d e f g h i j k)

-- * ID

{- | Typeclass to constrain Ugen identifiers.
Char inputs are hashed to generate longer seeds for when ir (constant) random Ugens are optimised.

> map resolveID [0::Int,1] == [0, 1]
> map resolveID ['α', 'β'] == [1439603815, 4131151318]
> map resolveID [('α', 'β'),('β', 'α')] == [3538183581, 3750624898]
> map resolveID [('α',('α', 'β')),('β',('α', 'β'))] == [0020082907, 2688286317]
> map resolveID [('α', 'α', 'β'),('β', 'α', 'β')] == [0020082907, 2688286317]
-}
class Murmur32.Hashable32 a => ID a where
    resolveID :: a -> Id
    resolveID = fromIntegral . Murmur32.asWord32 . Murmur32.hash32

instance ID Char where
instance ID Int where resolveID = id
instance (ID p,ID q) => ID (p,q) where
instance (ID p,ID q,ID r) => ID (p,q,r) where

{- | /n/ identifiers from /x/.

> id_seq 10 'α' == [1439603815 .. 1439603824]
-}
id_seq :: ID a => Int -> a -> [Id]
id_seq n x = take n [resolveID x ..]
