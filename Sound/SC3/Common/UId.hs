{-# Language FlexibleInstances #-}

-- | Unique identifier types and classes.
--   Used by non-deterministic (noise) and non-sharable (demand) unit generators.
module Sound.SC3.Common.UId where

import Control.Monad {- base -}
import Data.Functor.Identity {- base -}
import Data.List {- base -}
import qualified Data.Unique as Unique {- base -}

--import qualified Control.Monad.Trans.Identity as Identity {- transformers -}
import qualified Control.Monad.Trans.Reader as Reader {- transformers -}
import qualified Control.Monad.Trans.State as State {- transformers -}
import qualified Data.Digest.Murmur32 as Murmur32 {- hashable -}

import qualified Sound.SC3.Common.Base as Base {- hsc3 -}

-- * Id & UId

-- | Identifiers are integers.
type Id = Int

-- | A class indicating a monad (and functor and applicative) that will
-- generate a sequence of unique integer identifiers.
class (Functor m,Applicative m,Monad m) => UId m where
   generateUId :: m Int

-- | Requires FlexibleInstances.
instance UId (State.StateT Int Identity) where
    generateUId = State.get >>= \n -> State.put (n + 1) >> return n

instance UId IO where
    generateUId = liftM Unique.hashUnique Unique.newUnique

instance UId m => UId (Reader.ReaderT t m) where
   generateUId = Reader.ReaderT (const generateUId)

-- * UId_ST

-- | 'State.State' UId.
type UId_ST = State.State Int

-- | Alias for 'runIdentity'.
uid_id_eval :: Identity t -> t
uid_id_eval = runIdentity

-- | 'State.evalState' with initial state of zero.
--
-- > uid_st_eval (replicateM 3 generateUId) == [0,1,2]
uid_st_eval :: UId_ST t -> t
uid_st_eval x = State.evalState x 0

-- | Thread state through sequence of 'State.runState'.
uid_st_seq :: [UId_ST t] -> ([t],Int)
uid_st_seq =
    let swap (p,q) = (q,p)
        step_f n x = swap (State.runState x n)
    in swap . mapAccumL step_f 0

-- | 'fst' of 'uid_st_seq'.
--
-- > uid_st_seq_ (replicate 3 generateUId) == [0,1,2]
uid_st_seq_ :: [UId_ST t] -> [t]
uid_st_seq_ = fst . uid_st_seq

-- * Lift

-- | Unary UId lift.
liftUId1 :: UId m => (Int -> Base.Fn1 a b) -> Base.Fn1 a (m b)
liftUId1 f a = do
  n <- generateUId
  return (f n a)

-- | Binary UId lift.
liftUId2 :: UId m => (Int -> Base.Fn2 a b c) -> Base.Fn2 a b (m c)
liftUId2 f a b = do
  n <- generateUId
  return (f n a b)

-- | Ternary UId lift.
liftUId3 :: UId m => (Int -> Base.Fn3 a b c d) -> Base.Fn3 a b c (m d)
liftUId3 f a b c = do
  n <- generateUId
  return (f n a b c)

-- | Quaternary UId lift.
liftUId4 :: UId m => (Int -> Base.Fn4 a b c d e) -> Base.Fn4 a b c d (m e)
liftUId4 f a b c d = do
  n <- generateUId
  return (f n a b c d)

-- * ID

-- | Typeclass to constrain UGen identifiers.
--
-- > map resolveID [0::Int,1] == [3151710696,1500603050]
-- > map resolveID ['α','β'] == [1439603815,4131151318]
-- > map resolveID [('α','β'),('β','α')] == [3538183581,3750624898]
-- > map resolveID [('α',('α','β')),('β',('α','β'))] == [0020082907,2688286317]
-- > map resolveID [('α','α','β'),('β','α','β')] == [0020082907,2688286317]
class Murmur32.Hashable32 a => ID a where
    resolveID :: a -> Id
    resolveID = fromIntegral . Murmur32.asWord32 . Murmur32.hash32

instance ID Char where
instance ID Int where
instance (ID p,ID q) => ID (p,q) where
instance (ID p,ID q,ID r) => ID (p,q,r) where

-- | /n/ identifiers from /x/.
--
-- > id_seq 10 'α' == [1439603815 .. 1439603824]
id_seq :: ID a => Int -> a -> [Id]
id_seq n x = take n [resolveID x ..]
