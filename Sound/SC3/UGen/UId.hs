{-# Language FlexibleInstances,UndecidableInstances #-}
-- | Unique identifier class for use by non-deterministic (noise) and
-- non-sharable (demand) unit generators.
module Sound.SC3.UGen.UId where

--import Control.Monad
import Control.Monad.IO.Class
import Data.Unique

uidIO :: (Functor m,MonadIO m) => m Int
uidIO = fmap hashUnique (liftIO newUnique)

-- | A class indicating a monad that will generate a sequence of
--   unique integer identifiers.
class (Monad m) => UId m where
   generateUId :: m Int

--instance UId IO where
--   generateUId = liftM hashUnique newUnique

instance (MonadIO m,Functor m) => UId m where
   generateUId = uidIO

{-
{-# Language FlexibleInstances,UndecidableInstances #-}
import Control.Monad.IO.Class
import Sound.OSC
instance (Transport m) => UId m where
    generateUId = liftM hashUnique (liftIO newUnique)
-}
