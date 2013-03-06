-- | Unique identifier class for use by non-deterministic (noise) and
-- non-sharable (demand) unit generators.
module Sound.SC3.UGen.UId where

import Control.Monad {- base -}
import Control.Monad.IO.Class as M {- transformers -}
import Control.Monad.Trans.Reader {- transformers -}
import Data.Unique {- base -}
import Sound.OSC.Transport.FD as T {- hosc -}

-- | A class indicating a monad that will generate a sequence of
--   unique integer identifiers.
class (Functor m,M.MonadIO m) => UId m where
   generateUId :: m Int
   generateUId = fmap hashUnique (M.liftIO newUnique)

instance UId IO where
    generateUId = liftM hashUnique newUnique

instance (T.Transport t,Functor io,MonadIO io) => UId (ReaderT t io) where
   generateUId = ReaderT (M.liftIO . const generateUId)
