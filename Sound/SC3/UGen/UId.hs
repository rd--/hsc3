-- | Unique identifier class for use by non-deterministic (noise) and
-- non-sharable (demand) unit generators.
module Sound.SC3.UGen.UId where

import Control.Monad
import Control.Monad.IO.Class
import Data.Unique

-- | A class indicating a monad that will generate a sequence of
--   unique integer identifiers.
class (Functor m,MonadIO m) => UId m where
   generateUId :: m Int
   generateUId = fmap hashUnique (liftIO newUnique)

instance UId IO where
    generateUId = liftM hashUnique newUnique
