module Sound.SC3.UGen.UId where

import Control.Monad
import Data.Unique

-- | A class indicating a monad that will generate a sequence of
--   unique integer identifiers.
class (Monad m) => UId m where
   generateUId :: m Int

instance UId IO where
   generateUId = liftM hashUnique newUnique
