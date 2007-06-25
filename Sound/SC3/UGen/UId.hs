module Sound.SC3.UGen.UId where

import Control.Monad
import Data.Unique

class (Monad m) => UId m where
   generateUId :: m Int

instance UId IO where
   generateUId = liftM hashUnique newUnique
